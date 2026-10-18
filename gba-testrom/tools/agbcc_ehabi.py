#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Add EHABI unwind recipes and catch-site LSDA to Thumb output from agbcc.

The C backend marks exceptional calls and landing pads with local labels. This
pass adds frame recipes and catch-all call-site records without changing .text.
"""

import re
import sys
from pathlib import Path


FUNCTION_LABEL = re.compile(r"^([A-Za-z_.$][\w.$]*):\s*(?:@.*)?$")
EH_OP_MARKER = re.compile(
    r"^\.Lsn_eh_op_[A-Za-z0-9_]+_(\d+)_handler_(\d+)_(start|end):$"
)
EH_LANDING_MARKER = re.compile(
    r"^\.Lsn_eh_landing_[A-Za-z0-9_]+_(\d+):$"
)


def annotate(source: str) -> str:
    lines = source.splitlines()
    result = []
    i = 0
    while i < len(lines):
        result.append(lines[i])
        if lines[i].strip() != ".thumb_func":
            i += 1
            continue

        label = i + 1
        while label < len(lines) and not FUNCTION_LABEL.match(lines[label].strip()):
            if lines[label].strip() == ".fnstart":
                raise ValueError("assembly already contains EHABI function directives")
            result.append(lines[label])
            label += 1
        if label >= len(lines):
            raise ValueError(".thumb_func has no following function label")
        symbol = FUNCTION_LABEL.match(lines[label].strip()).group(1)

        size = label + 1
        size_pattern = re.compile(r"^\s*\.size\s+" + re.escape(symbol) + r"\s*,")
        while size < len(lines) and not size_pattern.match(lines[size]):
            size += 1
        if size >= len(lines):
            raise ValueError(f"function {symbol} has no .size directive")

        marker_lines = {}
        landing_lines = {}
        for line_index, line in enumerate(lines[label + 1 : size], label + 1):
            text = line.strip()
            call_match = EH_OP_MARKER.match(text)
            landing_match = EH_LANDING_MARKER.match(text)
            if call_match:
                key = (
                    int(call_match.group(1)),
                    int(call_match.group(2)),
                    call_match.group(3),
                )
                if key in marker_lines:
                    raise ValueError(f"function {symbol} repeats EHABI call marker {key}")
                marker_lines[key] = (line_index, text[:-1])
            if landing_match:
                landing_lines[int(landing_match.group(1))] = (line_index, text[:-1])

        call_edges = {(call_id, handler_id) for call_id, handler_id, _ in marker_lines}
        landing_call_ids = set()
        handler_ids = set()
        for call_id, handler_id in call_edges:
            if (call_id, handler_id, "start") not in marker_lines or (
                call_id,
                handler_id,
                "end",
            ) not in marker_lines:
                raise ValueError(
                    f"function {symbol} has an incomplete EHABI call marker {call_id}"
                )
            start_line, start_label = marker_lines[(call_id, handler_id, "start")]
            end_line, end_label = marker_lines[(call_id, handler_id, "end")]
            if end_line <= start_line:
                raise ValueError(f"function {symbol} has a reversed EHABI call range {call_id}")
            if handler_id not in landing_lines:
                raise ValueError(
                    f"function {symbol} has no EHABI landing marker {handler_id}"
                )
            landing_line, landing_label = landing_lines[handler_id]
            if landing_line <= label:
                raise ValueError(
                    f"function {symbol} has an invalid EHABI landing marker {handler_id}"
                )
            handler_ids.add(handler_id)
            landing_call_ids.add(
                (start_line, start_label, end_label, landing_label)
            )
        if set(landing_lines) - handler_ids:
            raise ValueError(f"function {symbol} has an unreferenced EHABI landing marker")
        call_ranges = sorted(
            (
                marker_lines[(call_id, handler_id, "start")][0],
                marker_lines[(call_id, handler_id, "end")][0],
            )
            for call_id, handler_id, edge in marker_lines
            if edge == "start"
        )
        for previous, current in zip(call_ranges, call_ranges[1:]):
            if previous[1] >= current[0]:
                raise ValueError(f"function {symbol} has overlapping EHABI call ranges")

        def constant_before(index, register):
            if index <= label + 1:
                return None
            instruction = lines[index - 1].strip()
            immediate = r"(-?(?:0x[0-9a-fA-F]+|[0-9]+))"
            move = re.fullmatch(r"mov\s+" + register + r",\s*#" + immediate, instruction)
            if move:
                return int(move.group(1), 0)
            load = re.fullmatch(r"ldr\s+" + register + r",\s*(\.L[\w.$]+)", instruction)
            if load:
                target = load.group(1) + ":"
                for literal_index in range(label + 1, size - 1):
                    if lines[literal_index].strip() == target:
                        word = re.fullmatch(r"\.word\s+" + immediate, lines[literal_index + 1].strip())
                        return int(word.group(1), 0) if word else None
            shift = re.fullmatch(r"lsl\s+" + register + r",\s*" + register + r",\s*#" + immediate, instruction)
            if shift:
                value = constant_before(index - 1, register)
                if value is not None:
                    return value << int(shift.group(1), 0)
            return None

        def register_stack_adjustment(index):
            match = re.fullmatch(r"(add|sub)\s+sp,\s*sp,\s*(r[0-9]+)", lines[index].strip())
            if match:
                value = constant_before(index, match.group(2))
                if value is not None:
                    return value if match.group(1) == "add" else -value
            return None

        frame_events = []
        frame_pointer = None
        moved_high_registers = {}
        k = label + 1
        while k < size:
            instruction = lines[k].strip()
            if instruction == "@ sn_xlat_stack_snapshot":
                break
            push = re.match(r"(?:push|stmfd\s+sp!|stmdb\s+sp!)\s*\{([^}]+)\}", instruction)
            if push:
                registers = [reg.strip() for reg in push.group(1).split(",")]
                restored = [moved_high_registers.pop(reg, reg) for reg in registers]
                frame_events.append(("save", restored))
                k += 1
                continue
            adjustment = re.match(
                r"(?:add|sub)\s+sp,\s*sp,\s*#(-?(?:0x[0-9a-fA-F]+|[0-9]+))$",
                instruction,
            )
            if adjustment:
                amount = int(adjustment.group(1), 0)
                if instruction.split(None, 1)[0] == "sub":
                    amount = -amount
                if amount < 0:
                    frame_events.append(("pad", -amount))
                    k += 1
                    continue
                break
            # Large fixed frames exceed Thumb's immediate SP adjustment range.
            # Only consume an adjacent, provably constant literal-load sequence.
            if re.fullmatch(r"ldr\s+r[0-9]+,\s*\.L[\w.$]+", instruction) and k + 1 < size:
                amount = register_stack_adjustment(k + 1)
                if amount is not None and amount < 0 and amount % 4 == 0:
                    frame_events.append(("pad", -amount))
                    k += 2
                    continue
            high_register_move = re.match(
                r"mov\s+r([0-7]),\s*(r(?:8|9|10|11|12)|sl|fp|ip)$",
                instruction,
            )
            if high_register_move:
                low, high = high_register_move.groups()
                high = {"sl": "r10", "fp": "r11", "ip": "r12"}.get(high, high)
                low = f"r{low}"
                if low in moved_high_registers:
                    raise ValueError(
                        f"function {symbol} overwrites an unsaved high register"
                    )
                moved_high_registers[low] = high
                k += 1
                continue
            fp = re.fullmatch(
                r"(mov|add|sub)\s+(r7|r11|fp),\s*sp(?:,\s*#(-?(?:0x[0-9a-fA-F]+|[0-9]+)))?",
                instruction,
            )
            if fp:
                if frame_pointer is not None:
                    raise ValueError(f"function {symbol} redefines its frame pointer")
                operation, register, offset = fp.groups()
                if operation != "mov" and offset is None:
                    raise ValueError(f"function {symbol} has an invalid frame pointer offset")
                offset = int(offset, 0) if offset is not None else 0
                if operation == "sub":
                    offset = -offset
                frame_pointer = "r11" if register == "fp" else register
                frame_events.append(("setfp", (frame_pointer, offset)))
                k += 1
                continue
            if not instruction or instruction.startswith("@"):
                k += 1
                continue
            break

        if moved_high_registers:
            raise ValueError(
                f"function {symbol} moves high registers without a matching saved low register"
            )

        if frame_pointer is not None:
            destination = re.compile(r"([a-z]+)\s+" + re.escape(frame_pointer) + r"(?=[,!])")
            reads_only = {"cmp", "cmn", "tst", "teq", "str", "strb", "strh", "push"}
            for line in lines[k:size]:
                match = destination.match(line.strip())
                if match and match.group(1) not in reads_only:
                    frame_pointer = None
                    frame_events = [(event, payload) for event, payload in frame_events if event != "setfp"]
                    break

        for body_index in range(k, size):
            text = lines[body_index].strip()
            if re.match(r"(?:push|stmfd\s+sp!|stmdb\s+sp!)\s*\{", text):
                raise ValueError(
                    f"function {symbol} saves registers after its unwindable prologue"
                )
            if frame_pointer is None and re.match(r"sub\s+sp,\s*sp,\s*#", text):
                raise ValueError(
                    f"function {symbol} allocates stack after its unwindable prologue"
                )
            if frame_pointer is None and re.match(r"(?:add|sub)\s+sp,\s*sp,\s*r\d+\b", text):
                amount = register_stack_adjustment(body_index)
                fixed_size = sum(payload for event, payload in frame_events if event == "pad")
                if amount is not None and amount == fixed_size and amount > 0:
                    continue
                raise ValueError(
                    f"function {symbol} adjusts SP dynamically; this annotator cannot describe it"
                )
        for body_index in range(k, size):
            text = lines[body_index].strip()
            if body_index > k and lines[body_index - 1].strip() == "@ sn_xlat_stack_snapshot":
                continue
            if frame_pointer is not None and re.match(
                r"(?:mov|add|sub)\s+" + re.escape(frame_pointer) + r",\s*sp\b", text
            ):
                raise ValueError(f"function {symbol} redefines its frame pointer")

        result.append(".fnstart")
        for event, payload in frame_events:
            if event == "save":
                # GNU as reverses these directives into the pop sequence.
                result.append(".save {" + ", ".join(dict.fromkeys(payload)) + "}")
            elif event == "pad":
                result.append(f".pad #{payload}")
            elif event == "setfp":
                register, offset = payload
                result.append(f".setfp {register}, sp, #{offset}")
        if landing_call_ids:
            result.append(".personality scalanative_personality")
        result.append(lines[label])
        result.extend(lines[label + 1 : size + 1])
        if landing_call_ids:
            lsda = f".Lsn_lsda_{i}"
            cs_table = f"{lsda}_callsites"
            cs_end = f"{cs_table}_end"
            result.extend([".handlerdata", f"{lsda}:", "\t.byte 0xff", "\t.byte 0xff", "\t.byte 1"])
            result.append(f"\t.uleb128 {cs_end}-{cs_table}")
            result.append(f"{cs_table}:")
            for _, start_label, end_label, landing_label in sorted(landing_call_ids):
                result.extend(
                    [
                        f"\t.uleb128 {start_label}-{symbol}",
                        f"\t.uleb128 {end_label}-{start_label}",
                        f"\t.uleb128 {landing_label}-{symbol}",
                        "\t.uleb128 0",
                    ]
                )
            result.append(f"{cs_end}:")
        result.append(".fnend")
        i = size + 1
    return "\n".join(result) + "\n"


def main() -> int:
    if len(sys.argv) != 3:
        print(f"usage: {sys.argv[0]} INPUT.s OUTPUT.s", file=sys.stderr)
        return 2
    source = Path(sys.argv[1]).read_text()
    try:
        output = annotate(source)
    except ValueError as error:
        print(f"EHABI annotation failed: {error}", file=sys.stderr)
        return 1
    Path(sys.argv[2]).write_text(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
