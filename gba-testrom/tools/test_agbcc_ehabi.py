# SPDX-License-Identifier: Apache-2.0
import unittest
import os
from pathlib import Path
import subprocess
import tempfile

from agbcc_ehabi import annotate


class AgbccEhabiTest(unittest.TestCase):
    def test_protected_original_local_precedes_register_saves(self):
        source = (".thumb_func\nprotected:\nsub sp, sp, #12\npush {r4, r5, r6, lr}\n"
                  "sub sp, sp, #12\nbl callee\nadd sp, sp, #12\npop {r4, r5, r6}\n"
                  "pop {r1}\nadd sp, sp, #12\nbx r1\n.size protected, .-protected\n")
        self.assertIn(".pad #12\n.save {r4, r5, r6, lr}\n.pad #12\nprotected:", annotate(source))

    def test_leaf_function_gets_finish_recipe(self):
        source = ".thumb_func\nleaf:\n\tbx\tlr\n.size leaf,.Lend-leaf\n"
        result = annotate(source)
        self.assertIn(".fnstart\nleaf:", result)
        self.assertIn(".size leaf,.Lend-leaf\n.fnend", result)
        self.assertIn("\tbx\tlr\n", result)

    def test_saved_lr_and_fixed_stack_allocation_are_described(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{lr}\n"
            "\tadd\tsp, sp, #-0xc\n"
            "\tbl\tcallee\n"
            "\tadd\tsp, sp, #0xc\n"
            "\tpop\t{r1}\n"
            "\tbx\tr1\n"
            ".size caller,.Lend-caller\n"
        )
        result = annotate(source)
        self.assertIn(".fnstart\n.save {lr}\n.pad #12\ncaller:", result)

    def test_thumb_high_register_save_groups_are_described(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{r4, r5, r6, r7, lr}\n"
            "\tmov\tr7, sl\n"
            "\tmov\tr6, r9\n"
            "\tmov\tr5, r8\n"
            "\tpush\t{r5, r6, r7}\n"
            "\tadd\tsp, sp, #-0x24\n"
            "\tbl\tcallee\n"
            ".size caller,.Lend-caller\n"
        )
        result = annotate(source)
        self.assertIn(
            ".fnstart\n.save {r4, r5, r6, r7, lr}\n"
            ".save {r8, r9, r10}\n.pad #36\ncaller:",
            result,
        )

    def test_frame_pointer_is_described(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{r7, lr}\n"
            "\tadd\tr7, sp, #0\n"
            "\tbl\tcallee\n"
            ".size caller,.Lend-caller\n"
        )
        self.assertIn(".save {r7, lr}\n.setfp r7, sp, #0", annotate(source))

    def test_dynamic_stack_uses_fixed_frame_pointer(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{r4, r5, r6, r7, lr}\n"
            "\tmov\tr7, sl\n\tmov\tr6, r9\n\tmov\tr5, r8\n"
            "\tpush\t{r5, r6, r7}\n"
            "\tadd\tsp, sp, #-0x38\n"
            "\tadd\tr7, sp, #0xc\n"
            "\tstr\tr0, [r7]\n"
            "\tsub\tsp, sp, r0\n"
            "\tbl\tcallee\n"
            ".size caller,.Lend-caller\n"
        )
        result = annotate(source)
        self.assertIn(".pad #56\n.setfp r7, sp, #12", result)
        # Adding metadata must preserve every original instruction and label.
        retained = [line for line in result.splitlines()
                    if not line.startswith((".fnstart", ".save", ".pad", ".setfp", ".fnend"))]
        self.assertEqual(source.splitlines(), retained)

    def test_body_can_take_fixed_stack_array_address_in_r7(self):
        source = (
            ".thumb_func\ncaller:\n\tpush\t{r7, lr}\n"
            "\tbl\tcallee\n\tmov\tr7, sp\n"
            ".size caller,.Lend-caller\n"
        )
        self.assertNotIn(".setfp", annotate(source))

    def test_overwritten_anchor_cannot_describe_dynamic_stack(self):
        source = (
            ".thumb_func\ncaller:\npush {r7, lr}\nmov r7, sp\n"
            "sub sp, sp, r0\nbl callee\nmov r7, r0\n"
            ".size caller,.-caller\n"
        )
        with self.assertRaisesRegex(ValueError, "dynamically"):
            annotate(source)

    def test_dynamic_stack_adjustment_is_rejected(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{lr}\n"
            "\tsub\tsp, sp, r0\n"
            "\tbl\tcallee\n"
            ".size caller,.Lend-caller\n"
        )
        with self.assertRaisesRegex(ValueError, "dynamically"):
            annotate(source)

    def test_assembled_frame_pointer_recipe_and_text_bytes(self):
        prefix = str(Path.home() / ".local/arm-gnu-toolchain/bin/arm-none-eabi-")
        assembler = os.environ.get("ARM_AS", prefix + "as")
        readelf = os.environ.get("ARM_READELF", prefix + "readelf")
        objcopy = os.environ.get("ARM_OBJCOPY", prefix + "objcopy")
        if not all(Path(tool).is_file() for tool in (assembler, readelf, objcopy)):
            self.skipTest("ARM binutils required")
        source = (
            ".code 16\n.thumb_func\ncaller:\n"
            "push {r4, r5, r6, r7, lr}\n"
            "mov r7, sl\nmov r6, r9\nmov r5, r8\npush {r5, r6, r7}\n"
            "add sp, sp, #-0x38\nadd r7, sp, #0xc\n"
            "str r0, [r7]\nadd sp, sp, r0\nbl callee\n"
            ".size caller,.-caller\n"
        )
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name, text in (("original", source), ("annotated", annotate(source))):
                assembly, obj, binary = (root / (name + ext) for ext in (".s", ".o", ".bin"))
                assembly.write_text(text)
                subprocess.run([assembler, "-mcpu=arm7tdmi", "-o", str(obj), str(assembly)], check=True)
                subprocess.run([objcopy, "-O", "binary", "--only-section=.text", str(obj), str(binary)], check=True)
            self.assertEqual((root / "original.bin").read_bytes(), (root / "annotated.bin").read_bytes())
            unwind = subprocess.check_output([readelf, "--unwind", str(root / "annotated.o")], text=True)
            self.assertIn("vsp = r7", unwind)
            self.assertIn("vsp = vsp + 44", unwind)
            self.assertIn("pop {r8, r9, r10}", unwind)
            self.assertIn("pop {r4, r5, r6, r7, r14}", unwind)

    def test_large_fixed_frame_literal_load_is_described(self):
        source = (
            ".thumb_func\ncaller:\npush {r4, lr}\n"
            "ldr r4, .Lframe\nadd sp, sp, r4\n"
            "bl callee\nmov r3, #0xc4\nlsl r3, r3, #2\nadd sp, sp, r3\n"
            "pop {r4}\npop {r1}\nbx r1\n"
            ".align 2\n.Lframe:\n.word -0x310\n.size caller,.-caller\n"
        )
        self.assertIn(".save {r4, lr}\n.pad #784", annotate(source))
        with self.assertRaisesRegex(ValueError, "dynamically"):
            annotate(source.replace("#0xc4", "#0xc3"))
        with self.assertRaisesRegex(ValueError, "dynamically"):
            annotate(source.replace(".word -0x310", ".word external_size"))

    def test_exception_markers_generate_catch_all_lsda(self):
        source = (
            ".thumb_func\ncaller:\n"
            "\tpush\t{lr}\n"
            ".Lsn_eh_op_sn_sym_caller_12_handler_7_start:\n"
            "\tbl\tmay_throw\n"
            ".Lsn_eh_op_sn_sym_caller_12_handler_7_end:\n"
            "\tpop\t{r1}\n"
            "\tbx\tr1\n"
            ".Lsn_eh_landing_sn_sym_caller_7:\n"
            "\tpop\t{r1}\n"
            "\tbx\tr1\n"
            ".size caller,.Lend-caller\n"
        )
        result = annotate(source)
        self.assertIn(".personality scalanative_personality", result)
        self.assertIn(".handlerdata", result)
        self.assertIn(".byte 0xff\n\t.byte 0xff\n\t.byte 1", result)
        self.assertIn(
            ".uleb128 .Lsn_eh_op_sn_sym_caller_12_handler_7_start-caller",
            result,
        )
        self.assertIn(".uleb128 .Lsn_eh_landing_sn_sym_caller_7-caller", result)
        self.assertIn(".uleb128 0\n.Lsn_lsda_", result)


    def test_stack_snapshot_in_r7_is_not_a_frame_pointer(self):
        source = ".thumb_func\nsnapshot:\npush {r7, lr}\n@ sn_xlat_stack_snapshot\nmov r7, sp\npop {r7}\npop {r0}\nbx r0\n.size snapshot, .-snapshot\n"
        result = annotate(source)
        self.assertIn(".save {r7, lr}", result)
        self.assertNotIn(".setfp", result)


if __name__ == "__main__":
    unittest.main()
