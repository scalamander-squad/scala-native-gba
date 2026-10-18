package scala.scalanative
package runtime

import scalanative.unsafe._

@extern
object StringConstants {
  // C pointer objects, not callable accessors; extern fields use var syntax.
  var snErrorPrefix: CString = extern
  var snFatalErrorPrefix: CString = extern
}
