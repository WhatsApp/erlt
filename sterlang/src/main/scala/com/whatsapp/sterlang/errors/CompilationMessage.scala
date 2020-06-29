package com.whatsapp.sterlang.errors

import com.whatsapp.sterlang.Pos.Located

object CompilationMessage {
  private val LINE_WIDTH = 80

  /** A message for the user regarding their code. */
  sealed trait Message {

    /** A general description (i.e. the title) of the issue. */
    protected def category: String

    /** Name of the file or description of the source that contains the issue. */
    protected def source: String

    /** A detailed description of the issue. */
    protected def description: String

    /** A list of locations relevant to this issue.
      *
      * Each location has an associated message.
      */
    protected def relevantLocations: List[Located[String]]

    /** A hint that can help resolve the issue. */
    protected def hint: Option[String] = Option.empty

    final override def toString: String = {
      val header = {
        val title = s"-- ${category.toUpperCase()} -"
        val source = " " + this.source
        val paddingLength = (LINE_WIDTH - title.length - source.length).min(0)
        val padding = "-" * paddingLength
        title + padding + source
      }

      // TODO: show the relevant portion in the source file instead of just the line number
      def showLocation(message: Located[String]): String =
        // TODO: [[SP]].toString does not do the right thing.
        s"${message.sourceLocation.toString}: ${message.value}"

      val parts = List(header, description) ++ relevantLocations.map(showLocation) ++ hint.toList
      parts.mkString("\n\n")
    }
  }

  /** Superclass of all errors caused by bad user input to the compiler.
    *
    * Errors caused by bugs in the compiler do not belong here.
    * They should instead raise a (subclass of) [[RuntimeException]].
    */
  abstract class CompilationError extends Error() with Message

  /** Superclass of all warnings issued to the user for problematic code. */
  abstract class CompilationWarning extends Error() with Message
}
