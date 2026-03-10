package wacc.frontend.integration

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Metadata extracted from an import integration test file */
case class ImportTestMetadata(
    path: Path,
    description: String,
    expectedError: String,
    expectedExitCode: Option[Int]
)

/** Utility object to parse integration test metadata for the frontend import resolution tests */
object ImportTestParser {

    def parseMetadata(file: Path): ImportTestMetadata = {
        val lines = Files.readAllLines(file).asScala.toVector

        val description = lines.find(_.startsWith("#")).getOrElse("").stripPrefix("# ").trim

        val error    = parseHeader("# Error:", lines)
        val exitCode = parseHeader("# Exit:", lines) match {
            case Nil       => None
            case head :: _ => Some(head.toInt)
        }

        ImportTestMetadata(
            path = file,
            description = description,
            expectedError = error.mkString("\n"),
            expectedExitCode = exitCode
        )
    }

    private def stripCommentLine(l: String): String = {
        if (l.startsWith("#")) l.drop(1).stripLeading()
        else l
    }

    private def startsWithHeader(l: String, header: String): Boolean =
        l.trim.startsWith(header)

    private def headerIndex(header: String, lines: Vector[String]): Int =
        lines.indexWhere(l => l.trim.startsWith(header))

    private def readCommentBlockAfterHeader(header: String, lines: Vector[String]): Vector[String] = {
        val i = headerIndex(header, lines)
        if (i < 0) Vector.empty
        else {
            val after = lines.drop(i + 1)
            after
                .takeWhile { l =>
                    val t = l.trim
                    t.nonEmpty &&
                    !startsWithHeader(t, "# Output:") &&
                    !startsWithHeader(t, "# Error:") &&
                    !startsWithHeader(t, "# Exit:") &&
                    !startsWithHeader(t, "# Program:")
                }
                .filter(_.startsWith("#"))
                .map(stripCommentLine)
        }
    }

    private def parseHeader(header: String, lines: Vector[String]): List[String] = {
        val i = headerIndex(header, lines)
        if (i < 0) Nil
        else {
            val headerLine = lines(i)
            val inline     = headerLine.drop(headerLine.indexOf(header) + header.length).trim
            val rest       = readCommentBlockAfterHeader(header, lines)
            val all        = (if (inline.nonEmpty) Vector(inline) else Vector.empty) ++ rest
            all.map(_.trim).filter(_.nonEmpty).toList
        }
    }
}