package wacc.backend.integration

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import wacc.*

/**
 * Utility object responsible for parsing integration test metadata files
 *
 * Test files are structured using commented header annotations (e.g `# Input:`,
 * `# Output:`) followed by the WACC program source. This parser extracts
 *   - A textual description
 *   - Optional input lines
 *   - Optional expected output lines
 *
 * and puts them into a [[WaccTestMetadata]] instance
 */
object TestParser {

    /**
     * Parse metadata from a WACC integration test file
     * 
     * Each test file has the format:
     *  # <description>
     *  # Input: <input>
     *  
     *  # Output:
     *  # <output line 1>
     *  # <output line 2>
     *  # ...
     *  #
     * 
     *  # Program:
     *  <wacc source>
     *
     * @param file the test file to parse metadata from
     * @return [[WaccTestMetadata]] populated from the file contents.
     *         If `Input` or `Output` headers are not present,
     *         the corresponding fields are empty.
     */
    def parseMetadata(file: Path): WaccTestMetadata = {
        val lines = Files.readAllLines(file).asScala.toVector
       
        val description = lines.find(_.startsWith("#")).getOrElse("").stripPrefix("# ").trim
        
        val input: List[String] = parseHeader("# Input:", lines)
        val output: List[String] = parseHeader("# Output:", lines)
        val exitCode: Option[Int] = parseHeader("# Exit:", lines) match {
            case Nil => None
            case head::_ => Some(head.toInt)
        }

        WaccTestMetadata(file, description, input, output.mkString("\n"), exitCode)
    }

    /** Remove the leading `#` comment marker from a line */
    private def stripCommentLine(l: String): String = {
        if (l.startsWith("#")) l.drop(1).stripLeading()
        else l
    }

    /** Check whether a trimmed line starts with a given header string */
    private def startsWithHeader(l: String, header: String): Boolean = l.trim.startsWith(header)

    /**
     * Find the index of a header line within the file.
     *
     * @param header the header string to find
     * @param lines the file contents
     * @return the index of the first line starting with the header,
     *         or -1 if not present
     */
    private def headerIndex(header: String, lines: Vector[String]): Int 
        = lines.indexWhere(l =>  l.trim.startsWith(header))

    /**
     * Read the comment block associated with the header
     * 
     * For example, if called with `"# Output:"` and the file contains:
     *   # Output: true
     *   # false
     *   # true
     *   # false
     *   #
     * 
     * this function returns Vector("true", "false", "true", "false")
     * 
     * @param header the header whose associated block should be read
     * @param lines the full file contents
     * 
     * @return the extracted comment lines (without leading `#`)
     */ 
    private def readCommentBlockAfterHeader(
        header: String, lines: Vector[String]
    ): Vector[String] = {
        val i = headerIndex(header, lines)
        if (i < 0) Vector.empty
        else {
            val after = lines.drop(i + 1)
            after
                .takeWhile { l =>
                    val t = l.trim
                    t.nonEmpty && 
                    !startsWithHeader(t, "# Input:") && 
                    !startsWithHeader(t, "# Output:") &&
                    !startsWithHeader(t, "# Program:")
                }
                .filter(_.startsWith("#"))
                .map(stripCommentLine)
        }
    }

    /**
      * Parse all metadata lines associated with a specific header 
      * This function handles:
      *  - Inline metadata on the header line itself (e.g. `# Input: 42`)
      *  - Following comment block lines
      *
      * @param header the header string to parse (e.g. `# Input:`)
      * @param lines the file contents
      * @return a list of trimmed, non-empty metadata lines
      *         Returns `Nil` if the header is not present
      */
    private def parseHeader(header: String, lines: Vector[String]): List[String] = {
        val i = headerIndex(header, lines)
        if (i < 0) Nil
        else {
            val headerLine = lines(i)
            val inline = headerLine.drop(headerLine.indexOf(header) + header.length).trim
            val rest = readCommentBlockAfterHeader(header, lines)
            val all = (if (inline.nonEmpty) Vector(inline) else Vector.empty) ++ rest
            all.map(_.trim).filter(_.nonEmpty).toList
        }
    }

}
