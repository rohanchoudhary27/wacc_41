package wacc.backend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.StreamHasToScala
import scala.jdk.CollectionConverters.*

/* Backend integration tests:

Works similar to the frontend integration tests, but with an extra "mode" config. The
configuration file is in the format:

<scope>: "compile" | "run"

Also the integration tests go over each wacc file under `examples` and extracts the metadata
inside comments. The output is checked against the (Expected) output metadata
*/

/**
  * Represents how to handle test WACC programs
  */
enum BackendTestMode {
    case Compile   /** Just compiles and produces an output `.s` assembly file */
    case Run       /** Compiles and outputs a `.s` assembly file and runs it */
    case Skip      /** Skips the test */
}

/**
 * Represents test metadata extracted from test files 
 */
case class WaccTestMetadata(
    path: Path,                 /* path to the file */
    description: String,        /* description, it is the first line of comments usually */
    input: List[String],        /* input list, it is indicated by `# Input: ` */
    expectedOutput: String,     /* expected output as string, it is indicated by `# Output: ` */
    sourceCode: String, // FIXME: don't need
    executionMode: BackendTestMode /* how we handle the test */
)

object BackendDriver {
    def run(metadata: WaccTestMetadata): String = {
        // this is placeholder and as we progress let's add our backend pipeline
        // to here as well. This should roughly do:
        //  1. Call WaccFrontend to get typed AST
        //  2. Generate assembly file
        //  3. If mode is "run":
        //       Cross compile .s to binary with gcc crosscompiler
        //       Execute with qemu-aarch64
        //       Provide metadata input to stdin
        //       Get the stdout and compare with metadata's expectedOutput
        println(s"Doing ${metadata.executionMode} for: ${metadata.path.getFileName}")
        "out"
    }
}

final class BackendIntegrationSpec extends AnyFlatSpec with Matchers {
    /* File location and path info */
    private val examplesRoot = Paths.get("examples")
    private val programExtension = "wacc"
    private val backendTestConfigPath = Paths.get("config", "BackendTestScope.txt")
    
    private val enabledScopes: Map[String, BackendTestMode]
        = readBackendConfig(backendTestConfigPath)

    info(s"enabled : ${enabledScopes}")

    /* Valid and invalid test scopes */
    private val validDir = examplesRoot.resolve("valid")
    private val invalidDir = examplesRoot.resolve("invalid")
     
    /* All the example WACC programs in the examples dir */
    private val allPrograms: Seq[Path] =
        getWaccProgramsUnder(validDir) ++ getWaccProgramsUnder(invalidDir)


    /* Group test file paths based on test categories and extract metadata */
    private val grouped: Map[String, Seq[WaccTestMetadata]] =
        allPrograms.groupBy { f =>

            /* Turn examples/{valid, invalid}/... into {valid, invalid}/... */
            val rel = examplesRoot.relativize(f).toString 

            /* Get the scope name (if the file is "invalid/syntaxError/basic/skpErr.wacc", 
               this gets "invalid/syntaxError") to be the groupBy key */
            rel.split('/').take(2).mkString("/") 
        }.map { (k, v) => { k -> v.map { parseMetadata(_) }.sortBy(_.path) } }

    info(s"${grouped}")

    /* Register each file as a scalatest test case */
    grouped.toSeq.sortBy(_._1).foreach { (groupName, testMetadata) =>
        behavior of s"wacc examples: $groupName"

        testMetadata.foreach { metadata =>

            /* Make the integration test expectation and title */
            val expectedOut = metadata.expectedOutput
            /* Display string for expected output */
            val expectedDisplay = expectedOut.replace("\n", "\\n") match
                case t if t.length <= 10 => t
                case t                  => t.take(10) + "..."

            val testDecl 
                = it should s"return status ${expectedDisplay} for ${metadata.path.toString()}"

            /* Disable test or compile to perform test */
            metadata.executionMode match {
                case BackendTestMode.Skip => testDecl ignore { }
                case _ => 
                    testDecl in {
                        val actualOut = BackendDriver.run(metadata)
                        actualOut shouldBe expectedOut
                    }
            }
        }
    }

    /**
      * Checks if a file should be enabled for an integration test.
      * 
      * @param file: The path to the WACC file
      * @return: True if the file belongs to an enabled scope, false otherwise
      */ 
    private def isEnabled(file: Path): Boolean = {
        if (enabledScopes.isEmpty) false
        else {
            val fileRel = examplesRoot.relativize(file).toString
            val scopeKey = scopeFor(file)
            enabledScopes.exists { (s, _) =>
                val scope = s.stripSuffix("/")
                fileRel == scope || scopeKey == scope || scopeKey.startsWith(scope + "/")
            }
        }
    }


    /**
      * Parse the backend integration test configuration file to find the tests to either
      * compile or compile & run.
      *
      * @param path The path to the backend configuration file
      * @return A map of scope to mode representation. Each key in the map represents an
      *         active test scope, and its corresponding key is `BackendTestMode.Compile` 
      *         or `BackendTestMode.Run`
      */
    private def readBackendConfig(path: Path): Map[String, BackendTestMode] = {
        if (!Files.exists(path)) Map.empty
        Files.readAllLines(path).asScala
            .map(_.takeWhile(_ != '#').trim) /* Treat '#' as a comment */
            .filter(_.nonEmpty)
            .map { line =>                 
                val parts = line.split(":")
                val scope = parts(0).trim
                val configMode: BackendTestMode = parts(1).trim.toLowerCase match {
                    case "compile" => BackendTestMode.Compile
                    case "run"     => BackendTestMode.Run
                }
                scope -> configMode
            }.toMap
    }

    /**
      * Parse metadata from the test file. Each test file has the format:
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
      * @return `WaccTestMetadata` the metadata object populated from the test file. If there are
      *         no `Output` or `Input` annotations in the file, the fields are set to 
      *         empty strings.
      */
    private def parseMetadata(file: Path): WaccTestMetadata = {
        val lines = Files.readAllLines(file).asScala
        val description = lines.find(_.startsWith("#")).getOrElse("").stripPrefix("# ")
        
        val input = lines.filter(_.startsWith("# Input:"))
            .map(_.stripPrefix("# Input:").trim).toList
            
        val output = lines.slice(
            lines.indexWhere(_.startsWith("# Output:")),
            lines.indexWhere(_.startsWith("# Program:"))
        ).filter(l => l.startsWith("#") && !l.contains("Output:"))
         .map(_.stripPrefix("#").trim).mkString("\n")

        // saim in here we need to get the execution mode but the thing is enabledScopes
        // contain the scopes so it may have valid/array/ and the file param for this function
        // is a full file, so we need to resolve it and check that its scope matches this,
        // i am leaving labs now so if you can do this it would be great, just replace the 
        // else branch
        val executionMode 
            = if (!isEnabled(file)) {BackendTestMode.Skip} else { BackendTestMode.Run }
        WaccTestMetadata(file, description, input, output, lines.mkString("\n"), executionMode)
    }


    /**
      * Extracts the parent directory of a file relative to the examples root.
      * Example: "examples/invalid/syntaxErr/basic/skpErr.wacc" -> "invalid/syntaxErr/basic"
      *
      * @param file: The path to the WACC file
      * 
      * @return: A string representation of the parent directory path
      */
    private def scopeFor(file: Path): String = {
        Option(file.getParent) match {
            case Some(parent) => examplesRoot.relativize(parent).toString
            case None => ""
        }
    }

    /**
      * Returns all WACC files under a directory
      * @param dir: The root directory to start the search
      * 
      * @return: A sequence of paths pointing to WACC files
      */
    private def getWaccProgramsUnder(dir: Path): Seq[Path] = {
        if (!Files.isDirectory(dir)) Seq.empty
        else Files.walk(dir).toScala(List)
            .filter(p => Files.isRegularFile(p)) 
            .filter(p => extensionOf(p).contains(programExtension))
    }

    /**
      * Extracts the file extension from a path
      * @param p: The path to extract from
      * @return: An Option containing the extension string or None if no extension exists
      */
    private def extensionOf(p: Path): Option[String] = {
        val name = p.getFileName.toString
        val idx = name.lastIndexOf('.')
        if (idx < 0 || idx == name.length - 1) None else Some(name.substring(idx + 1))
    }
}