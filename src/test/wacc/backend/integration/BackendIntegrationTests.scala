package wacc.backend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.StreamHasToScala
import scala.jdk.CollectionConverters.*
import scala.sys.process.*

import wacc.*
import wacc.ExitCodes._

/**
 * Represents how to handle test WACC programs
 */
enum BackendTestMode {
    case Compile   /** Compiles and produces an output `.s` assembly file */
    case Run       /** Compiles, assembles and links the `.s` file, and runs it */
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
    executionMode: BackendTestMode /* how we handle the test */
)

object BackendAdapter extends AnyFlatSpec {
    private val targetArchitecture = Files.readString(Paths.get("wacc.target")).trim
    private val outputDir = Paths.get("testOut")

    /**
     * Uses the backend pipeline to compile the code into an assembly file, then assembles and
     * links the assembly to the target architecture, then runs it with the emulator
     * 
     * @param metadata The test metadata which includes any stdin and stdout
     * 
     * @return `(String, Int)` A pair containing the output of the test file (assembled 
     *          and emulated), and the exit code of the pipeline
     */ 
    def run(metadata: WaccTestMetadata): (String, Int) = {
        if (!Files.exists(outputDir)) Files.createDirectories(outputDir)
        assume(
            targetArchitecture.equals("aarch64"), 
            s"Expected aarch64 architecture, found ${targetArchitecture}"
        )
        val baseName = metadata.path.getFileName.toString.stripSuffix(".wacc")
        val asmFile = outputDir.resolve(s"$baseName.s")
        val exeFile = outputDir.resolve(baseName)
        // this is placeholder and as we progress let's add our WACC pipeline
        // to here as well. This should roughly do:
        //  1. Call pipeline to generate assembly file
        //  2. If mode is "run":
        //       Cross compile .s to binary with gcc crosscompiler
        //       Execute with qemu-aarch64
        //       Provide metadata input to stdin
        //       Get the stdout and compare with metadata's expectedOutput
        println(s"Doing ${metadata.executionMode} for: ${metadata.path.getFileName}")  
        try {
            /* generate assembly using WACC pipeline */
            val exitCodeOfPipeline = generateAssembly(metadata.path, asmFile)

            metadata.executionMode match {
                case BackendTestMode.Run => 
                    /* assemble and link */
                    val compileStatus = crossCompile(asmFile, exeFile)
                    if (compileStatus != 0) ("Cross-compilation failed", compileStatus)
                    /* emulate and capture output */
                    else emulate(exeFile, metadata.input)
                /* for other execution modes don't return any output and return the exit code */
                case _ => ("", exitCodeOfPipeline) 
            }
        } finally {
            // Files.deleteIfExists(asmFile)
            // Files.deleteIfExists(exeFile)
        }
    }

    /**
     * Calls our WACC pipeline to produce an assembly (.s) file.
     * 
     * @param source the source WACC file path
     * @param destination the output assembly path
     */
    private def generateAssembly(source: Path, destination: Path): Int = {
        // TODO: we should replace this with .compile(source, destination) in the future
        // for now tho write a dummy assembly
        val mockAsm = ".text\n.global main\nmain:\n\tmov w0, #0\n\tret"
        Files.writeString(destination, mockAsm)
        0 // TODO: we return the exit code in the future
    }

    /**
     * Cross-compiles ARM assembly to an executable binary
     * Uses `aarch64-linux-gnu-gcc -o <EXE> -z noexecstack -march=armv8-a <ASM>
     * 
     * @param asmPath the input assembly file path
     * @param exePath the output executable file path
     * 
     * @return exit code of the operation
     */
    private def crossCompile(asmPath: Path, exePath: Path): Int = {
        val cmd = Seq(
            "aarch64-linux-gnu-gcc",
            "-o", exePath.toString,
            "-z", "noexecstack",
            "-march=armv8-a",
            asmPath.toString
        )
        cmd.! // Returns exit code
    }

    /**
     * Runs the executable via qemu and manages IO (passing any stdin)
     * Uses `qemu-aarch64 -L /usr/aarch64-linux-gnu/ <EXE>`
     * 
     * @param exePath input executable path from cross compiling
     * @param inputs list of input lines to pass into the program
     * 
     * @return `(String, Int)` the stdout and exit code from running the program
     */
    private def emulate(exePath: Path, inputs: List[String]): (String, Int) = {
        val stdout = new StringBuilder
        val qemuCmd = Seq("qemu-aarch64", "-L", "/usr/aarch64-linux-gnu", exePath.toString)

        val io = new ProcessIO(
            stdin => {
                val writer = new java.io.PrintWriter(stdin)
                inputs.foreach(writer.println)
                writer.close()
            },
            stdoutStream => {
                val reader = scala.io.Source.fromInputStream(stdoutStream)
                reader.getLines().foreach(l => stdout.append(l).append("\n"))
                reader.close()
            },
            _ => (info("there's a stderr")) // what should we do when there's stderr...
        )

        val exitCode = Process(qemuCmd).run(io).exitValue()
        (stdout.toString().trim, exitCode)
    }
}


/* Backend integration tests:
 * Works similar to the frontend integration tests, but with an extra "mode" config. The
 * configuration file is in the format:
 *     <scope>: "compile" | "run"
 * Also the integration tests go over each wacc file under `examples` and extracts the metadata
 * inside comments. The output is checked against the expected output metadata
*/
final class BackendIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
    /**
     * Checks that `aarch64-linux-gnu-gcc` and `qemu-aarch64` are installed on the environment
     */
    override def beforeAll(): Unit = {
        val tools = Seq("aarch64-linux-gnu-gcc", "qemu-aarch64")
        tools.foreach { tool =>
            val exitCode = Seq("which", tool).!
            if (exitCode != 0) {
                cancel(s"Missing required tool: $tool. Please install it to run backend tests")
            }
        }
    }

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
                = it should s"output ${expectedDisplay} for ${metadata.path.toString()}"
            
            val expectedExitCode = expectedStatus(metadata.path)

            /* Disable test or perform test */
            metadata.executionMode match {
                case BackendTestMode.Skip => testDecl ignore { }
                case BackendTestMode.Run => 
                    testDecl in {
                        /* check that outputs match and check exit code */
                        val (out, exitCode) = BackendAdapter.run(metadata)
                        out shouldBe expectedOut
                        exitCode shouldBe expectedExitCode
                    }
                case BackendTestMode.Compile => 
                    testDecl in {
                        /* just check the exit code */
                        val (_, exitCode) = BackendAdapter.run(metadata)
                        exitCode shouldBe expectedExitCode
                    }
            }
        }
    }

    /**
     * Determines the expected exit code for a WACC file based on its path.
     * examples/valid/  => 0 (all valid programs should return 0)
     * examples/invalid/syntaxErr/ => 100 (programs with syntax errors should return 100)
     * examples/invalid/semanticErr/ => 200 (programs with semantic errors should return 200)
     * examples/invalid/whack/ => 100 (the prologue questions all have syntax errors in them) 
     * otherwise, returns a general compiler failure code.
     * 
     * @param file The path to the WACC source file
     * 
     * @return: the integer exit code defined in WaccFrontend
     */
    private def expectedStatus(file: Path): Int = {

        /* Make the file relative to examples root to get the scope */
        val rel = examplesRoot.relativize(file).toString 

        if (rel.startsWith("valid/")) code(Success)
        else if (rel.startsWith("invalid/syntaxErr/") ||
                 rel.startsWith("invalid/whack/")) code(SyntaxError)
        else if (rel.startsWith("invalid/semanticErr/")) code(SemanticError)
        else code(CompilerFailure)
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

        def stripCommentLine(l: String): String = {
            if (l.startsWith("#")) l.drop(1).stripLeading()
            else l
        }

        def startsWithHeader(l: String, header: String): Boolean = l.trim.startsWith(header)

        def headerIndex(header: String): Int = lines.indexWhere(l =>  l.trim.startsWith(header))

        /**
         * Read the comment block associated with the header
         * 
         * For example, if this function is called as readCommentBlockAfterHeader("# Output:") with
         *   # Output: true
         *   # false
         *   # true
         *   # false
         *   #
         * This function will return ["true", "false", "true", "false"] 
         * 
         * @param header the header line string
         *
         * @return block metadata after header line
         */ 
        def readCommentBlockAfterHeader(header: String): List[String] = {
            val i = headerIndex(header)
            if (i < 0) List()
            else {
                val headerLine = lines(i)
                val firstLine  /* look for inline too */
                    = headerLine.drop(headerLine.indexOf(header) + header.length).trim
                val after = lines.drop(i + 1)
                after.toList
                    .takeWhile { l =>
                        val t = l.trim
                        t.nonEmpty && !startsWithHeader(t, "# Input:") && 
                            !startsWithHeader(t, "# Output:") && !startsWithHeader(t, "# Program:")
                    }
                    .filter(_.startsWith("#"))
                    .map(stripCommentLine)
                val all = (if (firstLine.nonEmpty) List(firstLine) else List()) ++ after
                all.map(_.trim).filter(_.nonEmpty)
            }
        }
       
        val description = lines.find(_.startsWith("#")).getOrElse("").stripPrefix("# ").trim
        
        val input: List[String] = readCommentBlockAfterHeader("# Input:")
        val output: List[String] = readCommentBlockAfterHeader("# Output:")

        val executionMode = if (!isEnabled(file)) {
            BackendTestMode.Skip
        } else {
            val fileRel = examplesRoot.relativize(file).toString
            val scopeKey = scopeFor(file)
            enabledScopes.find { (scope, _) =>
                val normalisedScope = scope.stripSuffix("/")
                fileRel == normalisedScope || scopeKey == normalisedScope || 
                    scopeKey.startsWith(normalisedScope + "/")
            }.map(_._2).getOrElse(BackendTestMode.Skip)
        }
        WaccTestMetadata(
            file, description, input, output.mkString("\n"), executionMode
        )
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