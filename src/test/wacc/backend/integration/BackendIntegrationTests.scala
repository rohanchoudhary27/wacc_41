package wacc.backend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfterAll
import java.nio.file.{Files, Path, Paths}

import scala.sys.process.stringSeqToProcess
import wacc.*
import wacc.ExitCodes.*
import wacc.integration.WaccIntegrationTestBase
import scala.sys.process.*

/**
 * Represents test metadata extracted from test files 
 */
case class WaccTestMetadata(
    path: Path,                   /* path to the file */
    description: String,          /* description, it is the first line of comments usually */
    input: List[String],          /* input list, it is indicated by `# Input: ` */
    expectedOutput: String,       /* expected output as string, it is indicated by `# Output: ` */
    expectedExitCode: Option[Int] /* expected exit code */
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
        //  2. Cross compile .s to binary with gcc crosscompiler
        //     Execute with qemu-aarch64
        //     Provide metadata input to stdin
        //     Get the stdout and compare with metadata's expectedOutput
        // println(s"Doing ${metadata} for: ${metadata.path.getFileName}")  
        try {
            /* generate assembly using WACC pipeline */
            val compileExitCode = generateAssembly(metadata.path, asmFile)
            
            if (compileExitCode != 0) {
                /* Compilation failed (syntax/semantic errors) - no code emitted */
                ("", compileExitCode)
            } else {
                /* assemble and link */
                val crossCompileStatus = crossCompile(asmFile, exeFile)
                if (crossCompileStatus != 0) ("Cross-compilation failed", crossCompileStatus)
                /* emulate and capture output */
                else emulate(exeFile, metadata.input)
            }
        } finally {
            Files.deleteIfExists(asmFile)
            Files.deleteIfExists(exeFile)
        }
    }

    /**
     * Calls our WACC pipeline to produce an assembly (.s) file.
     * 
     * @param source the source WACC file path
     * @param destination the output assembly path
     */
    private def generateAssembly(source: Path, destination: Path): Int = {
        val frontendRes = wacc.frontend.WaccFrontend.run(source)

        frontendRes.exitCode match {
            case CompilerSuccess =>
                val backendRes = wacc.backend.WaccBackend.run(
                    destination.toString,
                    frontendRes.typedAST.get
                )
                code(backendRes.exitCode)

            case failureCode => 
                code(failureCode)
        }
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
        
        // Create a logger that prints stderr to the console
        val logger = ProcessLogger(
            (o: String) => println(s"[GCC OUT]: $o"),
            (e: String) => println(s"[GCC ERR]: $e")
        )
        
        // Run with the logger
        cmd ! logger 
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
        val stderr = new StringBuilder
        val qemuCmd = Seq("qemu-aarch64", "-L", "/usr/aarch64-linux-gnu", exePath.toString)

        val io = new ProcessIO(
            stdin => {
                try {
                    val writer = new java.io.PrintWriter(stdin)
                    inputs.foreach { line =>
                        writer.println(line)
                    }
                    writer.flush()
                } finally {
                    stdin.close()
                }
            },
            stdoutStream => {
                try {
                    val reader = scala.io.Source.fromInputStream(stdoutStream)
                    reader.getLines().foreach(l => stdout.append(l).append("\n"))
                } finally {
                    stdoutStream.close()
                }
            },
            stderrStream => {
                try {
                    val reader = scala.io.Source.fromInputStream(stderrStream)
                    val errors = reader.mkString
                    if (errors.nonEmpty) {
                        stderr.append(errors)
                        info(s"stderr output: $errors")
                    }
                } finally {
                    stderrStream.close()
                }
            }
        )

        val exitCode = Process(qemuCmd).run(io).exitValue()
        (stdout.toString().trim, exitCode)
    }
}

/* configuration file path */
private val BACKEND_INTEGRATION_TEST_CONFIGURATION: Path 
    = Paths.get("config", "BackendTestScope.txt")

/* Backend integration tests:
 * Works similar to the frontend integration tests.
 * Also the integration tests go over each wacc file under `examples` and extracts the metadata
 * inside comments. The output is checked against the expected output metadata
*/
final class BackendIntegrationSpec extends WaccIntegrationTestBase(
    BACKEND_INTEGRATION_TEST_CONFIGURATION
) with BeforeAndAfterAll {

    /* Text length of expected output to display before a ... */
    val expectedOutLength = 10

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

    /* Group test file paths based on test categories and extract metadata */
    private val grouped: Map[String, Seq[WaccTestMetadata]] 
        = groupTestsBy {
            (k, v) => { k -> v.map { TestParser.parseMetadata(_) }.sortBy(_.path) }
        }

    /* Register each file as a scalatest test case */
    grouped.toSeq.sortBy(_._1).foreach { (groupName, testMetadata) =>
        behavior of s"wacc examples: $groupName"

        testMetadata.foreach { metadata =>
            /* Make the integration test expectation and title */
            val expectedOut = metadata.expectedOutput
            
            /* Display string for expected output */
            val expectedDisplay = expectedOut.replace("\n", "\\n") match
                case t if t.length <= expectedOutLength => t
                case t                  => t.take(expectedOutLength) + "..."
            
            val testDecl 
                = it should s"output ${expectedDisplay} for ${metadata.path.toString()}"
            
            /** Some test files have their expected exit codes embedded as a hear, so
                if it exists use that, else infer the exit code from its directory */
            val expectedExitCode = metadata.expectedExitCode.getOrElse(
                expectedStatus(metadata.path)
            )

            /* Disable test or compile to perform test */
            if (!isEnabled(metadata.path)) {
                testDecl ignore { }
            } else {
                testDecl in {
                    
                    val (out, exitCode) = BackendAdapter.run(metadata)

                    /* replace any address with `#addrs#` */
                    val (replacedOut, replacedExpectedOut) 
                                = OutputSpec.replaceAddr(out, expectedOut)

                    /* CHECK for any special directives */

                    OutputSpec.directive(expectedOut) match {
                        case Some((OutputDirective.SyntaxError, _)) =>
                            /* Don't compare stdout just check the right exit code */
                            exitCode shouldBe code(ExitCodes.SyntaxError)

                        case Some((OutputDirective.SemanticError, _)) =>
                            /* Don't compare stdout just check the right exit code */
                            exitCode shouldBe code(ExitCodes.SemanticError)

                        case Some((OutputDirective.RuntimeError, _)) =>
                            /* Runtime errors are not compile time so exit should be nonzero.
                               Also if there's a runtime error the exit code is specified is
                               with the `# Exit:` header, so the parsed metadata will contain it
                               and we can compare the exit codes */
                            exitCode should not be 0
                            exitCode shouldBe expectedExitCode

                        case None =>
                            replacedOut shouldBe replacedExpectedOut
                            exitCode shouldBe expectedExitCode
                    }
                }
            }
        }
    }
}