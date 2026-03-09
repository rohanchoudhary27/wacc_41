import wacc.frontend.WaccFrontend
import java.nio.file.Paths
val ast = WaccFrontend.run(Paths.get("AllIntegrationTests/custom/ExtensionsMilestone/Imports/valid/multiImport.wacc"))
println(s"Exit Code: ${ast.exitCode}")
