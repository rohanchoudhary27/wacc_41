package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

import wacc.frontend._

/** Unit tests for ImportResolver: */
class ImportResolverTests extends AnyFlatSpec with Matchers {

  /** Create a temporary directory that is deleted on JVM shutdown. */
  private def mkTempDir(): Path = {
    val dir = Files.createTempDirectory("import-resolver-test-")
    dir.toFile.deleteOnExit()
    dir
  }

  /** Write a `.wacc` file in `dir` at the relative path `rel`. */
  private def writeWacc(dir: Path, rel: String, content: String): Path = {
    val target = dir.resolve(rel)
    Files.createDirectories(target.getParent)
    Files.write(target, content.getBytes(StandardCharsets.UTF_8))
    target.toFile.deleteOnExit()
    target
  }

  /** A minimal valid module body (no functions, trivial main). */
  private val emptyModule: String =
    "begin skip end"

  /** Build a module with one or more int functions. */
  private def moduleWithFuncs(funcNames: String*): String = {
    val defs = funcNames
      .map(n => s"int $n() is return 0 end")
      .mkString("\n")
    // WACC grammar: begin <funcs> <stmt> end
    s"begin\n$defs\nskip\nend"
  }

  /** A dummy `SourcePos` used throughout the tests. */
  private val dummyPos: SourcePos = (1, 1)

  /** Build a `ModuleImport` node at `dummyPos`. */
  private def modImport(path: String, alias: Option[String] = None): ModuleImport[SyntacticData] =
    ModuleImport(path, alias, SyntacticData.Pos(dummyPos))

  /** Build a `FromImport` node at `dummyPos`. */
  private def fromImport(path: String, func: String): FromImport[SyntacticData] =
    FromImport(path, func, SyntacticData.Pos(dummyPos))

  // -------------------------------------------------------------------------
  // resolveModulePath
  // -------------------------------------------------------------------------

  "resolveModulePath" should "resolve a simple module name to <sourceDir>/name.wacc" in {
    val dir    = mkTempDir()
    val result = ImportResolver.resolveModulePath("math", dir)
    result shouldBe dir.resolve("math.wacc")
  }

  it should "convert dots to directory separators" in {
    val dir    = mkTempDir()
    val result = ImportResolver.resolveModulePath("utils.array", dir)
    result shouldBe dir.resolve("utils/array.wacc")
  }

  it should "handle a three-segment path" in {
    val dir    = mkTempDir()
    val result = ImportResolver.resolveModulePath("a.b.c", dir)
    result shouldBe dir.resolve("a/b/c.wacc")
  }

  it should "return a /stdlib/ path for stdlib modules (via wacc. prefix)" in {
    val dir    = mkTempDir()
    val result = ImportResolver.resolveModulePath("wacc.math", dir)
    result.toString should startWith("/stdlib/")
    result.toString should endWith("math.wacc")
  }

  // -------------------------------------------------------------------------
  // resolveAll – happy path: single module
  // -------------------------------------------------------------------------

  "resolveAll" should "resolve a single module and qualify its functions" in {
    val dir = mkTempDir()
    writeWacc(dir, "math.wacc", moduleWithFuncs("abs", "max"))

    val imports = List(modImport("math"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get
    modules should have length 1
    val m = modules.head
    m.qualifier shouldBe "math"
    m.funcs.map(_.ident.name) should contain allOf ("math.abs", "math.max")
  }

  it should "apply an alias when one is given" in {
    val dir = mkTempDir()
    writeWacc(dir, "utils.wacc", moduleWithFuncs("getLen"))

    val imports = List(modImport("utils", Some("u")))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get
    modules.head.qualifier shouldBe "u"
    modules.head.funcs.map(_.ident.name) should contain("u.getLen")
  }

  it should "handle a module with no functions (empty module)" in {
    val dir = mkTempDir()
    writeWacc(dir, "empty.wacc", emptyModule)

    val imports = List(modImport("empty"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get
    modules should have length 1
    modules.head.funcs shouldBe empty
  }

  // -------------------------------------------------------------------------
  // resolveAll – from imports
  // -------------------------------------------------------------------------

  it should "resolve a from-import and keep only the named function" in {
    val dir = mkTempDir()
    writeWacc(dir, "math.wacc", moduleWithFuncs("abs", "max", "min"))

    val imports = List(fromImport("math", "abs"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get
    modules should have length 1
    // from-import: name kept as "abs", no qualification
    modules.head.funcs.map(_.ident.name) should contain only "abs"
  }

  it should "return FuncNotFound when the named function does not exist" in {
    val dir = mkTempDir()
    writeWacc(dir, "math.wacc", moduleWithFuncs("abs"))

    val imports = List(fromImport("math", "nonexistent"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("left")
    val errors = result.left.toOption.get
    errors should have length 1
    errors.head shouldBe a[FuncNotFound]
    errors.head match {
      case FuncNotFound(mp, fn, _) =>
        mp shouldBe "math"
        fn shouldBe "nonexistent"
      case other => fail(s"Expected FuncNotFound, got $other")
    }
  }

  // -------------------------------------------------------------------------
  // resolveAll – file not found
  // -------------------------------------------------------------------------

  it should "return FileNotFound for a missing module" in {
    val dir = mkTempDir()

    val imports = List(modImport("missing"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("left")
    val errors = result.left.toOption.get
    errors should have length 1
    errors.head shouldBe a[FileNotFound]
    errors.head match {
      case FileNotFound(mp, _, _) => mp shouldBe "missing"
      case other                  => fail(s"Expected FileNotFound, got $other")
    }
  }

  it should "accumulate multiple FileNotFound errors" in {
    val dir = mkTempDir()

    val imports = List(modImport("a"), modImport("b"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("left")
    result.left.toOption.get should have length 2
  }

  /* resolveAll – cycle detection */
  it should "detect a direct cycle (A imports B, B imports A)" in {
    val dir = mkTempDir()
    writeWacc(dir, "a.wacc", "import b\nbegin skip end")
    writeWacc(dir, "b.wacc", "import a\nbegin skip end")

    val aPath = dir.resolve("a.wacc").toRealPath()
    // Simulate the resolver being called with `a.wacc` already on the stack
    val imports = List(modImport("b"))
    val result  = ImportResolver.resolveAll(imports, dir, visited = Set(aPath))

    result should be a Symbol("left")
    val errors = result.left.toOption.get
    errors.head shouldBe a[CyclicImport]
  }

  it should "detect a self-import" in {
    val dir = mkTempDir()
    writeWacc(dir, "self.wacc", "import self\nbegin skip end")

    val selfPath = dir.resolve("self.wacc").toRealPath()
    val imports  = List(modImport("self"))
    val result   = ImportResolver.resolveAll(imports, dir, visited = Set(selfPath))

    result should be a Symbol("left")
    result.left.toOption.get.head shouldBe a[CyclicImport]
  }

  it should "include a shared dependency only once (diamond pattern)" in {
    val dir = mkTempDir()
    // common.wacc has one function
    writeWacc(dir, "common.wacc", moduleWithFuncs("shared"))
    // a.wacc imports common, b.wacc imports common
    writeWacc(dir, "a.wacc",      "import common\nbegin skip end")
    writeWacc(dir, "b.wacc",      "import common\nbegin skip end")

    // main imports a and b
    val imports = List(modImport("a"), modImport("b"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get

    // "common" should appear exactly once despite being reachable from both a and b.
    val commonModules = modules.filter(_.qualifier == "common")
    commonModules should have length 1
  }

  it should "resolve transitive imports (main → mid → base)" in {
    val dir = mkTempDir()
    writeWacc(dir, "base.wacc", moduleWithFuncs("baseFunc"))
    writeWacc(dir, "mid.wacc",  s"import base\n${moduleWithFuncs("midFunc")}")

    val imports = List(modImport("mid"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules    = result.toOption.get
    val allQuals   = modules.map(_.qualifier)
    allQuals should contain("mid")
    allQuals should contain("base")
  }

  it should "resolve a subdirectory import (dot → path separator)" in {
    val dir = mkTempDir()
    writeWacc(dir, "utils/array.wacc", moduleWithFuncs("arrayLen"))

    val imports = List(modImport("utils.array"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val modules = result.toOption.get
    modules.head.qualifier           shouldBe "array"
    modules.head.funcs.head.ident.name shouldBe "array.arrayLen"
  }

  it should "resolve multiple independent imports and collect all functions" in {
    val dir = mkTempDir()
    writeWacc(dir, "a.wacc", moduleWithFuncs("fa"))
    writeWacc(dir, "b.wacc", moduleWithFuncs("fb"))

    val imports = List(modImport("a"), modImport("b"))
    val result  = ImportResolver.resolveAll(imports, dir)

    result should be a Symbol("right")
    val funcs = result.toOption.get.flatMap(_.funcs).map(_.ident.name)
    funcs should contain("a.fa")
    funcs should contain("b.fb")
  }
}
