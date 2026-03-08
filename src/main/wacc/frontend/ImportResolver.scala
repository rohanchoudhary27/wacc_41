package wacc.frontend

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.collection.mutable

/** Errors that can occur during import resolution. All contain the `SourcePos`
  * of the import declaration that triggered the error, and the error message. */
sealed trait ImportError {
  val pos: SourcePos
  def errMsg: String
}

/** The module file could not be found on the filesystem */
case class FileNotFound(modulePath: String, resolvedPath: Path, pos: SourcePos)
    extends ImportError {
  def errMsg: String = s"Module '$modulePath' not found (looked for '$resolvedPath')"
}

/** An import creates a cycle in the dependency graph */
case class CyclicImport(cycle: List[String], pos: SourcePos) extends ImportError {
  def errMsg: String = s"Circular import detected: ${cycle.mkString(" -> ")}"
}

/** A `from M import F` tries to import function `F` that does not appear in module `M` */
case class FuncNotFound(modulePath: String, funcName: String, pos: SourcePos)
    extends ImportError {
  def errMsg: String = s"Function '$funcName' not found in module '$modulePath'"
}

/** A successfully resolved module.
  *
  * @param path           The path of the module file.
  * @param qualifier      The effective name used to qualify function calls
  *                       (the alias if one was given, otherwise the last
  *                       segment of the dot-separated module path).
  * @param funcs          All `Func` nodes parsed from the module, with their
  *                       `ident.name` already prefixed by `qualifier`.
  */
case class ResolvedModule(
    path: Path,
    qualifier: String,
    funcs: List[Func[SyntacticData]]
)

/** Resolves import declarations into lists of `Func` nodes ready to be merged into 
  * the main `Program` AST.
  *
  * Path resolution rules:
  * `import xyz`       -> `<sourceDir>/xyz.wacc`
  * `import utils.xyz` -> `<sourceDir>/utils/xyz.wacc`
  * `import wacc.xyz`  -> bundled `/stdlib/xyz.wacc`
  */
object ImportResolver {

  /** Resolve a sequence of import declarations, recursively resolving any
    * nested imports found inside imported files.
    *
    * @param imports    The import list of the file currently being processed.
    * @param sourceDir  Directory of the file that owns these imports.
    * @param visited    Paths of all files in the current DFS stack
    * @param resolved   Paths of already-resolved modules across the
    *                   whole compilation (used to avoid diamond duplication).
    * @return           `Right` with a flat list of `ResolvedModule` (one per
    *                   unique file discovered), or `Left` with the list of
    *                   errors encountered during resolution.
    */
  def resolveAll(
      imports: List[Import[SyntacticData]],
      sourceDir: Path,
      visited: Set[Path] = Set.empty,
      resolved: mutable.Set[Path] = mutable.HashSet.empty
  ): Either[List[ImportError], List[ResolvedModule]] = {

    val errors  = mutable.ListBuffer[ImportError]()
    val modules = mutable.ListBuffer[ResolvedModule]()

    for (imp <- imports) {
      resolveOne(imp, sourceDir, visited, resolved) match {
        case Left(errs) => errors ++= errs
        case Right(ms)  => modules ++= ms
      }
    }

    if (errors.nonEmpty) Left(errors.toList)
    else Right(modules.toList)
  }

  /** Convert a dot-separated module path string to a filesystem `Path`
    * relative to `sourceDir`.
    *
    * @return The resolved path.
    */
  def resolveModulePath(modulePath: String, sourceDir: Path): Path = {
    val segments = modulePath.split("\\.").toList
    segments match {
      case "wacc" :: rest =>
        // Bundled stdlib resource. Actual reading is done via classloader in readSource().
        Paths.get(s"/stdlib/${rest.mkString("/")}.wacc")
      case _ =>
        // Relative to the importing file's directory.
        val relPath = (segments.init :+ (segments.last + ".wacc")).mkString("/")
        sourceDir.resolve(relPath)
    }
  }

  /** Resolve a single import declaration. May recurse for transitive imports. */
  private def resolveOne(
      imp: Import[SyntacticData],
      sourceDir: Path,
      visited: Set[Path],
      resolved: mutable.Set[Path]
  ): Either[List[ImportError], List[ResolvedModule]] = imp match {

    case ModuleImport(modulePath, aliasOpt, info) =>
      val qualifier = aliasOpt.getOrElse(modulePath.split("\\.").last)
      val absPath   = resolveModulePath(modulePath, sourceDir)
      resolveFile(modulePath, absPath, qualifier, fullImport = true, None, info.pos, 
      visited, resolved)

    case FromImport(modulePath, funcName, info) =>
      val qualifier = funcName  /* from-imports are unqualified: call f(…) */
      val absPath   = resolveModulePath(modulePath, sourceDir)
      resolveFile(modulePath, absPath, qualifier, fullImport = false, Some(funcName), info.pos, visited, resolved)
  }

  /** Core resolution logic for a single file.
    *
    * @param modulePath   Original dot-separated module path (for error messages).
    * @param filePath     The resolved filesystem path.
    * @param qualifier    Effective prefix/name for functions from this module.
    * @param fullImport   If true: take all funcs and qualify with `qualifier`.
    *                     If false: take only `onlyFunc` and register as-is.
    * @param onlyFunc     Non-None when `fullImport = false` (from import).
    */
  private def resolveFile(
      modulePath: String,
      filePath: Path,
      qualifier: String,
      fullImport: Boolean,
      onlyFunc: Option[String],
      pos: SourcePos,
      visited: Set[Path],
      resolved: mutable.Set[Path]
  ): Either[List[ImportError], List[ResolvedModule]] =
    readSource(filePath) match {
      case None =>
        Left(List(FileNotFound(modulePath, filePath, pos)))

      case Some(sourceCode) =>
        /* Get the canonical path so we can detect cycles and diamonds: */
        val canonical: Path =
          try filePath.toRealPath()
          catch { case _: Exception => filePath.toAbsolutePath.normalize() }

        if (visited.contains(canonical)) {
          /* Cycle detection */
          val cycleList = 
            visited.toList.map(_.getFileName.toString) :+ canonical.getFileName.toString
          Left(List(CyclicImport(cycleList, pos)))
        } else if (resolved.contains(canonical)) {
          /* Diamond deduplication — already resolved, skip silently */
          Right(List.empty)
        } else {
          /* Parse the module file: */
          parseModuleFuncs(sourceCode) match {
            case Left(_) =>
              Left(List(FileNotFound(modulePath, filePath, pos))) // treat parse error as unresolvable

            case Right(rawFuncs) =>
              /* Verify the function name actually exists (from-imports only): */
              val funcCheckError: Option[Left[List[ImportError], Nothing]] = onlyFunc match {
                case Some(fname) if !rawFuncs.exists(_.ident.name == fname) =>
                  Some(Left(List(FuncNotFound(modulePath, fname, pos))))
                case _ => None
              }

              funcCheckError.getOrElse {
                /* Qualify function names: */
                val qualifiedFuncs: List[Func[SyntacticData]] = {
                  val selected = onlyFunc match {
                    case Some(fname) => rawFuncs.filter(_.ident.name == fname)
                    case None        => rawFuncs
                  }
                  if (fullImport)
                    selected.map { f =>
                      f.copy(ident = f.ident.copy(name = s"$qualifier.${f.ident.name}"))
                    }
                  else
                    selected // from-import: keep name as-is
                }

                /* Mark this module as resolved (before recursing to handle cycles) */
                resolved += canonical
                val newVisited = visited + canonical

                /* Recursively resolve nested imports in the module */
                val nestedImports: List[Import[SyntacticData]] = parseModuleImports(sourceCode)
                val moduleSourceDir = Option(filePath.getParent).getOrElse(Paths.get("."))

                resolveAll(nestedImports, moduleSourceDir, newVisited, resolved) match {
                  case Left(errs)    => Left(errs)
                  case Right(nested) => Right(nested :+ ResolvedModule(canonical, qualifier, qualifiedFuncs))
                }
              }
          }
        }
    }

  /** Read the source of a module file. */
  private def readSource(filePath: Path): Option[String] = {
    val pathStr = filePath.toString
    if (pathStr.startsWith("/stdlib/")) {
      // Classloader resource
      Option(getClass.getResourceAsStream(pathStr)).map { resource =>
        val src = Source.fromInputStream(resource)
        try src.mkString
        finally src.close()
      }
    } else if (Files.exists(filePath)) {
      val src = Source.fromFile(filePath.toFile)
      try Some(src.mkString)
      finally src.close()
    } else {
      None
    }
  }

  /** Parse a WACC source string and return just the `Func` nodes. */
  private def parseModuleFuncs(source: String): Either[String, List[Func[SyntacticData]]] =
    parser.parse(source) match {
      case Right(Program(_, funcs, _, _)) => Right(funcs)
      case Left(err)                      => Left(err.toString)
    }

  /** Parse a WACC source string and return just the `Import` nodes. */
  private def parseModuleImports(source: String): List[Import[SyntacticData]] =
    parser.parse(source) match {
      case Right(Program(imports, _, _, _)) => imports
      case Left(_)                          => Nil
    }
}
