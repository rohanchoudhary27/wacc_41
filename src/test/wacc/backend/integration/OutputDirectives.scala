package wacc.backend.integration

enum OutputDirective(val repr: String) {
    /** Marker used in examples to indicate a syntax error is expected. */
    case SyntaxError extends OutputDirective("#syntax_error#")

    /** Marker used in examples to indicate a semantic error is expected. */
    case SemanticError extends OutputDirective("#semantic_error#")

    /** Marker used in examples to indicate a runtime error is expected. */
    case RuntimeError extends OutputDirective("#runtime_error#")
}

/**
  * Utility for parsing special directives embedded inside `# Output:` blocks
  * of WACC example test files
  *
  * It looks for special compiler/runtime directive tokens: `#syntax_error#`, `#semantic_error#`
  * `#runtime_error#`, and address placeholders (`#addrs#`) for printing addresses.
  */
object OutputSpec {
    // A regex to match addresses in output. This is kinda biased but I guess
    // we will return addresses in this pattern
    private val AddrRegex = "0x[0-9a-fA-F]+".r

    /**
      * If the expected output contains `#addrs#`, replaces all hexadecimal 
      * addresses in the actual output with `#addrs#`
      *
      * @param actual   the program's actual stdout
      * @param expected the expected stdout from the example metadata
      * @return tuple `(replaced actual, expected)`
      */
    def replaceAddr(actual: String, expected: String): (String, String) = {

        // Only replace addresses if the expected output contains the placeholder.
        if (expected.contains("#addrs#")) {
            val actualReplaced = AddrRegex.replaceAllIn(actual, "#addrs#")
            (actualReplaced, expected)
        } else {
            (actual, expected)
        }
    }

    /**
      * Detects whether the expected output represents a special directive
      * rather than literal stdout
      *
      * @param expected the expected output string extracted from metadata
      * @return `Some(directive)` if it is a recognised directive,
      *         otherwise `None`
      */
    def directive(expected: String): Option[(OutputDirective, String)] = 
        val lines = expected.stripTrailing().split("\n", -1).toVector
        // find the index where the directive occurs
        val idx = lines.indexWhere(l => OutputDirective.values.exists(_.repr == l.trim)) match {
            case -1 => None
            case i  => Some(i)
        }
        idx.map { i =>
            // if it exists return both the directive and the prefix string
            val dir = OutputDirective.values.find(_.repr == lines(i).trim).get
            val prefixLines = lines.take(i)
            val prefix = prefixLines.mkString("\n").stripTrailing()
            (dir, prefix)
        }
    
}

