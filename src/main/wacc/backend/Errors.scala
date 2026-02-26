package wacc.backend

import wacc.ExitCodes.* 

object InternalCompilerError {
    def undefinedRegister(varName: String) = 
        s"Attempted to lookup a virtual register before defining it in the environment.\nVariable name: $varName" 
    def invalidFreeType(recTyp: String) =
        s"Invalid type passed to 'free' statement. Must be an Array or Pair.\nReceived type: $recTyp"
    def expectedKnownType(recTyp: String) =
        s"Variable declaration requires a KnownType, but received an unknown/erased type.\nReceived type: $recTyp"
    def missingTypeInfo(node: String) =
        s"Expected type information in AST node, but none was found.\nNode info: $node"
    def missingRenameId(ident: String) = 
        s"Expected renameId information in Identifier node, but none was found.\nIdentifier info: $ident"
    
    def sizeMismatch(instruction: String, expected: Size, actual: Size): String =
        s"Register size mismatch in $instruction.Expected: ${expected.toString}, Actual: ${actual.toString}"
}

/**
  * Factory object for standardizing all backend error messages.
  */
object BackendError {
    /**
      * Helper to print the error to standard error and exit.
      */
    private def reportAndExit(message: String): Nothing = {
        report(message)
        sys.exit(code(CompilerFailure))
    }

    /**
      * Helper to print the error
      */
    private def report(message: String): String = s"Backend Error: $message"


    def undefinedRegister(varName: String): Nothing =
        reportAndExit(
            InternalCompilerError.undefinedRegister(varName),
        )

    def invalidFreeType(receivedType: String): Nothing =
        reportAndExit(
            InternalCompilerError.invalidFreeType(receivedType)
        )

    def expectedKnownType(receivedType: String): Nothing =
        reportAndExit(
            InternalCompilerError.expectedKnownType(receivedType),
        )

    def missingTypeInfo(nodeInfo: String): Nothing =
        reportAndExit(
            InternalCompilerError.missingTypeInfo(nodeInfo),
        )

    def missingRenameId(identInfo: String): Nothing =
        reportAndExit(
            InternalCompilerError.missingRenameId(identInfo),
        )

    def sizeMismatch(instruction: String, expected: Size, actual: Size): Nothing =
        reportAndExit(
            InternalCompilerError.sizeMismatch(instruction, expected, actual),
        )
}