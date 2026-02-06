package wacc

/**
* Initial string validity checks of the program before it is parsed.
* Ensures no non-ASCII chars are in the program string.
*/
object Sanitiser {

    /* Largest ASCII integer value */
    val AsciiBound = 127

    /**
    * Checks if a character is ASCII or not.
    * 
    * @param c: The character to test
    * 
    * @return: true if `c` is ASCII, false otherwise
    */
    private def isAscii(c: Char): Boolean = c <= AsciiBound

    /**
    * Sanitises the input program string, checking for non-ASCII characters
    * Reports a syntax error to the `ErroLogger` if a non-ASCII char is found
    * 
    * @param progStr: The program string (pre-parse) to check
    * 
    * @return: The program string unchanged (in case frontend stages are to be chained
    * idiomatically later)
    */
    def sanitise(progStr: String, errorLogger: ErrorLogger): String = {

        /* Track position in string */
        var currLine = 1
        var currCol = 1

        for (c <- progStr) {

            if (!isAscii(c)) {
                val errorMsg =
                    "\nSyntax error: Cannot have non-ASCII characters in WACC programs"
                errorLogger.logSemanticError((currLine, currCol), errorMsg)
            }

            /* Current line and column updates */
            if (c == '\n') {
                currLine += 1
                currCol = 1
            } else {
                currCol += 1
            }

        }
        progStr
    }

}
