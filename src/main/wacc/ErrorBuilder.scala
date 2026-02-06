package wacc

import parsley.errors.tokenextractors.MatchParserDemand

/**
* Custom ErrorBuilder to format syntax errors consistently.
* Formats position as "(line X, column Y):" and provides clean error content.
*/
object ErrorBuilder extends parsley.errors.ErrorBuilder[String] with MatchParserDemand {
    type Position = (Int, Int)
    type Source = Option[String]
    type ErrorInfoLines = Seq[String]
    type ExpectedItems = String
    type Messages = Seq[String]
    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Message = String
    type LineInfo = Seq[String]
    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String
    
    def pos(line: Int, col: Int): Position = (line, col)
    def source(sourceName: Option[String]): Source = sourceName
    
    /* Build final error string with custom position format */
    def build(pos: Position, source: Source, lines: ErrorInfoLines): String = {
        val (line, col) = pos
        val posStr = s"(line $line, column $col):"
        val content = lines.mkString("\n")
        s"$posStr\n$content"
    }
    
    def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, 
                     reasons: Messages, line: LineInfo): ErrorInfoLines =
        (unexpected.toSeq ++ expected.toSeq ++ reasons ++ line)
    
    def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines = msgs ++ line
    
    def combineExpectedItems(alts: Set[Item]): ExpectedItems = {
        if (alts.isEmpty) ""
        else if (alts.size == 1) alts.head
        else {
            val sorted = alts.toSeq.sorted
            sorted.init.mkString(", ") + ", or " + sorted.last
        }
    }
    
    def combineMessages(alts: Seq[Message]): Messages = alts
    
    def unexpected(item: Option[Item]): UnexpectedLine = 
        item.map(i => s"  unexpected $i")
    
    def expected(alts: ExpectedItems): ExpectedLine = 
        if (alts.isEmpty) None else Some(s"  expected $alts")
    
    def reason(reason: String): Message = s"  $reason"
    def message(msg: String): Message = s"  $msg"
    
    def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], 
                 lineNum: Int, errorPointsAt: Int, errorWidth: Int): LineInfo = {
        val before = linesBefore.map(l => s"  >$l")
        val err = s"  >$line"
        val pointer = "  " + (" " * errorPointsAt) + ("^" * Math.max(1, errorWidth))
        val after = linesAfter.map(l => s"  >$l")
        before ++ Seq(err, pointer) ++ after
    }
    
    val numLinesBefore: Int = 1
    val numLinesAfter: Int = 1
    
    def raw(item: String): Raw = s""""$item""""
    def named(item: String): Named = item
    val endOfInput: EndOfInput = "end of input"
}
