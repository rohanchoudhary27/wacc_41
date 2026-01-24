package wacc

def main(args: Array[String]): Unit = {
    println("hello WACC!")

    args.headOption match {
        case Some(expr) => parser.parse(expr) match {
            case Left(msg) => println(msg)
            case Right(x) => println(s"$expr = $x")
        }
        case None => println("please enter an expression")
    }
}
