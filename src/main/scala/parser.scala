import parsley.Parsley, Parsley._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import ast._
    
    lazy val program = INTEGER
    val result = fully(program)
}