structure A = Ast

signature PARSE = sig
  val parse : string -> Ast.Exp
  val parseFile : string -> Ast.Exp
end

structure Parse :> PARSE =
struct

  structure LinLrVals = LinLrValsFn (structure Token = LrParser.Token)
  structure LinLex = LinLexFn (structure Tokens = LinLrVals.Tokens)
  structure LinParse = Join
      (structure ParserData = LinLrVals.ParserData
       structure Lex = LinLex
       structure LrParser = LrParser)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
              TextIO.output(TextIO.stdOut,
                            "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in LinParse.parse(15,lexstream,print_error,())
      end

  fun parse' instream readline = let
      val inputFn = if not readline then TextIO.input else
                    (fn _ => case TextIO.inputLine instream of SOME s => s | _ => "")
      fun parseerror (s, i, p2) = TextIO.output(TextIO.stdOut,
                            "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
      val lexer = LrParser.Stream.streamify
                       (LinLex.makeLexer (fn _ => TextIO.input instream))
      val (absyn, _) = LinParse.parse(100, lexer, parseerror, ())
      in absyn end

  fun parseFile filename = parse' (TextIO.openIn filename) false

  fun parse s = parse' (TextIO.openString s) false

  fun parseIn filename = parse' (TextIO.openIn filename) true

end
