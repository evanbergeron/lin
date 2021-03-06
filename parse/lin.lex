structure A = Ast
structure Tokens = Tokens

exception UnbalancedComments

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0

local val commentLevel = ref 0 in

  (* Thank you to Kaustuv Chaudhuri, Frank Pfenning, Anand
   * Subramanian, and/or Taegyun Kim for these enterComment,
   * exitComment functions and their usages.
   *)
  fun enterComment yypos =
    (commentLevel := !commentLevel + 1)

  fun exitComment () =
    (commentLevel := !commentLevel - 1;
    !commentLevel = 0)

  fun eof () =
      (if (!commentLevel > 0)
       then raise UnbalancedComments
       else ();
       Tokens.EOF(!pos,!pos))
end

fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
        "line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor LinLexFn(structure Tokens: Lin_TOKENS));
%s COMMENT;
alpha=[A-Za-z];
id=[A-Za-z_\']*;
digit=[0-9];
ws = [\ \t];
%%
<INITIAL> \n       => (pos := (!pos) + 1; lex());
<INITIAL> {ws}+    => (lex());
<INITIAL> "Z"      => (Tokens.ZERO(!pos,!pos));
<INITIAL> "S"      => (Tokens.SUCC(!pos,!pos));
<INITIAL> "\\"     => (Tokens.LAM(!pos,!pos));
<INITIAL> "let"    => (Tokens.LET(!pos,!pos));
<INITIAL> "fix"    => (Tokens.FIX(!pos,!pos));
<INITIAL> "ifz"    => (Tokens.IFZ(!pos,!pos));
<INITIAL> "fun"    => (Tokens.FUN(!pos,!pos));
<INITIAL> "lin"    => (Tokens.LIN(!pos,!pos));
<INITIAL> "un"     => (Tokens.UN(!pos,!pos));
<INITIAL> "->"     => (Tokens.SARROW(!pos,!pos));
<INITIAL> "nat"    => (Tokens.NAT(!pos,!pos));
<INITIAL> "split"  => (Tokens.SPLIT(!pos,!pos));
<INITIAL> "fst"    => (Tokens.FST(!pos,!pos));
<INITIAL> "snd"    => (Tokens.SND(!pos,!pos));
<INITIAL> "with"   => (Tokens.WITH(!pos,!pos));
<INITIAL> "as"     => (Tokens.AS(!pos,!pos));
<INITIAL> "in"     => (Tokens.IN(!pos,!pos));
<INITIAL> "of"     => (Tokens.OF(!pos,!pos));
<INITIAL> "."      => (Tokens.DOT(!pos,!pos));
<INITIAL> ":"      => (Tokens.COLON(!pos,!pos));
<INITIAL> ","      => (Tokens.COMMA(!pos,!pos));
<INITIAL> "*"      => (Tokens.STAR(!pos,!pos));
<INITIAL> "="      => (Tokens.EQ(!pos,!pos));
<INITIAL> "("      => (Tokens.LPAREN(!pos,!pos));
<INITIAL> ")"      => (Tokens.RPAREN(!pos,!pos));
<INITIAL> "|"      => (Tokens.PIPE(!pos,!pos));
<INITIAL> {digit}+ => (Tokens.IDX (valOf (Int.fromString yytext), !pos, !pos));
<INITIAL> {id}     => (Tokens.ID(yytext, !pos, !pos));

<INITIAL> "(*"        => (YYBEGIN COMMENT; enterComment yypos; lex());
<INITIAL> "*)"        => (raise UnbalancedComments);

<COMMENT> "(*"        => (enterComment yypos; lex());
<COMMENT> "*)"        => (if exitComment () then YYBEGIN INITIAL else ();lex());
<COMMENT> \n          => (lex());
<COMMENT> .           => (lex());
