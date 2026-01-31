--		Copyright 2000 by Daniel R. Grayson

needs "reals.m2"

TeXmacsBegin = "\2"
TeXmacsEnd   = "\5"
fix := s -> replace("\2|\5","\33\\0",s)
fixn := s -> concatenate between("\0", apply(separate("\\0", s), fix))
red := p -> concatenate("<mstyle color=\"red\">",concatenate p,"</mstyle>")
mathMode := s -> concatenate("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",concatenate s,"</math>")
para := p -> concatenate("<p>",concatenate p,"</p>")
tmhtml := p -> concatenate(TeXmacsBegin,"html:",concatenate p,TeXmacsEnd)
mtable := x -> concatenate(
     "<mtable columnalign=\"right center left\">", newline,
     apply(x, row -> ( "<mtr>", apply(row, e -> ("<mtd>",e,"</mtd>",newline)), "</mtr>", newline ) ),
     "</mtable>", newline )
fmt := x -> concatenate lines try mathML x else mathML toString x;
po := () -> red concatenate("<mi>", interpreterDepth:"o", toString lineNumber,"</mi>")
Thing#{TeXmacs,Print} = send := v -> (
     << tmhtml fixn mathMode mtable {{po(), red "<mo>=</mo>",fmt v}})
Nothing#{TeXmacs,Print} = identity
InexactNumber#{TeXmacs,Print} = v -> withFullPrecision ( () -> send v )
Hypertext#{TeXmacs, Print} = v -> ( << tmhtml fixn html v )

tmAfterPrint = x -> (
    << endl
    << tmhtml fixn mathMode mtable {{po(), red "<mo>:</mo>",
	    concatenate apply(deepSplice sequence x,
		y -> if y =!= null then fmt y)}})

Thing#{TeXmacs,AfterPrint} = x -> (
    l := lookup(AfterPrint, class x);
    if l === null then return;
    s := l x;
    if s =!= null then tmAfterPrint s)
Thing#{TeXmacs,AfterNoPrint} = x -> (
    l := lookup(AfterNoPrint, class x);
    if l === null then return;
    s := l x;
    if s =!= null then tmAfterPrint s)

Thing#{TeXmacs, ErrorPrint}    = x -> mathML Abbreviate {x}
String#{TeXmacs, ErrorPrint}   = mathML
Symbol#{TeXmacs, ErrorPrint}   = x -> mathML("'" | toString x | "'")
Sequence#{TeXmacs, ErrorPrint} = args -> (
    concatenate apply(args, processArgs_ErrorPrint))

Thing#{TeXmacs, AfterErrorPrint}    = x -> ""
Symbol#{TeXmacs, AfterErrorPrint}   = x -> concatenate(newline,
    toString locate x, ": here is the first use of '", toString x, "'")
Sequence#{TeXmacs, AfterErrorPrint} = args -> (
    concatenate apply(args, processArgs_AfterErrorPrint))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
