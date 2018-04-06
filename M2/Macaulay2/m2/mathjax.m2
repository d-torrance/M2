-- some texMath that got stranded
texMath BasicList := s -> concatenate(
     if class s =!= List then texMath class s,
    "\\left\\{",
    between(",\\,",apply(toList s,texMath))
    ,"\\right\\}"
    )
texMath Array := x -> concatenate("\\left[", between(",", apply(x,texMath)), "\\right]")
texMath Sequence := x -> concatenate("\\left(", between(",", apply(x,texMath)), "\\right)")
texMath HashTable := x -> if x.?texMath then x.texMath else (
     concatenate flatten (
	 texMath class x,
	 "\\left\\{",
	 between(",\\,", apply(sortByName pairs x,(k,v) -> texMath k | "\\,\\Rightarrow\\," | texMath v)),
	 "\\right\\}"
	 )
      )
texMath MonoidElement := texMath @@ expression
texMath Type := x -> if x.?texMath then x.texMath else texMath toString x
texMath Function := x -> texMath toString x
texMath ScriptedFunctor := lookup(texMath,Type)
-- for a slightly different style:
-*
texMath Type := x -> if x.?texMath then x.texMath else "{\\textsf{" | toString x | "}}"
texMath Function := x -> "{\\textsf{" | toString x | "}}"
*-

-- strings -- compare with hypertext.m2
texVerbLiteralTable := new MutableHashTable
    scan(characters ascii(0 .. 255), c -> texVerbLiteralTable#c = c)
    texVerbLiteralTable#"!" = ///!\texttt{!}\verb!///
    --texVerbLiteralTable#"$" = ///!\texttt{\$}\verb!/// -- eww ugly fix of #375 of mathJax. not needed if not enclosing using $
    texVerbLiteralTable#"\\"= ///!\verb!\!\verb!/// -- eww ugly fix of #375 of mathJax
    -- unfortunately the next 2 (needed if the string happens to be in a {} group) may result in wrong font in normal LaTeX depending on encoding, see https://stackoverflow.com/questions/2339651/how-to-get-real-braces-in-ttfont-in-latex
    texVerbLiteralTable#"{" =///!\texttt{\{}\verb!/// -- eww ugly fix of #375 of mathJax
    texVerbLiteralTable#"}" =///!\texttt{\}}\verb!/// -- eww ugly fix of #375 of mathJax
texVerbLiteral = s -> concatenate apply(characters s, c -> texVerbLiteralTable#c)
--texMath String := s -> "\\verb|"|texVerbLiteral s|"|"
texMath String := s -> (
    ss := separate s;
    if #ss <=1 then replace(///\\verb!!///,"",///\verb!///|texVerbLiteral s|///!///) -- to optimize compilation
    else texMath stack ss
    )

-- this truncates very big nets
maxlen := 3000; -- randomly chosen
texMath Net := n -> (
    dep := depth n; hgt := height n;
    s:="";
    len:=0; i:=0;
    scan(unstack n, x->(
	    i=i+1;
	    len=len+#x;
	    if i<#n and len>maxlen then (
		s=s|"\\vdots\\\\"|"\\vphantom{\\big|}" | texMath last n | "\\\\[-2mm]";
		if i<hgt then (hgt=i; dep=1) else dep=i+1-hgt;
		break
		);
	    s=s|"\\vphantom{\\big|}" | texMath x | "\\\\[-2mm]";
	    ));
    "\\raise"|toString (2.65*(-dep+hgt-1))|"mm\\begin{array}{l}" | s | "\\end{array}"
    )

-- now the mathJax stuff per se
-- comments used to help the browser app
mathJaxTextComment := "<!--txt-->"; -- indicates what follows is pure text; default mode
mathJaxTexComment := "<!--tex-->"; -- indicates what follows is HTML with some TeX to be compiled
mathJaxHtmlComment := "<!--html-->"; -- indicates what follows is pure HTML

mathJax Thing := x -> concatenate(mathJaxTexComment,"\\(\\displaystyle ",htmlLiteral texMath x,"\\)") -- by default, for MathJax we use tex (as opposed to html)
--mathJax Thing := x -> concatenate(mathJaxTexComment."\\(\\require{action}\\displaystyle\\bbox[padding: 10px 0px]{\\toggle{",htmlLiteral texMath x,"}{"|htmlLiteral texMath net x|"}\\endtoggle}\\)") -- by default, for MathJax we use tex (as opposed to html)

-- text stuff
mathJax Hypertext := x -> concatenate(mathJaxHtmlComment, html x)
-- see also texMath Net above
mathJax Net := n -> mathJaxTexComment | "<span style=\"display:inline-table;vertical-align:" | toString(5.3*(height n-1)) | "mm\">" | concatenate apply(unstack n, x-> "\\(" | texMath x | "\\)<br/>") | "</span>"
mathJax String := lookup(mathJax,Thing) -- for now. might want to switch to HTML later, just like its ancestor net
mathJax Descent := x -> mathJaxHtmlComment | "<span style=\"display:inline-table\">" | concatenate sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then toString k -- sucks but no choice
	  else toString k | " : " | mathJax v
	  ) | "<br/>") | "</span>"


-- output routines

ZZ#{MathJax,InputPrompt} = ZZ#{Standard,InputPrompt}
ZZ#{MathJax,InputContinuationPrompt} = ZZ#{Standard,InputContinuationPrompt}

Thing#{MathJax,BeforePrint} = identity -- not sure what to put there

Nothing#{MathJax,Print} = identity

Thing#{MathJax,Print} = x -> (
     oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    << mathJaxTextComment;
    y := mathJax x; -- we compute the mathJax now (in case it produces an error)
    << endl << oprompt | y | mathJaxTextComment << endl;
    )

-- afterprint <sigh>

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

texAfterPrint :=  y -> ( y = select(deepSplice sequence y, x -> class x =!= Nothing);
	 << mathJaxTextComment;
	 z := htmlLiteral concatenate(texMath\y);
	 << endl << on() | " : " | mathJaxTexComment | "\\(" | z | "\\)" | mathJaxTextComment << endl;
	 )

Thing#{MathJax,AfterPrint} = x -> texAfterPrint class x;

Boolean#{MathJax,AfterPrint} = identity

Expression#{MathJax,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{MathJax,AfterPrint} = identity

Ideal#{MathJax,AfterPrint} = Ideal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{MathJax,AfterPrint} = MonomialIdeal#{MathJax,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)


Module#{MathJax,AfterPrint} = M -> (
     n := rank ambient M;
     texAfterPrint(ring M,"-module",
     if M.?generators then
     if M.?relations then (", subquotient of ",ambient M)
     else (", submodule of ",ambient M)
     else if M.?relations then (", quotient of ",ambient M) 
     else if n > 0 then
	  (", free",
	  if not all(degrees M, d -> all(d, zero)) 
	  then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
	  ))
     )


Matrix#{MathJax,AfterPrint} = Matrix#{MathJax,AfterNoPrint} = f -> texAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapArrow from {target f,source f}))

Net#{MathJax,AfterPrint} = identity

Nothing#{MathJax,AfterPrint} = identity

RingMap#{MathJax,AfterPrint} = RingMap#{MathJax,AfterNoPrint} = f -> texAfterPrint (class f," ",new MapArrow from {target f,source f})

Sequence#{MathJax,AfterPrint} = Sequence#{MathJax,AfterNoPrint} = identity

CoherentSheaf#{MathJax,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     texAfterPrint("coherent sheaf on ",X,
     if M.?generators then
     if M.?relations then (", subquotient of ", ambient F)
     else (", subsheaf of ", ambient F)
     else if M.?relations then (", quotient of ", ambient F)
     else if n > 0 then (
	  ", free"
	  -- if not all(degrees M, d -> all(d, zero))
	  -- then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  )
     )
 )

ZZ#{MathJax,AfterPrint} = identity