--		Copyright 1993-2003 by Daniel R. Grayson
-- revamped by P. Zinn-Justin 2020
-----------------------------------------------------------------------------
-- html input
-----------------------------------------------------------------------------

Hypertext = new Type of BasicList
Hypertext.synonym = "mark-up list"

HypertextParagraph = new Type of Hypertext		    -- one of these will be a paragraph
HypertextParagraph.synonym = "mark-up list paragraph"

HypertextContainer = new Type of Hypertext	    -- one of these may contain paragraphs or containers, and its method for printing has to handle the line breaks
HypertextContainer.synonym = "mark-up list container"

MarkUpType = new Type of SelfInitializingType
MarkUpType.synonym = "mark-up type"

new MarkUpType from Thing := (M,x) -> new M from {x}
new MarkUpType from List := (M,x) -> new M from x
new MarkUpType from Sequence := (M,x) -> new M from toList x

options MarkUpType := X -> X.Options

MarkUpType Net := (M,x) -> new M from {toString x}
MarkUpType String :=
MarkUpType Hypertext := (M,x) -> new M from {x}

IntermediateMarkUpType = new Type of MarkUpType	    -- this is for things like MENU, which do not correspond to an html entity, but have a recipe for translation into html
IntermediateMarkUpType.synonym = "intermediate mark-up type"

makeList := method()
makeList MarkUpType := X -> toString X
makeList Type       := X -> concatenate("new ", toString X, " from ")
toExternalString Hypertext := s -> concatenate(makeList class s, toExternalString toList s)
toString         Hypertext := s -> concatenate(makeList class s, toString         toList s)

lower := "abcdefghijklmnopqrstuvwxyz"
upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
tolower := new HashTable from apply(characters upper,characters lower,identity)
toupper := new HashTable from apply(characters lower,characters upper,identity)
toLower = s -> concatenate apply(characters s, c -> if tolower#?c then tolower#c else c)
toUpper = s -> concatenate apply(characters s, c -> if toupper#?c then toupper#c else c)

-- the tags that print with extra newlines after the close tag:
ewnl := BlockMix - set { "ins" } + set { "body", "tr", "li", "head", "html", "title", "link", "meta", "style" } 

htmlMarkUpType := opts -> s -> (
     off := "</" | s | ">";
     lt := "<";
     onoff := "/>";
     if ewnl#?s then (off = off|"\n";onoff = onoff|"\n");
     t -> (
	  o := "";
	  (opts',u) := try override(opts, toSequence t) else error(
	       "mark up type ",toString class t,
	       ": unrecognized option name(s): ", toString select(toList t, x -> instance(x,Option))
	       );
	  scanPairs(opts', (k,v) -> if v =!= null then o = concatenate(o, " ", k, "=", format v));
	  if u === () then concatenate(lt, s, o, onoff)
	  else concatenate(lt, s, o, ">", apply(sequence u,html), off)
	  ))

htmlGlobalAttr = { -- html global attributes
    "accesskey",
    "class",
    "contenteditable",
    --  "data-*", -- at the moment can't handle this
    "dir",
    "draggable",
    "dropzone",
    "hidden",
    "id",
    "lang",
    "spellcheck",
    "style",
    "tabindex",
    "title",
    "translate"
    }

htmlAttr = htmlGlobalAttr | { -- html global and event attributes
    "onafterprint",
    "onbeforeprint",
    "onbeforeunload",
    "onerror",
    "onhashchange",
    "onload",
    "onmessage",
    "onoffline",
    "ononline",
    "onpagehide",
    "onpageshow",
    "onpopstate",
    "onresize",
    "onstorage",
    "onunload"
}

withOptions = (v,x) -> (x.Options = new OptionTable from apply(flatten v,val -> if class val === Option then val else val=>null); x)
withQname   = (q,x) -> (
    x.qname = q;
    if x.?Options then html x := (htmlMarkUpType x.Options) q;
    x)

trimfront := x -> apply(x, line -> if not instance(line,String) then line else (
	  s := lines line;
	  r := if not s#?0 then line else concatenate between(newline, prepend(replace("^[[:space:]]+","",s#0), drop(s,1)));
	  if #r =!= 0 then r))

MarkUpType.GlobalAssignHook = (X,x) -> (
     if not x.?qname then withQname(toLower toString X,x);
     if not hasAttribute(x,ReverseDictionary) then setAttribute(x,ReverseDictionary,X);
     )

IntermediateMarkUpType.GlobalAssignHook = globalAssignFunction -- no qname, no default method for producing html

new MarkUpType := x -> error "obsolete 'new' method called"

BR         = withOptions_htmlAttr new MarkUpType of Hypertext		    -- HypertextParagraph?  no, because paragraphs are separated more by browsers
br         = BR{}

HR         = withOptions_htmlAttr new MarkUpType of HypertextParagraph
hr         = HR{}

PARA       = withQname_"p" withOptions_htmlAttr new MarkUpType of HypertextParagraph	    -- double spacing inside

ExampleItem = withQname_"code" withOptions_htmlAttr new MarkUpType of Hypertext
makeExampleItem = method()
makeExampleItem String := s -> ExampleItem s
makeExampleItem Thing := s -> error ("EXAMPLE expected a string or a PRE item, but encountered ", toString s)

EXAMPLE = method(Dispatch => Thing)
EXAMPLE VisibleList := x -> (
     x = nonnull trimfront toSequence x;
     if #x === 0 then error "empty list of examples encountered";
     TABLE splice { "class" => "examples", apply(x, item -> TR TD makeExampleItem item) }
     )
EXAMPLE String := x -> (
     if #x == 0 then error "empty example string";
     if x#0 == newline then error "empty first line in example";
     EXAMPLE {x}
     )

PRE        = withOptions_htmlAttr new MarkUpType of HypertextParagraph
makeExampleItem PRE := identity				    -- this will allow precomputed example text

TITLE      = withOptions_htmlGlobalAttr new MarkUpType of HypertextParagraph
HEAD       = withOptions_htmlGlobalAttr new MarkUpType of HypertextParagraph
BODY       = withOptions_htmlGlobalAttr new MarkUpType of HypertextContainer
IMG	   = withOptions_{htmlAttr,"alt","crossorigin","height","ismap","longdesc","referrerpolicy","sizes","src","srcset","usemap","width"} new MarkUpType of Hypertext
HTML       = withOptions_htmlGlobalAttr new MarkUpType of Hypertext
HEADER1    = withQname_"h1" withOptions_htmlAttr new MarkUpType of HypertextParagraph
HEADER2    = withQname_"h2" withOptions_htmlAttr new MarkUpType of HypertextParagraph
HEADER3    = withQname_"h3" withOptions_htmlAttr new MarkUpType of HypertextParagraph
HEADER4    = withQname_"h4" withOptions_htmlAttr new MarkUpType of HypertextParagraph
HEADER5    = withQname_"h5" withOptions_htmlAttr new MarkUpType of HypertextParagraph
HEADER6    = withQname_"h6" withOptions_htmlAttr new MarkUpType of HypertextParagraph
SUBSECTION = HEADER2
LITERAL    = withQname_"div" new IntermediateMarkUpType of Hypertext -- fake!!!!! check later
BLOCKQUOTE = withOptions_htmlAttr new MarkUpType of HypertextContainer
STRONG     = withOptions_htmlAttr new MarkUpType of Hypertext
SMALL      = withOptions_htmlAttr new MarkUpType of Hypertext
SUB        = withOptions_htmlAttr new MarkUpType of Hypertext
SUP        = withOptions_htmlAttr new MarkUpType of Hypertext
ITALIC     = withQname_"i" withOptions_htmlAttr new MarkUpType of Hypertext
TEX	   = withQname_"#PCDATA" new MarkUpType of Hypertext -- TEX really needs to be processed further so its output can be checked, too!
SPAN       = withOptions_htmlAttr new MarkUpType of Hypertext
TT         = withOptions_htmlAttr new MarkUpType of Hypertext
LI         = withOptions_htmlAttr new MarkUpType of HypertextContainer
EM         = withOptions_htmlAttr new MarkUpType of Hypertext
BOLD       = withQname_"b" withOptions_htmlAttr new MarkUpType of Hypertext
CODE       = withOptions_htmlAttr new MarkUpType of Hypertext
COMMENT    = new MarkUpType of Hypertext
CDATA      = new MarkUpType of Hypertext
LINK       = withOptions_{htmlGlobalAttr,"href","rel","title","type"} new MarkUpType of Hypertext
META       = withOptions_{htmlGlobalAttr,"name","content","http-equiv"} new MarkUpType of Hypertext

DL         = withOptions_htmlAttr new MarkUpType of Hypertext
DD         = withOptions_htmlAttr new MarkUpType of Hypertext
DT         = withOptions_htmlAttr new MarkUpType of Hypertext

HREF       = withQname_"a" new IntermediateMarkUpType of Hypertext
new HREF from List := (HREF,x) -> (
     if #x > 2 or #x == 0 then error "HREF list should have length 1 or 2";
     y := x#0;
     if not (
	  instance(y,String) 
	  or
	  instance(y,Sequence) and #y===2 and instance(y#0,String) and instance(y#1,String))
     then error "HREF expected URL to be a string or a pair of strings";
     x)

ANCHOR     = withQname_"a" withOptions_htmlAttr new MarkUpType of Hypertext

UL         = withOptions_htmlAttr new MarkUpType of HypertextParagraph
new UL from VisibleList := (UL,x) -> (
     x = nonnull x;
     if #x == 0 then error("empty element of type ", format toString UL, " encountered");
     apply(x, e -> (
	       if class e === TO then LI{TOH{e#0}}
	       else if class e === LI or instance(e,Option) then e
	       else LI e)))
ul = x -> (
     x = nonnull x;
     if #x>0 then UL x)

DIV        = withOptions_htmlAttr new MarkUpType of HypertextContainer
DIV1       = withQname_"div" withOptions_{"class"=>"single"} new MarkUpType of HypertextContainer -- phase this one out!

LABEL      = withOptions_{htmlAttr,"for","form"} new MarkUpType of Hypertext

TABLE      = withOptions_htmlAttr new MarkUpType of HypertextParagraph
TR         = withOptions_htmlAttr new MarkUpType of Hypertext
TD         = withOptions_htmlAttr new MarkUpType of HypertextContainer
ButtonTABLE  = new MarkUpType of HypertextParagraph

TO2        = withQname_"a" new IntermediateMarkUpType of Hypertext
new TO2 from Sequence := 
new TO2 from List := (TO2,x) -> { makeDocumentTag x#0, concatenate drop(toSequence x,1) }

TO         = withQname_"a" new IntermediateMarkUpType of Hypertext
new TO from List := (TO,x) -> if x#?1 then { makeDocumentTag x#0, concatenate drop(toSequence x,1) } else { makeDocumentTag x#0 }

TOH        = withQname_"span" new IntermediateMarkUpType of Hypertext
new TOH from List := (TOH,x) -> { makeDocumentTag x#0 }

LATER      = new IntermediateMarkUpType of Hypertext

new TO from Hypertext := 
new TOH from Hypertext := x -> error("TO of mark up list '", toString x, "'")

new TO from Thing := new TOH from Thing := (TO,x) -> new TO from {x} -- document tags can be sequences or arrays, so keep them intact

MENU       = withQname_"div" new IntermediateMarkUpType of HypertextParagraph	            -- like "* Menu:" of "info"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
