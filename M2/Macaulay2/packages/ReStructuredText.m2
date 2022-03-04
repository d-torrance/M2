newPackage("ReStructuredText",
    PackageImports => {"Text"}
    )

export {
    "reStructuredText"
    }
importFrom_Core {"setupRenderer"}

reStructuredText = method()

noopts := x -> select(x,e -> class e =!= Option and class e =!= OptionTable)

concatenateAndPrepend = s -> x -> concatenate(s, x)
concatenateAndPrependWithNewline = s -> x -> concatenate(s, x, newline)
concatenateAndAppend = s -> x -> concatenate(x, s)
concatenateAndNewline = concatenateAndAppend newline
concatenateAndUnderline = filler -> x -> (
    r := concatenate x;
    concatenate(
	r, newline,
	width r : filler, newline))
concatenateAndSurround = filler -> x -> (
    r := concatenate x;
    concatenate(
	width r : filler, newline,
	r, newline,
	width r : filler, newline))

setupRenderer(reStructuredText, concatenate, Hypertext)
setupRenderer(reStructuredText, concatenateAndNewline, HypertextParagraph)
setupRenderer(reStructuredText, concatenateAndNewline, HypertextContainer)
setupRenderer(reStructuredText, concatenateAndSurround  "=", HEADER1)
setupRenderer(reStructuredText, concatenateAndUnderline "=", HEADER2)
setupRenderer(reStructuredText, concatenateAndUnderline "-", HEADER3)

lvl := 0
renderList = mark -> x -> (
    lvl = lvl + 1;
    r := concatenate(
	if lvl == 1 then "" else newline, -- for nested lists
	apply(noopts x, y -> (
		z := first noopts y; -- check if we're nesting another list
		concatenate(
		    2 * (lvl - 1),
		    if instance(z, OL) or instance(z, UL) then "" else mark,
		    reStructuredText y))));
    lvl = lvl - 1;
    r)

reStructuredText OL := renderList "# "
reStructuredText UL := renderList "* "
reStructuredText DL := renderList ""
setupRenderer(reStructuredText, concatenateAndPrependWithNewline "  ", DD)

reStructuredText Thing := toString

end

loadPackage("ReStructuredText",
    FileName => "~/src/macaulay2/M2/M2/Macaulay2/packages/ReStructuredText.m2",
    Reload => true)

reStructuredText help rank

ZZ * String := (n, s) -> concatenate(n : s)
String * ZZ := (s, n) -> n * s

reStructuredText HEADER2 "foo"

toExternalString("foo" | newline | newline | newline)

reStructuredText UL {"foo", "bar", "baz"}
reStructuredText OL {"foo", "bar", UL {"foo", UL{"bar", "qux"}, "baz"}}

reStructuredText DL {DT {"foo"}, DD {"bar"}}
