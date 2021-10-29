-- -*- coding: utf-8 -*-
newPackage(
        "VectorGraphics",
        Version => "0.97",
        Date => "October 25, 2021", -- "May 18, 2018",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "A package to produce SVG graphics",
	Keywords => {"Graphics"},
        DebuggingMode => true,
	AuxiliaryFiles => true,
	PackageImports => {"Text"},
	PackageExports => {"Text"}
        )

export{"GraphicsType", "GraphicsObject", "GraphicsPoly",
    "GraphicsList", "Circle", "Light", "Ellipse", "Path", "Polygon", "Polyline", "GraphicsText", "Line", "GraphicsHtml",
    "gList", "viewPort", "rotation", "translation", "linearGradient", "radialGradient", "arrow", "plot",
    "Contents", "TextContent", "HtmlContent", "OneSided", "RadiusX", "RadiusY", "Specular", "Point1", "Point2", "RefPoint", "Size", "ViewPort",
    "Perspective", "FontSize", "AnimMatrix", "TransformMatrix", "Radius",
    "Blur", "Static", "PointList", "Axes", "Margin", "Mesh", "Draggable",
    "SVG",
    "gNode", "GraphicsNode", "place"
    }

protect Filter
protect Distance
protect Is3d
protect Animated
protect CurrentMatrix
protect PerspectiveMatrix
protect svgElement
protect lights
protect owner
protect js
protect RefPointFunc

debug Core

-- for now data-* need entering manually
htmlData={ "data-matrix","data-dmatrix","data-pmatrix","data-r","data-rx","data-ry","data-coords","data-onesided","data-origin","data-fontsize","data-static"}
svgAttr= htmlAttr | htmlData | { "transform", "filter" } -- what else ?

-- parsing of coordinates / matrices
gParse := method()
-- gParse Sequence := x -> gParse vector toList x -- retired due to https://github.com/Macaulay2/M2/issues/1548
gParse Array := x -> gParse vector toList x -- replaced with this
gParse Matrix := x -> (
    if rank source x =!= rank target x or rank source x < 2 or rank source x > 4 then error "wrong matrix";
    if rank source x == 2 then x++1++1 else if rank source x == 3 then x++1 else x
    )
gParse Vector := x -> (
    if rank class x < 2 or rank class x > 4 then error "wrong coordinates";
    if rank class x === 2 then x || vector {0,1.}
    else if rank class x === 3 then x || vector {1.}
    else if rank class x === 4 then x
    )
gParse List := l -> apply(l,gParse)
gParse Option := o -> o#0 => gParse o#1

GraphicsObject = new Type of HashTable -- ancestor type

GraphicsObject ++ List := (opts1, opts2) -> merge(opts1,
    new class opts1 from if any(opts2,x->x#0===symbol cache) then opts2 else append(opts2,symbol cache => new CacheTable),
    (x,y) -> if instance(x,Matrix) and instance(y,Matrix) then gParse y*gParse x else y -- for TransformMatrix and AnimMatrix
    ) -- cf similar method for OptionTable

-- a bunch of options are scattered throughout the code:
-- * all dimensions are redefined as dimensionless quantities: Radius, FontSize, etc
-- * TransformMatrix for static transformation
-- * AnimMatrix for animation transformation
-- * OneSided for 3d paths, polygons
-- * Static for objects that can't be rotated
--   i.e, they or their contents can rotate/autorotate, but the rotations of their ancestors won't affect them
--   useful for lights
-- * Blur (amount of blurriness relative to the size of the object)
-- GLOBAL options (only work if in outermost object)
-- * Size for picture size
-- * ViewPort for manual range of viewing window
-- * Perspective for 3d: can be a number or a whole 4d matrix (ideally, there'd be a function to translate...)
--   the matrix should be such that after transformation, the coordinates are (x,y,z,1-z/p) where the viewer is at (0,0,0) and the screen at z=-p
-- * Margin (leave blank around picture)
-- * Axes (draw axes)

-- 3d: turns on lights, axes are diff

GraphicsType = new Type of Type -- all usable Graphics objects are ~ self-initialized

GraphicsType List := (T,opts) -> (
    opts0 := T.Options;
    -- scan the first few arguments in case we skipped the keys for standard arguments. also, parse
    (opts2,opts1):=override(,toSequence (opts0|opts));
    opts1 = sequence opts1;
    if #opts1 > #opts0 then error "too many arguments";
    sty := new MutableHashTable from applyPairs(opts2,(k,v) -> if class k === String then (k,v));
    opts3 := new HashTable from applyPairs(opts2,(k,v) -> if class k =!= String then (k,v));
    new T from merge(hashTable (apply(#opts1, i -> opts0#i#0 => opts1#i)
	| { symbol style =>  sty, symbol cache => new CacheTable}),opts3,first)
)

perspective = persp -> (
    if instance(persp,Matrix) then gParse persp else (
	if persp === () then persp = 1000;
	matrix {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,-1/persp,1}} -- output is {x,y,z,1-z/p}
    -- note in particular that distance = z-p *cannot* be extracted from this;
    -- however, z/(1-z/p) is essentially inverse distance which is good enough for sorting purposes
    )
)

viewPort = g -> (
    svg g; -- need to be rendered
    g.cache.ViewPort
    )
viewPort1 := method() -- returns [xmin,ymin],[xmax,ymax]
viewPort1 GraphicsObject := x -> null

distance = method()
distance GraphicsObject := x -> 0_RR

-- bit of a hack: 2d objects Circle, Ellipse, etc get scaled in a 3d context
scale := (x,g) -> x_3/(g.cache.owner.PerspectiveMatrix*x)_3

project3d := (x,g) -> (
    y := g.cache.owner.PerspectiveMatrix*x;
    (1/y_3)*y^{0,1,2} -- used for e.g. ViewPort and distance
    )
project2d := (x,g) -> (
    y := project3d(x,g);
    vector {y_0,y_1}
)
project2d' := (x,g) -> (
    y := project3d(x,g);
    vector {y_0,-y_1} -- annoying sign for actual svg coords
)

graphicsIdCount := 0;
graphicsId := () -> (
    graphicsIdCount=graphicsIdCount+1;
    "Graphics_" | toString currentTime() | "_" | toString graphicsIdCount
    )

svgElement := method(Dispatch=>Type) -- to each GraphicsType is assigned a svg MarkupType

new GraphicsType of GraphicsObject from VisibleList := (T,T2,x) -> (
    g:=new Type;
    g.Options=x#1; -- TODO: should it be an actual table? then have to suppress the BS syntax
    s := new MarkUpType of Hypertext;
    addAttribute(s,svgAttr | if #x>=3 then x#2 else {});
    s.qname = x#0;
    svgElement g := g' -> s;
    g)

Ellipse = new GraphicsType of GraphicsObject from ( "ellipse",
    { symbol Center => vector {0.,0.}, symbol RadiusX => 50., symbol RadiusY => 50. },
    { "rx", "ry", "cx", "cy" }
    )
viewPort1 Ellipse := g -> (
    p := gParse(g.Center,g);
    sc := scale(p,g);
    p=project2d(p,g);
    rx:=g.RadiusX*sc;
    ry:=g.RadiusY*sc;
    r := vector {rx,ry};
    { p-r, p+r }
    )
distance Ellipse := g -> (
    -(project3d(gParse(g.Center,g),g))_2
    )

Circle = new GraphicsType of Ellipse from ( "circle",
    { symbol Center => vector {0.,0.}, symbol Radius => 50. },
    { "r", "cx", "cy" }
    )
viewPort1 Circle := g -> (
    p := gParse(g.Center,g);
    if instance(g.Radius,Number) then (
    	sc := scale(p,g);
    	r:=g.Radius*sc;
    	p=project2d(p,g);
	) else (
    	p=project2d(p,g);
	pp:=project2d(gParse(g.Radius,g),g)-p;
	r=sqrt(pp_0^2+pp_1^2);
	);
    r = vector {r,r};
    { p - r, p + r }
    )


GraphicsText = new GraphicsType of GraphicsObject from ( "text",
    { symbol RefPoint => vector {0.,0.}, symbol TextContent => "", symbol FontSize => 14. },
    { "x", "y" }
    )
viewPort1 GraphicsText := g -> (
    f := g.FontSize;
    p := gParse(g.RefPoint,g);
    sc := scale(p,g);
    p=project2d(p,g);
    f=f*sc;
    r := vector { f*0.6*length g.TextContent, 0.8*f }; -- width/height. very approximate TODO properly
    pp := p + vector {
	if g#?"text-anchor" then (if g#"text-anchor" == "middle" then -0.5*r_0 else if g#"text-anchor" == "end" then -r_0 else 0) else 0,
	if g#?"dominant-baseline" then (if g#"dominant-baseline" == "middle" then -0.5*r_1 else if g#"dominant-baseline" == "hanging" then -r_1 else 0) else 0
	};
    {pp,pp+r}
    )

Line = new GraphicsType of GraphicsObject from ( "line",
    { Point1 => vector {0.,0.}, Point2 => vector {50.,50.}},
    { "x1", "y1", "x2", "y2" }
    )
viewPort1 Line := g -> (
    p1 := project2d(gParse (g.Point1,g),g);
    p2 := project2d(gParse (g.Point2,g),g);
    p := transpose{entries p1,entries p2};
    { vector(min\p), vector(max\p) }
    )
distance Line := g -> (
    d1 := (project3d(gParse (g.Point1,g),g))_2;
    d2 := (project3d(gParse (g.Point2,g),g))_2;
    -0.5*(d1+d2)
    )

GraphicsPoly = new Type of GraphicsObject;

Polyline = new GraphicsType of GraphicsPoly from ( "polyline", { symbol PointList => {} }, { "points" } )
Polygon = new GraphicsType of GraphicsPoly from ( "polygon", { symbol PointList => {} }, { "points" } )
Path = new GraphicsType of GraphicsPoly from ( "path", { symbol PointList => {} }, { "d" } )
viewPort1 GraphicsPoly := g -> ( -- relative coordinates *not* supported, screw this
    s := select(g.PointList, x -> not instance(x,String));
    if #s == 0 then return null;
    s = transpose apply(s, x -> entries project2d(gParse(x,g),g));
    {vector(min\s), vector(max\s)}
    )

-- to make lists of them
GraphicsList = new GraphicsType of GraphicsObject from ( "g", { symbol Contents => {} } )
-- slightly simpler syntax: gList (a,b,c, opt=>xxx) rather than GraphicsList { {a,b,c}, opt=>xxx }
gList = true >> opts -> x -> (
    cnt := nonnull toList deepSplice if instance(x,List) then toSequence x else sequence x;
    if any(cnt,x->not instance(x,GraphicsObject)) then error "Contents should be a list of GraphicsObject only";
    GraphicsList { symbol Contents => cnt, opts }
)
viewPort1 GraphicsList := x -> (
    s := nonnull apply(x.Contents, y->y.cache.ViewPort);
    if #s===0 then null else (
	s = transpose s;
	mn := transpose (entries \ s#0);
	mx := transpose (entries \ s#1);
	{vector (min\mn), vector(max\mx)}
    )
)
-- lists with preferred coordinate
GraphicsNode = new Type of GraphicsList; -- from ( "g", { symbol Node => vector {0.,0.}, symbol Contents => {} } )
-- not GraphicsType because should only be created using gNode
gNode = true >> opts -> x -> (
    x = deepSplice x;
    ctr := x#0;
    cnt := nonnull toList drop(x,1);
    if any(cnt,x->not instance(x,GraphicsObject)) then error "Contents should be a list of GraphicsObject only";
    sty := new MutableHashTable from applyPairs(opts,(k,v) -> if class k === String then (k,v));
    c := new CacheTable;
    (new GraphicsNode from applyPairs(opts,(k,v) -> if class k =!= String then (k,v)))
    ++ {symbol Contents => cnt, symbol TransformMatrix => translation ctr,
	symbol RefPointFunc => g -> c.CurrentMatrix_3,
	symbol style => sty, symbol cache => c}
    )
Number * GraphicsNode := (x,v) -> new GraphicsNode from {
    symbol RefPointFunc => g -> x*gParse(v,g),
    symbol cache => new CacheTable,
    symbol style => new MutableHashTable,
    symbol Contents => {},
    symbol js => () -> "gTimes("|jsString x|","|jsString v|")"
    }
Vector + GraphicsNode :=
GraphicsNode + Vector :=
GraphicsNode + GraphicsNode := (v,w) -> new GraphicsNode from {
    symbol RefPointFunc => g -> gParse(v,g)+gParse(w,g),
    symbol cache => new CacheTable,
    symbol style => new MutableHashTable,
    symbol Contents => {},
    symbol js => () -> "gPlus("|jsString v|","|jsString w|")"
    }
-- TODO add arrays too? depending on gParse rewrite
- GraphicsNode := v -> (-1)*v
Vector - GraphicsNode :=
GraphicsNode - Vector :=
GraphicsNode - GraphicsNode := (v,w) -> v+(-1)*w

place = method()
place (Vector,GraphicsNode,Number,Number) :=
place (GraphicsNode,Vector,Number,Number) :=
place (GraphicsNode,GraphicsNode,Number,Number) := (v,w,a,b) -> new GraphicsNode from {
    symbol RefPointFunc => g -> place(gParse(v,g),gParse(w,g),a,b),
    symbol cache => new CacheTable,
    symbol style => new MutableHashTable,
    symbol Contents => {},
    symbol js => () -> "gPlace("|jsString v|","|jsString w|","|jsString a|","|jsString b|")"
    }
place (Vector,Vector,Number,Number) := (v,w,a,b) -> (
    u := w - v;
    perp := vector { u_1, -u_0, 0, 0 };
    v + a*u + b*perp
    )
intersection(Vector,Vector,Vector,Vector) := true >> o -> (v1,v2,w1,w2) -> ( -- intersect lines (v1,v2) and (w1,w2)
    cf := (i,j,k,l) -> v1_i*v2_j*w1_k*w2_l;
    v:=vector{-cf(0, 1, 0, 3) + cf(0, 1, 3, 0) + cf(0, 3, 0, 1) - cf(0, 3, 1, 0) + cf(1, 0, 0, 3) - cf(1, 0, 3, 0) - cf(3, 0, 0, 1) + cf(3, 0, 1, 0), -cf(0, 1, 1, 3) + cf(0, 1, 3, 1) + cf(1, 0, 1, 3) - cf(1, 0, 3, 1) + cf(1, 3, 0, 1) - cf(1, 3, 1, 0) - cf(3, 1, 0, 1) + cf(3, 1, 1, 0), -cf(0, 2, 1, 3) + cf(0, 2, 3, 1) + cf(1, 2, 0, 3) - cf(1, 2, 3, 0) + cf(2, 0, 1, 3) - cf(2, 0, 3, 1) - cf(2, 1, 0, 3) + cf(2, 1, 3, 0) + cf(2, 3, 0, 1) - cf(2, 3, 1, 0) - cf(3, 2, 0, 1) + cf(3, 2, 1, 0), -cf(0, 3, 1, 3) + cf(0, 3, 3, 1) + cf(1, 3, 0, 3) - cf(1, 3, 3, 0) + cf(3, 0, 1, 3) - cf(3, 0, 3, 1) - cf(3, 1, 0, 3) + cf(3, 1, 3, 0)};
    (1/v_3)*sub(v,RR)
    )
l={Vector,GraphicsNode}; l=l**l; l=l**l; l=drop(toSequence\flatten\l,1);
scan(l, t ->
intersection t := true >> o -> (v1,v2,w1,w2) -> new GraphicsNode from {
    symbol RefPointFunc => g -> intersection(gParse(v1,g),gParse(v2,g),gParse(w1,g),gParse(w2,g)),
    symbol cache => new CacheTable,
    symbol style => new MutableHashTable,
    symbol Contents => {},
    symbol js => () -> "gInter("|jsString v1|","|jsString v2|","|jsString w1|","|jsString w2|")"
    })

gParse GraphicsNode := identity

-- what's below should replace current gParse vector/node (or at least be a distinct function)
gParse (Array,GraphicsObject) :=
gParse (Vector,GraphicsObject) := (v,g) -> g.cache.CurrentMatrix*gParse v
gParse (GraphicsNode,GraphicsObject) := (v,g) -> v.RefPointFunc g

GraphicsHtml = new GraphicsType of GraphicsText from ( "foreignObject",
    { symbol RefPoint => vector {0.,0.}, symbol HtmlContent => null, symbol FontSize => 14. },
    { "x", "y", "xmlns" => "http://www.w3.org/1999/xhtml" }
    )
viewPort1 GraphicsHtml := g -> (
    p := project2d(gParse (g.RefPoint,g),g);
    { p, p } -- TODO properly
    )

-- lighting
Light = new GraphicsType of Circle from ( "circle",
    { symbol Center => vector {0,0,0,1.}, symbol Radius => 10, symbol Specular => 64, symbol Blur => 0.3, symbol Static => true, "opacity" => 0, "fill" => "#FFFFFF", "stroke" => "none" },
    { "r", "cx", "cy" } -- atm these are not inherited
    )
-- in case it's drawn, it's a circle

-- viewPort1 ignores lights if invisible
viewPort1 Light := g -> if toString g.style#"opacity" == "0" then null else (lookup(viewPort1,Circle)) g -- that opacity test sucks


--
animated := method()
animated GraphicsObject := x -> x.?AnimMatrix
animated GraphicsList := x -> x.?AnimMatrix or any(x.Contents,animated)

draggable := method()
draggable GraphicsObject := x -> x.?Draggable and x.Draggable
draggable GraphicsList := x -> x.?Draggable or any(x.Contents,draggable)


SVG = new MarkUpType of Hypertext
addAttribute(SVG,svgAttr|{"height","preserveAspectRatio","viewBox","width","x","xmlns"=>"http://www.w3.org/2000/svg","y","zoomAndPan"})

--
stableSort = x -> if #x <= 1 then x else (
xx := transpose {x,toList(0..#x-1)};
(transpose sort xx)#0
)

-- for javascript stuff
jsString = method(Dispatch=>Thing)
jsString Thing := toString
jsString Matrix := x -> "matrix(" | jsString entries x | ")"
jsString Vector := x -> "vector(" | jsString entries x | ")"
jsString VisibleList := x -> "[" | demark(",",jsString\x) | "]"
jsString MutableList := x -> jsString toList x
jsString HashTable := x -> "{" | demark(",",apply(pairs x, (key,val) -> jsString key | ":" | jsString val)) | "}"
jsString Option := x -> "times(" | jsString x#0 | "," | jsString x#1 | ")"
jsString GraphicsNode := x -> if x.?js then x.js() else "gNode("|x.cache.Options#"id"|")"

one := map(RR^4,RR^4,1)
updateTransformMatrix := (g,m) -> ( -- (object,matrix of parent)
    g.cache.CurrentMatrix = if g.?Static and g.Static then one else m; -- if static reset to perspective matrix
    if g.?TransformMatrix then g.cache.CurrentMatrix = g.cache.CurrentMatrix*gParse g.TransformMatrix;
    )

ac := (h,k,i,x) -> (
    if not h#?k then h#k=new MutableList;
    h#k#i=x;
    )

-- is3d=false has three effects:
-- * the data-* stuff is lightened (can be recreated from the normal parameters)
-- * the event listeners for 3d rotating the object with the mouse are deactivated
-- * lighting is deactivated
is3d = method()
is3d Vector := v -> rank class v > 2
is3d Array := a -> #a > 2
is3d Matrix := m -> rank source m > 2 and (rank source m === 3 or m^{2,3} != matrix {{0,0,1.,0},{0,0,0,1.}} or m_2 != vector {0,0,1.,0})
-- a bit messy: a 2d translation / rotation looks like {{c,-s,0,x},{s,c,0,y},{0,0,1,0},{0,0,1,0}}
is3d List := l -> any(l,is3d)
is3d' = g -> (g.cache.?Is3d and g.cache.Is3d) or (g.?AnimMatrix and is3d g.AnimMatrix) or (g.?TransformMatrix and is3d g.TransformMatrix)
is3d GraphicsObject := is3d'
is3d Ellipse :=
is3d Circle := g -> is3d' g or is3d g.Center or is3d g.Radius
is3d GraphicsHtml :=
is3d GraphicsText := g -> is3d' g or is3d g.RefPoint
is3d Light := g -> true
is3d Line := g -> is3d' g or is3d g.Point1 or is3d g.Point2
is3d GraphicsPoly := g -> is3d' g or any(g.PointList,is3d)
is3d Thing := x -> false
--is3d GraphicsNode :=
is3d GraphicsList := l -> (
    -- the interesting one: recurse up and down
    if is3d' l or any(l.Contents,is3d) then (
	scan(l.Contents, g->g.cache.Is3d=true);
	true
	) else false
    )

local svg1;
svgLookup := hashTable {
    symbol TransformMatrix => (g,x) -> (g.cache.Options#"data-matrix" = gParse x;),
    symbol AnimMatrix => (g,x) -> (g.cache.Options#"data-dmatrix" = gParse x;),
    symbol Radius => (g,x) -> (
	if instance(x,Number) then (
	    if is3d g then (
		p := gParse(g.Center,g);
	    	sc := scale(p,g);
	    	r := x * sc;
		g.cache.Options#"data-r"=x;
		) else r = x;
	    ) else (
	    p  =project2d(gParse(g.Center,g),g);
	    pp:=project2d(gParse(x,g),g)-p;
	    r=sqrt(pp_0^2+pp_1^2);
	    if is3d g or instance(x,GraphicsNode) then ac(g.cache.Options,"data-coords",1,gParse x);
	    );
	g.cache.Options#"r" = r;
	),
    symbol RadiusX => (g,x) -> (
	r := gParse(g.Center,g);
	sc := scale(r,g);
	g.cache.Options#"rx" = x * sc;
	if is3d g then g.cache.Options#"data-rx"=x;
	),
    symbol RadiusY => (g,x) -> (
	r := gParse(g.Center,g);
	sc := scale(r,g);
	g.cache.Options#"ry" = x * sc;
	if is3d g then g.cache.Options#"data-ry"=x;
	),
    symbol OneSided => (g,x) -> (g.cache.Options#"data-onesided"=x;),
    symbol FontSize => (g,x) -> (
    	-- bit of a hack: 2d objects Circle, Ellipse, etc get scaled in a 3d context
	r := gParse(g.RefPoint,g);
	sc := scale(r,g);
	f:=max(0,x*sc);
	g.style#"font-size" = toString f|"px";
	if is3d g then g.cache.Options#"data-fontsize"=x;
	if instance(g,GraphicsHtml) then ( -- a bit hacky because foreignObject is so buggy
	    g.style#"overflow"="visible"; -- makes width/height irrelevant
	    g.style#"width"="100%"; -- -- but still needed otherwise webkit won't render
	    g.style#"height"="100%";
	    )
	),
    symbol PointList => (g,x) -> (
	x1 := select(x,y->not instance(y,String));
	if is3d g or any(x1,y->instance(y,GraphicsNode)) then g.cache.Options#"data-coords"=apply(x1,gParse); -- be more subtle? select?
	s := demark(" ", flatten apply(x, y -> if not instance(y,String) then apply(entries project2d'(gParse(y,g),g),toString) else y));
	if instance(g,Path) then g.cache.Options#"d" = s else g.cache.Options#"points" = s;
	),
    symbol Center => (g,x) -> (
	if instance(x,GraphicsNode) or is3d g then ac(g.cache.Options,"data-coords",0,gParse x);
	x = project2d'(gParse(x,g),g);
	g.cache.Options#"cx" = x_0;
	g.cache.Options#"cy" = x_1;
	),
    symbol RefPoint => (g,x) -> (
	if instance(x,GraphicsNode) or is3d g then ac(g.cache.Options,"data-coords",0,gParse x);
	x = project2d'(gParse(x,g),g);
	g.cache.Options#"x" = x_0;
	g.cache.Options#"y" = x_1;
	),
    symbol Point1 => (g,x) -> (
	if instance(x,GraphicsNode) or is3d g then ac(g.cache.Options,"data-coords",0,gParse x);
	x = project2d'(gParse(x,g),g);
	g.cache.Options#"x1" = x_0;
	g.cache.Options#"y1" = x_1;
	),
    symbol Point2 => (g,x) -> (
	if instance(x,GraphicsNode) or is3d g then ac(g.cache.Options,"data-coords",1,gParse x);
	x = project2d'(gParse(x,g),g);
	g.cache.Options#"x2" = x_0;
	g.cache.Options#"y2" = x_1;
	),
    symbol Static => (g,x) -> (if x then g.cache.Options#"data-static" = "true";),
    symbol Contents => (g,x) -> (
	x = stableSort x;
	g.cache.Contents = apply(x, y -> svg1(y,g.cache.CurrentMatrix));
	),
    symbol TextContent => (g,x) -> (g.cache.Contents = {x};),
    symbol HtmlContent => (g,x) -> (g.cache.Contents = if instance(x,VisibleList) then toList x else {x};),
    symbol Draggable => (g,x) -> (if x then g.cache.Options#"class" = (if g.cache.Options#?"class" then g.cache.Options#"class" | " " else "") | "M2SvgDraggable";)
    }

-- produces SVG element hypertext
precompute := (g,m,c) -> ( -- 1st phase (object,current matrix,cache of owner)
    g.cache.Filter={}; -- clean up filters from past
    g.cache.owner=c; -- owner cache
    updateTransformMatrix(g,m);
    if instance(g,Light) then (
    	g.cache.Options = new MutableHashTable from {"id" => graphicsId()};
    	c.lights = append(c.lights,g);
    	)
    else if instance(g,GraphicsNode) then (
    	g.cache.Options = new MutableHashTable from {"id" => graphicsId()};
	);
    if g.?Contents then scan(g.Contents, x -> precompute(x,g.cache.CurrentMatrix,c));
    )

updateGraphicsCache := (g,m) -> ( -- 2nd phase (object,current matrix)
    updateTransformMatrix(g,m); -- it's already been done but annoying issue of objects that appear several times
    if g.?Contents then (
	is3d g; -- TODO better to force 3d state of children to be determined
	scan(g.Contents,x -> updateGraphicsCache(x,g.cache.CurrentMatrix));
	);
    if g.?OneSided and g.OneSided then determineSide g;
    g.cache.ViewPort = viewPort1 g; -- update the range
    g.cache.Distance = distance g; -- update the distance
    if not g.cache.?Options then g.cache.Options = new MutableHashTable; -- TEMP (cause of lights), do better
    filter g;
    )

svg1 = (g,m) -> ( -- 3rd phase (object,current matrix)
    s := try svgElement class g else return; -- what is the else for?
    updateTransformMatrix(g,m); -- it's already been done but annoying issue of objects that appear several times
    g.cache.Contents={};
    scan(keys g, key -> if svgLookup#?key then svgLookup#key(g,g#key));
    args := g.cache.Contents;
    if #g.cache.Options>0 then args = append(args, applyValues(new OptionTable from g.cache.Options,jsString));
    if hasAttribute(g,ReverseDictionary) then args = append(args, TITLE toString getAttribute(g,ReverseDictionary));
    style(s args,new OptionTable from g.style)
    )

svg = g -> (
    g.cache.lights={};
    g.cache.PerspectiveMatrix = perspective if g.?Perspective then g.Perspective else ();
    precompute(g,one,g.cache);
    updateGraphicsCache(g,one);
    svg1(g,one)
    )

globalAssignment GraphicsObject
toString GraphicsObject := g -> if hasAttribute(g,ReverseDictionary) then toString getAttribute(g,ReverseDictionary) else (lookup(toString,HashTable)) g
net GraphicsObject := g -> if hasAttribute(g,ReverseDictionary) then net getAttribute(g,ReverseDictionary) else (lookup(net,HashTable)) g
expression GraphicsObject := hold

shortSize := 3.8
short GraphicsObject := g -> (
    if not g.?Size then return hold(g++{Size=>shortSize});
    s := if instance(g.Size,Vector) then sqrt(g.Size_0^2+g.Size_1^2) else g.Size;
    if s<shortSize then hold g else hold(g++{Size=>shortSize})
    )

distance GraphicsPoly := g -> (
    s := select(g.PointList, x -> not instance(x,String));
    if #s == 0 then 0_RR else -sum(s,x->(project3d(gParse(x,g),g))_2) / #s
    )
distance GraphicsList := g -> (
    if #(g.Contents) == 0 then 0_RR else sum(g.Contents, x->x.cache.Distance) / #(g.Contents)
    )
GraphicsObject ? GraphicsObject := (x,y) -> y.cache.Distance ? x.cache.Distance
distance GraphicsText := g -> (
    y := gParse(g.RefPoint,g);
    -y_2/y_3
    )

-- defs
svgDefs = new MarkUpType of Hypertext
svgDefs.qname="defs"
addAttribute(svgDefs,svgAttr)

scanDefs := g -> (
    lst := g.cache.Filter;
    if g.?Contents then lst = lst | flatten apply(g.Contents,scanDefs);
    lst
    )

-- full SVG with the headers
new SVG from GraphicsObject := (S,g) -> (
    main := svg g; -- run this first because it will compute the ranges too
        if main === null then return {};
    ss := {};
    if g.?Perspective then ss = append(ss,"data-pmatrix" => jsString g.cache.PerspectiveMatrix);
    if g.?ViewPort then r := g.ViewPort else r = g.cache.ViewPort; -- should be cached at this stage
    if r === null or r#0 == r#1 then ( r={vector {0.,0.},vector {0.,0.}}; rr:=vector{0.,0.}; g.cache.Size=vector{0.,0.}; ) else (
	r = apply(r,numeric);
	rr = r#1 - r#0;
	if rr_0 == 0 then (
	    rr = vector { rr_1 * 16/10, rr_1 };
	    r = { vector { r#0_0 - 0.5*rr_0, r#0_1 }, vector { r#1_0 + 0.5*rr_0, r#1_1 } };
	    );
	if rr_1 == 0 then (
	    rr = vector { rr_0, rr_0 * 10/16 };
	    r = { vector { r#0_0, r#0_1 - 0.5*rr_1 }, vector {  r#1_0, r#1_1 + 0.5*rr_1 } };
	    );
	g.cache.Size = if not g.?Size then g.cache.Size = 20/sqrt(rr_0^2+rr_1^2)*rr
	else if instance(g.Size,Vector) then g.Size
	else g.Size/sqrt(rr_0^2+rr_1^2)*rr;
	);
    -- axes
    axes:=null; axeslabels:=null; defsList:={};
    if g.?Axes and g.Axes =!= false then (
	p := g.cache.PerspectiveMatrix;
	arr := arrow(.1); -- TODO size
	-- determine intersection of viewport with axes TODO more symmetrically
	xmin := (p_(3,3)*r#0_0-p_(0,3))/(p_(0,0)-p_(3,0)*r#0_0);
	xmax := (p_(3,3)*r#1_0-p_(0,3))/(p_(0,0)-p_(3,0)*r#1_0);
	if xmax < xmin then ( temp:=xmin; xmin=xmax; xmax=temp; );
	ymin := (p_(3,3)*r#0_1-p_(1,3))/(p_(1,1)-p_(3,1)*r#0_1);
	ymax := (p_(3,3)*r#1_1-p_(1,3))/(p_(1,1)-p_(3,1)*r#1_1);
	if ymax < ymin then ( temp2:=ymin; ymin=ymax; ymax=temp2; );
	if is3d g then (
	    zmax := 0.25*(xmax-xmin+ymax-ymin);
	    zmin := -zmax;
	    );
	axes0 := gList(
	    Line { Point1 => vector {xmin,0,0,1}, Point2 => vector {xmax,0,0,1}, "marker-end" => arr },
	    Line { Point1 => vector {0,ymin,0,1}, Point2 => vector {0,ymax,0,1}, "marker-end" => arr },
	    if is3d g then Line { Point1 => vector{0,0,zmin,1}, Point2 => vector {0,0,zmax,1}, "marker-end" => arr },
	    Perspective => p,
	    "stroke"=>"black", "stroke-width"=>"0.5%"
	    );
	axeslabels0 := gList(
	    -- we use GraphicsHtml here despite limitations of ForeignObject. could use GraphicsText instead
	    GraphicsHtml { symbol RefPoint => vector {xmax*1.06,0,0,1}, symbol HtmlContent => if instance(g.Axes,List) and #g.Axes>0 then g.Axes#0 else local x, FontSize => 0.08*min(rr_0,rr_1)},
	    GraphicsHtml { symbol RefPoint => vector {0,ymax*1.06,0,1}, symbol HtmlContent => if instance(g.Axes,List) and #g.Axes>1 then g.Axes#1 else local y, FontSize => 0.08*min(rr_0,rr_1)},
	    if is3d g then GraphicsHtml { symbol RefPoint => vector {0,0,zmax*1.06,1}, symbol HtmlContent => if instance(g.Axes,List) and #g.Axes>2 then g.Axes#2 else local z, FontSize => 0.08*min(rr_0,rr_1)},
	    Perspective => p,
	    );
	axes=svg axes0;
	axeslabels=svg axeslabels0;
	defsList = scanDefs axes0 | scanDefs axeslabels0;
	);
	-- put some extra blank space around picture
	margin := if g.?Margin then g.Margin else 0.1;
	r = { r#0-margin*rr, r#1+margin*rr }; rr = (1+2*margin)*rr;
    --
--    tag := graphicsId();
    classTag := "M2Svg";
    ss = ss | {
	"preserveAspectRatio" => "none",
--	"id" => tag,
	"style" => concatenate("width:",toString g.cache.Size_0,"em;",
	    "height:",toString g.cache.Size_1,"em;"
	    ),
	"viewBox" => concatenate between(" ",toString \ {r#0_0,-r#1_1,r#1_0-r#0_0,r#1_1-r#0_1}),
	};
    if is3d g or draggable g then ss = append(ss, "onmousedown" => "gfxMouseDown(event)"); -- TODO more customized: might want 2d background drag etc
    if is3d g then (
	classTag = classTag | " M2SvgDraggable";
	);
    ss = append(ss,"class" => classTag);
    if axes =!= null then ss = append(ss, axes);
    if axeslabels =!= null then ss = append(ss, axeslabels);
    ss = append(ss,main);
    defsList = unique ( defsList | scanDefs g );
    if #defsList>0 then ss=append(ss,svgDefs defsList);
    -- then autorotate button
    if animated g then (
	sizex := rr_0*min(0.5,1.5/g.cache.Size_0); sizey := rr_1*min(0.5,1.5/g.cache.Size_1); -- can't be larger than half the pic; default = 1.5em
	ss = append(ss,
	(svgElement GraphicsList) {
	    "transform" => "translate("|toString(r#0_0)|" "|toString(-r#1_1)|") scale("|toString sizex|" "|toString sizey|")",
	    "class" => "gfxauto",
	    "onclick" => "gfxToggleRotation(event)",
	    (svgElement Circle) { "cx" => "0.5", "cy" => "0.5", "r" => "0.45", "style" => "fill:white; stroke:black; stroke-width:0.05" },
	    (svgElement Polygon) { "class" => "gfxautoplay", "points" => "0.3,0.25 0.8,0.5 0.3,0.75", "style" => "stroke:none; fill:black" },
	    (svgElement Line) { "class" => "gfxautostop", "x1" => "0.3", "y1" => "0.25", "x2" => "0.3", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" },
	    (svgElement Line) { "class" => "gfxautostop", "x1" => "0.7", "y1" => "0.25", "x2" => "0.7", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" }
	    }
	));
    ss
    )

html GraphicsObject := g -> html SVG g;

-- now transformations
-- following 2 functions can be used to produce matrices to be fed to either
-- AnimMatrix (animation) or TransformMatrix (static)

rotation = args -> (
    args = sequence args;
    if #args>3 then error("Too many arguments");
    angle := args#0;
    threeD :=  #args === 3 or (#args === 2 and (( instance(args#1,Vector) and rank class args#1 === 3 ) or ( instance(args#1,Sequence) and #args#1 === 3 )));
    axis := promote(if threeD then if instance(args#1,Vector) then args#1 else vector toList args#1 else vector {0,0,1},RR);
    invr := 1/sqrt(axis_0^2+axis_1^2+axis_2^2);
    axis = invr*axis;
    cross := (axis#0)**transpose(axis#0);
    rot := cross + (sin angle) * matrix {{0,-axis_2,axis_1},{axis_2,0,-axis_0},{-axis_1,axis_0,0}} + (cos angle) * (1-cross);
    rot = rot ++ 1;
    if (#args==2 and threeD) or #args==1 then rot else (
	center := gParse last args;
	(translation(center))*rot*(translation(-center))
	)
    )
translation = vec -> (
    vec = gParse vec;
    matrix {{1,0,0,vec_0},{0,1,0,vec_1},{0,0,1,vec_2},{0,0,0,1}}
)
-- scaling = x -> matrix{{x,0,0,0},{0,x,0,0},{0,0,x,0},{0,0,0,1}}; -- sadly atm strokeWidth *does not* scale with scaling

determineSide = method()
determineSide GraphicsObject := x -> ()
determineSide GraphicsPoly := g -> (
    -- find first 3 coords
    coords := select(g.PointList, x -> not instance(x,String));
    if #coords<3 then ( remove(g.cache,Filter); return; );
    coords=apply(take(coords,3),x->gParse(x,g));
    coords = apply(coords, x -> (1/x_3)*x^{0,1});
    coords = {coords#1-coords#0,coords#2-coords#0};
    g.style#"visibility" = if coords#0_0*coords#1_1-coords#0_1*coords#1_0 < 0 then "hidden" else "visible";
    )

HypertextInternalLink = new Type of Hypertext -- could be useful elsewhere
toString HypertextInternalLink := net HypertextInternalLink := x -> (
    -- ideally we'd use "override" to get the tag, but...
    tag := (select(x, y -> instance(y,Option) and y#0==="id"))#0#1;
    "url(#"|tag|")"
)

svgFilter := new MarkUpType of HypertextInternalLink
addAttribute(svgFilter,svgAttr | {"x","y","width","height"})
svgFilter.qname="filter"
feGaussianBlur := new MarkUpType of Hypertext
addAttribute(feGaussianBlur,svgAttr|{"in","result","stdDeviation"})
feGaussianBlur.qname="feGaussianBlur";
feSpecularLighting := new MarkUpType of Hypertext
addAttribute(feSpecularLighting,svgAttr| {"result","specularExponent","lighting-color"})
feSpecularLighting.qname="feSpecularLighting"
fePointLight := new MarkUpType of Hypertext
addAttribute(fePointLight,svgAttr|{"x","y","z"})
fePointLight.qname="fePointLight"
feComposite := new MarkUpType of Hypertext
addAttribute(feComposite,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"})
feComposite.qname="feComposite"

filter = g -> (
    c := g.cache.owner;
    l := c.lights;
    p := c.PerspectiveMatrix;
    -- unrelated: pick up other filters from options (e.g., "fill"=>somegradient)
    g.cache.Filter = (if g.cache.?Filter then g.cache.Filter else {}) -- in rare cases (e.g., axes) that Filter doesn't exist
    | select(values g.style, y->instance(y,HypertextInternalLink));
    -- now main part
    if (g.?Blur and g.Blur != 0) or (#l > 0 and instance(g,GraphicsPoly)) then (
    	tag := graphicsId();
    	i:=0;
    	opts := { "id" => tag };
    	if g.?Blur then (
	    b := g.Blur;
	    opts = opts | { "x" => toString(-100*b)|"%", "y" => toString(-100*b)|"%", "width" => toString(100*(1+2*b))|"%", "height" => toString(100*(1+2*b))|"%" };
	    rng := g.cache.ViewPort; if rng =!= null then (
    	    	drng:=rng#1-rng#0;
    	    	r := b*min(drng_0,drng_1);
	    	g.cache.ViewPort={rng#0-vector{r,r},rng#1+vector{r,r}}; -- a bit of a hack
	    	opts = append(opts, feGaussianBlur { "in" => "SourceGraphic",
		    	"result" => "result"|toString i, "stdDeviation" => toString(0.5*r) } ); -- problem is, this should be updated dynamically as radius changes...
	    	i=i+1;
		)
    	    );
    	if is3d g and instance(g,GraphicsPoly) then (
    	    -- find first 3 coords
	    coords := select(g.PointList, x -> not instance(x,String));
    	    if #coords>=3 then (
	    	coords=apply(3,i->(xx:=gParse(coords#i,g);(1/xx_3)*xx^{0,1,2}));
	    	u:=coords#1-coords#0; v:=coords#2-coords#0; w:=vector{u_1*v_2-v_1*u_2,u_2*v_0-v_2*u_0,u_0*v_1-v_0*u_1}; w2:=w_0*w_0+w_1*w_1+w_2*w_2;
	    	if w_2<0 then w=-w; -- TODO better (no assumption on perspective) by using determineSide, cf js
	    	scan(l, gg -> (
	    	    	-- compute reflected coords
		    	light0 := gParse(gg.Center,gg);
		    	light := (1/light0_3)*light0^{0,1,2};
		    	lightrel := light-coords#0;
	    	    	sp := w_0*lightrel_0+w_1*lightrel_1+w_2*lightrel_2;
	    	    	c := 2*sp/w2;
		    	light = light - c*w;
		    	light = p*(light || vector {1});
		    	opts = opts | {
			    feSpecularLighting { "result" => "spec"|toString i, "specularExponent" => toString gg.Specular, "lighting-color" => if sp<0 then "black" else toString gg.style#"fill",
			    	fePointLight { "data-origin" => gg.cache.Options#"id", "x" => toString(light_0/light_3), "y" => toString(-light_1/light_3), "z" => toString(4*gg.Radius/light_3) } },
			    feComposite { "in" => "spec"|toString i, "in2" => "SourceGraphic", "operator" => "in", "result" => "clipspec"|toString i },
			    feComposite { "in" => (if i==0 then "SourceGraphic" else "result"|toString(i-1)),  "in2" => "clipspec"|toString i, "result" => "result"|toString i,
			    	"operator" => "arithmetic", "k1" => "0", "k2" => "1", "k3" => "1", "k4" => "0" }
			    };
	    	    	i=i+1;
	    	    	));
	    	);
	    );
    	g.cache.Options#"filter"=svgFilter opts;
    	g.cache.Filter=append(g.cache.Filter,g.cache.Options#"filter");
	)
    )

svgLinearGradient := new MarkUpType of HypertextInternalLink
addAttribute(svgLinearGradient,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"})
svgLinearGradient.qname="linearGradient"
svgRadialGradient := new MarkUpType of HypertextInternalLink
addAttribute(svgRadialGradient,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"} )
svgRadialGradient.qname="radialGradient"
svgStop := new MarkUpType of HypertextInternalLink
addAttribute(svgStop,svgAttr|{"offset"} )
svgStop.qname="stop"
linearGradient = true >> o -> stop -> (
    tag := graphicsId();
    svgLinearGradient ({
	"id" => tag,
    	o }
    | apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style }))
    )
radialGradient = true >> o -> stop -> (
    tag := graphicsId();
    svgRadialGradient ({
	"id" => tag,
    	o }
	| apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style }))
    )

svgMarker := new MarkUpType of HypertextInternalLink
addAttribute(svgMarker,svgAttr|{ "orient" => "auto", "markerWidth", "markerHeight", "refX", "refY", "markerUnits" => "userSpaceOnUse"})
svgMarker.qname="marker"

arrow = true >> o -> x -> (
    if x === () then x = 10. else x = numeric x;
    tag := graphicsId();
    svgMarker {
	"id" => tag,
	"markerWidth" => 1.5*x,
	"markerHeight" => 2*x,
	"refY" => x,
	svg(Polygon { symbol PointList => { vector {0,0}, vector {0,2*x}, vector {1.5*x,x} },
		Perspective => matrix {{1,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1}}*perspective(), -- eww
		"fill" => "black", "stroke" => "none", o })
	}
    )

-* TODO recreate at some point
gfxLabel = true >> o -> label -> (
    tag := graphicsId();
    f:=1; -- TEMP
--    s:="<marker id='"|tag|"' markerUnits='userSpaceOnUse' markerSizeX='"|toString(f*0.6*length label)|"' markerSizeY='"|toString f|"' refX='0' refY='0'>"; -- very approximate
    s:="<marker id='"|tag|"' markerSizeX='100' markerSizeY='100' refX='0' refY='0'>"; -- very approximate
    saveTransformMatrix := currentTransformMatrix;
    s=s|(svg new GraphicsText from (new GraphicsObject) ++ { "fill" => "black", "stroke" => "none" } ++ gParse o ++ { Point => vector {0,0}, Content => label });
    currentTransformMatrix = saveTransformMatrix;
    s=s|"</marker>";
    new GraphicsIdged from (tag,s)
    )
*-

-- note that the range is only where the curve actually lies, not the original range "r" provided.
-- the reason is that it's not clear how to force that original range (there are possible coordinate transformations etc)
plot = true >> o -> (P,r) -> (
    pkg := needsPackage "NumericalAlgebraicGeometry"; -- probably overkill
    sS := value pkg.Dictionary#"solveSystem";
    pkg2 := needsPackage "NAGtypes";
    Crd := pkg2.Dictionary#"Coordinates";
    R := ring P; -- R should have one or two variables
    if not instance(r,List) then error("incorrect ranges");
    if not instance(r#0,List) then r = { r };
    if #r>2 or (numgens R =!= #r and numgens R =!= #r+1) then error("incorrect number of variables / ranges");
    if numgens R === #r then R2 := coefficientRing R else R2 = (coefficientRing R) ( monoid [last gens R] );
    if (#r === 1) then ( r = r#0;
	if (o.?Mesh) then n := o.Mesh else n = 100;
	val := transpose apply(n+1, i -> (
		x := i*(r#1-r#0)/n+r#0;
		f := map(R2,R, matrix { if numgens R === 1 then { x } else { x, R2_0 } });
		y := if numgens R === 1 then { f P } else sort apply(sS { f P }, p -> first p#Crd); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(y, yy -> if abs imaginaryPart yy < 1e-6 then vector { x, realPart yy })));
	GraphicsList { "fill"=>"none", Axes=>gens R,
	    symbol Contents => apply(val, v -> Path { flag:=true; PointList => flatten apply(v, w -> if w === null then (flag=true; {}) else first({ if flag then "M" else "L", w },flag=false))}),
	    o }
	) else (
	if (o.?Mesh) then n = o.Mesh else n = 10;
	val = table(n+1,n+1,(i,j)->(
		x := i*(r#0#1-r#0#0)/n+r#0#0;
		y := j*(r#1#1-r#1#0)/n+r#1#0;
		f := map(R2,R, matrix { if numgens R === 2 then { x,y } else { x, y, R2_0 } });
		z := if numgens R === 2 then { f P } else sort apply(sS { f P }, p -> first p#Crd); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(z, zz -> if abs imaginaryPart zz < 1e-6 then vector { x, y, realPart zz })));
	GraphicsList { Axes=>gens R,
	    symbol Contents => flatten flatten table(n,n,(i,j) -> for k from 0 to min(#val#i#j,#val#(i+1)#j,#val#i#(j+1),#val#(i+1)#(j+1))-1 list (
		    if val#i#j#k === null or val#(i+1)#j#k === null or val#i#(j+1)#k === null or val#(i+1)#(j+1)#k === null then continue;
		    Polygon { PointList => { val#i#j#k, val#(i+1)#j#k, val#(i+1)#(j+1)#k, val#i#(j+1)#k } } ) ), -- technically this is wrong -- the quad isn't flat, we should make triangles
	o }
    )
)

-- existing types that get a new output
horiz := p -> gList prepend(Line{[0,0],[p#0,0]},apply(#p,i->Line{[0,-1-i],[p#i,-1-i]}))
vert := p -> gList prepend(Line{[0,0],[0,-p#0]},apply(#p,i->Line{[i+1,0],[i+1,-p#i]}))
html Partition := p -> if #p===0 then "&varnothing;" else html gList(
    vert conjugate p,horiz p,Size=>vector{2*p#0,2*#p},"stroke-width"=>0.05)
-- TODO: graphs

beginDocumentation()
multidoc ///
 Node
  Key
   VectorGraphics
  Headline
   A package to produce SVG graphics
  Description
   Text
    {\bf VectorGraphics} is a package to produce SVG 2d and 3d graphics.
    All usable types are descendents of the type GraphicsObject, and are self-initializing.
    Coordinates can be entered as vectors in $\mathbb{R}^2$, $\mathbb{R}^3$ or $\mathbb{R}^4$
    ($\mathbb{R}^4$ is projective coordinates); alternatively, one can enter them as sequences.
    With the default perspective matrix,
    the x axis points to the right, the y axis points up, and the z axis points towards the viewer.
    All types are option tables, i.e., their arguments are options. There are two types of options:
    VectorGraphics options, that are symbols (e.g., {\tt Radius} for circles);
    and styling options, which are CSS style options,
    and which are {\bf strings} (e.g., {\tt "fill"} for fill color).
    {\bf VectorGraphics} does not use units (coordinates are dimensionless).
    In @ TO {Standard} @ mode, the graphical objects are not directly visible; to export them to SVG
    in order to embed them into a web page, use @ TO {html} @. In @ TO {WebApp} @ mode, the graphical objects
    are shown as output.
 Node
  Key
   GraphicsObject
  Headline
   The ancestor class of all VectorGraphics objects
 Node
  Key
   GraphicsPoly
  Headline
   The ancestor class of complex VectorGraphics objects
 Node
  Key
   GraphicsList
  Headline
   A list of VectorGraphics objects
  Description
   Text
    A class that represents a list of @ TO {VectorGraphics} @ objects, displayed together. See also @ TO{gList} @.
 Node
  Key
   Circle
  Headline
   An SVG circle
  Description
   Text
    An SVG circle. The two compulsory options are Center (coordinates of the center) and Radius (radius).
    In 3d, gives a decent approximation of a sphere.
   Example
    Circle{Center=>vector {10,10},Radius=>50,"fill"=>"green","stroke"=>"none"}
    Circle{[10,10],50} -- equivalent syntax for coordinates
 Node
  Key
   Line
  Headline
   An SVG line
  Description
   Text
    A simple SVG line. The two compulsory options are Point1 and Point2, which are vectors (or sequences) describing the two endpoints.
   Example
    Line{Point1=>vector{0,0},Point2=>vector{2,1},"stroke"=>"green"}
    Line{[0,0],[2,1],"stroke-width"=>0.1} -- simplified syntax
 Node
  Key
   Light
  Headline
   A source of light
  Description
   Text
    A source of light for a 3d SVG picture.
    This corresponds to the SVG "specular" lighting, use the property Specular. The location is given by Center.
    By default a Light is invisible (it has opacity 0) and is unaffected by matrix transformations outside it (Static true).
   Example
    Light{"fill"=>"yellow","opacity"=>1}
    v={[74.5571, 52.0137, -41.6631],[27.2634, -29.9211, 91.4409],[-81.3041, 57.8325, 6.71156],[-20.5165, -79.9251, -56.4894]};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    c={"red","green","blue","yellow"};
    tetra=gList(apply(0..3,i->Polygon{f#i,"fill"=>c#i,"stroke"=>"none"}),
	Light{[110,0,0],Radius=>10,"opacity"=>"1"},ViewPort=>{vector{-110,-100},vector{110,100}},
	Size=>40,TransformMatrix=>rotation(-1.5,[4,1,0]))
  Caveat
   Do not use the same Light object multiple times within the same @ TO {GraphicsObject} @.
 Node
  Key
   GraphicsNode
  Headline
   xxx
  Description
   Text
    [only use with gNode]
  Caveat
   Required GraphicsNode objects should appear exactly once within a given @ TO {GraphicsObject} @.
 Node
  Key
   Ellipse
  Headline
   An SVG ellipse
  Description
   Text
    An SVG ellipse. The three compulsory options are Center (coordinates of the center) and RadiusX, RadiusY (radii).
   Example
    Ellipse{Center=>vector{10,10},RadiusX=>50,RadiusY=>20,"stroke"=>"none","fill"=>"red"}
    Ellipse{[10,10],50,20,"stroke"=>"blue"} -- equivalent syntax
  Caveat
   Does not really make sense in a 3d context.
 Node
  Key
   Path
  Headline
   An SVG path
  Description
   Text
    An SVG path. It follows the syntax of SVG paths, except successive commands must be grouped together in a list called PointList.
   Example
    c=Circle{Radius=>0.1,"fill"=>"black"};
    v1=gNode([0,0],c,Draggable=>true); v2=gNode([0,1],c,Draggable=>true); v3=gNode([2,1],c,Draggable=>true); v4=gNode([1,2],c,Draggable=>true);
    gList(Path{{"M",v1,"C",v2,v3,v4}},v1,v2,v3,v4)
 Node
  Key
   Polygon
  Headline
   An SVG polygon
  Description
   Text
    An SVG polygon. The coordinates must form a list called PointList. (the difference with Polyline is that the last coordinate is reconnected to the first)
   Example
    Polygon{PointList=>{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   Polyline
  Headline
   An SVG sequence of lines
  Description
   Text
    An SVG sequence of lines. The coordinates must form a list called PointList. (the difference with Polygon is that the last coordinate is not reconnected to the first)
   Example
    Polyline{PointList=>{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GraphicsText
  Headline
   SVG text
  Description
   Text
    Some SVG text. The location of the start of the text is given by the option RefPoint.
    The text itself is the option TextContent (a string).
    The text can be "stroke"d or "fill"ed.
    Font size should be specified with FontSize.
   Example
    GraphicsText{TextContent=>"Test","stroke"=>"red","fill"=>"none","stroke-width"=>0.5}
    gList(GraphicsText{[0,0],"P",FontSize=>14},GraphicsText{[7,0],"AUL",FontSize=>10})
  Caveat
   Currently, cannot be rotated. (coming soon)
 Node
  Key
   gList
  Headline
    Group together VectorGraphics objects
  Description
   Text
    gList(a,b,...,c, options) results in a new @ TO{GraphicsList} @ object containing a,b,...,c
    and the given options.
   Example
    a=gList(Line{[-100, 15, 78], [-9, 100, 4]},
	Line{[-96, -49, -100], [46, -100, 52]},
	Line{[-100, -42, -51], [59, 100, 76]},
	Line{[-100, 66, 54], [83, -100, -27]})
    b=gList(Line{[-30, 100, 20], [9, -100, 8]},
	Line{[-78, -73, -100], [-64, 84, 100]},
	"stroke"=>"red")
    gList(a,b,Size=>30)
 Node
  Key
   viewPort
  Headline
    ViewPort of view port
  Description
   Text
    viewPort gives the range of view port occupied by a @ TO {VectorGraphics} @ object, as computed by the package.
    See also @ TO{ViewPort} @.
  Caveat
    At the moment viewPort does not take into account the width of "stroke"s.
 Node
  Key
   rotation
  Headline
   Computes a rotation matrix
  Usage
   rotation ( angle, axis, center)
   rotation ( angle, center)
  Description
   Text
    Produces a rotation encoded as a 4x4 matrix that can be used as an argument to @TO{TransformMatrix}@ or @TO{AnimMatrix}@.
    For a 3d rotation, use 3d vectors for axis and center.
    For a 2d rotation, use a 2d vector for the center.
    In both cases, the center is optional.
 Node
  Key
   translation
  Headline
   Computes a translation matrix
  Description
   Text
    Produces a translation encoded as a 4x4 matrix that can be used as an argument to @TO{TransformMatrix}@ or @TO{AnimMatrix}@.
    The vector can be 2d or 3d.
   Example
    v={vector{7.456, 5.201, -4.166}, vector{2.7263, -2.992, 9.144},
       vector{-8.130, 5.783, 0.671}, vector {-2.052, -7.993, -5.649}};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    tetra=gList(apply(4,i->Polygon{f#i,"fill"=>"white"}))
    g = memoize(n -> if n==0 then tetra else gList apply(4,i->g(n-1)++{TransformMatrix=>translation(2^(n-1)*v#i)}))
    apply(4,g)
  Usage
   translation ( vector )
 Node
  Key
   OneSided
  Description
   Text
    A property of @ TO{GraphicsPoly} @ 3d objects, means that polygons must be drawn only if they are facing the correct way.
 Node
  Key
   ViewPort
  Headline
   Fix the view port
  Description
   Text
    An option to fix manually the view port range of a @ TO {VectorGraphics} @ object.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
    See also @ TO{viewPort} @ and @ TO{Margin} @.
 Node
  Key
   Size
  Headline
   Set the size of a picture
  Description
   Text
    An option to fix the size of the @ TO {VectorGraphics} @ object in line width units.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
    Can be either a vector { width, height } or a number (diagonal).
 Node
  Key
   Perspective
  Headline
   Set the amount of perspective
  Description
   Text
    A 4x4 matrix that is applied to 3d coordinates for perspective.
    One can instead provide a real number $p$, which is equivalent to placing the screen
    centered at $z=0$ and the viewer at $(0,0,p)$.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
 Node
  Key
   AnimMatrix
  Headline
   Create a rotation animation matrix
  Description
   Text
    An option to create a rotation animation for the @ TO {VectorGraphics} @ 3d object.
    The value can be a single 4x4 matrix, or a list which is cycled.
    The syntax {\tt n => ...} can be used to repeat a sequence n times (where {\tt 0} means infinity).
    The animation automatically loops (use {\tt 0 => \{ \}} to stop!)
    In order for the animation to work, {\tt VectorGraphics.css} and {\tt VectorGraphics.js} must be included in the web page.
   Example
    (anim1=rotation(0.1,[0,0,1],[0,0]); anim2=rotation(-0.1,[0,0,1],[0,0]);); anim3 = { 5 => {5 => anim1, 5 => anim2}, 10 => anim1 };
    gList(Polygon{{[-1,0],[1,0.1],[1,-0.1]},"fill"=>"red",AnimMatrix=>anim1},Circle{[1,0],0.1},Circle{[0,0],1})
    gList(Polygon{{[-1,0],[1,0.1],[1,-0.1]},"fill"=>"red",AnimMatrix=>anim3},Circle{[1,0],0.1},Circle{[0,0],1})
 Node
  Key
   TransformMatrix
  Headline
   Create a rotation matrix
  Description
   Text
    An option to rotate the coordinates of the @ TO {VectorGraphics} @ 3d object.
    Must be a 4x4 matrix (projective coordinates).
   Example
    a=Polygon{{[-1,0],[1,0.1],[1,-0.1]},"fill"=>"red"}
    gList(a,a++{TransformMatrix=>rotation(2*pi/3)})
 Node
  Key
   Blur
  Headline
   An option to blur a VectorGraphics object
  Description
   Text
    This corresponds to the feGaussianBlur SVG filter.
    The value is the amount of blurriness relative to the size of the object.
  Caveat
    In animated 3d, the amount of blurriness does not vary as the size varies.
 Node
  Key
   Static
  Headline
   An option to make a VectorGraphics object unmoving
  Description
   Text
    The @ TO {VectorGraphics} @ 3d object is unaffected by matrix transformations of its ancestors.
 Node
  Key
   linearGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the linearGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "x1", "y1", "x2", "y2") are used to determine the orientation of the gradient.
   Example
    Ellipse{[60,60],40,30, "fill"=>linearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   radialGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the radialGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "cx", "cy", "r", "fx", "fy") are used to position the gradient.
   Example
    Ellipse{[60,60],40,30, "fill"=>radialGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   plot
  Headline
   Draws a curve or surface
  Description
   Text
    Draws a curve or surface defined implicitly or explicitly by a polynomial.
    The first argument is a polynomial, the second is a (list of) range(s) of variable(s).
    If the number of ranges is equal to the number of variables of the polynomial, the graph of the polynomial
    is drawn. If it is one fewer, then the zero set of the polynomial is drawn.
    The option Mesh specifies the number of sampled values of the variables.
   Example
    R=RR[x,y];
    P=y^2-(x+1)*(x-1)*(x-2);
    plot(P,{-2,3},"stroke-width"=>0.05,Size=>40,"stroke"=>"red")
 Node
  Key
   Axes
  Headline
   An option to draw axes
 Node
  Key
   Margin
  Headline
   An option to specify the margin
  Description
   Text
    The margin is proportional to the size of the image.
    It increases the view port beyond the value returned by @ TO{viewPort} @ or set by @ TO{ViewPort} @.
   Example
    Circle{"fill"=>"red","stroke"=>"none",Margin=>0}
    Circle{"fill"=>"red","stroke"=>"none",Margin=>0.5}
 Node
  Key
   arrow
  Headline
   A marker to add to paths
  Description
   Text
    Must be used as styling options "marker-start", "marker-mid" or "marker-end", to add an arrow to a path.
    Argument is size of arrow.
   Example
    Polyline{PointList=>{[0,0],[50,50],[0,100],[50,150]},"stroke"=>"yellow","stroke-width"=>5,"marker-end"=>arrow(10,"fill"=>"orange"),Margin=>0.3}
 Node
  Key
   GraphicsHtml
  Headline
   Html content inside a VectorGraphics object
  Description
   Text
    Some arbitrary HTML content, specified by the option HtmlContent (a @ TO{Hypertext} @ object or other content to render in HTML).
  Caveat
   Due to a limitation of <foreignObject>, coordinates are rounded to the nearest integer. So use large enough coordinate systems.
 Node
  Key
   SVG
  Headline
   hypertext SVG item
 Node
  Key
   GraphicsType
  Headline
   A particular type of type used by VectorGraphics, similar to SelfInitializingType.
///
undocumented {
    Contents, TextContent, HtmlContent, RefPoint, Specular, Radius, Point1, Point2, PointList, Mesh, FontSize, RadiusX, RadiusY,
    (symbol ++, GraphicsObject, List), (symbol ?,GraphicsObject,GraphicsObject), (symbol SPACE,GraphicsType,List),
    (expression, GraphicsObject), (html,GraphicsObject), (net,GraphicsObject), (toString,GraphicsObject), (short,GraphicsObject),
    (NewOfFromMethod,GraphicsType,GraphicsObject,VisibleList), (NewFromMethod,SVG,GraphicsObject),
}

end--

-- ex of use
gr=linearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")};
gList(Ellipse{[0,0],90,30,"stroke"=>"none","fill"=>gr,Blur=>0.3},GraphicsText{[-65,-7],"Macaulay2",FontSize=>25,"stroke"=>"black","fill"=>"white"},Size=>20)

a=Circle{"fill"=>"yellow","stroke"=>"green",Size=>vector{1,1}}
b=Line{[10,10],[20,50],"stroke"=>"black"}
c=Circle{[50,50],50,"fill"=>"blue","fill-opacity"=>0.25}
d=Ellipse{[60,60],40,30, "fill"=>"blue", "stroke"=>"grey"}
e=Polyline{{[0,0],[100,100],[100,50]},"fill"=>"pink","stroke"=>"green"}
f=Polygon{{[0,10],[100,10],[90,90],[0,80]},"stroke"=>"red","fill"=>"white",Draggable=>true}
gList (f,a,b,c,d,e,Draggable=>true)
-- or
rgb={"red","green","blue"};
scan(rgb, x -> (value x <- Circle{"fill"=>x,"stroke"=>"black",Size=>vector{0.8,0.8},Margin=>0}))
value\rgb
R=QQ[x_red,x_green,x_blue]
describe R
x_red^2-x_green^2
factor oo

-- or
z=Polygon{{[0,0],[0,50],[50,50],[50,0]},"fill"=>"white"}
b1=Path{{"M", [0, 25], "Q", [25, 25], [25, 0], "M", [50, 25], "Q", [25, 25], [25, 50]},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
b2=Path{{"M", [0, 25], "Q", [25, 25], [25, 0], "M", [50, 25], "Q", [25, 25], [25, 50]},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
b=gList(z,b1,b2,Size=>vector{2,2},Margin=>0)
a1=Path{{"M", [50, 25], "Q", [25, 25], [25, 0], "M", [0, 25], "Q", [25, 25], [25, 50]},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
a2=Path{{"M", [50, 25], "Q", [25, 25], [25, 0], "M", [0, 25], "Q", [25, 25], [25, 50]},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
a=gList(z,a1,a2,Size=>vector{2,2},Margin=>0)
tile = (I,i,j)->(if m_(i+1,j+1)%I == 0 then if c_(i+1,j+1)%I==0 then () else a else b);
tiledRow = (I,i)->new RowExpression from apply(n,j->tile(I,i,j));
loopConfig = I->new ColumnExpression from apply(k,i->tiledRow(I,i)); -- no such a thing as ColumnExpression. there should

-- or
barside1=Path{{"M",[80,60,100],"L",[80,55,100],"L",[220,55,100],"L",[220,60,100],"Z"},"fill"=>"#222","stroke-width"=>0}; -- stroke-width shouldn't be necessary
triangle1=Path{{"M",[-50,160,2],"L",[0,80,2],"L",[50,160,2],"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
triangle2=Path{{"M",[30,160,98],"L",[80,80,98],"L",[130,160,98],"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
edge1=Path{{"M",[30,160,98],"L",[30,160,102],"L",[80,80,102],"L",[80,80,98],"Z"},"fill"=>"#4080e0","stroke-width"=>1};
edge2=Path{{"M",[130,160,98],"L",[130,160,102],"L",[80,80,102],"L",[80,80,98],"Z"},"fill"=>"#4080e0","stroke-width"=>1};
bartop=Path{{"M",[80,55,98],"L",[80,55,102],"L",[220,55,102],"L",[220,55,98],"Z"},"fill"=>"#aaa","stroke-width"=>0}; -- stroke-width shouldn't be necessary
thread=Path{{"M",[80,55,100],"L",[80,80,100],"Z"},"stroke"=>"#111","stroke-width"=>0.5,"stroke-opacity"=>0.8};
gList{barside1,triangle1,triangle2,edge1,edge2,bartop,thread}

-- draggable tetrahedron
circ = Circle{Radius=>5,"fill"=>"black"};
v=apply(([74.5571, 52.0137, -41.6631],[27.2634, -29.9211, 91.4409],[-81.3041, 57.8325, 6.71156],[-20.5165, -79.9251, -56.4894]),
    v1 -> gNode(v1,circ,Draggable=>true));
c={"red","green","blue","yellow"};
vv={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
triangles=apply(0..3,i->Path{{"M",vv#i#0,"L",vv#i#1,"L",vv#i#2,"Z"},"fill"=>c#i,OneSided=>true});
gList(triangles,v,Light{[100,0,0],"opacity"=>"1",Radius=>10,Draggable=>true},ViewPort=>{vector{-100,-150},vector{150,150}},Size=>50,TransformMatrix=>rotation(-1.5,[0,1,0]))

-- dodecahedron
vertices={vector{-137.638,0.,26.2866},vector{137.638,0.,-26.2866},vector{-42.5325,-130.902,26.2866},vector{-42.5325,130.902,26.2866},vector{111.352,-80.9017,26.2866},vector{111.352,80.9017,26.2866},vector{-26.2866,-80.9017,111.352},vector{-26.2866,80.9017,111.352},vector{-68.8191,-50.,-111.352},vector{-68.8191,50.,-111.352},vector{68.8191,-50.,111.352},vector{68.8191,50.,111.352},vector{85.0651,0.,-111.352},vector{-111.352,-80.9017,-26.2866},vector{-111.352,80.9017,-26.2866},vector{-85.0651,0.,111.352},vector{26.2866,-80.9017,-111.352},vector{26.2866,80.9017,-111.352},vector{42.5325,-130.902,-26.2866},vector{42.5325,130.902,-26.2866}};
faces={{14,9,8,13,0},{1,5,11,10,4},{4,10,6,2,18},{10,11,7,15,6},{11,5,19,3,7},{5,1,12,17,19},{1,4,18,16,12},{3,19,17,9,14},{17,12,16,8,9},{16,18,2,13,8},{2,6,15,0,13},{15,7,3,14,0}};
centers=apply(faces,f->1/5*sum(f,i->vertices#i));
steps=30;
dodeca=apply(faces,centers,(f,c)->Polygon{apply(f,j->vertices#j),"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
label=apply(#vertices,i->GraphicsText{vertices#i,toString i});
dodecasplit=apply(faces,centers,(f,c)->Polygon{apply(f,j->vertices#j),
	AnimMatrix=>apply(steps,j->rotation(2*pi/5/steps*4*min(j/steps,1-j/steps),c,c)*translation(0.075*sin(2*pi*j/steps)*c)),
	"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
d=gList(dodecasplit,"fill-opacity"=>0.65,AnimMatrix=>rotation(0.02,[1,2,3]));
d1=gList(d,TransformMatrix=>translation[200,0,0]); -- using alternate syntax of Array instead of Vector
d2=gList(d,TransformMatrix=>translation[-200,0,0]);
gList(d1,d2,ViewPort=>{vector{-400,-400},vector{400,400}},SizeY=>25,"stroke-width"=>2)

p=random splice{0..11};

-- icosahedron
vertices={vector{0.,0.,-95.1057},vector{0.,0.,95.1057},vector{-85.0651,0.,-42.5325},vector{85.0651,0.,42.5325},vector{68.8191,-50.,-42.5325},vector{68.8191,50.,-42.5325},vector{-68.8191,-50.,42.5325},vector{-68.8191,50.,42.5325},vector{-26.2866,-80.9017,-42.5325},vector{-26.2866,80.9017,-42.5325},vector{26.2866,-80.9017,42.5325},vector{26.2866,80.9017,42.5325}};
faces={{1,11,7},{1,7,6},{1,6,10},{1,10,3},{1,3,11},{4,8,0},{5,4,0},{9,5,0},{2,9,0},{8,2,0},{11,9,7},{7,2,6},{6,8,10},{10,4,3},{3,5,11},{4,10,8},{5,3,4},{9,11,5},{2,7,9},{8,6,2}};
icosa=apply(faces,f->Polygon{apply(f,j->vertices#j),"fill"=>"gray","stroke"=>"none"});
i=gList(toSequence icosa,TransformMatrix=>matrix{{0.7,0,0,0},{0,0.7,0,0},{0,0,0.7,0},{0,0,0,1}})

rnd = () -> random(-1.,1.); cols={"red","green","blue","yellow","magenta","cyan"};
gList(i, apply(toSequence cols, c -> Light{100*vector{1.5+rnd(),rnd(),rnd()},Radius=>10,"opacity"=>1,"fill"=>c,Specular=>20,AnimMatrix=>rotation(0.02,[rnd(),rnd(),rnd()])}),ViewPort=>{vector{-200,-200},vector{200,200}},Size=>40)

subdivide = (v,f) -> (
    u := v#0;
    c := u_0*u_0+u_1*u_1+u_2*u_2;
    e := unique flatten apply(f,x->{sort{x#0,x#1},sort{x#0,x#2},sort{x#1,x#2}});
    mid := apply(e, x -> (u=0.5*(v#(x#0)+v#(x#1)); r:=sqrt(c/(u_0*u_0+u_1*u_1+u_2*u_2)); r*u));
    ff := flatten apply(f, x -> (
	    i:=#v+position(e,y->y==sort{x#0,x#1});
	    j:=#v+position(e,y->y==sort{x#0,x#2});
	    k:=#v+position(e,y->y==sort{x#1,x#2});
	    {{x#0,i,j},{x#1,i,k},{x#2,j,k},{i,j,k}}
	    ));
    (v|mid,ff)
    )
(v2,f2)=subdivide(vertices,faces);
(v3,f3)=subdivide(v2,f2);
sph=apply(f3,f->Polygon{apply(f,j->v3#j),"stroke"=>"white","stroke-width"=>0.01,"fill"=>"gray"});
gList(sph, apply(toSequence cols, c -> Light{100*vector{1.5+rnd(),rnd(),rnd()},Radius=>10,"fill"=>c,Specular=>10,AnimMatrix=>rotation(0.02,[rnd(),rnd(),rnd()])}),ViewPort=>{vector{-200,-200},vector{200,200}},Size=>40)

-- explicit plot
R=RR[x,y]; P=0.1*(x^2-y^2);
gList(plot(P,{{-10,10},{-10,10}},Mesh=>15,"stroke-width"=>0.05,"fill"=>"gray"),Light{[200,0,-500],Specular=>10,"fill"=>"rgb(180,0,100)"},Light{[-200,100,-500],Specular=>10,"fill"=>"rgb(0,180,100)"},Size=>50,Axes=>false)

-- implicit plot
R=RR[x,y];
P=y^2-(x+1)*(x-1)*(x-2);
plot(P,{-2,3},"stroke-width"=>0.05,SizeY=>25,"stroke"=>"red")

-- Schubert calculus
a=gList(Line{[-100, 15, 78], [-9, 100, 4]},Line{[-96, -49, -100], [46, -100, 52]},Line{[-100, -42, -51], [59, 100, 76]},Line{[-100, 66, 54], [83, -100, -27]})
b=gList(Line{[-30, 100, 20], [9, -100, 8]},Line{[-78, -73, -100], [-64, 84, 100]},"stroke"=>"red")
c=gList(Polygon{{[-100,100,100],[-100,-100,100],[-100,-100,-100],[-100,100,-100]}},
		  Polygon{{[100,100,100],[100,-100,100],[100,-100,-100],[100,100,-100]}},
		  Polygon{{[100,-100,100],[-100,-100,100],[-100,-100,-100],[100,-100,-100]}},
		  Polygon{{[100,100,100],[-100,100,100],[-100,100,-100],[100,100,-100]}},
		  Polygon{{[100,100,-100],[100,-100,-100],[-100,-100,-100],[-100,100,-100]}},
		  Polygon{{[100,100,100],[100,-100,100],[-100,-100,100],[-100,100,100]}},
		  "stroke"=>"black","fill"=>"grey", "opacity"=>"0.25")
gList(a,b,c,Size=>30)

--
n=10;
v1=apply(n,i->vector {cos(2*pi*i/n),sin(2*pi*i/n),0.1});
v2=apply(n,i->vector {cos(2*pi*i/n),sin(2*pi*i/n),-0.1});
l=apply(n,i->Polygon{{v1#i,v2#i,v2#((i+1)%n),v1#((i+1)%n)},"fill"=>"hsl("|toString(360.*i/n)|",100%,50%)"});
a=gList(l,AnimMatrix=>rotation(0.2,vector{0,0,1}));
m=50;r=apply(m,i->rotation(0.05,vector{cos(2*pi*i/m),sin(2*pi*i/m),0}));
b=gList(a,AnimMatrix=>r)
--c=gList(a,TransformMatrix=>(map(RR^3,RR^3,0.5)++1)*rotation(2,vector{1,2,3}),AnimMatrix=>r)
--gList(b,c)

-- stars
n=100;
speed=100;
far=-10000;
screen=1000;
stars=apply(n,i->(
z=speed*(random(far,screen)//speed);
Circle{[random(-200,200),random(-200,200),z],10,"fill"=>"yellow","stroke"=>"none",Blur=>0.3, -- TODO: make blurriness dynamically depend on size
AnimMatrix=>{((screen-z)//speed)=>translation [0,0,speed],translation [0,0,far-screen],((-far+z)//speed)=>translation [0,0,speed]}}
));
gList(stars,ViewPort=>{vector{-100,-100},vector{100,100}})
style(SVG oo,"background"=>"black")

-- removed (might be added back if correctly implemented in 3d)
 Node
  Key
   Rectangle
  Headline
   An SVG rectangle
  Description
   Text
    An SVG rectangle. The SW coordinate is given as Point, the difference between NE and SW corners is given as GraphicsSize.
   Example
    Rectangle{(10,10),(20,50),"fill"=>"pink","stroke"=>"black"} -- first argument is Point, second GraphicsSize
  Caveat
   Rectangle can only be used in 2d. Use Polygon for 3d.

