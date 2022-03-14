-- -*- coding: utf-8 -*-
newPackage(
        "OnlineLookup",
        Version => "0.6",
        Date => "Mar 2, 2022",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "Look up mathematical information online",
	Keywords => {"System"},
        DebuggingMode => false,
	AuxiliaryFiles => true,
	CacheExampleOutput => true,
	OptionalComponentsPresent => try getWWW "http://blogs.unimelb.edu.au/paul-zinn-justin/" then true else false,
	PackageImports => {"Text"}
        )

export {"oeis","urlEncode","isc"}

debug Core

-- TODO might need more encoding. see also html.m2
urlEncode = s -> if s === null then s else (
     s = replace("\\s", "%20", s);
     s = replace(",", "%2C", s);
     s = replace("/", "%2F", s);
     s
     )

oeisHTTP := "http://oeis.org";
oeisHTTPS := "https://oeis.org";

oeis = method(TypicalValue => NumberedVerticalList,
    Options => {Limit => 100, Position => 0})
oeis VisibleList := o -> L -> oeis (demark(",",toString\L),o)
oeis String := o -> search -> (
    if not (options OnlineLookup).OptionalComponentsPresent then return null;
    url:=oeisHTTP|"/search?q="|urlEncode search|"&fmt=text&start="|o.Position|"&n="|o.Limit; -- limit the number of results
    www := last splitWWW getWWW url;
    ans := select("(?<=^%N ).*$",www);    
    NumberedVerticalList apply(ans, line -> SPAN(
            blank := regex(" ",line);
            if blank === null then line -- shouldn't happen
            else (
                pos := blank#0#0;
                seq := substring(0,pos,line);
                {HREF {oeisHTTPS|"/"|seq,seq} ,substring(pos,line)}
            )))
    )
-- e.g. oeis {1,2,7,42}

isc = method()

isc Constant := isc @@ numeric
isc RR := x -> (
    s := format(0,-1,1000,1000,"",x);
    isc substring(s,0,#s-1) -- remove last digit for now... TODO better
    )
isc String := s -> (
    if not (options OnlineLookup).OptionalComponentsPresent then return null;
    url := "http://wayback.cecm.sfu.ca/cgi-bin/isc/lookup?number="|urlEncode s|"&lookup_type=simple";
    www := last splitWWW getWWW url;
    ans := select("(?<=<PRE>)[\\s\\S]*?(?=</PRE>)",www);
    if #ans == 0 then return {};
    ans = first ans;
    lst := select(separate("\n\n",ans),x->#x>3 and substring(x,0,3)=="<B>");
    VerticalList apply(lst,x->SPAN replace("<.*?>","",x))
    )


beginDocumentation()
multidoc ///
 Node
  Key
   OnlineLookup
  Headline
   Look up mathematical information online
  Description
   Text
    The purpose of this package is to collect helper functions that allow to query web sites for mathematical
    information and format it into Macaulay2 output.
    At present, it contains two such functions, @TO{oeis}@ and @TO{isc}@, but more will be implemented in the future.
 Node
  Key
   oeis
   (oeis, String)
   (oeis, VisibleList)
   [oeis, Limit]
   [oeis, Position]
  Headline
   OEIS lookup
  Description
   Text
    This function looks up the argument (a list of integers or a string) in the Online Encyclopedia of Integer Sequences
    (@HREF "http://oeis.org/"@).
   Example
    oeis {1,3,31,1145}
   Text
    Optional arguments @TT"Limit"@ and @TT"Position"@ allow to control the maximum amount of listed answers.
   Example
    L = apply(5,n->n!);
    oeis (L,Limit=>5)
    oeis (L,Limit=>1,Position=>2)
 Node
  Key
   isc
   (isc, String)
   (isc, RR)
  Headline
   ISC lookup
  Description
   Text
    This function looks up the argument (a real number or a string) in the Inverse Symbolic Calculator.
    (@HREF "http://wayback.cecm.sfu.ca/projects/ISC/"@).
   Example
    isc (2*sqrt pi)
 Node
  Key
   urlEncode
  Headline
   URL encoding
  Description
   Text
    This function provides a minimal encoding of a string in order to be used as part of a URL.
///
