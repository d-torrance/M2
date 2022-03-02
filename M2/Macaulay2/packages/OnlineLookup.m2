-- -*- coding: utf-8 -*-
newPackage(
        "OnlineLookup",
        Version => "0.5",
        Date => "Mar 2, 2022",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "Look up mathematical information online",
	Keywords => {"System"},
        DebuggingMode => false,
	AuxiliaryFiles => false,
	PackageImports => {"Text"}
        )

export {"oeis","urlEncode"}

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

beginDocumentation()
multidoc ///
 Node
  Key
   oeis
  Headline
   OEIS lookup
  Description
   Text
    This function looks up the argument (a list of integers or a string) in the Online Encyclopedia of Integer Sequences (@HREF "http://oeis.org/"@).
   Example
    oeis {1,3,31,1145}
///
