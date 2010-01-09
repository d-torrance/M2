-- -*- coding: utf-8 -*-
newPackage(
	"BuchbergerForBrp",
    	Version => "1.0", 
    	Date => "April 28, 2005",
    	Authors => {
	     {Name => "Jane Doe", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "an example Macaulay2 package",
	AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {}
exportMutable {}

-- Run Buchberger on the ideal I generated by Brps, return GB (Brps)

R = ZZ/2[x,y,z]
poly = new Brp from {{1,0,0}}

-- list of input polynomials
F = {poly}  -- list of input

-- pairs of indexes referring to polynomials, changing as we find more pairs
listOfPairs = {}

-- growing list of generators for basis 
G ={}

-- FPs have negative indices 
-- n FPs, m Brps
-- listOfPairs = every FP with every Brp, every Brp with every Brp
G = F
nextIndex = #F 

while notEmpty listOfPairs
  pair = first listOfPairs
  listOfPairs = remove(listOfPairs, pair)
  S = SPolynomial(pair)
  remainder = reduce (S,G)
  if (remainder != 0 ) then (
    nextIndex += 1
    append(G, remainder) 
    makeMorePairs(G, remainder)
  )
)

minimizeBasis(G)
  



beginDocumentation()
document { 
	Key => BuchbergerForBrp,
	Headline => "BuchbergerForBrp making use of bit-wise representation",
	EM "PackageTemplate", " is an example package which can
	be used as a template for user packages."
	}
TEST ///
  assert(false)
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp
