-- some helper code:

getName = x -> (new OptionTable from x).Name
commaAnd = x -> concatenate (
     if #x > 2 then (between (", ", drop(x, -1)), ", and ", x#-1)
     else between (" and ", x))

changesHelper = method(Options => {Certification => false})
changesHelper String := opt -> pkgname -> changesHelper({pkgname}, opt)
changesHelper List := opt -> pkgnames -> (
    scan(pkgnames, pkgname -> (
	    pkg := needsPackage pkgname;
	    << "LI { "
	    << if opt.Certification then "star, \" \", " else ""
	    << "TO \"" << pkgname << "::" << pkgname, "\", a package by "
	    << commandAnd apply((options pkg).Authors, getName)
	    << " for " << (options pkg).Headline << ", has been "
	    << if opt.Certification then "certified" else "added"
	    << ".\" },"
	    << endl)))

changesHelper "Graphs"


x = {"PruneComplex", "CohomCalg", "Topcom", "ReflexivePolytopesDB", "AbstractToricVarieties", "TestIdeals", "AlgebraicSplines", "TriangularSets", "Chordal", "Tropical", "SymbolicPowers", "Complexes", "GroebnerWalk", "RandomMonomialIdeals", "Matroids", "NumericalImplicitization"}
y = for i in x list needsPackage i
getName = x -> (new OptionTable from x).Name
commaAnd = x -> concatenate (
     if #x > 2 then (between (", ", drop(x, -1)), ", and ", x#-1)
     else between (" and ", x))
for i in y do (
     << ///	       	    LI { TO "///
	  << i
	  << ///::///
	  << i
	  << ///", ", a package by ///
          << commaAnd (for a in (options i).Authors list getName a)
	  << /// for ///
	  << (options i).Headline
	  << ///, has been added." },/// 
	  << endl
     )
