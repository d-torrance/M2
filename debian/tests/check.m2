skip = {"Macaulay2Doc", -- https://github.com/Macaulay2/M2/issues/1157
	-- waiting for topcom to enter debian
	"Polyhedra", "ToricInvariants", "Truncations", "Matroids",
	"ReflexivePolytopesDB", "SymbolicPowers", "Topcom",
	-- waiting for normaliz (#960614)
	"Normaliz", "NormalToricVarieties", "MultiplierIdeals"
	}
pkgs = separate_" " version#"packages"
scan(pkgs, pkg -> if not member(pkg, skip) then (
		print("checking " | pkg | "...");
		check pkg
		)
	)
