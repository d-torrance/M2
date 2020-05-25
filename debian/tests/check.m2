print "====================================================="
print "running basic tests..."
print "====================================================="
cmd = "M2 --check 1 -q --stop -E \"exit 0\""
print cmd
if run cmd == 0 then print "basic tests passed" else error "basic tests failed"

skip = {-- waiting for topcom to enter debian
	"Polyhedra", "ToricInvariants", "Truncations", "Matroids",
	"ReflexivePolytopesDB", "SymbolicPowers", "Topcom",
	-- https://github.com/Macaulay2/M2/issues/1173
	"StatePolytope",
	-- waiting for phcpack (#820848)
	"PHCpack", "MonodromySolver",
	-- uses factor, so waiting for flint 2.6
	"QuillenSuslin",
	-- bertini is non-free, so we will always skip this
	"Bertini",
	-- getting "assignment to protected global variable 'MaxRoundTol'"
	-- error which I can't seem to reproduce.  skip for now
	"SumsOfSquares"
	}
pkgs = separate_" " version#"packages"
scan(pkgs, pkg -> if not member(pkg, skip) then (
		print("=====================================================");
		print("checking " | pkg | "...");
		print("=====================================================");
		check pkg
		)
	)
