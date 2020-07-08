print "====================================================="
print "running basic tests..."
print "====================================================="
cmd = "M2 --silent --check 1 -q --stop -E \"exit 0\""
print cmd
if run cmd == 0 then print "basic tests passed" else error "basic tests failed"

rootDir = "M2/Macaulay2/tests/"
dirs = {"normal", "slow"}
scan(dirs, dir -> (
		print "=====================================================";
		print("running " | dir | " tests...");
		print "=====================================================";
		fullDir = rootDir | dir;
		tests = sort select(readDirectory(rootDir | dir),
			file -> match("m2$", file));
		tests = delete("interrupt-handling.m2", tests);
		cmd = "M2 --silent --stop -q --no-debug --no-randomize ";
		cmd = cmd | "-e 'path=join(path,{\"" | fullDir | "\"})' < ";
		scan(tests, test -> (
				tmpFile := temporaryFileName();
				output := " > " | tmpFile | ".out 2>&1";
				print("testing: " | test);
				result := run(cmd | fullDir | "/" | test |
					output );
				if result != 0 then
				error(get(tmpFile | ".out"));
				removeFile(tmpFile | ".out")
				)
			)
		)
	)

skip = {-- waiting for phcpack (#820848)
	"PHCpack", "MonodromySolver",
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
