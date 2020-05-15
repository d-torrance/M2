skip = {"Macaulay2Doc" -- https://github.com/Macaulay2/M2/issues/1157
	}
pkgs = separate_" " version#"packages"
scan(pkgs, pkg -> if not member(pkg, skip) then (
		print("checking " | pkg | "...");
		check pkg
		)
	)
