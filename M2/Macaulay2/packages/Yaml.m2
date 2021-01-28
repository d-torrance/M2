newPackage(
    "Yaml",
    Version => "0.1",
    Date => "2021-01-28",
    Headline => "import and export YAML",
    Authors => {{
	Name => "Doug Torrance",
	Email => "dtorrance@piedmont.edu",
	HomePage => "https://webwork.piedmont.edu/~dtorrance"
	}},
    PackageExports => {"Parsing"}
)

export {
    "yaml"
}

yaml = method()
yaml Thing := yaml @@ toString
yaml Net := identity
yaml VisibleList := L -> stack apply(L, x -> "- " | yaml x)
yaml HashTable := H -> (
    w := max \\ length \ keys H + 2;
    stack apply(keys H, k -> pad(toString k | ":", w) | yaml H#k)
)
