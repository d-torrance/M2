--- status: DRAFT
--- author(s): from doc2, MES
--- notes: 

document { 
     Key => characters,
     Headline => "get characters from a string",
     Usage => "characters s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  List => {"of the characters in the string ", TT "s"}
	  },
     "The characters are represented by strings.",
     PARA{},
     EXAMPLE "characters \"asdf\"",
     PARA{},
     "utf8 characters are correctly treated:",
     PARA{},
     EXAMPLE "s = \"π ≈ 3.14159\"; characters s",
     PARA{},
     "To get the one-byte characters of the string, use ", TO "toList", " instead:",
     PARA{},
     EXAMPLE "toList s",
     SeeAlso => {String}
     }

