debug LanguageServer

-- header with Content-Type, as written by addHeader
assert Equation(parseLSP addHeader "{}", (2, "{}"))

-- Content-Length only, with and without whitespace
assert Equation(parseLSP "Content-Length: 2\r\n\r\n{}", (2, "{}"))
assert Equation(parseLSP "Content-Length:2\r\n\r\n{}", (2, "{}"))

-- body is returned as is; start takes the first len bytes
assert Equation(parseLSP "Content-Length: 2\r\n\r\n{}Content-Length: 3",
    (2, "{}Content-Length: 3"))
assert Equation(parseLSP "Content-Length: 7\r\n\r\n{}", (7, "{}"))

-- errors: no Content-Length, incomplete header
assert(try parseLSP "Content-Type: foo\r\n\r\n{}" then false else true)
assert(try parseLSP "Content-Length: 2\r\n" then false else true)
