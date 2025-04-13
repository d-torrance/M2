beginDocumentation()

apiKey := "dcb31709b452b1cf9dc26972add0fda6"

desmosPlot = method()
desmosPlot String := str -> show HTML {
    SCRIPT {
	"src" => "https://www.desmos.com/api/v1.9/calculator.js?apiKey=" | apiKey},
    DIV {
	"id" => "calculator",
	"style" => "width: 100vh; height: 100vh;"
	},
    SCRIPT { "var elt = document.getElementById('calculator');
	var calculator = Desmos.GraphingCalculator(elt);
	calculator.setExpression({latex: '" | str | "'});"}}
desmosPlot RingElement := f -> desmosPlot(toString f | " = 0")


R = QQ[x,y]
desmosPlot(y^2 - (x^3 + x + 1))
