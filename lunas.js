/*
 * Currently not implemented:
 * table.__newindex
 * table.__mode
 * table.__tostring
 * table.__pairs
 * table.__ipairs
 * table.__gc
 */

 /*
  * Things to implement:
	* vargs (...)
	* ipairs / pairs / for loop for them
	* multiple function return -> assigning to multiple variables
	*/

var TokenType = {
	EOF : "EOF",
	ERROR : "Error",

	LEFT_PAREN : "(",
	RIGHT_PAREN : ")",
	LEFT_BRACE : "{",
	RIGHT_BRACE : "}",
	LEFT_BRACKET : "[",
	RIGHT_BRACKET : "]",
	COMMA : ",",
	DOT : ".",
	MINUS : "-",
	PLUS : "+",
	SLASH : "/",
	STAR : "*",
	COLON : ":",
	SLASH_SLASH : "//",
	CELL : "#",
	PERCENT : "%",
	CARET : "^",
	DOT_DOT : "..",

	TILDE : "~",
	TILDE_EQUAL : "~=",
	EQUAL : "=",
	EQUAL_EQUAL : "==",
	GREATER : ">",
	GREATER_EQUAL : ">=",
	LESS : "<",
	LESS_EQUAL : "<=",

	IDENTIFIER : "Identifier",
	STRING : "String",
	NUMBER : "Number",

	AND : "and",
	ELSE : "else",
	FALSE : "false",
	FUNCTION : "function",
	FOR : "for",
	IF : "if",
	NIL : "nil",
	OR : "or",
	RETURN : "return",
	TRUE : "true",
	LOCAL : "local",
	WHILE : "while",
	NOT : "not",
	END : "end",
	THEN : "then",
	ELSEIF : "elseif",
	DO : "do"
}

var keywords = {
	"and" : TokenType.AND,
	"else" : TokenType.ELSE,
	"false" : TokenType.FALSE,
	"function" : TokenType.FUNCTION,
	"for" : TokenType.FOR,
	"if" : TokenType.IF,
	"nil" : TokenType.NIL,
	"or" : TokenType.OR,
	"return" : TokenType.RETURN,
	"true" : TokenType.TRUE,
	"local" : TokenType.LOCAL,
	"while" : TokenType.WHILE,
	"not" : TokenType.NOT,
	"end" : TokenType.END,
	"then" : TokenType.THEN,
	"elseif" : TokenType.ELSEIF,
	"do" : TokenType.DO
}

var scanner = {}

scanner.createToken = function(type) {
	return {
		line : scanner.line,
		type : type,
		start : scanner.start,
		length : scanner.position - scanner.start
	}
}

scanner.decideToken = function(expected, a, b) {
	return scanner.createToken(scanner.match(expected) ? a : b)
}

scanner.setSource = function(source) {
	scanner.source = source
	scanner.position = 0
	scanner.start = 0
	scanner.line = 1
	scanner.hadError = false
}

scanner.error = function(message, where) {
	scanner.hadError = true
	console.error(`[line ${scanner.line}] Error${where ? " " + where : ""}: ${message}`)

	return scanner.createToken(TokenType.ERROR)
}

scanner.isAtEnd = function() {
	return scanner.position >= scanner.source.length
}

scanner.peek = function() {
	if (scanner.isAtEnd()) {
		return "\0"
	}

	return scanner.source.charAt(scanner.position)
}

scanner.peekNext = function() {
	if (scanner.position >= scanner.source.length - 1) {
		return "\0"
	}

	return scanner.source.charAt(scanner.position + 1)
}

scanner.advance = function() {
	scanner.position++
	return scanner.source.charAt(scanner.position - 1)
}

scanner.match = function(expected) {
	if (scanner.peek() != expected) {
		return false
	}

	scanner.position++
	return true
}

scanner.skipWhitespace = function() {
	while (true) {
		var c = scanner.peek()

		switch (c) {
			case " ":
			case "\r":
			case "\t":
				scanner.advance()
				break

			case "\n":
				scanner.advance()
				scanner.line++;
				break

			// TODO: multiline comment
			case "-":
				if (scanner.peekNext() == "-") {
					while (!scanner.isAtEnd() && scanner.peek() != "\n") {
						scanner.advance()
					}

					break
				} else {
					return
				}

			default:
				return
		}
	}
}

function isDigit(ch) {
	return "0123456789".indexOf(ch) !== -1
}

function isAlpha(ch){
  return ch.length === 1 && (ch >= "a" && ch <= "z" || ch >= "A" && ch <= "Z" || ch == "_");
}

function isAlphaNumeric(ch) {
	return isDigit(ch) || isAlpha(ch)
}

scanner.scanToken = function() {
	scanner.skipWhitespace()
	scanner.start = scanner.position

	if (scanner.isAtEnd()) {
		return scanner.createToken(TokenType.EOF)
	}

	var char = scanner.advance()

	if (isDigit(char)) {
		while (isDigit(scanner.peek())) {
			scanner.advance();
		}

		if (scanner.peek() == '.' && isDigit(scanner.peekNext())) {
			scanner.advance();

			while (isDigit(scanner.peek())) {
				scanner.advance();
			}
		}

		return scanner.createToken(TokenType.NUMBER)
	}

	if (isAlpha(char)) {
		while (isAlphaNumeric(scanner.peek())) {
			scanner.advance()
		}

		var name = scanner.source.substring(scanner.start, scanner.position)
		var keyword = keywords[name]

		if (keyword) {
			return scanner.createToken(keyword)
		}

		return scanner.createToken(TokenType.IDENTIFIER)
	}

	switch (char) {
		case "(": return scanner.createToken(TokenType.LEFT_PAREN)
		case ")": return scanner.createToken(TokenType.RIGHT_PAREN)
		case "{": return scanner.createToken(TokenType.LEFT_BRACE)
		case "}": return scanner.createToken(TokenType.RIGHT_BRACE)
		case "[": return scanner.createToken(TokenType.LEFT_BRACKET)
		case "]": return scanner.createToken(TokenType.RIGHT_BRACKET)
		case ":": return scanner.createToken(TokenType.COLON)
		case "+": return scanner.createToken(TokenType.PLUS)
		case "-": return scanner.createToken(TokenType.MINUS)
		case "*": return scanner.createToken(TokenType.STAR)
		case ",": return scanner.createToken(TokenType.COMMA)
		case "#": return scanner.createToken(TokenType.CELL)
		case "%": return scanner.createToken(TokenType.PERCENT)
		case "^": return scanner.createToken(TokenType.CARET)

		case "/": return scanner.decideToken("/", TokenType.SLASH_SLASH, TokenType.SLASH)
		case ">": return scanner.decideToken("=", TokenType.GREATER_EQUAL, TokenType.GREATER)
		case "<": return scanner.decideToken("=", TokenType.LESS_EQUAL, TokenType.LESS)
		case "~": return scanner.decideToken("=", TokenType.TILDE_EQUAL, TokenType.TILDE)
		case "=": return scanner.decideToken("=", TokenType.EQUAL_EQUAL, TokenType.EQUAL)
		case ".": return scanner.decideToken(".", TokenType.DOT_DOT, TokenType.DOT)

		case "\"": {
			while (scanner.peek() != "\"" && !scanner.isAtEnd()) {
	      if (scanner.peek() == "\n") {
					line++;
				}

	      scanner.advance()
	    }

	    if (scanner.isAtEnd()) {
	      return scanner.error("Unterminated string")
	      return
	    }

	    scanner.advance()
			return scanner.createToken(TokenType.STRING)
		}
	}

	return scanner.error("Unexpected character", char)
}

scanner.next = function() {
	scanner.previous = scanner.current
	scanner.current = scanner.scanToken()

	return scanner.current
}

var parser = {}

parser.error = function(message, token) {
	parser.hadError = true
	console.error(`[line ${token ? token.line : "?"}] Error${token ? " at " + getLiteral(token) : ""}: ${message}`)
}

parser.consume = function(expected, error) {
	if (!parser.match(expected)) {
		parser.error(error, scanner.current)
	}

	return scanner.previous
}

parser.advance = function() {
	return scanner.next()
}

parser.match = function(type) {
	if (scanner.current.type == type) {
		parser.advance()
		return true
	}

	return false
}

parser.matches = function(types) {
	for (var i in types) {
		if (parser.match(types[i])) {
			return true
		}
	}

	return false
}

function getLiteral(token) {
	return scanner.source.substring(token.start, token.start + token.length)
}

parser.parsePrimary = function() {
	if (parser.match(TokenType.IDENTIFIER)) {
		if (parser.multipleAssign) {
			var literal = getLiteral(scanner.previous)
			parser.usedFunctions[literal] = true

			return literal
		}

		var code = []

		do {
			var literal = getLiteral(scanner.previous)
			parser.usedFunctions[literal] = true

			if (code.length == 0 && scanner.current.type == TokenType.COMMA) {
				parser.multipleAssign = true
				code.push("[ ")
			}

			code.push(literal)

			if (scanner.current.type == TokenType.COMMA) {
				code.push(", ")
			}
		} while (parser.match(TokenType.COMMA) && parser.match(TokenType.IDENTIFIER))

		if (parser.multipleAssign) {
			code.push(" ]")
		}

		return code.join("")
	}

	if (parser.match(TokenType.NUMBER)) {
		return parseFloat(getLiteral(scanner.previous))
	}

	if (parser.match(TokenType.STRING)) {
		return getLiteral(scanner.previous)
	}

	if (parser.match(TokenType.TRUE)) {
		return "true"
	}

	if (parser.match(TokenType.FALSE)) {
		return "false"
	}

	if (parser.match(TokenType.NIL)) {
		return "null"
	}

	if (parser.match(TokenType.LEFT_BRACE)) {
		if (parser.match(TokenType.RIGHT_BRACE)) {
			return "{}"
		}

		var decided = false
		var array = false
		var expression = []

		while (!parser.match(TokenType.RIGHT_BRACE)) {
			var hadBracket = false

			if (parser.match(TokenType.LEFT_BRACKET)) {
				if (!decided) {
					decided = true
					array = false

					expression.push("{ ")
				} else if (array) {
					parser.error("Syntax error");
				}

				hadBracket = true
			}

			var key = parser.parseEquality()

			if (hadBracket) {
				parser.consume(TokenType.RIGHT_BRACKET, "] expected")
			}

			if (parser.match(TokenType.COMMA) || parser.match(TokenType.RIGHT_BRACE)) {
				if (!decided) {
					decided = true
					array = true

					expression.push("[ 0, ")
				} else if (!array) {
					parser.error("Syntax error");
				}

				expression.push(key)

				if (scanner.previous.type == TokenType.RIGHT_BRACE) {
					expression.push(" ]\n")
					break
				} else {
					expression.push(", ")
				}
			} else {
				parser.consume(TokenType.EQUAL, "= expected")

				if (!decided) {
					decided = true
					array = false

					expression.push("{ ")
				} else if (array) {
					parser.error("Syntax error");
				}

				expression.push(key)
				expression.push(" : ")
				expression.push(parser.parseExpression())

				if (parser.match(TokenType.RIGHT_BRACE)) {
					expression.push(" }\n")
					break
				} else {
					parser.consume(TokenType.COMMA, ", expected")
					expression.push(", ")
				}
			}
		}

		return expression.join("")
	}

	if (parser.match(TokenType.FUNCTION)) {
		parser.consume(TokenType.LEFT_PAREN, ") expected")
		var call = [ "function(" ]

		if (scanner.current.type != TokenType.RIGHT_PAREN) {
			do {
				call.push(parser.parseExpression())

				if (scanner.current.type == TokenType.COMMA) {
					call.push(", ")
				}
			} while (parser.match(TokenType.COMMA))
		}

		parser.consume(TokenType.RIGHT_PAREN, ") expected")

		call.push(")")
		call.push(" {\n")
		call.push(parser.parseBlock())

		return call.join("")
	}

	if (parser.match(TokenType.LEFT_PAREN)) {
		var expr = parser.parseExpression()
		parser.consume(TokenType.RIGHT_PAREN, ") expected")

		return [ "(", expr, ")" ].join("")
	}

	console.error("Unexpected token " + scanner.current.type)
	throw new Error("Something went badly wrong!")

	scanner.next()

	return ""
}

parser.parseCall = function() {
	var expr = parser.parsePrimary()

	while (parser.match(TokenType.LEFT_BRACKET)) {
		var index = parser.parseExpression()

		parser.consume(TokenType.RIGHT_BRACKET, "] expected")
		parser.usedFunctions["__index"] = true

		if (scanner.current.type == TokenType.EQUAL) {
			expr = [ expr, "[" , index, "]" ].join("")
		} else {
			if (index.charAt(0) == "\"") {
				expr = [ "__index(", expr, ", " , index, ")" ].join("")
			} else {
				expr = [ "__index(", expr, ", \"", index, "\")" ].join("")
			}
		}
	}

	while (true) {
		if (parser.match(TokenType.LEFT_PAREN)) {
			var call = []
			parser.metaUnaryMethods["__call"] = true

			call.push("__call(")
			call.push(expr)
			call.push(", [")

			if (parser.addSelf) {
				call.push(parser.addSelf)
				parser.addSelf = null
			}

			if (scanner.current.type != TokenType.RIGHT_PAREN) {
				do {
					call.push(parser.parseExpression())

					if (scanner.current.type == TokenType.COMMA) {
						call.push(", ")
					}
				} while (parser.match(TokenType.COMMA))
			}

			parser.consume(TokenType.RIGHT_PAREN, ") expected")

			call.push("])")
			expr = call.join("")
		} else if (parser.match(TokenType.DOT) || parser.match(TokenType.COLON)) {
			if (scanner.previous.type == TokenType.COLON) {
					parser.addSelf = expr
			}

			var name = getLiteral(parser.consume(TokenType.IDENTIFIER, "property name expected"))
			parser.usedFunctions["__index"] = true

			if (scanner.current.type == TokenType.EQUAL) {
				expr = [ expr, ".", name ].join("")
			} else {
				 expr = [ "__index(", expr, ", \"", name, "\")" ].join("")
			}
		} else {
			break
		}
	}

	return expr
}

var metaMethods = {
	"+" : "__add",
	"-" : "__sub",
	"*" : "__mul",
	"/" : "__div",
	"//" : "__idiv",
	"^" : "__pow",
	"%" : "__mod",
	".." : "__concat"
}

var unaryMetaMethods = {
	"-" : "__unm",
	"#" : "__len",
	"not" : "__not",
	"call" : "__call"
}

parser.parseUnary = function() {
	if (parser.matches([ TokenType.MINUS, TokenType.NOT, TokenType.CELL ])) {
		var name = unaryMetaMethods[getLiteral(scanner.previous)]
		parser.metaUnaryMethods[name] = true
		return [ name, "(", parser.parseUnary(), ")" ].join("")
	}

	return parser.parseCall()
}

parser.parseMultiplication = function() {
	var expr = parser.parseUnary()

	while (parser.matches([ TokenType.STAR, TokenType.SLASH, TokenType.SLASH_SLASH, TokenType.PERCENT, TokenType.CARET ])) {
		var name = metaMethods[getLiteral(scanner.previous)]
		parser.metaBinaryMethods[name] = true
		expr = [ name, "(", expr, ", ", parser.parseUnary(), ")" ].join("")
	}

	return expr
}

parser.parseAddition = function() {
	var expr = parser.parseMultiplication()

	while (parser.matches([ TokenType.PLUS, TokenType.MINUS, TokenType.DOT_DOT ])) {
		var name = metaMethods[getLiteral(scanner.previous)]
		parser.metaBinaryMethods[name] = true
		expr = [ name, "(", expr, ", ", parser.parseUnary(), ")" ].join("")
	}

	return expr
}

parser.parseComparison = function() {
	var expr = parser.parseAddition()

	while (parser.matches([ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL ])) {
		expr = [ expr, " ", getLiteral(scanner.previous), " ", parser.parseAddition() ].join("")
	}

	return expr
}

parser.parseEquality = function() {
	var expr = parser.parseComparison()

	while (parser.matches([ TokenType.EQUAL_EQUAL, TokenType.TILDE_EQUAL ])) {
		parser.usedFunctions["__eq"] = true
		expr = [ scanner.previous.type == TokenType.TILDE_EQUAL ? "!__eq(" : "__eq(", expr, ", ",  parser.parseComparison(), ")" ].join("")
	}

	return expr
}

parser.parseAnd = function() {
	var expr = parser.parseEquality()

	while (parser.match(TokenType.AND)) {
		expr = [ expr, " && ", parser.parseEquality() ].join("")
	}

	return expr
}

parser.parseOr = function() {
	var expr = parser.parseAnd()

	while (parser.match(TokenType.OR)) {
		expr = [ expr, " || ", parser.parseAnd() ].join("")
	}

	return expr
}

parser.parseAssigment = function() {
	var expr = parser.parseOr()

	if (parser.match(TokenType.EQUAL)) {
		if (parser.multipleAssign) {
			var code = [ expr, " = [ " ]

			do {
				code.push(parser.parseExpression())

				if (scanner.current.type == TokenType.COMMA) {
					code.push(", ")
				}
			} while (parser.match(TokenType.COMMA))

			code.push(" ]")
			parser.multipleAssign = false

			return code.join("")
		} else {
			return [ expr, " = ", parser.parseAssigment() ].join("")
		}
	}

	return expr
}

parser.parseExpression = function() {
	return parser.parseAssigment()
}

parser.parseExpressionStatement = function() {
	return parser.parseExpression() + "\n"
}

parser.parseEmptyBlock = function(a, b) {
	var block = []
	var oldDepth = parser.depth

	parser.depth += "\t"

	var type = scanner.current.type

	while (type != TokenType.END && type != TokenType.EOF && type != a && type != b) {
		block.push(parser.depth + parser.parseStatement())
		type = scanner.current.type
	}

	parser.depth = oldDepth
	block.push("}")

	return block.join("")
}

parser.parseBlock = function(a, b) {
	var code = parser.parseEmptyBlock(a, b)

	if (a && scanner.current.type == a) {
	} else if (b && scanner.current.type == b) {
	} else {
		parser.consume(TokenType.END, "end expected to close the block")
	}

	return code
}

parser.parseFunction = function() {
	var name = []
	var lastName
	var call
	var addSelf = false

	name.push(getLiteral(parser.consume(TokenType.IDENTIFIER, "Function name expected")))

	while (parser.match(TokenType.DOT) || parser.match(TokenType.COLON)) {
		if (scanner.previous.type == TokenType.COLON) {
			addSelf = true
		}

		name.push(".")
		lastName = getLiteral(parser.consume(TokenType.IDENTIFIER, "Function name expected"))
		name.push(lastName)
	}

	name = name.join("")
	parser.consume(TokenType.LEFT_PAREN, ") expected")

	if (lastName) {
		call = [ name, " = function(" ]
	} else {
		call = [ "function ", name, "(" ]
	}

	if (scanner.current.type != TokenType.RIGHT_PAREN) {
		if (addSelf) {
			addSelf = false
			call.push("self,")
		}

		do {
			call.push(parser.parseExpression())

			if (scanner.current.type == TokenType.COMMA) {
				call.push(", ")
			}
		} while (parser.match(TokenType.COMMA))
	} else if (addSelf) {
		call.push("self")
	}

	parser.consume(TokenType.RIGHT_PAREN, ") expected")

	call.push(")")
	call.push(" {\n")
	call.push(parser.parseBlock())
	call.push("\n")

	return call.join("")
}

parser.parseStatement = function() {
	if (parser.match(TokenType.IF)) {
		var condition = parser.parseExpression()
		parser.consume(TokenType.THEN, "then expected after condition")

		var body = parser.parseBlock(TokenType.ELSE, TokenType.ELSEIF)
		var statement = [ "if (", condition, ") {\n", body ]

		while (parser.match(TokenType.ELSEIF)) {
			var condition = parser.parseExpression()
			parser.consume(TokenType.THEN, "then expected after condition")

			var body = parser.parseEmptyBlock(TokenType.ELSE, TokenType.ELSEIF)
			statement.push([ " else if (", condition, ") {\n", body ].join(""))
		}

		if (parser.match(TokenType.ELSE)) {
			statement.push([ " else {\n", parser.parseEmptyBlock() ].join(""))
		}

		return statement.join("")
	}

	if (parser.match(TokenType.WHILE)) {
		var condition = parser.parseExpression()
		parser.consume(TokenType.DO, "DO expected after condition")

		return [ "while (", condition, ") {\n", parser.parseBlock() ].join("")
	}

	if (parser.match(TokenType.FOR)) {
		var local = parser.match(TokenType.LOCAL)
		var name = getLiteral(parser.consume(TokenType.IDENTIFIER, "Variable name expected"))

		parser.consume(TokenType.EQUAL, "= expected")

		var from = parser.parseExpression()
		parser.consume(TokenType.COMMA, ", expected")
		var to = parser.parseExpression()
		var step = 1

		if (parser.match(TokenType.COMMA)) {
			step = parseFloat(parser.parseExpression())
		}

		parser.consume(TokenType.DO, "do expected")

		var body = parser.parseBlock()

		return [ "for (", local ? "var " + name : name, " = ", parseFloat(from), "; ", name,
			step > 0 ? " < " : " > ", parseFloat(to), "; ", name, " += ", step, ") {\n", body ].join("")
	}

	if (parser.match(TokenType.RETURN)) {
		var next = scanner.current.type

		if (next == TokenType.END || next == TokenType.ELSE || next == TokenType.ELSEIF) {
			return "return []\n"
		} else {
			var code = [ "return [ ", parser.parseExpression() ]

			while (parser.match(TokenType.COMMA)) {
				code.push(", ")
				code.push(parser.parseExpression())
			}

			code.push(" ]\n")

			return code.join("")
		}
	}

	if (parser.match(TokenType.LOCAL)) {
		if (parser.match(TokenType.FUNCTION)) {
			return parser.parseFunction()
		}

		var code = [ "var " ]
		var name = getLiteral(parser.consume(TokenType.IDENTIFIER, "Expected local variable name"))
		var num = 0

		if (parser.match(TokenType.COMMA)) {
			code.push("[ ")
		}

		code.push(name)

		if (scanner.previous.type == TokenType.COMMA) {
			do {
				num ++
				code.push(", ")
				code.push(getLiteral(parser.consume(TokenType.IDENTIFIER, "Expected local variable name")))
			} while (parser.match(TokenType.COMMA))
		}

		if (num > 0) {
			code.push(" ]")
		}

		if (parser.match(TokenType.EQUAL)) {
			code.push(" = ")

			if (num > 0) {
				code.push("[ ")
			}

			code.push(parser.parseExpression())

			while (parser.match(TokenType.COMMA)) {
				code.push(", ")
				code.push(parser.parseExpression())
			}

			if (num > 0) {
				code.push(" ]")
			}
		}

		code.push("\n")
		return code.join("")
	}

	if (parser.match(TokenType.FUNCTION)) {
		return parser.parseFunction()
	}

	return parser.parseExpressionStatement()
}

function getKeyByValue(object, value) {
  return Object.keys(object).find(key => object[key] === value);
}

var lunas = {}

lunas.compile = function(source) {
	scanner.setSource(source)
	parser.hadError = false
	parser.depth = ""
	parser.usedFunctions = []
	parser.metaBinaryMethods = []
	parser.metaUnaryMethods = []
	parser.multipleAssign = false

	var js = []
	scanner.next()

	while (true) {
		if (scanner.current.type == TokenType.EOF) {
			if (parser.hadError) {
				return null
			}

			var data = []

			for (var name in parser.metaBinaryMethods) {
				var op = getKeyByValue(metaMethods, name)

				if (name == "__concat") {
					op = "+"
				} else if (name == "__pow") {
					op = "**"
				}

				data.push(`function ${name}(a, b) {
	if (typeof a === "object" && typeof a.__metatable === "object" && typeof a.__metatable.${name} === "function") {
		return a.__metatable.${name}(a, b)
	} else {
		return ${name == "__idiv" ? `(a/b>>0)` : `a ${op} b`}
	}
}\n`)
			}

			for (var name in parser.metaUnaryMethods) {
				var op = getKeyByValue(unaryMetaMethods, name)

				if (name == "__call") {
					data.push(`function __call(a, args) {
	if (typeof a === "object" && typeof a.__metatable === "object" && typeof a.__metatable.__call === "function") {
		return a.__metatable.__call.apply(null, args)
	} else {
		return a.apply(null, args)
	}
}\n`)
				} else {
					data.push(`function ${name}(a) {
	if (typeof a === "object" && typeof a.__metatable === "object" && typeof a.__metatable.${name} === "function") {
		return a.__metatable.${name}(a)
	} else {
		return ${name == "__len" ? `(typeof a === "object" ? Object.keys(a).length : a.length - 1)` : (name == "__not" ? "!a": `${op} a`)}
	}
}\n`)
				}
			}

			for (var name in parser.usedFunctions) {
				var fn = std[name]

				if (fn) {
					data.push(fn)
				}
			}

			return data.join("") + js.join("")
		}

		js.push(parser.parseStatement())
	}
}

var std = {}

std.print = "\nprint = console.log\n"
std.getmetatable = `\nfunction getmetatable(t) {
	if (typeof t === "object") {
		return t.__metatable
	}

	return null
}\n`

std.setmetatable = `\nfunction setmetatable(t, m) {
	if (typeof t === "object") {
		t.__metatable = m
	}
}\n`

std.__index = `\nfunction __index(a, i) {
	if (typeof a === "object" && a[i] == null && typeof a.__metatable === "object" && typeof a.__metatable.__index === "function") {
		return a.__metatable.__index(a, i)
	} else {
		return a[i]
	}
}\n`

std.__eq = `\nfunction __eq(a, b) {
	if (typeof a === "object" && typeof b === "object" && typeof a.__metatable === "object"
			&& a.__metatable == b.__metatable) {

		return a.__metatable.__eq(a, b)
	} else {
		return a == b
	}
}\n`

std.type = `\nfunction type(o) {
	if (o == null || o == undefined) {
		return "nil"
	} else if (typeof o == "object") {
		return "table"
	}

	return typeof o
}\n`

// Maaagic
std.unpack = `\nfunction unpack(o) {
	return o
}\n`