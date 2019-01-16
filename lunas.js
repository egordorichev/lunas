var TokenType = {
	EOF : "EOF",
	ERROR : "Error",

	LEFT_PAREN : "(",
	RIGHT_PAREN : ")",
	LEFT_BRACE : "{",
	RIGHT_BRACE : "}",
	COMMA : ",",
	DOT : ".",
	MINUS : "-",
	PLUS : "+",
	SLASH : "/",
	STAR : "*",

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
	ELSEIF : "elseif"
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
	"elseif" : TokenType.ELSEIF
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
				// Ignore whitespace.
				break

			case "\n":
				scanner.advance()
				scanner.line++;
				break

			case "/":
				if (scanner.peekNext() == "/") {
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
		case "-": return scanner.createToken(TokenType.MINUS)
		case "+": return scanner.createToken(TokenType.PLUS)
		case "*": return scanner.createToken(TokenType.STAR)
		case ".": return scanner.createToken(TokenType.DOT)
		case ",": return scanner.createToken(TokenType.COMMA)
		case "/": return scanner.createToken(TokenType.SLASH)
		case "=": return scanner.createToken(TokenType.EQUAL)

		case ">": return scanner.decideToken("=", TokenType.GREATER_EQUAL, TokenType.GREATER)
		case "<": return scanner.decideToken("=", TokenType.LESS_EQUAL, TokenType.LESS)
		case "~": return scanner.decideToken("=", TokenType.TILDE_EQUAL, TokenType.TILDE)
		case "=": return scanner.decideToken("=", TokenType.EQUAL_EQUAL, TokenType.EQUAL)

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
		return getLiteral(scanner.previous)
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

parser.parseUnary = function() {
	if (parser.matches([ TokenType.MINUS, TokenType.NOT ])) {
		var literal = getLiteral(scanner.previous)

		if (literal == "not") {
			literal = "!"
		}

		return [ literal, parser.parseUnary() ].join("")
	}

	return parser.parsePrimary()
}

parser.parseMultiplication = function() {
	var expr = parser.parseUnary()

	while (parser.matches([ TokenType.STAR, TokenType.SLASH ])) {
		expr = [ expr, " ", getLiteral(scanner.previous), " ", parser.parseUnary() ].join("")
	}

	return expr
}

parser.parseAddition = function() {
	var expr = parser.parseMultiplication()

	while (parser.matches([ TokenType.PLUS, TokenType.MINUS ])) {
		expr = [ expr, " ", getLiteral(scanner.previous), " ", parser.parseMultiplication() ].join("")
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
		expr = [ expr, " ", getLiteral(scanner.previous), " ", parser.parseComparison() ].join("")
	}

	return expr
}

parser.parseAssigment = function() {
	var expr = parser.parseEquality()

	if (parser.match(TokenType.EQUAL)) {
		return [ expr, " = ", parser.parseAssigment() ].join("")
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

	return parser.parseExpressionStatement()
}

parser.parseDeclaration = function() {
	if (parser.match(TokenType.LOCAL)) {
		var name = getLiteral(parser.consume(TokenType.IDENTIFIER, "Expected local variable name"))
		var init = "null"

		if (parser.match(TokenType.EQUAL)) {
			init = parser.parseExpression()
		}

		return [ "var ", name, " = ", init ].join("")
	}

	return parser.parseStatement()
}

parser.parse = function(token) {
	return parser.parseDeclaration()
}

var lunas = {}

lunas.compile = function(source) {
	scanner.setSource(source)
	parser.hadError = false
	parser.depth = ""

	var js = []

	while (true) {
		var token = scanner.next()

		if (token.type == TokenType.EOF) {
			if (parser.hadError) {
				return null
			}

			return js.join("")
		}

		js.push(parser.parse(token))
	}
}