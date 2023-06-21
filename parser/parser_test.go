package parser

import (
	"fmt"
	"testing"

	"github.com/malcolmrebughini/monkey-go/ast"
	"github.com/malcolmrebughini/monkey-go/lexer"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLetStatements(t *testing.T) {
	tests := []struct {
		input              string
		expectedIdentifier string
		expectedValue      any
	}{
		{"let x = 5;", "x", 5},
		{"let y = true;", "y", true},
		{"let foobar = y;", "foobar", "y"},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		require.Len(t, program.Statements, 1)
		stmt := program.Statements[0]
		testLetStatement(t, stmt, tt.expectedIdentifier)
		val := stmt.(*ast.LetStatement).Value
		testLiteralExpression(t, val, tt.expectedValue)
	}
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	require.Equal(t, "let", s.TokenLiteral())
	letStmt, ok := s.(*ast.LetStatement)
	require.True(t, ok)
	require.Equal(t, name, letStmt.Name.Value)
	require.Equal(t, name, letStmt.Name.TokenLiteral())
	return true
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()

	require.Len(t, errors, 0)
}

func TestReturnStatements(t *testing.T) {
	tests := []struct {
		input              string
		expectedIdentifier string
		expectedValue      any
	}{
		{"return 5;", "return", 5},
		{"return 10;", "return", 10},
		{"return foobar;", "return", "foobar"},
	}
	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		require.Len(t, program.Statements, 1)
		stmt := program.Statements[0]
		val := stmt.(*ast.ReturnStatement).ReturnValue
		testLiteralExpression(t, val, tt.expectedValue)
	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	require.Len(t, program.Statements, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)

	ident, ok := stmt.Expression.(*ast.Identifier)
	require.True(t, ok)

	require.Equal(t, "foobar", ident.Value)
	require.Equal(t, "foobar", ident.TokenLiteral())

}

func TestBooleanExpression(t *testing.T) {
	inputs := []struct {
		input    string
		expected bool
	}{
		{"true;", true},
		{"false;", false},
	}

	for _, tt := range inputs {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		require.Len(t, program.Statements, 1)
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		require.True(t, ok)

		ident, ok := stmt.Expression.(*ast.Boolean)
		require.True(t, ok)

		require.Equal(t, tt.expected, ident.Value)
		require.Equal(t, fmt.Sprintf("%v", tt.expected), ident.TokenLiteral())
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	require.Len(t, program.Statements, 1)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)

	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
	require.True(t, ok)
	require.Equal(t, int64(5), literal.Value)
	require.Equal(t, "5", literal.TokenLiteral())
}

func TestParsingPrefixExpressions(t *testing.T) {
	prefixTests := []struct {
		input        string
		operator     string
		integerValue any
	}{
		{"!5;", "!", 5},
		{"-15;", "-", 15},
		{"!true;", "!", true},
		{"!false;", "!", false},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		require.Len(t, program.Statements, 1)
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		require.True(t, ok)
		exp, ok := stmt.Expression.(*ast.PrefixExpression)
		require.True(t, ok)
		require.Equal(t, tt.operator, exp.Operator)

		testLiteralExpression(t, exp.Right, tt.integerValue)
	}

}

func testIntegerLiteral(t *testing.T, il ast.Expression, value int64) {
	integ, ok := il.(*ast.IntegerLiteral)

	require.True(t, ok)
	require.Equal(t, value, integ.Value)
	require.Equal(t, fmt.Sprintf("%d", value), integ.TokenLiteral())
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  any
		operator   string
		rightValue any
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
		{"true == true", true, "==", true},
		{"true != false", true, "!=", false},
		{"false == false", false, "==", false},
	}

	for _, tt := range infixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		require.Len(t, program.Statements, 1)
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		require.True(t, ok)
		exp, ok := stmt.Expression.(*ast.InfixExpression)
		require.True(t, ok)

		testLiteralExpression(t, exp.Left, tt.leftValue)
		require.Equal(t, tt.operator, exp.Operator)
		testLiteralExpression(t, exp.Right, tt.rightValue)
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"-a * b",
			"((-a) * b)",
		},
		{
			"!-a",
			"(!(-a))",
		},
		{
			"a + b + c",
			"((a + b) + c)",
		},
		{
			"a + b - c",
			"((a + b) - c)",
		},
		{
			"a * b * c",
			"((a * b) * c)",
		},
		{
			"a * b / c",
			"((a * b) / c)",
		},
		{
			"a + b / c",
			"(a + (b / c))",
		},
		{
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		},
		{
			"5 >4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
		{
			"true",
			"true",
		},
		{
			"false",
			"false",
		},
		{
			"3 > 5 == false",
			"((3 > 5) == false)",
		},
		{
			"3 < 5 == true",
			"((3 < 5) == true)",
		},
		{
			"1 + (2 + 3) + 4",
			"((1 + (2 + 3)) + 4)",
		},
		{
			"(5 + 5) * 2",
			"((5 + 5) * 2)",
		},
		{
			"2 / (5 + 5)",
			"(2 / (5 + 5))",
		},
		{
			"-(5 + 5)",
			"(-(5 + 5))",
		},
		{
			"!(true == true)",
			"(!(true == true))",
		},
		{
			"a + add(b * c) + d",
			"((a + add((b * c))) + d)",
		},
		{
			"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		},
		{
			"add(a + b + c * d / f + g)",
			"add((((a + b) + ((c * d) / f)) + g))",
		},
		{
			"a * [1, 2, 3, 4][b * c] * d",
			"((a * ([1, 2, 3, 4][(b * c)])) * d)",
		},
		{
			"add(a * b[2], b[1], 2 * [1, 2][1])",
			"add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)
		assert.Equal(t, tt.expected, program.String())
	}

}

func TestStringLiteralExpression(t *testing.T) {
	input := `"hello world";`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	require.Len(t, program.Statements, 1)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)

	literal, ok := stmt.Expression.(*ast.StringLiteral)
	require.True(t, ok)
	require.Equal(t, "hello world", literal.Value)
}

func TestParsingArrayLiterals(t *testing.T) {
	input := "[1, 2 * 2, 3 + 3]"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	array, ok := stmt.Expression.(*ast.ArrayLiteral)

	require.True(t, ok)
	assert.Len(t, array.Elements, 3)
	testIntegerLiteral(t, array.Elements[0], 1)
	testInfixEpressions(t, array.Elements[1], 2, "*", 2)
	testInfixEpressions(t, array.Elements[2], 3, "+", 3)
}

func TestParsingIndexExpressions(t *testing.T) {
	input := "myArray[1 + 1]"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	indexExp, ok := stmt.Expression.(*ast.IndexExpression)
	require.True(t, ok)
	testIdentifier(t, indexExp.Left, "myArray")
	testInfixEpressions(t, indexExp.Index, 1, "+", 1)
}

func TestParsingHashLiteralsStringKeys(t *testing.T) {
	input := `{"one": 1, "two": 2, "three": 3}`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	hash, ok := stmt.Expression.(*ast.HashLiteral)
	require.True(t, ok)
	assert.Len(t, hash.Pairs, 3)

	expected := map[string]int64{
		"one":   1,
		"two":   2,
		"three": 3,
	}

	for key, value := range hash.Pairs {
		literal, ok := key.(*ast.StringLiteral)
		assert.True(t, ok)
		expectedValue := expected[literal.String()]
		testIntegerLiteral(t, value, expectedValue)
	}
}

func TestParsingEmptyHashLiteral(t *testing.T) {
	input := `{}`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	hash, ok := stmt.Expression.(*ast.HashLiteral)
	require.True(t, ok)
	assert.Len(t, hash.Pairs, 0)
}

func TestParsingHashLiteralsWithExpressions(t *testing.T) {
	input := `{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	hash, ok := stmt.Expression.(*ast.HashLiteral)
	require.True(t, ok)
	assert.Len(t, hash.Pairs, 3)

	tests := map[string]func(ast.Expression){
		"one": func(e ast.Expression) {
			testInfixEpressions(t, e, 0, "+", 1)
		},
		"two": func(e ast.Expression) {
			testInfixEpressions(t, e, 10, "-", 8)
		},
		"three": func(e ast.Expression) {
			testInfixEpressions(t, e, 15, "/", 5)
		},
	}

	for key, value := range hash.Pairs {
		literal, ok := key.(*ast.StringLiteral)
		require.True(t, ok)

		testFunc, ok := tests[literal.String()]
		require.True(t, ok)

		testFunc(value)
	}
}

func testIdentifier(t *testing.T, exp ast.Expression, value string) {
	ident, ok := exp.(*ast.Identifier)
	assert.True(t, ok)
	assert.Equal(t, value, ident.Value)
	assert.Equal(t, value, ident.TokenLiteral())
}

func testLiteralExpression(t *testing.T, exp ast.Expression, expected any) {
	switch v := expected.(type) {
	case int:
		testIntegerLiteral(t, exp, int64(v))
		return
	case int64:
		testIntegerLiteral(t, exp, v)
		return
	case string:
		testIdentifier(t, exp, v)
		return
	case bool:
		testBooleanLiteral(t, exp, v)
		return
	}

	t.Errorf("type of exp not handled. got=%T", exp)
}

func testInfixEpressions(t *testing.T, exp ast.Expression, left any, operator string, right any) {
	opExp, ok := exp.(*ast.InfixExpression)
	require.True(t, ok)
	testLiteralExpression(t, opExp.Left, left)
	require.Equal(t, operator, opExp.Operator)
	testLiteralExpression(t, opExp.Right, right)
}

func testBooleanLiteral(t *testing.T, exp ast.Expression, value bool) {
	bo, ok := exp.(*ast.Boolean)
	require.True(t, ok)
	require.Equal(t, value, bo.Value)
	require.Equal(t, fmt.Sprintf("%t", value), bo.TokenLiteral())
}

func TestIfExpression(t *testing.T) {
	input := `if (x < y) { x }`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)
	require.Len(t, program.Statements, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	exp, ok := stmt.Expression.(*ast.IfExpression)
	require.True(t, ok)
	testInfixEpressions(t, exp.Condition, "x", "<", "y")
	require.Len(t, exp.Consequence.Statements, 1)
	consequence, ok := exp.Consequence.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	testIdentifier(t, consequence.Expression, "x")
	require.Nil(t, exp.Alternative)
}

func TestIfElseExpression(t *testing.T) {
	input := `if (x < y) { x } else { y }`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)
	require.Len(t, program.Statements, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	exp, ok := stmt.Expression.(*ast.IfExpression)
	require.True(t, ok)
	testInfixEpressions(t, exp.Condition, "x", "<", "y")
	require.Len(t, exp.Consequence.Statements, 1)
	consequence, ok := exp.Consequence.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	testIdentifier(t, consequence.Expression, "x")

	consequence, ok = exp.Alternative.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	testIdentifier(t, consequence.Expression, "y")
}

func TestFunctionLiteralParsing(t *testing.T) {
	input := `fn(x, y) { x + y }`

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	require.Len(t, program.Statements, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	function, ok := stmt.Expression.(*ast.FunctionLiteral)
	require.True(t, ok)
	require.Len(t, function.Parameters, 2)

	testLiteralExpression(t, function.Parameters[0], "x")
	testLiteralExpression(t, function.Parameters[1], "y")

	require.Len(t, function.Body.Statements, 1)
	bodyStmt, ok := function.Body.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)
	testInfixEpressions(t, bodyStmt.Expression, "x", "+", "y")
}

func TestFunctionParameterParsing(t *testing.T) {
	tests := []struct {
		input          string
		expectedParams []string
	}{
		{input: "fn() {};", expectedParams: []string{}},
		{input: "fn(x) {};", expectedParams: []string{"x"}},
		{input: "fn(x, y, z) {};", expectedParams: []string{"x", "y", "z"}},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		stmt := program.Statements[0].(*ast.ExpressionStatement)
		function := stmt.Expression.(*ast.FunctionLiteral)

		assert.Equal(t, len(tt.expectedParams), len(function.Parameters))

		for i, ident := range tt.expectedParams {
			testLiteralExpression(t, function.Parameters[i], ident)
		}
	}
}

func TestCallExpressionParsing(t *testing.T) {
	input := "add(1, 2 * 3, 4 + 5);"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	require.Len(t, program.Statements, 1)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	require.True(t, ok)

	exp, ok := stmt.Expression.(*ast.CallExpression)
	require.True(t, ok)

	testIdentifier(t, exp.Function, "add")

	require.Len(t, exp.Arguments, 3)

	testLiteralExpression(t, exp.Arguments[0], 1)
	testInfixEpressions(t, exp.Arguments[1], 2, "*", 3)
	testInfixEpressions(t, exp.Arguments[2], 4, "+", 5)
}
