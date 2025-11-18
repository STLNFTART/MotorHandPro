#!/usr/bin/env python3
"""
PrimalLang Parser - Builds Abstract Syntax Tree from tokens
Supports control flow, mathematical operations, and meta-constraints
"""
from dataclasses import dataclass
from typing import List, Optional, Union, Any
from enum import Enum, auto
from .lexer import Token, TokenType, lex


# AST Node Types
@dataclass
class ASTNode:
    """Base class for all AST nodes"""
    line: int
    column: int


@dataclass
class Program(ASTNode):
    """Root program node"""
    statements: List['Statement']


@dataclass
class Number(ASTNode):
    """Numeric literal"""
    value: float


@dataclass
class String(ASTNode):
    """String literal"""
    value: str


@dataclass
class Boolean(ASTNode):
    """Boolean literal"""
    value: bool


@dataclass
class Identifier(ASTNode):
    """Variable or function name"""
    name: str


@dataclass
class ArrayLiteral(ASTNode):
    """Array literal: []"""
    elements: List['Expression']


@dataclass
class BinaryOp(ASTNode):
    """Binary operation: left op right"""
    operator: str
    left: 'Expression'
    right: 'Expression'


@dataclass
class UnaryOp(ASTNode):
    """Unary operation: op expr"""
    operator: str
    operand: 'Expression'


@dataclass
class IndexAccess(ASTNode):
    """Array indexing: array[index]"""
    array: 'Expression'
    index: 'Expression'


@dataclass
class FunctionCall(ASTNode):
    """Function call: func(args...)"""
    name: str
    arguments: List['Expression']


@dataclass
class LetStatement(ASTNode):
    """Variable declaration: let x = expr"""
    name: str
    value: 'Expression'
    is_constant: bool = False


@dataclass
class Assignment(ASTNode):
    """Assignment: x = expr or x[i] = expr"""
    target: Union[Identifier, IndexAccess]
    value: 'Expression'


@dataclass
class FunctionDef(ASTNode):
    """Function definition: define f(params) { body }"""
    name: str
    parameters: List[str]
    body: 'Block'


@dataclass
class Block(ASTNode):
    """Code block: { statements... }"""
    statements: List['Statement']


@dataclass
class ReturnStatement(ASTNode):
    """Return statement: return expr"""
    value: Optional['Expression']


@dataclass
class ForLoop(ASTNode):
    """For loop: for i from start to end { body }"""
    variable: str
    start: 'Expression'
    end: 'Expression'
    body: 'Block'


@dataclass
class WhileLoop(ASTNode):
    """While loop: while condition { body }"""
    condition: 'Expression'
    body: 'Block'


@dataclass
class IfStatement(ASTNode):
    """If statement: if condition then { body } else { alt }"""
    condition: 'Expression'
    then_block: 'Block'
    else_block: Optional['Block']


@dataclass
class UniverseDecl(ASTNode):
    """Universe declaration: universe U = create()"""
    name: str


@dataclass
class EvolveStatement(ASTNode):
    """Evolve statement: evolve U with F"""
    universe: str
    function: str


@dataclass
class MetaConstraint(ASTNode):
    """Meta constraint: meta => (expr)"""
    condition: 'Expression'


@dataclass
class PrintStatement(ASTNode):
    """Print statement: print(expr) or display(expr)"""
    expression: 'Expression'
    is_display: bool = False


# Type aliases
Expression = Union[Number, String, Boolean, Identifier, ArrayLiteral, BinaryOp, UnaryOp, IndexAccess, FunctionCall]
Statement = Union[LetStatement, Assignment, FunctionDef, ReturnStatement, ForLoop, WhileLoop, IfStatement,
                  UniverseDecl, EvolveStatement, MetaConstraint, PrintStatement, Expression]


class Parser:
    """Recursive descent parser for PrimalLang"""

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.position = 0

    def parse(self) -> Program:
        """Parse the token stream into an AST"""
        statements = []
        while not self._is_at_end():
            # Skip newlines at statement level
            if self._match(TokenType.NEWLINE):
                continue

            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        return Program(statements=statements, line=1, column=1)

    def _parse_statement(self) -> Optional[Statement]:
        """Parse a single statement"""
        # Skip newlines
        while self._match(TokenType.NEWLINE):
            pass

        if self._is_at_end():
            return None

        # Universe declaration
        if self._match(TokenType.UNIVERSE):
            return self._parse_universe()

        # Variable declaration
        if self._check(TokenType.LET) or self._check(TokenType.CONSTANT):
            return self._parse_let()

        # Function definition
        if self._match(TokenType.DEFINE):
            return self._parse_function_def()

        # Control flow
        if self._match(TokenType.FOR):
            return self._parse_for_loop()

        if self._match(TokenType.WHILE):
            return self._parse_while_loop()

        if self._match(TokenType.IF):
            return self._parse_if_statement()

        if self._match(TokenType.RETURN):
            return self._parse_return()

        # Evolve statement
        if self._match(TokenType.EVOLVE):
            return self._parse_evolve()

        # Meta constraint
        if self._match(TokenType.META):
            return self._parse_meta()

        # Print/display
        if self._check(TokenType.PRINT) or self._check(TokenType.DISPLAY):
            return self._parse_print()

        # Assignment or expression
        expr = self._parse_expression()

        # Check for assignment
        if self._match(TokenType.ASSIGN) or self._match(TokenType.APL_ASSIGN):
            if isinstance(expr, (Identifier, IndexAccess)):
                value = self._parse_expression()
                return Assignment(target=expr, value=value, line=expr.line, column=expr.column)
            else:
                raise SyntaxError(f"Invalid assignment target at line {expr.line}")

        return expr

    def _parse_universe(self) -> UniverseDecl:
        """Parse universe declaration"""
        tok = self._previous()
        name_tok = self._consume(TokenType.IDENTIFIER, "Expected universe name")
        self._consume(TokenType.ASSIGN, "Expected '='")
        self._consume(TokenType.CREATE, "Expected 'create'")
        self._consume(TokenType.LPAREN, "Expected '('")
        self._consume(TokenType.RPAREN, "Expected ')'")

        return UniverseDecl(name=name_tok.value, line=tok.line, column=tok.column)

    def _parse_let(self) -> LetStatement:
        """Parse let/constant declaration"""
        is_const = self._check(TokenType.CONSTANT)
        tok = self._advance()

        name_tok = self._consume(TokenType.IDENTIFIER, "Expected variable name")
        self._consume(TokenType.ASSIGN, "Expected '=' or '←'") if not self._match(TokenType.APL_ASSIGN) else None

        value = self._parse_expression()

        return LetStatement(name=name_tok.value, value=value, is_constant=is_const,
                           line=tok.line, column=tok.column)

    def _parse_function_def(self) -> FunctionDef:
        """Parse function definition"""
        tok = self._previous()
        name_tok = self._consume(TokenType.IDENTIFIER, "Expected function name")

        self._consume(TokenType.LPAREN, "Expected '('")
        params = []
        if not self._check(TokenType.RPAREN):
            params.append(self._consume(TokenType.IDENTIFIER, "Expected parameter name").value)
            while self._match(TokenType.COMMA):
                params.append(self._consume(TokenType.IDENTIFIER, "Expected parameter name").value)

        self._consume(TokenType.RPAREN, "Expected ')'")
        body = self._parse_block()

        return FunctionDef(name=name_tok.value, parameters=params, body=body,
                          line=tok.line, column=tok.column)

    def _parse_for_loop(self) -> ForLoop:
        """Parse for loop: for i from start to end { body }"""
        tok = self._previous()
        var_tok = self._consume(TokenType.IDENTIFIER, "Expected loop variable")
        self._consume(TokenType.FROM, "Expected 'from'")
        start = self._parse_expression()
        self._consume(TokenType.TO, "Expected 'to'")
        end = self._parse_expression()
        body = self._parse_block()

        return ForLoop(variable=var_tok.value, start=start, end=end, body=body,
                      line=tok.line, column=tok.column)

    def _parse_while_loop(self) -> WhileLoop:
        """Parse while loop"""
        tok = self._previous()
        condition = self._parse_expression()
        body = self._parse_block()

        return WhileLoop(condition=condition, body=body, line=tok.line, column=tok.column)

    def _parse_if_statement(self) -> IfStatement:
        """Parse if statement"""
        tok = self._previous()
        condition = self._parse_expression()
        self._consume(TokenType.THEN, "Expected 'then'")
        then_block = self._parse_block()

        else_block = None
        if self._match(TokenType.ELSE):
            else_block = self._parse_block()

        return IfStatement(condition=condition, then_block=then_block, else_block=else_block,
                          line=tok.line, column=tok.column)

    def _parse_return(self) -> ReturnStatement:
        """Parse return statement"""
        tok = self._previous()
        value = None
        if not self._check(TokenType.NEWLINE) and not self._is_at_end():
            value = self._parse_expression()

        return ReturnStatement(value=value, line=tok.line, column=tok.column)

    def _parse_evolve(self) -> EvolveStatement:
        """Parse evolve statement"""
        tok = self._previous()
        universe_tok = self._consume(TokenType.IDENTIFIER, "Expected universe name")
        self._consume(TokenType.WITH, "Expected 'with'")
        function_tok = self._consume(TokenType.IDENTIFIER, "Expected function name")

        return EvolveStatement(universe=universe_tok.value, function=function_tok.value,
                              line=tok.line, column=tok.column)

    def _parse_meta(self) -> MetaConstraint:
        """Parse meta constraint"""
        tok = self._previous()
        self._consume(TokenType.ARROW, "Expected '=>'")
        self._consume(TokenType.LPAREN, "Expected '('")
        condition = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        return MetaConstraint(condition=condition, line=tok.line, column=tok.column)

    def _parse_print(self) -> PrintStatement:
        """Parse print/display statement"""
        is_display = self._check(TokenType.DISPLAY)
        tok = self._advance()
        self._consume(TokenType.LPAREN, "Expected '('")
        expr = self._parse_expression()
        self._consume(TokenType.RPAREN, "Expected ')'")

        return PrintStatement(expression=expr, is_display=is_display,
                             line=tok.line, column=tok.column)

    def _parse_block(self) -> Block:
        """Parse code block { ... }"""
        tok = self._consume(TokenType.LBRACE, "Expected '{'")
        statements = []

        while not self._check(TokenType.RBRACE) and not self._is_at_end():
            if self._match(TokenType.NEWLINE):
                continue
            stmt = self._parse_statement()
            if stmt:
                statements.append(stmt)

        self._consume(TokenType.RBRACE, "Expected '}'")

        return Block(statements=statements, line=tok.line, column=tok.column)

    def _parse_expression(self) -> Expression:
        """Parse expression (logical OR has lowest precedence)"""
        return self._parse_logical_or()

    def _parse_logical_or(self) -> Expression:
        """Parse logical OR"""
        left = self._parse_logical_and()

        while self._match(TokenType.OR):
            op_tok = self._previous()
            right = self._parse_logical_and()
            left = BinaryOp(operator='or', left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_logical_and(self) -> Expression:
        """Parse logical AND"""
        left = self._parse_implies()

        while self._match(TokenType.AND):
            op_tok = self._previous()
            right = self._parse_implies()
            left = BinaryOp(operator='and', left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_implies(self) -> Expression:
        """Parse logical implication"""
        left = self._parse_comparison()

        if self._match(TokenType.IMPLIES):
            op_tok = self._previous()
            right = self._parse_implies()  # Right associative
            return BinaryOp(operator='implies', left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_comparison(self) -> Expression:
        """Parse comparison operators"""
        left = self._parse_addition()

        while self._match(TokenType.EQ, TokenType.NE, TokenType.LT, TokenType.GT,
                          TokenType.LE, TokenType.GE):
            op_tok = self._previous()
            op_map = {
                TokenType.EQ: '==', TokenType.NE: '!=',
                TokenType.LT: '<', TokenType.GT: '>',
                TokenType.LE: '<=', TokenType.GE: '>='
            }
            right = self._parse_addition()
            left = BinaryOp(operator=op_map[op_tok.type], left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_addition(self) -> Expression:
        """Parse addition and subtraction"""
        left = self._parse_multiplication()

        while self._match(TokenType.PLUS, TokenType.MINUS):
            op_tok = self._previous()
            op = '+' if op_tok.type == TokenType.PLUS else '-'
            right = self._parse_multiplication()
            left = BinaryOp(operator=op, left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_multiplication(self) -> Expression:
        """Parse multiplication, division, and modulo"""
        left = self._parse_power()

        while self._match(TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO):
            op_tok = self._previous()
            op_map = {TokenType.MULTIPLY: '*', TokenType.DIVIDE: '/', TokenType.MODULO: '%'}
            op = op_map[op_tok.type]
            right = self._parse_power()
            left = BinaryOp(operator=op, left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_power(self) -> Expression:
        """Parse exponentiation (right associative)"""
        left = self._parse_unary()

        if self._match(TokenType.POWER):
            op_tok = self._previous()
            right = self._parse_power()  # Right associative
            return BinaryOp(operator='^', left=left, right=right,
                           line=op_tok.line, column=op_tok.column)

        return left

    def _parse_unary(self) -> Expression:
        """Parse unary operators"""
        if self._match(TokenType.MINUS, TokenType.NOT):
            op_tok = self._previous()
            op = '-' if op_tok.type == TokenType.MINUS else 'not'
            operand = self._parse_unary()
            return UnaryOp(operator=op, operand=operand,
                          line=op_tok.line, column=op_tok.column)

        return self._parse_postfix()

    def _parse_postfix(self) -> Expression:
        """Parse postfix expressions (function calls, array indexing)"""
        expr = self._parse_primary()

        while True:
            if self._match(TokenType.LPAREN):
                # Function call
                if not isinstance(expr, Identifier):
                    raise SyntaxError(f"Cannot call non-identifier at line {expr.line}")

                args = []
                if not self._check(TokenType.RPAREN):
                    args.append(self._parse_expression())
                    while self._match(TokenType.COMMA):
                        args.append(self._parse_expression())

                self._consume(TokenType.RPAREN, "Expected ')'")
                expr = FunctionCall(name=expr.name, arguments=args,
                                   line=expr.line, column=expr.column)

            elif self._match(TokenType.LBRACKET):
                # Array indexing
                index = self._parse_expression()
                self._consume(TokenType.RBRACKET, "Expected ']'")
                expr = IndexAccess(array=expr, index=index,
                                  line=expr.line, column=expr.column)
            else:
                break

        return expr

    def _parse_primary(self) -> Expression:
        """Parse primary expressions (literals, identifiers, groups)"""
        tok = self._peek()

        # Literals
        if self._match(TokenType.NUMBER):
            return Number(value=float(self._previous().value),
                         line=tok.line, column=tok.column)

        if self._match(TokenType.STRING):
            value = self._previous().value
            # Remove quotes
            value = value[1:-1] if len(value) >= 2 else value
            return String(value=value, line=tok.line, column=tok.column)

        if self._match(TokenType.TRUE):
            return Boolean(value=True, line=tok.line, column=tok.column)

        if self._match(TokenType.FALSE):
            return Boolean(value=False, line=tok.line, column=tok.column)

        # Array literal
        if self._match(TokenType.LBRACKET):
            elements = []
            if not self._check(TokenType.RBRACKET):
                elements.append(self._parse_expression())
                while self._match(TokenType.COMMA):
                    elements.append(self._parse_expression())

            self._consume(TokenType.RBRACKET, "Expected ']'")
            return ArrayLiteral(elements=elements, line=tok.line, column=tok.column)

        # Identifier
        if self._match(TokenType.IDENTIFIER):
            return Identifier(name=self._previous().value,
                            line=tok.line, column=tok.column)

        # Grouped expression
        if self._match(TokenType.LPAREN):
            expr = self._parse_expression()
            self._consume(TokenType.RPAREN, "Expected ')'")
            return expr

        raise SyntaxError(f"Unexpected token {tok.type} at line {tok.line}, column {tok.column}")

    # Helper methods
    def _match(self, *types: TokenType) -> bool:
        """Check if current token matches any of the given types and advance"""
        for token_type in types:
            if self._check(token_type):
                self._advance()
                return True
        return False

    def _check(self, token_type: TokenType) -> bool:
        """Check if current token is of given type"""
        if self._is_at_end():
            return False
        return self._peek().type == token_type

    def _advance(self) -> Token:
        """Move to next token"""
        if not self._is_at_end():
            self.position += 1
        return self._previous()

    def _is_at_end(self) -> bool:
        """Check if we've reached EOF"""
        return self._peek().type == TokenType.EOF

    def _peek(self) -> Token:
        """Get current token without advancing"""
        return self.tokens[self.position]

    def _previous(self) -> Token:
        """Get previous token"""
        return self.tokens[self.position - 1]

    def _consume(self, token_type: TokenType, message: str) -> Token:
        """Consume a token of given type or raise error"""
        if self._check(token_type):
            return self._advance()

        current = self._peek()
        raise SyntaxError(f"{message} at line {current.line}, column {current.column}. Got {current.type}")


def parse(source: str) -> Program:
    """Convenience function to parse source code"""
    tokens = lex(source)
    parser = Parser(tokens)
    return parser.parse()
