#!/usr/bin/env python3
"""
PrimalLang Lexer - Tokenizes PrimalLang source code
Supports APL-inspired mathematical operators and control system primitives
"""
import re
from dataclasses import dataclass
from typing import List, Optional
from enum import Enum, auto


class TokenType(Enum):
    # Literals
    NUMBER = auto()
    STRING = auto()
    IDENTIFIER = auto()

    # Keywords
    UNIVERSE = auto()
    CREATE = auto()
    LET = auto()
    CONSTANT = auto()
    DEFINE = auto()
    RETURN = auto()
    FOR = auto()
    FROM = auto()
    TO = auto()
    IN = auto()
    IF = auto()
    THEN = auto()
    ELSE = auto()
    WHILE = auto()
    PRINT = auto()
    DISPLAY = auto()
    EVOLVE = auto()
    WITH = auto()
    META = auto()
    IMPLIES = auto()
    AND = auto()
    OR = auto()
    NOT = auto()
    TRUE = auto()
    FALSE = auto()

    # Operators
    PLUS = auto()
    MINUS = auto()
    MULTIPLY = auto()
    DIVIDE = auto()
    POWER = auto()
    MODULO = auto()

    # Comparison
    EQ = auto()
    NE = auto()
    LT = auto()
    GT = auto()
    LE = auto()
    GE = auto()
    ARROW = auto()  # =>

    # Assignment
    ASSIGN = auto()  # =
    APL_ASSIGN = auto()  # ←

    # Delimiters
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    COMMA = auto()
    SEMICOLON = auto()
    COLON = auto()
    DOT = auto()

    # APL operators
    APL_RHO = auto()  # ⍴ (reshape/size)
    APL_IOTA = auto()  # ⍳ (index generator)
    APL_OMEGA = auto()  # ⍵ (right argument)
    APL_ALPHA = auto()  # ⍺ (left argument)

    # Special
    NEWLINE = auto()
    EOF = auto()
    COMMENT = auto()


@dataclass
class Token:
    type: TokenType
    value: str
    line: int
    column: int


class Lexer:
    """Lexer for PrimalLang with support for APL symbols and control primitives"""

    # Token patterns - order matters!
    TOKEN_PATTERNS = [
        # Comments
        (r'//[^\n]*', TokenType.COMMENT),
        (r'/\*.*?\*/', TokenType.COMMENT),

        # Numbers (including scientific notation)
        (r'\d+\.?\d*([eE][+-]?\d+)?', TokenType.NUMBER),

        # Strings
        (r'"(?:[^"\\]|\\.)*"', TokenType.STRING),
        (r"'(?:[^'\\]|\\.)*'", TokenType.STRING),

        # Keywords (must come before IDENTIFIER)
        (r'\buniverse\b', TokenType.UNIVERSE),
        (r'\bcreate\b', TokenType.CREATE),
        (r'\blet\b', TokenType.LET),
        (r'\bconstant\b', TokenType.CONSTANT),
        (r'\bdefine\b', TokenType.DEFINE),
        (r'\breturn\b', TokenType.RETURN),
        (r'\bfor\b', TokenType.FOR),
        (r'\bfrom\b', TokenType.FROM),
        (r'\bto\b', TokenType.TO),
        (r'\bin\b', TokenType.IN),
        (r'\bif\b', TokenType.IF),
        (r'\bthen\b', TokenType.THEN),
        (r'\belse\b', TokenType.ELSE),
        (r'\bwhile\b', TokenType.WHILE),
        (r'\bprint\b', TokenType.PRINT),
        (r'\bdisplay\b', TokenType.DISPLAY),
        (r'\bevolve\b', TokenType.EVOLVE),
        (r'\bwith\b', TokenType.WITH),
        (r'\bmeta\b', TokenType.META),
        (r'\bimplies\b', TokenType.IMPLIES),
        (r'\band\b', TokenType.AND),
        (r'\bor\b', TokenType.OR),
        (r'\bnot\b', TokenType.NOT),
        (r'\btrue\b', TokenType.TRUE),
        (r'\bfalse\b', TokenType.FALSE),

        # Identifiers
        (r'[A-Za-z_][A-Za-z0-9_]*', TokenType.IDENTIFIER),

        # APL symbols
        (r'⍴', TokenType.APL_RHO),
        (r'⍳', TokenType.APL_IOTA),
        (r'⍵', TokenType.APL_OMEGA),
        (r'⍺', TokenType.APL_ALPHA),
        (r'←', TokenType.APL_ASSIGN),

        # Operators (multi-char first)
        (r'=>', TokenType.ARROW),
        (r'==', TokenType.EQ),
        (r'!=', TokenType.NE),
        (r'<=', TokenType.LE),
        (r'>=', TokenType.GE),
        (r'<', TokenType.LT),
        (r'>', TokenType.GT),
        (r'\^', TokenType.POWER),
        (r'\+', TokenType.PLUS),
        (r'-', TokenType.MINUS),
        (r'\*', TokenType.MULTIPLY),
        (r'/', TokenType.DIVIDE),
        (r'%', TokenType.MODULO),
        (r'=', TokenType.ASSIGN),

        # Delimiters
        (r'\(', TokenType.LPAREN),
        (r'\)', TokenType.RPAREN),
        (r'\{', TokenType.LBRACE),
        (r'\}', TokenType.RBRACE),
        (r'\[', TokenType.LBRACKET),
        (r'\]', TokenType.RBRACKET),
        (r',', TokenType.COMMA),
        (r';', TokenType.SEMICOLON),
        (r':', TokenType.COLON),
        (r'\.', TokenType.DOT),

        # Newlines
        (r'\n', TokenType.NEWLINE),

        # Skip whitespace (not newlines)
        (r'[ \t\r]+', None),  # None means skip
    ]

    def __init__(self, source: str):
        self.source = source
        self.position = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []

    def tokenize(self) -> List[Token]:
        """Tokenize the entire source"""
        while self.position < len(self.source):
            self._next_token()

        # Add EOF token
        self.tokens.append(Token(TokenType.EOF, '', self.line, self.column))
        return self.tokens

    def _next_token(self):
        """Extract the next token from source"""
        # Try each pattern
        for pattern, token_type in self.TOKEN_PATTERNS:
            regex = re.compile(pattern)
            match = regex.match(self.source, self.position)

            if match:
                value = match.group(0)

                # Skip whitespace and comments
                if token_type is None or token_type == TokenType.COMMENT:
                    self._advance(len(value))
                    return

                # Create token
                token = Token(token_type, value, self.line, self.column)
                self.tokens.append(token)

                # Advance position
                self._advance(len(value))
                return

        # No pattern matched - error
        char = self.source[self.position]
        raise SyntaxError(f"Unexpected character '{char}' at line {self.line}, column {self.column}")

    def _advance(self, count: int):
        """Advance position and track line/column"""
        for _ in range(count):
            if self.position < len(self.source):
                if self.source[self.position] == '\n':
                    self.line += 1
                    self.column = 1
                else:
                    self.column += 1
                self.position += 1


def lex(source: str) -> List[Token]:
    """Convenience function to tokenize source code"""
    lexer = Lexer(source)
    return lexer.tokenize()
