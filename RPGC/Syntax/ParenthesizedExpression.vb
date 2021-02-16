Public Class ParenthesizedExpression
    Inherits ExpresionSyntax

    Public Overrides Property Kind As TokenKind = TokenKind.TK_PARENEXP
    Public Property OPENParen As SyntaxToken
    Public Property expression As ExpresionSyntax
    Public Property CLOSEParen As SyntaxToken

    Public Sub New(open_Paren As SyntaxToken, expr As ExpresionSyntax, close_Paren As SyntaxToken)
        OPENParen = open_Paren
        CLOSEParen = close_Paren
        expression = expr
        kind = TokenKind.TK_PARENEXP
    End Sub

End Class
