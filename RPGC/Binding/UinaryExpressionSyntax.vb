Public Class UinaryExpressionSyntax
    Inherits ExpresionSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_UNIEXP
    Public Property operand As SyntaxToken
    Public Property right As ExpresionSyntax

    Public Sub New(op As SyntaxToken, b As ExpresionSyntax)
        right = b
        operand = op
    End Sub

End Class
