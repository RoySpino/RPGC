Public Class BinaryExpressionSyntax
    Inherits ExpresionSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_BYNARYEXPR
    Public Property left As ExpresionSyntax
    Public Property operatorToken As SyntaxToken
    Public Property right As ExpresionSyntax

    Public Sub New(A As ExpresionSyntax, operation As SyntaxToken, B As ExpresionSyntax)
        left = A
        right = B

        operatorToken = operation
    End Sub

End Class
