Public Class BinaryExpressionSyntax
    Inherits ExpresionSyntax

    Public Property left As ExpresionSyntax
    Public Property operatorToken As SyntaxToken
    Public Property right As ExpresionSyntax

    Public Sub New(A As ExpresionSyntax, operation As SyntaxToken, B As ExpresionSyntax)
        left = A
        right = B
        kind = TokenKind.TK_BYNARYEXPR

        operatorToken = operation
    End Sub

End Class
