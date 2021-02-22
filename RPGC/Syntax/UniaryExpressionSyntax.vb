Public Class UniaryExpressionSyntax
    Inherits ExpresionSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_UNIEXP
    Public Property Operand As SyntaxToken
    Public Property right As ExpresionSyntax

    Public Sub New(_oparand As SyntaxToken, B As ExpresionSyntax)
        right = B
        Operand = _oparand
    End Sub
End Class
