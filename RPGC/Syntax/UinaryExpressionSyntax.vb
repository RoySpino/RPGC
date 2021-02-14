Public Class UinaryExpressionSyntax
    Inherits ExpresionSyntax

    Public Property Operand As SyntaxToken
    Public Property right As ExpresionSyntax

    Public Sub New(_oparand As SyntaxToken, B As ExpresionSyntax)
        right = B
        kind = TokenKind.TK_UNIEXP
        Operand = _oparand
    End Sub
End Class
