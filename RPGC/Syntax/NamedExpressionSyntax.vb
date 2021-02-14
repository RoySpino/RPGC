Public Class NamedExpressionSyntax
    Inherits ExpresionSyntax

    Public Property IDENTIFIERTOKEN As SyntaxNode

    Public Sub New(identifier As SyntaxToken)
        IDENTIFIERTOKEN = identifier
        kind = TokenKind.TK_NAMEDEXP
    End Sub
End Class
