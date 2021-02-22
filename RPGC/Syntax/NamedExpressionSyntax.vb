Public Class NamedExpressionSyntax
    Inherits ExpresionSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_NAMEDEXP
    Public Property IDENTIFIERTOKEN As SyntaxNode

    Public Sub New(identifier As SyntaxToken)
        IDENTIFIERTOKEN = identifier
    End Sub
End Class
