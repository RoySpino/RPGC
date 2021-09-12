Public Class GoToStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_GOTO
    Public Property TagKeyword As SyntaxToken
    Public Property LableName As String

    Public Sub New(TagStatement As SyntaxToken, Name As String)
        LableName = Name
        TagKeyword = TagStatement
    End Sub

End Class
