Public Class ElseStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_ELSE
    Public Property ElseKeyword As SyntaxToken
    Public Property ElseStatement As StatementSyntax

    Public Sub New(else_keyword As SyntaxToken, elseStmnt As StatementSyntax)
        ElseKeyword = else_keyword
        ElseStatement = elseStmnt
    End Sub
End Class
