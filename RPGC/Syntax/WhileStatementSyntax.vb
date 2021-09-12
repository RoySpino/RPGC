Public Class WhileStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_DOW
    Public Property Keyword As SyntaxToken
    Public Property Condition As ExpresionSyntax
    Public Property Body As StatementSyntax

    Public Sub New(keywrd As SyntaxToken, condition_ As ExpresionSyntax, body_ As StatementSyntax)
        Keyword = keywrd
        Condition = condition_
        Body = body_
    End Sub
End Class
