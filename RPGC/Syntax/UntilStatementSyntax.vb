Public Class UntilStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_DOU
    Public Property Keyword As SyntaxToken
    Public Property Condition As ExpresionSyntax
    Public Property Body As StatementSyntax

    Public Sub New(keyWrd As SyntaxToken, Condition_ As ExpresionSyntax, body_ As StatementSyntax)
        keyword = keyWrd
        Condition = Condition_
        Body = body_
    End Sub
End Class
