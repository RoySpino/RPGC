Public Class IfStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_IF
    Public Property IfKeyword As SyntaxToken
    Public Property Condition As ExpresionSyntax
    Public Property ThenStatement As StatementSyntax
    Public Property ElseBlock As ElseStatementSyntax

    Public Sub New(if_Keyword As SyntaxToken, booStmnt As ExpresionSyntax, thenBlock As StatementSyntax, else_Block As ElseStatementSyntax)
        IfKeyword = if_Keyword
        Condition = booStmnt
        ThenStatement = thenBlock
        ElseBlock = else_Block
    End Sub

End Class
