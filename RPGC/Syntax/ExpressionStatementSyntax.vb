Public Class ExpressionStatementSyntax
    Inherits StatementSyntax
    Public Overrides Property kind As TokenKind = TokenKind.TK_EXPRNSTMNT
    Public Property Expression As ExpresionSyntax

    Public Sub New(_expresion As ExpresionSyntax)
        Expression = _expresion
    End Sub
End Class
