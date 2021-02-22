Public Class BoundExpressionStatement
    Inherits BoundStatement

    Public Property Expression As BoundExpression
    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_EXPRSTMT

    Public Sub New(expr As BoundExpression)
        Expression = expr
    End Sub
End Class
