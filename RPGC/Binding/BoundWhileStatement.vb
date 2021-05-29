Public Class BoundWhileStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_WHILESTMT
    Public Property Condition As BoundExpression
    Public Property Body As BoundStatement

    Public Sub New(_condition As BoundExpression, _body As BoundStatement)
        Condition = _condition
        Body = _body
    End Sub
End Class
