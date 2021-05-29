Public Class BoundUntilStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_DOUNTIL
    Public Property Condition As BoundExpression
    Public Property Body As BoundStatement

    Public Sub New(_condition As BoundExpression, _body As BoundStatement)
        Condition = _condition
        Body = _body
    End Sub
End Class
