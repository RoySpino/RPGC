Public Class BoundIfStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_IFSTMT
    Public Property Condition As BoundExpression
    Public Property ThenStatement As BoundStatement
    Public Property ElseStatement As BoundStatement

    Public Sub New(condition_ As BoundExpression, thenStmt As BoundStatement, elseStmt As BoundStatement)
        Condition = condition_
        ThenStatement = thenStmt
        ElseStatement = elseStmt
    End Sub
End Class
