Public Class BoundVariableExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_VAREX
    Public Name As String
    Public Variable As VariableSymbol

    Public Sub New(_var As VariableSymbol)
        Variable = _var
        typ = _var.GetType()
        Name = _var.name
    End Sub

End Class
