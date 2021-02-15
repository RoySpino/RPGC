Public Class BoundVariableExpression
    Inherits BoundExpression

    Public Name As String
    Public Variable As VariableSymbol

    Public Sub New(_var As VariableSymbol)
        Variable = _var
        typ = _var.GetType()
        Name = _var.name
        tok = BoundNodeToken.BNT_VAREX
    End Sub

End Class
