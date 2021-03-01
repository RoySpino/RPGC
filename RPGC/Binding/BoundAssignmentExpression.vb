Public Class BoundAssignmentExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_ASNEX
    Public expression As BoundExpression
    Public Variable As VariableSymbol

    Public Sub New(_var As VariableSymbol, exp As BoundExpression)

        tok = BoundNodeToken.BNT_ASNEX
        typ = exp.typ
        expression = exp
        Variable = _var
    End Sub
End Class
