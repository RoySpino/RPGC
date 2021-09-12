Public Class BoundForStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_FORSTMT
    Public Property Variable As VariableSymbol
    Public Property LBound As BoundExpression
    Public Property Ubound As BoundExpression
    Public Property IncrementBy As BoundExpression
    Public Property Body As BoundStatement
    Public Property IsCountUP As Boolean

    Public Sub New(_variable As VariableSymbol, _lBound As BoundExpression, _ubound As BoundExpression, _body As BoundStatement, _isCountUp As Boolean, Optional _incrementBy As BoundExpression = Nothing)
        Variable = _variable
        LBound = _lBound
        Ubound = _ubound
        Body = _body
        IsCountUP = _isCountUp

        If IncrementBy Is Nothing Then
            IncrementBy = New BoundLiteralExp(1)
        Else
            IncrementBy = _incrementBy
        End If
    End Sub
End Class
