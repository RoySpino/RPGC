Public Class BoundLiteralExp
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_LITEX
    Public Value As Object

    Public Sub New(_val As Object)
        Value = _val
        typ = _val.GetType()
    End Sub
End Class
