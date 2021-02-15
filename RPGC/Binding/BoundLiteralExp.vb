Public Class BoundLiteralExp
    Inherits BoundExpression

    Public Value As Object

    Public Sub New(_val As Object)
        Value = _val
        typ = _val.GetType()
        tok = BoundNodeToken.BNT_LITEX
    End Sub
End Class
