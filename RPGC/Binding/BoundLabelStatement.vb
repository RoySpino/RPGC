Public Class BoundLabelStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_LABEL
    Public Property Label As LabelSymbol

    Public Sub New(symbol As LabelSymbol)
        Label = symbol
    End Sub
End Class
