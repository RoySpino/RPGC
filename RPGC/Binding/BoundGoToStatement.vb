Public Class BoundGoToStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_GOTOCOND
    Public Property Label As LabelSymbol

    Public Sub New(lbl As LabelSymbol)
        Label = lbl
    End Sub
End Class
