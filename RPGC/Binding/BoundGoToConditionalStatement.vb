Public Class BoundGoToConditionalStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_GOTOCOND
    Public Property Label As LabelSymbol
    Public Property Condition As BoundExpression
    Public Property JumpWhenFalse As Boolean

    Public Sub New(lbl As LabelSymbol, _Condition As BoundExpression, Optional _jumpWhenFalse As Boolean = False)
        Label = lbl
        Condition = _Condition
        JumpWhenFalse = _jumpWhenFalse
    End Sub
End Class
