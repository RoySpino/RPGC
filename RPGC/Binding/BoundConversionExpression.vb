Public Class BoundConversionExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_CONVEXP
    Public Property Type_ As TypeSymbol
    Public Property _Expression As BoundExpression

    Public Sub New(type As TypeSymbol, expression As BoundExpression)
        Type_ = type
        _Expression = expression
    End Sub
End Class
