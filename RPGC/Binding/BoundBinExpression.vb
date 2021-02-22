Public Class BoundBinExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_BINEX
    Public OP As BoundBinOperator
    Public Left As BoundExpression
    Public Right As BoundExpression

    Public Sub New(lft As BoundExpression, _op As BoundBinOperator, rit As BoundExpression)
        OP = _op
        Left = lft
        Right = rit

        typ = OP.ResultType
    End Sub

    ' /////////////////////////////////////////////////////////////////////////////////
    Public Function resultType()
        Return OP.ResultType
    End Function
End Class
