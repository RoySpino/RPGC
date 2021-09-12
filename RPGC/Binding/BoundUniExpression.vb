Public Class BoundUniExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_UINEX
    Public OP As BoundUniOperator
    Public right As BoundExpression

    Public Sub New(_op As BoundUniOperator, operand As BoundExpression)
        OP = _op
        right = operand

        typ = operand.typ
    End Sub

    ' /////////////////////////////////////////////////////////////////////////////////
    Public Function resultType() As TypeSymbol
        Return OP.resultType
    End Function
End Class
