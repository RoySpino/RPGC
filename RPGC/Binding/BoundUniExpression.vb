Public Class BoundUniExpression
    Inherits BoundExpression

    Public OP As BoundUniOperator
    Public right As BoundExpression

    Public Sub New(_op As BoundUniOperator, operand As BoundExpression)
        OP = _op
        right = operand

        typ = operand.typ
        tok = BoundNodeToken.BNT_UINEX
    End Sub

    ' /////////////////////////////////////////////////////////////////////////////////
    Public Function resultType() As Type
        Return OP.resultType
    End Function
End Class
