Public Class BoundUniOperator
    Public syntaxKind As TokenKind
    Public tok As BoundUniOpToken
    Public OperatorType As Type
    Public resultType As Type

    Private Shared OPERATORS() As BoundUniOperator = {
            New BoundUniOperator(TokenKind.TK_NOT, BoundUniOpToken.BUO_NOT, GetType(Boolean)),
            New BoundUniOperator(TokenKind.TK_ADD, BoundUniOpToken.BUO_IDENTITY, GetType(Int32)),
            New BoundUniOperator(TokenKind.TK_SUB, BoundUniOpToken.BUO_NEGATION, GetType(Int32))
        }

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, op As BoundUniOpToken, opType As Type)
        syntaxKind = synKind
        tok = op
        OperatorType = opType
        resultType = opType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, op As BoundUniOpToken, opType As Type, resType As Type)
        syntaxKind = synKind
        tok = op
        OperatorType = opType
        resultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Shared Function bind(kind As TokenKind, opType As Type) As BoundUniOperator
        For Each op As BoundUniOperator In OPERATORS
            If op.OperatorType = opType And op.syntaxKind = kind Then
                Return op
            End If
        Next
    End Function

End Class
