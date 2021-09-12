Public Class BoundUniOperator
    Public syntaxKind As TokenKind
    Public tok As BoundUniOpToken
    Public OperatorType As TypeSymbol
    Public resultType As TypeSymbol

    Private Shared OPERATORS() As BoundUniOperator = {
            New BoundUniOperator(TokenKind.TK_NOT, BoundUniOpToken.BUO_NOT, TypeSymbol.Indicator),
            New BoundUniOperator(TokenKind.TK_ADD, BoundUniOpToken.BUO_IDENTITY, TypeSymbol.Integer_),
            New BoundUniOperator(TokenKind.TK_SUB, BoundUniOpToken.BUO_NEGATION, TypeSymbol.Integer_)
        }

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, op As BoundUniOpToken, opType As TypeSymbol)
        syntaxKind = synKind
        tok = op
        OperatorType = opType
        resultType = opType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, op As BoundUniOpToken, opType As TypeSymbol, resType As TypeSymbol)
        syntaxKind = synKind
        tok = op
        OperatorType = opType
        resultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Shared Function bind(kind As TokenKind, opType As TypeSymbol) As BoundUniOperator
        For Each op As BoundUniOperator In OPERATORS
            If op.OperatorType.Equals(opType) = True And op.syntaxKind = kind Then
                Return op
            End If
        Next

        Return Nothing
    End Function

End Class
