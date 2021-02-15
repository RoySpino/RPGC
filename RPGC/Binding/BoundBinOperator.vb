Public Class BoundBinOperator
    Public SyntaxKind As TokenKind
    Public tok As BoundBinOpToken
    Public RightType As Type
    Public LeftType As Type
    Public ResultType As Type

    Public Shared OPERATORS() As BoundBinOperator = {
            New BoundBinOperator(TokenKind.TK_ADD, BoundBinOpToken.BBO_ADD, GetType(Int32)),
            New BoundBinOperator(TokenKind.TK_SUB, BoundBinOpToken.BBO_SUB, GetType(Int32)),
            New BoundBinOperator(TokenKind.TK_MULT, BoundBinOpToken.BBO_MULT, GetType(Int32)),
            New BoundBinOperator(TokenKind.TK_DIV, BoundBinOpToken.BBO_DIV, GetType(Int32)),
            New BoundBinOperator(TokenKind.TK_AND, BoundBinOpToken.BBO_AND, GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_OR, BoundBinOpToken.BBO_OR, GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_EQ, BoundBinOpToken.BBO_EQ, GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_NE, BoundBinOpToken.BBO_NE, GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_GE, BoundBinOpToken.BBO_GE, GetType(Int32), GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_GT, BoundBinOpToken.BBO_GT, GetType(Int32), GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_LE, BoundBinOpToken.BBO_LE, GetType(Int32), GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_LT, BoundBinOpToken.BBO_LT, GetType(Int32), GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_EQ, BoundBinOpToken.BBO_EQ, GetType(Int32), GetType(Boolean)),
            New BoundBinOperator(TokenKind.TK_NE, BoundBinOpToken.BBO_NE, GetType(Int32), GetType(Boolean))}

    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, typ As Type)
        SyntaxKind = synKind
        tok = kind
        LeftType = typ
        RightType = typ
        ResultType = typ
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, typ As Type, resType As Type)
        SyntaxKind = synKind
        tok = kind
        LeftType = typ
        RightType = typ
        ResultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, Ltyp As Type, Rtyp As Type, resType As Type)
        SyntaxKind = synKind
        tok = kind
        LeftType = Ltyp
        RightType = Rtyp
        ResultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Shared Function bind(kind As TokenKind, Ltype As Type, Rtype As Type)
        For Each op As BoundBinOperator In OPERATORS
            If (op.SyntaxKind = kind And op.LeftType = Ltype And op.RightType = Rtype) Then
                Return op
            End If
        Next

        Return Nothing
    End Function
End Class
