Public Class BoundBinOperator
    Public SyntaxKind As TokenKind
    Public tok As BoundBinOpToken
    Public RightType As TypeSymbol
    Public LeftType As TypeSymbol
    Public ResultType As TypeSymbol

    Public Shared OPERATORS() As BoundBinOperator = {
            New BoundBinOperator(TokenKind.TK_ADD, BoundBinOpToken.BBO_ADD, TypeSymbol.Integer_),
            New BoundBinOperator(TokenKind.TK_SUB, BoundBinOpToken.BBO_SUB, TypeSymbol.Integer_),
            New BoundBinOperator(TokenKind.TK_MULT, BoundBinOpToken.BBO_MULT, TypeSymbol.Integer_),
            New BoundBinOperator(TokenKind.TK_DIV, BoundBinOpToken.BBO_DIV, TypeSymbol.Integer_),
            New BoundBinOperator(TokenKind.TK_AND, BoundBinOpToken.BBO_AND, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_OR, BoundBinOpToken.BBO_OR, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_EQ, BoundBinOpToken.BBO_EQ, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_NE, BoundBinOpToken.BBO_NE, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_GE, BoundBinOpToken.BBO_GE, TypeSymbol.Integer_, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_GT, BoundBinOpToken.BBO_GT, TypeSymbol.Integer_, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_LE, BoundBinOpToken.BBO_LE, TypeSymbol.Integer_, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_LT, BoundBinOpToken.BBO_LT, TypeSymbol.Integer_, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_EQ, BoundBinOpToken.BBO_EQ, TypeSymbol.Integer_, TypeSymbol.Indicator),
            New BoundBinOperator(TokenKind.TK_NE, BoundBinOpToken.BBO_NE, TypeSymbol.Integer_, TypeSymbol.Indicator)}

    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, typ As TypeSymbol)
        SyntaxKind = synKind
        tok = kind
        LeftType = typ
        RightType = typ
        ResultType = typ
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, typ As TypeSymbol, resType As TypeSymbol)
        SyntaxKind = synKind
        tok = kind
        LeftType = typ
        RightType = typ
        ResultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Sub New(synKind As TokenKind, kind As BoundBinOpToken, Ltyp As TypeSymbol, Rtyp As TypeSymbol, resType As TypeSymbol)
        SyntaxKind = synKind
        tok = kind
        LeftType = Ltyp
        RightType = Rtyp
        ResultType = resType
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Shared Function bind(kind As TokenKind, Ltype As TypeSymbol, Rtype As TypeSymbol)
        For Each op As BoundBinOperator In OPERATORS
            If op.SyntaxKind = kind And op.LeftType.Equals(Ltype) = True And op.RightType.Equals(Rtype) = True Then
                Return op
            End If
        Next

        Return Nothing
    End Function
End Class
