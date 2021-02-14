Public Class LiteralExpressionSyntax
    Inherits ExpresionSyntax

    Public Property LiterToken As SyntaxToken
    Public value As Object

    Public Sub New(token As SyntaxToken)
        Me.New(token, token.sym)
    End Sub

    ' /////////////////////////////////////////////////////////////////////
    Public Sub New(token As SyntaxToken, val As Object)
        kind = TokenKind.TK_LITEXPR

        Select Case token.tok
            Case TokenKind.TK_INTEGER,
                 TokenKind.TK_ZONED,
                 TokenKind.TK_PACKED
                value = Convert.ToInt32(val)
            Case Else
                value = val
        End Select

        LiterToken = token
    End Sub
End Class
