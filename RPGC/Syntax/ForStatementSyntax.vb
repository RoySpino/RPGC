Public Class ForStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_FOR
    Public Property Keyword As SyntaxToken
    Public Property Identifier As SyntaxToken
    Public Property EqualsToken As SyntaxToken
    Public Property LowerBound As ExpresionSyntax
    Public Property Keyword_To As SyntaxToken
    Public Property UpperBound As ExpresionSyntax
    Public Property Keyword_By As SyntaxToken
    Public Property Increment As ExpresionSyntax
    Public Property Body As StatementSyntax

    Public Sub New(keywrd As SyntaxToken, identfir As SyntaxToken, eqTok As SyntaxToken, lowrBound As ExpresionSyntax, keywrd_To As SyntaxToken, uprBound As ExpresionSyntax, bdy As StatementSyntax, Optional keywrd_By As SyntaxToken = Nothing, Optional inc As ExpresionSyntax = Nothing)
        Keyword = keywrd
        Identifier = identfir
        EqualsToken = eqTok
        LowerBound = lowrBound
        UpperBound = uprBound
        Keyword_To = keywrd_To
        Body = bdy

        If keywrd_By Is Nothing Then
            Keyword_By = New SyntaxToken(TokenKind.TK_TO, 0, 0, "TO")
        Else
            Keyword_To = keywrd_By

            If inc Is Nothing Then
                Increment = New LiteralExpressionSyntax(New SyntaxToken(TokenKind.TK_INTEGER, 0, 0, "1"))
            Else
                Increment = inc
            End If
        End If
    End Sub
End Class
