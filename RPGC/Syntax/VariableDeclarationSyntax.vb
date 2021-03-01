Public Class VariableDeclarationSyntax
    Inherits StatementSyntax

    ' D<varNmae>        S             10  0 inz(0)
    ' Dcl-s <varNmae> zoned(10: 0) inz(0);
    Public Overrides Property kind As TokenKind = TokenKind.TK_VARDECLR
    Public Property Keyword As SyntaxToken
    Public Property Identifier As SyntaxToken
    Public Property Typ As Type
    Public Property InitKeyWord As SyntaxToken
    Public Property Initilizer As ExpresionSyntax


    Public Sub New(_keyword As SyntaxToken, _identifier As SyntaxToken, _typ As Type, Optional _initKeyWord As SyntaxToken = Nothing, Optional _initilizer As ExpresionSyntax = Nothing)
        Keyword = _keyword
        Identifier = _identifier
        Typ = _typ

        If InitKeyWord Is Nothing = False Then
            InitKeyWord = _initKeyWord
            Initilizer = _initilizer
        Else
            InitKeyWord = New SyntaxToken(TokenKind.TK_INZ, 0, 0, "INZ")

            Select Case _typ.Name
                Case "Boolean"
                    Initilizer = New LiteralExpressionSyntax(New SyntaxToken(TokenKind.TK_INDON, 0, 0, True))
                Case "Int32"
                    Initilizer = New LiteralExpressionSyntax(New SyntaxToken(TokenKind.TK_INTEGER, 0, 0, 0))
            End Select
        End If
    End Sub
End Class

