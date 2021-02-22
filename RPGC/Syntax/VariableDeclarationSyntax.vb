Public Class VariableDeclarationSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_VARDECLR
    Public Property Keyword As SyntaxToken
    Public Property Identifier As SyntaxToken
    Public Property Typ As Type
    Public Property InitKeyWord As SyntaxToken
    Public Property Initilizerl As ExpresionSyntax


    Public Sub New(_keyword As SyntaxToken, _identifier As SyntaxToken, _typ As Type, _initKeyWord As SyntaxToken, _initilizer As ExpresionSyntax)
        Keyword = _keyword
        Identifier = _identifier
        Typ = _typ
        InitKeyWord = _initKeyWord
        Initilizerl = _initilizer
    End Sub
End Class

