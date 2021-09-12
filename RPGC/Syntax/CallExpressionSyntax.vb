Public Class CallExpressionSyntax
    Inherits ExpresionSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_CALL
    Public Property FunctionName As SyntaxToken
    Public Property CloseParen As SyntaxToken
    Public Property Arguments As SeperatedSyntaxList(Of ExpresionSyntax)
    Public Property OpenParen As SyntaxToken

    Public Sub New(fucnName As SyntaxToken, openPar As SyntaxToken, args As SeperatedSyntaxList(Of ExpresionSyntax), closePar As SyntaxToken)
        FunctionName = fucnName
        Arguments = args
        OpenParen = openPar
        CloseParen = closePar
    End Sub
End Class
