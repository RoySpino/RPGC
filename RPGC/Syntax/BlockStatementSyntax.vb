Imports System.Collections.Immutable

Public Class BlockStatementSyntax
    Inherits StatementSyntax

    Public Overrides Property kind As TokenKind = TokenKind.TK_BLOCKSYNTX

    Public Property BlockStartToken As SyntaxToken
    Public Property Statements As ImmutableArray(Of StatementSyntax)
    Public Property BlockEndToken As SyntaxToken

    Public Sub New(_blockStartToken As SyntaxToken, _statements As ImmutableArray(Of StatementSyntax), _blockEndToken As SyntaxToken)
        BlockEndToken = _blockStartToken
        Statements = _statements
        BlockEndToken = _blockEndToken
    End Sub
End Class
