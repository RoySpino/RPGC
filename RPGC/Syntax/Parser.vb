Imports System.Collections.Immutable

Public Class Parser
    Private lex As Lexer = Nothing
    Private tok As TokenKind
    Private current As TokenKind
    Private pos As Integer
    Private tcount As Integer
    Private diagnostics As DiagnosticBag
    Public ReadOnly tokens As ImmutableArray(Of SyntaxToken)

    Public Sub New(txt As String)
        Dim _tokens As List(Of SyntaxToken) = New List(Of SyntaxToken)()

        lex = New Lexer(txt)

        pos = 0

        While True
            If True Then

            End If
        End While
    End Sub
End Class
