Public Class SyntaxTree
    Private diagnostic As DiagnosticBag
    Public ROOT As CompilationUnit
    Public EOFT As SyntaxToken
    Public text As SourceText

    Private Sub New(source As SourceText)
        Dim par As Parser
        Dim rot As CompilationUnit
        Dim diagnos As DiagnosticBag

        par = New Parser(source)
        rot = par.parseCompilationUnit()
        diagnos = par.getDiagnostics()

        text = source
        ROOT = rot
        EOFT = ROOT.EndOfFileToken
        diagnostic = diagnos
    End Sub

    ' ///////////////////////////////////////////////////////////////////////
    Public Function getDiagnostics() As DiagnosticBag
        Return diagnostic
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Shared Function Parce(txt As String) As SyntaxTree
        Dim SourceText As SourceText = SourceText.from(txt)

        Return Parce(SourceText)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Shared Function Parce(text As SourceText) As SyntaxTree
        Return New SyntaxTree(text)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Shared Iterator Function parceToken(text As SourceText) As IEnumerable(Of SyntaxToken)
        Dim lex As Lexer
        Dim token As SyntaxToken

        lex = New Lexer(text)

        While True
            token = lex.doLex()
            If token.kind = TokenKind.TK_EOI Then
                Exit While
            End If

            Yield token
        End While
    End Function
End Class
