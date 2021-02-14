Public Class SyntaxTree
    Private diagnostic As DiagnosticBag = New DiagnosticBag()
    Public ROOT As ExpresionSyntax
    Public EOFT As SyntaxToken
    Public text As SourceText

    Public Sub New(source As SourceText, _diagnostics As DiagnosticBag, _root As ExpresionSyntax, EndOfFileToken As SyntaxToken)
        text = source
        ROOT = _root
        EOFT = EndOfFileToken
        diagnostic = _diagnostics
    End Sub

    ' ///////////////////////////////////////////////////////////////////////
    Public Function getDgetDiagnosticsiag() As DiagnosticBag
        Return diagnostic
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Shared Function Parce(txt As String) As SyntaxTree
        Dim paar As Parser

        paar = New Parser(text)

        Return paar.parse()
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Shared Function Parce(text) As SyntaxTree
        Dim paar As Parser

        paar = New Parser(text)

        Return paar.parse()
    End Function
End Class
