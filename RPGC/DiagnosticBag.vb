Imports System.Linq
Public Class DiagnosticBag
    Private _diagnostic As List(Of Diagnostics) = New List(Of Diagnostics)()

    Public Sub report(spn As TextSpan, msg As String)
        Dim diag As Diagnostics

        diag = New Diagnostics(spn, msg)
        _diagnostic.Add(diag)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub report(msg As String, start As Integer, len As Integer)
        Dim diag As Diagnostics

        diag = New Diagnostics(New TextSpan(start, len), msg)
        _diagnostic.Add(diag)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportInvalidNumber(symbol As String, typVal As Type, str As Integer, len As Integer)
        Dim msg As String

        msg = String.Format("rpgc: the symbol {0} is not a valid {1}", symbol, typVal)
        report(msg, str, len)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportBadCharacter(symbol As String, position As Integer)
        Dim msg As String

        msg = String.Format("rpgc: the symbol {0} is not a valid {1}", symbol)
        report(msg, (position - 1), 1)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportUnexpectedToken(spn As TextSpan, actual As TokenKind, )
        Dim msg As String

        msg = String.Format("rpgc: the symbol {0} is not a valid {1}", symbol)
        report(msg, (position - 1), 1)
    End Sub
End Class
