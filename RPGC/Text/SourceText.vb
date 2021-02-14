Imports System.Collections.Immutable

Public Class SourceText
    Public Property Lines As ImmutableArray(Of TextLines)

    Public Sub New(txt As String)
        _text = txt
        Lines = parseLines(Me, txt)
    End Sub

    Public Shared Function from(txt As String)
        Return New SourceText(txt)
    End Function
End Class
