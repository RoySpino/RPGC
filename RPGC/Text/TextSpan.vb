Public Class TextSpan
    Public START, LENGTH, sEND As Integer
    Public LineNo, LinePos As Integer

    Public Sub New(strt As Integer, len As Integer, Optional ln As Integer = 0, Optional lp As Integer = 0)
        START = strt
        LENGTH = len
        sEND = strt + len
        LineNo = ln
        LinePos = lp
    End Sub

    Public Shared Function fromBounds(strStart As Integer, strEnd As Integer, Optional ln As Integer = 0, Optional lp As Integer = 0) As TextSpan
        Dim len As Integer

        len = strEnd - strStart

        Return New TextSpan(strStart, strEnd, ln, lp)
    End Function
End Class
