Public Class TextSpan
    Public START, LENGTH, sEND As Integer

    Public Sub New(strt As Integer, len As Integer)
        START = strt
        LENGTH = len
        sEND = strt + len
    End Sub

    Public Shared Function fromBounds(strStart As Integer, strEnd As Integer) As TextSpan
        Dim len As Integer

        len = strEnd - strStart

        Return New TextSpan(strStart, strEnd)
    End Function
End Class
