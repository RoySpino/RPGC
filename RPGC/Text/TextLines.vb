Public Class TextLines
    Public Property Text As SourceText
    Public Property Start As Integer
    Public Property Length As Integer
    Public Property LengthWithLineBreak As Integer
    Public spanEnd As Integer

    Public Sub New(txt As sourceText, strt As Integer, len As Integer, lwlb As Integer)
        Text = txt
        Start = strt
        Length = len
        LengthWithLineBreak = lwlb
    End Sub

    ' /////////////////////////////////////////////////////////////////////////////
    Public Function Span() As TextSpan
        Return New TextSpan(Start, Length)
    End Function

    ' /////////////////////////////////////////////////////////////////////////////
    Public Function SpanWithLineBreak() As TextSpan
        Return New TextSpan(Start, LengthWithLineBreak)
    End Function

    ' /////////////////////////////////////////////////////////////////////////////
    Public Overrides Function ToString() As String
        Return Text.ToString(Span())
    End Function
End Class
