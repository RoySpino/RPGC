Public Class Diagnostics
    Public SPAN As TextSpan
    Private MESSAGE As String

    Public Sub New(spn As TextSpan, msg As String)
        SPAN = spn
        MESSAGE = msg
    End Sub

    ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 
    Public Overrides Function ToString() As String
        Return MESSAGE
    End Function
End Class
