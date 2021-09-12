Public Class StructNode
    Public linePos As Integer
    Public chrPos As Integer
    Public symbol As String

    Public Sub New(l As Integer, ch As Integer, sym As String)
        linePos = l
        chrPos = ch
        symbol = sym
    End Sub
End Class
