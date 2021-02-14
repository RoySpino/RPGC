Public Class StructNode
    Public linePos As Integer
    Public charPos As Integer
    Public symbol As String

    Public Sub New(l As Integer, ch As Integer, sym As String)
        linePos = l
        charPos = ch
        symbol = sym
    End Sub
End Class
