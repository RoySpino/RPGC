Public Class VariableSymbol
    Public name As String
    Public _type As Type

    Public Sub New(nam As String, typ As Type)
        name = nam
        _type = typ
    End Sub
End Class
