Public Class VariableSymbol
    Public Property name As String
    Public Property _type As Type
    Public Property IsReadOnly As Boolean

    Public Sub New(nam As String, typ As Type, Optional cons As Boolean = False)
        name = nam
        _type = typ
        IsReadOnly = cons
    End Sub
End Class
