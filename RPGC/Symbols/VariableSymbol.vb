Public Class VariableSymbol
    Inherits Symbol
    Public Overrides Property Kind As SymbolKind = SymbolKind.SYM_VARIABLE
    Public Property Type_ As TypeSymbol
    Public Property IsReadonly_ As Boolean

    Public Sub New(name As String, isReadOnly As Boolean, _type As TypeSymbol)
        MyBase.New(name)

        Type_ = _type
        IsReadonly_ = isReadOnly
    End Sub
End Class
