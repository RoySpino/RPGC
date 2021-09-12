Public Class ParamiterSymbol
    Inherits VariableSymbol

    Public Overrides Property Kind As SymbolKind = SymbolKind.SYM_PARAMITER
    Public Overloads Property Type_ As TypeSymbol

    Public Sub New(name As String, type As TypeSymbol)
        MyBase.New(name, True, type)

        Type_ = type
    End Sub
End Class
