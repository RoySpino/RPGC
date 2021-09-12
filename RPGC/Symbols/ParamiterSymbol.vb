Public Class ParamiterSymbol
    Inherits VariableSymbol

    Public Overrides Property Kind As SymbolKind = SymbolKind.SYM_PARAMITER
    Public Property Type_ As TypeSymbol


    Public Overrides Property Kind As SymbolKind
        Get
            Return MyBase.Kind
        End Get
        Set(value As SymbolKind)
            MyBase.Kind = value
        End Set
    End Property

    Public Sub New(name As String, type As TypeSymbol)
        MyBase.New(name, True, type)

        Type_ = type
    End Sub
End Class
