Imports System.Collections.Immutable

Public Class FunctionSymbol
    Inherits Symbol

    Public Overrides Property Kind As SymbolKind = SymbolKind.SYM_FUNCTION
    Public Property Paramiter As ImmutableArray(Of ParamiterSymbol)
    Public Property Type_ As TypeSymbol

    Public Sub New(name As String, parm As ImmutableArray(Of ParamiterSymbol), retType As TypeSymbol)
        MyBase.New(name)

        Paramiter = parm
        Type_ = retType
    End Sub
End Class
