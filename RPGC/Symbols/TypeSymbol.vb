Public Class TypeSymbol
    Inherits Symbol

    Public Overrides Property kind As SymbolKind = SymbolKind.SYM_TYPE
    Public Shared ReadOnly Integer_ As TypeSymbol = New TypeSymbol("INT(10)")
    Public Shared ReadOnly Indicator As TypeSymbol = New TypeSymbol("IND")
    Public Shared ReadOnly Char_ As TypeSymbol = New TypeSymbol("CHAR")
    Public Shared ReadOnly Varchar As TypeSymbol = New TypeSymbol("VARCHAR")
    Public Shared ReadOnly Date_ As TypeSymbol = New TypeSymbol("DATE")
    Public Shared ReadOnly Time As TypeSymbol = New TypeSymbol("TIME")
    Public Shared ReadOnly DateTime As TypeSymbol = New TypeSymbol("DATETIME")
    Public Shared ReadOnly Float As TypeSymbol = New TypeSymbol("FLOAT(8)")
    Public Shared ReadOnly Void As TypeSymbol = New TypeSymbol("?")
    Public Shared ReadOnly Error_ As TypeSymbol = New TypeSymbol("?")

    Public Sub New(nam As String)
        MyBase.New(nam)
    End Sub
End Class
