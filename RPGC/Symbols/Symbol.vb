Public Class Symbol
    Property Name As String
    MustOverride Property Kind As SymbolKind

    Public Sub New(nam As String)
        Name = nam
    End Sub

    ' /////////////////////////////////////////////////////////////////////
    Public Overrides Function ToString() As String
        Return Name
    End Function
End Class

' ////////////////////////////////////////////////////////////////////////////
' /////     /////     /////     /////     /////     /////     /////     /////
' //////////////////////////////////////////////////////////////////////////

Public Enum SymbolKind
    SYM_VARIABLE
    SYM_FUNCTION
    SYM_PROCEDURE
    SYM_SUBRUTINE
    SYM_TYPE
    SYM_PARAMITER
End Enum