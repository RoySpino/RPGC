Imports System.Collections.Immutable

Public Class BoundGlobalScope
    Public Property Preveous As BoundGlobalScope
    Public Property Diagnostic As ImmutableArray(Of Diagnostics)
    Public Property Variables As ImmutableArray(Of VariableSymbol)
    Public Property Statement As BoundStatement

    Public Sub New(prev As BoundGlobalScope, diag As ImmutableArray(Of Diagnostics), vars As ImmutableArray(Of VariableSymbol), stmnt As BoundStatement)
        Preveous = prev
        Diagnostic = diag
        Variables = vars
        Statement = stmnt
    End Sub
End Class
