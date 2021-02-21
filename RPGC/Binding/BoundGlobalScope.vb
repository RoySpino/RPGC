Imports System.Collections.Immutable

Public Class BoundGlobalScope
    Public Property Preveous As BoundGlobalScope
    Public Property Diagnostic As ImmutableArray(Of Diagnostics)
    Public Property Variables As ImmutableArray(Of VariableSymbol)
    Public Property Expression As BoundExpression

    Public Sub New(prev As BoundGlobalScope, diag As ImmutableArray(Of Diagnostics), vars As ImmutableArray(Of VariableSymbol), expr As BoundExpression)
        Preveous = prev
        Diagnostic = diag
        Variables = vars
        Expression = expr
    End Sub
End Class
