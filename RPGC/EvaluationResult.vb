Imports System.Collections.Immutable

Public Class EvaluationResult
    Public Property _Diagnostics As ImmutableArray(Of Diagnostics)
    Public value As Object

    Public Sub New(diag As ImmutableArray(Of Diagnostics), val As Object)
        _Diagnostics = diag
        value = val
    End Sub
End Class
