Public Class EvaluationResult
    Public _Diagnostics As DiagnosticBag = New DiagnosticBag()
    Public value As Object

    Public Sub New(diag As DiagnosticBag, val As Object)
        _Diagnostics = diag
        value = val
    End Sub
End Class
