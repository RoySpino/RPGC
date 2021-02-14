Public Class Complation
    Public syntax As SyntaxTree
    Private diognost As DiagnosticBag = New DiagnosticBag()

    Public Sub New(tree As SyntaxTree)
        syntax = tree

        'diognost.AddRange(tree.getDiagnostics())
    End Sub

    Public Function evaluate(_variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult
        Dim bindr As Binder = New Binder(_variables)
        Dim boundExpr As BoundExpression = bindr.BindExpression(syntax.ROOT)
        Dim eval As Evaluator

        diognost.AddRange(syntax.getDiagnostics())
        diognost.AddRange(bindr.diagnostics)

        If diognost.Any() Then
            Return New EvaluationResult(diognost, Nothing)
        End If

        eval = New Evaluator(boundExpr, _variables)

        Dim value = eval.Evaluate()

        Return New EvaluationResult(Nothing, value)
    End Function
End Class
