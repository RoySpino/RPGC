Imports System.Collections.Immutable
Imports System.Threading

Public Class Complation
    Public Property syntax As SyntaxTree
    Private diognost As DiagnosticBag = New DiagnosticBag()
    Private _globalScope As BoundGlobalScope
    Private previous As Complation

    Public Sub New(tree As SyntaxTree)
        syntax = tree
        previous = Nothing
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Sub New(prev As Complation, tree As SyntaxTree)
        syntax = tree
        previous = prev
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Friend ReadOnly Property GlobalScope_ As BoundGlobalScope
        Get
            Dim gs As BoundGlobalScope
            If _globalScope Is Nothing Then
                gs = Binder.bindGlobalScope(previous?.GlobalScope_, syntax.ROOT)
                Interlocked.CompareExchange(_globalScope, gs, Nothing)
            End If

            Return _globalScope
        End Get
    End Property

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Function continueWith(tree_ As SyntaxTree) As Complation
        Return New Complation(Me, tree_)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Function evaluate(_variables As Dictionary(Of VariableSymbol, Object)) As EvaluationResult
        Dim diognos As ImmutableArray(Of Diagnostics)
        Dim eval As Evaluator
        Dim value As Object

        diognos = syntax.getDiagnostics().Concat(globalScope_.Diagnostic).ToImmutableArray()

        If diognost.Any() Then
            Return New EvaluationResult(diognos, Nothing)
        End If

        eval = New Evaluator(_globalScope.Statement, _variables)
        value = eval.Evaluate()

        Return New EvaluationResult(ImmutableArray(Of Diagnostics).Empty, value)
    End Function
End Class
