Imports System.Collections.Immutable
Imports System.IO
Imports System.Threading

Public Class Complation
    Public Property syntax As SyntaxTree
    Private diognost As DiagnosticBag = New DiagnosticBag()
    Private _globalScope As BoundGlobalScope
    Private previous As Complation

    Public Sub New(tree As SyntaxTree)
        syntax = tree
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
        Dim st As BoundBlockStatement
        Dim eval As Evaluator
        Dim value As Object
        ' BoundGlobalScope globalScope = Binder.bindGlobalScope(,Syntax.ROOT);
        ' BoundExpression boundExpr = globalScope.Expression;

        ' call property {globalScope_} to bind syntax tree
        diognos = syntax.getDiagnostics().Concat(GlobalScope_.Diagnostic).ToImmutableArray()

        If diognos.Any() Then
            Return New EvaluationResult(diognos, Nothing)
        End If

        st = getStatement()
        eval = New Evaluator(st, _variables)
        value = eval.Evaluate()

        Return New EvaluationResult(ImmutableArray(Of Diagnostics).Empty, value)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Sub emitTree(writer As TextWriter)
        Dim stmnt As BoundStatement

        stmnt = getStatement()
        _globalScope.Statement.writeTo(writer)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function getStatement() As BoundBlockStatement
        Dim stmnt As BoundStatement

        stmnt = _globalScope.Statement
        Return Lowerer.Lower(stmnt)
    End Function
End Class
