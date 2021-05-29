Imports System.Linq
Imports System.Collections.Immutable
Imports System.Collections.Generic

Public Class Lowerer
    Inherits BoundTreeRewriter

    Private _labelCount As Integer

    Private Sub New()
        '
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Private Function generateLable() As LabelSymbol
        Dim lbl As String

        lbl = $"^Lable{_labelCount}"
        _labelCount += 1

        Return New LabelSymbol(lbl)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function Lower(Statement As BoundStatement) As BoundBlockStatement
        Dim lorr As Lowerer
        Dim res As BoundStatement
        Dim results As BoundBlockStatement

        lorr = New Lowerer()
        res = lorr.rewriteStatement(Statement)
        results = flatten(res)

        Return results
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function flatten(stmnt As BoundStatement) As BoundBlockStatement
        Dim builder As ImmutableArray(Of BoundStatement).Builder
        Dim stack As Stack(Of BoundStatement)
        Dim current As BoundStatement
        Dim blocks As BoundBlockStatement

        builder = ImmutableArray.CreateBuilder(Of BoundStatement)
        stack = New Stack(Of BoundStatement)()
        stack.Push(stmnt)

        While stack.Count > 0
            current = stack.Pop()

            If GetType(BoundBlockStatement).Equals(current) Then
                blocks = current

                For Each s As BoundStatement In blocks.Statements.Reverse
                    stack.Push(s)
                Next
            Else
                builder.Add(current)
            End If
        End While

        Return New BoundBlockStatement(builder.ToImmutable())
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteStatement(statement As BoundStatement) As BoundStatement
    End Function
End Class
