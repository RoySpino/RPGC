
Imports System.Collections.Generic
Imports System.Collections.Immutable


Public NotInheritable Class SeperatedSyntaxList(Of T As SyntaxNode)
    Inherits SeperatedSyntaxList_
    Interface IEnumerable(Of SyntaxNode)
    End Interface

    Private ReadOnly SeperatorsAndNodes As ImmutableArray(Of SyntaxNode)

    Public Sub New(lst As ImmutableArray(Of SyntaxNode))
        SeperatorsAndNodes = lst
    End Sub

    ' ///////////////////////////////////////////////////////////////////
    Public Function Count() As Integer
        Return (SeperatorsAndNodes.Length + 1) >> 1
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Public Function At(idx As Integer) As T
        If idx < 0 Or idx >= Count() Then
            Return Nothing
        End If

        Return SeperatorsAndNodes(idx * 2)
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Public Function getParamiterAt(idx As Integer) As T
        Return SeperatorsAndNodes(idx * 2)
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Public Function getSeperatorAt(idx As Integer) As SyntaxToken
        If idx < 0 Or idx >= Count() Then
            Return Nothing
        End If

        Return SeperatorsAndNodes(idx * 2 + 1)
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Public Overrides Function getWithSeperators() As ImmutableArray(Of SyntaxNode)
        Return SeperatorsAndNodes
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
        Return GetEnumerator()
    End Function

    ' ///////////////////////////////////////////////////////////////////
    Public Iterator Function GetEnumerator() As IEnumerator(Of SyntaxNode) Implements IEnumerable(Of T).GetEnumerator
        Dim cnt As Integer = Count()

        For i As Integer = 0 To (cnt - 1)
            Yield Me.At(i)
        Next
    End Function
End Class

' ////////////////////////////////////////////////////////////////////////////
' /////     /////     /////     /////     /////     /////     /////     /////
' //////////////////////////////////////////////////////////////////////////

Public MustInherit Class SeperatedSyntaxList_
    Public Sub New()
    End Sub

    Public MustOverride Function getWithSeperators() As ImmutableArray(Of SyntaxNode)
End Class