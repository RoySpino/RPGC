Imports System.IO
Imports System.Reflection

Public Class SyntaxNode
    Public Overridable Property kind As TokenKind
    Public Property sym As Object

    Public Sub New()
        kind = 0
    End Sub

    ' /////////////////////////////////////////////////////////////////////////
    Public Iterator Function getCildren() As IEnumerable(Of SyntaxNode)
        Dim properties() As PropertyInfo
        Dim children As IEnumerable(Of SyntaxNode) = Nothing

        properties = Me.GetType().GetProperties(BindingFlags.Public Or BindingFlags.Instance)

        For Each prop As PropertyInfo In properties
            If (GetType(SyntaxNode).IsAssignableFrom(prop.PropertyType)) Then
                Yield prop.GetValue(Me)
            Else
                If (GetType(IEnumerable(Of SyntaxNode)).IsAssignableFrom(prop.PropertyType)) Then

                    children = prop.GetValue(Me)
                    For Each child As SyntaxNode In children
                        Yield child
                    Next
                End If
            End If
        Next
    End Function

    ' /////////////////////////////////////////////////////////////////////////
    Public ReadOnly Property Span As TextSpan
        Get
            Dim first As TextSpan = getCildren().First.Span
            Dim last As TextSpan = getCildren().Last.Span
            Return TextSpan.fromBounds(first.START, last.sEND)
        End Get
    End Property

    ' /////////////////////////////////////////////////////////////////////////
    Private Shared Sub printTree(txtWriter As TextWriter, node As SyntaxNode, Optional indent As String = "", Optional isLast As Boolean = True)
        Dim t As SyntaxToken

        If node Is Nothing Then
            Return
        End If

        Dim marker As String = IIf((isLast = True), "└──", "├──")
        txtWriter.Write(indent + marker + node.kind)

        If node.GetType.Equals(GetType(SyntaxToken)) Then
            t = node
            txtWriter.Write(" " & t.sym)
        End If

        txtWriter.WriteLine("")

        Dim lastChild = node.getCildren().LastOrDefault()

        indent += IIf((isLast = True), "   ", "│  ")
        For Each child As SyntaxNode In node.getCildren()
            printTree(txtWriter, child, indent, child.Equals(lastChild))
        Next
    End Sub

    ' /////////////////////////////////////////////////////////////////////////
    Public Sub writeTo(writer As TextWriter)
        printTree(writer, Me)
    End Sub

    ' /////////////////////////////////////////////////////////////////////////
    Public Overrides Function ToString() As String
        Using writer As StringWriter = New StringWriter()
            writeTo(writer)
            Return writer.ToString()
        End Using
    End Function


    'Public MustOverride Function getCildren() As IEnumerable(Of SyntaxNode)

End Class
