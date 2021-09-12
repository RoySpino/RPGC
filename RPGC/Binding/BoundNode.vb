Imports System.IO
Imports System.Reflection

Public MustInherit Class BoundNode
    Public Overridable Property tok As BoundNodeToken

    Public Iterator Function getChildren() As IEnumerable(Of BoundNode)
        Dim properties() As PropertyInfo
        Dim children As IEnumerable(Of BoundNode) = Nothing
        Dim chd As BoundNode

        properties = Me.GetType().GetProperties(BindingFlags.Public Or BindingFlags.Instance)

        For Each prop As PropertyInfo In properties
            If (GetType(BoundNode).IsAssignableFrom(prop.PropertyType)) Then
                chd = prop.GetValue(Me)
                If chd Is Nothing = False Then
                    Yield chd
                End If
            Else
                If (GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(prop.PropertyType)) Then
                    children = prop.GetValue(Me)
                    For Each child As BoundNode In children
                        If child Is Nothing = False Then
                            Yield child
                        End If
                    Next
                End If
            End If
        Next
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Public Iterator Function getProperties() As IEnumerable(Of (name As String, value As Object))
        Dim properties() As PropertyInfo
        Dim value As Object

        properties = Me.GetType().GetProperties(BindingFlags.Public Or BindingFlags.Instance)

        For Each prop As PropertyInfo In properties
            If prop.Name = NameOf(tok) Or prop.Name = NameOf(BoundBinExpression.OP) Then
                Continue For
            End If

            If GetType(BoundNode).IsAssignableFrom(prop.PropertyType) Or
                GetType(IEnumerable(Of BoundNode)).IsAssignableFrom(prop.PropertyType) Then
                Continue For
            End If

            value = prop.GetValue(Me)
            If value Is Nothing = False Then
                Yield (prop.Name, value)
            End If
        Next
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Private Shared Sub printTree(writer As TextWriter, node As BoundNode, Optional indent As String = "", Optional isLast As Boolean = True)
        Dim lastChild As BoundNode
        Dim marker As String
        Dim text As String
        Dim isToConsole, isFirstProp As Boolean
        Dim xtmp As IEnumerable(Of BoundNode)

        If node Is Nothing Then
            Return
        End If

        isToConsole = writer.Equals(Console.Out)
        marker = IIf((isLast = True), "└──", "├──")

        If isToConsole = True Then
            Console.ForegroundColor = ConsoleColor.Gray
        End If

        writer.Write(indent & marker)

        Console.ForegroundColor = getConsoleColor(node)
        text = getText(node)
        writer.Write(text)
        Console.ResetColor()

        isFirstProp = True

        For Each v As (name As String, value As Object) In node.getProperties()
            If isFirstProp = True Then
                isFirstProp = False
            Else
                Console.ForegroundColor = ConsoleColor.DarkYellow
                writer.Write(", ")
            End If

            Console.ForegroundColor = ConsoleColor.Green
            writer.Write(" " & v.name)
            Console.ForegroundColor = ConsoleColor.Gray
            writer.Write(" = ")
            Console.ForegroundColor = ConsoleColor.Cyan
            writer.Write(v.value)
        Next

        writer.WriteLine()
        indent &= IIf(isLast = True, "   ", "|  ")

        lastChild = node.getChildren().LastOrDefault()
        xtmp = node.getChildren()

        For Each child As BoundNode In xtmp
            printTree(writer, child, indent, child.Equals(lastChild))
        Next
    End Sub

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function getText(node As BoundNode) As String
        Dim t1 As BoundBinExpression
        Dim u1 As BoundUniExpression

        If node Is GetType(BoundBinExpression) Then
            t1 = node
            Return t1.OP.tok.ToString() & "_Expression"
        End If

        If node Is GetType(BoundUniExpression) Then
            u1 = node
            Return u1.OP.tok.ToString() & "_Expression"
        End If

        Return node.tok.ToString()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function getConsoleColor(node As BoundNode) As ConsoleColor
        If node Is GetType(BoundExpression) Then
            Return ConsoleColor.DarkCyan
        End If

        If node Is GetType(BoundStatement) Then
            Return ConsoleColor.Magenta
        End If

        Return ConsoleColor.Gray
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Public Sub writeTo(writer As TextWriter)
        printTree(writer, Me)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Function ToString() As String
        Using writer As StringWriter = New StringWriter()
            writeTo(writer)
            Return writer.ToString()
        End Using
    End Function
End Class
