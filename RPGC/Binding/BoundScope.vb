Imports System.Collections.Immutable

Public Class BoundScope
    Public variables As Dictionary(Of String, VariableSymbol) = New Dictionary(Of String, VariableSymbol)()
    Public Property Parant As BoundScope

    Public Sub New(par As BoundScope)
        Parant = par
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function declareVar(var As VariableSymbol) As Boolean
        If variables.ContainsKey(var.name) = True Then
            Return False
        End If

        variables.Add(var.name, var)
        Return True
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function lookupVar(name As String, ByRef var As VariableSymbol) As Boolean
        ' try to find the variale within this scope
        If variables.TryGetValue(name, var) = True Then
            Return True
        End If

        ' failed to find the variable check if there Is a parant
        If Parant Is Nothing Then
            Return False
        End If

        ' return parants variable
        Return Parant.lookupVar(name, var)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function getDeclaredVariables() As ImmutableArray(Of VariableSymbol)
        Return variables.Values.ToImmutableArray()
    End Function
End Class
