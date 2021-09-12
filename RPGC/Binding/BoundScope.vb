Imports System.Collections.Immutable

Public Class BoundScope
    Public variables As Dictionary(Of String, VariableSymbol) = New Dictionary(Of String, VariableSymbol)()
    Public functions As Dictionary(Of String, FunctionSymbol) = New Dictionary(Of String, FunctionSymbol)()
    Public Property Parant As BoundScope

    Public Sub New(par As BoundScope)
        Parant = par

        If (par Is Nothing) = False Then
            variables = New Dictionary(Of String, VariableSymbol)(par.variables)
        End If
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function declareVar(var As VariableSymbol) As Boolean
        If variables Is Nothing Then
            variables = New Dictionary(Of String, VariableSymbol)()
        End If

        If variables.ContainsKey(var.Name) = True Then
            Return False
        End If

        variables.Add(var.name, var)
        Return True
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function lookupVar(name As String, ByRef var As VariableSymbol) As Boolean
        ' failed to find the variable check if there Is a parant
        If variables Is Nothing Then
            variables = New Dictionary(Of String, VariableSymbol)()
        End If

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

    ' ///////////////////////////////////////////////////////////////////////////
    Public Function checkLocalVariables(name As String) As Boolean
        Dim dmy As VariableSymbol = Nothing

        ' dont do no variables declared
        If variables Is Nothing Then
            Return False
        End If

        ' try to find the variale within this scope
        If variables.TryGetValue(name, dmy) = True Then
            Return True
        End If

        Return False
    End Function



    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    ' ////////////////////////////////////////////////////////////////////////////////////////////////////
    ' ///////////////////////////////////////////////////////////////////////////
    Public Function declareFunciton(funct As FunctionSymbol) As Boolean
        If functions Is Nothing Then
            functions = New Dictionary(Of String, FunctionSymbol)()
        End If

        If functions.ContainsKey(funct.Name) = True Then
            Return False
        End If

        functions.Add(funct.Name, funct)

        Return True
    End Function


    ' ///////////////////////////////////////////////////////////////////////////
    Public Function lookupFuncion(name As String, var As FunctionSymbol) As Boolean
        If functions Is Nothing Then
            functions = New Dictionary(Of String, FunctionSymbol)()
        End If

        ' try to find the variale within this scope
        If functions.TryGetValue(name, var) = True Then
            Return True
        End If

        ' failed to find the variable check if there Is a parant
        If Parant Is Nothing Then
            Return False
        End If

        ' return parants variable
        Return Parant.lookupFuncion(name, var)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Function checkLocalFunctions(name As String) As Boolean
        Dim xtn As String

        ' dont do no functions declared
        If functions Is Nothing Then
            Return False
        End If

        ' try to find the function within this scope
        xtn = (From fn In functions
               Where fn.Key = name).FirstOrDefault().Key

        ' Not found in current scope check parant
        If xtn Is Nothing Then
            If (Parant Is Nothing) = False Then
                Parant.checkLocalFunctions(name)
            End If
        Else
            Return False
        End If

        ' return true when found
        Return True
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////
    Public Function getDeclaredVariables() As ImmutableArray(Of VariableSymbol)
        If variables Is Nothing Then
            Return ImmutableArray(Of VariableSymbol).Empty
        End If

        Return variables.Values.ToImmutableArray()
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Function getDeclaredFunctions() As ImmutableArray(Of FunctionSymbol)
        If functions Is Nothing Then
            Return ImmutableArray(Of FunctionSymbol).Empty
        End If

        Return functions.Values.ToImmutableArray()
    End Function

End Class
