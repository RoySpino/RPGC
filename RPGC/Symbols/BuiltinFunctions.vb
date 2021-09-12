Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Linq

Public Class BuiltinFunctions
    Public Shared ReadOnly dsply As FunctionSymbol = New FunctionSymbol("dsply", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Char_)), TypeSymbol.Void)
    Public Shared ReadOnly cout As FunctionSymbol = New FunctionSymbol("cout", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Char_)), TypeSymbol.Void)
    Public Shared ReadOnly cin As FunctionSymbol = New FunctionSymbol("cin", ImmutableArray.Create(New ParamiterSymbol("", TypeSymbol.Char_)), TypeSymbol.Char_)
    Public Shared ReadOnly cat As FunctionSymbol = New FunctionSymbol("cat", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Char_)), TypeSymbol.Char_)
    Public Shared ReadOnly BIF_rem As FunctionSymbol = New FunctionSymbol("rem", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_), New ParamiterSymbol("B", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_abs As FunctionSymbol = New FunctionSymbol("abs", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_char As FunctionSymbol = New FunctionSymbol("char", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Char_)
    Public Shared ReadOnly BIF_check As FunctionSymbol = New FunctionSymbol("check", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_), New ParamiterSymbol("B", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_checkr As FunctionSymbol = New FunctionSymbol("checkr", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_), New ParamiterSymbol("B", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_date As FunctionSymbol = New FunctionSymbol("date", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Date_)
    Public Shared ReadOnly BIF_dec As FunctionSymbol = New FunctionSymbol("dec", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_dech As FunctionSymbol = New FunctionSymbol("dech", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_diff As FunctionSymbol = New FunctionSymbol("diff", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_), New ParamiterSymbol("B", TypeSymbol.Integer_), New ParamiterSymbol("C", TypeSymbol.Integer_)), TypeSymbol.Integer_)
    Public Shared ReadOnly BIF_trim As FunctionSymbol = New FunctionSymbol("trim", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Integer_)), TypeSymbol.Char_)
    Public Shared ReadOnly BIF_Lookup As FunctionSymbol = New FunctionSymbol("lookup", ImmutableArray.Create(New ParamiterSymbol("A", TypeSymbol.Char_), New ParamiterSymbol("B", TypeSymbol.Char_)), TypeSymbol.Integer_)

    Public Shared Function getAll() As IEnumerable(Of FunctionSymbol)
        Dim lst As List(Of FunctionSymbol) = New List(Of FunctionSymbol)()
        Dim arr() As FieldInfo

        arr = GetType(BuiltinFunctions).GetFields(BindingFlags.Public Or BindingFlags.Static)

        arr = (From fun In arr
               Where fun.FieldType = GetType(FunctionSymbol)
               Select fun.GetValue(Nothing)).ToArray()
        ' Return arr.Where(Function(fun) fun.FieldType = GetType(FunctionSymbol)) _
        '           .Select(Function(fun) fun.GetValue(Nothing))\



        For Each fun As FieldInfo In GetType(BuiltinFunctions).GetFields(BindingFlags.Public Or BindingFlags.Static)
            If fun.FieldType = GetType(FunctionSymbol) Then
                lst.Add(fun.GetValue(Nothing))
            End If
        Next

        Return lst.ToImmutableArray()
    End Function

End Class
