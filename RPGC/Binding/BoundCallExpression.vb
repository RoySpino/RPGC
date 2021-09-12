

Imports System.Collections.Immutable

Public Class BoundCallExpression
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_CALLEXP
    Public Property Type As TypeSymbol
    Public Property function_ As FunctionSymbol
    Public Property args As ImmutableArray(Of BoundExpression)

    Public Sub New(fun As FunctionSymbol, arg As ImmutableArray(Of BoundExpression))
        '
    End Sub
End Class
