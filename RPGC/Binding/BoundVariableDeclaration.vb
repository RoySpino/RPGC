Public Class BoundVariableDeclaration
    Inherits BoundStatement

    Public Property Variable As VariableSymbol
    Public Property Initalizer As BoundExpression
    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_VARDECLR

    Public Sub New(var_ As VariableSymbol, init As BoundExpression)
        Variable = var_
        Initalizer = init
    End Sub
End Class
