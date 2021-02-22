Imports System.Collections.Immutable

Public Class BoundBlockStatement
    Inherits BoundStatement

    Public Property Statements As ImmutableArray(Of BoundStatement)
    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_BLOCKSTMT

    Public Sub New(stmnt As ImmutableArray(Of BoundStatement))
        Statements = stmnt
    End Sub
End Class
