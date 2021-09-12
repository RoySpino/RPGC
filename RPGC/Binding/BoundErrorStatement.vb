Public Class BoundErrorStatement
    Inherits BoundStatement

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_ERROREXP

    Sub New()
        '
    End Sub
End Class
