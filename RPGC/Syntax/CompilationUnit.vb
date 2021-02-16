Public Class CompilationUnit
    Inherits SyntaxNode

    Public Property Expression As ExpresionSyntax
    Public Property EndOfFileToken As SyntaxToken

    Public Sub New(exp As ExpresionSyntax, eofToken As SyntaxToken)
        Expression = exp
        EndOfFileToken = eofToken
    End Sub
End Class
