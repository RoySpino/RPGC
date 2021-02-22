Public Class CompilationUnit
    Inherits SyntaxNode

    Public Property Statement As StatementSyntax
    Public Property EndOfFileToken As SyntaxToken

    Public Sub New(_satement As StatementSyntax, eofToken As SyntaxToken)
        Statement = _satement
        EndOfFileToken = eofToken
    End Sub
End Class
