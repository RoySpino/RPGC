﻿Public Class AssignmentExpressionSyntax
    Public Property IDENTIFIERTOKEN As SyntaxToken
    Public Property ASSIGNMENTTOKEN As SyntaxToken
    Public Property EXPRESSION As ExpresionSyntax

    Public Sub New(identifer As SyntaxToken, assignmentSymbol As SyntaxToken, express As ExpresionSyntax)
        IDENTIFIERTOKEN = identifer
        ASSIGNMENTTOKEN = assignmentSymbol
        EXPRESSION = express
    End Sub
End Class
