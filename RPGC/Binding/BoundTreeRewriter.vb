Public Class BoundTreeRewriter
    Inherits BoundNode

    Public Function rewriteStatement(node As BoundStatement) As BoundStatement
        Select Case node.tok
            Case BoundNodeToken.BNT_BLOCKSTMT
                Return rewriteBlockStatement(node)
            Case BoundNodeToken.BNT_EXPRSTMT
                Return rewriteExpressionStatement(node)
            Case BoundNodeToken.BNT_VARDECLR
                Return rewriteVariableDeclaration(node)
            Case BoundNodeToken.BNT_IFSTMT
                Return rewriteIfStatement(node)
            Case BoundNodeToken.BNT_WHILESTMT
                Return rewriteBoundWhileStatement(node)
            Case BoundNodeToken.BNT_FORSTMT
                Return rewriteBoundForStatement(node)
            Case BoundNodeToken.BNT_DOUNTIL
                Return rewriteBoundDoUntilStatement(node)
            Case BoundNodeToken.BNT_LABEL
                Return rewriteBoundLabelStatement(node)
            Case BoundNodeToken.BNT_GOTO
                Return rewriteBoundGoToStatement(node)
            Case BoundNodeToken.BNT_GOTOCOND
                Return rewriteBoundGoToConditinalStatement(node)
            Case Else
                Throw New Exception($"Unexpected node{node.tok}")
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Function rewriteExpression(node As BoundExpression)
        Select Case node.tok
            Case BoundNodeToken.BNT_UINEX
                Return rewriteUniaryExpression(node)
            Case BoundNodeToken.BNT_LITEX
                Return rewriteLiteralExpression(node)
            Case BoundNodeToken.BNT_VAREX
                Return rewriteVariableExpression(node)
            Case BoundNodeToken.BNT_ASNEX
                Return rewriteAssignmentExpression(node)
            Case BoundNodeToken.BNT_BINEX
                Return rewriteBinaryExpression(node)
            Case Else
                Throw New Exception($"Unexpected node{node.tok}")
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteBinaryExpression(node As BoundBinExpression) As Object
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteAssignmentExpression(node As BoundAssignmentExpression) As Object
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteVariableExpression(node As BoundVariableExpression) As Object
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteLiteralExpression(node As BoundLiteralExp) As Object
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteUniaryExpression(node As BoundUniExpression) As Object
        Throw New NotImplementedException()
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ' /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////
    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundGoToConditinalStatement(node As BoundGoToConditionalStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundGoToStatement(node As BoundGoToStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundLabelStatement(node As BoundLabelStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundDoUntilStatement(node As BoundUntilStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundForStatement(node As BoundForStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundWhileStatement(node As BoundWhileStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteIfStatement(node As BoundIfStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteVariableDeclaration(node As BoundVariableDeclaration) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteExpressionStatement(node As BoundExpressionStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBlockStatement(boundBlockStatement As BoundBlockStatement) As BoundStatement
        Throw New NotImplementedException()
    End Function
End Class
