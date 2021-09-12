Imports System.Collections.Immutable

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
    Public Function rewriteExpression(node As BoundExpression) As BoundExpression
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
    Private Function rewriteBinaryExpression(node As BoundBinExpression) As BoundExpression
        Dim left, right As BoundExpression

        left = rewriteExpression(node.Left)
        right = rewriteExpression(node.Right)

        If left.Equals(node.Left) And right.Equals(node.Right) Then
            Return node
        End If

        Return New BoundBinExpression(left, node.OP, right)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteAssignmentExpression(node As BoundAssignmentExpression) As BoundExpression
        Dim expression As BoundExpression

        expression = rewriteExpression(node.expression)
        If expression.Equals(node.expression) Then
            Return node
        End If

        Return New BoundAssignmentExpression(node.Variable, expression)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteVariableExpression(node As BoundVariableExpression) As BoundExpression
        Return node
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteLiteralExpression(node As BoundLiteralExp) As BoundExpression
        Return node
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Private Function rewriteUniaryExpression(node As BoundUniExpression) As BoundExpression
        Dim operand As BoundExpression

        operand = rewriteExpression(node.right)
        If operand.Equals(node.right) Then
            Return node
        End If

        Return New BoundUniExpression(node.OP, operand)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    ' /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////     /////
    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundGoToConditinalStatement(node As BoundGoToConditionalStatement) As BoundStatement
        Dim condition As BoundExpression
        condition = rewriteExpression(node.Condition)

        If condition.Equals(node) Then
            Return node
        End If

        Return New BoundGoToConditionalStatement(node.Label, condition, node.JumpWhenFalse)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundGoToStatement(node As BoundGoToStatement) As BoundStatement
        Return node
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundLabelStatement(node As BoundLabelStatement) As BoundStatement
        Return node
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundDoUntilStatement(node As BoundUntilStatement) As BoundStatement
        Dim condition As BoundExpression
        Dim body As BoundStatement

        condition = rewriteExpression(node.Condition)
        body = rewriteStatement(node.Body)

        If condition.Equals(node.Condition) And body.Equals(node.Body) Then
            Return node
        End If

        Return New BoundUntilStatement(condition, body)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundForStatement(node As BoundForStatement) As BoundStatement
        Dim lowerBound As BoundExpression
        Dim uperBound As BoundExpression
        Dim body As BoundStatement

        lowerBound = rewriteExpression(node.LBound)
        uperBound = rewriteExpression(node.Ubound)
        body = rewriteStatement(node.Body)

        If lowerBound.Equals(node.LBound) And uperBound.Equals(node.Ubound) And body.Equals(node.Body) Then
            Return node
        End If

        Return New BoundForStatement(node.Variable, lowerBound, uperBound, body, node.IsCountUP)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBoundWhileStatement(node As BoundWhileStatement) As BoundStatement
        Dim condition As BoundExpression
        Dim body As BoundStatement

        condition = rewriteExpression(node.Condition)
        body = rewriteStatement(node.Body)

        If condition.Equals(node.Condition) And body.Equals(node.Body) Then
            Return node
        End If

        Return New BoundWhileStatement(condition, body)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteIfStatement(node As BoundIfStatement) As BoundStatement
        Dim condition As BoundExpression
        Dim trueStatementBody As BoundStatement
        Dim elseStaetmentBody As BoundStatement

        condition = rewriteExpression(node.Condition)
        trueStatementBody = rewriteStatement(node.ThenStatement)
        elseStaetmentBody = Nothing

        If node.ElseStatement Is Nothing Then
            elseStaetmentBody = rewriteStatement(node.ElseStatement)
        End If

        If condition.Equals(node.Condition) And trueStatementBody.Equals(node.ThenStatement) And elseStaetmentBody.Equals(node.ElseStatement) Then
            Return node
        End If

        Return New BoundIfStatement(condition, trueStatementBody, elseStaetmentBody)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteVariableDeclaration(node As BoundVariableDeclaration) As BoundStatement
        Dim initilizer As BoundExpression

        initilizer = rewriteExpression(node.Initalizer)

        If initilizer.Equals(node.Initalizer) Then
            Return node
        End If

        Return New BoundVariableDeclaration(node.Variable, initilizer)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteExpressionStatement(node As BoundExpressionStatement) As BoundStatement
        Dim expression As BoundExpression

        expression = rewriteExpression(node.Expression)

        If expression.Equals(node.Expression) Then
            Return node
        End If

        Return New BoundExpressionStatement(expression)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overridable Function rewriteBlockStatement(node As BoundBlockStatement) As BoundStatement
        Dim builder As ImmutableArray(Of BoundStatement).Builder = Nothing
        Dim oldStatement As BoundStatement
        Dim newStatement As BoundStatement

        For i As Integer = 0 To (node.Statements.Length - 1)
            oldStatement = node.Statements(i)
            newStatement = rewriteStatement(oldStatement)

            If newStatement.Equals(oldStatement) = False Then
                If builder Is Nothing Then
                    builder = ImmutableArray.CreateBuilder(Of BoundStatement)(node.Statements.Length)

                    For u As Integer = 0 To (i - 1)
                        builder.Add(node.Statements(u))
                    Next
                End If
            End If

            If builder Is Nothing = False Then
                builder.Add(newStatement)
            End If
        Next

        If builder Is Nothing = False Then
            Return node
        End If

        Return New BoundBlockStatement(builder.MoveToImmutable())
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Protected Overridable Function rewriteCallExression(node As BoundCallExpression)
        Dim builder As ImmutableArray(Of BoundExpression).Builder = Nothing
        Dim oldArgument As BoundExpression
        Dim newArgument As BoundExpression

        For i As Integer = 0 To (node.args.Length - 1)

            oldArgument = node.args(i)
            newArgument = rewriteExpression(oldArgument)

            If newArgument <> oldArgument Then
                If builder Is Nothing Then
                    builder = ImmutableArray.CreateBuilder < BoundExpression > (node.args.Length)

                    For u As Integer = 0 To (i - 1)
                        builder.Add(node.args[u])
                    End If
            End If

            If (builder Is Nothing) = False Then
                builder.Add(newArgument)
            End If
        Next

                        If builder Is Nothing Then
                        Return node
                    End If

                    Return New BoundCallExpression(node.funciton, builder.MoveToImmutable())
        End Function
End Class
