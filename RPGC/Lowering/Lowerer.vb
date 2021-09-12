Imports System.Linq
Imports System.Collections.Immutable
Imports System.Collections.Generic
Imports RPGC.Syntax

Public Class Lowerer
    Inherits BoundTreeRewriter

    Private _labelCount As Integer

    Private Sub New()
        '
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Private Function generateLable() As LabelSymbol
        Dim lbl As String

        lbl = $"^Lable{_labelCount}"
        _labelCount += 1

        Return New LabelSymbol(lbl)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function Lower(Statement As BoundStatement) As BoundBlockStatement
        Dim lorr As Lowerer
        Dim res As BoundStatement
        Dim results As BoundBlockStatement

        lorr = New Lowerer()
        res = lorr.rewriteStatement(Statement)
        results = flatten(res)

        Return results
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function flatten(stmnt As BoundStatement) As BoundBlockStatement
        Dim builder As ImmutableArray(Of BoundStatement).Builder
        Dim stack As Stack(Of BoundStatement)
        Dim current As BoundStatement
        Dim blocks As BoundBlockStatement

        builder = ImmutableArray.CreateBuilder(Of BoundStatement)
        stack = New Stack(Of BoundStatement)()
        stack.Push(stmnt)

        While stack.Count > 0
            current = stack.Pop()

            If GetType(BoundBlockStatement).Equals(current) Then
                blocks = current

                For Each s As BoundStatement In blocks.Statements.Reverse
                    stack.Push(s)
                Next
            Else
                builder.Add(current)
            End If
        End While

        Return New BoundBlockStatement(builder.ToImmutable())
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Function rewriteIfStatement(node As BoundIfStatement) As BoundStatement

        Dim endLable, elseLable As LabelSymbol
        Dim gotoEndStatement As BoundGoToStatement
        Dim result As BoundBlockStatement
        Dim endLableStatement, elseLableStatement As BoundLabelStatement
        Dim gotoWhenFalse As BoundGoToConditionalStatement

        If node.ElseStatement Is Nothing Then
            ' if <condition> ;
            '     <then Statement>
            ' endif;
            '                  * 
            ' gotoWhenfalse <condition> end
            ' <then Statement>
            ' end:
            endLable = generateLable()
            gotoWhenFalse = New BoundGoToConditionalStatement(endLable, node.Condition, True)
            endLableStatement = New BoundLabelStatement(endLable)
            result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
                    gotoWhenFalse,
                    node.ThenStatement,
                    endLableStatement))

            Return rewriteStatement(result)
        Else
            ' if <condition> ;
            '     <then Statement>
            ' else;
            '     <else Statement>
            ' endif;
            '                  * 
            ' gotoWhenfalse <condition> else
            ' <then Statement>
            ' goto end
            ' else:
            ' <else Statement>
            ' end:
            endLable = generateLable()
            elseLable = generateLable()

            gotoWhenFalse = New BoundGoToConditionalStatement(elseLable, node.Condition, True)
            gotoEndStatement = New BoundGoToStatement(endLable)
            elseLableStatement = New BoundLabelStatement(elseLable)
            endLableStatement = New BoundLabelStatement(endLable)
            result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
                    gotoWhenFalse,
                    node.ThenStatement,
                    gotoEndStatement,
                    elseLableStatement,
                    node.ElseStatement,
                    endLableStatement
                ))

            Return rewriteStatement(result)
        End If
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Function rewriteBoundForStatement(node As BoundForStatement) As BoundStatement
        ' treet for loop as a while loop
        ' for <variable> = 0 to <limit>;
        '     <loop statement>
        ' endfor;
        '              * 
        ' dow <condition> ;
        '     <loop statement>
        '     variable += inc
        ' enddo;
        Dim varDeclareation As BoundStatement
        Dim condition As BoundBinExpression
        Dim inc As BoundExpressionStatement
        Dim variableExpression As BoundVariableExpression
        Dim whileBlock As BoundBlockStatement
        Dim result As BoundBlockStatement
        Dim whileStatement As BoundWhileStatement

        varDeclareation = New BoundVariableDeclaration(node.Variable, node.LBound)
        variableExpression = New BoundVariableExpression(node.Variable)
        condition = New BoundBinExpression(variableExpression,
                                               BoundBinOperator.bind(TokenKind.TK_LE, GetType(Integer), GetType(Integer)),
                                               node.Ubound)
        inc = New BoundExpressionStatement(
                New BoundAssignmentExpression(
                    node.Variable,
                    New BoundBinExpression(
                            variableExpression,
                                BoundBinOperator.bind(TokenKind.TK_ADD, GetType(Integer), GetType(Integer)),
                                New BoundLiteralExp(1)
                            )
                    )
            )
        whileBlock = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(node.Body, inc))
        whileStatement = New BoundWhileStatement(condition, whileBlock)

        result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(varDeclareation, whileStatement))

        Return rewriteStatement(result)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Function rewriteBoundWhileStatement(node As BoundWhileStatement) As BoundStatement
        ' dow <condition> ;
        '     <loop statement>
        '     variable += inc
        ' enddo;
        ' 
        ' goto checkLable
        ' continueLable:
        ' <body>
        ' checkLable:
        ' gotoTrue <condition> continueLable
        ' endLable:

        Dim endLable, checkLable, continueLable As LabelSymbol
        Dim gotoCheck As BoundGoToStatement
        Dim continueLableStatement As BoundLabelStatement
        Dim checkLableStatement As BoundLabelStatement
        Dim gotoTrue As BoundGoToConditionalStatement
        Dim endLableStatement As BoundLabelStatement
        Dim result As BoundBlockStatement

        continueLable = generateLable()
        checkLable = generateLable()
        endLable = generateLable()

        gotoCheck = New BoundGoToStatement(checkLable)
        continueLableStatement = New BoundLabelStatement(continueLable)
        checkLableStatement = New BoundLabelStatement(checkLable)
        gotoTrue = New BoundGoToConditionalStatement(continueLable, node.Condition, False)
        endLableStatement = New BoundLabelStatement(endLable)

        result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
                gotoCheck,
                continueLableStatement,
                node.Body,
                checkLableStatement,
                gotoTrue,
                endLableStatement
            ))

        Return rewriteStatement(result)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Function rewriteBoundDoUntilStatement(node As BoundUntilStatement) As BoundStatement
        ' dou <condition> ;
        '     <loop statement>
        '     variable += inc
        ' enddo;
        ' 
        ' goto checkLable
        ' continueLable:
        ' <body>
        ' checkLable:
        ' gotoTrue <condition> continueLable
        ' endLable:

        Dim endLable, checkLable, continueLable As LabelSymbol
        Dim gotoCheck As BoundGoToStatement
        Dim continueLableStatement As BoundLabelStatement
        Dim checkLableStatement As BoundLabelStatement
        Dim gotoTrue As BoundGoToConditionalStatement
        Dim endLableStatement As BoundLabelStatement
        Dim result As BoundBlockStatement

        continueLable = generateLable()
        checkLable = generateLable()
        endLable = generateLable()

        gotoCheck = New BoundGoToStatement(checkLable)
        continueLableStatement = New BoundLabelStatement(continueLable)
        checkLableStatement = New BoundLabelStatement(checkLable)
        gotoTrue = New BoundGoToConditionalStatement(continueLable, node.Condition, True)
        endLableStatement = New BoundLabelStatement(endLable)

        result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
                gotoCheck,
                continueLableStatement,
                node.Body,
                checkLableStatement,
                gotoTrue,
                endLableStatement
            ))

        Return rewriteStatement(result)
    End Function
End Class
