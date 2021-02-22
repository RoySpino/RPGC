Public Class Evaluator
    Private ReadOnly ROOT As BoundStatement
    Private variables As Dictionary(Of VariableSymbol, Object)
    Private lastValue As Object

    Public Sub New(rot As BoundStatement)
        ROOT = rot
    End Sub

    ' //////////////////////////////////////////////////////////////
    Public Sub New(rot As BoundStatement, var As Dictionary(Of VariableSymbol, Object))
        ROOT = rot
        variables = var
    End Sub

    ' //////////////////////////////////////////////////////////////
    Private Function DoComparison(operation As String, TL As Type, TR As Type, L As Object, R As Object) As Boolean
        Dim intL, intR As Integer
        Dim strL, strR As String
        Dim douL, douR As Double
        Dim datL, datR As DateTime

        If TL = GetType(Integer) Then
            intL = Convert.ToInt32(L)
            intR = Convert.ToInt32(R)

            Select Case operation
                Case ">"
                    Return intL > intR
                Case ">="
                    Return intL >= intR
                Case "<"
                    Return intL < intR
                Case Else
                    Return intL <= intR
            End Select
        Else
            If TL = GetType(Integer) Then
                strL = L.ToString()
                strR = R.ToString()

                Select Case operation
                    Case ">"
                        Return strL > strR
                    Case ">="
                        Return strL >= strR
                    Case "<"
                        Return strL < strR
                    Case Else
                        Return strL <= strR
                End Select
            Else
                If TL = GetType(Integer) Then
                    douL = Convert.ToDouble(L)
                    douR = Convert.ToDouble(R)

                    Select Case operation
                        Case ">"
                            Return douL > douR
                        Case ">="
                            Return douL >= douR
                        Case "<"
                            Return douL < douR
                        Case Else
                            Return douL <= douR
                    End Select
                Else
                    datL = Convert.ToDateTime(L)
                    datR = Convert.ToDateTime(R)

                    Select Case operation
                        Case ">"
                            Return datL > datR
                        Case ">="
                            Return datL >= datR
                        Case "<"
                            Return datL < datR
                        Case Else
                            Return datL <= datR
                    End Select
                End If
            End If
        End If
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function EvaluatStatement(node As BoundStatement) As Object
        Select Case node.tok
            Case BoundNodeToken.BNT_BLOCKSTMT
                evaluateBlockStatemnt(node)
            Case BoundNodeToken.BNT_EXPRSTMT
                evaluateExpressionStatement(node)
            Case Else
                Throw New Exception(String.Format("unexpected token {0}", ROOT.tok))
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Sub evaluateBlockStatemnt(node As BoundBlockStatement)
        For Each statement As BoundStatement In node.Statements
            EvaluatStatement(statement)
        Next
    End Sub

    ' //////////////////////////////////////////////////////////////
    Private Sub evaluateExpressionStatement(stmnt As BoundExpressionStatement)
        lastValue = EvaluatExpression(stmnt.Expression)
    End Sub

    ' //////////////////////////////////////////////////////////////
    Private Function EvaluatExpression(root As BoundExpression) As Object
        Select Case root.tok
            Case BoundNodeToken.BNT_ASNEX
                Return evaluateAssignmentExpression(root)
            Case BoundNodeToken.BNT_LITEX
                Return evaluateBoundLiteral(root)
            Case BoundNodeToken.BNT_UINEX
                Return evaluateBoundUniaryExpression(root)
            Case BoundNodeToken.BNT_BINEX
                Return evaluateBoundBinaryExpression(root)
            Case BoundNodeToken.BNT_VAREX
                Return evaluateVariableExpression(root)
            Case Else
                Throw New Exception(String.Format("unexpected token {0}", root.tok))
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function evaluateVariableExpression(vtmp As BoundVariableExpression) As Object
        Dim varb As Object
        varb = variables(vtmp.Variable)
        Return varb
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function evaluateAssignmentExpression(atmp As BoundAssignmentExpression) As Object
        Dim value As Object

        value = EvaluatExpression(atmp.expression)
        variables(atmp.Variable) = value

        Return value
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function evaluateBoundLiteral(bLit As BoundLiteralExp) As Object
        Return bLit.Value
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function evaluateBoundUniaryExpression(uiTmp As BoundUniExpression) As Object
        Dim value As Object
        Dim operand As Integer
        Dim chameleonOBJ As Object

        chameleonOBJ = EvaluatExpression(uiTmp.right)

        Select Case uiTmp.OP.tok
            Case BoundUniOpToken.BUO_IDENTITY
                operand = Convert.ToInt32(chameleonOBJ)
                value = operand
            Case BoundUniOpToken.BUO_NEGATION
                operand = Convert.ToInt32(chameleonOBJ)
                value = (-1 * operand)
            Case BoundUniOpToken.BUO_NOT
                value = Not Convert.ToBoolean(chameleonOBJ)
            Case Else
                Throw New Exception(String.Format("unrecognized uinary Operator [{0}] ", uiTmp.tok))
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Function evaluateBoundBinaryExpression(biTmp As BoundBinExpression) As Object
        Dim chameleonOBJL As Object
        Dim chameleonOBJR As Object
        Dim ans As Object

        chameleonOBJL = EvaluatExpression(biTmp.Left)
        chameleonOBJR = EvaluatExpression(biTmp.Right)

        Select Case biTmp.OP.tok
            Case BoundBinOpToken.BBO_ADD
                ans = Convert.ToInt32(chameleonOBJL) + Convert.ToInt32(chameleonOBJR)
            Case BoundBinOpToken.BBO_SUB
                ans = Convert.ToInt32(chameleonOBJL) - Convert.ToInt32(chameleonOBJR)
            Case BoundBinOpToken.BBO_MULT
                ans = Convert.ToInt32(chameleonOBJL) * Convert.ToInt32(chameleonOBJR)
            Case BoundBinOpToken.BBO_DIV
                ans = Convert.ToInt32(chameleonOBJL) / Convert.ToInt32(chameleonOBJR)
            Case BoundBinOpToken.BBO_AND
                ans = Convert.ToBoolean(chameleonOBJL) And Convert.ToBoolean(chameleonOBJR)
            Case BoundBinOpToken.BBO_OR
                ans = Convert.ToBoolean(chameleonOBJL) Or Convert.ToBoolean(chameleonOBJR)
            Case BoundBinOpToken.BBO_EQ
                ans = Equals(chameleonOBJL, chameleonOBJR)
            Case BoundBinOpToken.BBO_NE
                ans = Not Equals(chameleonOBJL, chameleonOBJR)
            Case BoundBinOpToken.BBO_GE
                ans = DoComparison(">=", chameleonOBJL.GetType(), chameleonOBJR.GetType(), chameleonOBJL, chameleonOBJR)
            Case BoundBinOpToken.BBO_GT
                ans = DoComparison(">", chameleonOBJL.GetType(), chameleonOBJR.GetType(), chameleonOBJL, chameleonOBJR)
            Case BoundBinOpToken.BBO_LE
                ans = DoComparison("<=", chameleonOBJL.GetType(), chameleonOBJR.GetType(), chameleonOBJL, chameleonOBJR)
            Case BoundBinOpToken.BBO_LT
                ans = DoComparison("<", chameleonOBJL.GetType(), chameleonOBJR.GetType(), chameleonOBJL, chameleonOBJR)
            Case Else
                Throw New Exception(String.Format("unexpected Binary operator {0}", biTmp.OP.tok))
        End Select

        Return ans
    End Function

    ' //////////////////////////////////////////////////////////////
    Public Function Evaluate() As Object
        EvaluatStatement(ROOT)
        Return lastValue
    End Function
End Class
