Public Class Evaluator
    Private ReadOnly ROOT As BoundBlockStatement
    Private variables As Dictionary(Of VariableSymbol, Object)
    Private lastValue As Object
    Private randnum As Random

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

    ' //////////////////////////////////////////////////////////////////////////
    Private Function evaluateConversionExpression(node As BoundConversionExpression) As Object
        Dim value As Object = EvaluatExpression(node._Expression)

        If node.Type_.Equals(TypeSymbol.Indicator) Then
            Return Convert.ToBoolean(value)
        End If

        If node.Type_.Equals(TypeSymbol.Integer_) Then
            Return Convert.ToInt32(value)
        End If

        If node.Type_.Equals(TypeSymbol.Char_) Then
            Return value.ToString()
        End If

        If node.Type_.Equals(TypeSymbol.Float) Then
            Return Convert.ToDouble(value)
        End If

        If node.Type_.Equals(TypeSymbol.Date_) Or node.Type_.Equals(TypeSymbol.DateTime) Then
            Return Convert.ToDateTime(value.ToString())
        End If

        If node.Type_.Equals(TypeSymbol.Time) Then
            Return Convert.ToDateTime($"1900-01-01 {value}")
        End If

        Throw New Exception($"Unexpected type {node.Type_}")
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
                Throw New Exception(String.Format("unrecognized uinary Operator ({0}) ", uiTmp.tok))
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Function evaluateBoundBinaryExpression(biTmp As BoundBinExpression) As Object
        Dim chameleonOBJL As Object
        Dim chameleonOBJR As Object
        Dim ans As Object

        chameleonOBJL = EvaluatExpression(biTmp.Left)
        chameleonOBJR = EvaluatExpression(biTmp.Right)

        Select Case biTmp.OP.tok
            Case BoundBinOpToken.BBO_ADD
                If biTmp.typ.Equals(TypeSymbol.Char_) Or biTmp.typ.Equals(TypeSymbol.Varchar) Then
                    ans = chameleonOBJL.ToString() + chameleonOBJR.ToString()
                Else
                    ans = Convert.ToInt32(chameleonOBJL) + Convert.ToInt32(chameleonOBJR)
                End If
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
                ans = Not (Equals(chameleonOBJL, chameleonOBJR))
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

    ' //////////////////////////////////////////////////////////////////////////
    Private Function evaluateCallExpression(node As BoundCallExpression) As Object
        Dim msg As String
        Dim max As Integer
        Dim val0 As String
        Dim val1 As String
        Dim val2 As String
        Dim val As Object

        If node.function_.Equals(BuiltinFunctions.cin) Then
            Return Console.ReadLine()
        ElseIf node.function_.Equals(BuiltinFunctions.cout) Then
            msg = EvaluatExpression((node.args(0))).ToString()
            Console.WriteLine(msg)
            Return Nothing

        ElseIf node.function_.Equals(BuiltinFunctions.dsply) Then
            msg = EvaluatExpression((node.args(0))).ToString()
            Console.WriteLine(msg)
            Return Nothing



        ElseIf node.function_.equals(BuiltinFunctions.BIF_Int) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Try
                Return Convert.ToInt32(val)
            Catch ex As Exception
                Throw New Exception($"Input string ‘{Val()}’ was not in a correct format")
            End Try
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_char) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return val.ToString()
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Log) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return CInt(Math.Log(Convert.ToDouble(val)))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Log10) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return CInt(Math.Log(Convert.ToDouble(val)) / Math.Log(10.0))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_abs) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return CInt(Math.Abs(Convert.ToDouble(val)))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Sqrt) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return CInt(Math.Sqrt(Convert.ToDouble(val)))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_rem) Then
            val0 = EvaluatExpression((node.args(0))).ToString()
            val1 = EvaluatExpression((node.args(1))).ToString()
            Return CInt((Convert.ToDouble(val0) Mod Convert.ToDouble(val1)))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Len) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return Val.Length
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Lower) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return Val.ToLower()
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Upper) Then
            val = EvaluatExpression((node.args(0))).ToString()
            Return val.ToUpper()
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Subst) Then
            val0 = EvaluatExpression((node.args(0))).ToString()
            val1 = EvaluatExpression((node.args(1))).ToString()
            val2 = EvaluatExpression((node.args(2))).ToString()
            Return val0.Substring(Convert.ToInt32(val1), Convert.ToInt32(val2))
        ElseIf node.function_.Equals(BuiltinFunctions.BIF_Rand) Then
            val0 = EvaluatExpression((node.args(0))).ToString()

            If randnum Is Nothing Then
                randnum = New Random()

                max = Convert.ToInt32(val0)
                Return randnum.Next(max)
            Else
                Throw New Exception($"unknown function ‘{node.function_.Name}’")
            End If
        End If
    End Function

    ' //////////////////////////////////////////////////////////////
    Private Sub evaluateVariableDaclaration(node As BoundVariableDeclaration)
        Dim value As Object

        value = EvaluatExpression(node.Initalizer)
        variables(node.Variable) = value
        lastValue = value
    End Sub

    ' //////////////////////////////////////////////////////////////
    Private Sub evaluateExpressionStatement(stmnt As BoundExpressionStatement)
        lastValue = EvaluatExpression(stmnt.Expression)
    End Sub

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
    Public Function Evaluate() As Object

        Dim lableToIndex As Dictionary(Of LabelSymbol, Integer)
        Dim l As BoundLabelStatement
        Dim s As BoundStatement
        Dim cgts As BoundGoToConditionalStatement
        Dim gs As BoundGoToStatement
        Dim index As Integer
        Dim cond As Boolean
        Dim lblName As String

        lableToIndex = New Dictionary(Of LabelSymbol, Integer)

        For i As Integer = 0 To (ROOT.Statements.Length - 1)
            If ROOT.Statements(i) Is GetType(BoundLabelStatement) Then
                l = ROOT.Statements(i)
                lableToIndex.Add(l.Label, i + 1)
            End If
        Next
        index = 0

        While index < ROOT.Statements.Length
            s = ROOT.Statements(index)

            Select Case s.tok
                Case BoundNodeToken.BNT_EXPRSTMT
                    evaluateExpressionStatement(s)
                Case BoundNodeToken.BNT_VARDECLR
                    evaluateVariableDaclaration(s)
                Case BoundNodeToken.BNT_GOTOCOND
                    cgts = s
                    cond = EvaluatExpression(cgts.Condition)
                    If ((cond = True And cgts.JumpWhenFalse = False) Or (cond = False And cgts.JumpWhenFalse = True)) Then
                        lblName = cgts.Label.Name
                        index = lableToIndex(cgts.Label)
                        Continue While
                    End If
                Case BoundNodeToken.BNT_GOTO
                    gs = s
                    lblName = gs.Label.Name
                    index = lableToIndex(gs.Label)
                Case BoundNodeToken.BNT_LABEL
                Case Else
                    Throw New Exception(String.Format("unexpected token {0}", s.tok))
            End Select

            index += 1
        End While

        Return lastValue
    End Function
End Class
