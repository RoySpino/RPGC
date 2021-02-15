Imports RPGC.UniaryExpressionSyntax


Public Class Binder
    Public diagnostics As DiagnosticBag = New DiagnosticBag()
    Public variables As Dictionary(Of VariableSymbol, Object)

    Public Sub New(_var As Dictionary(Of VariableSymbol, Object))
        variables = _var
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Function BindExpression(syntax As ExpresionSyntax) As BoundExpression
        Select Case syntax.kind
            Case TokenKind.TK_PARENEXP
                Return BindParenthesizedExpression(syntax)
            Case TokenKind.TK_LITEXPR
                Return BindLiteralExp(syntax)
            Case TokenKind.TK_UNIEXP
                Return BindUniExpression(syntax)
            Case TokenKind.TK_BYNARYEXPR
                Return BindBinExpression(syntax)
            Case TokenKind.TK_NAMEDEXP
                Return BindNamedExpression(syntax)
            Case TokenKind.TK_ASSIGN
                Return BindAssignmentExpression(syntax)
            Case Else
                Throw New Exception(String.Format("Unexpected Syntax {0}", syntax.kind))
        End Select
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindParenthesizedExpression(syntax As ParenthesizedExpression)
        Return BindExpression(syntax.expression)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindNamedExpression(syntax As NamedExpressionSyntax)
        Dim name As String
        Dim _variable As VariableSymbol

        name = syntax.IDENTIFIERTOKEN.sym.ToString()
        _variable = variables.Keys.FirstOrDefault(
            Function(v)
                Return v.name = name
            End Function)

        If _variable Is Nothing Then
            diagnostics.reportUndefinedName(syntax.IDENTIFIERTOKEN.Span, name)
            Return New BoundLiteralExp(0)
        End If

        Return New BoundVariableExpression(_variable)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

        Dim name As String
        Dim boundExp As BoundExpression
        Dim _var, existingVar As VariableSymbol

        name = syntax.IDENTIFIERTOKEN.sym.ToString()
        boundExp = BindExpression(syntax.EXPRESSION)
        existingVar = variables.Keys.FirstOrDefault(
            Function(v) 'v >= v.Name == name
                Return v.name = name
            End Function)

        If existingVar Is Nothing = False Then
            variables.Remove(existingVar)
        End If

        _var = New VariableSymbol(name, boundExp.typ)
        variables(_var) = Nothing

        Return New BoundAssignmentExpression(_var, boundExp)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindLiteralExp(syntax As LiteralExpressionSyntax) As BoundExpression
        Dim value As Object
        value = IIf(syntax.value Is Nothing, 0, syntax.value)
        Return New BoundLiteralExp(value)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindUniExpression(syntax As UinaryExpressionSyntax) As BoundExpression
        Dim expression As BoundExpression
        Dim boundOperatorKind As BoundUniOperator

        expression = BindExpression(syntax.right)
        boundOperatorKind = BoundUniOperator.bind(syntax.operand.kind, expression.typ)

        ' account for errors
        If boundOperatorKind Is Nothing Then
            diagnostics.reportUndefinedUniaryOp(syntax.Operand.span, syntax.Operand.sym.ToString(), expression.type);
                Return expression
        End If

        Return New BoundUniExpression(boundOperatorKind, expression)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindBinExpression(syntax As BinaryExpressionSyntax) As BoundExpression

        Dim Left, Right As BoundExpression
        Dim boundOperatorKind As BoundBinOperator

        Left = BindExpression(syntax.left)
        Right = BindExpression(syntax.right)
        boundOperatorKind = BoundBinOperator.bind(syntax.operatorToken.kind, Left.typ, Right.typ)

        ' account for errors
        If boundOperatorKind Is Nothing Then
            diagnostics.reportUndefinedBynaryOp(syntax.operatorToken.span, syntax.operatorToken.sym.ToString(), Left.typ, Right.typ)
            Return Left
        End If

        Return New BoundBinExpression(Left, boundOperatorKind, Right)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Function getDiagnostics() As DiagnosticBag
        Return diagnostics
    End Function
End Class


Public Enum BoundNodeToken
    BNT_UINEX
    BNT_LITEX
    BNT_VAREX
    BNT_ASNEX
    BNT_BINEX
End Enum
Public Enum BoundUniOpToken
    BUO_IDENTITY
    BUO_NEGATION
    BUO_NOT
End Enum
Public Enum BoundBinOpToken
    BBO_ADD
    BBO_SUB
    BBO_MULT
    BBO_DIV

    BBO_AND
    BBO_NOT
    BBO_OR
    BBO_GE
    BBO_GT
    BBO_LE
    BBO_LT
    BBO_EQ
    BBO_NE
End Enum