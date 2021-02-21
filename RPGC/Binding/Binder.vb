Imports System.Collections.Immutable

Public Class Binder
    Public diagnostics As DiagnosticBag = New DiagnosticBag()
    Public scope As BoundScope

    Public Sub New(parant As BoundScope)
        scope = New BoundScope(parant)
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
    Public Shared Function bindGlobalScope(prev As BoundGlobalScope, syntax As CompilationUnit)
        Dim parantScop As BoundScope
        Dim bind As Binder
        Dim expression As BoundExpression
        Dim vars As ImmutableArray(Of VariableSymbol)
        Dim diag As ImmutableArray(Of Diagnostics)

        parantScop = createParantScope(prev)
        bind = New Binder(parantScop)
        expression = bind.BindExpression(syntax.Expression)
        vars = bind.scope.getDeclaredVariables()
        diag = bind.diagnostics.ToImmutableArray()

        If prev Is Nothing = False Then
            diag = diag.InsertRange(0, prev.Diagnostic)
        End If

        Return New BoundGlobalScope(prev, diag, vars, expression)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Shared Function createParantScope(prev As BoundGlobalScope) As BoundScope
        Dim stk As Stack(Of BoundGlobalScope) = New Stack(Of BoundGlobalScope)()
        Dim parant as BoundScope = Nothing
        Dim _scope As BoundScope

        While prev Is Nothing = False
            stk.Push(prev)
            prev = prev.Preveous
        End While

        While stk.Count > 0
            prev = stk.Pop()
            _scope = New BoundScope(parant)

            For Each v As VariableSymbol In prev.Variables
                _scope.declareVar(v)
            Next

            parant = _scope
        End While

        Return parant
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
        _variable = Nothing

        If scope.lookupVar(name, _variable) = False Then
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

        If scope.lookupVar(name, _var) = False Then
            _var = New VariableSymbol(name, boundExp.typ)
            scope.declareVar(_var)
        End If

        If boundExp.typ <> _var._type Then
            'diagnostics.reportVariableAlreadyDeclared(syntax.IDENTIFIERTOKEN.Span, name)
            diagnostics.reportCannotConvertType(syntax.EXPRESSION.Span, name, boundExp.typ, _var._type)
            Return boundExp
        End If

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
            diagnostics.reportUndefinedUniaryOp(syntax.operand.Span, syntax.operand.sym.ToString(), expression.typ)
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
            diagnostics.reportUndefinedBynaryOp(syntax.operatorToken.Span, syntax.operatorToken.sym.ToString(), Left.typ, Right.typ)
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