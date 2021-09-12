Imports System.Collections.Immutable

Public Class Binder
    Public diagnostics As DiagnosticBag = New DiagnosticBag()
    Public scope As BoundScope

    Public Sub New(parant As BoundScope)
        scope = New BoundScope(parant)
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Shared Function BindGlobalScope(prev As BoundGlobalScope, syntax As CompilationUnit) As BoundGlobalScope
        Dim parantScop As BoundScope
        Dim bind As Binder
        Dim stmt As BoundStatement
        Dim vars As ImmutableArray(Of VariableSymbol)
        Dim diag As ImmutableArray(Of Diagnostics)

        parantScop = createParantScope(prev)
        bind = New Binder(parantScop)
        stmt = bind.BindStatements(syntax.Statement)
        vars = bind.scope.getDeclaredVariables()
        diag = bind.diagnostics.ToImmutableArray()

        If (prev Is Nothing) = False Then
            diag = diag.InsertRange(0, prev.Diagnostic)
        End If

        Return New BoundGlobalScope(prev, diag, vars, stmt)
    End Function


    ' ///////////////////////////////////////////////////////////////////////////////
    Private Shared Function createParantScope(prev As BoundGlobalScope) As BoundScope
        Dim stk As Stack(Of BoundGlobalScope) = New Stack(Of BoundGlobalScope)()
        Dim parant As BoundScope = Nothing
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
    Private Shared Function createRootScope() As BoundScope
        Dim Res As BoundScope

        Res = New BoundScope(Nothing)

        ' declare built in functions
        For Each fnc As FunctionSymbol In BuiltinFunctions.getAll()
            Res.declareFunciton(fnc)
        Next

        ' declare builtin variables
        For Each v As String In SyntaxFacts.getAllIndicators()
            Res.declareVar(New VariableSymbol($"*IN{v}", False, TypeSymbol.Indicator))
        Next

        Return Res
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindStatements(syntax As StatementSyntax) As BoundStatement
        Select Case syntax.kind
            Case TokenKind.TK_BLOCKSYNTX
                Return BindBlockStatement(syntax)
            Case TokenKind.TK_EXPRNSTMNT
                Return BindExpressionStatement(syntax)
            Case TokenKind.TK_VARDECLR
                Return BindVariableDeclaration(syntax)
            Case TokenKind.TK_IF
                Return BindIfStatement(syntax)
            Case TokenKind.TK_DOW
                Return BindWhileStatement(syntax)
            Case TokenKind.TK_DOU
                Return BindUntilStatement(syntax)
            Case TokenKind.TK_FOR
                Return BindForStatement(syntax)
            Case TokenKind.TK_GOTO
                Return BindGoToStatement(syntax)
            Case TokenKind.TK_TAG
                Return BindTagStatement(syntax)
            Case Else
                Throw New Exception(String.Format("Unexpected Syntax {0}", syntax.kind))
        End Select
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////
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
            Case TokenKind.TK_CALL,
                 TokenKind.TK_CALLP,
                 TokenKind.TK_CALLB
                Return BindCallExpression(syntax)
            Case Else
                Throw New Exception(String.Format("Unexpected Syntax {0}", syntax.kind))
        End Select
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression
        Throw New NotImplementedException()
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Public Function BindExpression(syntax As ExpresionSyntax, expectedResult As TypeSymbol) As BoundExpression
        Dim result As BoundExpression

        result = BindExpression(syntax)

        If result.typ.Equals(expectedResult) = False Then
            diagnostics.reportCannotConvert(syntax.Span, result.typ, expectedResult)
        End If

        Return result
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundIfStatement
        Dim condition As BoundExpression
        Dim thenStatement As BoundStatement
        Dim elseStatement As BoundStatement

        ' set condition and then statments
        condition = BindExpression(syntax.Condition, TypeSymbol.Indicator)
        thenStatement = BindStatements(syntax.ThenStatement)

        ' set else statement
        If syntax.ElseBlock Is Nothing = False Then
            elseStatement = BindStatements(syntax.ElseBlock.ElseStatement)
        Else
            elseStatement = Nothing
        End If

        Return New BoundIfStatement(condition, thenStatement, elseStatement)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
        Dim name As String
        Dim isReadOnly As Boolean
        Dim expression As BoundExpression
        Dim variable As VariableSymbol

        name = syntax.Identifier.sym.ToString()
        isReadOnly = (syntax.kind = TokenKind.TK_VARDCONST)
        expression = BindExpression(syntax.Initilizer)
        variable = New VariableSymbol(name, isReadOnly, expression.typ)

        If scope.declareVar(variable) = False Then
            diagnostics.reportVariableAlreadyDeclared(syntax.Span, name)
        End If

        Return New BoundVariableDeclaration(variable, expression)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindBlockStatement(syntax As BlockStatementSyntax) As BoundStatement
        Dim statements As ImmutableArray(Of BoundStatement).Builder
        Dim statement As BoundStatement

        scope = New BoundScope(scope)
        statements = ImmutableArray.CreateBuilder(Of BoundStatement)()

        For Each syntaxStatement As StatementSyntax In syntax.Statements
            statement = BindStatements(syntaxStatement)
            statements.Add(statement)
        Next

        scope = scope.Parant

        Return New BoundBlockStatement(statements.ToImmutable())
    End Function

    ' ///////////////////////////////////////////////////////////////////////////////
    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
        Dim expression As BoundExpression

        expression = BindExpression(syntax.Expression)

        Return New BoundExpressionStatement(expression)
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
        Dim _var As VariableSymbol = Nothing

        name = syntax.IDENTIFIERTOKEN.sym.ToString()
        boundExp = BindExpression(syntax.EXPRESSION)

        If scope.lookupVar(name, _var) = False Then
            diagnostics.reportUndefinedName(syntax.IDENTIFIERTOKEN.Span, name)
            Return boundExp
        End If

        If _var.IsReadonly_ = True Then
            diagnostics.reportAssignmentOfConstantVar(syntax.IDENTIFIERTOKEN.Span, name)
        End If

        If boundExp.typ.Equals(_var.Type_) = False Then
            diagnostics.reportVariableAlreadyDeclared(syntax.IDENTIFIERTOKEN.Span, name)
            'diagnostics.reportCannotConvertType(syntax.EXPRESSION.Span, name, boundExp.typ, _var._type)
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

    ' ////////////////////////////////////////////////////////////////////////////////////////
    Private Function BindTagStatement(syntax As TagStatementSyntax) As BoundStatement
        Dim lbl As LabelSymbol

        lbl = New LabelSymbol(syntax.LableName)

        Return New BoundLabelStatement(lbl)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////
    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement
        Dim condition As BoundExpression
        Dim body As BoundStatement

        condition = BindExpression(syntax.Condition, TypeSymbol.Indicator)
        body = BindStatements(syntax.Body)

        ' loop error no condition was given
        If condition Is Nothing Or condition.typ.Equals(TypeSymbol.Error_) = True Then
            diagnostics.reportLoopWithoutCondition(syntax.Span, syntax.Keyword.sym.ToString())
            Return New BoundErrorStatement()
        End If

        Return New BoundWhileStatement(condition, body)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////
    Private Function BindGoToStatement(syntax As GoToStatementSyntax) As BoundStatement
        Dim lbl As LabelSymbol

        lbl = New LabelSymbol(syntax.LableName)

        Return New BoundGoToStatement(lbl)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////
    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement
        Dim name As String = syntax.Identifier.sym.ToString()
        Dim LBound As BoundExpression
        Dim UBound As BoundExpression
        Dim countBy As BoundExpression
        Dim body As BoundStatement
        Dim variable As VariableSymbol = Nothing
        Dim isCountUp As Boolean

        LBound = BindExpression(syntax.LowerBound)
        UBound = BindExpression(syntax.UpperBound)
        countBy = BindExpression(syntax.Increment)

        isCountUp = (syntax.Keyword_By.kind = TokenKind.TK_TO)

        ' variable must be declared before the for loop Is used
        If scope.checkLocalVariables(name) = False Then
            diagnostics.reportVariableDoesNotExist(syntax.Identifier.Span, name)
            Return New BoundErrorStatement()
        Else
            variable = scope.variables(name)
        End If

        body = BindStatements(syntax.Body)

        Return New BoundForStatement(variable, LBound, UBound, body, isCountUp, countBy)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////////
    Private Function BindUntilStatement(syntax As UntilStatementSyntax) As BoundStatement
        Dim condition As BoundExpression
        Dim body As BoundStatement

        condition = BindExpression(syntax.Condition, TypeSymbol.Indicator)
        body = BindStatements(syntax.Body)

        ' loop error no condition was given
        If condition Is Nothing Or condition.typ.Equals(TypeSymbol.Error_) = True Then
            diagnostics.reportLoopWithoutCondition(syntax.Span, syntax.Keyword.sym.ToString())
            Return New BoundErrorStatement()
        End If

        Return New BoundUntilStatement(condition, body)
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
    BNT_EXPRSTMT
    BNT_BLOCKSTMT
    BNT_VARDECLR
    BNT_IFSTMT
    BNT_GOTOCOND
    BNT_WHILESTMT
    BNT_FORSTMT
    BNT_DOUNTIL
    BNT_LABEL
    BNT_GOTO
    BNT_CALLEXP
    BNT_ERROREXP
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