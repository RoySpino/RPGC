Imports System.Collections.Immutable

Public Class Parser
    Private lex As Lexer = Nothing
    Private tok As SyntaxToken
    Private current As SyntaxToken
    Private pos As Integer
    Private tcount As Integer
    Private diagnostics As DiagnosticBag = New DiagnosticBag()
    Public ReadOnly tokens As ImmutableArray(Of SyntaxToken)
    Public ReadOnly source As SourceText

    Public Sub New(txt As SourceText)
        Dim _tokens As List(Of SyntaxToken) = New List(Of SyntaxToken)()

        lex = New Lexer(txt)
        pos = 0
        source = txt

        ' get token
        While True
            tok = lex.doLex()

            ' get token
            If tok.kind = TokenKind.TK_BADTOKEN Then
                _tokens.Add(tok)
                Exit While
            End If

            ' exit on error
            If tok Is Nothing = False And tok.kind <> TokenKind.TK_SPACE Then
                _tokens.Add(tok)
            End If

            ' save avalable tokens
            If tok.kind = TokenKind.TK_EOI Then
                Exit While
            End If
        End While

        tokens = _tokens.ToImmutableArray()
        diagnostics.AddRange(lex.getDiagnostics())
        tcount = _tokens.Count

        If tokens.Count > 0 Then
            current = tokens(0)
        End If
    End Sub

    ' ///////////////////////////////////////////////////////////////////////
    Private Function nextToken() As SyntaxToken
        Dim ret As SyntaxToken

        If pos >= tcount Then
            Return Nothing
        Else
            ret = tokens(pos)
            pos += IIf(((pos + 1) >= tcount), -1, 1)
            current = tokens(pos)
            Return ret
        End If

    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function peek(offset As Integer) As SyntaxToken
        Dim index As Integer

        index = pos + offset

        If index >= tcount Then
            Return tokens(tcount - 1)
        Else
            Return tokens(index)
        End If
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function match(tok As TokenKind)
        If current.kind = tok Then
            Return nextToken()
        End If

        ' report error
        diagnostics.reportUnexpectedToken(current.Span, current.kind, tok)
        Return New SyntaxToken(current.kind, current.line, current.pos, current.sym)
    End Function
    ' ///////////////////////////////////////////////////////////////////////

    Private Function parseBinaryExpresion(Optional parentPrecedence As Integer = 0) As ExpresionSyntax
        Dim precedence, uniPrecedence As Integer
        Dim left, right, operand As ExpresionSyntax
        Dim operatorToken As SyntaxToken

        uniPrecedence = SyntaxFacts.getUinaryOporatorPrecedence(current.kind)

        ' account for nagative(-) symbol before numbers
        If uniPrecedence > parentPrecedence Then
            operatorToken = nextToken()
            operand = parsePrimaryExpresion()
            left = New UniaryExpressionSyntax(operatorToken, operand)
        Else
            left = parsePrimaryExpresion()
        End If

        ' regulare numerical expressions
        While True
            precedence = SyntaxFacts.getBinaryOporatorPrecedence(current.kind)
            If precedence = 0 Or precedence <= parentPrecedence Then
                Exit While
            End If


            operatorToken = nextToken()
            right = parseBinaryExpresion(precedence)
            left = New BinaryExpressionSyntax(left, operatorToken, right)
        End While

        Return left
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parceExpression() As ExpresionSyntax
        Return parceAssignmentExpression()
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parceAssignmentExpression() As ExpresionSyntax

        Dim right As ExpresionSyntax
        Dim identifierToken, operatorToken As SyntaxToken

        If peek(0).kind = TokenKind.TK_BADTOKEN Then
            Return Nothing
        End If

        If peek(0).kind = TokenKind.TK_IDENTIFIER And peek(1).kind = TokenKind.TK_ASSIGN Then
            identifierToken = nextToken()
            operatorToken = nextToken()
            right = parceAssignmentExpression()
            Return New AssignmentExpressionSyntax(identifierToken, operatorToken, right)
        End If

        Return parseBinaryExpresion()
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parsePrimaryExpresion() As ExpresionSyntax
        Select Case current.kind
            Case TokenKind.TK_PARENOPEN
                Return parseTokenParethesizedExpression()
            Case TokenKind.TK_PARENOPEN,
                 TokenKind.TK_INDOFF
                Return parseTokenBoolenLiteral()
            Case TokenKind.TK_INTEGER,
                 TokenKind.TK_ZONED,
                  TokenKind.TK_PACKED
                Return parseNumberLiteral()
            Case TokenKind.TK_IDENTIFIER
                Return parseTokenNamedExpression()
            Case Else
                Return parseTokenNamedExpression()
        End Select
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parseTokenNamedExpression() As ExpresionSyntax
        Dim identifier As SyntaxToken

        identifier = match(TokenKind.TK_IDENTIFIER)
        Return New NamedExpressionSyntax(identifier)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parseTokenBoolenLiteral() As ExpresionSyntax

        Dim keyworkdTok As SyntaxToken
        Dim bolValue As Boolean

        bolValue = (current.kind = TokenKind.TK_INDON)

        keyworkdTok = IIf(bolValue = True, match(TokenKind.TK_INDON), match(TokenKind.TK_INDOFF))

        Return New LiteralExpressionSyntax(keyworkdTok, bolValue)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parseTokenParethesizedExpression() As ExpresionSyntax

        Dim Left, righ As SyntaxToken
        Dim exp As ExpresionSyntax

        Left = match(TokenKind.TK_PARENOPEN)
        exp = parseBinaryExpresion()
        righ = match(TokenKind.TK_PARENCLOSE)

        Return New ParenthesizedExpression(Left, exp, righ)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    Private Function parseNumberLiteral() As ExpresionSyntax
        Dim numberToken As SyntaxToken

        numberToken = match(TokenKind.TK_INTEGER)

        Return New LiteralExpressionSyntax(numberToken)
    End Function

    ' ///////////////////////////////////////////////////////////////////////
    'Public Function parse() As SyntaxTree
    '
    '    Dim ret As ExpresionSyntax
    '    Dim tmp As SyntaxToken
    '
    '    ret = parceExpression()
    '
    '    tmp = match(TokenKind.TK_EOI)
    '
    '    Return New SyntaxTree(source, diagnostics, ret, tmp)
    'End Function

    ' ///////////////////////////////////////////////////////////////////////
    Public Function parseCompilationUnit() As CompilationUnit

        Dim ret As ExpresionSyntax
        Dim tmp As SyntaxToken

        ret = parceExpression()

        tmp = match(TokenKind.TK_EOI)

        Return New CompilationUnit(ret, tmp)
    End Function


    ' ///////////////////////////////////////////////////////////////////////
    Public Function getDiagnostics() As DiagnosticBag
        Return diagnostics
    End Function
End Class
