Public Class Lexer
    Private source As SourceText
    Private tmpVal As String
    Private pos As Integer
    Private linePos As Integer
    Private lineNum As Integer
    Private sSize As Integer
    Private peekPos As Integer
    Private curChar As String
    Private curSpec As String
    Private diagnostics As DiagnosticBag
    Private onEvalLine As Boolean
    Private doFreeLex As Boolean = True
    Private lineElem As List(Of StructNode)
    Private strucLexLine As List(Of SyntaxToken) = New List(Of SyntaxToken)()

    Private start As Integer
    Private kind As TokenKind
    Private value As Object

    Public Sub New(sour As SourceText)
        source = sour
        pos = -1
        linePos = 0
        curSpec = "H"
        lineNum = 1
        sSize = sour.Length()

        nextChar()
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function nextChar()
        pos += 1

        If pos >= sSize Then
            curChar = Chr(0)
        Else
            curChar = source.charAt(pos)

            If curChar = vbNewLine Then
                pos += 1
                lineNum += 1
                linePos = 0
                curChar = source.charAt(pos)
            End If

            linePos += 1
        End If
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function peek(offset As Integer) As String
        Dim index As Integer

        index = pos + offset

        If index >= sSize Then
            Return Chr(0)
        Else
            Return source.charAt(index)
        End If
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Sub swap(ByRef a As StructNode, ByRef b As StructNode)
        Dim tmp As StructNode

        tmp = a
        a = b
        b = tmp
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function decToInd(ind As String) As String
        Return "*IN" & ind
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function getAssignmentOrComparisonToken()
        Dim ret As TokenKind

        ' fist [=] is an assignment all others are comparisons
        ' reset on new line
        If onEvalLine = True Then
            ret = TokenKind.TK_ASSIGN
            onEvalLine = False
        Else
            ret = TokenKind.TK_EQ
        End If

        Return ret
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function getLessGreaterThanOperator(first As String, op As String) As TokenKind
        Dim ret As TokenKind

        If first = "<" Then
            Select Case op
                Case ">"
                    ret = TokenKind.TK_NE
                    tmpVal = ("" & first & op)
                    nextChar()
                Case "="
                    ret = TokenKind.TK_LE
                    tmpVal = ("" & first & op)
                    nextChar()
                Case Else
                    ret = TokenKind.TK_LT
                    tmpVal = ("" & first & op)
                    nextChar()
            End Select
        Else
            Select Case op
                Case "="
                    ret = TokenKind.TK_GE
                    tmpVal = ("" & first & op)
                    nextChar()
                Case Else
                    ret = TokenKind.TK_GT
                    tmpVal = ("" & first & op)
                    nextChar()
            End Select
        End If

        Return ret
    End Function
    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function chkOnEvalLine(symbol As String) As Boolean
        Return (symbol.Contains("EVAL") Or symbol = "IF" Or symbol = "FOR" Or
                    symbol = "WHEN" Or symbol = "CALLP" Or symbol = "DOW" Or symbol = "DOU")

    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Function doLex()
        Dim symbol As String = ""

        kind = TokenKind.TK_BADTOKEN

        value = Nothing

        If doFreeLex = False Then
            Return Nothing
        End If

        If pos = 0 And curChar = "*" And peek(1) = "*" Then
            start = pos
            While (Char.IsWhiteSpace(curChar) = False)
                symbol += curChar
                nextChar()
            End While

            symbol = symbol.ToUpper()
            If symbol <> "**FREE" Then
                'doStructLex()
                doFreeLex = False
            End If

            kind = TokenKind.TK_SPACE
            value = ""
        End If

        ' c++ style comment line
        value = peek(1)
        If curChar = "/" And peek(1) = "/" Then
            ignoreCommentLine()
        End If

        Select Case curChar
            Case Chr(0)
                start += 1
                symbol = "_"
                kind = TokenKind.TK_EOI
                value = "_"
            Case ";"
                start += 1
                symbol = ";"
                kind = TokenKind.TK_SEMI
                onEvalLine = True
                nextChar()
            Case "+"
                start += 1
                kind = TokenKind.TK_ADD
                value = "+"
                nextChar()
            Case "-"
                start += 1
                kind = TokenKind.TK_SUB
                value = "-"
                nextChar()
            Case "*"
                start += 1
                symbol = readCompilerConstantsOrMult()
            Case "/"
                start += 1
                kind = TokenKind.TK_DIV
                value = "/"
                nextChar()
            Case "("
                start += 1
                kind = TokenKind.TK_PARENOPEN
                value = "("
                nextChar()
            Case ")"
                start += 1
                kind = TokenKind.TK_PARENCLOSE
                value = ")"
                nextChar()
            Case "="
                start += 1
                kind = getAssignmentOrComparisonToken()
                value = "="
                nextChar()
            Case "<"
                start += 1
                kind = getLessGreaterThanOperator("<", peek(1))
                value = tmpVal
                nextChar()
            Case ">"
                start += 1
                kind = getLessGreaterThanOperator(">", peek(1))
                value = tmpVal
                nextChar()
            Case "%"
                symbol = readBuiltInFunctions()
            Case "@",
                 "#",
                 "$"
                symbol = readIdentifierOrKeyword()
            Case "0",
                 "1",
                 "2",
                 "3",
                 "4",
                 "5",
                 "6",
                 "7",
                 "8",
                 "9"
                symbol = readNumberToken()
            Case Chr(32),
                 Chr(10),
                 Chr(9),
                 Chr(13)
                readWiteSpace()
            Case Else
                If Char.IsLetter(curChar) = True Then
                    value = readIdentifierOrKeyword()
                Else
                    If Char.IsWhiteSpace(curChar) = True Then
                        value = ""
                        readWiteSpace()
                    Else
                        diagnostics.reportBadCharacter(curChar, 1)
                        value = ""
                        symbol = curChar.ToString()
                    End If
                End If
        End Select

        If symbol = Nothing Then
            symbol = ""
        End If

        Return New SyntaxToken(kind, lineNum, start, value)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function readNumberToken() As String

        Dim symbol As String = ""
        Dim intDummy As Integer

        start = pos

        While Char.IsDigit(curChar) = True
            symbol += curChar
            nextChar()
        End While

        If Integer.TryParse(symbol, intDummy) = False Then
            diagnostics.reportInvalidNumber(symbol, Type.GetType("Integer"), start, symbol.Length)
        End If

        kind = TokenKind.TK_INTEGER
        value = intDummy

        Return symbol
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Sub readWiteSpace()
        Dim symbol As String = ""

        start = pos
        While (Char.IsWhiteSpace(curChar) = True)
            symbol += curChar
            nextChar()
        End While

        kind = TokenKind.TK_SPACE
        value = ""
        start += 1
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function readIdentifierOrKeyword() As String
        Dim symbol As String = ""

        start = pos
        While (Char.IsLetter(curChar) = True)
            symbol += curChar
            nextChar()
        End While

        ' assign keyword token
        kind = SyntaxFacts.getKeywordKind(symbol)

        ' chech if symbol Is a valid variabel name
        If (kind = TokenKind.TK_BADTOKEN) Then
            If (SyntaxFacts.isValidVariable(symbol)) Then
                value = symbol
                kind = TokenKind.TK_IDENTIFIER
                start += 1
            End If
        End If

        onEvalLine = chkOnEvalLine(symbol)

        Return symbol
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function readCompilerConstantsOrMult() As String

        Dim symbol, peekStr As String
        Dim peekChar As Char

        start = pos
        peekPos = 0
        symbol = "*"
        peekStr = ""

        ' /check if the word Is an indicator by checking the first 2 chars
        nextChar()
        peekChar = curChar
        While (Asc(peekChar) > 32)
            peekStr += peekChar
            peekPos += 1
            peekChar = peek(peekPos)
        End While

        symbol = ("*" + peekStr)
        value = symbol
        peekStr = ("*" + peekStr.ToUpper())

        ' /a single * as passed 
        If (symbol = "*") Then
            nextChar()
            kind = TokenKind.TK_MULT
            Return "*"
        End If

        If (symbol = "**") Then
            ' /exponental
        End If

        ' /check if the symbol Is an indicator
        kind = SyntaxFacts.getBuiltInIndicator(peekStr)
        If (kind <> TokenKind.TK_BADTOKEN) Then
            nextChar()
            While Char.IsLetterOrDigit(curChar) = True
                nextChar()
            End While
            start += 1

            Return symbol
        End If

        ' /check compiler constants
        kind = SyntaxFacts.getCompilerConstans(peekStr)
        If (kind <> TokenKind.TK_BADTOKEN) Then
            nextChar()
            While Char.IsLetterOrDigit(curChar) = True
                nextChar()
            End While
            start += 1

            Return symbol
        End If

        kind = TokenKind.TK_MULT
        Return "*"
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Function readBuiltInFunctions() As String
        Dim symbol As String = ""

        start = pos
        symbol = "%"

        nextChar()
        While (Char.IsLetter(curChar) = True)
            symbol += curChar
            nextChar()
        End While

        kind = SyntaxFacts.getBuiltInFunction(symbol.ToUpper())
        value = symbol
        start += 1

        Return symbol
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Sub ignoreCommentLine()

        start = pos
        While (True)
            nextChar()
            If curChar = vbNewLine Or Asc(curChar) = 0 Then
                Exit While
            End If
        End While
        start += 1

        kind = TokenKind.TK_SPACE
    End Sub

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Function getDiagnostics() As DiagnosticBag
        Return diagnostics
    End Function
End Class
