﻿Public Class Decimator
    Private Shared specChkStr As String = "H"
    Private Shared isProcSection As Boolean = False
    Private Shared kind As TokenKind
    Private Shared curChar As String
    Private Shared Value As Object
    Private Shared start As Integer
    Private Shared tmpVal As Integer
    Private Shared diagnostics As DiagnosticBag = New DiagnosticBag()
    Private Shared assignmentCnt As Integer
    Private Shared onEvalLine As Boolean
    Private Shared parenCnt As Integer = 0
    Private Shared lineType As String = ""
    Private Shared onBooleanLine As Boolean

    Private Shared Function isGoodSpec(spec As String) As Boolean

        Dim prevSpec As String
        Dim prevSpecVal, curSpecVal As Integer
        Dim specVal(,) As Integer = {{72, 1}, {70, 2}, {68, 3}, {73, 4}, {67, 5}, {79, 6}}

        prevSpec = specChkStr(specChkStr.Length - 1)
        prevSpecVal = 1
        curSpecVal = -1

        ' see if current line Is in the pocedure section
        If spec = "P" And (prevSpec = "C" Or prevSpec = "P") Then

            isProcSection = True
            specChkStr = "D"
            Return True
        End If

        ' get spec value
        For i As Integer = 0 To (specVal.Length - 1)
            If Chr(specVal(i, 0)) = spec Then
                curSpecVal = specVal(i, 1)
                Exit For
            End If
        Next

        ' check if specs are grouped And ordered together
        If isProcSection = False Then
            If curSpecVal >= prevSpecVal Then
                specChkStr += spec
                Return True
            Else
                Return False
            End If
        Else
            If prevSpecVal >= curSpecVal And (curSpecVal = 68 Or curSpecVal = 67) Then
                specChkStr += spec
                Return True
            Else
                Return False
            End If
        End If
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function decimateCSpec(lineNo As Integer, line As String) As List(Of StructNode)

        Dim ret As List(Of StructNode) = New List(Of StructNode)()
        Dim strLen As Integer
        Dim sym As String
        Dim isOnEvalLine As Boolean = False

        Dim slicer(,) As Integer = {
                {1, 2},
                {3, 1},
                {4, 2},
                {6, 14},
                {20, 10},
                {30, 14},
                {44, 14},
                {58, 5},
                {63, 2},
                {65, 2},
                {67, 2},
                {69, 2}
            }

        ' check spec position
        If isGoodSpec(line(0)) = False Then
            Return Nothing
        End If

        strLen = line.Length - 1

        For i As Integer = 0 To (slicer.Length - 1)
            If strLen <= 0 Then
                Exit For
            End If

            If (isOnEvalLine = False) Then
                ' slice standard RPG C spec
                If strLen >= slicer(i, 1) Then
                    sym = line.Substring(slicer(i, 0), slicer(i, 1))
                Else
                    sym = line.Substring(slicer(i, 0))
                End If
                strLen -= slicer(i, 1)
            Else
                ' slice evaluation
                sym = line.Substring(slicer(i, 0))
                strLen = 0
            End If

            ' trim string
            If i = 7 Or i = 8 Then
                sym = sym.TrimStart()
            Else
                sym = sym.TrimEnd()
            End If

            ' check if on eval line
            isOnEvalLine = (i = 4 And (sym.Contains("EVAL") Or sym = "FOR" Or sym = "IF"))

            ret.Add(New StructNode(lineNo, slicer(i, 0), sym.TrimEnd()))
        Next

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function decimateDSpec(lineNo As Integer, line As String) As List(Of StructNode)

        Dim ret As List(Of StructNode) = New List(Of StructNode)()
        Dim strLen As Integer
        Dim sym As String
        Dim isOnEvalLine As Boolean = False

        Dim slicer(,) As Integer = {
                {1, 15},
                {16, 1},
                {17, 1},
                {18, 2},
                {20, 7},
                {27, 7},
                {34, 1},
                {35, 2},
                {38, 33}
            }

        ' check spec position
        If isGoodSpec(line(0)) = False Then
            Return Nothing
        End If

        strLen = line.Length - 1

        For i As Integer = 0 To (slicer.Length - 1)
            If strLen <= 0 Then
                Exit For
            End If

            ' slice string according to column position
            If strLen >= slicer(i, 1) Then
                sym = line.Substring(slicer(i, 0), slicer(i, 1))
            Else
                sym = line.Substring(slicer(i, 0))
            End If
            strLen -= slicer(i, 1)

            ' trim string
            If i = 4 Or i = 5 Or i = 7 Then
                sym = sym.TrimStart()
            Else
                sym = sym.TrimEnd()
            End If

            ret.Add(New StructNode(lineNo, slicer(i, 0), sym.TrimEnd()))
        Next

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function decimatePSpec(lineNo As Integer, line As String) As List(Of StructNode)

        Dim ret As List(Of StructNode) = New List(Of StructNode)()
        Dim strLen As Integer
        Dim sym As String
        Dim isOnEvalLine As Boolean = False

        Dim slicer(,) As Integer = {
                {1, 15},
                {18, 1},
                {30, 4},
                {34, 1},
                {35, 2},
                {38, 33}
            }

        ' check spec position
        If isGoodSpec(line(0)) = False Then
            Return Nothing
        End If

        strLen = line.Length - 1

        For i As Integer = 0 To (slicer.Length - 1)
            If strLen <= 0 Then
                Exit For
            End If

            ' slice string according to column position
            If strLen >= slicer(i, 1) Then
                sym = line.Substring(slicer(i, 0), slicer(i, 1))
            Else
                sym = line.Substring(slicer(i, 0))
            End If
            strLen -= slicer(i, 1)

            ' trim string
            sym = sym.Trim()

            ret.Add(New StructNode(lineNo, slicer(i, 0), sym.TrimEnd()))
        Next

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function decimateFSpec(lineNo As Integer, line As String) As List(Of StructNode)

        Dim ret As List(Of StructNode) = New List(Of StructNode)()
        Dim strLen As Integer
        Dim sym As String
        Dim isOnEvalLine As Boolean = False

        Dim slicer(,) As Integer = {
                {1, 10},
                {11, 1},
                {12, 1},
                {13, 1},
                {14, 1},
                {15, 1},
                {16, 1},
                {22, 1},
                {28, 1},
                {30, 7},
                {38, 33}
            }

        ' check spec position
        If isGoodSpec(line(0)) = False Then
            Return Nothing
        End If

        strLen = line.Length - 1

        For i As Integer = 0 To (slicer.Length - 1)
            If strLen <= 0 Then
                Exit For
            End If

            ' slice string according to column position
            If strLen >= slicer(i, 1) Then
                sym = line.Substring(slicer(i, 0), slicer(i, 1))
            Else
                sym = line.Substring(slicer(i, 0))
            End If
            strLen -= slicer(i, 1)

            ret.Add(New StructNode(lineNo, slicer(i, 0), sym.TrimEnd()))
        Next

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function decimateHSpec(lineNo As Integer, line As String) As List(Of StructNode)

        Dim ret As List(Of StructNode) = New List(Of StructNode)()
        Dim strLen As Integer
        Dim sym As String
        Dim isOnEvalLine As Boolean = False

        Dim slicer(,) As Integer = {
                {1, 79}
            }

        ' check spec position
        If isGoodSpec(line(0)) = False Then
            Return Nothing
        End If

        strLen = line.Length - 1

        For i As Integer = 0 To (slicer.Length - 1)
            If strLen <= 0 Then
                Exit For
            End If

            ' slice string according to column position
            If strLen >= slicer(i, 1) Then
                sym = line.Substring(slicer(i, 0), slicer(i, 1))
            Else
                sym = line.Substring(slicer(i, 0))
            End If
            strLen -= slicer(i, 1)

            ret.Add(New StructNode(lineNo, slicer(i, 0), sym.TrimEnd()))
        Next

        Return ret
    End Function

    '////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function doLex(factor As StructNode, Optional KeyWord As String = "") As List(Of SyntaxToken)
        Dim ret As List(Of SyntaxToken) = New List(Of SyntaxToken)()
        Dim symbol As String = ""
        Dim line As String
        Dim sz, linePos, idx As Integer

        line = factor.symbol
        start = factor.chrPos
        linePos = factor.linePos
        sz = line.Length
        assignmentCnt = 0

        For i As Integer = 0 To (sz - 1)
            curChar = line(i)
            idx = IIf(((i + 1) >= sz), (sz - 1), (i + 1))

            ' exit decimator lexer
            If Asc(curChar) < 32 Then
                Exit For
            End If

            ' c++ style comment line
            If linePos >= 1 And (curChar = "/" And line(idx) = "/") Then
                ignoreCommentLine(line, i)
            End If

            Select Case Asc(curChar)
                Case 43
                    kind = TokenKind.TK_ADD
                    Value = "+"
                Case 45
                    kind = TokenKind.TK_SUB
                    Value = "-"
                Case 42
                    symbol = readCompilerConstantsOrMult(line, i)
                    Value = symbol
                Case 47
                    kind = TokenKind.TK_DIV
                    Value = "/"
                Case 40
                    kind = TokenKind.TK_PARENOPEN
                    Value = "("
                    parenCnt += 1
                Case 41
                    kind = TokenKind.TK_PARENCLOSE
                    Value = ")"
                    parenCnt -= 1
                Case 61
                    kind = getAssignmentOrComparisonToken(line, i)
                    Value = "="
                Case 60
                    kind = getLessGreaterThanOperator("<", line(idx), i)
                    Value = tmpVal
                Case 62
                    kind = getLessGreaterThanOperator(">", line(idx), i)
                    Value = tmpVal
                Case 37
                    symbol = readBuiltInFunctions(line, i)
                    Value = symbol
                Case 64,
                     35,
                     36
                    symbol = readIdentifierOrKeyword(line, i)
                    Value = symbol
                Case 30,
                     31,
                     32,
                     33,
                     34,
                     35,
                     36,
                     37,
                     38,
                     39
                    symbol = readNumberToken(line, i)
                    Value = symbol
                Case 32,
                10,
                9,
                13,
                    readWiteSpace(line, i)
                    symbol = " "
                Case Else
                    If Char.IsLetter(curChar) = True Then
                        symbol = readIdentifierOrKeyword(line, i)
                    Else
                        If Char.IsWhiteSpace(curChar) = True Then
                            symbol = ""
                            readWiteSpace(line, i)
                        Else
                            diagnostics.reportBadCharacter(curChar, 1)
                            symbol = curChar.ToString()
                        End If
                        Value = symbol
                    End If
            End Select

            ret.Add(New SyntaxToken(kind, linePos, (start), Value))
            start += symbol.Length
        Next

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function readNumberToken(line As String, ByRef pos As Integer) As String
        Dim symbol As String = ""
        Dim intDummy As Integer

        ' get the numeric symbol
        While pos < line.Length And Char.IsDigit(line(pos)) = True
            symbol += line(pos)
            pos += 1
        End While
        pos -= 1

        ' try to covert to intgeger
        If Integer.TryParse(symbol, intDummy) = False Then
            diagnostics.reportInvalidNumber(symbol, TypeSymbol.Integer_, start + symbol.Length, symbol.Length)
            kind = TokenKind.TK_BADTOKEN
        Else
            kind = TokenKind.TK_INTEGER
        End If

        Value = intDummy

        Return symbol
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function readWiteSpace(line As String, ByRef pos As Integer) As String
        Dim symbol As String = ""

        While pos < line.Length And Char.IsWhiteSpace(line(pos)) = True
            symbol += curChar
            pos += 1
        End While
        pos -= 1

        kind = TokenKind.TK_SPACE
        Value = ""

        Return symbol
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Shared Function readIdentifierOrKeyword(line As String, ByRef pos As Integer) As String
        Dim symbol As String = ""

        While pos < line.Length And Char.IsLetter(line(pos)) = True
            symbol += line(pos)
            pos += 1
        End While
        pos -= 1

        ' assign keyword token
        kind = SyntaxFacts.getKeywordKind(symbol)

        ' chech if symbol Is a valid variabel name
        If (kind = TokenKind.TK_BADTOKEN) Then
            If (SyntaxFacts.isValidVariable(symbol)) Then
                Value = symbol
                kind = TokenKind.TK_IDENTIFIER
            End If
        End If

        Return symbol
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    'special indicators and constants defalts to mult if nothing is found
    Private Shared Function readCompilerConstantsOrMult(line As String, ByRef pos As Integer) As String
        Dim symbol As String
        Dim peekStr As String
        Dim peekChar As String
        Dim peekPos As Integer

        peekPos = pos
        symbol = "*"
        peekStr = ""

        ' check if the word Is an indicator by checking the first 2 chars
        peekPos += 1
        pos += 1
        While (peekPos < line.Length And Char.IsLetterOrDigit(line(peekPos)) = True)
            peekStr += line(peekPos)
            peekPos += 1

            If (peekPos >= line.Length) Then
                Exit While
            End If

            peekChar = line(peekPos)
        End While

        symbol = ("*" + peekStr)
        Value = symbol
        peekStr = ("*" + peekStr.ToUpper())

        ' check special case * values
        Select Case (symbol)
            Case "*"
                kind = TokenKind.TK_MULT
                Return symbol
            Case "**"
                kind = TokenKind.TK_EXPONENT
                Return symbol
        End Select

        ' check if the symbol Is an indicator
        kind = SyntaxFacts.getBuiltInIndicator(peekStr)
        If kind <> TokenKind.TK_BADTOKEN Then
            While pos < line.Length And Char.IsLetterOrDigit(line(pos)) = True
                pos += 1
                pos -= 1

                Return symbol
            End While
        End If

        ' check compiler constants
        kind = SyntaxFacts.getCompilerConstans(peekStr)
        If kind <> TokenKind.TK_BADTOKEN Then
            While pos < line.Length And Char.IsLetterOrDigit(line(pos)) = True
                pos += 1
                pos -= 1

                Return symbol
            End While
        End If

        ' symbol that was passed was Not a reserved word
        ' assuming multiply operation
        kind = TokenKind.TK_MULT
        pos -= 1
        Return "*"
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function readBuiltInFunctions(line As String, ByRef pos As Integer) As String
        Dim symbol As String = ""
        symbol = "%"

        pos += 1
        While pos < line.Length And Char.IsLetter(curChar) = True

            symbol += curChar
            pos += 1
        End While
        pos -= 1

        kind = SyntaxFacts.getBuiltInFunction(symbol.ToUpper())
        Value = symbol

        Return symbol
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function ignoreCommentLine(line As String, ByRef pos As Integer) As String
        While True
            pos += 1
            If Asc(line(pos)) = 10 Or Asc(line(pos)) = 0 Or Asc(line(pos)) = 13 Then
                Exit While
            End If
        End While

        kind = TokenKind.TK_SPACE
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function getAssignmentOrComparisonToken(line As String, ByRef pos As Integer) As String
        Dim ret As TokenKind

        onEvalLine = (parenCnt = 0)

        ' check if the current line is a comparison or assignment
        If onEvalLine = True And assignmentCnt < 1 Then
            ret = TokenKind.TK_ASSIGN
            assignmentCnt += 1
        Else
            ret = TokenKind.TK_EQ
        End If

        Return ret
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Private Shared Function getLessGreaterThanOperator(first As String, op As String, ByRef pos As Integer) As String
        Dim ret As TokenKind

        If first = "<" Then
            Select Case op
                Case ">"
                    ret = TokenKind.TK_NE
                    tmpVal = ("" + first + op)
                    pos += 1
                Case "="
                    ret = TokenKind.TK_LE
                    tmpVal = ("" + first + op)
                    pos += 1
                Case Else
                    ret = TokenKind.TK_LT
                    tmpVal = ("" + first)
            End Select
        Else
            Select Case op
                Case "="
                    ret = TokenKind.TK_GE
                    tmpVal = ("" + first + op)
                    pos += 1
                Case Else
                    ret = TokenKind.TK_GT
                    tmpVal = ("" + first)
            End Select
        End If

        Return ret
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getComparisonOpCode(node As StructNode) As SyntaxToken
        Dim op As String

        op = node.symbol
        op = op.Substring(op.Length - 2)

        Select Case (op)
            Case "EQ"
                Return New SyntaxToken(TokenKind.TK_EQ, node.linePos, node.chrPos, node.symbol)
            Case "NE"
                Return New SyntaxToken(TokenKind.TK_NE, node.linePos, node.chrPos, node.symbol)

            Case "LT"
                Return New SyntaxToken(TokenKind.TK_LT, node.linePos, node.chrPos, node.symbol)
            Case "GT"
                Return New SyntaxToken(TokenKind.TK_GT, node.linePos, node.chrPos, node.symbol)

            Case "GE"
                Return New SyntaxToken(TokenKind.TK_GE, node.linePos, node.chrPos, node.symbol)
            Case "LE"
                Return New SyntaxToken(TokenKind.TK_LE, node.linePos, node.chrPos, node.symbol)
            Case Else
                Return New SyntaxToken(TokenKind.TK_BADTOKEN, node.linePos, node.chrPos, node.symbol)
        End Select
    End Function

    Public Shared Function getComparisonInd(node1 As StructNode, node2 As StructNode, node3 As StructNode) As StructNode
        Dim tmp As StructNode

        ' get the first non blank symbol
        If node1.symbol.Trim().Length > 0 Then
            tmp = node1
        Else
            If node2.symbol.Trim().Length > 0 Then
                tmp = node2
            Else
                tmp = node3
            End If
        End If

        ' convert two digit indicator to standard indicator
        tmp.symbol = $"*IN{tmp.symbol}"

        Return tmp
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function doDecimation(lineNo As Integer, line As String) As List(Of StructNode)
        Dim specificatin As String
        Dim tmp As String

        tmp = line.ToUpper()
        specificatin = line(0)

        If line.Length = 0 Then
            Return New List(Of StructNode)()
        End If

        Select Case specificatin
            Case "H"
                Return decimateHSpec(lineNo, tmp)
            Case "F"
                Return decimateFSpec(lineNo, tmp)
            Case "D"
                Return decimateDSpec(lineNo, tmp)
            Case "I"
                Return Nothing
            Case "C"
                Return decimateCSpec(lineNo, tmp)
            Case "O"
                Return Nothing
            Case "P"
                Return decimatePSpec(lineNo, tmp)
            Case Else
                Return Nothing
        End Select

    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function cSpecRectifier(lst As List(Of StructNode), ByRef diagnostics As DiagnosticBag)

        Dim ret As List(Of SyntaxToken) = New List(Of SyntaxToken)()
        Dim tToken As SyntaxToken
        Dim itmCnt As Integer
        Dim OpCode As String

        itmCnt = lst.Count
            onEvalLine = false
            onBooleanLine = False

        ' factor 1 is not empty and has no key word
        If itmCnt = 4 And lst(3).symbol.Length > 0 Then
            ret.Add(New SyntaxToken(TokenKind.TK_BADTOKEN, lst(itmCnt).linePos, lst(itmCnt).chrPos, lst(3).symbol))
            diagnostics.reportMissingFactor1(New TextSpan(lst(3).chrPos, lst(3).symbol.Length),
                                                 lst(3).linePos)
        End If

        ' rectify structured code to free lexicon
        If itmCnt >= 6 Then
            'check If the opcode Is valid
            OpCode = lst(4).symbol.Trim()

            If SyntaxFacts.isValidOpCode(OpCode) = False Then
                diagnostics.reportBadOpcode(OpCode, lst(4).linePos)
                ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(4).linePos, lst(4).chrPos, OpCode))
                Return ret
            End If

            ' perform rectifier
            Select Case OpCode
                Case "ADD",
                         "SUB",
                         "MULT",
                         "DIV"
                    If lst(3).symbol = "" Then
                        ' +=,-=,*=,/= factor 2 to factor 3
                        ret.AddRange(doLex(lst(6)))
                        ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, OpCode))
                        ret.AddRange(doLex(lst(6)))
                        ret.AddRange(doLex(lst(4)))
                        ret.AddRange(doLex(lst(5)))
                    Else
                        ' factors 1,2 and 3
                        ret.AddRange(doLex(lst(6)))
                        ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, OpCode))
                        ret.AddRange(doLex(lst(3)))
                        ret.AddRange(doLex(lst(4)))
                        ret.AddRange(doLex(lst(5)))
                    End If
                Case "CALLB",
                         "CALLP"
                    onBooleanLine = True
                Case "COMP"
                    ret.AddRange(doLex(getComparisonInd(lst(9), lst(10), lst(11))))
                    ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(4).linePos, lst(4).chrPos, "COMP"))
                    ret.AddRange(doLex(lst(3)))
                    ret.Add(New SyntaxToken(SyntaxFacts.getindicatorOperation(lst(9).symbol, lst(10).symbol, lst(11).symbol), lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(5)))
                Case "CIN"
                    'ret.AddRange(doLex(lst(6)))
                    ret.AddRange(doLex(lst(3)))
                    ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, "CIN"))
                    ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.Add(New SyntaxToken(TokenKind.TK_PARENOPEN, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.Add(New SyntaxToken(TokenKind.TK_PARENCLOSE, lst(4).linePos, lst(4).chrPos, OpCode))
                Case "COUT",
                         "PRINT",
                         "DSPLY"
                    ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.Add(New SyntaxToken(TokenKind.TK_PARENOPEN, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.Add(New SyntaxToken(TokenKind.TK_PARENCLOSE, lst(4).linePos, lst(4).chrPos, OpCode))
                Case "DO"
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, OpCode))
                Case "DOUGE",
                         "DOUGT",
                         "DOULE",
                         "DOULT",
                         "DOUEQ",
                         "DOUNE"
                    ret.Add(New SyntaxToken(TokenKind.TK_DOU, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.AddRange(doLex(lst(4)))
                    ret.AddRange(doLex(lst(5)))
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                    lineType = "DOU"
                Case "DOU"
                    onBooleanLine = True
                    If lst(3).symbol <> "" Then
                        ' somthing was entered in factor
                        ret.Add(New SyntaxToken(TokenKind.TK_SPACE, lst(3).linePos, lst(3).chrPos, ""))
                        diagnostics.reportBadFactor(New TextSpan(lst(3).chrPos, lst(3).symbol.Length), 1, lst(3).linePos)
                    Else
                        ret.Add(New SyntaxToken(TokenKind.TK_DOU, lst(4).linePos, lst(4).chrPos, OpCode))
                        ret.AddRange(doLex(lst(5)))
                        ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                        lineType = "DOU"
                    End If
                Case "DOWGE",
                         "DOWGT",
                         "DOWLE",
                         "DOWLT",
                         "DOWEQ",
                         "DOWNE"
                    ret.Add(New SyntaxToken(TokenKind.TK_DOW, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.AddRange(doLex(lst(4)))
                    ret.AddRange(doLex(lst(5)))
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                    lineType = "DOW"
                Case "DOW"
                    onBooleanLine = True
                    If lst(3).symbol <> "" Then
                        ' somthing was entered in factor 1
                        ret.Add(New SyntaxToken(TokenKind.TK_SPACE, lst(3).linePos, lst(3).chrPos, ""))
                        diagnostics.reportBadFactor(New TextSpan(lst(3).chrPos, lst(3).symbol.Length), 1, lst(3).linePos)
                    Else
                        ret.Add(New SyntaxToken(TokenKind.TK_DOW, lst(4).linePos, lst(4).chrPos, OpCode))
                        ret.AddRange(doLex(lst(5)))
                        ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                        lineType = "DOW"
                    End If
                Case "IF"
                    onBooleanLine = True
                    If lst(3).symbol <> "" Then
                        ' somthing was entered in factor
                        ret.Add(New SyntaxToken(TokenKind.TK_SPACE, lst(3).linePos, lst(3).chrPos, ""))
                        diagnostics.reportBadFactor(New TextSpan(lst(3).chrPos, lst(3).symbol.Length), 1, lst(3).linePos)
                    Else
                        ret.Add(New SyntaxToken(TokenKind.TK_IF, lst(4).linePos, lst(4).chrPos, OpCode))
                        ret.AddRange(doLex(lst(5)))
                        ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                        lineType = "IF"
                    End If
                Case "IFGE"
                Case "IFGT"
                Case "IFLE"
                Case "IFLT"
                Case "IFEQ"
                Case "IFNE"
                    ret.Add(New SyntaxToken(TokenKind.TK_IF, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.AddRange(doLex(lst(4)))
                    ret.AddRange(doLex(lst(5)))
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                    lineType = "IF"
                Case "ELSE"
                    ret.Add(New SyntaxToken(TokenKind.TK_ELSE, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                    lineType = "ELSE"
                Case "END"
                    ret.Add(New SyntaxToken(TokenKind.TK_BLOCKEND, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDDO"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDDO, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDIF"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDIF, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDFOR"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDFOR, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDMON"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDMON, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDSL"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDSL, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ENDSR"
                    ret.Add(New SyntaxToken(TokenKind.TK_ENDSR, lst(4).linePos, lst(4).chrPos, OpCode))
                    lineType = ""
                Case "ORGE",
                         "ORGT",
                         "ORLE",
                         "ORLT",
                         "OREQ",
                         "ORNE"
                    ret.Add(New SyntaxToken(TokenKind.TK_OR, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.Add(getComparisonOpCode(lst(4)))
                    ret.AddRange(doLex(lst(5)))
                Case "ANDGE",
                         "ANDGT",
                         "ANDLE",
                         "ANDLT",
                         "ANDEQ",
                         "ANDNE"
                    ret.Add(New SyntaxToken(TokenKind.TK_AND, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(3)))
                    ret.Add(getComparisonOpCode(lst(4)))
                    ret.AddRange(doLex(lst(5)))
                Case "EVAL"
                Case "EVALR"
                    onEvalLine = True
                    If lst(3).symbol <> "" Then
                        ' somthing was entered in factor
                        ret.Add(New SyntaxToken(TokenKind.TK_SPACE, lst(3).linePos, lst(3).chrPos, ""))
                        diagnostics.reportBadFactor(New TextSpan(lst(3).chrPos, lst(3).symbol.Length), 1, lst(3).linePos)
                    Else
                        ret.AddRange(doLex(lst(5)))
                    End If
                Case "FOR"
                    onEvalLine = True
                    If lst(3).symbol <> "" Then
                        'somthing was entered in factor
                        ret.Add(New SyntaxToken(TokenKind.TK_SPACE, lst(3).linePos, lst(3).chrPos, ""))
                        diagnostics.reportBadFactor(New TextSpan(lst(3).chrPos, lst(3).symbol.Length), 1, lst(3).linePos)
                    Else
                        ret.AddRange(doLex(lst(4)))
                        ret.AddRange(doLex(lst(5)))
                        ret.Add(New SyntaxToken(TokenKind.TK_BLOCKSTART, lst(4).linePos, lst(4).chrPos, ""))
                    End If
                Case "MOVE"
                    ret.AddRange(doLex(lst(6)))
                    ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, "MOVE"))
                    ret.AddRange(doLex(lst(5)))
                Case "TAG"
                    tToken = New SyntaxToken(TokenKind.TK_TAG, lst(4).linePos, lst(3).chrPos, OpCode)
                    tToken.special = lst(3).symbol
                    ret.Add(tToken)
                    ret.AddRange(doLex(lst(3)))
                Case "GOTO"
                    ret.Add(New SyntaxToken(TokenKind.TK_GOTO, lst(4).linePos, lst(5).chrPos, OpCode))
                    ret.AddRange(doLex(lst(5)))
                Case "Z-ADD"
                    ret.AddRange(doLex(lst(6)))
                    ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, OpCode))
                    ret.Add(New SyntaxToken(TokenKind.TK_INTEGER, lst(4).linePos, lst(4).chrPos, "0"))
                    ret.Add(New SyntaxToken(TokenKind.TK_ADD, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(5)))
                Case "Z-SUB"
                    ret.AddRange(doLex(lst(6)))
                    ret.Add(New SyntaxToken(TokenKind.TK_ASSIGN, lst(6).linePos, lst(6).chrPos, OpCode))
                    ret.AddRange(doLex(New StructNode(lst(4).linePos, lst(4).chrPos, "0")))
                    ret.Add(New SyntaxToken(TokenKind.TK_SUB, lst(4).linePos, lst(4).chrPos, OpCode))
                    ret.AddRange(doLex(lst(5)))
            End Select
        End If

        Return ret
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function dSpecRectifier(lst As List(Of StructNode), ByRef diagnostics As DiagnosticBag) As List(Of SyntaxToken)
        Dim ret As List(Of SyntaxToken) = New List(Of SyntaxToken)()
        Dim dclType As String

        dclType = lst(3).symbol

        Select Case (dclType)
            Case "C"
                ret.Add(New SyntaxToken(TokenKind.TK_VARDCONST, lst(0).linePos, 1, "C"))
                ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(0).linePos, lst(0).chrPos, lst(0).symbol))
                ret.Add(New SyntaxToken(SyntaxFacts.getRPGType(lst(6).symbol), lst(6).linePos, lst(6).chrPos, lst(6).symbol))
            Case "DS"
            Case "PI"
            Case "PR"
            Case "S"
            Case Else
                ret.Add(New SyntaxToken(TokenKind.TK_VARDECLR, lst(0).linePos, 1, "S"))
                ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(0).linePos, lst(0).chrPos, lst(0).symbol))
                ret.Add(New SyntaxToken(TokenKind.TK_IDENTIFIER, lst(0).linePos, lst(0).chrPos, lst(0).symbol))
                ret.Add(New SyntaxToken(SyntaxFacts.getRPGType(lst(6).symbol), lst(6).linePos, lst(6).chrPos, lst(6).symbol))
        End Select

        Return ret
    End Function
End Class
