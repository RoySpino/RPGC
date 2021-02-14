Public Class SyntaxFacts
    Public Shared badChars_Var() As Char = {".", "\", ",", "<", ">", "?", "", ":", """", "'", "[", "]", "{", "}", "+", "*", "-", "=", "(", ")", "^", "!", "`", "~"}
    Public Shared badChars_ToStartVar() As Char = {"_", "&", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}

    ' extention method 
    ' useage: tokenKind.TK_ADD.getBinaryOporatorPrecedence()
    Public Shared Function getBinaryOporatorPrecedence(tok As TokenKind) As Integer

        Select Case (tok)
            Case TokenKind.TK_MULT,
                 TokenKind.TK_DIV
                Return 5
            Case TokenKind.TK_ADD,
                 TokenKind.TK_SUB
                Return 4
            Case TokenKind.TK_EQ,
                 TokenKind.TK_NE,
                 TokenKind.TK_GE,
                 TokenKind.TK_GT,
                 TokenKind.TK_LE,
                 TokenKind.TK_LT
                Return 3
            Case TokenKind.TK_AND
                Return 2
            Case TokenKind.TK_OR
                Return 1
            Case Else
                Return 0
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    ' extention method 
    ' useage: tokenKind.TK_ADD.getBinaryOporatorPrecedence()
    Public Shared Function getUinaryOporatorPrecedence(tok As TokenKind) As Integer
        Select Case tok

            Case TokenKind.TK_ADD,
                 TokenKind.TK_SUB,
                 TokenKind.TK_NOT
                Return 6
            Case Else
                Return 0
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getKeywordKind(symbol As String) As TokenKind
        symbol = symbol.ToUpper()

        If symbol.StartsWith("*") Then
            Return getBuiltInIndicator(symbol)
        End If
        If symbol.StartsWith("%") Then
            Return getBuiltInFunction(symbol)
        End If

        Select Case symbol
            Case "ADD"
                Return TokenKind.TK_ADD
            Case "AND"
                Return TokenKind.TK_AND
            Case "DIV"
                Return TokenKind.TK_DIV
            Case "IF"
                Return TokenKind.TK_IF
            Case "IFEQ"
                Return TokenKind.TK_EQ
            Case "IFGE"
                Return TokenKind.TK_GE
            Case "IFGT"
                Return TokenKind.TK_GT
            Case "IFLE"
                Return TokenKind.TK_LE
            Case "IFLT"
                Return TokenKind.TK_LT
            Case "IFNE"
                Return TokenKind.TK_NE
            Case "MOVE"
                Return TokenKind.TK_ASSIGN
            Case "MOVEA"
                Return TokenKind.TK_MOVEA
            Case "MOVEL"
                Return TokenKind.TK_MOVEL
            Case "NOT"
                Return TokenKind.TK_NOT
            Case "MULT"
                Return TokenKind.TK_MULT
            Case "OR"
                Return TokenKind.TK_OR
            Case "SUB"
                Return TokenKind.TK_SUB
            Case Else
                If Char.IsDigit(symbol(0)) = True Then
                    Return TokenKind.TK_BADTOKEN
                Else
                    Return TokenKind.TK_IDENTIFIER
                End If
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getBuiltInFunction(symbol As String) As TokenKind
        symbol = symbol.ToUpper()
        Select Case (symbol)
            Case "%ABS"
                Return TokenKind.TK_BIFABS
            Case "%CHAR"
                Return TokenKind.TK_BIFCHAR
            Case "%CHECK"
                Return TokenKind.TK_CHECK
            Case "%CHECKR"
                Return TokenKind.TK_CHECKR
            Case "%DATE"
                Return TokenKind.TK_BIFDATE
            Case "%DAYS"
                Return TokenKind.TK_BIFDAYS
            Case "%DEC"
                Return TokenKind.TK_BIFDEC
            Case "%DECH"
                Return TokenKind.TK_BIFDECH
            Case "%DIFF"
                Return TokenKind.TK_BIFDIFF
            Case "%EDITC"
                Return TokenKind.TK_BIFEDITC
            Case "%EDITW"
                Return TokenKind.TK_BIFEDITW
            Case "%ELEM"
                Return TokenKind.TK_BIFELEM
            Case "%EOF"
                Return TokenKind.TK_BIFEOF
            Case "%EQUAL"
                Return TokenKind.TK_BIFEQUAL
            Case "%ERROR"
                Return TokenKind.TK_BIFERROR
            Case "%FIELDS"
                Return TokenKind.TK_BIFFIELDS
            Case "%FOUND"
                Return TokenKind.TK_BIFFOUND
            Case "%HOURS"
                Return TokenKind.TK_BIFHOURS
            Case "%INTH"
                Return TokenKind.TK_BIFINTH
            Case "%MINUTES"
                Return TokenKind.TK_BIFMINUTES
            Case "%MONTHS"
                Return TokenKind.TK_BIFMONTHS
            Case "%MSSECONDS"
                Return TokenKind.TK_BIFMSECONDS
            Case "%OPEN"
                Return TokenKind.TK_BIFOPEN
            Case "%PARMS"
                Return TokenKind.TK_BIFPARMS
            Case "%REPLACE"
                Return TokenKind.TK_BIFREPLACE
            Case "%SCAN"
                Return TokenKind.TK_BIFSCAN
            Case "%SECONDS"
                Return TokenKind.TK_BIFSECONDS
            Case "%SIZE"
                Return TokenKind.TK_BIFSIZE
            Case "%STATUS"
                Return TokenKind.TK_BIFSTATUS
            Case "%SUBST"
                Return TokenKind.TK_BIFSUBST
            Case "%TIMESTAMP"
                Return TokenKind.TK_BIFTIMESTAMP
            Case "%TRIM"
                Return TokenKind.TK_BIFTRIM
            Case "%TRIML"
                Return TokenKind.TK_BIFTRIML
            Case "%TRIMR"
                Return TokenKind.TK_BIFTRIMR
            Case "%YEARS"
                Return TokenKind.TK_BIFYEARS
            Case Else
                Return TokenKind.TK_BADTOKEN
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getBuiltInIndicator(symbol As String) As TokenKind

        Dim num As String
        Dim isStartsWith_IN, is5CharsLong As Boolean
        symbol = symbol.ToUpper()

        ' set boolean values
        isStartsWith_IN = symbol.Contains("IN")
        is5CharsLong = (symbol.Length = 5)

        ' check boolean values
        If isStartsWith_IN = True And is5CharsLong = True Then
            num = symbol.Substring(3)

            Select Case (num)
                Case "LR"
                    Return TokenKind.TK_INLR
                Case "L1"
                    Return TokenKind.TK_INL1
                Case "L2"
                    Return TokenKind.TK_INL2
                Case "L3"
                    Return TokenKind.TK_INL3
                Case "L4"
                    Return TokenKind.TK_INL4
                Case "L5"
                    Return TokenKind.TK_INL5
                Case "L6"
                    Return TokenKind.TK_INL6
                Case "L7"
                    Return TokenKind.TK_INL7
                Case "L8"
                    Return TokenKind.TK_INL8
                Case "L9"
                    Return TokenKind.TK_INL9
                Case "M1"
                    Return TokenKind.TK_INM1
                Case "M2"
                    Return TokenKind.TK_INM2
                Case "M3"
                    Return TokenKind.TK_INM3
                Case "M4"
                    Return TokenKind.TK_INM4
                Case "M5"
                    Return TokenKind.TK_INM5
                Case "M6"
                    Return TokenKind.TK_INM6
                Case "M7"
                    Return TokenKind.TK_INM7
                Case "M8"
                    Return TokenKind.TK_INM8
                Case "M9"
                    Return TokenKind.TK_INM9
                Case "OA"
                    Return TokenKind.TK_INOA
                Case "OG"
                    Return TokenKind.TK_INOG
                Case "OV"
                    Return TokenKind.TK_INOV
                Case "RT"
                    Return TokenKind.TK_INRT

                Case "01"
                    Return TokenKind.TK_IN01
                Case "02"
                    Return TokenKind.TK_IN02
                Case "03"
                    Return TokenKind.TK_IN03
                Case "04"
                    Return TokenKind.TK_IN04
                Case "05"
                    Return TokenKind.TK_IN05
                Case "06"
                    Return TokenKind.TK_IN06
                Case "07"
                    Return TokenKind.TK_IN07
                Case "08"
                    Return TokenKind.TK_IN08
                Case "09"
                    Return TokenKind.TK_IN09
                Case "10"
                    Return TokenKind.TK_IN10

                Case "11"
                    Return TokenKind.TK_IN11
                Case "12"
                    Return TokenKind.TK_IN12
                Case "13"
                    Return TokenKind.TK_IN13
                Case "14"
                    Return TokenKind.TK_IN14
                Case "15"
                    Return TokenKind.TK_IN15
                Case "16"
                    Return TokenKind.TK_IN16
                Case "17"
                    Return TokenKind.TK_IN17
                Case "18"
                    Return TokenKind.TK_IN18
                Case "19"
                    Return TokenKind.TK_IN19
                Case "20"
                    Return TokenKind.TK_IN20

                Case "21"
                    Return TokenKind.TK_IN21
                Case "22"
                    Return TokenKind.TK_IN22
                Case "23"
                    Return TokenKind.TK_IN23
                Case "24"
                    Return TokenKind.TK_IN24
                Case "25"
                    Return TokenKind.TK_IN25
                Case "26"
                    Return TokenKind.TK_IN26
                Case "27"
                    Return TokenKind.TK_IN27
                Case "28"
                    Return TokenKind.TK_IN28
                Case "29"
                    Return TokenKind.TK_IN29
                Case "30"
                    Return TokenKind.TK_IN30

                Case "31"
                    Return TokenKind.TK_IN31
                Case "32"
                    Return TokenKind.TK_IN32
                Case "33"
                    Return TokenKind.TK_IN33
                Case "34"
                    Return TokenKind.TK_IN34
                Case "35"
                    Return TokenKind.TK_IN35
                Case "36"
                    Return TokenKind.TK_IN36
                Case "37"
                    Return TokenKind.TK_IN37
                Case "38"
                    Return TokenKind.TK_IN38
                Case "39"
                    Return TokenKind.TK_IN39
                Case "40"
                    Return TokenKind.TK_IN40

                Case "41"
                    Return TokenKind.TK_IN41
                Case "42"
                    Return TokenKind.TK_IN42
                Case "43"
                    Return TokenKind.TK_IN43
                Case "44"
                    Return TokenKind.TK_IN44
                Case "45"
                    Return TokenKind.TK_IN45
                Case "46"
                    Return TokenKind.TK_IN46
                Case "47"
                    Return TokenKind.TK_IN47
                Case "48"
                    Return TokenKind.TK_IN48
                Case "49"
                    Return TokenKind.TK_IN49
                Case "50"
                    Return TokenKind.TK_IN50

                Case "51"
                    Return TokenKind.TK_IN51
                Case "52"
                    Return TokenKind.TK_IN52
                Case "53"
                    Return TokenKind.TK_IN53
                Case "54"
                    Return TokenKind.TK_IN54
                Case "55"
                    Return TokenKind.TK_IN55
                Case "56"
                    Return TokenKind.TK_IN56
                Case "57"
                    Return TokenKind.TK_IN57
                Case "58"
                    Return TokenKind.TK_IN58
                Case "59"
                    Return TokenKind.TK_IN59
                Case "60"
                    Return TokenKind.TK_IN60

                Case "61"
                    Return TokenKind.TK_IN61
                Case "62"
                    Return TokenKind.TK_IN62
                Case "63"
                    Return TokenKind.TK_IN63
                Case "64"
                    Return TokenKind.TK_IN64
                Case "65"
                    Return TokenKind.TK_IN65
                Case "66"
                    Return TokenKind.TK_IN66
                Case "67"
                    Return TokenKind.TK_IN67
                Case "68"
                    Return TokenKind.TK_IN68
                Case "69"
                    Return TokenKind.TK_IN69
                Case "70"
                    Return TokenKind.TK_IN70

                Case "71"
                    Return TokenKind.TK_IN71
                Case "72"
                    Return TokenKind.TK_IN72
                Case "73"
                    Return TokenKind.TK_IN73
                Case "74"
                    Return TokenKind.TK_IN74
                Case "75"
                    Return TokenKind.TK_IN75
                Case "76"
                    Return TokenKind.TK_IN76
                Case "77"
                    Return TokenKind.TK_IN77
                Case "78"
                    Return TokenKind.TK_IN78
                Case "79"
                    Return TokenKind.TK_IN89
                Case "80"
                    Return TokenKind.TK_IN30

                Case "81"
                    Return TokenKind.TK_IN81
                Case "82"
                    Return TokenKind.TK_IN82
                Case "83"
                    Return TokenKind.TK_IN83
                Case "84"
                    Return TokenKind.TK_IN84
                Case "85"
                    Return TokenKind.TK_IN85
                Case "86"
                    Return TokenKind.TK_IN86
                Case "87"
                    Return TokenKind.TK_IN87
                Case "88"
                    Return TokenKind.TK_IN88
                Case "89"
                    Return TokenKind.TK_IN89
                Case "90"
                    Return TokenKind.TK_IN90

                Case "91"
                    Return TokenKind.TK_IN91
                Case "92"
                    Return TokenKind.TK_IN92
                Case "93"
                    Return TokenKind.TK_IN93
                Case "94"
                    Return TokenKind.TK_IN94
                Case "95"
                    Return TokenKind.TK_IN95
                Case "96"
                    Return TokenKind.TK_IN96
                Case "97"
                    Return TokenKind.TK_IN97
                Case "98"
                    Return TokenKind.TK_IN98
                Case "99"
                    Return TokenKind.TK_IN99

                Case Else
                    Return TokenKind.TK_BADTOKEN
            End Select
        Else
            Select Case (symbol)
                Case "*ON"
                    Return TokenKind.TK_INDON
                Case "*OFF"
                    Return TokenKind.TK_INDOFF
                Case Else
                    Return TokenKind.TK_BADTOKEN
            End Select
        End If
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function isKeyword(kw As String) As Boolean
        Dim max, mid, min, diff As Integer
        Dim keyWords() As String = {"ACQ", "ADDDUR", "ALLOC", "AND", "ASSIGN", "BEGSR",
                                    "CHAIN", "CLEAR",
                                    "CLOSE", "COMMIT", "DEALLOC", "DEFINE", "DELETE",
                                    "DOU", "DOW", "DSPLY", "DUMP", "ELSE", "END", "ENDCS", "ENDDO", "ENDFOR",
                                    "ENDIF", "ENDMON", "ENDSL", "ENDSR", "EXCEPT", "EXFMT", "EXSR",
                                    "EXTRCT", "FEOD", "FOR", "FORCE", "IF", "IN", "ITER", "LEAVE",
                                    "LEAVESR", "LITEXPR", "MHHZO", "MONITOR",
                                    "MULT", "NOT", "OCCUR", "OPEN", "OR",
                                    "OTHER", "OUT", "POST", "READ", "READC",
                                    "READE", "READP", "READPE", "REALLOC", "REL", "RESET", "RETURN", "ROLBK", "SCAN",
                                    "SELECT", "SEMI", "SETGT", "SETLL", "SHTDN", "SORTA",
                                    "SUBDUR", "SUBST",
                                    "UNIEXP", "UNIOP", "UNLOCK", "UPDATE", "WHEN", "WRITE"}

        max = keyWords.Length
        min = 0
        mid = max >> 1

        While True
            ' check symbol
            If keyWords(mid) = kw Then
                Return True
            End If

            ' set New range
            If (String.Compare(keyWords(mid), kw) < 0) Then
                    min = mid
                Else
                    max = mid
                End If

                ' compute next symbol
                diff = (max - min)
                mid = diff >> 1
                mid += min

                ' exit symbol Not found
                If (diff <= 1) Then
                    Return False
                End If
        End While

        Return False
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function isValidOpCode(kw As String) As Boolean
        Dim max, mid, min, diff As Integer
        Dim keyWords() As String = {"ACQ", "ADD", "ADDDUR", "ALLOC", "ANDGE", "ANDGT", "ANDLE", "ANDLT", "ASSIGN", "BEGSR",
                                    "CAB", "CALL", "CALLB", "CALLP", "CAS", "CAT", "CHAIN", "CHECK", "CHECKR", "CLEAR",
                                    "CLOSE", "COLON", "COMMIT", "COMP", "DATE", "DEALLOC", "DEFINE", "DELETE", "DIV",
                                    "DO", "DOU", "DOW", "DSPLY", "DUMP", "ELSE", "END", "ENDCS", "ENDDO", "ENDFOR",
                                    "ENDIF", "ENDMON", "ENDSL", "ENDSR", "EVAL", "EVALR", "EXCEPT", "EXFMT", "EXSR",
                                    "EXTRCT", "FEOD", "FOR", "FORCE", "GOTO", "IF", "IN", "ITER", "KFLD", "KLIST", "LEAVE",
                                    "LEAVESR", "LITEXPR", "LOOKUP", "MHHZO", "MHLZO", "MLHZO", "MLLZO", "MONITOR", "MOVE",
                                    "MOVEA", "MOVEL", "MULT", "MVR", "NEXT", "NONE", "NOT", "OCCUR", "OPEN", "ORGE",
                                    "ORGT", "ORLE", "ORLT", "OTHER", "OUT", "PARM", "PLIST", "POST", "READ", "READC",
                                    "READE", "READP", "READPE", "REALLOC", "REL", "RESET", "RETURN", "ROLBK", "SCAN",
                                    "SELECT", "SEMI", "SETGT", "SETLL", "SETOFF", "SETON", "SHTDN", "SORTA", "SPACE",
                                    "SQRT", "SUB", "SUBDUR", "SUBST", "TAG", "TEST", "TESTB", "TESTN", "TESTZ", "TIME",
                                    "UNIEXP", "UNIOP", "UNLOCK", "UPDATE", "WHEN", "WRITE", "XFOOT", "XLATE", "Z-ADD", "Z-SUB"}

        max = keyWords.Length
        min = 0
        mid = max >> 1

        While True
            ' check symbol
            If keyWords(mid) = kw Then
                Return True
            End If

            ' set New range
            If (String.Compare(keyWords(mid), kw) < 0) Then
                min = mid
            Else
                max = mid
            End If

            ' compute next symbol
            diff = (max - min)
            mid = diff >> 1
            mid += min

            ' exit symbol Not found
            If (diff <= 1) Then
                Return False
            End If
        End While

        Return False
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function isValidVariable(v As String) As Boolean
        ' variable must start with a valid char
        ' And Not contain any bad characters
        If v.IndexOfAny(badChars_ToStartVar) > 0 Then
            If v.IndexOfAny(badChars_Var) < 0 Then
                Return True
            End If
        End If


        Return False
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getCompilerConstans(constKw As String) As TokenKind
        Select Case constKw
            Case "*BLANK",
                 "*BLANKS"
                Return TokenKind.TK_STRING
            Case "*ZERO",
                 "*ZEROS"
                Return TokenKind.TK_INTEGER
            Case "*HIVAL",
                 "*LOVAL",
                 "*ALL",
                 "*ALLX"
                Return TokenKind.TK_BADTOKEN
        End Select

        Return TokenKind.TK_BADTOKEN
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getColum3Kyes(symbol As StructNode) As SyntaxToken

        Dim kind As TokenKind

        Select Case symbol.symbol
            Case "N"
                kind = TokenKind.TK_NOT
            Case ""
                kind = TokenKind.TK_SPACE
            Case Else
                kind = TokenKind.TK_BADTOKEN
        End Select

        Return New SyntaxToken(kind, symbol.linePos, symbol.charPos, symbol.symbol)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getColum2Kyes(symbol As StructNode) As SyntaxToken
        Dim kind As TokenKind

        Select Case symbol.symbol
            Case "AN"
                kind = TokenKind.TK_AND
            Case "OR"
                kind = TokenKind.TK_OR
            Case "LR"
                kind = TokenKind.TK_INLR
            Case "L0"
                kind = TokenKind.TK_INDON
            Case "L1"
                kind = TokenKind.TK_INL1
            Case "L2"
                kind = TokenKind.TK_INL2
            Case "L3"
                kind = TokenKind.TK_INL3
            Case "L4"
                kind = TokenKind.TK_INL4
            Case "L5"
                kind = TokenKind.TK_INL5
            Case "L6"
                kind = TokenKind.TK_INL6
            Case "L7"
                kind = TokenKind.TK_INL7
            Case "L8"
                kind = TokenKind.TK_INL8
            Case "L9"
                kind = TokenKind.TK_INL9
            Case "SR"
                kind = TokenKind.TK_SPACE
            Case Else
                kind = TokenKind.TK_BADTOKEN
        End Select

        Return New SyntaxToken(kind, symbol.linePos, symbol.charPos, symbol.symbol)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getIdentifierToken(symbol As StructNode) As SyntaxToken

        Dim sym As String
        Dim kind As TokenKind

        sym = symbol.symbol

        ' check if there are any numbers at the start of variable
        If Char.IsDigit(sym(0)) = True Then
            kind = TokenKind.TK_BADTOKEN
        Else
            kind = TokenKind.TK_BADTOKEN

            ' check if there are illegal chars in name
            If sym.IndexOfAny(badChars_ToStartVar) > 0 Then
                If (sym.IndexOfAny(badChars_Var) < 0) Then
                    kind = TokenKind.TK_IDENTIFIER
                End If
            End If
        End If

        Return New SyntaxToken(kind, symbol.linePos, symbol.charPos, sym)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getKeywordToken(symbol As StructNode) As SyntaxToken

        Dim kind As TokenKind

        Select Case symbol.symbol
            Case "ADD"
                kind = TokenKind.TK_ADD
            Case "AND"
                kind = TokenKind.TK_AND
            Case "DIV"
                kind = TokenKind.TK_DIV
            Case "IF"
                kind = TokenKind.TK_IF
            Case "IFEQ"
                kind = TokenKind.TK_EQ
            Case "IFGE"
                kind = TokenKind.TK_GE
            Case "IFGT"
                kind = TokenKind.TK_GT
            Case "IFLE"
                kind = TokenKind.TK_LE
            Case "IFLT"
                kind = TokenKind.TK_LT
            Case "IFNE"
                kind = TokenKind.TK_NE
            Case "MOVE"
                kind = TokenKind.TK_ASSIGN
            Case "MOVEA"
                kind = TokenKind.TK_MOVEA
            Case "MOVEL"
                kind = TokenKind.TK_MOVEL
            Case "NOT"
                kind = TokenKind.TK_NOT
            Case "MULT"
                kind = TokenKind.TK_MULT
            Case "OR"
                kind = TokenKind.TK_OR
            Case "SUB"
                kind = TokenKind.TK_SUB
            Case Else
                If Char.IsDigit(symbol.symbol(0)) = True Then
                    kind = TokenKind.TK_BADTOKEN
                Else
                    kind = TokenKind.TK_IDENTIFIER
                End If
        End Select

        Return New SyntaxToken(kind, symbol.linePos, symbol.charPos, symbol.symbol)
    End Function

    ' ////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function twoDigitIndicators(ind As StructNode) As SyntaxToken

        Dim kind As TokenKind
        Dim sym As String

        kind = TokenKind.TK_SPACE

        ' if symbol is blank empy then return space token
        If String.IsNullOrEmpty(ind.symbol) = True Then
            Return New SyntaxToken(kind, ind.linePos, ind.charPos, "  ")
        Else
            sym = ind.symbol
        End If

        ' assign indicator
        Select Case sym
            Case "  "
                kind = TokenKind.TK_SPACE
            Case "01"
                kind = TokenKind.TK_IN01
            Case "02"
                kind = TokenKind.TK_IN02
            Case "03"
                kind = TokenKind.TK_IN03
            Case "04"
                kind = TokenKind.TK_IN04
            Case "05"
                kind = TokenKind.TK_IN05
            Case "06"
                kind = TokenKind.TK_IN06
            Case "07"
                kind = TokenKind.TK_IN07
            Case "08"
                kind = TokenKind.TK_IN08
            Case "09"
                kind = TokenKind.TK_IN09
            Case "10"
                kind = TokenKind.TK_IN10
            Case "11"
                kind = TokenKind.TK_IN11
            Case "12"
                kind = TokenKind.TK_IN12
            Case "13"
                kind = TokenKind.TK_IN13
            Case "14"
                kind = TokenKind.TK_IN14
            Case "15"
                kind = TokenKind.TK_IN15
            Case "16"
                kind = TokenKind.TK_IN16
            Case "17"
                kind = TokenKind.TK_IN17
            Case "18"
                kind = TokenKind.TK_IN18
            Case "19"
                kind = TokenKind.TK_IN19
            Case "20"
                kind = TokenKind.TK_IN20

            Case "21"
                kind = TokenKind.TK_IN21
            Case "22"
                kind = TokenKind.TK_IN22
            Case "23"
                kind = TokenKind.TK_IN23
            Case "24"
                kind = TokenKind.TK_IN24
            Case "25"
                kind = TokenKind.TK_IN25
            Case "26"
                kind = TokenKind.TK_IN26
            Case "27"
                kind = TokenKind.TK_IN27
            Case "28"
                kind = TokenKind.TK_IN28
            Case "29"
                kind = TokenKind.TK_IN29
            Case "30"
                kind = TokenKind.TK_IN30

            Case "31"
                kind = TokenKind.TK_IN31
            Case "32"
                kind = TokenKind.TK_IN32
            Case "33"
                kind = TokenKind.TK_IN33
            Case "34"
                kind = TokenKind.TK_IN34
            Case "35"
                kind = TokenKind.TK_IN35
            Case "36"
                kind = TokenKind.TK_IN36
            Case "37"
                kind = TokenKind.TK_IN37
            Case "38"
                kind = TokenKind.TK_IN38
            Case "39"
                kind = TokenKind.TK_IN39
            Case "40"
                kind = TokenKind.TK_IN40

            Case "41"
                kind = TokenKind.TK_IN41
            Case "42"
                kind = TokenKind.TK_IN42
            Case "43"
                kind = TokenKind.TK_IN43
            Case "44"
                kind = TokenKind.TK_IN44
            Case "45"
                kind = TokenKind.TK_IN45
            Case "46"
                kind = TokenKind.TK_IN46
            Case "47"
                kind = TokenKind.TK_IN47
            Case "48"
                kind = TokenKind.TK_IN48
            Case "49"
                kind = TokenKind.TK_IN49
            Case "50"
                kind = TokenKind.TK_IN50

            Case "51"
                kind = TokenKind.TK_IN51
            Case "52"
                kind = TokenKind.TK_IN52
            Case "53"
                kind = TokenKind.TK_IN53
            Case "54"
                kind = TokenKind.TK_IN54
            Case "55"
                kind = TokenKind.TK_IN55
            Case "56"
                kind = TokenKind.TK_IN56
            Case "57"
                kind = TokenKind.TK_IN57
            Case "58"
                kind = TokenKind.TK_IN58
            Case "59"
                kind = TokenKind.TK_IN59
            Case "60"
                kind = TokenKind.TK_IN60

            Case "61"
                kind = TokenKind.TK_IN61
            Case "62"
                kind = TokenKind.TK_IN62
            Case "63"
                kind = TokenKind.TK_IN63
            Case "64"
                kind = TokenKind.TK_IN64
            Case "65"
                kind = TokenKind.TK_IN65
            Case "66"
                kind = TokenKind.TK_IN66
            Case "67"
                kind = TokenKind.TK_IN67
            Case "68"
                kind = TokenKind.TK_IN68
            Case "69"
                kind = TokenKind.TK_IN69
            Case "70"
                kind = TokenKind.TK_IN70

            Case "71"
                kind = TokenKind.TK_IN71
            Case "72"
                kind = TokenKind.TK_IN72
            Case "73"
                kind = TokenKind.TK_IN73
            Case "74"
                kind = TokenKind.TK_IN74
            Case "75"
                kind = TokenKind.TK_IN75
            Case "76"
                kind = TokenKind.TK_IN76
            Case "77"
                kind = TokenKind.TK_IN77
            Case "78"
                kind = TokenKind.TK_IN78
            Case "79"
                kind = TokenKind.TK_IN79
            Case "80"
                kind = TokenKind.TK_IN80

            Case "81"
                kind = TokenKind.TK_IN81
            Case "82"
                kind = TokenKind.TK_IN82
            Case "83"
                kind = TokenKind.TK_IN83
            Case "84"
                kind = TokenKind.TK_IN84
            Case "85"
                kind = TokenKind.TK_IN85
            Case "86"
                kind = TokenKind.TK_IN86
            Case "87"
                kind = TokenKind.TK_IN87
            Case "88"
                kind = TokenKind.TK_IN88
            Case "89"
                kind = TokenKind.TK_IN89
            Case "90"
                kind = TokenKind.TK_IN90


            Case "91"
                kind = TokenKind.TK_IN91
            Case "92"
                kind = TokenKind.TK_IN92
            Case "93"
                kind = TokenKind.TK_IN93
            Case "94"
                kind = TokenKind.TK_IN94
            Case "95"
                kind = TokenKind.TK_IN95
            Case "96"
                kind = TokenKind.TK_IN96
            Case "97"
                kind = TokenKind.TK_IN97
            Case "98"
                kind = TokenKind.TK_IN98
            Case "99"
                kind = TokenKind.TK_IN99
            Case "LR"
                kind = TokenKind.TK_INLR

            Case "H1"
                kind = TokenKind.TK_INH1
            Case "H2"
                kind = TokenKind.TK_INH2
            Case "H3"
                kind = TokenKind.TK_INH3
            Case "H4"
                kind = TokenKind.TK_INH4
            Case "H5"
                kind = TokenKind.TK_INH5
            Case "H6"
                kind = TokenKind.TK_INH6
            Case "H7"
                kind = TokenKind.TK_INH7
            Case "H8"
                kind = TokenKind.TK_INH8
            Case "H9"
                kind = TokenKind.TK_INH9
            Case "KA"
                kind = TokenKind.TK_INKA

            Case "KB"
                kind = TokenKind.TK_INKB

            Case "KC"
                kind = TokenKind.TK_INKC

            Case "KD"
                kind = TokenKind.TK_INKD

            Case "KE"
                kind = TokenKind.TK_INKE

            Case "KF"
                kind = TokenKind.TK_INKF

            Case "KG"
                kind = TokenKind.TK_INKG

            Case "KH"
                kind = TokenKind.TK_INKH

            Case "KI"
                kind = TokenKind.TK_INKI

            Case "KJ"
                kind = TokenKind.TK_INKJ

            Case "KK"
                kind = TokenKind.TK_INKK

            Case "KL"
                kind = TokenKind.TK_INKL

            Case "KM"
                kind = TokenKind.TK_INKM

            Case "KN"
                kind = TokenKind.TK_INKN

            Case "KO"
                kind = TokenKind.TK_INKO

            Case "KP"
                kind = TokenKind.TK_INKP

            Case "KQ"
                kind = TokenKind.TK_INKQ

            Case "KR"
                kind = TokenKind.TK_INKR

            Case "KS"
                kind = TokenKind.TK_INKS

            Case "KT"
                kind = TokenKind.TK_INKT

            Case "KU"
                kind = TokenKind.TK_INKU

            Case "KV"
                kind = TokenKind.TK_INKV

            Case "KW"
                kind = TokenKind.TK_INKW

            Case "KX"
                kind = TokenKind.TK_INKX

            Case "1"
                kind = TokenKind.TK_INL1
            Case "L2"
                kind = TokenKind.TK_INL2
            Case "L3"
                kind = TokenKind.TK_INL3
            Case "L4"
                kind = TokenKind.TK_INL4
            Case "L5"
                kind = TokenKind.TK_INL5
            Case "L6"
                kind = TokenKind.TK_INL6
            Case "L7"
                kind = TokenKind.TK_INL7
            Case "L8"
                kind = TokenKind.TK_INL8
            Case "L9"
                kind = TokenKind.TK_INL9
            Case "M1"
                kind = TokenKind.TK_INM1
            Case "M2"
                kind = TokenKind.TK_INM2
            Case "M3"
                kind = TokenKind.TK_INM3
            Case "M4"
                kind = TokenKind.TK_INM4
            Case "M5"
                kind = TokenKind.TK_INM5
            Case "M6"
                kind = TokenKind.TK_INM6
            Case "M7"
                kind = TokenKind.TK_INM7
            Case "M8"
                kind = TokenKind.TK_INM8
            Case "M9"
                kind = TokenKind.TK_INM9
            Case "MR"
                kind = TokenKind.TK_INMR
            Case "OA"
                kind = TokenKind.TK_INOA
            Case "OG"
                kind = TokenKind.TK_INOG
            Case "OV"
                kind = TokenKind.TK_INOV
            Case "RT"
                kind = TokenKind.TK_INRT
            Case Else
                kind = TokenKind.TK_BADTOKEN
        End Select

        Return New SyntaxToken(kind, ind.linePos, ind.charPos, ind.symbol)
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getText(kind As TokenKind) As String
        Select Case (kind)
            Case TokenKind.TK_ADD
                Return "+"
        End Select

        Return Nothing
    End Function
End Class
