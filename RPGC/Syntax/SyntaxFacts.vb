
Public Class SyntaxFacts
    Public Shared badChars_Var() As Char = {".", "\", ",", "<", ">", "?", "", ":", """", "'", "[", "]", "{", "}", "+", "*", "-", "=", "(", ")", "^", "!", "`", "~"}
    Public Shared badChars_ToStartVar() As Char = {"_", "&", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}



    'Public Static Char[] badChars_Var = { '.', '\\', ',', '<', '>', '?', '', ':', '"', '\'', '[', ']', '{', '}', '+', '*', '-', '=', '(', ')', '^', '!', '`', '~' }
    'Public Static Char[] badChars_ToStartVar = { '_', '&', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' }
    Private Shared struckeyWords() As String = {
                                "ACQ", "ADD", "ADDDUR", "ALLOC", "ANDEQ", "ANDGE", "ANDGT", "ANDLE", "ANDLT", "ANDNE", "ASSIGN", "BEGSR",
                                "CAB", "CALL", "CALLB", "CALLP", "CAS", "CAT", "CHAIN", "CHECK", "CHECKR", "CIN", "CLEAR", "CLOSE", "COLON",
                                "COMMIT", "COMP", "COUT", "DATE", "DEALLOC", "DEFINE", "DELETE", "DIV", "DO", "DOU", "DOUEQ", "DOUGE",
                                "DOUGT", "DOULE", "DOULT", "DOUNE", "DOW", "DOWEQ", "DOWGE", "DOWGT", "DOWLE", "DOWLT", "DOWNE", "DSPLY",
                                "DUMP", "ELSE", "END", "ENDCS", "ENDDO", "ENDFOR", "ENDIF", "ENDMON", "ENDSL", "ENDSR", "EVAL", "EVALR",
                                "EXCEPT", "EXFMT", "EXSR", "EXTRCT", "FEOD", "FOR", "FORCE", "GOTO", "IF", "IFEQ", "IFGE", "IFGT", "IFLE",
                                "IFLT", "IFNE", "IN", "ITER", "KFLD", "KLIST", "LEAVE", "LEAVESR", "LITEXPR", "LOOKUP", "MHHZO", "MHLZO",
                                "MLHZO", "MLLZO", "MONITOR", "MOVE", "MOVEA", "MOVEL", "MULT", "MVR", "NEXT", "NONE", "NOT", "OCCUR", "OPEN",
                                "OREQ", "ORGE", "ORGT", "ORLE", "ORLT", "ORNE", "OTHER", "OUT", "PARM", "PLIST", "POST", "PRINT", "READ",
                                "READC", "READE", "READP", "READPE", "REALLOC", "REL", "RESET", "RETURN", "ROLBK", "SCAN", "SELECT", "SEMI",
                                "SETGT", "SETLL", "SETOFF", "SETON", "SHTDN", "SORTA", "SPACE", "SQRT", "SUB", "SUBDUR", "SUBST", "TAG", "TEST",
                                "TESTB", "TESTN", "TESTZ", "TIME", "UNIEXP", "UNIOP", "UNLOCK", "UPDATE", "WHEN", "WRITE", "XFOOT", "XLATE",
                                "Z-ADD", "Z-SUB"}
    Private Shared freeKeyWords() As String = {"ACQ", "ADDDUR", "ALLOC", "AND", "ASSIGN", "BEGSR",
                                "CIN", "CHAIN", "CLEAR",
                                "CLOSE", "COMMIT", "COUT", "DCL-C", "DCL-S", "DEALLOC", "DEFINE", "DELETE",
                                "DOU", "DOW", "DSPLY", "DUMP", "ELSE", "END", "ENDCS", "ENDDO", "ENDFOR",
                                "ENDIF", "ENDMON", "ENDSL", "ENDSR", "EXCEPT", "EXFMT", "EXSR",
                                "EXTRCT", "FEOD", "FOR", "FORCE", "IF", "IN", "ITER", "LEAVE",
                                "LEAVESR", "LITEXPR", "MHHZO", "MONITOR",
                                "MULT", "NOT", "OCCUR", "OPEN", "OR",
                                "OTHER", "OUT", "POST", "PRINT", "READ", "READC",
                                "READE", "READP", "READPE", "REALLOC", "REL", "RESET", "RETURN", "ROLBK", "SCAN",
                                "SELECT", "SEMI", "SETGT", "SETLL", "SHTDN", "SORTA",
                                "SUBDUR", "SUBST",
                                "UNIEXP", "UNIOP", "UNLOCK", "UPDATE", "WHEN", "WRITE"}
    Private Shared freeBIFWithNoParan() As String = {"DSPLY", "SORTA", "CIN", "COUT", "READ", "READE", "SETLL", "SETGT", "OPEN", "CLOSE", "READC", "DELETE", "WRITE", "UPDATE"}
    Private Shared BIIndicators() As String = {"01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                               "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60",
                                               "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                               "91", "92", "93", "94", "95", "96", "97", "98", "99", "LR", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "KA", "KB", "KC", "KD", "KE", "KF", "KG", "KH", "KI", "KJ", "KK",
                                               "KL", "KM", "KN", "KO", "KP", "KQ", "KR", "KS", "KT", "KU", "KV", "KW", "KX", "L0", "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "M1", "M2", "M3", "M4", "M5", "M6", "M7",
                                               "M8", "M9", "MR", "OA", "OG", "OV", "RT"}

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
            Case "BEGSR"
                ' return TokenKind.TK_SUBRTNDCL
                Return TokenKind.TK_BLOCKSTART
            Case "DIV"
                Return TokenKind.TK_DIV
            Case "DCL-C"
                Return TokenKind.TK_VARDCONST
            Case "DCL-DS"
                Return TokenKind.TK_VARDDATAS
            Case "DCL-PROC"
                ' return TokenKind.TK_VARDECLR
                Return TokenKind.TK_BLOCKSTART
            Case "DCL-S"
                Return TokenKind.TK_VARDECLR
            Case "ELSE"
                Return TokenKind.TK_ELSE
            Case "END-PROC"
                Return TokenKind.TK_ENDPROC
            Case "ENDDO"
                Return TokenKind.TK_ENDDO
            Case "ENDIF"
                Return TokenKind.TK_ENDIF
            Case "ENDFOR"
                Return TokenKind.TK_ENDFOR
            Case "ENDMON"
                Return TokenKind.TK_ENDMON
            Case "ENDSL"
                Return TokenKind.TK_ENDSL
            Case "ENDSR"
                Return TokenKind.TK_ENDSR
            Case "END"
                Return TokenKind.TK_BLOCKEND
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
            Case "INT10"
                Return TokenKind.TK_INTEGER
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
            Case "MONITOR"
                Return TokenKind.TK_MONITOR
            Case "OR"
                Return TokenKind.TK_OR
            Case "SELECT"
                Return TokenKind.TK_SELECT
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
            num = (From ind In BIIndicators
                   Where ($"*IN{ind}".Equals(symbol) = True)).FirstOrDefault()

            ' check if indicator was found in the list
            If (num Is Nothing) = False Then
                Return TokenKind.TK_IDENTIFIER
            Else
                Return TokenKind.TK_BADTOKEN
            End If
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
        Dim symbol As String

        ' find the symbol in the list
        symbol = (From nt In freeBIFWithNoParan
                  Where (nt = kw)).FirstOrDefault()

        Return (symbol Is Nothing) = False
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function isValidOpCode(kw As String) As Boolean

        Dim ishere As String

        ' find the symbol in the list
        ishere = (From nt In struckeyWords
                  Where nt = kw).FirstOrDefault()

        Return (ishere Is Nothing) = False
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
            Case "SR",
                 "  ",
                 ""
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
            Case "AND",
                 "ANDLE",
                 "ANDLT",
                 "ANDGE",
                 "ANDGT",
                 "ANDEQ",
                 "ANDNE"
                kind = TokenKind.TK_AND
            Case "OR",
                 "ORLE",
                 "ORLT",
                 "ORGE",
                 "ORGT",
                 "OREQ",
                 "ORNE"
                kind = TokenKind.TK_OR
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
            Case "SUB"
                kind = TokenKind.TK_SUB
            Case "CIN",
                 "COUT",
                 "DSPLY",
                 "SUBST"
                kind = TokenKind.TK_IDENTIFIER
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
            Case "  ",
                     ""
                kind = TokenKind.TK_SPACE
            Case "01",
                     "02",
                     "03",
                     "04",
                     "05",
                     "06",
                     "07",
                     "08",
                     "09",
                     "10",
                     "11",
                     "12",
                     "13",
                     "14",
                     "15",
                     "16",
                     "17",
                     "18",
                     "19",
                     "20",
                     "21",
                     "22",
                     "23",
                     "24",
                     "25",
                     "26",
                     "27",
                     "28",
                     "29",
                     "30",
                     "31",
                     "32",
                     "33",
                     "34",
                     "35",
                     "36",
                     "37",
                     "38",
                     "39",
                     "40",
                     "41",
                     "42",
                     "43",
                     "44",
                     "45",
                     "46",
                     "47",
                     "48",
                     "49",
                     "50",
                     "51",
                     "52",
                     "53",
                     "54",
                     "55",
                     "56",
                     "57",
                     "58",
                     "59",
                     "60",
                     "61",
                     "62",
                     "63",
                     "64",
                     "65",
                     "66",
                     "67",
                     "68",
                     "69",
                     "70",
                     "71",
                     "72",
                     "73",
                     "74",
                     "75",
                     "76",
                     "77",
                     "78",
                     "79",
                     "80",
                     "81",
                     "82",
                     "83",
                     "84",
                     "85",
                     "86",
                     "87",
                     "88",
                     "89",
                     "90",
                     "91",
                     "92",
                     "93",
                     "94",
                     "95",
                     "96",
                     "97",
                     "98",
                     "99",
                     "LR",
                     "H1",
                     "H2",
                     "H3",
                     "H4",
                     "H5",
                     "H6",
                     "H7",
                     "H8",
                     "H9",
                     "KA",
                     "KB",
                     "KC",
                     "KD",
                     "KE",
                     "KF",
                     "KG",
                     "KH",
                     "KI",
                     "KJ",
                     "KK",
                     "KL",
                     "KM",
                     "KN",
                     "KO",
                     "KP",
                     "KQ",
                     "KR",
                     "KS",
                     "KT",
                     "KU",
                     "KV",
                     "KW",
                     "KX",
                     "L0",
                     "L1",
                     "L2",
                     "L3",
                     "L4",
                     "L5",
                     "L6",
                     "L7",
                     "L8",
                     "L9",
                     "M1",
                     "M2",
                     "M3",
                     "M4",
                     "M5",
                     "M6",
                     "M7",
                     "M8",
                     "M9",
                     "MR",
                     "OA",
                     "OG",
                     "OV",
                     "RT"
                kind = TokenKind.TK_IDENTIFIER
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

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getRPGType(vtype As String) As TokenKind

        Select Case (vtype.ToUpper())
            Case "I"
                Return TokenKind.TK_INTEGER
            Case "F"
                Return TokenKind.TK_FLOAT
            Case "P"
                Return TokenKind.TK_PACKED
            Case "S"
                Return TokenKind.TK_ZONED
            Case "N"
                Return TokenKind.TK_INDICATOR
            Case "D"
                Return TokenKind.TK_DATE
            Case "T"
                Return TokenKind.TK_TIME
            Case "Z"
                Return TokenKind.TK_TIMESTAMP
            Case "A", " "
                Return TokenKind.TK_STRING
            Case Else
                Return TokenKind.TK_BADTOKEN
        End Select
    End Function


    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getRPGTypeFree(vtype As String) As TokenKind

        Select Case vtype.ToUpper()
            Case "INT"
                Return TokenKind.TK_INTEGER
            Case "FLOAT"
                Return TokenKind.TK_FLOAT
            Case "PACKED"
                Return TokenKind.TK_PACKED
            Case "ZONED"
                Return TokenKind.TK_ZONED
            Case "IND"
                Return TokenKind.TK_INDICATOR
            Case "DATE"
                Return TokenKind.TK_DATE
            Case "TIME"
                Return TokenKind.TK_TIME
            Case "TIMESTAMP"
                Return TokenKind.TK_TIMESTAMP
            Case "CHAR", "VARCHAR"
                Return TokenKind.TK_STRING
            Case Else
                Return TokenKind.TK_BADTOKEN
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function doColectAnotherCard(symbol As String) As Boolean
        Select Case (symbol)
            Case "ANDEQ",
                "ANDNE",
                "ANDLT",
                "ANDGT",
                "ANDGE",
                "ANDLE",
                "OREQ",
                "ORNE",
                "ORLT",
                "ORGT",
                "ORGE",
                "ORLE",
                "IFEQ",
                "IFNE",
                "IFLT",
                "IFGT",
                "IFGE",
                "IFLE",
                "DOWEQ",
                "DOWNE",
                "DOWLT",
                "DOWGT",
                "DOWGE",
                "DOWLE",
                "DOUEQ",
                "DOUNE",
                "DOULT",
                "DOUGT",
                "DOUGE",
                "DOULE"
                Return True
            Case Else
                Return False
        End Select
    End Function


    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getindicatorOperation(Hi As String, Lo As String, Eq As String) As TokenKind
        Dim cmp As String = ""

        Hi = Hi.Trim()
        Lo = Lo.Trim()
        Eq = Eq.Trim()

        cmp += IIf(String.IsNullOrEmpty(Hi), "1", "0")
        cmp += IIf(String.IsNullOrEmpty(Lo), "1", "0")
        cmp += IIf(String.IsNullOrEmpty(Eq), "1", "0")

        Select Case cmp
            Case "001"
                Return TokenKind.TK_EQ
            Case "010"
                Return TokenKind.TK_LT
            Case "011"
                Return TokenKind.TK_LE
            Case "100"
                Return TokenKind.TK_GT
            Case "101"
                Return TokenKind.TK_GE
            Case "110"
                Return TokenKind.TK_NE
            Case Else
                Return TokenKind.TK_BADTOKEN
        End Select
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////
    Public Shared Function getAllIndicators() As String()
        Return BIIndicators
    End Function
End Class
