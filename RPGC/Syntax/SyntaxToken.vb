﻿
Public Class SyntaxToken
    Inherits SyntaxNode

    Public Overrides Property kind As TokenKind
    Public Property pos As Integer
    Public Property line As Integer
    Public Property sym As Object

    Public special As String

    Public Sub New(k As TokenKind, l As Integer, p As Integer, s As Object)
        kind = k
        line = l
        pos = p
        sym = s
    End Sub

    ' ////////////////////////////////////////////////////////
    Public Overloads ReadOnly Property Span As TextSpan
        Get
            Dim tmp As String

            If sym Is Nothing Then
                tmp = ""
            Else
                tmp = sym.ToString()
            End If

            Return New TextSpan(pos, tmp.Length)
        End Get
    End Property

    ' ////////////////////////////////////////////////////////
    Friend Function isMissing() As Boolean
        Return (sym Is Nothing)
    End Function
End Class

' //////////////////////////////////////////////////////////////////
' /////     /////     /////     /////     /////     /////     /////
' //////////////////////////////////////////////////////////////// 

Public Enum TokenKind
    TK_ACQ
    TK_ADD
    TK_ADDDUR
    TK_ALLOC
    TK_AND
    TK_ASSIGN
    TK_BADTOKEN
    TK_BEGSR
    TK_BIFABS
    TK_BIFCHAR
    TK_BIFCHECKR
    TK_BIFDATE
    TK_BIFDAYS
    TK_BIFDEC
    TK_BIFDECH
    TK_BIFDIFF
    TK_BIFEDITC
    TK_BIFEDITW
    TK_BIFELEM
    TK_BIFEOF
    TK_BIFEQUAL
    TK_BIFERROR
    TK_BIFFIELDS
    TK_BIFFOUND
    TK_BIFHOURS
    TK_BIFINTH
    TK_BIFMINUTES
    TK_BIFMONTHS
    TK_BIFMSECONDS
    TK_BIFOPEN
    TK_BIFPARMS
    TK_BIFREPLACE
    TK_BIFSCAN
    TK_BIFSECONDS
    TK_BIFSIZE
    TK_BIFSTATUS
    TK_BIFSUBST
    TK_BIFTIMESTAMP
    TK_BIFTRIM
    TK_BIFTRIML
    TK_BIFTRIMR
    TK_BIFYEARS
    TK_BITOFF
    TK_BITON
    TK_BLOCKEND
    TK_BLOCKSTART
    TK_BLOCKSYNTX
    TK_BYNARYEXPR
    TK_CAB
    TK_CALL
    TK_CALLB
    TK_CALLP
    TK_CAS
    TK_CAT
    TK_CHAIN
    TK_CHECK
    TK_CHECKR
    TK_CLEAR
    TK_CLOSE
    TK_COLON
    TK_COMMIT
    TK_COMP
    TK_COMPLATIONUNT
    TK_DATE
    TK_DATETIME
    TK_DEALLOC
    TK_DEFINE
    TK_DELETE
    TK_DIV
    TK_DO
    TK_DOU
    TK_DOW
    TK_DSPLY
    TK_DUMP
    TK_ELSE
    TK_ENDCS
    TK_ENDDO
    TK_ENDIF
    TK_ENDFOR
    TK_ENDMON
    TK_ENDSR
    TK_ENDSL
    TK_EOI
    TK_EQ
    TK_EVAL
    TK_EVALR
    TK_EXCEPT
    TK_EXFMT
    TK_EXPONENT
    TK_EXPRNSTMNT
    TK_EXSR
    TK_EXTRCT
    TK_FEOD
    TK_FLOAT
    TK_FOR
    TK_FORCE
    TK_GE
    TK_GOTO
    TK_GRAPHIC
    TK_GT
    TK_IDENTIFIER
    TK_IF
    TK_IN
    TK_INDOFF
    TK_INDON
    TK_IN01
    TK_IN02
    TK_IN03
    TK_IN04
    TK_IN05
    TK_IN06
    TK_IN07
    TK_IN08
    TK_IN09
    TK_IN10
    TK_IN11
    TK_IN12
    TK_IN13
    TK_IN14
    TK_IN15
    TK_IN16
    TK_IN17
    TK_IN18
    TK_IN19
    TK_IN1P
    TK_IN20
    TK_IN21
    TK_IN22
    TK_IN23
    TK_IN24
    TK_IN25
    TK_IN26
    TK_IN27
    TK_IN28
    TK_IN29
    TK_IN30
    TK_IN31
    TK_IN32
    TK_IN33
    TK_IN34
    TK_IN35
    TK_IN36
    TK_IN37
    TK_IN38
    TK_IN39
    TK_IN40
    TK_IN41
    TK_IN42
    TK_IN43
    TK_IN44
    TK_IN45
    TK_IN46
    TK_IN47
    TK_IN48
    TK_IN49
    TK_IN50
    TK_IN51
    TK_IN52
    TK_IN53
    TK_IN54
    TK_IN55
    TK_IN56
    TK_IN57
    TK_IN58
    TK_IN59
    TK_IN60
    TK_IN61
    TK_IN62
    TK_IN63
    TK_IN64
    TK_IN65
    TK_IN66
    TK_IN67
    TK_IN68
    TK_IN69
    TK_IN70
    TK_IN71
    TK_IN72
    TK_IN73
    TK_IN74
    TK_IN75
    TK_IN76
    TK_IN77
    TK_IN78
    TK_IN79
    TK_IN80
    TK_IN81
    TK_IN82
    TK_IN83
    TK_IN84
    TK_IN85
    TK_IN86
    TK_IN87
    TK_IN88
    TK_IN89
    TK_IN90
    TK_IN91
    TK_IN92
    TK_IN93
    TK_IN94
    TK_IN95
    TK_IN96
    TK_IN97
    TK_IN98
    TK_IN99
    TK_INH1
    TK_INH2
    TK_INH3
    TK_INH4
    TK_INH5
    TK_INH6
    TK_INH7
    TK_INH8
    TK_INH9
    TK_INKA
    TK_INKB
    TK_INKC
    TK_INKD
    TK_INKE
    TK_INKF
    TK_INKG
    TK_INKH
    TK_INKI
    TK_INKJ
    TK_INKK
    TK_INKL
    TK_INKM
    TK_INKN
    TK_INKO
    TK_INKP
    TK_INKQ
    TK_INKR
    TK_INKS
    TK_INKT
    TK_INKU
    TK_INKV
    TK_INKW
    TK_INKX
    TK_INL1
    TK_INL2
    TK_INL3
    TK_INL4
    TK_INL5
    TK_INL6
    TK_INL7
    TK_INL8
    TK_INL9
    TK_INLR
    TK_INM1
    TK_INM2
    TK_INM3
    TK_INM4
    TK_INM5
    TK_INM6
    TK_INM7
    TK_INM8
    TK_INM9
    TK_INMR
    TK_INOA
    TK_INOG
    TK_INOV
    TK_INU1
    TK_INU2
    TK_INU3
    TK_INU4
    TK_INU5
    TK_INU6
    TK_INU7
    TK_INU8
    TK_INRT
    TK_INTEGER
    TK_ITER
    TK_KFLD
    TK_KLIST
    TK_LEAVE
    TK_LEAVESR
    TK_LOOKUP
    TK_LE
    TK_LITEXPR
    TK_LT
    TK_MHHZO
    TK_MHLZO
    TK_MLHZO
    TK_MLLZO
    TK_MONITOR
    TK_MOVE
    TK_MOVEA
    TK_MOVEL
    TK_MULT
    TK_MVR
    TK_NAMEDEXP
    TK_NE
    TK_NEXT
    TK_NONE
    TK_NOT
    TK_OCCUR
    TK_OPEN
    TK_OR
    TK_OTHER
    TK_OUT
    TK_PACKED
    TK_PARENEXP
    TK_PARENCLOSE
    TK_PARENOPEN
    TK_PARM
    TK_PLIST
    TK_POST
    TK_READ
    TK_READC
    TK_READE
    TK_READP
    TK_READPE
    TK_REALLOC
    TK_REL
    TK_RESET
    TK_RETURN
    TK_ROLBK
    TK_SCAN
    TK_SELECT
    TK_SEMI
    TK_SETGT
    TK_SETLL
    TK_SETOFF
    TK_SETON
    TK_SHTDN
    TK_SORTA
    TK_SPACE
    TK_SQRT
    TK_STRING
    TK_SUB
    TK_SUBDUR
    TK_SUBST
    TK_TAG
    TK_TEST
    TK_TESTB
    TK_TESTN
    TK_TESTZ
    TK_TIME
    TK_UNIOP
    TK_UNIEXP
    TK_UNLOCK
    TK_UPDATE
    TK_WHEN
    TK_WRITE
    TK_XFOOT
    TK_XLATE
    TK_ZADD
    TK_ZONED
    TK_ZSUB
    TK_INZ
    TK_VARDECLR
    TK_VARDDATAS
    TK_VARDCONST
    TK_ENDPROC
    TK_TO
    TK_INDICATOR
    TK_TIMESTAMP
End Enum
