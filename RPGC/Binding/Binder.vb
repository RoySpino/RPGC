Public Class Binder
End Class

Public Enum BoundNodeToken
    BNT_UINEX
    BNT_LITEX
    BNT_VAREX
    BNT_ASNEX
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