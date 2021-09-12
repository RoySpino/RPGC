Public Class Conversion
    Public Shared ReadOnly NONE_ As Conversion = New Conversion(False, False, False)
    Public Shared ReadOnly IDENTITY_ As Conversion = New Conversion(True, True, True)
    Public Shared ReadOnly IMPLICIT_ As Conversion = New Conversion(True, False, True)
    Public Shared ReadOnly EXPLICIT_ As Conversion = New Conversion(True, False, False)

    Public Property Exits As Boolean
    Public Property Identity As Boolean
    Public Property Implicit As Boolean


    Private Sub New(exits_ As Boolean, isIdentity_ As Boolean, isImplicit_ As Boolean)
        Exits = exits_
        Identity = isIdentity_
        Implicit = isImplicit_
    End Sub

    ' //////////////////////////////////////////////////////////////////////////////////////
    Public Function Explicit() As Boolean
        Return Exits = True And Implicit = False
    End Function

    ' //////////////////////////////////////////////////////////////////////////////////////
    Public Shared Function Clasifiyer(from_ As TypeSymbol, To_ As TypeSymbol) As Conversion
        ' to string
        If from_.Equals(TypeSymbol.Indicator) = True Or from_.Equals(TypeSymbol.Integer_) = True Or from_.Equals(TypeSymbol.Date_) = True Or from_.Equals(TypeSymbol.DateTime) = True Or from_.Equals(TypeSymbol.Float) = True Then
            If To_.Equals(TypeSymbol.Char_) = True Then
                Return Conversion.EXPLICIT_
            End If
        End If

        ' char to integer boolean float date time timestamp
        If from_.Equals(TypeSymbol.Char_) = True Then
            If To_.Equals(TypeSymbol.Indicator) = True Or To_.Equals(TypeSymbol.Integer_) = True Or To_.Equals(TypeSymbol.Date_) = True Or To_.Equals(TypeSymbol.DateTime) = True Or To_.Equals(TypeSymbol.Float) = True Or To_.Equals(TypeSymbol.Time) = True Then
                Return Conversion.EXPLICIT_
            End If
        End If

        ' integer to float
        If from_.Equals(TypeSymbol.Integer_) = True Then
            If To_.Equals(TypeSymbol.Float) = True Then
                Return Conversion.EXPLICIT_
            End If
        End If

        ' both types are the same
        If from_.Equals(To_) = True Then
            Return Conversion.IDENTITY_
        End If

        Return Conversion.NONE_
    End Function
End Class
