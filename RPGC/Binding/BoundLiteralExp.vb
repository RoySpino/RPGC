Public Class BoundLiteralExp
    Inherits BoundExpression

    Public Overrides Property tok As BoundNodeToken = BoundNodeToken.BNT_LITEX
    Public Property Value As Object

    Public Sub New(_val As Object)
        Value = _val

        If Value Is GetType(Integer) Then
            typ = TypeSymbol.Integer_
        Else
            If Value Is GetType(Double) Or Value Is GetType(Double) Then
                typ = TypeSymbol.Float
            Else
                If Value Is GetType(Boolean) Then
                    typ = TypeSymbol.Indicator
                Else
                    If Value Is GetType(String) Or Value Is GetType(Char) Then
                        typ = TypeSymbol.Char_
                    Else
                        If Value Is GetType(VariableSymbol) Then
                            typ = (DirectCast(Value, VariableSymbol)).Type_
                        Else
                            Throw New Exception($"Unexpected literal {Value} of type{Value.GetType()}")
                        End If
                    End If
                End If
            End If
        End If
    End Sub
End Class
