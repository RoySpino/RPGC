Imports System.Linq
Public Class DiagnosticBag
    Implements IEnumerable(Of Diagnostics)

    Private _diagnostic As List(Of Diagnostics) = New List(Of Diagnostics)()

    Public Sub report(spn As TextSpan, msg As String)
        Dim diag As Diagnostics

        diag = New Diagnostics(spn, msg)
        _diagnostic.Add(diag)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub report(msg As String, start As Integer, len As Integer)
        Dim diag As Diagnostics

        diag = New Diagnostics(New TextSpan(start, len), msg)
        _diagnostic.Add(diag)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportInvalidNumber(symbol As String, typVal As Type, str As Integer, len As Integer)
        Dim msg As String

        msg = String.Format("rpgc: the symbol {0} is not a valid {1}", symbol, typVal)
        report(msg, str, len)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportBadCharacter(symbol As String, position As Integer)
        Dim msg As String

        msg = String.Format("rpgc: the symbol {0} is not a valid {1}", symbol)
        report(msg, (position - 1), 1)
    End Sub

    ' ////////////////////////////////////////////////////////////////////////
    Public Sub reportUnexpectedToken(span As TextSpan, actual As TokenKind, expected As TokenKind)
        Dim message As String

        message = String.Format("rpgc:({2}): unexpected token [{0}] expected [{1}]", actual, expected, "{0},{1}")

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportUndefinedUniaryOp(span As TextSpan, opSym As String, opType As Type)
        Dim message As String

        message = String.Format("rpgc:({2}): uninary operator [{0}] is not defined for type {1}", opSym, opType, "{0},{1}")

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportUndefinedBynaryOp(span As TextSpan, opSym As String, LeftType As Type, RightType As Type)
        Dim message As String

        message = String.Format("rpgc:({3}): binnary operator [{0}] is not defined for types {1} and {2}", opSym, LeftType, RightType, "{0},{1}")

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportUndefinedName(span As TextSpan, name As String)
        Dim message As String

        message = String.Format("rpgc:({1}): the variable [{0}] is not defined", name, "{0},{1}")

        report(span, message)
    End Sub

    Friend Sub reportCannotConvertType(span As TextSpan, name As String, typeA As Type, typeB As Type)
        Dim message As String

        message = String.Format("rpgc: can not convert variable [{0}] from {1} to {2}", name, typeA, typeB)

        report(span, message)
    End Sub

    Friend Sub reportVariableAlreadyDeclared(span As TextSpan, name As String)
        Dim message As String

        message = String.Format("rpgc:({1},{2}): redeclaration of ‘{0}’", name, 0, 0)

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportMissingFactor1(span As TextSpan, lp As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},6): factor 1 without Key word on line {0}", lp)

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportNotLeftJustified(span As TextSpan, factor As Integer, lp As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},{2}): factor {0} is not left justified", factor, lp, span.START)

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportNotRightJustified(span As TextSpan, factor As Integer, lp As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},{2}): factor {0} is not right justifide", factor, lp, span.START)

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportBadFactor(span As TextSpan, factor As Integer, lp As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},{2}): factor {0} is not empty", factor, lp, span.START)

        report(span, message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportBadSpec(symbol As String, linePosition As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},1): unknown specification [{0}]", symbol, linePosition)

        report(New TextSpan(0, 1), message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub reportBadOpcode(symbol As String, linePosition As Integer)
        Dim message As String

        message = String.Format("rpgc:({1},21): unknown Operation Code [{0}]", symbol, linePosition)

        report(New TextSpan(20, symbol.Length), message)
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Function GetEnumerator() As IEnumerator
        Return _diagnostic.GetEnumerator()
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Function IEnumerable_GetEnumerator() As IEnumerator(Of Diagnostics) Implements IEnumerable(Of Diagnostics).GetEnumerator
        Return _diagnostic.GetEnumerator()
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Private Function IEnumerable_GetEnumerator1() As IEnumerator Implements IEnumerable.GetEnumerator
        Return GetEnumerator()
    End Function

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub AddRange(a As DiagnosticBag)
        If a Is Nothing = False Then
            _diagnostic.AddRange(a._diagnostic)
        End If
    End Sub

    ' //////////////////////////////////////////////////////////////////////////
    Public Sub Clear()
        _diagnostic.Clear()
    End Sub
End Class
