Imports System.Collections.Immutable

Public Class SourceText
    Public Property Lines As ImmutableArray(Of TextLine)
    Private _Text As String

    Public Sub New(txt As String)
        _Text = txt
        Lines = parseLines(Me, txt)
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////
    Public Shared Function parseLines(sText As SourceText, text As String) As ImmutableArray(Of TextLine)

        Dim ret As ImmutableArray(Of TextLine).Builder
        Dim line As TextLine
        Dim lineStart, lineBreakWdith As Integer
        Dim pos As Integer

        ret = ImmutableArray.CreateBuilder(Of TextLine)()
        lineStart = 0
        pos = 0

        While pos < text.Length
            lineBreakWdith = GetLineWithLineBreak(text, pos)

            If lineBreakWdith = 0 Then
                pos += 1
            Else

                addLine(ret, sText, pos, lineStart, lineBreakWdith)
                pos += lineBreakWdith
                lineStart = pos
            End If
        End While

        If (pos > lineStart) Then
            addLine(ret, sText, pos, lineStart, 0)
        End If

        Return ret.ToImmutable()
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Private Shared Sub addLine(output As ImmutableArray(Of TextLine).Builder, sText As SourceText, pos As Integer, lineStart As Integer, lineBreakWith As Integer)
        Dim ret As TextLine

        ret = New TextLine(sText,
                           lineStart,
                           (pos - lineStart),
                           (pos + lineBreakWith) - lineStart)
        output.Add(ret)
    End Sub

    ' ///////////////////////////////////////////////////////////////////////////
    ' get the number of caracters on line break
    Private Shared Function GetLineWithLineBreak(text As String, pos As Integer) As Integer
        Dim curchar As String
        Dim peekchar As String

        curchar = text(pos)
        peekchar = IIf((pos + 1) >= text.Length, Chr(0), text(pos))

        ' check for [New line] and [Carriage return]
        If Asc(curchar) = 10 And Asc(peekchar) = 13 Then
            Return 2
        End If

        If Asc(curchar) = 10 Or Asc(peekchar) = 13 Then
            Return 1
        End If

        Return 0
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Shared Function from(txt As String)
        Return New SourceText(txt)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Overrides Function ToString() As String
        Return _Text
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Overloads Function ToString(start As Integer, Len As Integer) As String
        Return _Text.Substring(start, Len)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Overloads Function ToString(span As TextSpan) As String
        Dim start, len, tot As Integer

        start = span.START
        len = span.LENGTH
        tot = len + start

        If tot > _Text.Length Then
            Return ""
        End If

        Return _Text.Substring(span.START, span.LENGTH)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Overloads Function ToString(start As Integer) As String

        'return empty string when outside the range of the string
        If (start < 0 Or start >= _Text.Length) Then
            Return ""
        End If

        Return _Text.Substring(start)
    End Function

    ' ///////////////////////////////////////////////////////////////////////////
    Public Function getLineIndes(pos As Integer) As Integer
        Dim max, Mid, min, diff As Integer

        max = Lines.Length
        min = 0
        Mid = max >> 1

        While True
            ' check symbol
            If Lines(Mid).Start = pos Then
                Return Mid
            End If

            ' set New range
            If Lines(Mid).Start < pos Then
                min = Mid
            Else
                max = Mid
            End If

            ' compute next symbol
            diff = (max - min)
            Mid = diff >> 1
            Mid += min

            ' exit symbol Not found
            If diff <= 1 Then
                Exit While
            End If
        End While

        Return 0
    End Function
End Class
