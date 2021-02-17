Imports System.Text

Module Module1

    Sub Main()
        Dim ln As String
        Dim prefix As String
        Dim _error As String
        Dim sufix As String
        Dim outLn As String
        Dim lineIndex As Integer
        Dim lineNumber As Integer
        Dim charPos As Integer
        Dim doShowTree As Boolean = False
        Dim isBlank As Boolean
        Dim stree As SyntaxTree
        Dim complatin As Complation
        Dim bexpr As EvaluationResult
        Dim RPGDiagnostics As DiagnosticBag = New DiagnosticBag()
        Dim variables As Dictionary(Of VariableSymbol, Object) = New Dictionary(Of VariableSymbol, Object)
        Dim line As TextLine
        Dim spanPrefix As TextSpan
        Dim spanError As TextSpan
        Dim sbuilder As StringBuilder = New StringBuilder()
        Dim Text As SourceText

        While True
            If sbuilder.length = 0 Then
                Console.Write(">>> ")
            Else
                Console.Write("  | ")
            End If


            ln = Console.ReadLine()
            isBlank = String.IsNullOrEmpty(ln)

            If (sbuilder.Length = 0) Then
                If (ln = "exit") Then
                    Exit While
                End If
            End If

            sbuilder.AppendLine(ln)
            outLn = sbuilder.ToString()

            stree = SyntaxTree.Parce(outLn)
            If (isBlank = False And stree.getDiagnostics().Any() = False) Then
                Continue While
            End If

            complatin = New Complation(stree)
            bexpr = complatin.evaluate(variables)
            RPGDiagnostics.AddRange(bexpr._Diagnostics)

            If doShowTree = True Then
                Console.BackgroundColor = ConsoleColor.Green
                Console.ForegroundColor = ConsoleColor.Black

                If stree Is Nothing = False Then
                    stree.ROOT.writeTo(Console.Out)
                End If

                Console.ResetColor()
            End If

            If RPGDiagnostics.Count() > 0 Then
                Text = stree.text

                For Each x As Diagnostics In RPGDiagnostics
                    lineIndex = Text.getLineIndex(x.SPAN.START)
                    lineNumber = lineIndex + 1
                    line = Text.Lines(lineIndex)
                    charPos = x.SPAN.START - Text.Lines(lineIndex).Start + 1


                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine(String.Format(x.ToString(), lineNumber, charPos))
                    Console.ResetColor()

                    For i As Integer = 0 To (ln.Length - 1)
                        If i < x.SPAN.START And i <= x.SPAN.sEND Then
                            Console.ForegroundColor = ConsoleColor.Red
                        End If
                        If i <= x.SPAN.sEND Then
                            Console.ResetColor()
                        End If
                    Next


                    spanPrefix = TextSpan.fromBounds(line.Start, x.SPAN.START)
                    spanError = TextSpan.fromBounds(x.SPAN.START, x.SPAN.sEND)

                    prefix = stree.text.ToString(spanPrefix)
                    _error = stree.text.ToString(spanError)
                    sufix = stree.text.ToString(x.SPAN.sEND)

                    Console.Write("     " + prefix)
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.Write(_error)
                    Console.ResetColor()
                    Console.WriteLine(sufix)
                Next
            Else
                ' show compiled result
                Console.ForegroundColor = ConsoleColor.DarkMagenta
                Console.WriteLine(bexpr.value)
                Console.ResetColor()
            End If

            sbuilder.Clear()
        End While
    End Sub


End Module
