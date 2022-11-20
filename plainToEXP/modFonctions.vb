Imports VB = Microsoft.VisualBasic

Module modFonctions
    Public processus As System.Diagnostics.Process
    Public entree As System.IO.StreamWriter
    Public sortie As System.IO.StreamReader

    Public Sub chargerMoteur(chemin As String)
        Dim chaine As String

        processus = New System.Diagnostics.Process()

        processus.StartInfo.RedirectStandardOutput = True
        processus.StartInfo.UseShellExecute = False
        processus.StartInfo.RedirectStandardInput = True
        processus.StartInfo.CreateNoWindow = True
        processus.StartInfo.WorkingDirectory = My.Application.Info.DirectoryPath
        processus.StartInfo.FileName = chemin
        processus.Start()
        processus.PriorityClass = 64 '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)

        entree = processus.StandardInput
        sortie = processus.StandardOutput

        entree.WriteLine("uci")
        chaine = ""
        While InStr(chaine, "uciok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        entree.WriteLine("setoption name threads value 1")

        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While
    End Sub

    Public Sub dechargerMoteur()
        entree.Close()
        sortie.Close()
        processus.Close()

        entree = Nothing
        sortie = Nothing
        processus = Nothing
    End Sub

    Public Function defragEXP(cheminEXP As String, profMin As Integer, verbose As Boolean, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader) As String
        Dim chaine As String, message As String

        message = ""

        entreeDefrag.WriteLine("exp-defrag")
        entreeDefrag.WriteLine(cheminEXP)
        entreeDefrag.WriteLine(profMin)

        entreeDefrag.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortieDefrag.ReadLine
            If verbose Then
                If InStr(chaine, "Defragging", CompareMethod.Text) > 0 Then
                    Console.WriteLine(Replace(Replace(chaine, ": ", " : "), "). ", ")" & vbCrLf))
                ElseIf InStr(chaine, "Writing") > 0 Then
                    Console.WriteLine(Replace(Replace(Replace(chaine, My.Application.Info.DirectoryPath & "\", ""), "[", ""), "]", ""))
                End If
            End If
            Threading.Thread.Sleep(1)
        End While

        If My.Computer.FileSystem.FileExists(cheminEXP & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminEXP & ".bak")
        End If

        If My.Computer.FileSystem.FileExists(cheminEXP) Then

            entreeDefrag.WriteLine("setoption name Experience File value <empty>")
            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("setoption name Experience File value " & cheminEXP)
            message = ""
            While message = ""
                chaine = sortieDefrag.ReadLine
                If InStr(chaine, "info", CompareMethod.Text) > 0 _
                And InStr(chaine, "string", CompareMethod.Text) > 0 _
                And (InStr(chaine, "collision", CompareMethod.Text) > 0 Or InStr(chaine, "duplicate", CompareMethod.Text) > 0) Then
                    message = Replace(chaine, cheminEXP, nomFichier(cheminEXP)) & vbCrLf
                End If
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While
        End If

        Return message
    End Function

    Public Function defragHypnos(cheminEXE As String, cheminEXP As String, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader) As String
        Dim chaine As String, message As String
        Dim commande As New Process()

        message = ""

        'defrag
        commande.StartInfo.FileName = cheminEXE
        commande.StartInfo.WorkingDirectory = My.Computer.FileSystem.GetParentPath(cheminEXE)
        commande.StartInfo.Arguments = " defrag " & nomFichier(cheminEXP)
        commande.StartInfo.UseShellExecute = False
        commande.Start()
        commande.WaitForExit()

        'nettoyage
        If My.Computer.FileSystem.FileExists(cheminEXP & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminEXP & ".bak")
        End If

        'maj
        If My.Computer.FileSystem.FileExists(cheminEXP) Then

            entreeDefrag.WriteLine("setoption name Experience File value <empty>")
            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("setoption name Experience File value " & cheminEXP)
            message = ""
            While message = ""
                chaine = sortieDefrag.ReadLine
                If InStr(chaine, "info", CompareMethod.Text) > 0 _
                And InStr(chaine, "string", CompareMethod.Text) > 0 _
                And (InStr(chaine, "collision", CompareMethod.Text) > 0 Or InStr(chaine, "duplicate", CompareMethod.Text) > 0) Then
                    message = Replace(chaine, cheminEXP, nomFichier(cheminEXP)) & vbCrLf
                End If
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While
        End If

        Return message
    End Function

    Public Function droite(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Right(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Sub entreeEXP(ByRef tabEXP() As Byte, positionEPD As String, coupUCI As String, scoreCP As Integer, facteur As Integer, prof As Integer, compteur As Integer, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader, ByRef moteur_court As String)
        Dim compteurHEX As String

        'rnbqkbnr/2pppppp/1p5B/p7/3P4/P7/1PP1PPPP/RN1QKBNR b KQkq - 1 3
        '6078880BD90221F8
        'F8(248) 21(33) 02(2) D9(217) 0B(11) 88(136) 78(120) 60(96)
        ' 0       1      2     3       4      5       6       7
        Array.Copy(epdToEXP(entreeDefrag, sortieDefrag, positionEPD), 0, tabEXP, 0, 8)

        'g8h6
        '0 000 111(8) 110(g) 101(6) 111(h)
        '0000(0) 1111(F) 1010(A) 1111(F)
        'AF(175) OF(15)
        ' 8       9
        Array.Copy(moveToEXP(coupUCI), 0, tabEXP, 8, 2)

        If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
            'score cp 936
            '450
            scoreCP = CInt(CInt(scoreCP) * 100 / facteur)
            'hex 0384
            '84(132) 03(3)
            ' a       b
            Array.Copy(scoreToEXP(scoreCP), 0, tabEXP, 10, 2)

            'depth 14
            '0E(14)
            ' c
            tabEXP(12) = prof

            tabEXP(13) = 0 'd
            tabEXP(14) = 0 'e
            tabEXP(15) = 0 'f

            'count 3109
            'hex 0C25
            '25(37) 0C(12)
            ' 0      1
            compteurHEX = Hex(compteur)
            compteurHEX = StrDup(4 - Len(compteurHEX), "0") & compteurHEX
            Array.Copy(inverseurHEX(compteurHEX, 2), 0, tabEXP, 16, 2)

            tabEXP(18) = 0 '2
            tabEXP(19) = 0 '3

            '??? N/A
            tabEXP(20) = 0 '4
            tabEXP(21) = 128 '5

            'next eval N/A
            tabEXP(22) = 0 '6
            tabEXP(23) = 128 '7

            'next x eval N/A
            tabEXP(24) = 0 '8
            tabEXP(25) = 128 '9

            '??? 0
            tabEXP(26) = 0 'a
            tabEXP(27) = 0 'b

            'padding
            tabEXP(28) = 0 'c
            tabEXP(29) = 0 'd
            tabEXP(30) = 0 'e
            tabEXP(31) = 0 'f

        ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
            Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then

            tabEXP(10) = 0 'a
            tabEXP(11) = 0 'b

            'score cp 936
            '450
            scoreCP = CInt(CInt(scoreCP) * 100 / facteur)
            'hex 0384
            '84(132) 03(3) 00(0) 00(0)
            ' c       d     e     f
            Array.Copy(scoreToEXP(scoreCP), 0, tabEXP, 12, 2)
            If scoreCP > 0 Then
                tabEXP(14) = 0 'e
                tabEXP(15) = 0 'f
            Else
                tabEXP(14) = 255 'e
                tabEXP(15) = 255 'f
            End If

            'depth 14
            '0E(14)
            ' 0
            tabEXP(16) = prof

            tabEXP(17) = 0 '1
            tabEXP(18) = 0 '2
            tabEXP(19) = 0 '3

            'count 3109
            'hex 0C25
            '25(37) 0C(12)
            ' 4      5    
            compteurHEX = Hex(compteur)
            compteurHEX = StrDup(4 - Len(compteurHEX), "0") & compteurHEX
            Array.Copy(inverseurHEX(compteurHEX, 2), 0, tabEXP, 20, 2)

            tabEXP(22) = 0 '6
            tabEXP(23) = 0 '7

        End If
    End Sub

    Public Function epdPieces(fen As String) As Integer
        fen = gauche(fen, fen.IndexOf(" "))
        fen = Replace(fen, "/", "")
        fen = Replace(fen, "1", "")
        fen = Replace(fen, "2", "")
        fen = Replace(fen, "3", "")
        fen = Replace(fen, "4", "")
        fen = Replace(fen, "5", "")
        fen = Replace(fen, "6", "")
        fen = Replace(fen, "7", "")
        fen = Replace(fen, "8", "")
        fen = Replace(fen, "r", "1", , , CompareMethod.Text)
        fen = Replace(fen, "n", "1", , , CompareMethod.Text)
        fen = Replace(fen, "b", "1", , , CompareMethod.Text)
        fen = Replace(fen, "q", "1", , , CompareMethod.Text)
        fen = Replace(fen, "k", "1", , , CompareMethod.Text)
        fen = Replace(fen, "p", "1", , , CompareMethod.Text)

        Return Len(fen)
    End Function

    Public Function epdToEXP(entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As Byte()
        Dim key As String

        entreedefrag.WriteLine("position fen " & startpos)

        entreedefrag.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortiedefrag.ReadLine
        End While

        key = Replace(key, "Key: ", "")

        '6078880BD90221F8 =>  F8 21 02  D9 0B  88  78 60
        '                    248 33  2 217 11 136 120 96

        Return inverseurHEX(key, 8)

    End Function

    Public Function gauche(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Left(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function heureFin(depart As Integer, i As Long, max As Long, Optional reprise As Long = 0, Optional formatCourt As Boolean = False) As String
        If formatCourt Then
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dd/MM/yy HH:mm:ss")
        Else
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dddd' 'd' 'MMM' @ 'HH'h'mm'm'ss")
        End If
    End Function

    Public Function inverseurHEX(chaine As String, taille As Integer) As Byte()
        Dim i As Integer, index As Integer, tab(taille - 1) As Byte

        index = 0
        For i = Len(chaine) To 2 Step -2
            tab(index) = Convert.ToInt64(droite(gauche(chaine, i), 2), 16)
            index = index + 1
        Next

        Return tab
    End Function

    Public Function moveToEXP(coup As String, Optional litteral As Boolean = False) As Byte()
        Dim coupBIN As String, i As Integer, cumul As Integer, coupHEX As String

        'g8h6
        '0 000 111(8) 110(g) 101(6) 111(h)

        coupBIN = ""

        If Not litteral Then
            If coup = "e1c1" Then
                coupBIN = "1 100 000 100 000 000" 'equivalent e1a1

            ElseIf coup = "e8c8" Then
                coupBIN = "1 100 111 100 111 000" 'equivalent e8a8

            ElseIf coup = "e1g1" Then
                coupBIN = "1 100 000 100 000 111" 'equivalent e1h1

            ElseIf coup = "e8g8" Then
                coupBIN = "1 100 111 100 111 111" 'equivalent e8h8
            End If
        End If

        If coupBIN = "" Then
            coupBIN = "0 000 "
            If Len(coup) = 5 Then
                Select Case droite(coup, 1)
                    Case "N", "n"
                        coupBIN = "0 001 "
                    Case "B", "b"
                        coupBIN = "0 101 "
                    Case "R", "r"
                        coupBIN = "0 110 "
                    Case "Q", "q"
                        coupBIN = "0 111 "
                End Select
            End If

            'ligne de départ
            If coup.Substring(1, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(1, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(1, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(1, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(1, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(1, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(1, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(1, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne de départ
            If coup.Substring(0, 1) = "a" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(0, 1) = "b" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(0, 1) = "c" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(0, 1) = "d" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(0, 1) = "e" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(0, 1) = "f" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(0, 1) = "g" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(0, 1) = "h" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'ligne d'arrivée
            If coup.Substring(3, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(3, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(3, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(3, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(3, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(3, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(3, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(3, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne d'arrivée
            If coup.Substring(2, 1) = "a" Then
                coupBIN = coupBIN & "000"
            ElseIf coup.Substring(2, 1) = "b" Then
                coupBIN = coupBIN & "001"
            ElseIf coup.Substring(2, 1) = "c" Then
                coupBIN = coupBIN & "010"
            ElseIf coup.Substring(2, 1) = "d" Then
                coupBIN = coupBIN & "011"
            ElseIf coup.Substring(2, 1) = "e" Then
                coupBIN = coupBIN & "100"
            ElseIf coup.Substring(2, 1) = "f" Then
                coupBIN = coupBIN & "101"
            ElseIf coup.Substring(2, 1) = "g" Then
                coupBIN = coupBIN & "110"
            ElseIf coup.Substring(2, 1) = "h" Then
                coupBIN = coupBIN & "111"
            End If
        End If

        '0 000 111(8) 110(g) 101(6) 111(h)
        coupBIN = Replace(coupBIN, " ", "")

        '0000111110101111
        cumul = 0
        For i = 1 To Len(coupBIN)
            cumul = cumul + CInt(gauche(droite(coupBIN, i), 1)) * 2 ^ (i - 1)
        Next
        coupHEX = Hex(cumul)
        coupHEX = StrDup(CInt(Len(coupBIN) / 4 - Len(coupHEX)), "0") & coupHEX

        '0000(0) 1111(F) 1010(A) 1111(F)
        'AF(175) OF(15)

        Return inverseurHEX(coupHEX, 2)
    End Function

    Public Function nomFichier(chemin As String) As String
        Return My.Computer.FileSystem.GetName(chemin)
    End Function

    Public Function scoreToEXP(eval As Integer) As Byte()
        Dim tab(1) As Byte, scoreHEX As String

        scoreHEX = ""
        If eval > 0 Then
            scoreHEX = Hex(eval * 2)
        Else
            scoreHEX = Hex(eval * 2 + 65535)
        End If
        scoreHEX = StrDup(4 - Len(scoreHEX), "0") & scoreHEX

        Return inverseurHEX(scoreHEX, 2)
    End Function

End Module
