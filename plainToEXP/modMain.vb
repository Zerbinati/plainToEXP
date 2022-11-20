Imports System.IO
Imports System.Text

Module modMain

    Public moteurEXP As String
    Public moteur_court As String
    Public fichierEXP As String

    Sub Main()
        Dim lecture As TextReader, fichierPLAIN As String, fichierINI As String
        Dim chaine As String, tabChaine() As String, ligne As String, tabTmp() As String
        Dim positionEPD As String, coup As String, prof As Integer, scoreCP As Integer, facteur As Integer
        Dim compteur As Integer, compteurHEX As String, indexTampon As Integer
        Dim nbPositions As Integer, depart As Integer, cumul As Long, tailleFichier As Long
        Dim tabEXP(0) As Byte, tabTampon(0) As Byte
        Dim reponse As String, nbRejets As Integer, minPieces As Integer
        
        If My.Computer.FileSystem.GetFileInfo(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Documents\Visual Studio 2013\Projects\plainToEXP\plainToEXP\bin\Debug\plainToEXP.exe").LastWriteTime > My.Computer.FileSystem.GetFileInfo(My.Application.Info.AssemblyName & ".exe").LastWriteTime Then
            MsgBox("Il existe une version plus récente de ce programme !", MsgBoxStyle.Information)
            End
        End If

        fichierPLAIN = Replace(Command(), """", "")
        If fichierPLAIN = "" Then
            End
        End If
        Try
            tailleFichier = FileLen(fichierPLAIN)
        Catch ex As Exception
            End
        End Try

        fichierEXP = Replace(fichierPLAIN, "_plain.txt", ".exp")
        If My.Computer.FileSystem.FileExists(fichierEXP) Then
            If MsgBox("The " & nomFichier(fichierEXP) & " file already exists." & vbCrLf & "Do you want to delete it ?", MsgBoxStyle.YesNo) = MsgBoxResult.Yes Then
                My.Computer.FileSystem.DeleteFile(fichierEXP)
            End If
        End If

        'chargement parametres
        moteurEXP = "20T Eman 8.20 x64 BMI2.exe"
        If My.Computer.Name = "PLEXI" Then
            moteurEXP = "20T Eman 8.20 x64 PCNT.exe"
        End If
        fichierINI = My.Computer.Name & ".ini"
        If My.Computer.FileSystem.FileExists(fichierINI) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "moteurEXP"
                                    moteurEXP = Replace(tabTmp(1), """", "")
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurEXP = " & moteurEXP & vbCrLf, False)

        reponse = InputBox("What was the depth of analysis ?", nomFichier(fichierPLAIN), "14")
        If reponse = "" Then
            End
        End If
        prof = CInt(reponse)
        If Not IsNumeric(prof) Then
            End
        End If

        reponse = InputBox("What was the score's factor ?", nomFichier(fichierPLAIN), "208")
        If reponse = "" Then
            End
        End If
        facteur = CInt(reponse)
        If Not IsNumeric(facteur) Then
            End
        End If

        reponse = InputBox("At least how many pieces should the position contain ?", nomFichier(fichierPLAIN), "22")
        If reponse = "" Then
            End
        End If
        minPieces = CInt(reponse)
        If Not IsNumeric(minPieces) Then
            End
        End If

        lecture = My.Computer.FileSystem.OpenTextFileReader(fichierPLAIN, New System.Text.UTF8Encoding(False))

        moteur_court = nomFichier(moteurEXP)

        Console.Write("Loading " & moteur_court & "... ")
        chargerMoteur(moteurEXP)
        Console.WriteLine("OK")

        positionEPD = ""
        coup = ""
        scoreCP = 0
        'stats
        nbPositions = 0
        nbRejets = 0
        depart = Environment.TickCount
        cumul = 0
        indexTampon = 0

        Console.Write("Conversion " & nomFichier(fichierPLAIN) & "... ")
        If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
            ReDim tabEXP(31)
            ReDim tabTampon(319999)
        ElseIf InStr(moteur_court, "eman 6", CompareMethod.Text) = 0 Then
            ReDim tabEXP(23)
            ReDim tabTampon(239999)
            My.Computer.FileSystem.WriteAllText(fichierEXP, "SugaR Experience version 2", False, New System.Text.UTF8Encoding(False))
        End If
        Do
            'fen rnbqkbnr/2pppppp/1p5B/p7/3P4/P7/1PP1PPPP/RN1QKBNR b KQkq - 1 3
            'move g8h6
            'score 936
            'ply 5
            'result 0
            'e
            ligne = lecture.ReadLine()
            cumul = cumul + Len(ligne) + 2

            If gauche(ligne, 3) = "fen" Then
                'fen rnbqkbnr/2pppppp/1p5B/p7/3P4/P7/1PP1PPPP/RN1QKBNR b KQkq - 1 3
                'rnbqkbnr/2pppppp/1p5B/p7/3P4/P7/1PP1PPPP/RN1QKBNR b KQkq - 1 3
                positionEPD = Replace(ligne, "fen ", "")
            ElseIf gauche(ligne, 4) = "move" Then
                'move g8h6
                'g8h6
                coup = Replace(ligne, "move ", "")
            ElseIf gauche(ligne, 5) = "score" Then
                'score 936
                '936
                scoreCP = CInt(Replace(ligne, "score ", ""))
            ElseIf ligne = "e" Then
                If minPieces <= epdPieces(positionEPD) Then 'on limite par le nombre de pièces de la position
                    entreeEXP(tabEXP, positionEPD, coup, scoreCP, facteur, prof, 1, entree, sortie, moteur_court)

                    'stats
                    nbPositions = nbPositions + 1
                    If indexTampon + 24 <= tabTampon.Length Then
                        Array.Copy(tabEXP, 0, tabTampon, indexTampon, 24)
                        indexTampon = indexTampon + 24
                    Else
                        'on vide le tampon dans le fichierEXP
                        My.Computer.FileSystem.WriteAllBytes(fichierEXP, tabTampon, True)
                        Array.Clear(tabTampon, 0, tabTampon.Length)
                        indexTampon = 0
                    End If
                Else
                    nbRejets = nbRejets + 1
                End If
                
                If (nbPositions + nbRejets) Mod 50000 = 0 Then
                    Console.Clear()
                    Console.Title = My.Computer.Name & " : Conversion @ " & Format(cumul / tailleFichier, "0%") & " (" & heureFin(depart, cumul, tailleFichier, , True) & ")"
                    Console.WriteLine("Moves  : " & Trim(Format(nbPositions, "# ### ### ##0")))
                    Console.WriteLine("Reject : " & Trim(Format(nbRejets, "# ### ### ##0")))
                    Console.WriteLine("Rate   : " & Trim(Format(nbPositions / (Environment.TickCount - depart), "# ### ### ##0 pos/ms")))
                End If

                'nettoyage
                positionEPD = ""
                coup = ""
                scoreCP = 0
                compteur = 0
                compteurHEX = ""
                Array.Clear(tabEXP, 0, tabEXP.Length)
            End If
        Loop Until ligne Is Nothing
        lecture.Close()

        If indexTampon > 0 Then
            'on vide le tampon dans le fichierEXP
            ReDim Preserve tabTampon(indexTampon - 1)
            My.Computer.FileSystem.WriteAllBytes(fichierEXP, tabTampon, True)
        End If
        Console.WriteLine("OK")

        Console.Clear()
        Console.Title = My.Computer.Name & " : Conversion @ " & Format(cumul / tailleFichier, "0%")
        Console.WriteLine("Moves  : " & Trim(Format(nbPositions, "# ### ### ##0")))
        Console.WriteLine("Reject : " & Trim(Format(nbRejets, "# ### ### ##0")))
        Console.WriteLine("Rate   : " & Trim(Format(nbPositions / (Environment.TickCount - depart), "# ### ### ##0")) & " moves/ms")

        Console.WriteLine("Defragging...")
        If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
            Console.WriteLine(defragEXP(fichierEXP, 1, False, entree, sortie))
        ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 Then
            Console.WriteLine(defragEXP(fichierEXP, 4, False, entree, sortie))
        ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
            Console.WriteLine(defragHypnos(moteurEXP, fichierEXP, entree, sortie))
        End If

        dechargerMoteur()

        Console.WriteLine("Press ENTER to close the window.")
        Console.ReadLine()

    End Sub

End Module
