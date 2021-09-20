  Subroutine CalcAbsIG()
! Caculates %IG=RI*NR*BR or %IG=TI/(1+CC)*NR*BR if TI is known values given in the input ENSDF file
! TK - Only the first data set will be processed
  Use  GabsMod
  Implicit None
  Character(len=*), Parameter                    :: Subr = 'CalcAbsIG'
  Character(len=80)                              :: Card, SGCard
  Character(len=132)                             :: Line,NRline, NTline,BRline,Nline
  Integer(kind=4)                                :: iDS = 0
  Integer(kind=4)                                :: iC = 0
  Integer(kind=4)                                :: IoStat = 0, dPos = 0
! NR * BR -----------------------------------------------------------
  Real(kind=4)                                   ::   wN            ! NR * BR
  Real(kind=4)                                   ::  wDN            ! UNC 
  Character(len=10)                              ::  cwN = ' '
  Character(len=2)                               :: cwDN = ' '
  Character(len=2)                               :: owDN = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'

  Character(len=20)                              :: cDateTime = ' '
  Real(kind=4)                                   :: DCC_L, DCC_H
  Character(len=20)                              :: NsrKey = ' '
  Character(len=6)                               :: wcDCC = ' '
  Integer(kind=4), External                      :: TypStr
  Integer(kind=4), External                      :: LenStr
  Character(len=20), External                    :: NS_DateTime 
  Integer(kind=4), External                      :: ScanQuantOpVal
  Call LoadENSDF()
  Write (DefOut,'(a)') '  '
  Write (DefOut,'(a)') '  Running in Calculation mode * * * * *'
! Scan N record for NR and BR in the input file ===================================================
  Write (DefOut,'(a)') '  Output file opened: '//Trim(NewFile)
  Write (DefOut,'(a)') '  Report file:        '//Trim(RptFile)                                                                
  Write (RptLun,'(a)') '  * * * '//Trim(Version)//' Report file  * * *'    
  Write (RptLun,'(a)') '  Calculating %RI using NR and BR from the ENSDF input file  * * * *'
  Write (RptLun,'(a)') ' '
!.Write date                                                        
  cdateTime = NS_DateTime()                                     
  Write (RptLun,'(a,i2.2,a,i2.2,a)') '  Current date: '//Trim(cDateTime)                                
!.Input file
  Write (RptLun,'(a)') '  ENSDF input file: '//Trim(InpFile)           
  LoopDS : Do iDS = 1, nDS
    If (cNR(iDS) == ' ') Then
      Write (DefOut,'(a)') '  <F> blank NR field not allowed'
      Stop
    End If
!...Calculate NR*BR ===========================================================
     wN = NR(iDS)*BR(iDS)
!   DNR & DBR blank -----------------------------------------------------------
    If (cDNR(iDS) == ' ' .and. cDBR(iDS) == ' ') Then
      Call CNVU2S_n(wN, 0.03*wN, cwN, 10, cwDN, 2)
      cWDN = ' '
      owDN = '  '
!   DNR blank, DBR numerical -------------------------------------------------
    Else If (cDNR(iDS) == ' ' .and. TypStr(Trim(cDBR(iDS))) == 1) Then
      wDN = wN*(DBR(iDS)/BR(iDS))
      Call CNVU2S_n(wN, wDN, cwN, 10, cwDN, 2)
      owDN = 'SU'
!   DNR numerical, DBR blank -------------------------------------------------
    Else If (TypStr(Trim(cDNR(iDS))) == 1 .and. cDBR(iDS) == ' ') Then
      wDN = wN*(DNR(iDS)/NR(iDS))
      Call CNVU2S_n(wN, wDN, cwN, 10, cwDN, 2)
!   DNR & DBR numerical ------------------------------------------------------
    Else If (TypStr(Trim(cDNR(iDS))) == 1 .and. TypStr(Trim(cDBR(iDS))) == 1) Then
      wDN = wN * Sqrt( (DNR(iDS)/NR(iDS))**2 + (DBR(iDS)/BR(iDS))**2 )
      Call CNVU2S_n(wN, wDN, cwN, 10, cwDN, 2)
      owDN = 'SU'
!   DNR or DBR appriximate ---------------------------------------------------
    Else If (cDNR(iDS) == 'AP' .or. cDBR(iDS) == 'AP') Then
      Call CNVU2S_n(wN,  0.03*wN, cwN, 10, cwDN, 2)
      cwDN = 'AP'
      owDN = 'AP'
!   Trap erors
    Else
      Write (DefOut,'(a)') '  <F> Programming error in '//Subr// &
         ' NR='//Trim(cNR(iDS))//'('//Trim(cDNR(iDS))//')'// &
         ' BR='//Trim(cBR(iDS))//'('//Trim(cDBR(iDS))//')'
      Stop
    End If
    Call LbSup(cwDN)
!...Report NR, NT, BR and N
    NRline = ' '
    NTline = ' '
    BRline = ' '
    Select Case (DecType(iDS))
!   =================================================================
!...B-, B+ or EC data sets 
!   =================================================================
    Case (4,5,8,9,10,11,12)
!.....NR ------------------------------------------------------------
      Call LbSup(cNR(iDS))
      NRline = '  Normalization:    NR= '
      If (cNR(iDS) /= ' ') Then
        NRline = Trim(NRline)//Trim(cNR(iDS))
        If (cDNR(iDS) /= ' ') Then
          NRline = Trim(NRLine)//'('//Trim(cDNR(iDS))//')'
        End If
      Else
        NRline = Trim(NRline)//' 1 [assumed]'
      End If
!.....NT -----------------------------------------------------------
      If (cNT(iDS) /= ' ') Then
        Call LbSup(cNT(iDS))
        NTline = '                    NT= '
        NTline = Trim(NTline)//Trim(cNT(iDS))
        If (cDNT(iDS) /= ' ') Then
          NTline = Trim(NTLine)//'('//Trim(cDNT(iDS))//')'
        Else
          NTline = Trim(NTline)//' 1 [assumed]'
        End If
      End If
!.....BR ----------------------------------------------------------
      Call LbSup(cBR(iDS))
      BRline = '                    BR= '
      If (cBR(iDS) /= ' ') Then
        BRline = Trim(BRline)//Trim(cBR(iDS))
        If (cDBR(iDS) /= ' ') Then
          BRline = Trim(BRline)//'('//Trim(cDBR(iDS))//')'
        End If
      Else
        BRline = Trim(BRline)//'1 [assumed]'
      End If
!.....N factor ---------------------------------------------------
      Call LbSup(cwN)
      Nline  = '               N=NR*BR= '
      Call LbSup(cwDN)
      Nline = trim(Nline)//Trim(cwN) 
      If (cwDN /= ' ') Then
        Nline = Trim(Nline)//'('//Trim(cwDN)//')'
      End If
!.....Header lines -----------------------------------------------      
      Write (DefOut,'(a)') ' '
      Write (RptLun,'(a)') ' '
      Write (DefOut,'(a)') '  Data set: '//Trim(DSID(iDS))
      Write (RptLun,'(a)') '  Data set: '//Trim(DSID(iDS))
      If (NRline /= ' ') Then
        Write (DefOut,'(a)') Trim (NRline)
        Write (RptLun,'(a)') Trim (NRline)
      End If
      If (BRline /= ' ') Then
        Write (DefOut,'(a)') Trim (BRline)
        Write (RptLun,'(a)') Trim (BRline)
      End If
      If (NTline /= ' ') Then
        Write (DefOut,'(a)') Trim (NTline)
        Write (RptLun,'(a)') Trim (NTline)
      End If
!       Write (DefOut,'(a)') Trim (NBline)
!       Write (RptLun,'(a)') Trim (NBline)
      If (Nline /= ' ') Then
        Write (DefOut,'(a)') Trim (Nline)
        Write (RptLun,'(a)') Trim (Nline)
      End If
    Case Default
    End Select
!   Calculate normalised intensities ==================================================
    iC = 0
    LoopGamma : Do While (iC < DS_nCards(iDS))
      iC = iC+1
      Card = DS_Cards(iDS,iC)
      Call UPSTR(Card(6:9))
!     normalization and new PN records
      If (Card(6:9) == '  N ') Then
        Card(6:9) = DS_Cards(iDS,iC)(6:9)
        Write (NewLun,'(a)') Card
        Write (NewLun,'(a)') Card(1:5)//' PN                                                                     4  '
!     Old PN record, trush it
      Else If (Card(6:9) == ' PN ') Then
!     Gamma record
      Else If (Card(6:9) == '  G ') Then
        cIG = ' '
        cDIG = ' '
!.......=======================================================================
!       RI - relative photon intensity is given %IG = N* RI
!.......=======================================================================
         cRI = Card(22:29)
        Call LbSup(cRI)
        cDRI = Card(30:31)
        Call LbSup(cDRI)
!       Process only non-blank RI
        If (cRI /= ' ') Then
!         RI must be a FORTRAN number
          If (TypStr(Trim(cRI)) == -2 .or. TypStr(Trim(cRI)) == 1) Then
            Call CNVS2U(cRI, cDRI, RI, DRI)
!           Both DRI and DN are blank
            If (cDRI == ' ' .and. cwDN == ' ') Then
              IG = RI * wN                       
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = ' '
!           DRI blank, DN numeric
            Else If (cDRI == ' ' .and. TypStr(Trim(cwDN)) == 1) Then
               IG = RI * wN  
              DIG = RI * wDN
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DRI blank, DN approximate (AP)
            Else If (cDRI == ' ' .and. cwDN == 'AP') Then
              IG = RI * wN                       
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = cwDN
!           DRI numeric, DN blank
            Else If (TypStr(Trim(cDRI)) == 1 .and. cwDN == ' ') Then
               IG =  RI * wN                       
              DIG = DRI * wN 
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DRI numeric, DN approximate (AP)
            Else If (TypStr(Trim(cDRI)) == 1 .and. cwDN == 'AP') Then
               IG =  RI * wN                       
              DIG = 0.0 
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = 'AP'
!           Both DRI and DN are numeric
            Else If (TypStr(Trim(cDRI)) == 1 .and. TypStr(Trim(cwDN)) == 1) Then
               IG = RI * wN                       
              DIG = RI*wN * Sqrt( (wDN/wN)**2 + (DRI/RI)**2 ) 
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DRI not numeric DN numeric
            Else If (TypStr(Trim(cDRI)) /= 1 .and. TypStr(Trim(cwDN)) == 1) Then
               IG = RI * wN                       
              cDIG = cDRI ! retain DRI (limit, AP)
              If (wDN > 0.0 .and. wN > 0.0) Then
                Call CNVU2S_n(IG, IG*wDN/wN, cIG, 8, cDIG, 2)
              Else
                Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              End If
              If (cDRI == 'AP') cDIG = 'AP'
!           DN not numeric
            Else If (TypStr(Trim(cDRI)) == 1 .and. TypStr(Trim(cwDN)) /= 1) Then
               IG =  RI * wN                       
              DIG = DRI* wN
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
              cDRI = cwDN 
!           Both DRI and DN not numeric
            Else If (TypStr(Trim(cDRI)) /= 1 .and. TypStr(Trim(cwDN)) /= 1) Then
                IG = RI * wN                       
              cDIG = cwDN 
            End If
            Call LbSup(cRI)
            Call LbSup(cDRI)
          End If
        End If ! non-blank RI
!.......=======================================================================
!       TI - Total intensity is given %IG = N* TI/(1+CC)
!.......=======================================================================
         cTI = Card(65:74)
        cDTI = Card(75:76)
         cCC = Card(56:62)
        cDCC = Card(63:64)
!.......Look for CC on S_G cards if no CC given on G-card 
        LoopSG : Do While (cCC == ' ') 
          iC = iC+1
          If (iC > DS_nCards(iDS)) Exit LoopSG
          SGCard = DS_Cards(iDS,iC)
          Call UPSTR(SGCard)
          If (SGCard(6:9) == '  G ' .or. SGCard(6:9) /= 'S G ' .or. SGCard(7:9) /= ' G ') Then
            iC = iC-1
            Exit LoopSG
          End If
          If (SGCard(6:9) == 'S G ') Then
            IoStat = ScanQuantOpVal(SGCard(10:), 'CC', cCC, wcDCC, CC, DCC_L, DCC_H, NsrKey, .FALSE.)
            If (IoStat /= +1 ) Cycle LoopSG
            If (DCC_L /= DCC_H) Then
              Write (DefOut,'(a)') '      <F> CC must have blank or symmetric uncertainty!'
              Write (DefOut,'(i8,a)')   iC,' Card: '//Trim(SGCard)
              Stop
            End If
            cDCC = wcDCC(1:2)
            Exit LoopSG
          End If
        End Do LoopSG
        Call CNVS2U(cCC, cDCC, CC, DCC)
!.......Process only non-blank TI
        If (cTI /= ' ') Then
!.........TI mus be a FORTRAN number
          If (TypStr(Trim(cTI)) == -2 .or. TypStr(Trim(cTI)) == 1) Then
            Call CNVS2U(cTI, cDRI, TI, DRI)
!           Both DTI and DN are blank
            If (cDTI == ' ' .and. cwDN == ' ') Then
              IG = wN * TI/(1.0+CC)                      
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = ' '
!           DTI blank, DN numeric
            Else If (cDTI == ' ' .and. TypStr(Trim(cwDN)) == 1) Then
               IG = wN * TI/(1.0+CC)  
              DIG = Sqrt( ((wDN*TI)/(1.0+CC))**2 + ((DTI*wN)/(1.0+CC))**2 + ((-DCC*wN*TI)/(1.0+CC)**2)**2)
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DTI blank, DN approximate (AP)
            Else If (cDTI == ' ' .and. cwDN == 'AP') Then
               IG = wN * TI/(1.0+CC)  
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = cwDN
              DIG = 0.0
!           DTI numeric, DN blank
            Else If (TypStr(Trim(cDTI)) == 1 .and. cwDN == ' ') Then
               IG = wN * TI/(1.0+CC)  
              DIG = Sqrt( ((wDN*TI)/(1.0+CC))**2 + ((DTI*wN)/(1.0+CC))**2 + ((-DCC*wN*TI)/(1.0+CC)**2)**2)
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DTI numeric, DN approximate (AP)
            Else If (TypStr(Trim(cDTI)) == 1 .and. cwDN == 'AP') Then
               IG = wN * TI/(1.0+CC)  
              DIG = 0.0
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = 'AP'
!           Both DTI and DN are numeric
            Else If (TypStr(Trim(cDTI)) == 1 .and. TypStr(Trim(cwDN)) == 1) Then
               IG = wN * TI/(1.0+CC)  
              DIG = Sqrt( ((wDN*TI)/(1.0+CC))**2 + ((DTI*wN)/(1.0+CC))**2 + ((-DCC*wN*TI)/(1.0+CC)**2)**2)
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DTI not numeric DN numeric
            Else If (TypStr(Trim(cDTI)) /= 1 .and. TypStr(Trim(cwDN)) == 1) Then
               IG = wN * TI/(1.0+CC)  
              DIG = Sqrt( ((wDN*TI)/(1.0+CC))**2 + ((DTI*wN)/(1.0+CC))**2 + ((-DCC*wN*TI)/(1.0+CC)**2)**2)
              Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
!           DTI numeric DN not numeric
            Else If (TypStr(Trim(cDTI)) == 1 .and. TypStr(Trim(cwDN)) /= 1) Then
               IG = wN * TI/(1.0+CC)  
              DIG = Sqrt( ((wDN*TI)/(1.0+CC))**2 + ((DTI*wN)/(1.0+CC))**2 + ((-DCC*wN*TI)/(1.0+CC)**2)**2)
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = cwDN 
!           Both DTI and DN not numeric
            Else If (TypStr(Trim(cDTI)) /= 1 .and. TypStr(Trim(cwDN)) /= 1) Then
               IG = wN * TI/(1.0+CC)  
              Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
              cDIG = cwDN
            End If
            Call LbSup(cIG)
            Call LbSup(cDIG)
          End If
        End If ! Non-blank TI
!       Save Gamma record 
        Card(6:9) = DS_Cards(iDS,iC)(6:9)
        Write (NewLun,'(a)') Card
!       Add new Gamma continuation record with  "%IG="
        If (cIG /= ' ') Then
          Line= '  E='//DS_Cards(iDS,iC)(10:21)//'  %IG='//Trim(cIG)
          If (cDIG /= ' ') Line = Trim(Line)//'('//Trim(cDIG)//')'
          Write(RptLun,'(a)') Trim(Line)
          Call LbSup(cIG)
          Call LbSup(cDIG)
!         BLANK
          Select Case (TypStr(cDIG))
          Case (0)
            Card(6:) = '2 G %IG='//Trim(cIG)
!         ALPHA
          Case (2)
            Card(6:) = '2 G %IG '//Trim(cDIG)//' '//Trim(cIG)
!         NUMERIC 
          Case Default
            Card(6:) = '2 G %IG='//Trim(cIG)//' '//Trim(cDIG)
          End Select
          Write (NewLun,'(a)') Card
        End If
!     Remove Gamma continuation record with  "%IG=" 
      Else If (Card(6:13) == '2 G %IG=') Then
          Card = Card
!     Any other card, just save it
      Else
        Card(6:9) = DS_Cards(iDS,iC)(6:9)
        Write (NewLun,'(a)') Card
      End If
    End Do LoopGamma
  End Do LoopDS
  Return
  End