  Subroutine CalcAbsRI()
! Caculates RI and TI using the NR and BR values given in the input ENSDF file
! TK - Only the first data set will be processed
  Use  GabsMod
  Implicit None
  Character(len=*), Parameter                    :: Subr = 'CalcAbsRI'
  Integer(kind=4)                                :: iG = 0
  Character(len=80)                              :: Card
  Character(len=132)                             :: Line
  Integer(kind=4)                                :: iDS
! NR * BR -----------------------------------------------------------
  Real(kind=4)                                   ::   wN            ! NR * BR
  Real(kind=4)                                   ::  wDN            ! UNC 
  Character(len=10)                              ::  cwN = ' '
  Character(len=2)                               :: cwDN = ' '
  Character(len=2)                               :: owDN = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'

  Character(len=2)                               :: cDX = ' '
  Character(len=20)                              :: cDateTime = ' '
  Integer(kind=4), External                      :: TypStr
  Character(len=20), External                    :: NS_DateTime 
  Call LoadENSDF()
  Write (DefOut,'(a)') '  '
  Write (DefOut,'(a)') '  Running in Calculation mode * * * * *'
! Scan N record for NR and BR in the input file ===================================================
  Write (DefOut,'(a)') '  Output file opened: '//Trim(NewFile)
  Write (DefOut,'(a)') '  Report file:        '//Trim(RptFile)                                                                
  Write (RptLun,'(a)') '  * * * '//Trim(Version)//' Report file  * * *'    
  Write (RptLun,'(a)') '  Calculating %RI using NR and BR from the ENSDF input file'
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
!   Calculate NR*BR
    wN = NR(iDS)*BR(iDS)
    wDN = 0.0
!   DNR & DBR blank
    If (cDNR(iDS) == ' ' .and. cDBR(iDS) == ' ') Then
      wDN = 0.0
      Call CNVU2S(wN, 0.03*wN, cwN, 10, cwDN, 2)
      cWDN = ' '
      owDN = '  '
!   DNR blank, DBR numerical
    Else If (cDNR(iDS) == ' ' .and. TypStr(cDBR(iDS)) == 1) Then
      wDN = NR(iDS)*DBR(iDS)    
      Call CNVU2S(wN, wDN, cwN, 10, cwDN, 2)
      owDN = 'SU'
!   DNR numerical, DBR blank
    Else If (TypStr(cDNR(iDS)) == 1 .and. cDBR(iDS) == ' ') Then
      wDN = BR(iDS)*DNR(iDS)    
      Call CNVU2S(wN, wDN, cwN, 10, cwDN, 2)
!   DNR & DBR numerical
    Else If (TypStr(cDNR(iDS)) == 1 .and. TypStr(cDBR(iDS)) == 1) Then
      wDN = wN * sqrt( (DNR(iDS)/NR(iDS))**2 + (DBR(iDS)/BR(iDS))**2)
      Call CNVU2S(wN, wDN, cwN, 10, cwDN, 2)
      owDN = 'SU'
!   DNR or DBR appriximate
    Else If (cDNR(iDS) == 'AP' .or. cDBR(iDS) == 'AP') Then
      wDN = 0.0
      Call CNVU2S(wN, 0.03*wN, cwN, 10, cwDN, 2)
      cwDN = 'AP'
      owDN = 'AP'
!   Trap erors
    Else
      Write (DefOut,'(a)') '  <F> Programming error in '//Subr// &
         ' NR='//Trim(cNR(iDS))//'('//Trim(cDNR(iDS))//')'// &
         ' BR='//Trim(cBR(iDS))//'('//Trim(cDBR(iDS))//')'
      Stop
    End If
    Call LbSup(cNR(iDS))
    Line = '  Normalization: NR = '//Trim(cNR(iDS))
    If (cDNR(iDS) /= ' ') Then
      Line = Trim(Line)//'('//Trim(cDNR(iDS))//')'
    End If
    Call LbSup(cBR(iDS))
    Line = Trim(Line)//'   BR = '//Trim(cBR(iDS))
    If (cDBR(iDS) /= ' ') Then
      Line = Trim(Line)//'('//Trim(cDBR(iDS))//')'
    End If
      Call LbSup(cwN)
      Call LbSup(cwDN)
      Line = trim(Line)//'   NR*BR = '//Trim(cwN) 
    If (cwDN /= ' ') Then
      Line = Trim(Line)//'('//Trim(cwDN)//')'
    End If
    Write (DefOut,'(a)') ' '
    Write (DefOut,'(a)') '  Data set: '//Trim(DSID(iDS))
    Write (DefOut,'(a)') Trim (line)
    Write (RptLun,'(a)') ' '
    Write (RptLun,'(a)') '  Data set: '//Trim(DSID(iDS))
    Write (RptLun,'(a)') Trim (line)
!   Calculate normalised intensities ==================================================
    LoopGamma : Do iG = 1, DS_nCards(iDS)
      Card = DS_Cards(iDS,iG)
!     normalization and new PN records
      If (Card(6:9) == '  N ') Then
        Write (NewLun,'(a)') Card
        Write (NewLun,'(a)') Card(1:5)//' PN                                                                     4  '
!     Old PN record, trush it
      Else If (Card(6:9) == ' PN ') Then
!     Gamma record
      Else If (Card(6:9) == '  G ') Then
!       RI - relative photon intensity -------------------------------------------
         cRI = Card(22:29)
        Call LbSup(cRI)
        cDRI = Card(30:31)
        Call LbSup(cDRI)
!       Process only non-blank RI
        If (cRI /= ' ') Then
!         RI a FORTRAN number
          If (TypStr(Trim(cRI)) == -2 .or. TypStr(Trim(cRI)) == 1) Then
            Call CNVS2U(cRI, cDRI, RI, DRI)
!           Both DRI and DN are blank
            If (cDRI == ' ' .and. cwDN == ' ') Then
              RI = RI * wN                       
              Call CNVU2S(RI, RI*0.03, cRI, 8, cDRI, 2)
              cDRI = ' '
!           DRI blank, DN numeric
            Else If (cDRI == ' ' .and. TypStr(Trim(cwDN)) == 1) Then
              RI = RI * wN                       
              Call CNVU2S(RI, RI*wDN/wN, cRI, 8, cDRI, 2)
!           DRI blank, DN approximate (AP)
            Else If (cDRI == ' ' .and. cwDN == 'AP') Then
              RI = RI * wN                       
              Call CNVU2S(RI, RI*wDN/wN, cRI, 8, cDRI, 2)
              cDRI = cwDN
!           DRI numeric, DN blank
            Else If (TypStr(Trim(cDRI)) == 1 .and. cwDN == ' ') Then
              DRI = DRI * wN 
               RI =  RI * wN                       
              Call CNVU2S(RI, DRI, cRI, 8, cDRI, 2)
!           DRI numeric, DN approximate (AP)
            Else If (TypStr(Trim(cDRI)) == 1 .and. cwDN == 'AP') Then
              DRI = 0.0 
               RI =  RI * wN                       
              Call CNVU2S(RI, 0.03*RI, cRI, 8, cDRI, 2)
              cDRI = 'AP'
!           Both DRI and DN are numeric
            Else If (TypStr(Trim(cDRI)) == 1 .and. TypStr(cwDN) == 1) Then
              DRI = RI*wN * Sqrt( (wDN/wN)**2 + (DRI/RI)**2 ) 
               RI = RI * wN                       
              Call CNVU2S(RI, DRI, cRI, 8, cDRI, 2)
!           DRI not numeric
            Else If (TypStr(Trim(cDRI)) /= 1 .and. TypStr(cwDN) == 1) Then
               RI = RI * wN                       
              cDRI = cDRI ! no change
              If (wDN > 0.0 .and. wN > 0.0) Then
                Call CNVU2S(RI, RI*wDN/wN, cRI, 8, cDX, 2)
              Else
                Call CNVU2S(RI, RI*0.03, cRI, 8, cDX, 2)
              End If
              If (cDRI == 'AP') cDX = 'AP'
!           DN not numeric
            Else If (TypStr(Trim(cDRI)) == 1 .and. TypStr(cwDN) /= 1) Then
               RI = RI * wN                       
              DRI = DRI*wN
              Call CNVU2S(RI, DRI, cRI, 8, cDRI, 2)
              cDRI = cwDN 
!           Both DRI and DN not numeric
            Else If (TypStr(Trim(cDRI)) /= 1 .and. TypStr(cwDN) /= 1) Then
               RI = RI * wN                       
              cDRI = cwDN 
            End If
            Call LbSup(cRI)
            Call LbSup(cDRI)
          End If
        End If ! non-blank RI
!       TI - total transition intensity -------------------------------------------
         cTI = Card(65:74)
        cDTI = Card(75:76)
!       Process only non-blank TI
        If (cTI /= ' ') Then
!         TI a FORTRAN number
          If (TypStr(Trim(cTI)) == -2 .or. TypStr(Trim(cTI)) == 1) Then
            Call CNVS2U(cTI, cDRI, TI, DRI)
!           Both DTI and DN are blank
            If (cDTI == ' ' .and. cwDN == ' ') Then
              TI = TI * wN                       
              Call CNVU2S(TI, TI*0.03, cTI, 8, cDTI, 2)
              cDTI = ' '
!           DTI blank, DN numeric
            Else If (cDTI == ' ' .and. TypStr(Trim(cwDN)) == 1) Then
              TI = TI * wN                       
              Call CNVU2S(TI, TI*wDN/wN, cTI, 8, cDTI, 2)
!           DTI blank, DN approximate (AP)
            Else If (cDTI == ' ' .and. cwDN == 'AP') Then
              TI = TI * wN                       
              Call CNVU2S(TI, TI*wDN/wN, cTI, 8, cDTI, 2)
              cDTI = cwDN
!           DTI numeric, DN blank
            Else If (TypStr(Trim(cDTI)) == 1 .and. cwDN == ' ') Then
              DTI = DTI * wN 
               TI =  TI * wN                       
              Call CNVU2S(TI, DTI, cTI, 8, cDTI, 2)
!           DTI numeric, DN approximate (AP)
            Else If (TypStr(Trim(cDTI)) == 1 .and. cwDN == 'AP') Then
              DTI = 0.0 
               TI =  TI * wN                       
              Call CNVU2S(TI, 0.03*TI, cTI, 8, cDTI, 2)
              cDTI = 'AP'
!           Both DTI and DN are numeric
            Else If (TypStr(Trim(cDTI)) == 1 .and. TypStr(cwDN) == 1) Then
              DTI = TI * wN * Sqrt( (wDN/wN)**2 + (DTI/TI)**2 ) 
               TI = TI * wN                       
              Call CNVU2S(TI, DTI, cTI, 8, cDTI, 2)
!           DTI not numeric
            Else If (TypStr(Trim(cDTI)) /= 1 .and. TypStr(cwDN) == 1) Then
               TI = TI * wN                       
              cDTI = cDTI ! no change
              If (wDN > 0.0 .and. wN > 0.0) Then
                Call CNVU2S(TI, TI*wDN/wN, cTI, 8, cDX, 2)
              Else
                Call CNVU2S(TI, TI*0.03, cTI, 8, cDX, 2)
              End If
              If (cDTI == 'AP') cDX = 'AP'
!           DN not numeric
            Else If (TypStr(Trim(cDTI)) == 1 .and. TypStr(cwDN) /= 1) Then
               TI = TI * wN                       
              DTI = DTI*wN
              Call CNVU2S(TI, DTI, cTI, 8, cDTI, 2)
              cDTI = cwDN 
!           Both DTI and DN not numeric
            Else If (TypStr(Trim(cDTI)) /= 1 .and. TypStr(cwDN) /= 1) Then
               TI = TI * wN                       
              cDTI = cwDN 
            End If
            Call LbSup(cTI)
            Call LbSup(cDTI)
          End If
        End If ! Non-blank TI
!       Save Gamma record 
        Write (NewLun,'(a)') Card
!       Add new Gamma continuation record with  "%IG="
        If (cRI /= ' ') Then
          Card(6:) = '2 G %IG='//Trim(cRI)
          If (cDRI /= ' ') Card = Trim(Card)//' '//Trim(cDRI)
          Write (NewLun,'(a)') Card
          Line= '  E='//DS_Cards(iDS,iG)(10:21)//'   %IG='//Trim(cRI)
          If (cDRI /= ' ') Line = Trim(Line)//' '//Trim(cDRI)
         Write(RptLun,'(a)') Trim(Line)
        End If
!     Remove Gamma continuation record with  "%IG=" 
      Else If (Card(6:13) == '2 G %IG=') Then
          Card = Card
!     Any other card, just save it
      Else
        Write (NewLun,'(a)') Card
      End If
    End Do LoopGamma
  End Do LoopDS
  Return
  End