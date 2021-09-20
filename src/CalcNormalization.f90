  Subroutine  CalcNormalization()
  Use  GabsMod
  Implicit None
  Character(len=*), Parameter                   :: Subr='CalcNormalization'
  Character(len=80)                             :: Card, SGCard
  Character(len=1)                              :: X                                
  Integer(kind=4)                               :: iC = 0
  Integer(kind=4)                               :: iDS=0  ! Number of data sets; no more than 3
  Integer(kind=4)                               :: nCard= 0
  Integer(kind=4)                               :: IoStat= 0
  Real(kind=4)                                  :: DCC_L, DCC_H
  Character(len=20)                              :: NsrKey = ' '
  Character(len=6)                              :: wcDCC = ' '
  Integer(kind=4), External                     :: TypStr
  Integer(kind=4), External                      :: ScanQuantOpVal
! =======================================================================================
! STEP#1 Loads ENSDF file into memory, Opens Report and Output ENSDF file
! =======================================================================================
  Write (DefOut,'(a)') '  '
  Write (DefOut,'(a)') '  Calculating new normalization factor * * * * *'
! NR field must be blank
  Do iDS = 1, nDS
    If (cNR(iDS) /= ' ' .or. cDNR(iDS)  /= ' ') Then
      Write (DefOut,'(a)') '      <F> To Calculate new normalization factor(s) the NR field(s) must be blank'
      Stop
    End If
  End Do
! BR field - Single data set
  If (nDS == 1) Then
    If (cBR(1) == ' ') Then
      cBR(1) = '1.0'
       BR(1) = 1.0
      DBR(1) = 0.0
      Write (DefOut,'(a)') '      <I> BR was blank, assumed to be 1.0'
      Write (RptLun,'(a)') '      <I> BR was blank, assumed to be 1.0'
    End If
! Multiple data set - must be blank
  Else
    Do iDS = 1, nDS
      If (cBR(iDS) /= ' ' .or. cDBR(iDS)  /= ' ') Then
        Write (DefOut,'(a)') '      <F> To Calculate new normalization factor the BR must be blank'
        Stop
      End If
    End Do
  End If
!
  iDS=1                                                               
  nCard = 0
! =======================================================================================
! STEP#2 Scans relevant ENSDF records
! =======================================================================================
  Write (RptLun,'(a)')  '  Calculating new normalization factor * * * * *'
  Write (RptLun,'(a)')  '  New ENSDF file:   '//Trim(NewFile)                                                                
  LoopDS : Do iDS = 1, nDS
    nCG(iDS) = 0
    Write (DefOut,'(a)') ' '
    Write (DefOut,'(a)') '  Data set: '//Trim(DSID(iDS))
    Write (RptLun,'(a)') ' '
    Write (RptLun,'(a)') '  Data set: '//Trim(DSID(iDS))
!    Write (DefOut,'(a)') ' '
!    Write (RptLun,'(a)') ' '
    Write (DefOut,'(a)') '  Transitions used for normalization:'
    Write (RptLun,'(a)') '  Transitions used for normalization:'
    nGNorm = 0
    iC = 0
    LoopCGamma : Do While (iC < DS_nCards(iDS))
      iC = iC+1
      Card = DS_Cards(iDS,iC)
      Select Case (Card(6:9))
!.....Normalization record --------------------------------------------------                           
      Case ('  N ') 
        If (cNB(iDS) == ' ') Then
            cNB(iDS) = '1.0'
             NB(iDS) = 1.0
        Else
          Call CNVS2U(cNB(iDS), cDNB(iDS), NB(iDS), DNB(iDS))
        End If
!.......Ground state direct feeding from continuation N-card
        If (cIGS(iDS) == ' ') Then
             IGS(iDS) = 0.0
            DIGS(iDS) = 0.0
        Else
          Call CNVS2U(cIGS(iDS), cDIGS(iDS), IGS(iDS), DIGS(iDS))
        End If
        CascadeG(iDS)= Card(80:80)     ! Cascading gamam-rays
        Call UpStr(CascadeG(iDS))
        If (iDS > 1 .and. CascadeG(iDS) == 'C') Then
          Write (DefOut,'(a)') '      <F> Cascading gamma-rays for normalisation only allowed for single data set'
          Stop
        End If
!.....GAMMA record ----------------------------------------------------------                           
      Case ('  G ')
         cRI = Card(22:29)
        cDRI = Card(30:31)
         cCC = Card(56:62)
        cDCC = Card(63:64)
         cTI = Card(65:74)
        cDTI = Card(75:76)
        X = Card(79:79)
        Call UPSTR(X)
!.......GAMMA record used for normalization ----------------------------------  
        If (X == ' ') Cycle LoopCGamma
        If (X /= 'X') Then
          Write (DefOut,'(a)') '      <E> Use only "X" in column 79'
          Write (DefOut,'(i8,a)') iC,'   '//Card
          Stop
        End If
        Write (RptLun,'(a)') '    '//Trim(Card)
!.......VERIFY DRI and DTI fields
        If (cDRI == 'CA' .or. CDRI == 'AP' .or. &
            CDRI == 'LT' .or. CDRI == 'LE' .or. &
            CDRI == 'GT' .or. CDRI == 'GE') Then 
          Write (DefOut,'(a)') '      <E> DRI is not valid for normalization'
          Write (DefOut,'(i8,a)') iC,'   '//Card
          Stop
        End If
        If (cDTI == 'CA' .or. cDTI == 'AP' .or. &
            cDTI == 'LT' .or. cDTI == 'LE' .or. &
            cDTI == 'GT' .or. cDTI == 'GE') Then 
          Write (DefOut,'(a)') '      <E> DTI is not valid for normalization'
          Write (DefOut,'(i8,a)') iC,'   '//Card
          Stop
        End If
        nCG(iDS) = nCG(iDS)+1
        If (nCG(iDS) > MaxCG) Then
          Write (DefOut,'(a,i3)') '      <E> Too many calibration gammas per DataSet. Maximum: ',MaxCG
          Write (DefOut,'(i8,a)') iC,'   '//Card
          Stop
        End If
        CG(nCG(iDS),iDS)%iC = iC                                        ! Card index
!.......Cascading gamma rays 
        If (CascadeG(iDS) == 'C') nCas(iDS) = nCas(iDS) + 1 
        Write (DefOut,'(a)') '    '//Card
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
!.......Convert STRING values to NUMERICAL
        Call CNVS2U(cRI,cDRI,RI,DRI)
        Call CNVS2U(cCC,cDCC,CC,DCC)
        If (cCC /= ' ' .and. cDCC == ' ') DCC = CC * BrIccUnc
        Call CNVS2U(cTI,cDTI,TI,DTI)
!       ============================================================================
!       Verify numerical values
!       ============================================================================
!       (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
        If (cRI /= '  ' .and. cTI == ' ') Then 
          Call LbSup(cRI)
          Call LbSup(cDRI)
! 13-Feb-2019 to allow blank DRI
          If ((TypStr(Trim(cRI)) /= -2 .and. TypStr(Trim(cRI)) /= 1)) Then
             Write (DefOut,'(a)') '      <E> RI and/or DTI is non-numerical'
             Write (DefOut,'(i8,a)') iC,'   '//Card
             Stop
           End If         
!       =====================================================================================
!       (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!       (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
        Else If (cTI /= '  ') Then 
          Call LbSup(cTI)
          Call LbSup(cDTI)
          If ((TypStr(Trim(cTI)) /= -2 .and. TypStr(Trim(cTI)) /= 1) .or. &
               TypStr(Trim(cDTI)) /= 1) Then
            Write (DefOut,'(a)') '      <E> TI and/or DTI is non-numerical'
            Write (DefOut,'(i8,a)') iC,'   '//Card
            Stop
          End If         
        End If                
!.......Copies Input String & numerical values
        CG(nCG(iDS),iDS)%cRI  =  cRI
        CG(nCG(iDS),iDS)%cDRI = cDRI
        CG(nCG(iDS),iDS)%RI   =   RI
        CG(nCG(iDS),iDS)%DRI  =  DRI
        CG(nCG(iDS),iDS)%cCC  =  cCC
        CG(nCG(iDS),iDS)%cDCC = cDCC
        CG(nCG(iDS),iDS)%CC   =   CC
        CG(nCG(iDS),iDS)%DCC  =  DCC
        CG(nCG(iDS),iDS)%cTI  =  cTI
        CG(nCG(iDS),iDS)%cDTI = cDTI
        CG(nCG(iDS),iDS)%TI   =   TI
        CG(nCG(iDS),iDS)%DTI  =  DTI
      End Select
    End Do LoopCGamma
!...No Gamma selected for normalization
    If (nCG(1)+nCG(2) == 0) Then
      Write (DefOut,'(a)') '      <F> No transition has been marked for normalization'
      Stop
    End If
  End Do LoopDS
  nDS= iDS - 1    ! actual number of data sets                                                   
! =======================================================================================
! STEP#3 Calculates NR(iDS) and BR(iDS, only for multiple data sets) 
! =======================================================================================
  Select Case (nDS)
! SINGLE DATASET ========================================================================
  Case (1)
    Call CalibSingleDS()
  Case Default
    Call CalibMultipleDS()
  End Select

  Return
  End