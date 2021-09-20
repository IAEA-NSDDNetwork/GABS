  Subroutine CalibSingleDS()
! Calculates NR for a single data set
!   NR = (100-IGS)/(100*sTI); sTI = sum TI(i)= sum RI(i)*(1+CC(i))
! Assumed that all TI(i), RI(i), CC(i) and IGS are indepndent, are NOT correlated
  Use  GabsMod
  Implicit None
  Character(len=*), Parameter                    :: Subr = 'CalibSingleDS'
  Character(len=10)                              ::  cE
  Character(len=2)                               :: cDE
  Character(len=80)                              :: Card, SGcard, IGline, GContCard
  Character(len=1)                               :: X
  Integer(kind=4)                                :: IoStat 
  Integer(kind=4)                                :: jG = 0     
  Integer(kind=4)                                :: iG0 = 0
  Real(kind=4)                                   :: sTI = 0.0    
  Real(kind=4)                                   :: rCas = 0.0
  Real(kind=4)                                   ::   IG_NR = 0.0
  Real(kind=4)                                   ::  DIG_NR = 0.0
  Character(Len= 8)                              ::  cIG_NR = ' '     
  Character(Len= 2)                              :: cDIG_NR = ' '    
!
  Integer(kind=4)                                :: iC = 0
  Integer(kind=4)                                :: lC = 0
  Integer(kind=4)                                :: dPos = 0
  Real(kind=4)                                   :: DCC_L, DCC_H
  Real(kind=8), Dimension(MaxCG)                 :: aDNR = 0.0D0
  Real(kind=8)                                   :: wDNR = 0.0D0
  Character(len=20)                              :: NsrKey = ' '
  Character(len=6)                               :: wcDCC = ' '
  Character(len=132)                             :: Line = ' '
  Integer(kind=4), External                      :: ScanQuantOpVal
  Integer(kind=4), External                      :: TypStr
  Integer(kind=4), External                      :: LenStr
! =============================================================================
! Step 1: Evaluates summed total intensity
! =============================================================================  
  sTI = 0.0
  LoopTI : Do jG=1, nCG(1)
!   ===========================================================================
!   (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
    If (CG(jG,1)%cRI /= '  ' .and. CG(jG,1)%cTI == ' ') Then 
      CG(jG,1)%TI  = CG(jG,1)%RI * (1.0+CG(jG,1)%CC)
      CG(jG,1)%DTI = Sqrt((CG(jG,1)%DRI*(1+CG(jG,1)%CC))**2 + (CG(jG,1)%RI*CG(jG,1)%DCC)**2)
!   ===========================================================================
!   (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!   (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
    Else If (CG(jG,1)%cTI /= '  ' .and. CG(jG,1)%CC /= 0.0) Then 
      CG(jG,1)%RI  = CG(jG,1)%TI /(1.0+CG(jG,1)%CC)
      CG(jG,1)%DRI  = Sqrt( (CG(jG,1)%DTI /(1.0+CG(jG,1)%CC))**2 + (CG(jG,1)%DCC*CG(jG,1)%TI/(1.0+CG(jG,1)%CC)**2)**2)
    End If  
    sTI = sTI + CG(jG,1)%TI                        ! summed TI
  End Do LoopTI
! =============================================================================
! Step 2: Evaluates NR(1) & DNR(1)
! =============================================================================  
!...Cascade gamma rays                                                
  If (nCas(nDS) > 0) Then
    rCas = Float(max0(1,nCas(nDS)))
  Else
    rCas = 1.0
  End If
!  If (IGS(1) == 0.0) rCas =rCas*100.0
  NR(1) = rCas*(100.0-IGS(1))/(sTI)
!.......Calculates
  DNR(1) = (DIGS(1)*sTI)**2
  Do jG = 1, nCG(1)
!   ===========================================================================
!   (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
    If (CG(jG,1)%cRI /= '  ' .and. CG(jG,1)%cTI == ' ') Then 
      DNR(1) = DNR(1) + ((100.-IGS(1)) * CG(jG,1)%DRI * (1.+CG(jG,1)%CC))**2 + &
                        ((100.-IGS(1)) * CG(jG,1)%RI  *     CG(jG,1)%DCC)**2
!   ===========================================================================
!   (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!   (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
    Else If (CG(jG,1)%cTI /= '  ') Then 
      DNR(1) = DNR(1) + ((100.-IGS(1)) * CG(jG,1)%DTI)**2
    End If  
  End Do
  DNR(1) = rCas *  Sqrt(DNR(1))/(sTI**2)
!....   convert NR(1) and DNR(1) to ENSDF style. Example: 0.384 21
  cNR(1) = ' '
  cDNR(1) = ' '
  If (DNR(1) > 0.0) Then
    Call CNVU2S_n(NR(1),DNR(1),cNR(1),8,cDNR(1),2)
  Else
    Call CNVU2S_n(NR(1),0.03*NR(1),cNR(1),8,cDNR(1),2)
    cDNR(1) = ' '
  End If
  If (nDS > 1) cDNR(1) = '  '
!....   Convert BR(1) and DBR(1) to ENSDF style. EXAMPLE: 0.55 4
   cBR(1) = ' '
  cDBR(1) = ' '
  If (DBR(1) > 0.0) Then
    Call CNVU2S_n(BR(1),DBR(1),cBR(1),8,cDBR(1),2)
  Else
    Call CNVU2S_n(BR(1),BR(1)*0.03,cBR(1),8,cDBR(1),2)
    cDBR(1) = ' '
  End If
  If (DBR(1) < 1.0E-20) cDBR(1) = '  '
  Call LbSup( cNR(1))
  Call LbSup(cDNR(1))
  Call LbSup( cBR(1))
  Call LbSup(cDBR(1))
! =============================================================================
! Step 3: Scan all cards, updates N-records, generates new 2_G cards with %IG
! =============================================================================
  iC = 0    ! card index, used to identift calibration transitions
  LoopCards : Do While (iC < DS_nCards(1))
    iC= iC+1
    Card = DS_Cards(1,iC)
    Call UpStr(Card(6:9))
    Select Case (Card(6:9))
!   ============================================================================================
!   normalization record
!   ============================================================================================
    Case ('  N ')
      Write (DefOut,'(a)') ' '
      Write (RptLun,'(a)') ' '
      Line = '  Normalization: '//Trim(DSID(1))//'  NR'
      Select Case (TypStr(cDNR(1)))
!     Blank
      Case (0)
        Line = Trim(Line)//'='//Trim(cNR(1))//'  BR'
!     Numerical
      Case (1)
        Line = Trim(Line)//'='//Trim(cNR(1))//' ('//Trim(cDNR(1))//')  BR'
!     AP, LT, LE, GT, GE
      Case (2)
        Line = Trim(Line)//' '//Trim(cDNR(1))//' '//Trim(cNR(1))//'  BR'
      Case Default
        Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
        Stop
      End Select
      Select Case (TypStr(cDBR(1)))
!     Blank
      Case (0)
        Line = Trim(Line)//'='//Trim(cBR(1))
!     Numerical
      Case (1)
        Line = Trim(Line)//'='//Trim(cBR(1))//' ('//Trim(cDBR(1))//')'
!     AP, LT, LE, GT, GE
      Case (2)
        Line = Trim(Line)//' '//Trim(cDBR(1))//' '//Trim(cBR(1))
      Case Default
        Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
        Stop
      End Select
      Write (DefOut,'(a)') Trim(Line)
      Write (RptLun,'(a)') Trim(Line)
      Call LbSup( cNR(1))
      Call LbSup(cDNR(1))
      Call PutField( cNR(1), Card(10:19)) 
      Call PutField(cDNR(1), Card(20:21))
      Card (6:9)=DS_Cards(1,iC)(6:9)
      Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
      If (IoStat /= 0) Then
        Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
        Stop
      End If
!   ============================================================================================
!   normalization continuation record - do no save to new file
!   ============================================================================================
    Case ('2 N ')
!   =========================================================================
!   GAMMA record
!   =========================================================================
    Case ('  G ')
        cE = Card(10:19)
       cDE = Card(20:21)
        RI = 0.0
       cRI = Card(22:29)
       DRI = 0.0
      cDRI = Card(30:31)
        CC = 0.0
       cCC = Card(56:62)
       DCC = 0.0
      cDCC = Card(63:64)
        TI = 0.0
       cTI = Card(65:74)
       DTI = 0.0
      cDTI = Card(75:76)
      X = Card(79:79)
      Card(79:79) = ' '
       cIG = ' '
      cDIG = ' '
      lC = iC
!.....Look for CC on S_G cards if no CC given on G-card -------------------
      LoopSG : Do While (cCC == ' ') 
        iC = iC+1
        If (iC > DS_nCards(1)) Exit LoopSG
        SGCard = DS_Cards(1,iC)
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
!.....Calculates %IG for GAMMA records ---------------------------------------------
      LoopProcG : Do While (.TRUE.)
!       ============================================================================
!       Verify numerical values
!       ============================================================================
!       (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
        If (cRI /= '  ' .and. cTI == ' ') Then 
          Call LbSup(cRI)
          Call LbSup(cDRI)
          If ((TypStr(Trim(cRI)) /= -2 .and. TypStr(Trim(cRI)) /= 1)) Then
            Write (DefOut,'(a)') '      <E> RI and/or DTI is non-numerical'
            cIG = cRI
          End If         
          Call CNVS2U(cRI,cDRI,RI,DRI)
!       =====================================================================================
!       (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!       (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
!       Calculates RI from TI and CC
        Else If (cTI /= '  ' .and. cCC /= ' ') Then 
          Call LbSup(cTI)
          Call LbSup(cDTI)
          If ((TypStr(Trim(cTI)) /= -2 .and. TypStr(Trim(cTI)) /= 1) .or. &
               TypStr(Trim(cDTI)) /= 1) Then
            cIG = cTI
          End If         
          Call CNVS2U(cTI,cDTI,TI,DTI)
          Call CNVS2U(cCC,cDCC,CC,DCC)
          If (cCC /= ' ' .and. cDCC == ' ') DCC = CC * BrIccUnc
           RI = TI/(1.0+CC)
          DRI =  Sqrt((DTI/(1+CC))**2 + (DCC*TI/(1.0+CC)**2)**2)
        End If
!.......Evelautes %IG and %DIG
        IG = RI * NR(1) * BR(1)
! ......Check if Gamma Used for normalisation ---------------------------------------
        iG0 = 0
        Do jG = 1, nCG(1)
          If (CG(jG,1)%iC == iC ) iG0 = jG
        End Do
!.......Calibration GAMMA-ray ------------------------------------------------------
        If (iG0 > 0) Then
          Card(79:79) = ' '
          Do jG = 1, nCG(1)
            aDNR(jG)  = 0.0D0
          End Do
          wDNR = 0.0
          Do jG = 1, nCG(1)
            If (iG0 == jG) Then
              aDNR(iG0) = (CG(iG0,1)%RI**2)  * (sTI**2) * (DIGS(1)**2) + &
                         ((-100.D0+IGS(1))**2) * (CG(iG0,1)%RI**4) * (CG(iG0,1)%DCC**2) + &
                         ((-100.D0+IGS(1))**2) * ((sTI-CG(iG0,1)%TI)**2) * (CG(iG0,1)%DRI**2)
            Else
              aDNR(jG) = ((-100.D0+IGS(1))**2) * (CG(iG0,1)%RI**2) * (CG(jG,1)%RI**2) * (CG(jG,1)%DCC**2) + &
                         ((1.0D0+CG(jG,1)%CC)**2) * ((-100.D0+IGS(1))**2) * (CG(iG0,1)%RI**2) * (CG(jG,1)%DRI**2)
            End If
            wDNR = wDNR + aDNR(jG) 
          End Do
          wDNR = Dsqrt(wDNR)/(sTI**2)
          DIG = RI * NR(1)*BR(1)*Sqrt( (wDNR/(RI * NR(1)))**2 + (DBR(1)/BR(1))**2)
!.......Other GAMMA rays
        Else
          DIG = Sqrt( (DRI*NR(1)*BR(1))**2 + (RI*DNR(1)*BR(1))**2 + (RI*NR(1)*DBR(1))**2 )
        End If
!.......If only one normalisation transition
        If (nCG(1) == 1) Then
          DIG = RI * Sqrt( (DNR(1)*BR(1))**2 + (NR(1)*DBR(1))**2 )
        End If
        If ((cDRI == 'LT' .or. cDRI == 'LE' .or. cDRI == 'AP' .or. cDRI == 'GT' .or. cDRI == 'GE' ) .and. cTI == ' ') Then
          Call CNVU2S_n(IG, IG*0.03, cIG,8,cDIG,2)
          cDIG = cDRI
        Else If (cDTI == 'LT' .or. cDTI == 'LE' .or. cDTI == 'AP' .or. cDTI == 'GT' .or. cDTI == 'GE' ) Then
          Call CNVU2S_n(IG, IG*0.03, cIG, 8, cDIG, 2)
          cDIG = cDTI
        Else
          If (DIG > 0.0) Call CNVU2S_n(IG, DIG, cIG, 8, cDIG, 2)
        End If
        Exit LoopProcG
      End Do LoopProcG
      iC = lC
!     Save G-card and G-continuation with %IG, if needed
      Card = DS_Cards(1,iC)
      Card (6:9)=DS_Cards(1,iC)(6:9)
      Card(79:79) = ' '
      Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
      If (IoStat /= 0) Then
        Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
        Stop
      End If
      Call LbSup(cIG)
      Call LbSup(cDIG)
      If (IG > 0.0) Then
        Call LbSup(cE)
        Call LbSup(cDE)
        IGline = '  E='//Trim(cE)
        If (TypStr(cDE) == 1) Then
          IGline = Trim(IGline)//'('//Trim(cDE)//')'
        Else
          IGline = Trim(IGline)//' '//Trim(cDE)
        End If
!       Evaluate uncertainty using DNR(1) for comparison
        If (X == 'X') Then
           IG_NR = IG
          DIG_NR = Sqrt( (DRI*NR(1)*BR(1))**2 + (RI*DNR(1)*BR(1))**2 + (RI * NR(1) * DBR(1))**2)
          Call CNVU2S_n(IG_NR, DIG_NR, cIG_NR, 8, cDIG_NR, 2)
          Call LbSup(cIG_NR)
          Call LbSup(cDIG_NR)
          Select Case (TypStr(cDIG))
!         BLANK cDIG
          Case (0)
            IGLine(20:) = ' %IG='//Trim(cIG)
            IGLine(40:) = 'Compare with %IG='//Trim(cIG_NR)
            Card(6:) = '2 G %IG='//Trim(cIG)
!         Numeric cDIG
          Case (1)
            Card(6:)    = '2 G %IG='//Trim(cIG)//' '//Trim(cDIG)
            IGline(20:) =    ' %IG='//Trim(cIG)//' ('//Trim(cDIG)//')'
            IGLine(40:) = 'Compare with %IG='//Trim(cIG_NR)//' ('//Trim(cDIG_NR)//')'
!         AP, LE, LT, GE, GT cDIG
          Case (2)
            IGLine(20:) = ' %IG '//trim(cDIG)//' '//Trim(cIG)
            IGLine(40:) = 'Compare with %IG '//Trim(cDIG_NR)//' '//Trim(cIG_NR)
            Card(6:) = '2 G %IG '//Trim(cDIG)//' '//Trim(cIG)
          Case Default
            Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
            Stop
          End Select
        Else
          cIG_NR = ' '
          cDIG_NR = ' '
          Select Case (TypStr(cDIG))
!         BLANK cDIG
          Case (0)
            IGLine(20:) = ' %IG='//Trim(cIG)
            Card(6:) = '2 G %IG='//Trim(cIG)
!         Numeric cDIG
          Case (1)
            Card(6:)    = '2 G %IG='//Trim(cIG)//' '//Trim(cDIG)
            IGline(20:) =    ' %IG='//Trim(cIG)//' ('//Trim(cDIG)//')'
!         AP, LE, LT, GE, GT cDIG
          Case (2)
            IGLine(20:) = ' %IG '//trim(cDIG)//' '//Trim(cIG)
            Card(6:) = '2 G %IG '//Trim(cDIG)//' '//Trim(cIG)
          Case Default
            Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
            Stop
          End Select
        End If
       dPos = LenStr(Card)
       Card (7:9)=DS_Cards(1,iC)(7:9)
       Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
       If (IoStat /= 0) Then
         Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
         Stop
       End If
       Write (DefOut,'(a)') Trim(IGline)
       Write (RptLun,'(a)') Trim(IGline)
       
      End If
!   =========================================================================
!   GAMMA continuation record
!   =========================================================================
    Case ('2 G ')
      GContCard = Card
      Call LbSup(GContCard(10:))
      If (GContCard(10:13) == '%IG=') Cycle LoopCards
       Card (7:9)=DS_Cards(1,iC)(7:9)
       Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
       If (IoStat /= 0) Then
         Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
         Stop
       End If
!...All other cards ===========================================================
    Case Default
       Card (6:9)=DS_Cards(1,iC)(6:9)
       Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
       If (IoStat /= 0) Then
         Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
         Stop
       End If
    End Select
  End Do LoopCards
! =============================================================================
  Return
  End