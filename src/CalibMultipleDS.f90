  Subroutine CalibMultipleDS()
! Calculates NR and BR for a multiple data sets
  Use  GabsMod
  Implicit None
  Character(len=*), Parameter          :: Subr = 'CalibMultipleDS'
  Character(len=10)                    ::  cE
  Character(len=2)                     :: cDE
  Character(len=80)                    :: Card, SGcard, IGline, GContCard
  Character(len=1)                     :: X
  Integer(kind=4)                      :: IoStat 
  Integer(kind=4)                      :: t, tt         ! Data set
  Integer(kind=4)                      :: i   
  Integer(kind=4)                      :: j, jj         ! Gamma-ray index, included in normalisation
  Real(kind=4)                         :: rCas = 0.0
  Real(kind=4)                         ::   IG_NR = 0.0
  Real(kind=4)                         ::  DIG_NR = 0.0
  Real(kind=4), Dimension(MaxDS)       :: wsTI = 0.0    
  Real(kind=4)                         :: wN             
  Real(kind=4), Dimension(MaxDS)       :: wBR           
  Real(kind=4), Dimension(MaxDS,MaxDS) :: wR            
  Real(kind=4), Dimension(MaxDS,MaxDS) :: wDR2          
  Real(kind=4)                         :: wsTI2GDG2     ! wsTI2GDG2 = sum{t} (sum{j} TI(j,t))**2 (DG(t)**2/G(t)**4)
  Real(kind=4), Dimension(MaxDS)       :: wssRTI        ! wssRTI(i) = sum{j,t} R(t,i)*T(j,t)
  Real(kind=4), Dimension(MaxDS)       :: wsRTI         ! wsRTI(i) = sum{j,t} R(t,i)*T(j,t) [t /= i)
  Real(kind=4), Dimension(MaxDS)       :: wsDTI2        ! wsDTI2(i) = sum{j} DTI(j,i)**2 
  Real(kind=4), Dimension(MaxDS)       :: wsR2DTI2      ! wsR2DTI2(i) = sum{j,t} R(t,i)**2 DTI(j,t)**2 [t /= i] 
  Real(kind=4), Dimension(MaxDS)       :: wsTI2DR2      ! wsTI2DR2(i) = sum{t} (sum{j} TI(j,t))**2 DR(t,i)**2 [t /= i] 
  Real(kind=4)                         :: wD2           ! wD2 = sum{j,t) (DTI(j,t) / G(t))**2 [j /= l; l-th gamma being processed]
  Real(kind=4)                         :: wC            ! wC = sum{j,t) TI(j,t) / G(t)  [j /= l; l gamma being processed]
  Real(kind=4), Dimension(MaxDS)       :: BTIa = 0.0    ! BTIa = sum{j} (TI(j,1)*TI(j,2))**2
  Real(kind=4), Dimension(MaxDS)       :: BTIb = 0.0    ! BTIb = sum{j} (TI(j,1)*DTI(j,2))**2
  Real(kind=4), Dimension(MaxDS)       :: BCCRIa = 0.0  !
  Real(kind=4), Dimension(MaxDS)       :: BCCRIb = 0.0  ! 
  Character(Len= 8)                    ::  cIG_NR = ' '     
  Character(Len= 2)                    :: cDIG_NR = ' '    
  Character(len=132)                   :: Line = ' '
!
  Integer(kind=4)                      :: iC = 0
  Integer(kind=4)                      :: lC = 0, dPos
  Real(kind=4)                         :: DCC_L, DCC_H
  Character(len=20)                    :: NsrKey = ' '
  Character(len=6)                     :: wcDCC = ' '
  Integer(kind=4), External            :: ScanQuantOpVal
  Integer(kind=4), External            :: TypStr
  Integer(kind=4), External                      :: LenStr
! =============================================================================
! Step 1: Evaluates calibration matrixes
!         N = 100/Sun{j,t} TI{j,t}/G(t)
! =============================================================================  
! G(t) is the fraction of the t-th decay branch, which DOES NOT populate the
! corresponding ground state
! Historically NB(t) was used pass the information: NB(t) = (100-IGS(t))/100
!   On output GABS will clear the NB field
  Do t=1,nDS
     G(t) = NB(t)*(100.0-IGS(t))/100.0
    DG(t) = Sqrt( (DIGS(t)*BR(t)/100.0)**2 +((100.0-IGS(t))*DBR(t)/100.)**2)
  End Do
  
  Do t = 1, nDS
    Do i = 1, nDS
       wR (t,i) =  G(i) / G(t)
      wDR2(t,i) = (G(i) / G(t))**2 *( (dG(i)/G(i))**2 + (dG(t)/G(t))**2 ) 
    End Do
  End Do
  wN = 0.0
  LoopDS : Do t = 1, nDS
    wsTI(t) = 0.0
    wBR(t) = 0.0
    wsDTI2(t) = 0.0
    LoopG  : Do j = 1, nCG(t)
!     ===========================================================================
!     (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
      If (CG(j,t)%cRI /= '  ' .and. CG(j,t)%cTI == ' ') Then 
        CG(j,t)%TI = CG(j,t)%RI * (1.0+CG(j,t)%CC)
        CG(j,t)%DTI = Sqrt((CG(j,t)%DRI*(1.0+CG(j,t)%CC))**2 + (CG(j,t)%RI*CG(j,t)%DCC)**2)
!     ===========================================================================
!     (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!     (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
      Else If (CG(j,t)%cTI /= '  ' .and. CG(j,t)%CC > 0.0) Then 
        CG(j,t)%RI  = CG(j,t)%TI /(1.0+CG(j,t)%CC)
        CG(j,t)%DRI  = Sqrt( (CG(j,t)%DTI /(1.0+CG(j,t)%CC))**2 + (CG(j,t)%DCC*CG(j,t)%TI/(1.0+CG(j,t)%CC)**2)**2)
      End If                
!     ===========================================================================
        wN = wN + CG(j,t)%TI / G(t)                        ! wN = sum{j,t} T(j,t) / G(t)
        wsTI(t) =  wsTI(t) + CG(j,t)%TI                    ! wsTI(i) =  sum{j} CG(j,i)%TI
        wBR(t) = wBR(t) + CG(j,t)%TI / G(t)                ! wBR(t) = sum{j} T(j,t) / G(t) 
        wsDTI2(t) = wsDTI2(t) + CG(j,t)%DTI**2             ! wsDTI2(i) = sum{j} DTI(j,i)**2
    End Do LoopG
  End Do LoopDS
  wsTI2GDG2 = 0.0
  Do i =1, nDS
    wssRTI(i) = 0.0
    wsR2DTI2(i) = 0.0
    wsTI2DR2(i) = 0.0
    wsRTI(i) = 0.0
    wsTI2GDG2 = wsTI2GDG2 + wsTI(i)**2 * DG(i)**2 / G(i)**4  ! wsTI2GDG2 = sum{t} (sum{j} TI(j,t)**2 (DG(t)**2/G(t)**4)
    Do t=1, nDS !nDS= 2
      If (t /= i) Then
        wsTI2DR2(i) = wsTI2DR2(i) + wsTI(t)**2 * wDR2(t,i) ! wsTI2DR2(i) = sum{t} (sum{j} TI(j,t))**2 DR(t,i)**2 [t /= i] 
      End If
      Do j = 1, nCG(t)
        wssRTI(i) = wssRTI(i) + wR(t,i)*CG(j,t)%TI           ! wssRTI(i) = sum{j,t} R(t,i)*T(j,t)
        If (t /= i) Then
          wsRTI(i) = wsRTI(i) + wR(t,i)*CG(j,t)%TI           ! wsRTI(i) = sum{j,t} R(t,i)*T(j,t) [t /= i)
          wsR2DTI2(i) = wsR2DTI2(i) + wR(t,i)**2 * CG(j,t)%DTI**2  ! wsR2DTI2(i) = sum{j,t} R(t,i)**2 DTI(j,t)**2 [t /= i] 
        End If
      End Do
    End Do
  End Do
! =============================================================================
! Step 2: Evaluates NR(t) & DNR(t)
!   evaluating NR(t), BR(t) and DBR(t) by using N = NR(t) * BR(t)
! =============================================================================  
  N = 100.0 / wN
  Call Calc_BTI(0, BTIa,BTIb,BCCRIa,BCCRIb)
  Do t = 1, nDS
    BR(t) = wBR(t) / wN 
    DBR(t) = ( (((-100.+IGS(2))**2*DIGS(1)**2 + (-100.+IGS(1))**2*DIGS(2)**2)*(BTIa(1)*BTIa(2))**2 + ((-100.+IGS(1))*(-100.+IGS(2)))**2*  &
                 (BTIa(1)**2*BTIb(2) + BTIa(2)**2*BTIb(1)))/((-100+IGS(2))*BTIa(1) + (-100+IGS(1))*BTIa(2))**4 )**0.5
    NR(t) = N / BR(t)
    If (DBR(t) > 0.0) Then
      Call CNVU2S_n(BR(t),DBR(t),cBR(t),8,cDBR(t),2)
    Else
      Call CNVU2S_n(BR(t),BR(t)*0.03,cBR(t),8,cDBR(t),2)
      cDBR(t) = ' '
    End If
    Call CNVU2S_n(NR(t),NR(t)*0.03,cNR(t),8,cDNR(t),2)
    cDNR(t) = ' '
  End Do
! =============================================================================
! Step 3: Evaluates %IG(t,j)
! =============================================================================
  LoopProcDS : Do t = 1, nDS
!...Cascade gamma rays                                                
    If (nCas(nDS) > 0) Then
      rCas = Float(max0(1,nCas(nDS)))*100.0
    Else
      rCas = 1.0
    End If
!   ===========================================================================
!   Loop for each card in the data set
!   ===========================================================================
    iC = 0
    LoopCards : Do While (iC < DS_nCards(t))
      iC= iC+1
      Card = DS_Cards(t,iC)
      Call UpStr(Card(6:9))
      Select Case (Card(6:9))
!     =========================================================================
!     normalization record
!     =========================================================================
      Case ('  N ')
        Write (DefOut,'(a)') ' '
        Write (RptLun,'(a)') ' '
        Line = '  Normalization: '//Trim(DSID(t))//'  NR'
        Select Case (TypStr(cDNR(t)))
!       Blank
        Case (0)
          Line = Trim(Line)//'='//Trim(cNR(t))//'  BR'
!       Numerical
        Case (1)
          Line = Trim(Line)//'='//Trim(cNR(t))//' ('//Trim(cDNR(t))//')  BR'
!       AP, LT, LE, GT, GE
        Case (2)
          Line = Trim(Line)//' '//Trim(cDNR(t))//' '//Trim(cNR(t))//'  BR'
        Case Default
          Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
          Stop
        End Select
        Select Case (TypStr(cDBR(t)))
!       Blank
        Case (0)
          Line = Trim(Line)//'='//Trim(cBR(t))
!       Numerical
        Case (1)
          Line = Trim(Line)//'='//Trim(cBR(t))//' ('//Trim(cDBR(t))//')'
!       AP, LT, LE, GT, GE
        Case (2)
          Line = Trim(Line)//' '//Trim(cDBR(t))//' '//Trim(cBR(t))
        Case Default
          Write (DefOut,'(a)') ' <F> Programming error in '//Trim(Subr)
          Stop
        End Select
        Write (DefOut,'(a)') Trim(Line)
        Write (RptLun,'(a)') Trim(Line)
        Call LbSup( cNR(t))
        Call PutField( cNR(t), Card(10:19)) 
        Call LbSup( cBR(t))
        Call PutField( cBR(t), Card(32:39)) 
        Call LbSup( cDBR(t))
        Call PutField( cDBR(t), Card(40:41)) 
        Card(42:55) = ' '
        Card (6:9)=DS_Cards(t,iC)(6:9)
        Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
        If (IoStat /= 0) Then
          Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
          Stop
        End If
!     =========================================================================
!     normalization continuation record - do no save to new file
!     =========================================================================
      Case ('2 N ')
!     =========================================================================
!     GAMMA record
!     =========================================================================
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
         cIG = ' '
        cDIG = ' '
          lC = iC
!.....  Look for CC on S_G cards if no CC given on G-card =====================
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
!.......=========================================================================
!.......Process GAMMA records
!.......=========================================================================
        LoopProcG : Do While (.TRUE.)
!         Verify numerical values and calculates TI
!         (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
          If (cRI /= '  ' .and. cTI == ' ') Then 
            Call LbSup(cRI)
            Call LbSup(cDRI)
            If ((TypStr(Trim(cRI)) /= -2 .and. TypStr(Trim(cRI)) /= 1)) Then
              Write (DefOut,'(a)') '      <E> RI and/or DTI is non-numerical'
              cIG = cRI
            End If         
            Call CNVS2U(cRI,cDRI,RI,DRI)
            Call CNVS2U(cCC,cDCC,CC,DCC)
            If (cCC /= ' ' .and. cDCC == ' ') DCC = CC * BrIccUnc
             TI = RI*(1.0+CC)
            DTI =  Sqrt((DRI*(1.0+CC))**2 + (DCC*TI)**2)
!         =======================================================================
!         (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!         (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
          Else If (cTI /= '  ' .and. cDCC /= ' ') Then 
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
            DRI =  Sqrt((DTI/(1.0+CC))**2 + (DCC*TI/(1.0+CC)**2)**2)
          End If
! ........Gamma Used for normalisation ------------------------------------------
          If (X == 'X') Card(79:79) = ' '
!.........=====================================================================
!         Evelautes %IG and %DIG, uncertainties are based on Eqn 9 of 1096Br26 ...
!            Also see Erratum NIMA 249 (1986) 461
!.........=====================================================================
           IG = RI * NR(t) * BR(t)
           wD2 = 0.0   
           wC = 0.0
           Do tt = 1, nDS
             Do jj = 1, nCG(tt)
               If (iC /= CG(jj,tt)%iC) Then
                 wD2 = wD2 + ( CG(jj,tt)%DTI / G(tt) )**2               ! wD2(l) = sum{j,t) (DTI(j,t) / G(t))**2 [j /= l] 
                 wC = wC +  CG(jj,tt)%TI / G(tt)                        ! wC = sum{j,t) TI(j,t) / G(t)  [j /= l; l gamma being processed]
               Else If (iC /= CG(jj,tt)%iC) Then
               End If
             End Do
           End Do
          Exit LoopProcG
        End Do LoopProcG
        
        Call Calc_BTI(0, BTIa,BTIb,BCCRIa,BCCRIb)
        
        IF (t == 1) THEN
             DIG = (( ((-100.+IGS(1))*(-100.+IGS(2))*DRI*(-(-100.+IGS(1))*BTIa(2) - (-100.+IGS(2))*BTIa(1) + (1.+CC)*RI*(-100.+IGS(2))) )**2 - &
                   RI**2*(-100.+IGS(1))**2*(-100.+IGS(2))**4*(1.+CC)**2*DRI**2 + &
                   RI**2*( (-100.+IGS(1))**2*(-100.+IGS(2))**4*( BCCRIa(1) + BCCRIb(1) ) + (-100.+IGS(1))**4*(-100.+IGS(2))**2*(BCCRIa(2)+BCCRIb(2)) + &
                   (-100.+IGS(1))**4*DIGS(2)**2*BTIa(2)**2 + (-100.+IGS(2))**4*DIGS(1)**2*BTIa(1)**2 ) )/( BTIa(2)*(-100.+IGS(1))+BTIa(1)*(-100.+IGS(2)) )**4 )**0.5
        ELSE
             DIG = (( ((-100.+IGS(1))*(-100.+IGS(2))*DRI*(-(-100.+IGS(1))*BTIa(2) - (-100.+IGS(2))*BTIa(1) + (1.+CC)*RI*(-100.+IGS(1))) )**2 - &
                   RI**2*(-100.+IGS(1))**4*(-100.+IGS(2))**2*(1.+CC)**2*DRI**2 + &
                   RI**2*( (-100.+IGS(1))**2*(-100.+IGS(2))**4*( BCCRIa(1) + BCCRIb(1) ) + (-100.+IGS(1))**4*(-100.+IGS(2))**2*(BCCRIa(2)+BCCRIb(2)) + &
                   (-100.+IGS(1))**4*DIGS(2)**2*BTIa(2)**2 + (-100.+IGS(2))**4*DIGS(1)**2*BTIa(1)**2 ) )/( BTIa(2)*(-100.+IGS(1))+BTIa(1)*(-100.+IGS(2)) )**4 )**0.5  
        ENDIF
             
        If (DIG > 0.0) Then
          Call CNVU2S_n(IG, DIG, cIG, 8 , cDIG, 2)
        Else
          Call CNVU2S_n(IG, IG+0.03, cIG, 8 , cDIG, 2)
        End If
        
        If (cDRI == 'LT' .or. cDRI == 'LE' .or. cDRI == 'AP' .or. cDRI == 'GT' .or. cDRI == 'GE' ) Then
          Call CNVU2S_n(IG, IG*0.03, cIG,8,cDIG,2)
          cDIG = cDRI
        Else If (cDTI == 'LT' .or. cDTI == 'LE' .or. cDTI == 'AP' .or. cDTI == 'GT' .or. cDTI == 'GE' ) Then
          Call CNVU2S_n(IG, IG*0.03, cIG,8,cDIG,2)
          cDIG = cDTI
        Else If (cDIG == ' ') Then
          Call CNVU2S_n(IG, IG*0.03, cIG,8,cDIG,2)
          cDIG = ' '
        Else
          If (DIG > 0.0) Call CNVU2S_n(IG, DIG, cIG,8,cDIG,2)
        End If
!.......=======================================================================
        iC = lC
!       Save G-card and G-continuation with %IG, if needed
        Card = DS_Cards(1,iC)
         Card(79:79) = ' '
        Card (6:9)=DS_Cards(1,iC)(6:9)
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
!       Evaluate uncertainty using DNR(t) for comparison
        If (X == 'X') Then
           IG_NR = IG
          DIG_NR = Sqrt( (DRI*NR(t)*BR(t))**2 + (RI*DNR(t)*BR(t))**2 + (RI * NR(t) * DBR(t))**2)
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
        Card (7:9)=DS_Cards(t,iC)(7:9)
        Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
        If (IoStat /= 0) Then
           Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
           Stop
        End If
        Write (DefOut,'(a)') Trim(IGline)
        Write (RptLun,'(a)') Trim(IGline)
         
        End If
!     =========================================================================
!     GAMMA continuation record
!     =========================================================================
      Case ('2 G ')
        GContCard = Card
        Call LbSup(GContCard(10:))
        If (GContCard(10:13) == '%IG=') Cycle LoopCards
         Card (6:9)=DS_Cards(t,iC)(6:9)
         Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
         If (IoStat /= 0) Then
           Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
           Stop
         End If
!..  .All other cards ===========================================================
      Case Default
         Card (6:9)=DS_Cards(t,iC)(6:9)
         Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)                         ! Save updated card
         If (IoStat /= 0) Then
           Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
           Stop
         End If
      End Select
    End Do LoopCards
  End Do LoopProcDS
! =============================================================================
  Return
  End