!.....=============================================================================================
  Subroutine GAMMAS()
  Use GabsMod
  Implicit None
  Character(len=*), Parameter                 :: Subr = 'Gammas'
  Integer(kind=4)                             :: IoStat
  Integer(kind=4)                             :: iDS                  ! current data set
  Integer(kind=4)                             :: ifg
  Integer(kind=4)                             :: iC
  Real(kind=4)                                :: u = 0.0
   Real da,d21,sdtot1,sdtemp,sstemp,sstot1
   Real c21,dabsg,ddcc,drii,gg,rii,ru1
   Real ryabs,yy,dyy,yyabs,yabsg
   Real(kind=4)                               :: snor1
   Character(len=1)                           :: X
   Character(len=2)                           :: IiDBX
   Character(len=2)                           ::iDBX
   Character(len=10)                          ::  cE
   Character(len=2)                           :: cDE
   Character(len=7)                           ::  BL
   Character(len=8)                           ::  iBX, IiBX
   Character(len=18)                          :: STA
   Character(len=80)                          :: Card
   Integer(kind=4)                            :: FG               ! =1 Gamma used for normalization
!
   BL=' '
! =============================================================================
! Loop for each data set
! =============================================================================
  LoopDS : Do iDS = 1, nDS
    SNOR1=100.0 * SNOR
    DNR(iDS)= RU * SNOR
!   Calculate NR(iDS)*BR(iDS) and corresponding relative uncertainty U.
    If (nDS == 1) Then
      SNOR1=SNOR1 * BR(1)
      U = U * BR(1)
    End If
!   =============================================================================
!   Loop for each card in the data set
!   =============================================================================
    LoopCard : Do iC = 1, DS_nCards(iDS)
      Card = DS_Cards(iDS,iC)
      FG=0
      Call UpStr(Card(6:9))
      Select Case (Card(6:9))
!     ============================================================================================
!     normalization record
!     ============================================================================================
      Case ('  N ')
!....   Process NR(iDS), DNR(iDS), BR(iDS), DBR(iDS)
!....   For single-dataset calculation remove relative uncertainty in BR(1),
!       and then calculate DNR(1).
        If (nDS == 1) Then
          RU=(RU/100.0)**2-(DBR(1)/BR(1))**2
          RU=SQRT(RU) * 100.0
        End If
        U = RU * NR(iDS) / 100.0
!....   convert NR(iDS) and DNR(iDS) to ENSDF style. Example: 0.384 21
         cNR(iDS) = ' '
        cDNR(iDS) = ' '
        Call CNVU2S(NR(iDS),DNR(iDS),cNR(iDS),8,cDNR(iDS),2)
        If (nDS > 1) cDNR(iDS) = '  '
!....   Convert BR(iDS) and DBR(iDS) to ENSDF style. EXAMPLE: 0.55 4
         cBR(iDS) = ' '
        cDBR(iDS) = ' '
        If (DBR(iDS) > 0.0) Then
          Call CNVU2S(BR(iDS),DBR(iDS),cBR(iDS),8,cDBR(iDS),2)
        Else
          Call CNVU2S(BR(iDS),BR(iDS)*0.03,cBR(iDS),8,cDBR(iDS),2)
        End If
        If (DBR(iDS) < 1.0E-20) cDBR(iDS) = '  '
        Call LbSup( cNR(iDS))
        Call LbSup(cDNR(iDS))
        Call LbSup( cBR(iDS))
        Call LbSup(cDBR(iDS))
        If (iDS == 1) Then
          Write (DefOut,'(a)') ' '
          Write (RptLun,'(a)') ' '
          Write (DefOut,'(a)') '  Normalization: '//Trim(DSID(iDS))//&
                               '  NR= '//Trim(cNR(iDS))//' '//Trim(cDNR(iDS))// &
                               '  BR= '//Trim(cBR(iDS))//' '//Trim(cDBR(iDS))
          Write (RptLun,'(a)') '  Normalization: '//Trim(DSID(iDS))//&
                               '  NR= '//Trim(cNR(iDS))//' '//Trim(cDNR(iDS))// &
                               '  BR= '//Trim(cBR(iDS))//' '//Trim(cDBR(iDS))
        Else
          Write (DefOut,'(a)') '                 '//Trim(DSID(iDS))//&
                               '  NR= '//Trim(cNR(iDS))//' '//Trim(cDNR(iDS))// &
                               '  BR= '//Trim(cBR(iDS))//' '//Trim(cDBR(iDS))
          Write (RptLun,'(a)') ' '
          Write (RptLun,'(a)') '                 '//Trim(DSID(iDS))//&
                               '  NR= '//Trim(cNR(iDS))//' '//Trim(cDNR(iDS))// &
                               '  BR= '//Trim(cBR(iDS))//' '//Trim(cDBR(iDS))
        End If           
        Card(10:  ) = ' '
        Card(10:19) = cNR(iDS) 
        Card(20:21) = cDNR(iDS)
        Card(22:29) =  cNT(iDS)
        Card(30:31) = cDNT(iDS)
        Card(32:39) =  cBR(iDS)
        Card(40:41) = cDBR(iDS)
        Card(42:49) =  cNB(iDS)
        Card(50:55) = cDNB(iDS)
        Card(56:62) =  cNP(iDS)
        Card(63:64) = cDNP(iDS)
        Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)
        If (IoStat /= 0) Then
          Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
          Stop
        End If
!     ============================================================================================
!     GAMMA record
!     ============================================================================================
      Case ('  G ')
          cE = Card(10:19)
         cDE = Card(20:21)
         cRI = Card(22:29)
        cDRI = Card(30:31)
         cCC = Card(56:62)
        cDCC = Card(63:64)
         cTI = Card(65:74)
        cDTI = Card(75:76)
        X = Card(79:79)
        Call UPSTR(X)
        FG = 0                               ! Gamma not used for normalisation
        If (X == 'X') Then
          FG=1                               ! Gama used for normalization
          Card(79:79) = ' '
        End if
        Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)
        If (IoStat /= 0) Then
          Write (DefOut,'(a)') '  <F> Could not write to '//Trim(NewFile)//' file'
          Stop
        End If
!       Blank cRI - nothing to do  
        If (cRI == '  ')  Cycle LoopCard
!.......Convert STRING values to NUMERICAL
        Call CNVS2U(cRI,cDRI,RI,DRI)
        Call CNVS2U(cCC,cDCC,CC,DCC)
        If (cCC /= ' ' .and. cDCC == ' ') DCC = CC * BrIccUnc
        Call CNVS2U(cTI,cDTI,TI,DTI)
        SNOR1 = 100.0 * SNOR
        U = RU * SNOR
        If (nDS == 1) Then
          SNOR1 =SNOR1 * BR(1)
          U = U * BR(1)
        End If
        IFG = 0
        DYY = 0.0
        YY= RI
        Select Case (cDRI)
        Case ('CA','AP')
          IFG = 1
          DYY = YY * 0.5
        Case ('LT','LE')            
          IFG = 1
           YY = YY * 0.5
          DYY = YY
        Case Default
          IFG = 0
          DYY = DRI
        End Select
!       Set default uncertainty for relative photon intensity to 20%.
        If (cCC /= BL ) Then
          DA = DCC**2
          SDTEMP = (((1.0+ CC)**2)*((DYY)**2)+ (YY **2) * DA) / (NB(iDS) * NB(iDS))
         Else
           SDTEMP = DYY **2/(NB(iDS) * NB(iDS))
         End If
         SDTOT1 = SDTOT - SDTEMP
         D21 = SDTOT1 / (SSTOT **2)
         If (cCC /= BL) SSTEMP = (YY*(1.0+CC ))/NB(iDS)
         If (cCC == BL) SSTEMP =  YY/   NB(iDS)
         SSTOT1 = SSTOT - SSTEMP
         C21 = (SSTOT1 / SSTOT) **2
         RII = YY
         DRII = DYY
         DDCC =SQRT (DA)
         GG = NB(iDS)
         RU1 = D21+C21*((DRII/RII)**2)+((DDCC*RII)/(GG*SSTOT))**2+G2
!        What comes now is a correction made on 5/20/91.
!        It takes care of the case where br was measured.
         If (nDS == 1) RU1=RU1 + (DBR(iDS)/BR(iDS))**2
!        TK 31-Mar-2015 Prevent RU1 to be negative
         RU1 = SQRT(Abs(RU1))
         YABSG = 100.0 * SNOR * RII
         DABSG = RU1  * YABSG
!        For single-data set calculation multiply uncertainty by branching ratio BR(1).
         If (nDS == 1) DABSG = DABSG * BR(1)
         If (nDS == 1) YABSG = YABSG * BR(1)
!        Write RI(ABS) and uncertainty on specific G-records on GABS.RPT
         ryabs = (DYY/YY)**2 + (ru/100.0)**2 + (dbr(iDS)/br(iDS))**2
         ryabs = sqrt(ryabs)
         yyabs = ryabs * YABSG
         Call CNVU2S(YABSG,yyabs,iiBX,8,iiDBX,2)
         If (iiBX(1:8) == ' ') Then
           Write (RptLun,'(a,f)') '  <E> Could not convert RI=',YABSG
           Write (DefOut,'(a,f)') '  <E> Could not convert RI=',YABSG
           Stop
         End If
         Call LBSUP(iiBX)
         Call CNVU2S(YABSG,DABSG,iBX,8,iDBX,2)
         If (iBX(1:8) == ' ') Then
           Write (RptLun,'(a,f)') '  <E> Could not convert RI=',YABSG
           Write (DefOut,'(a,f)') '  <E> Could not convert RI=',YABSG
           Stop
         End If
         STA='2 G %IG='
         Call LBSUP(iBX)
         If (FG == 1) Then
           Write (RptLun,'(a)') '    E='//cE//' '//cDE//' %IG='//Trim(iBX)//' '//iDBX//' per 100 dis. Compare with '//Trim(iiBX)//' '//iiDBX
           Write (NewLun,'(a)') NUCID//Trim(STA)//Trim(iBX)//' '//iDBX !//', using the calculated normalization.'
         Else
           Write (RptLun,'(a)') '    E='//cE//' '//cDE//' %IG=' //Trim(iiBX)//' '//iiDBX//' per 100 dis.'
           Write (NewLun,'(a)') NUCID//Trim(STA)//Trim(iiBX)//' '//iiDBX ! //', using the calculated normalization.'
         End If
!     ============================================================================================
!     ALL other record
!     ============================================================================================
      Case Default
!         Writes cards into new ENSDF file
          If (Card(6:13) /= '2 G %IG=') Write (NewLun,'(a)',IoStat=IoStat) Trim(Card)
          If (IoStat /= 0) Then
            Write (DefOut,'(a)') '  <F> '//Trim(InpFile)//' file could not be opened'
            Stop
          End If
      End Select
    End Do LoopCard
  End Do LoopDS  
  Return
  End Subroutine GAMMAS
