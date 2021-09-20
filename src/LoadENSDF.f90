  Subroutine LoadENSDF()
! Loads ENSDF file into memory
  Use GabsMod
  Implicit None
  Character(len=*), Parameter                    :: Subr = 'LoadENSF'
  Character(len=80)                              :: Card
  Character(len=80)                              :: Line = ' '
  Integer(kind=4)                                :: IoStat = 0
  Integer(kind=4)                                :: iDS = 0
  Integer(kind=4)                                :: iCard =0 
  Integer(kind=4)                                :: lIGS, lBL
  Logical(kind=4)                                :: nSeen = .FALSE.
  Integer(kind=4), External                      :: TypStr
  Integer(kind=4), External                      :: IndexF
  Integer(kind=4), External                      :: LenStr
  Integer(kind=4), External                      :: ScanQuantOpVal
  Character(len=20), External                    :: NS_DateTime
  
  Open (unit=InpLun,file=InpFile,Form='Formatted',status='old',IoStat=IoStat)                     
  If (IoStat /= 0) Then
    Write (DefOut,'(a)') '      <F> '//Trim(InpFile)//' file could not be opened'
    Stop
  End If
  Write (DefOut,'(a)') '  Loading input file: '//Trim(InpFile)

  iDS = 1
  iCard = 0
  Nseen = .FALSE.
  DS_nCards(1:2) = 0
  LoopDS : Do While (.TRUE.)
    Read (InpLun,'(a)',IoStat=IoStat) Card
    If (IoStat /= 0) Then
      If (iDS > 0) iDS=iDs-1
      Exit LoopDS
    End If
    iCard = iCard+1
    DS_nCards(iDS) = DS_nCards(iDS) +1
    DS_Cards(iDS,DS_nCards(iDS)) = Card
!   END record -------------------------------------------------------------------
    If (Card == '     ') Then
      If (Nseen) Then
        Write (DefOut,'(a)') '    Finished reading data set'
!     ENSDF file has consequtive blank/END record
      Else If (iCard == 1) Then
        Write (DefOut,'(a)') '      <F> Check input file, more than one blank/END records was found!'
        Stop
      Else
!     No N-record was found
        Write (DefOut,'(a)') '      <F> No N-record found!'
        Stop
      End If
      iDS = iDS+1
      If (iDS == 4) Exit  LoopDS
      Nseen = .FALSE.
      iCard = 0
    End if
!   IDENTIFICATION record ===============================================================
    If (Card(1:5) /= ' ' .and. Card(6:9) == '    ') Then
        DSID(iDS) = Card(10:39)
        Call UpStr(DSID(iDS))
 ! 2001TuZZ: ENSDF formats document and FMTCHK only has 14:               DS%DecType
                                                                     !   0 => UNKNOWN, 
                                                                     !   1 => A, 
                                                                     !   2 => P
                                                                     !   3 => N
                                                                     !   4 => B-
                                                                     !   5 => B+/EC
                                                                     !   6 => IT
                                                                     !   7 => SF
                                                                     !   8 => B-N
                                                                     !   9 => B-P/B+P/ECP
                                                                     !  10 => B-A/B+A/ECA
                                                                     !  11 => B-2N
                                                                     !  12 => B+2P/EC2P
                                                                     !  13 => Multiparticle decay
                                                                     !  14 => Heavy ion (e.g. 14C)
   
       If (IndexF(DSID(iDS),1,' A DECAY') > 0) Then
          DecType(iDS) = 1
       Else If (IndexF(DSID(iDS),1,' P DECAY') > 0) Then
          DecType(iDS) = 2
       Else If (IndexF(DSID(iDS),1,' N DECAY') > 0) Then
          DecType(iDS) = 3
       Else If (IndexF(DSID(iDS),1,' B- DECAY') > 0) Then
          DecType(iDS) = 4
       Else If (IndexF(DSID(iDS),1,' B+ DECAY') > 0) Then
          DecType(iDS) = 5
       Else If (IndexF(DSID(iDS),1,' EC DECAY') > 0) Then
          DecType(iDS) = 5
       Else If (IndexF(DSID(iDS),1,' IT DECAY') > 0) Then
          DecType(iDS) = 6
       Else If (IndexF(DSID(iDS),1,' SF DECAY') > 0) Then
          DecType(iDS) = 7
       Else If (IndexF(DSID(iDS),1,' B-N DECAY') > 0) Then
          DecType(iDS) = 8
       Else If (IndexF(DSID(iDS),1,' B-P DECAY') > 0) Then
          DecType(iDS) = 9
       Else If (IndexF(DSID(iDS),1,' B+P DECAY') > 0) Then
          DecType(iDS) = 9
       Else If (IndexF(DSID(iDS),1,' ECP DECAY') > 0) Then
          DecType(iDS) = 9
       Else If (IndexF(DSID(iDS),1,' B-A DECAY') > 0) Then
          DecType(iDS) = 10
       Else If (IndexF(DSID(iDS),1,' B+A DECAY') > 0) Then
          DecType(iDS) = 10
       Else If (IndexF(DSID(iDS),1,' ECA DECAY') > 0) Then
          DecType(iDS) = 10
       Else If (IndexF(DSID(iDS),1,' B-2N DECAY') > 0) Then
          DecType(iDS) = 11
       Else If (IndexF(DSID(iDS),1,' B+2P DECAY') > 0) Then
          DecType(iDS) = 12
       Else If (IndexF(DSID(iDS),1,' EC2P DECAY') > 0) Then
          DecType(iDS) = 12
       Else
          Write (DefOut,'(a)') '    <E> It is not a valid Decay data set: '//Trim(DSID(iDS))
          Write (DefOut,'(a)') '        Valid decay data sets:'
          Write (DefOut,'(a)') '        Valid decay data sets:'
          Write (DefOut,'(a)') '          A DECAY'
          Write (DefOut,'(a)') '          P DECAY'
          Write (DefOut,'(a)') '          N DECAY'
          Write (DefOut,'(a)') '          B- DECAY'
          Write (DefOut,'(a)') '          B+/EC DECAY'
          Write (DefOut,'(a)') '          IT DECAY'
          Write (DefOut,'(a)') '          SF DECAY'
          Write (DefOut,'(a)') '          B-N DECAY'
          Write (DefOut,'(a)') '          B-P/B+P/ECP DECAY'
          Write (DefOut,'(a)') '          B-A/B+A/ECA DECAY'
          Write (DefOut,'(a)') '          B-2N DECAY'
          Write (DefOut,'(a)') '          B+2P/EC2P DECAY'
          Stop
       End If
      Write (DefOut,'(a)') '    Data set: '//Trim(DSID(iDS))
    End If
!   NORMALIZATION record ================================================================
    If (Card(1:5) /= ' ' .and. Card(6:9) == '  N ') Then
      If (Nseen) Then
        Write (DefOut,'(a)') '      <F> Only ONE normalization record per data set allowed'
        Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
        Stop
      End If
      Nseen = .TRUE.
      Call UPSTR(Card)
      Line = ' '
      NucId = Card(1:5)
!     Extract NR -----------------------------------------------------------------
       cNR(iDS) = Card(10:19)
      cDNR(iDS) = Card(20:21)
      Call LbSup(cNR(iDS))
      Call LbSup(cDNR(iDS))
      Call UpStr(cDNR(iDS))
      If (cNR(iDS) /= ' ') Then
        Select Case (cDNR(iDS))
        Case ('LT', 'LE', 'GT', 'GE', 'AP' )
          Write (DefOut,'(a)') '      <F> DNR not numerical'
          Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
          Stop
        Case ('  ')
        Case Default
          If (TypStr(Trim(cDNR(iDS))) /= 1 ) Then
            Write (DefOut,'(a)') '      <F> DNR not numerical'
            Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
            Stop
          End If
        End Select
        Call CNVS2U(cNR(iDS),cDNR(iDS),NR(iDS),DNR(iDS))
        If (NR(iDS) <= 0.0) Then
          Write (DefOut,'(a)') '      <F> Negative  NR not allowed'
          Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
          Stop
        End If
      Else 
!         Write (DefOut,'(a)') '      <W> No NR given'
      End If
!     Extract NT ---------------------------------------------------------------
       cNT(iDS) = Card(22:29)
      cDNT(iDS) = Card(30:31)
      Call LbSup(cNT(iDS))
      Call LbSup(cDNT(iDS))
      If (cNT(iDS) /= ' ') Then  
        Select Case (cDNT(iDS))
        Case (' ') 
        Case ('LT', 'LE', 'GT', 'GE', 'AP')
          Write (DefOut,'(a)') '      <F> DNT not numerical'
          Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
          Stop
        Case Default
          If (TypStr(Trim(cDNT(iDS))) /= 1 ) Then
            Write (DefOut,'(a)') '      <F> DNT not numerical'
            Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
            Stop
          End If
        End Select
        Call CNVS2U(cNT(iDS),cDNT(iDS),NT(iDS),DNT(iDS))
        If (NT(iDS) <= 0.0) Then
          Write (DefOut,'(a)') '      <F> Negative  NT not allowed'
          Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
          Stop
        End If
      Else 
!         Write (DefOut,'(a)') '      <W> No NT given'
      End If
!     Extract BR ---------------------------------------------------------------
       cBR(iDS) = Card(32:39)
      cDBR(iDS) = Card(40:41)
      Call LbSup(cBR(iDS))
      Call LbSup(cDBR(iDS))
      If (cBR(iDS) /= ' ') Then
        If (cDBR(iDS) /= ' ') Then
          If (cDBR(iDS) == 'LT' .or. cDBR(iDS) == 'LE' .or. &
              cDBR(iDS) == 'GT' .or. cDBR(iDS) == 'GE' .or. & 
              cDBR(iDS) == 'AP' .or. TypStr(Trim(cDBR(iDS))) /= 1 ) Then
            Write (DefOut,'(a)') '      <F> DBR not numerical'
            Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
            Stop
          End If
        End If
        Call CNVS2U(cBR(iDS),cDBR(iDS),BR(iDS),DBR(iDS))
        If (BR(iDS) <= 0.0) Then
          Write (DefOut,'(a)') '      <F> Negative  BR not allowed'
          Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
          Stop
        End If
      Else 
!         Write (DefOut,'(a)') '      <W> No BR given'
      End If
!     Extract NB ---------------------------------------------------------------
       cNB(iDS) = Card(42:49)
      cDNB(iDS) = Card(50:55)
      Call LbSup(cNB(iDS))
      Call LbSup(cDNB(iDS))
!     Only needed for B-, B+, EC decay data sets
      Select Case (DecType(iDS))
      Case (4,5,8,9,10,11,12)
        If (cNB(iDS) /= ' ') Then
          If (cDNB(iDS) /= ' ') Then
            If (cDNB(iDS) == 'LT' .or. cDNB(iDS) == 'LE' .or. &
                cDNB(iDS) == 'GT' .or. cDNB(iDS) == 'GE' .or. & 
                cDNB(iDS) == 'AP' .or. TypStr(Trim(cDNB(iDS))) /= 1 ) Then
              Write (DefOut,'(a)') '      <F> DNB not numerical'
              Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
              Stop
            End If
          End If
          Call CNVS2U(cNB(iDS),cDNB(iDS),NB(iDS),DNB(iDS))
          If (NB(iDS) <= 0.0) Then
            Write (DefOut,'(a)') '      <F> Negative  NB not allowed'
            Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
            Stop
          End If
        Else 
           Write (DefOut,'(a)') '      <W> No NB given, assumed NB=1'
            NB(iDS) = 1.0
           DNB(iDS) = 0.0
        End If
      Case (1,2,3,6,7)
            NB(iDS) = 1.0
           DNB(iDS) = 0.0
      End Select
!     Cascading gamma=rays -------------------------------------------------------------------------
      CascadeG(iDS) = Card(80:80)
      Call UpStr(CascadeG(IDS))
!     Blank (No Cascading gamma or "C" allowed
      Select Case (CascadeG(IDS))
      Case (' ','C')
      Case Default
        Write (DefOut,'(a)') '      <F> Unexpected character in column 80 (cascade gamma-rays)'
        Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
        Stop
      End Select
      
    End If
!   NORMALIZATION continuation record ===================================================
    If (Card(1:5) /= ' ' .and. Card(6:9) == '2 N ') Then
      If (.NOT. Nseen) Then
        Write (DefOut,'(a)') '      <F> No  normalization record was seen'
        Write (DefOut,'(i8,a)')   iCard,' Card: '//Trim(Card)
        Stop
      End If
!     Extract IB0 ---------------------------------------------------------------
      lIGS = Index(Card(10:), 'IGS=')
      If (lIGS > 0) Then
        Card(1:10+lIGS+2) = ' '
        Call LbSup(Card)
        lBL = Index(Card(1:), ' ')
         cIGS(iDS) = Card(1:Min0(LenStr(Card),LBL))
        cDIGS(iDS) = Card(LBL+1:lBL+2)
        Call CNVS2U(cIGS(iDS),cDIGS(iDS),IGS(iDS),DIGS(iDS))
      End If
    End If
!   GAMMA record ========================================================================
    If (Card(1:5) /= ' ' .and. Card(6:9) == '  G ') Then
    End If
  End Do LoopDS
  nDS = iDS
  Close (InpLun)
  Return
  End