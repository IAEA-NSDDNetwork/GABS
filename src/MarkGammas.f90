  Subroutine MarkGammas()
! Marks all transition going to the ground state with X (DRI>0) and Y (DRI blank)
! Search criteria: LEVEL energy (LE) should be the same as GAMMA energy (E) within
!   3 times the GAMMA energy uncertainty (DE): Abs(LE-E) <= 3*DE
!   If DE = 0, a 1 keV window was used.
  Use  GabsMod
  Implicit None
  Character(len=80)                              :: Card
  Character(len=132)                             :: Line
  Integer(kind=4)                                :: iiG, iDS, Ini
  Real(kind=4)                                   ::   LE  = 0.0              ! level energy
  Real(kind=4)                                   ::  DLE  = 0.0
  Character(len=10)                              ::  cLE = ' '
  Character(len=2)                               :: cDLE = ' '
  Real(kind=4)                                   ::   E = 0.0          ! transition energy
  Real(kind=4)                                   ::  DE = 0.0
  Character(len=10)                              ::  cE = ' '
  Character(len=2)                               :: cDE = ' '
  Real(kind=4)                                   ::   RIgs = 0.0       ! Summed Relative photon intensity to g.s.
  Real(kind=4)                                   ::  DRIgs = 0.0
  Character(len=8)                               ::  cRIgs = ' '
  Character(len=2)                               :: cDRIgs = ' '
  Real(kind=4)                                   ::   TIw = 0.0       
  Real(kind=4)                                   ::  DTIw = 0.0
  Character(len=10)                              ::  cTIw = ' '
  Character(len=2)                               :: cDTIw = ' '
  Real(kind=4)                                   ::   TIgs = 0.0       ! relative total intensity to g.s.
  Real(kind=4)                                   ::  DTIgs = 0.0
  Character(len=10)                              ::  cTIgs = ' '
  Character(len=2)                               :: cDTIgs = ' '
  Logical(kind=4)                                :: GSseen = .FALSE.  
  Logical(kind=4)                                :: TI_Given = .FALSE.  
  Logical(kind=4)                                :: DTI_Numeric = .TRUE.  
  Logical(kind=4)                                :: DRI_Numeric = .TRUE.  
  Character(len=1)                               :: Flag = ' '
  Integer(kind=4), External                      :: TypStr
  Integer(kind=4), External                      :: LenStr

  Write (RptLun,'(a)') '  * * * '//Trim(Version)//' Report file  * * *'                                 
  Write (DefOut,'(a)') '  Searching for ground state transitions * * * * *'
  Write (RptLun,'(a)') '  Searching for ground state transitions * * * * *'
  Write (DefOut,'(a)') '  ENSDF input file: '//Trim(InpFile)                                                                
  Write (RptLun,'(a)') '  ENSDF input file: '//Trim(InpFile)                                                                
  Write (DefOut,'(a)') '  New ENSDF   file: '//Trim(NewFile)                                                                
  Write (RptLun,'(a)') '  New ENSDF   file: '//Trim(NewFile)                                                                
  Write (DefOut,'(a)') '  Report file:      '//Trim(RptFile)    
  If (nDS == 1) Then
    Write (DefOut,'(a)') '  NR field on the Normalisation record will be blanked'   
  Else
    Write (DefOut,'(a)') '  NR & BR fields on the Normalisation record will be blanked'   
  End If
  LoopDS : Do iDS = 1, nDS
    Write (DefOut,'(a)') ' '
    Write (RptLun,'(a)') ' '
    Write (DefOut,'(a)') '  Data set: '//Trim(DSID(iDS))
    Write (RptLun,'(a)') '  Data set: '//Trim(DSID(iDS))
    Write (DefOut,'(a)') '     Transitions to the G.S.'
    Write (RptLun,'(a)') '     Transitions to the G.S.'
    Write (DefOut,'(a)') '     Level         GE         RI       DRI   TI         DTI  Flag'
    Write (RptLun,'(a)') '     Level         GE         RI       DRI   TI         DTI  Flag'
!                      '                   121.6214      121.6211   60       3  Y
    LoopGamma : Do iiG = 1, DS_nCards(iDS)
      Card = DS_Cards(iDS,iiG)
      Call UPSTR(Card(6:9))
      Select Case (Card(6:9))
!     NORMALISATION record
      Case ('  N ') 
                     Card(10:21) = ' '    ! NR
        If (nDS > 1) Card(32:41) = ' '    ! BR
!     LEVEL record
      Case ('  L ') 
         cLE = Card(10:19)
        cDLE = Card(20:21)
        Call CNVS2U(cLE,cDLE,LE,DLE)
!       1st state should be G.S.
        If (LE == 0.0) Then
          GSseen = .TRUE.
        Else If (.NOT. GSseen) Then
          Write (DefOut,'(a)') '  <F> First level should be the ground state'
          Write (DefOut,'(a)') '     Card: '//Trim(Card)
          Stop
        End If
!     GAMMA record
      Case ('  G ') 
        Flag = ' '
!       Gamma from an excited state
        If (LE > 0.0) Then
           cE = Card(10:19)
          cDE = Card(20:21)
          Call CNVS2U(cE,cDE,E,DE)
!         Check if it going to the GS
          If (Abs(LE-E) <= Max(3*DE,1.0)) Then
             RI = 0.0
            DRI = 0.0
             TI = 0.0
            DTI = 0.0
            cRI = Card(22:29)
            Call LbSup(cRI)
            cDRI = Card(30:31)
            Call LbSup(cDRI)
            Call UPSTR(cDRI)
             cCC = Card(56:62)
            Call LbSup(cCC)
            cDCC = Card(63:64)
            Call LbSup(cDCC)
            Call UPSTR(cDCC)
            cTI = Card(65:74)
            Call LbSup(cTI)
            cDTI = Card(75:76)
            Call LbSup(cDTI)
            Call UPSTR(cDTI)
             cTIw = ' '
            cDTIw = ' '
!           Assign "X" if TI or RI non-zero
!           RI given ==============================================================
            If (cRI /= ' ') Then
              Call CNVS2U(cRI, cDRI, RI, DRI)
              Call CNVS2U(cCC, cDCC, CC, DCC)
              If (cCC /= ' ' .and. cDCC == ' ') Then
                DCC = BrIccUnc*CC
              End If
              Select Case (cDRI)
              Case (' ', 'AP')
                DRI_Numeric = .FALSE.
                Flag = 'Y'
                RIgs =  RIgs+ RI    
                DTIw = 0.0
                If (cTI == ' ') Then
                  TIw = RI*(1.0+CC)
                  If (DCC > 0.0) Then
                    DTIw = (RI*DCC)**2
                    If (Sqrt(DTIw)/TIw > 0.0001) Then
                      Call CNVU2S_n(TIw, Sqrt(DTIw), cTIw, 8, cDTIw, 2)
                    Else
                      Call CNVU2S_n(TIw, TIw*0.03, cTIw, 8, cDTIw, 2)
                      cDTIw = ' '
                    End If
                  Else
                    Call CNVU2S_n(TIw, TIw*0.03, cTIw, 8, cDTIw, 2)
                    cDTIw = ' '
                  End If
                   TIgs =  TIgs +  TIw
                  DTIgs = DTIgs + DTIw
                End If
              Case ('LT','LE')            
                DRI_Numeric = .FALSE.
                Flag = 'Y'
                 RIgs = RIgs + RI
                If (cTI == ' ') Then
                  TIw = RI*(1.0+CC)
                  Call CNVU2S_n(TIw, 0.03*TIw, cTIw, 8, cDTIw, 2)
                  TIgs =  TIgs + TIw
                 DTIgs = DTIgs + (TIw*0.5)**2
                End If
              Case Default
!               RI a FORTRAN/integer number
                If ((TypStr(Trim(cRI)) == -2 .or. TypStr(Trim( cRI)) == 1) .and. &
                                                  TypStr(Trim(cDRI)) == 1)   Then
                   RIgs = RIgs + RI
                  DRIgs = DRIgs + DRI**2
                  If (cTI == ' ') Then
                    TIw = RI*(1.0+CC)
                    DTIw =((1.0+CC)*DRI)**2 + (RI*DCC)**2
                    Call CNVU2S_n(TIw, sqrt(DTIw), cTIw, 8, cDTIw, 2)
                    If (DCC == 0.0 .and. DRI == 0.0) cDTIw = ' '        
                    TIgs = TIgs + TIw
                    DTIgs = DTIgs + DTIw
                  End If
                  Flag = 'X'
                Else
                  Flag = 'Y'
                End If
              End Select
            End If
!           TI given ==============================================================
            If (cTI /= ' ') Then
              Call CNVS2U(cTI, cDTI, TI, DTI)
              TI_given = .TRUE.
              Select Case (cDTI)
              Case (' ') 
                DTI_numeric = .FALSE.
                Flag = 'Y'              
                TIgs =  TIgs+ TI
              Case ('AP') 
                DTI_numeric = .FALSE.
                Flag = 'Y'              
                TIgs =  TIgs+ TI
              Case('LT', 'LE')            
                DTI_numeric = .FALSE.
                Flag = 'Y'    
                TIgs =  TIgs+ TI*0.5
                DTIgs = DTIgs + (TI*0.5)**2
              Case Default
!               TI a FORTRAN/integer number
                If (TypStr(Trim(cTI)) == -2 .or. TypStr(Trim(cTI)) == 1) Then
                   cTIw =  cTI
                  cDTIw = cDTI
                  If (cDTI /= ' ')  Flag = 'X'
                End If
                TIgs =  TIgs+ TI
                DTIgs = DTIgs + DTI**2
              End Select
            End If
            Card(79:79) = Flag
            Line = ' '
            Call LbSup(cLE)
            Call LbSup(cE)
            Call LbSup(cRI)
            Call LbSup(cDRI)
            Call LbSup(cTIw)
            Call LbSup(cDTIw)
            Line(6:) = cLE//'    '//cE//' '//cRI//' '//cDRI//'    '//cTIw//' '//cDTIw//'   '//Flag
            If (cRI == ' ' .and. cTI /= ' ') Then
              Ini = Max0(65, LenStr(Line))
              Line = Line(1:Ini)//'  <W> No RI given!'
            Else If (cRI == ' ' .and. cTI == ' ') Then
              Ini = Max0(65, LenStr(Line))
              Line = Line(1:Ini)//'  <E> No RI or TI given!'
              Flag = ' '
            Else If (cRI /= ' ' .and. cTI == ' ') Then
              Ini = Max0(65, LenStr(Line))
              If (CC > 0.0) Then
                Line = Line(1:Ini)//'  <W> TI calculated from RI & CC'
              Else
                Line = Line(1:Ini)//'  <W> No CC given, TI = RI !'
              End If
            End If
            Write(DefOut,'(a)') Trim(Line)
            Write(RptLun,'(a)') Trim(Line)
            
          End If 
        End If
      Case Default
      End Select
      Card (6:9)=DS_Cards(iDS,iiG)(6:9)
      Write (NewLun,'(a)') Card
    End Do LoopGamma
  End Do LoopDS
  If (RIgs > 0.0 .or. TIgs > 0.0) Then
    Write(DefOut,'(a)') ' '
    Write(RptLun,'(a)') ' '
    Line = '     Summed intensities  '
    If (RIgs > 0.0) Then
      If (DRIgs > 0.0) Then
        DRIgs =Sqrt(DRIgs)
        Call CNVU2S_n(RIgs, dRIgs, cRIgs, 10, cDRIgs, 2)
      Else
        Call CNVU2S_n(RIgs, 0.03*RIgs, cRIgs, 10, cDRIgs, 2)
        cDRIgs = ' '
      End If
      Call LbSup(cRIgs)
      Call LbSup(cDRIgs)
      Line(31:) = cRIgs//' '//cDRIgs
    End If
    If (TIgs > 0.0) Then
      If (DTIgs > 0.0) Then
        DTIgs =Sqrt(DTIgs)
        Call CNVU2S_n(TIgs, dTIgs, cTIgs, 10, cDTIgs, 2)
      Else
        Call CNVU2S_n(TIgs, 0.03*TIgs, cTIgs, 10, cDTIgs, 2)
        cDTIgs = ' '
      End If
      Call LbSup(cTIgs)
      Call LbSup(cDTIgs)
      Line(46:) = cTIgs//' '//cDTIgs
    End If
    Write(DefOut,'(a)') Trim(Line)
    Write(RptLun,'(a)') Trim(Line)
  End If
  Return
  End