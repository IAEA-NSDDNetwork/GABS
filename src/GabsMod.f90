  Module GabsMod
  Character(len=*),Parameter                               :: Version = 'GABS Version 12 [20-Jun-2021]'
  Integer(kind=4), Parameter                               :: DefIn  = 5
  Integer(kind=4), Parameter                               :: DefOut = 6
  Integer(kind=4), Parameter                               :: InpLun = 10
  Integer(kind=4), Parameter                               :: NewLun = 20
  Integer(kind=4), Parameter                               :: RptLun = 30
  Integer(kind=4), Parameter                               :: MaxCards = 10000
  Integer(kind=4), Parameter                               :: MaxDS = 2
  Integer(kind=4), Parameter                               :: MaxCG = 100
  Integer(kind=4), Parameter                               :: MaxFileName = 132
  Real(kind=4), Parameter                                  :: BrIccUnc = 0.014
  Character(len=MaxFileName), Public                       :: InpFile = ' '
  Character(len=MaxFileName), Public                       :: NewFile = ' '
  Character(len=MaxFileName), Public                       :: RptFile = ' '
  Integer(kind=4), Public                                  :: nDS = 0             ! Number of data sets in the file
  Character(len=80), Dimension(MaxDS,MaxCards), Public     :: DS_Cards = ' '
  Integer(kind=4), Dimension(MaxDs), Public                :: DS_nCards = 0
  Real(kind=4), Public                                     ::  DStot
  Real(kind=4), Public                                     ::   Stot
  Real(kind=4), Public                                     ::  SStot
  Real(kind=4), Public                                     :: DSStot
  Real(kind=4), Public                                     :: g2
  Real(kind=4), Public                                     :: Snor
  Character(len=5), Public                                 :: NUCID
  Character(len=30), Dimension(MaxDS), Public              :: DSID 
! 2001TuZZ: ENSDF formats document and FMTCHK only has 14:               DecType
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
   
  Integer(kind=4), Dimension(MaxDS), Public                :: DecType = 0    ! Decay mode 
! Multiplier for converting relative photon intensity (RI in the GAMMA record) to photons per 100 decays 
!   of the parent through the decay branch 
  Real(kind=4), Dimension(MaxDS), Public                   ::   NR           ! 
  Real(kind=4), Dimension(MaxDS), Public                   ::  DNR
  Character(Len=10), Dimension(MaxDS), Public              ::  cNR = ' '     ! Card(10:19)
  Character(Len= 2), Dimension(MaxDS), Public              :: cDNR = ' '     ! Card(20:21)
! Multiplier for converting relative transition intensity (including conversion electrons) [TI in the 
!   GAMMA record to transitions per 100 decays of the parent through this decay branch
  Real(kind=4), Dimension(MaxDS), Public                   ::   NT
  Real(kind=4), Dimension(MaxDS), Public                   ::  DNT
  Character(Len= 8), Dimension(MaxDS), Public              ::  cNT = ' '     ! Card(22:29)
  Character(Len= 2), Dimension(MaxDS), Public              :: cDNT = ' '     ! Card(30:31)
! Branching ratio multiplier for converting intensity per 100 decays through this decay branch to 
!   intensity per 100 decays of the parent nuclide.
  Real(kind=4), Dimension(MaxDS), Public                   ::   BR
  Real(kind=4), Dimension(MaxDS), Public                   ::  DBR
  Character(Len=8), Dimension(MaxDS), Public               ::  cBR = ' '     ! Card(32:39)
  Character(Len= 2), Dimension(MaxDS), Public              :: cDBR = ' '     ! Card(40:41)
! Multiplier for converting relative b- and EC intensities (IB in the B- record; IB, IE, TI in the EC record) 
!   to intensities per 100 decays through this decay branch.
  Real(kind=4), Dimension(MaxDS), Public                   ::   NB
  Real(kind=4), Dimension(MaxDS), Public                   ::  DNB
  Character(Len= 8), Dimension(MaxDS), Public              ::  cNB = ' '     ! Card(42:49)
  Character(Len= 6), Dimension(MaxDS), Public              :: cDNB = ' '     ! Card(50:55)
! Multiplier for converting per hundred delayed-transition intensities to per hundred decays of precursor
  Real(kind=4), Dimension(MaxDS), Public                   ::   NP
  Real(kind=4), Dimension(MaxDS), Public                   ::  DNP
  Character(Len= 7), Dimension(MaxDS), Public              ::  cNP = ' '     ! Card(56:62)
  Character(Len= 2), Dimension(MaxDS), Public              :: cDNP = ' '     ! Card(63:64)
!   Total Alpha, Beta, EC decay intensity to the ground state in absolute (%) scale
  Real(kind=4), Dimension(MaxDS), Public                   ::   IGS = 0.0
  Real(kind=4), Dimension(MaxDS), Public                   ::  DIGS = 0.0
  Real(kind=4), Dimension(MaxDS), Public                   ::  DIGS_L = 0.0
  Real(kind=4), Dimension(MaxDS), Public                   ::  DIGS_H = 0.0 
  Character(Len= 8), Dimension(MaxDS), Public              ::  cIGS = ' '     ! N continuation Card
  Character(Len= 6), Dimension(MaxDS), Public              :: cDIGS = ' '     ! 
!
  Real(kind=4),  Public                                    ::   N = 0.0      ! N= NR*NR
  Real(kind=4), Dimension(MaxDS), Public                   ::  G = 0.0
  Real(kind=4), Dimension(MaxDS), Public                   :: DG = 0.0
!
  Integer(kind=4), Public                                  :: nGNorm = 0     ! # of Gamma rays used fro normalisation
  Character(Len=1), Dimension(MaxDS), Public               :: CascadeG       ! Card(80:80) Cacade of Gamma rays
  Integer(kind=4), Dimension(MaxDS), Public                :: nCas = 0
  Real(kind=4), Public                                     :: ru              
  Character(len=2), Public                                 :: GabsMode = 'F' ! F - normal operation; i.e. fit NR or BR
                                                                             ! M - mark G-transitions feeding to the g.s.; NO fit is made
                                                                             ! C - Calculate TI using NR and BR from the input file; NO fit is made
! RI ----------------------------------------------------------------
  Real(kind=4), Public                                     ::   RI           ! Card(22-29)
  Real(kind=4), Public                                     ::  DRI           ! Card(30:31)
  Character(len=8), Public                                 ::  cRI = ' '
  Character(len=2), Public                                 :: cDRI = ' '
  Character(len=2), Public                                 :: oDRI = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! %IG ----------------------------------------------------------------
  Real(kind=4), Public                                     ::   IG           ! Absolute photon intensity
  Real(kind=4), Public                                     ::  DIG           ! 
  Character(len=8), Public                                 ::  cIG = ' '
  Character(len=2), Public                                 :: cDIG = ' '
  Character(len=2), Public                                 :: oDIG = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! CC ----------------------------------------------------------------
  Real(kind=4), Public                                     ::   CC           ! Card(56-62)
  Real(kind=4), Public                                     ::  DCC           ! Card(63-64)
  Character(len=8), Public                                 ::  cCC = ' '
  Character(len=2), Public                                 :: cDCC = ' '
  Character(len=2), Public                                 :: oDCC = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! TI ----------------------------------------------------------------
  Real(kind=4), Public                                     ::   TI = 0.0      ! Card(65-74)
  Real(kind=4), Public                                     ::  DTI = 0.0      ! Card(75-76)
  Character(len=10), Public                                ::  cTI = ' '
  Character(len=2), Public                                 :: cDTI = ' '
  Character(len=2), Public                                 :: oDTI = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
!
  Real(kind=4), Dimension(MaxDS), Public                   ::  S               ! summed total intensity of transitions going to the ground state       
  Real(kind=4), Dimension(MaxDS), Public                   :: DS               ! uncertaity on S    
  Real(kind=4), Dimension(MaxDS), Public                   ::  SS       
  Real(kind=4), Dimension(MaxDS), Public                   :: DSS       
  Real(kind=4), Dimension(MaxDS), Public                   ::  SBTOT        
  Real(kind=4), Dimension(MaxDS), Public                   :: DSBTOT        
  Real(kind=4), Dimension(MaxDS), Public                   :: DSRTOT           
  Real(kind=4), Dimension(MaxDS), Public                   :: DBRR                                                      
  Real(kind=4), Dimension(MaxDS,MaxDS), Public             ::  SB
  Real(kind=4), Dimension(MaxDS,MaxDS), Public             :: DSB
  Real(kind=4), Dimension(MaxDS,MaxDS), Public             :: DSR
!  DATA DBRR,SBTOT,DSBTOT,DSRTOT/3*0.0,3*0.0,3*0.0,3*0.0/    
! Clibration transitions -----------------------------------------------
  Type CGtype
! RI ----------------------------------------------------------------
    Real(kind=4)                                         ::   RI = 0.0     ! Card(22-29)
    Real(kind=4)                                         ::  DRI = 0.0     ! Card(30:31)
    Character(len=8)                                     ::  cRI = ' '
    Character(len=2)                                     :: cDRI = ' '
    Character(len=2)                                     :: oDRI = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! %IG ----------------------------------------------------------------
    Real(kind=4)                                         ::   IG = 0.0     ! Absolute photon intensity
    Real(kind=4)                                         ::  DIG = 0.0     ! 
    Character(len=8)                                     ::  cIG = ' '
    Character(len=2)                                     :: cDIG = ' '
    Character(len=2)                                     :: oDIG = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! CC ----------------------------------------------------------------
    Real(kind=4)                                         ::   CC = 0.0     ! Card(56-62)
    Real(kind=4)                                         ::  DCC = 0.0     ! Card(63-64)
    Character(len=8)                                     ::  cCC = ' '
    Character(len=2)                                     :: cDCC = ' '
    Character(len=2)                                     :: oDCC = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
! TI ----------------------------------------------------------------
    Real(kind=4)                                         ::   TI = 0.0      ! Card(65-74)
    Real(kind=4)                                         ::  DTI = 0.0      ! Card(75-76)
    Character(len=10)                                    ::  cTI = ' '
    Character(len=2)                                     :: cDTI = ' '
    Character(len=2)                                     :: oDTI = ' '      ! '  ','SU', 'LT','LE','GT','GE','AP'
!
    Integer(kind=4)                                      :: iDS = 0         ! Data set
    Integer(kind=4)                                      :: iC = 0          ! Card number in the file
  End Type
!
  Type(CGType), Dimension(MaxCG,MaxDS)                   :: CG
  Integer(kind=4), Dimension(MaxDS)                      :: nCG = 0         ! calibration Gammas for each DS
  End Module GabsMod
