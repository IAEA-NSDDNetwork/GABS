   Subroutine CNVU2S_n(Y,Dy,sX,lenX,sDX,lenDX)
!
!  Converts the Real number Y(or X for double prec), with uncertainty DY(or DX for double rec),
!  into string format according to the following rounding format:
!  The three highest order digits of the error lie between 
!     (a) 100  and 354, we round to two significant digits. 
!     (b) 355 and 949, we round to one significant digit. 
!     (c) 950 and 999, we round up to 1000 and keep two significant digits
    
!   K. Nakamura et al., JPG 37, 075021 (2010) (http://pdg.lbl.gov)
    
!     Y     

!     X     is the double precision number to be converted.
!     DX    is the double precision uncertainty in X.
!     sX    is the output string for X (and in format 2 also DX).

!     sDX   is the output string for DX (formats 1 and 3 only).
! input length specifier for sDX (formats 1 and 3).
!     or a format flag (format 2 and 4).
!
  Implicit None
! Dummy arguments ===================================================================
  Real(Kind=4)                                   ::  Y      ! input Real number to be converted.
  Real(Kind=4)                                   :: DY      ! input Real uncertainty in Y.
  Real(Kind=8)                                   ::  Z
  Real(Kind=8)                                   :: DZ
  Integer(Kind=4)                                ::  lenX   ! input length specifier for sX.
  Integer(Kind=4)                                :: lenDX   ! input length specifier for sDX 
  Character(len=*)                               ::  sX     ! Output string
  Character(len=*)                               :: sDX     ! 
! Local variables ==================================================================
  Integer(kind=4), Parameter                     :: DefOut = 6
  Character(len=*), Parameter                    :: Subr = 'CnvU2S_n'
  Real(Kind=8)                                   :: DX, wDX
  Real(Kind=8)                                   ::  X =0
  Integer(Kind=4)                                :: iDX = 0
  Integer(Kind=4)                                :: ipwr = 0
  Integer(Kind=4)                                :: iX = 0
  Logical(Kind=4), External                      :: IVRFLW
  Integer(Kind=4), External                      :: LENSTR

!  Logical(kind=4), Parameter                     :: Dbg= .TRUE.
  Logical(kind=4), Parameter                     :: Dbg= .FALSE.
  Logical(kind=4), Save                          :: Opened = .FALSE.
  Integer(kind=4), Parameter                     :: DbgLun= 12345
! ENTRY for CNVU2S_n (single precision) ================================================
   X =  Y
  DX = DY
  Go To 10
! ENTRY for DCNVUS_n (double precision) ================================================
  ENTRY DCNVUS_n(Z,DZ,sX,lenX,sDX,lenDX)
   X =  Z
  DX = DZ
10      Continue
! =======================================================================================
! Verifyes input
  If (DX <= 0.0) Then
    Write (DefOut,'(a)') ' <E> '//Subr//' was called with negative or zero DX'
    Return
  Else If (lenX < 5) Then
    Write (DefOut,'(a)') ' <E> lenX is too small in '//Subr
    Return
  Else If (lenDX < 2) Then
    Write (DefOut,'(a)') ' <E> lenDX is too small in '//Subr
    Return
  End If 
! Find appropriate power for DX to be converted to a 3 digits inteter ===================
  wDX = DX
  iPwr = 0
! wDX < 1.0, MULTIPLY by 10.0.
  Do While (wDX < 100.)
    wDX = wDX*10.0
    iPwr = iPwr - 1
  End Do
! wDX > 1000.0, DIVIDE by 10.0.
  Do While (wDX > 999.0)
    wDX = wDX/10.0
    iPwr = iPwr + 1
  End Do
!  iDX =nInt(wDX)
  iDX=iFix(Sngl(wDX))
!  dwDX =wDX - dFloat(iwDX)
  
! Check if there will be an Integer overflow 
  If (IVRFLW(X, iPwr)) Then
     sX = '*************************************'
    sDX = '*************************************'
    Return
  End If
! when IX and IDX are multiple of 10, reduce them by 10--skip this   
  Select Case (iDX)
! two digits
  Case (100:354)
    iDX = nInt(dFloat(iDX)/10.)
    iPwr = iPwr +1
  Case (355:949)
    iDX =nInt(dFloat(iDX)/100.)
    iPwr = iPwr +2
  Case (950:999)
    iDX =10
    iPwr = iPwr +2
  Case Default
  End Select
  Call SCALX(X, iX, iPwr)
  Call KNVIX(iX, iPwr, sX, lenX)
  Call KNVI2S(iDX, sDX, lenDX)
  If (.NOT. Dbg) Return
  If (.NOT. Opened) Then
      Open(Unit=DbgLun,File='Gabs1.py', Form='Formatted', Status='Replace')
      Write (DbgLun,'(a)') 'import uncertainties '
      Write (DbgLun,'(a)') 'from uncertainties import * '
      Write (DbgLun,'(a)') 'from uncertainties.umath import * '
      Write (DbgLun,'(a)') 'import math, sys '
      Write (DbgLun,'(a)') ' '
      Write (DbgLun,'(a)') ' '
      Opened = .TRUE.
  End If
  Write (DbgLun,'(a,f,a,f,a)') 'xdx=ufloat(',X,',',DX,')'
  Call NOLBLANK(sX)
  Call NOLBLANK(sDX)
  Write (DbgLun,'(a,f,a,f,a)') "gabsxdx= '"//Trim(sX)//" "//Trim(sDX)//"  ",X,"  ",DX," '"
  Write (DbgLun,'(a)') "print 'python: ',xdx,'  gabs: ',gabsxdx"
  Return
  End Subroutine CNVU2S_n
!
