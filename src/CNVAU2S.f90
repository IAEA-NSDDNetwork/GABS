   Subroutine CNVAU2S(Y,DYpos,DYneg,SX,LenX,sDXpos,sDXneg,LenDX)
!  Converts the real number Y (or X for double prec), with
!  optional assymmetric uncertainties DYpos and DYned 
!  (or DXpos and DXneg for double prec),
!  into string format.  One of four formats is selected based
!  on the values of DY(or DX) and LenDX.
!
!   Y       is the input real number to be converted.
!  DY       is the input real uncertainty in Y.
!   X       is the double precision number to be converted.
!  DXpos    is the double precision positive uncertainty in X.
!  DXneg    is the double precision negative uncertainty in X.
!           NOTE Sign(DXneg) = +1
!  SX       is the output string for X (and in format 2 also DX).
!  LenX     is the input length specifier for SX.
!  sDXpos   is the output string for DXpos (formats 1 and 3 only).
!  sDXneg   is the output string for DXneg (formats 1 and 3 only).
!  LenDX    is the input length specifier for sDXpos (formats 1 and 3).
!     or a format flag (format 2 and 4).
!
! FORMAT 1:  DXpos > 0.0, DXneg < 0.0, LenDX > 0.
!            SX and sDXpos and sDXneg are set.
!            The smallest of sDXpos and sDXneg will be in the range 1 to 35;
!            the other will be set at the same scale. This procedure similar to
!            the one used for symmetric uncertainties (CNVU2S) 
!            The three highest order digits of the error lie between 
!            (a) 100  and 354, we round to two significant digits. 
!            (b) 355 and 949, we round to one significant digit. 
!            (c) 950 and 999, we round up to 1000 and keep two significant digits
!            K. Nakamura et al., JPG 37, 075021 (2010) (http://pdg.lbl.gov)

!            However, if largest uncertainty more than 3 digits, the scaling factor 
!            will be reduced until this uncertainty will be within 3 digits. In
!            this case the rounded value of the smallest uncertainty will be "1"
    
!
!            SX will be set as appropriate for the specified uncertainty.
!
!     FORMAT 2:  DX > 0.0, LenDX <= 0.
!     SX only is set, sDXpos is not modified.
!     X and DX are formatted into SX.  The uncertainty is not
!     constrained to the range 1 to 25 if DX > 25.0.
!     If LenDX=0, results will be set to the "natural" number of
!     significant Digits.
!     If LenDX<0, results will be set to -LenDX significant
!     Digits.
!
!     FORMAT 3:  DX = 0.0, LenDX >= 0.
!     SX and sDXpos are set.
!     SX will be set using 4 significant Digits.
!     sDXpos will be blanked out to a length of LenDX.
!
!     FORMAT 4:  DX = 0.0, LenDX < 0.
!     SX only is set, sDXpos is not modified.
!     SX will be set using -LenDX significant Digits.
  Implicit None
! Subroutine arguments
  Real(Kind=4)                                   :: Y, DYpos, DYneg
  Real(kind=8)                                   :: Z, DZpos, DZneg
  Integer(Kind=4)                                :: LenDX, LenX
  Character(Len=*)                               :: sDXpos, sDXneg, SX
! Local variables
  Real(kind=8)                                   :: X, DX1, DX2, wDX1    ! Index 1 for smaller Uncertainty of 
                                                                       !  [DYpos,DYneg] or [DZpos,DZneg]
  Logical(Kind=4) PosNeg         ! = TRUE if DYpos/DZpos is the smallest
  Character(Len=10) sDX1, sDX2   ! temporary character storages
  Real(kind=8)                                   :: T
      Integer(Kind=4) :: i,iblk,iPwr,iSig,iX,LenXt
      Integer(Kind=4) :: iDX1, iDX2
      Integer(Kind=4) :: INT
      Real(Kind=4), Intrinsic :: FLOAT
      Real(Kind=8), Intrinsic :: dAbs,DBLE,DLOG10,DSIGN
      Logical(Kind=4),External :: IVRFLW
      Integer(Kind=4),External :: LENSTR, TYPSTR
      Character(Len=10) :: Temp
!     ENTRY for CNVU2S (single precision)
      X = Y
      If ( DYpos <= Abs(DYneg)) Then
        DX1 = DYpos
        DX2 = Abs(DYneg)
        PosNeg = .TRUE.
      Else
        DX2 = DYpos
        DX1 = Abs(DYneg)
        PosNeg = .FALSE.
      End If
      sDXpos = ' '
      sDXneg = ' '
      Go To 10
!     ENTRY for DCNVUS (double precision)
      ENTRY DCNVAUS(Z,DZpos,DZneg,SX,LenX,sDXpos,sDXneg,LenDX)
      X = Z
      If ( DZpos <= dAbs(DZneg)) Then
        DX1 = DZpos
        DX2 = dAbs(DZneg)
        PosNeg = .TRUE.
      Else
        DX2 = DZpos
        DX1 = dAbs(DZneg)
        PosNeg = .FALSE.
      End If
      sDXpos = ' '
      sDXneg = ' '
!--   Determine formats based on values of DX1 and LenDX.
   10 If (DX1 <= 0.0) Then
!--      Format 3:  SX has 4 sig. Digits, sDX1 blanked.
!--      Format 4:  SX has -LenDX sig. Digits, sDX1 untouched.
         iSig = 4
         If (LenDX < 0) iSig = -LenDX
!        ...FIND PROPER iPwr.
         T = 0.0D0
         If (dAbs(X) > 1.0D-35) T = DLOG10(dAbs(X))
         If (T < 0.0D0) T = T - 1.0D0
         iPwr = INT(T) - iSig + 1
!        Check I there will be an Integer overflow I SCALX is
!        Called
         If (IVRFLW(X,iPwr)) Then
            SX = '*************************************'
            If (LenDX == 0) Then
              sDXpos = ' '
              sDXneg = ' '
            End If
            Return
         End If
         Call SCALX(X,iX,iPwr)
         Call KNVIX(iX,iPwr,SX,LenX)
         If (LenDX >= 0) Then
            sDX1 = ' '
            sDX1 = ' '
         End If
      Else If (LenDX > 0) Then
!--      FORMAT 1:  SX, sDX1 (1, 35).
!       Find appropriate power for DX to be converted to a 3 digits inteter ===================
        wDX1 = DX1
        iPwr = 0
!       wDX1 < 1.0, MULTIPLY by 10.0.
        Do While (wDX1 < 100.)
          wDX1 = wDX1*10.0
          iPwr = iPwr - 1
        End Do
!       wDX1 > 1000.0, DIVIDE by 10.0.
        Do While (wDX1 > 999.0)
          wDX1 = wDX1/10.0
          iPwr = iPwr + 1
        End Do
        iDX1=iFix(Sngl(wDX1))
        Select Case (iDX1)
!       two digits
        Case (100:354)
          iDX1 = nInt(dFloat(iDX1)/10.)
          iPwr = iPwr +1
        Case (355:949)
          iDX1 =nInt(dFloat(iDX1)/100.)
          iPwr = iPwr +2
        Case (950:999)
          iDX1 =10
          iPwr = iPwr +2
        Case Default
        End Select
        iDX2 = nInt(DX2*10.0D0**(-iPwr))
        Do While (iDX2 > 999)
          iPwr = iPwr +1
          iDX2 = nInt(DX2*10.0D0**(-iPwr))
          iDX1 = Max0(nInt(DX1*10.0D0**(-iPwr)),1)
        End Do
        If (IVRFLW(X,iPwr)) Then
            SX = '*************************************'
            sDXpos = '*************************************'
            sDXneg = '*************************************'
            Return
         End If
         Call SCALX(X,iX,iPwr)
         Select Case (LenDX)
         Case ( 1: 9)
           Call KNVIX(iX,iPwr,SX,LenX)
           Call KNVI2S(iDX1,SDX1,LenDX)
           Call KNVI2S(iDX2,SDX2,LenDX)
         Case (11:19)
           Call KNVISX(iX,iPwr,SX,LenX)
           Call KNVI2S(iDX1,SDX1,LenDX-10)
           Call KNVI2S(iDX2,SDX2,LenDX-10)
         Case Default
            SX = '*************************************'
            SDXpos = '*************************************'
            SDXneg = '*************************************'
            Return
         End Select
      Else
!--      FORMAT 2:  SX only (sDXpos included).
         iPwr = 0
         iDX1 = DX1
         i = 1
         Do While (dAbs((DX1-FLOAT(iDX1))/DX1) > 1.0D-4)
            iPwr = iPwr - 1
            DX1 = DX1*1.0D+1
            iDX1 = INT(DX1+0.9)
            i = i + 1
!           Not converging - abort nicely
            If (i > 100) Then
               SX = '*************************************'
               Return
            End If
         End Do
         Call KNVI2S(iDX1,Temp,0)
         Do While (.TRUE.)
!           LenDX less than zero indicates number of significant Digits
!           to retain. I LenDX=0, than default to "natural" number
!           of significant Digits
            If (LenDX < 0 .and. LENSTR(Temp) /= -LenDX) Then
               If (LenSTR(Temp) < -LenDX) Then
                  iPwr = iPwr - 1
                  DX1 = DX1*10.0
                  iDX1 = INT(DX1+0.9)
                  DX2 = DX2*10.0
                  iDX2 = INT(DX2+0.9)
               End If
               If (LenSTR(Temp) > -LenDX) Then
                  iPwr = iPwr + 1
                  DX1 = DX1/10.
                  iDX1 = INT(DX1+0.9)
                  DX2 = DX1/10.
                  iDX2 = INT(DX2+0.9)
               End If
               Call KNVI2S(iDX1,Temp,0)
               CYCLE
            End If
!           Check I there will be an Integer overflow I SCALX is
!           Called
            If (IVRFLW(X,iPwr)) Then
               SX = '*************************************'
               Return
            End If
            Call SCALX(X,iX,iPwr)
            Call KNVIX(iX,iPwr,SX,LenX)
            If (SX(1:1) /= '*') Then
               Call SQZSTR(SX,' ')
               Call ADDSTR(Temp,1,' ')
               LenXt = LENSTR(SX) + LENSTR(Temp)
               If (LenXt <= LenX) Then
                  iblk = LenX - LenXt
                  SX(LenSTR(SX)+1:LenX) = Temp
                  Do i = 1,iblk
                     Call ADDSTR(SX,1,' ')
                  End Do
               Else
                  SX = '*************************************'
               End If
            End If
            EXIT
         End Do
      End If
!--   Return to calling routine. 
      Call LbSup(sDX1)
      Call LbSup(sDX2)
      If (sDX2 == '0') sDX2= '1'
      If (PosNeg) Then
        sDXpos = '+'//sDX1
        sDXneg = '-'//sDX2
      Else
        sDXpos = '+'//sDX2
        sDXneg = '-'//sDX1
      End If
      Return
      End Subroutine CNVAU2S


