  Integer(kind=4) Function ScanQuantOpVal(iString, Quant, strVal, strUnc, Val, UncL, UncH, NsrKey, Remove)
! Reads <Quant><Op><strVal><strUnc><(NsrKey)> value from continuation records
! By Tibor Kibedi (Tibor.Kibedi@anu.edu.au)
!	Department of Nuclear Physics, Research School of Physical Sciences and Engineering
!	Institute of Advanced Studies, The Australian National University
!	Canberra, ACT, 0200, Australia

! Return Values
! = +1 all OK
! =  0 Error
  Implicit None
! Subroutine arguments
  Character*(*) iString                ! Data from Continuation/Comment cards
  Character*(*) Quant                  ! Quantity to search for
  Character*(*) strVal                 ! Value
  Character*(*) strUnc                 ! standard Uncertainty, 12 +2-3, AP, LE, etc
  Real*4 Val                           ! Numerical Value
  Real*4 UncL                          ! uncertainty, negative
  Real*4 UncH                          ! uncertainty, positive
  Character*(*) Nsrkey                 ! Nsr Key Numbers (max of 3, separated by coma)
  Logical*4 Remove                     ! =1; Remove <Quant><Op><strVal><strUnc><(NsrKey)> from string
! Local arguments
  Integer*4 lS, lQ, lSep, iOp, fS, lBL, lOB, lCB, fVal, fUnc, IP, IM
  Character(len=100)                             :: String
  Character*4 Op(11)
  Integer*4  lOp(11)
  Character*70 QuantOp
  Character*7 strL, strH
  Integer*4, External :: LenStr, RlScn
  Data  Op/'=   ','<   ','>   ','<=  ','>=  ',' EQ ',' AP ',' LT ',' LE ',' GT ',' GE '/
  Data lOp/ 1,     1,     1,     2,     2,     4,     4,     4,     4,     4,     4/
  ScanQuantOpVal = 0
!  strVal = ' '
!  strUnc = ' '
!  Val = 0.0
!  UncL = 0.0
!  UncH = 0.0
  String = Trim(iString)
  fVal = 0
  fUnc = 0
  lS = LenStr(String)
! Handle cases when there is a space in front of the $ sign
  Call REPCHR(String,'$','$ ')
  Call REPCHR(String,'$  ','$ ')
  lS = LenStr(String)
  lSep = Index(String(1:lS), '$')
  If (lSep == 0) lSep = lS
  lQ = LenStr(Quant)
! Handle trivial cases
  If (lS+2 < lQ) Return
  LoopOp : Do iOp =1, 11
    fS = lQ + lOp(iOp)
    If (fS+1 > lS) Return
    QuantOp = Quant(1:lQ)//Op(iOp)
    If (String(1:fS) == QuantOp(1:fS)) Then
      QuantOp = String
      Call ReplaceChar(QuantOp, 1, fS, ' ')
      Call LbSup(QuantOp)
!     Look for a separation character
      If (lS > LenStr(QuantOp)) Exit LoopOp
      lSep = Index(QuantOp(1:lS), '$')
      If (lSep > 0) QuantOp(lSep:) = ' '
      lBL = Index(QuantOp(1:LenStr(QuantOp)), ' ')
      lOB = Index(QuantOp(1:LenStr(QuantOp)), '(')
!     Val is given as EKC AP 1.23
      If(lBL == 0 .and. lOB == 0 .and. LenStr(QuantOp) > 0) Then
        fVal = LenStr(QuantOp)+1
!     Val is given as EKC AP 1.23_
      Else If (lBL > 0 .and. LBL < lOB .or. &
               lBL > 0 .and. lOB == 0) Then
        fVal = lBL
!     Val is given as EKC AP 1.23(
      Else If ((lOB > 0 .and. LBL == 0) .or. &
               (lOB > 0 .and. LBL > lOB)) Then
        fVal = lOB
      End If
      If (fVal > 1) Then
        strVal = QuantOp(1:fVal-1)
!     Trap error
      Else
        Exit LoopOp
      End if
      Select Case (iOp)
      Case (1)
!       Must be a Blank character between Strval and StrUnc 
        If (QuantOp(fVal:fVal) == ' ' .and. LenStr(QuantOp) >= fVal+1) Then
!         Look for a separation character
          lBL = Index(QuantOp(fVal+1:LenStr(QuantOp)), ' ')
          lOB = Index(QuantOp(fVal+1:LenStr(QuantOp)), '(')
!         Val&Unc is given as EKC=1.23_+2-3
          If(lBL == 0 .and. lOB == 0 .and. LenStr(QuantOp) > fVal) Then
            fUnc = LenStr(QuantOp)+1
!         Val&Unc is given as EKC=1.23_+2-3_
          Else If (lBL > 0 .and. LBL < lOB ) Then
            fUnc = lBL+fVal
!         Val&Unc is given as EKC AP 1.23_+2-3(
          Else If ((lOB > 0 .and. LBL == 0) .or. &
                  (lOB > 0 .and. LBL > lOB)) Then
            fUnc = lOB+fVal
          End If
          If (fUnc > 1) Then
            strUnc = QuantOp(fVal+1:fUnc-1)
          End If        
        End If
!       Try to convert numbers
        If (LenStr(strVal) == 0 .and. LenStr(strUnc) == 0) Exit LoopOp
!       Look for +/- signs 
        iP = Index(strUnc, '+')                                       
        iM = Index(strUnc, '-')                                       
!       Patch for people who put asymmetric uncertainties in backwards 
        If (iM == 0 .or. iM > iP) Then                              
          StrL = strUnc(iM+1:6)                                         
        Else                                                           
          StrL = strUnc(iM+1:iP-1)                                      
        End If                                                          
        Call SqzStr(StrL, ' ')                                        
        If (iM == 0) Then 
          StrH = StrL                                                 
        Else                                                          
          If(iP < iM) Then                                         
            StrH = strUnc(iP+1:iM-1)                                  
          Else                                                       
            StrH = strUnc(iP+1:6)                                     
          End If                                                      
          Call SqzStr(StrH, ' ')                                     
        End If                                                         
!       Convert assymmetric uncertanties
        Call SqzStr(strVal, ' ')                                       
        Call Cnvs2U(strVal, StrL, Val, UncL)
        Call Cnvs2U(strVal, StrH, Val, UncH)
	 Exit LoopOp
      Case Default
        strUnc = Op(iOp)
	 Exit LoopOp
      End Select
    End If
  End Do LoopOp
! NsrKeyNo
  lOB = Index(QuantOp(1:LenStr(QuantOp)), '(')
  lCB = Index(QuantOp(1:LenStr(QuantOp)), ')')
  If (lCB-LOB >= 8 .and. lCB-LOB <= 26) NsrKey = QuantOp(lOB+1:lCB-1)
! Trap errors
! No Value was given
  If (LenStr(strVal) == 0) Then
    Return
  End if
  If (RlScn(StrVal, 1, Val)  == 0) Then
    Return
  End if
! All OK
  ScanQuantOpVal = +1
! No removal required
  If (.NOT. Remove) Return
! No value found
  If (fVal == 0) Return
  Call DELSTR(String, 1, lSep)
  Return
  End