  Subroutine PutField(InpStr, OutStr)
! Puts InpStr into OutStr if content has been changed
  Use GabsMod
  Implicit None
! Dummy arguments
  Character(len=*)                               :: InpStr
  Character(len=*)                               :: OutStr
! Local variables
  Integer(kind=4)                                :: LenInp = 0
  Integer(kind=4)                                :: lInp = 0
  Character(len=80)                              :: wInpStr = ' '
  Integer(kind=4)                                :: LenOut = 0
  Integer(kind=4)                                :: lOut = 0
  Character(len=80)                              :: wOutStr = ' '
  Character(len=80)                              :: eStr = ' '
  Integer(kind=4), External                      :: LenStr
  eStr = '********************************************************************************'
  wInpStr = ' '
  LenInp = Len(InpStr)
  wInpStr(1:LenInp) = InpStr
  Call LbSup(wInpStr)
  lInp = LenStr(wInpStr)
  LenOut = Len(OutStr)
  wOutStr(1:LenOut) = OutStr
  Call LbSup(wOutStr)
  lOut = LenStr(wOutStr)
! InpStr blank, nothing to do
  If (lInp == 0) Then
! InpStr and OutStr have the same content; do not change OutStr  
  Else If (lInp == lOut .and. wInpStr(1:lInp) == wOutStr(1:lOut)) Then
  
! InpStr and OutStr have the same length, but differtent content
  Else If (lInp == lOut .and. wInpStr(1:lInp) /= wOutStr(1:lOut)) Then
    OutStr(1:lInp) = wInpStr(1:lInp)
! InpStr shorter than OutStr, insert it into OutStr with padded blank
    Else If (lInp < LenOut) Then
    OutStr = ' '
    OutStr(2:) = wInpStr(1:lInp)
! InpStr longer than OutStr, insert it into OutStr with padded blank
  Else If (lInp >= LenOut) Then
    OutStr =  wInpStr(1:LenOut)
  Else
  End If
  Return
  End