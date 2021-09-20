  Subroutine ReplaceChar(String, From, To, Chr)
! This Subroutine replace character in String FROM TO with Chr
  Implicit NONE
  Character*(*) String
  Integer*4 From, To, i
  Character*(1) Chr
  If (From > To) Return
  Do i=From, To
    String(i:i) = Chr
  End Do
  Return
  End
