  Subroutine Extension(OldName, Ext, NewName)
!
!      this Subroutine replaces the Extension of the OldName with Ext and Returnes the name in NewName
!
!      OldName and NewName have to be declared as Character*.. OldName
!
!      If the NewName array is shorter than the OldName, you may have a problem
!      the Subroutine checks it and truncates the Extension If necessary
!
  Implicit None
! Subroutine arguments --------------------------------------------------------
  Character(len=*)                                  :: OldName
  Character(len=*)                                  :: NewName
  Character(len=*)                                  :: Ext
! Local variables -------------------------------------------------------------
  Integer(kind=4)                                   :: lOld, lNew, lExt, Leng, lPoInt
  Integer(kind=4), External                         :: lString
  Integer(kind=4), External                         :: IndexF

  lOld = Len(OldName)                                    ! Length of array OldName
  lNew = Len(NewName)                                    ! Length of array NewName
  lExt = Len(Ext)                                        ! Length of array Ext
  Leng = lString(OldName)                                ! get number of Characters
  lPoint = INDEXF(OldName,1,'.')
  If ((lExt+lPoInt)>lNew) lPoInt = lNew-lExt             ! check for Length problems
  NewName(1:lNew)=OldName(1:lPoInt-1)//'.'//Ext(1:lExt)  ! merge the names
  Return
  End
