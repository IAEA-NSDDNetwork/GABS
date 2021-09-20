  Subroutine Calc_BTI(j0, BTIa,BTIb,BCCRIa,BCCRIb)
  Use GabsMod
  Implicit None
! Dummy arguments
  Integer(kind=4)                                :: t             ! Decay channel index
  Integer(kind=4)                                :: j0            ! Calibration transition index of [1:nCG(t)]
                                                                  !   = 0  sDGTI will be evaluated for all nCG(t) transitions
                                                                  !   > 0  sDGTI will exclude j0-th transition
  Real(kind=4), Dimension(MaxDS)                 :: BTIa          ! BTIa = sum{j} (TI(j,1)*TI(j,2))**2
  Real(kind=4), Dimension(MaxDS)                 :: BTIb          ! BTIb = sum{j} (TI(j,1)*DTI(j,2))**2
  Real(kind=4), Dimension(MaxDS)                 :: BCCRIa        !
  Real(kind=4), Dimension(MaxDS)                 :: BCCRIb        ! 

! Local variables
  Integer(kind=4)                                :: j
  
  Do t= 1, nDS
      BTIa(t) = 0.0
      BTIb(t) = 0.0
      BCCRIa(t)=0.0
      BCCRIb(t)=0.0
      Do j = 1, nCG(t)
          BTIa(t) = BTIa(t) + CG(j,t)%TI 
          IF (cG(j,t)%IC == j0) Cycle
          BTIb(t) = BTIb(t) + CG(j,t)%DTI**2
          BCCRIa(t) = BCCRIa(t) + (CG(j,t)%RI*CG(j,t)%DCC)**2
          BCCRIb(t) = BCCRIb(t) + ((1.+CG(j,t)%CC)*CG(j,t)%DRI)**2
      End Do
  End Do
  
  Return
  End