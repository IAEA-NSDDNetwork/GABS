! #################################################################################################
! file: DateTime.f90
! Converts Xdate and Xtime into the format (20 char)   
! example: 
! Xtime = 102701.625 
! Xdate = 20050923
! DateTime = 10:27:01 23-Sep-2005
!--------------------------------------------------------------------------------------------------
! by Tibor Kibedi (Tibor.Kibedi@anu.edu.au)
!        Department of Nuclear Physics, Research School of Physical Sciences and Engineering
!        Institute of Advanced Studies, The Australian National University
!        Canberra  ACT  0200, Australia
! [2008Ki07] T. Kibedi, T.W. Burrows, M.B. Trzhaskovskaya, P.M. Davidson, C.W. Nestor Jr.,
!            "Evaluation of theoretical conversion coefficients using BrIcc"
!            Nucl. Instr. and Meth. A v589 (2008) 202, doi:10.1016/j.nima.2008.02.051
!--------------------------------------------------------------------------------------------------
  Character(len=20) Function  NS_DateTime()
  Implicit None
  Character(len=10)                              :: XDate, Xtime
  Character(len=3)                               :: MM
! Standard system call returns
  Call Date_And_Time(Date=Xdate, Time=Xtime)
  Select Case (Xdate(5:6))
  Case ('01')
    MM = 'Jan'
  Case ('02')
    MM = 'Feb'
  Case ('03')
    MM = 'Mar'
  Case ('04')
    MM = 'Apr'
  Case ('05')
    MM = 'May'
  Case ('06')
    MM = 'Jun'
  Case ('07')
    MM = 'Jul'
  Case ('08')
    MM = 'Aug'
  Case ('09')
    MM = 'Sep'
  Case ('10')
    MM = 'Oct'
  Case ('11')
    MM = 'Nov'
  Case ('12')
    MM = 'Dec'
  End Select
  NS_DateTime = Xtime(1:2)//':'//Xtime(3:4)//':'//Xtime(5:6)//' '//Xdate(7:8)//'-'//MM//'-'//Xdate(1:4)
  Return
  End