!     ******************************************************************
!     *                                                                 
!     *                  PROGRAM GABS(Version 10)                       
!     *                       EdgarDo  Browne                            
!     *                  Lawrence Berkeley Laboratory                   
!     *                     Adapted for IBM PC by                       
!     *                       Coral M. Baglin                           
!     *                        September 1991                           
!     *                         August 1993                             
!     *                            May 2000                             
!     *                         November 2010                           
!     *   Version 10.a July 2013, Dec 2014 (T. Kibedi)
!     *                                                                 
!     *   This program reads ENSDF decay data sets and calculates       
!     *   branching ratios (BR, DBR), gamma-ray normalizing factors     
!     *   (NR, DNR), and uncertainties in absolute gamma-ray intensities
!     *   GABS writes results to GABSPC.RPT, and it can also create     
!     *   new ENSDF datasets which include the calculated data.         
!     *   GABS consists of a main program and a few functions.          
!     *   It uses the Character-string subroutine CNVU2S from the       
!     *   Fortran NSDFLIB library, which is maintained by the Brookhaven
!     *   National Laboratory.  This Fortran library must be compiled   
!     *   and linked to GABS.                                           
!     *   This program originally was written in FORTRAN 77, for a      
!     *   VAX-11/8600 computer with the VMS operating system.           
!     *                                                                 
!     *   Program modified for use on IBM PC by Coral Baglin, September 
!     *   1991, as follows:                                             
!     *      * Variables BR, DBR and GARR removed from DATA and unnamed 
!     *        COMMON statements and initialized in separate Do  loop.   
!     *      * FORMATs 1040 and 1050  and associated WRITE statemnents i
!     *        ENSDFN reworked, introducing new variables IVAR1 and IVAR
!     *      * Modified to prevent continuations of normalization commen
!     *        records being confused with N records.                   
!     *      * Modified so no attempt to write ENSDF-format output file 
!     *        unless file requested by user.  Error messages are now   
!     *        written to GABS.RPT file instead of ENSDF output file.   
!     *                                                                 
!     *   Revision, August 1993 (CMB):                                  
!     *      * Avoid looking for X or Y in col. 79 on continuation,     
!     *        Do cumentation or comment G records.                      
!     *      * Avoid reading PN, DN records as though they were N record
!     *      * Avoid changing original PN record when writing new file. 
!     *   Revision, August 27, 1993 (T.W. Burrows)                      
!     *      * Delinted using VAX FORTRAN-lint 2.83                     
!     *      * Added machine-dependent coding                           
!     *      * Moved variable type declarations before COMMON's (Some   
!     *      *   compilers require this order).                         
!     *      * Changed terminal output unit from 5 to 6                 
!     *      * Added version/date stamp to terminal output              
!     *   Revision, May 10, 2000 (E. Browne)                            
!     *      * User can give a different name to GABSPC.RPT             
!     *      * (Default name is GABSPC.RPT)                             
!     *      * Program checks for the status of both input and          
!     *      * output files.                                            
!     *      * Added calculation for cascade gamma rays.                
!     *      * Program adds current date to report file (GABSPC.RPT).   
!     *      * Program uses subroutine CNVU2S from BNL'S NSDFLIB library
!     *      * instead of ENSDFN (to write numbers in ENSDF style).     
!     *   Revision, May 19, 2000 (T.W. Burrows)                         
!     *      * Explicitly typed all variables                           
!     *      * Added machine dependent coding (Use the program SETMDC)  
!     *          - ANS - ANSI standard FORTRAN 77                       
!     *          - DVF - Digital Visual Fortran (MS WinDo ws)            
!     *          - UNX - f77 compiler under the Linux OS (2-Feb-2001)   
!     *          - VAX - OpenVMS, VMS                                   
!     *   Revision November 15, 2010 (E. Browne)                        
!     *      * 1.4% uncertainty for ICC using program BRICC             
!     *   Version 11 July 2013, Dec 2014 (T. Kibedi)
!     *      * Converted to FORTRAN 90, removed some of the old comments
!     *      * Program now overwtite existing report and output ENSDF files
!     *      * Corrected logic to accept lower case characters in Column 79
!     *      * Interactive / Batch usage by command line arguments
!     *   Revision March 31 2015 (T. Kibedi)
!     *      * Bug fixes in GAMMAS routine to avoid negative values of RU1
!     *      * Minor modification in source code to comply with FORTRAN 90
!     *   Version 11.a 7-Oct-2015 (T. Kibedi)
!     *      * Execution will stop if more than 3 END records (blank lines) found in the input file
!     *   Version 11.b 24-Aug-2016 (T. Kibedi)
!     *      * %IG=Val in new file is on a GAMMA contrinuation recordf
!     *   Version 11.c 8-Jan-2017 (T.Kibedi)
!     *      * In some cases testing the END record FG == 0 and NO calculation was made
!     *        New variable used to count the number of normalization transitions
!     *      * normalization transitions reported on terminal consol
!     * Version 11.e 25-Nov-2017 (T.Kibedi)  
!     *      * normalization transitions tested if RI is non-blank
!     * Version 11.f 16-May-2018 (T.Kibedi) 
!     *      *  Updated protocol for RI and TI values of transitions marked to use for normalization
!     *      * (a) RI - non-blank; TI -     blank  --> RI,DRI,CC,DCC used for normalization
!     *      * (b) RI -     blank; TI - non-blank  -->   TI used for normalization (E0, highly converted transition) 
!     *      * (c) RI - non-blank; TI - non-blank  -->   TI used for normalization
!     *      * (d) RI -     blank; TI -     blank  -->   Error message is generated 
!     *      * uncertainty on %IG for transitions NOT used for normalization was incorrectly reported
!     =======================================================================================================
!     * Version 12  04-Jan-2019 (T.Kibedi) Major modification
!            * Program logic has been changed and simplified, Limits, CA and AP in the DRI or DTI fields 
!              no longer are allowed
!            * ENSDF file loaded to memory using the new LoadENSDF subroutine
!            * All  variables declared in the GabsMod.f90  module.
!            * Old GAMMA comment cards with %GI= replaced
!            * Names of the calculation report file ".rpt" and the new ENSDF file ".new" are
!              created from the input file
!            * New fuctions added to run GABS interactively or from the command line
!                 gabs -F <InputFile>    Fits NR & BR using transitions marked with "X".
!                                        New ENSDF file will be created with updated N record, as well 
!                                          G-comment records with absolute %GI= values
!                                        Gamma-rays used for normalization should
!                                          - RI > 0 and/or TI > 0; TI will be used if both non-zero
!                                          - marked with "X" if DRI > 0 or DTI > 0 (if TI > 0)
!                 gabs -C <InputFile>    Calculates %GI using NR and BR from the N-record
!                                        in the input file. Calculation report file and new ENSDF 
!                                        files will be created
!                 gabs -M <InputFile>    Lists transitions to the ground state and
!                                        marks them with "X" (DRI > 0) and "Y" (DRI blank)  
!    * Update     20-Sep-2019  (T.Kibedi)
!                                        - Barlraj Singh noted, that columns (6:9) have been converted
!                                          to upper case. Code corrected
!                                        - "2 N" continuation records were removed
!    * Update     21-Apr-2020  (T.Kibedi)
!                                        - Filip Kondev noted, that %DIG was evaluated incorrectly
!                                          for cases of BR<1.0
!    * Update     17-Jun-2021  (T.Kibedi)
!                                        - Filip Kondev noted, that using -M option TI values on G-cards
!                                          hav not been added to the calculation of total g.s. feeding
! =================================================================================================
  PROGRAM GABS       
  Use GabsMod 
  Implicit None
  Character(len=1)                               :: Answ    
  Character(len=MaxFileName)                     :: wFile
  Integer(kind=4), Parameter                     :: nFiles = 5
  Character(len=MaxFileName), Dimension(nFiles)  :: cArray
  Character(len=MaxFileName)                     :: String
  Integer(kind=4)                                :: nPar = 0
  Integer(kind=4)                                :: iDS = 0
  Integer(kind=4)                                :: IoStat = 0
  Integer(kind=4), External                      :: IndexF        
  Character(len=20), External                    :: NS_DateTime
  Open (UNIT=DefOut,FORM='FORMATTED',CARRIAGECONTROL='FORTRAN')
    
!  GabsMode = 'F'   ! Fit normalization
  Do iDS =1, MaxDS                                                     
     cBR(iDS) =' '   
    cDBR(iDS) = ' '
      BR(iDS) = 0.0                                                       
     dBR(iDS) = 0.0
    nCas(iDS) = 0.0
  End Do                                                  
! =======================================================================================
! STEP#1 Input/Output files & execution control
! =======================================================================================
  Write (DefOut,'(a)') '  ===========  '//Trim(Version)//'  ==========='     
!.....Get command line arguments --------------------------------------------------------
  nPar = nFiles
  Call GET_COMMAND_Line(' ',cArray,nPar)
  Select Case (nPar)
!.No Argument - Interactive usage ---------------------------------------------------
  Case (0)
    Write (DefOut,'(A)')              ' GABS: [F] - NR and BR will be obtained from a fit (using G`s marked with "X")'
    Write (DefOut,'(a)')              '       [C] - Calculate TI using NR and BR from the  N-record in the input file'
    Write (DefOut,'(a)')              '       [M] - Mark transitions going to the g.s. by "X" (DRI>0) & "Y" (DRI blank)'
    Write (DefOut,'(A)',Advance='NO') ' GABS: Enter Operation mode (def=F): '                                                  
    Read (DefIn,'(A)') Answ     
    Call UpStr(Answ)
    Select Case (Answ)
    Case (' ','F')
      GabsMode = '-F'
    Case ('C')
      GabsMode = '-C'
    Case ('M')
      GabsMode = '-M'
    Case Default
      Write (DefOut,'(a)') '  <F> Invalid option detected: '//Trim(cArray(2))
      Write (DefOut,'(a)') '       -F   fit NR and BR (normal execution)'
      Write (DefOut,'(a)') '       -C   calculate TI using NR and BR from the input file'
      Write (DefOut,'(a)') '       -M   mark transitions to g.s. with "X" (DRI>0 or DTI>0)) or'
      Write (DefOut,'(a)') '               "Y" (DRI or DTI blank)'
      Stop
    End Select
    Write (DefOut,'(A)',Advance='NO') ' GABS: Enter input file name: '                    
    Read (DefIn,'(A)') InpFile                                               
    Call Extension(InpFile,'rpt',RptFile)
    wFile = ' '
    Write (DefOut,'(A)',Advance='NO') ' GABS: Enter REPORT FILE name (def='//Trim(RptFile)//'): '                                                  
    Read (DefIn,'(A)') wFile                                              
    If (wFile /= ' ') RptFile=Trim(wFile)                             
!.Blank or '?' only ---------------------------------------------------------------
  Case (1)
    String = Carray(1)
!...Help instructions
    If (Trim(String) == '?') Then
      Call GabsHelp()
      Stop
    End If
!.<Mode> <InputFile>
  Case (2)
    Call UpStr(cArray(1))
    GabsMode = cArray(1)(1:2)
    InpFile = Trim(cArray(2))
!   Report file -------------------------------------------------------------------------
    Call Extension(InpFile,'rpt',RptFile)
    Write (DefOut,'(a)') '  Report file:        '//Trim(RptFile)                                                                
!.Too many parameters ---------------------------------------------------------------
  Case Default
    Call GabsHelp()
  End Select
! ===================================================================================
! Process requested operation
! Reads and verify ENSDF Input file
  Call LoadENSDF()
! ===================================================================================
! Report file
  If (Trim(InpFile) == Trim(RptFile)) Then
    Write (DefOut,'(a)') '      <F> Input and Report file names are the same!'
    Stop
  End If
  Open (unit=RptLun,file=RptFile,Form='Formatted',status='replace',IoStat=IoStat)   
  If (IoStat /= 0) Then
    Write (DefOut,'(a)') '  <F> '//Trim(RptFile)//' file could not be created'
    Stop
  End If
  Write (RptLun,'(a)') '  * * * * * * '//Trim(Version)//' Report file  * * * * * * '                                 
  Write (RptLun,'(a)') '  Current date: '//NS_DateTime()                               
  Write (RptLun,'(a)') '  ENSDF input file: '//Trim(InpFile)                                                                
! ===================================================================================
  Select Case (GabsMode)
  Case ('-F')
    Call Extension(InpFile,'new',NewFile)
    If (Trim(InpFile) == Trim(NewFile)) Then
      Write (DefOut,'(a)') '      <F> Input and Output file names are the same!'
      Stop
    End If
    Open (unit=NewLun,file=NewFile,Form='Formatted',status='replace',IoStat=IoStat)   
    If (IoStat /= 0) Then
      Write (DefOut,'(a)') '  <F> '//Trim(NewFile)//' file could not be created'
      Stop
    End If
    Call CalcNormalization()
! -----------------------------------------------------
  Case ('-C')
    Call Extension(InpFile,'new',NewFile)
    If (Trim(InpFile) == Trim(NewFile)) Then
      Write (DefOut,'(a)') '      <F> Input and Output file names are the same!'
      Stop
    End If
    Open (unit=NewLun,file=NewFile,Form='Formatted',status='replace',IoStat=IoStat)   
    If (IoStat /= 0) Then
      Write (DefOut,'(a)') '  <F> '//Trim(NewFile)//' file could not be created'
      Stop
    End If
    Call CalcAbsIG()
! -----------------------------------------------------
  Case ('-M')
    Call Extension(InpFile,'in',NewFile)
    If (Trim(InpFile) == Trim(NewFile)) Then
      Write (DefOut,'(a)') '      <F> Input and Output file names are the same!'
      Stop
    End If
    Open (unit=NewLun,file=NewFile,Form='Formatted',status='replace',IoStat=IoStat)   
    If (IoStat /= 0) Then
      Write (DefOut,'(a)') '  <F> '//Trim(NewFile)//' file could not be created'
      Stop
    End If
    Call MarkGammas()
! -----------------------------------------------------
  End Select
  Write (DefOut,'(a)') ' '
  Write (DefOut,'(a)') '  Calculations completed'

  Stop
  End
