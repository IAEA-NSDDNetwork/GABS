!.....=============================================================================================  
      Subroutine GabsHelp() 
      Use GabsMod
      Implicit None                                                
      Write (DefOut,'(a)') '  Usage with command line arguments:'
      Write (DefOut,'(a)') '  GABS <Mode> <InputFile>'              
      Write (DefOut,'(a)') '   InputFile  ENSDF file, G-rays marked with "X" in column 79'              
      Write (DefOut,'(a)') '             Blank DRI or DTI allowed, but sum[i] DTI(i)**2 should not be zero'
      Write (DefOut,'(a)') '   Mode to control execution'
      Write (DefOut,'(a)') '     -F   NR and BR (multiple data set only!) will be calculated from'
      Write (DefOut,'(a)') '             G`s marked with "X" and direct feeding to the ground state (IGS)'
      Write (DefOut,'(a)') '             Output: report (*.rpt), new ensdf (*.new)'
      Write (DefOut,'(a)') '     -C   Calculate TI using NR and BR from the N-record in the input file'
      Write (DefOut,'(a)') '             Output: report (*.rpt), new ensdf (*.new)'
      Write (DefOut,'(a)') '     -M   Lists transitions going to the g.s. and RI>0 or TI>0 with'
      Write (DefOut,'(a)') '             "X" (DRI>0 or DTI>0) or "Y" (blank or limits in DRI or DTI)'
      Write (DefOut,'(a)') '             Total RI and TI for g.s. transitions also calculated'
      Write (DefOut,'(a)') '             Output: report (*.rpt), GABS input (*.in)'
      Stop
      End Subroutine GabsHelp
