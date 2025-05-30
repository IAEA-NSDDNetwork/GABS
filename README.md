# GABS

This program reads ENSDF decay data sets and calculates  branching ratios (BR, DBR), gamma-ray normalizing factors     
(NR, DNR), and uncertainties in absolute gamma-ray intensities GABS writes results to GABSPC.RPT, and it can also create   new ENSDF datasets which include the calculated data.  

GABS consists of a main program and a few functions. It uses the Character-string subroutine CNVU2S from the       
Fortran NSDFLIB library, which is maintained by the Brookhaven National Laboratory.  This Fortran library must be compiled and linked to GABS.         

This program originally was written in FORTRAN 77, for a  VAX-11/8600 computer with the VMS operating system.         

GABS is part of the [ENSDF Analysis and Utility Programs](https://nds.iaea.org/public/ensdf_pgm/).

Address any feedback to Tibor Kibedi at Tibor.Kibedi@anu.edu

## Change history

#### 2021-06

Version 12

Bug fix: using -M option TI values on G-cards have not been added to the calculation of total g.s. feeding

## Disclaimer

Neither the author nor anybody else makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness or usefulness of any information disclosed, or represents that its use would not infringe privately owned rights.

