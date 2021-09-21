<b>GABS new version</b> 
<br>
Gamma-ray absolute intensity and normalization calculation.
<br>
Please report problems and suggestions to Tibor.Kibedi@anu.edu.au


<b>2021-06</b>
<br>
Bug fix: 
<br>
(a) %DIG was incorrectly evaluated if BR < 1.0; 
<br>
(b) using the "-M" option to find transitions feeding to the ground state, TI values are not included in the summed total intensity. 
<br>
(c) %IG values were produced from the total intensities (TI), when no conversion coefficient (CC) was given.In these cases %IG=TI, instead of %IG=TI/(1+CC). 
<br>
<br>
<b>2020-08</b>
<br>
Bug fixing
<br><br>
<b>2020-04</b> 
<br>
Correction of %IG uncertainties calculation for transitions used for normalisation when BR smaller than 1.0


<b>2019-09</b> 
<br>
GABS new version <br>
The new version of GABS presented at the last NSDD meeting in Vienna. 
<br>
The new code has three operation modes: 
<br>
[F] - NR and BR will be obtained from a fit (using G`s marked with "X")<br>
[C] - Calculate TI using NR and BR from the N-record in the input file<br>
[M] - Mark transitions going to the g.s. by "X" (DRI>0) & "Y" (DRI blank)<br>
The syntax to use in the [F] mode is simply "gabs -f myensdf.ens".<br>
For further information see the NSDD 2019 presentation on GABS. 
