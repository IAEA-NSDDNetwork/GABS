# GABS new version
Gamma-ray absolute intensity and normalization calculation. GABS is part of the [ENSDF Analysis and Utility Programs](https://nds.iaea.org/public/ensdf_pgm/).

Mantained by: Tibor.Kibedi@anu.edu.au


## Change history

#### 2021-06
* Bug fixing: 
  - `%DIG` was incorrectly evaluated if `BR < 1.0`;
  - using the "-M" option to find transitions feeding to the ground state, `TI` values are not included in the summed total intensity.
  - `%IG` values were produced from the total intensities `TI`, when no conversion coefficient `CC` was given. In these cases `%IG=TI`, instead of `%IG=TI/(1+CC)`.

#### 2020-08
* Bug fixing

#### 2020-04
* Correction of `%IG` uncertainties calculation for transitions used for normalisation when `BR < 1.0`.


#### 2019-09
* Release of *GABS new version*. The new version of GABS was presented at the last NSDD meeting in Vienna. 

  The new code has three operation modes: 
  - **[F]** - `NR` and `BR` will be obtained from a fit (using G`s marked with "X")<br>
  - **[C]** - Calculate `TI` using `NR` and `BR` from the N-record in the input file<br>
  - **[M]** - Mark transitions going to the g.s. by "X" (DRI>0) & "Y" (DRI blank)<br>

  The syntax to use in the [F] mode is simply ```gabs -f myensdf.ens```.
 
  For further information see the  [NSDD 2019 presentation on GABS](https://nds.iaea.org/nsdd/NSDD2019/Presentations/2019_NSDD_GABS_Kibedi.pdf)
