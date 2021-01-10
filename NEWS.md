NEWS 
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

<b>&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;</b>

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor 
  and patch)

* New additions without breaking backward compatibility bumps the minor 
  (and resets the patch)

* Bug fixes and misc changes bumps the patch

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clinmon" target="_blank">clinmon</a> VERSION 0.5.5
----------------------------------------------------------------

* Simplified the package, to calculate all the indexes in `clinmon()`. `clinmon()` uses a *continuous* recording and returns a dataframe with hemodynamic indexes for every period, epoch or block depending on the chosen output. Includes `COest`, `CPPopt`, `CVRi`, `Dx`, `Mx`, `PI`, `PRx`, `PWA`, `RI`, and `Sx` (see details).

* Test data set is added to the package in two resolutions 1000 Hz, and 10 Hz. The corresponding deleter-file is also added. 

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clinmon" target="_blank">clinmon</a> VERSION 0.2.1
----------------------------------------------------------------

* Corrected the error, that missing values from artefacts deletion in blocks and epochs results in a errorgenous correlation coefficient.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clinmon" target="_blank">clinmon</a> VERSION 0.2.0
----------------------------------------------------------------

* This is the first version of the package put on CRAN

* Correlation coefficient based indices `Dx`, `Sx`, `Mx` and `PRx` are functional, and validated through ICM+

* Furthermore indices such as `CVRi`, `PI`, `RI`, `PWA`, and `CO` are functional.

* `CPPopt` and `TFA` will be added in future versions
