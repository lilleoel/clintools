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

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.5
----------------------------------------------------------------

* Added the a `resting_delay` setting to the `dilations` function and documentation. Used to assess delay the subsequent period for assessment.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.4
----------------------------------------------------------------

* Added the `dilations` function and documentation. Used to assess dilations from `PLR3000`.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.3
----------------------------------------------------------------

* Updated `sRCT` function and documentation. Now applies random effect of site, and add relative risks together using ln-number instead of absolute numbers

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.2
----------------------------------------------------------------

* Updated `clinmon` documentation

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.1
----------------------------------------------------------------

* Optimised `rrGcomp`
* Updated `clinmon` documentation
* Minor bugfixing for `PLR3000` 

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.9.0
----------------------------------------------------------------

* Added the `sRCT`-function to generate simulated randomized clinical trials. Much more work to add 

* Added the `rrGcomp`-function to calculate relative risk using G-computation from an individual participant dataframe. 

* Added the `PLR3000`-function to convert data extracted from NeurOptics PLR-3000 to a long format dataframe. 

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.8.5
----------------------------------------------------------------

* Added the `iscus`-function to convert XML microdialysis data to a dataframe.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.8.2
----------------------------------------------------------------

* Small bugfixes for the `TFA` and `clinmon` functions, especially the overlapping functionality did not produce Mx, PRx or CPPopt.


<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.8.1
----------------------------------------------------------------

* Updated descriptions and helper-files.


<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.8.0
----------------------------------------------------------------

* Changed name from clinmon to clintools to allow for inclusion of other function than only hemodynamic indices.

* Based on the matlab-script created by David Simpsons in 2015 the `TFA`-function have been added to calculate the transfer function analysis.

* The test data from 2015 has also been addedd `data(tfa_sample_data)`, `data(tfa_sample_data1)`, and `data(tfa_sample_data2)` to underline the script generates the same results as the Matlab-script.

* Added the `ortable`-function which generates an Odds ratio table from a logistic regression `glm`.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.5.5
----------------------------------------------------------------

* Simplified the package, to calculate all the indices in `clinmon()`. `clinmon()` uses a *continuous* recording and returns a dataframe with hemodynamic indices for every period, epoch or block depending on the chosen output. Includes `COest`, `CPPopt`, `CVRi`, `Dx`, `Mx`, `PI`, `PRx`, `PWA`, `RI`, and `Sx` (see details).

* Test data set is added to the package in two resolutions 1000 Hz, and 10 Hz. The corresponding deleter-file is also added. 

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.2.1
----------------------------------------------------------------

* Corrected the error, that missing values from artefacts deletion in blocks and epochs results in a errorgenous correlation coefficient.

<b>CHANGES</b> IN <a href="https://github.com/lilleoel/clintools" target="_blank">clintools</a> VERSION 0.2.0
----------------------------------------------------------------

* This is the first version of the package put on CRAN

* Correlation coefficient based indices `Dx`, `Sx`, `Mx` and `PRx` are functional, and validated through ICM+

* Furthermore indices such as `CVRi`, `PI`, `RI`, `PWA`, and `CO` are functional.

* `CPPopt` and `TFA` will be added in future versions
