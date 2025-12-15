
# 
# This makefile requires an R installation with the following libraries:
# 
# Main document: MSCMT, parallel, readstata13, BMisc
# Appendices (add.): sandwich, lmtest, car, multcomp, arm
# 



# -------------------- MAKE targets --------------------
# MAIN DOCUMENT
main: IntroPlot.pdf scm_est.Rdata scm_plots_2panel.pdf Tab_DiD.tex
# APPENDICES
appdx: Tab_weights.tex Tab_est.tex SCM_sensitivity_A.pdf SCM_sensitivity_B.pdf x_SCM_FR_plot.pdf Tab_DiDmeans.tex Tab_DiDptrends.tex DiD_placebo.pdf Tab_DiDEU.tex Tab_DiDenep.tex





# -------------------- Main document --------------------


# Figure 1 [Introduction plot]
IntroPlot.pdf: IntroPlot.R CountryEPData.dta scm_est.Rdata
	R CMD BATCH --no-save IntroPlot.R

# Synth. control. model estimates
scm_est.Rdata: SCM_est.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save SCM_est.R

# Figure 2 [plots SCM estimates and placebos]
scm_plots_2panel.pdf: SCM_plots_2panel.R scm_est.Rdata
	R CMD BATCH --no-save SCM_plots_2panel.R
	
# Table 1 [DiD estimates]
# estimate models:
DiD_est.Rdata: DiD.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save DiD.R
# create latex table:
Tab_DiD.tex: Tab_DiD.Rnw DiD_est.Rdata
	Rscript -e "library(knitr) ; knit('Tab_DiD.Rnw')"




# -------------------- Appendices --------------------


# ------ Appendix B ------

# Table B.1 [W* and V matrices]
Tab_weights.tex: Tab_weights.Rnw scm_est.Rdata
	Rscript -e "library(knitr) ; knit('Tab_weights.Rnw')"

# Table B.2 [Placebo p-values]
Tab_est.tex: Tab_est.Rnw scm_est.Rdata
	Rscript -e "library(knitr) ; knit('Tab_est.Rnw')"

# Figure B.1 [Sensitivity analysis predictors]
SCM_sensitivity_A.pdf: SCM_sensitivity_A.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save SCM_sensitivity_A.R

# Figure B.2 [Sensitivity analysis country excl.]
SCM_sensitivity_B.pdf: SCM_sensitivity_B.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save SCM_sensitivity_B.R

# Figure B.3 [Comparison w/ and w/out France]
x_SCM_est_FR.Rdata: x_SCM_est_FR.R
	R CMD BATCH --no-save x_SCM_est_FR.R
x_SCM_FR_plot.pdf: x_SCM_FR_plot.R x_SCM_est_FR.Rdata
	R CMD BATCH --no-save x_SCM_FR_plot.R


# ------ Appendix C ------

# Table C.1 [pre-/post-reform diff. in means]
DiD_prelim.Rdata: DiD_prelim.R CountryEPData.dta
	R CMD BATCH --no-save DiD_prelim.R
Tab_DiDmeans.tex: Tab_DiDmeans.Rnw DiD_prelim.Rdata
	Rscript -e "library(knitr) ; knit('Tab_DiDmeans.Rnw')"

# Table C.2 [parellel trends]
Tab_DiDptrends.tex: Tab_DiDptrends.Rnw DiD_prelim.Rdata
	Rscript -e "library(knitr) ; knit('Tab_DiDptrends.Rnw')"

# Figure C.1 [Placebo DiD analysis]
DiD_placebo.pdf: DiD_placebo.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save DiD_placebo.R


# ------ Appendix D ------

# Table D.1 [DiD estimates Euroscepticism]
DiDEU_est.Rdata: DiDEU.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save DiDEU.R
Tab_DiDEU.tex: Tab_DiDEU.Rnw DiDEU_est.Rdata
	Rscript -e "library(knitr) ; knit('Tab_DiDEU.Rnw')"


# ------ Appendix E ------

# Table E. 1 [DiD estimates, effective number of parties]
DiDenep_est.Rdata: DiDenep.R CountryEPData.dta CountryEPData_covIP.dta
	R CMD BATCH --no-save DiDenep.R
Tab_DiDenep.tex: Tab_DiDenep.Rnw DiDenep_est.Rdata
	Rscript -e "library(knitr) ; knit('Tab_DiDenep.Rnw')"
	
