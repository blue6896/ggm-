R codes for the simulation of Gaussian Graphical Models with R package QUIC 

<Files> 

SIMUGENERATE.R generates simulated input matrix 
SIMUPARAMETE.R weights hub nodes or non-hub nodes in simulated input matrix
SIMUFINDTUNR.R finds tuning paramters for AIC and BIC
REALTUNECVAL.R finds tuning paramters for Cross-Validation 
SIMUFINDTPTN.R computes True positive rates (TPR), True negative rates (TNR), and accuracy (=(TPR+TNR)/2) 

<How to Run>

In this version, if you run SIMUMERGEALL.R (run by test.sh using qsub, if possible), this program implements
weighted Gaussian Graphical Models on simulated data. Files fig1show.R and fig2show.R genearting figure1.tiff 
and figure2.tiff each, are run if you run SIMUMERGEALL.R. Currently the variable repe, the number of replication
in SIMUMERGEALL.R code is set to 500. 

Please e-mail me at jai.woo.lee.gr@dartmouth.edu if you have any questions

Thank you!

Jai Woo Lee   