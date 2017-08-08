This folder is located at https://github.com/blue6896/ggm-/
R codes for the simulation of Gaussian Graphical Models with R package QUIC 

<Initialization>

Please download and install R package QUIC and all prerequisite R packages for QUIC installation 

<Files> 

SIMUGENERATE.R generates simulated input matrix 
SIMUPARAMETE.R weights hub nodes or non-hub nodes in simulated input matrix
SIMUFINDTUNE.R finds tuning paramters for AIC and BIC
REALTUNECVAL.R finds tuning paramters for Cross-Validation 
SIMUFINDTPTN.R computes True positive rates (TPR) and True negative rates (TNR)

<How to Run>

In this version, if you run SIMUMERGEALL.R (run by test.sh using qsub, if possible), this program implements
weighted Gaussian Graphical Models on simulated data. Files fig1show.R and fig2show.R genearting fig1.tiff 
and fig2.tiff each, are run if you run SIMUMERGEALL.R. Currently the variable repe, the number of replication
in SIMUMERGEALL.R code is set to 500. 

For SIMUMERGEALL.R, you can modify parameters of the functions to make data matrix as you want. 
parameters: 
 nn: number of genes 
 samples: number of observations for each gene 
 case: different scenarios weighting nodes => when case =1, only hub nodes are weighted, when case=2, 
 half of hub nodes and half of non-hub nodes are weighted, when case =3, only non-hub nodes are weighted 
 and when case =4, randomly chosen nodes are weighted. 
 numhub: number of hubs on network 
 bighub: the number of neighboring edges of hub nodes are controlled 
 shlb: the lower bound of neighboring edges of non-hub nodes are controlled 
 shub: the upper bound of neighboring edges of non-hub nodes are controlled 
 bigwgt: the weight for nodes 
 disconnected: the number of nodes without any neighboring edges 


Please e-mail me at jai.woo.lee.gr@dartmouth.edu if you have any questions

Thank you!

Jai Woo Lee   
