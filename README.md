# YSC2019
R code for the workshop at the 2019 Young Statisticians Conference (YSC2019) in Canberra. Workshop title: "Maximising the use of Australian Bureau of Statistics Data Products and Analysis Tools".

The code reads in the data and fits:
* Binomial model using the data without areas
* Poisson model using the data without areas
* Binomial model with a random intercept for SA4 (Generalised linear mixed model - GLMM) using an area-level data set

The data came from Table Builder at the Australian Bureau of Statistics.

The CSV file `sa4datafilled.csv` was too big to upload, so I used a compressed version `sa4datafilled.rds` in the R code. 

Thanks to James Brown for the data and SAS code.
