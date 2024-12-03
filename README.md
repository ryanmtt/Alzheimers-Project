# Interactions between genetic liability to Alzheimer’s disease and blood pressure in dementia risk
This repository accompanies the paper: 

"Interactions between genetic liability to Alzheimer’s disease and blood pressure in dementia risk"

which looks into the evidence surrounding an interaction between blood pressure and AD risk on dementia risk, through use of PRS in logistic regressions.

# Environment details
R version 4.4.2 

The following R packages are required for analysis:
```R
   install.packages(c("flextable", "dplyr", "ggplot2", "purrr", "data.table", "readr", "tidyr", "patchwork"))
```

# Analysis Overview
We preform a series of logistic regressions using polygenic risk scores derived from individuals in the UK biobank
## Analysis components
This can be broken down into 4 main steps:

 1. Data preprocessing - formatting SNPs and constructing PRS for AD, SBP, DBP, WMH
 See `1-PRS-creation` directory 
 2. Data cleaning and merging phenotype and genotype data
 See `2-cleaning-and-merging` directory 
 3. Primary and supplementary regression creation  
 See `3-data-analysis` directory

## Directory structure
CSV files containting the relevant SNPs from the 4 GWAS are held in:
```
Project/CSV-FINAL 
```
Lists of SNPs and the bgen files containing the SNPs present in each individual in UKB to be used to compute PRS are stored in a directory:
```
Project/PRS
```
R code to clean the data, merge it, and run the analysis are stored in:
```
Project/R-code
```

