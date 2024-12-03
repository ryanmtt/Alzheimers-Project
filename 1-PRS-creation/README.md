# SNP pre-processing and creating PRS

First, SNP list csvs must be formatted such that "Beta" are all risk increasing, and there are columns; rsid| Effect allele | Beta. The following R file gets the csv files in the CSV-FINAL directory into appropriate txt format- `CSV-convert.R`

Next, the correct bgen files and formatting in order to preform the PRS calculations is achieved via terminal commands in a HPC specified in `SNP-imputation.R`. This process produces the needed files for DBP SNPs, and this is repeated for AD, SBP and WMH SNPs in the same way. 

Finally, PRS are calculated through the terminal on a HPC, seen in `PRS-calculation.R`.

