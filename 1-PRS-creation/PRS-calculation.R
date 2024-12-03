# Move SNP files from home server onto HPC 
# log into HPC 
# Ensure plink is installed
```
module spider plink2
module load apps/plink2/2.00a6LM
```
# Then run the following code, which will produce a .sscore file with the PRS score of each individual in UKB:
# For AD:
module load plink
plink2 \
--bgen instruments_AD_Ryan_Final.bgen ref-first \
--sample ukb22828_c1_b0_v3_s487038.sample \
--score Effect_AD_Final.txt no-mean-imputation list-variants cols=+scoresums \
--out AD_PRS

# For SBP:

# For DBP:

# For WMH:

# Finally, move these scores into the Project/PRS on home server 
