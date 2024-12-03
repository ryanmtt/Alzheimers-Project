# Move SNP files from home server onto HPC 
# log into HPC 
# Ensure plink is installed
# Ensure the converted .txt files have been moved into the HPC 
module spider plink2
module load apps/plink2/2.00a6LM
# Then run the following code, which will produce a .sscore file with the PRS score of each individual in UKB:
# For AD:
module load plink
plink2 \
--bgen instruments_AD_MR.bgen ref-first \
--sample ukb22828_c1_b0_v3_s487038.sample \
--score Effect_AD_Final.txt no-mean-imputation list-variants cols=+scoresums \
--out AD_PRS

# For SBP:
plink2 \
--bgen instruments_SBP_MR.bgen ref-first \
--sample ukb22828_c1_b0_v3_s487038.sample \
--score Effect_SBP_Final.txt no-mean-imputation list-variants cols=+scoresums \
--out SBP_PRS

# For DBP:
plink2 \
--bgen instruments_DBP_MR.bgen ref-first \
--sample ukb22828_c1_b0_v3_s487038.sample \
--score Effect_DBP_Final.txt no-mean-imputation list-variants cols=+scoresums \
--out DBP_PRS

# For WMH:
plink2 \
--bgen instruments_WMH_MR.bgen ref-first \
--sample ukb22828_c1_b0_v3_s487038.sample \
--score Effect_WMH_Final.txt no-mean-imputation list-variants cols=+scoresums \
--out WMH_PRS

# Finally, move these scores into the Project/PRS directory on home server to be loaded into R for analysis 
