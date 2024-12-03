# get csv files in appropriate txt format:

# AD:
AD_SNPs <- read.csv("CSV-FINAL/AD_SNPs_F2.csv")
AD.SNPs.ff <- AD_SNPs[, c("rsid", "effect_allele","Beta")]
write.table(AD.SNPs.ff, file = "PRS/Effect_AD_Final.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

# SBP:
# For SBP, need to convert negative effects to the positive effect allele:
SBP.SNPs <- read.csv("CSV-FINAL/SBP SNPs.csv")
swap_pos <- function(effect_allele, noneffect_allele, Beta) {
  if (Beta < 0) {
    list(effect_allele= noneffect_allele, noneffect_allele = effect_allele, Beta =  -Beta)
  } 
  else {
    list(effect_allele = effect_allele, noneffect_allele = noneffect_allele, Beta = Beta)
  }
}

# Apply the function to the data
SBP.SNPs.f <- SBP.SNPs %>%
  rowwise() %>%
  mutate(result = list(swap_pos(effect_allele, noneffect_allele, Beta))) %>%
  mutate(effect_allele = result$effect_allele,
         noneffect_allele = result$noneffect_allele,
         Beta = result$Beta) %>%
  select(-result)
write.csv(SBP.SNPs.f , "CSV-FINAL/SBP_SNPs_F.csv", row.names = FALSE)
SBP.SNPs.t <- SBP.SNPs.f[, c("rsid", "effect_allele","Beta")]
write.table(SBP.SNPs.t, file = "C:/Users/ryanm/Documents/University/Medical Statistics and Health Data Science/Dissertation/PRS/Effect_SBP_Final.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

# For DBP:
DBP.SNPs <- read.csv("CSV-FINAL/DBP SNPs.csv")

# Apply the function to the data
DBP.SNPs.f <- DBP.SNPs %>%
  rowwise() %>%
  mutate(result = list(swap_pos(effect_allele, noneffect_allele, Beta))) %>%
  mutate(effect_allele = result$effect_allele,
         noneffect_allele = result$noneffect_allele,
         Beta = result$Beta) %>%
  select(-result)

write.csv(DBP.SNPs.f , "CSV-FINAL/DBP_SNPs_F.csv", row.names = FALSE)
DBP.SNPs.t <- DBP.SNPs.f[, c("rsid", "effect_allele","Beta")]
write.table(DBP.SNPs.t, file = "PRS/Effect_DBP_Final.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

# WMH:
WMH_SNPs <- read.csv("CSV-FINAL/WMH SNPs.csv")
WMH.SNPs.f <- WMH_SNPs[, c("rsid", "effect_allele","Beta")]
write.table(WMH.SNPs.f, file = "PRS/Effect_WMH_Final.txt", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
