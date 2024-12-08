# set working directory to the /Project
# (a): Phenotypic data read in and exploration
# (b): Cleaning phenotypic data through exclusions 
# (c): Merging phenotypic and genotypic data into final dataset 

# (a): Phenotypic data read in and exploration

# load in the UK biobank phenotypic data:
table1 <- function (..., useNA = 'ifany') base::table(..., useNA = useNA)
df <- fread(".../UK-Biobank-Data/data_ukb.csv")
str(df)
summary(df) 

# variables have values 0.0, 1.0, 2.0
# these show the different follow ups, and generally we're just interested in 
# the first instance ='0.0'"

# duplicate the dataset:
df2 <- df
# want to get variable that indicates if someone had a Alzheimer's or vascular dementia dignosis
# so take their date of disease report and programme as a 1, and then NA as 0:
df2 <- df2 %>% mutate(DemAlz=ifelse(!is.na(`Date of alzheimer's disease report.0.0`),1,0))
table1(df2$DemAlz) #4451
df2  <- df2 %>% mutate(DemVas=ifelse(!is.na(`Date of vascular dementia report.0.0`),1,0))
table1(df2$DemVas) #2183
df2  <- df2 %>% mutate(DemAllCause=ifelse(!is.na(`Date of all cause dementia report.0.0`),1,0))                                               
table1(df2$DemAllCause) #10049
# take first SBP/DBP reading:
colnames(df2)[colnames(df2)=="Diastolic blood pressure, automated reading.0.0"] <- "DBP" 
summary(df2$DBP)
hist(df2$DBP)
colnames(df2)[colnames(df2)=="Systolic blood pressure, automated reading.0.0"] <- "SBP" 
summary(df2$SBP)
hist(df2$SBP)

#check out white matter hyper intensities
colnames(df2)[colnames(df2)=="Total volume of white matter hyperintensities (from T1 and T2_FLAIR images).2.0"] <- "WMH" 
summary(df2$WMH)
hist(df2$WMH)

# Quick look at the distribution of Alzheimer's by age, gender and deprevation:

# Age at baseline: 
tapply(df2$`Age at recruitment.0.0`, df2$DemAlz, mean, na.rm=TRUE)
tapply(df2$`Age at recruitment.0.0`, df2$DemAlz, sd, na.rm=TRUE) 
# Alzheimer's sufferers have an older mean age than non-sufferers as expected 

#Gender
addmargins(table(df2$Sex.0.0, df2$DemAlz))
round(prop.table(table(df2$Sex.0.0, df2$DemAlz),2),3)
# women are more prevalent in the sample than men

#Townsend
tapply(df2$`Townsend deprivation index at recruitment.0.0`, df2$DemAlz, mean, na.rm=TRUE)
tapply(df2$`Townsend deprivation index at recruitment.0.0`, df2$DemAlz, sd, na.rm=TRUE) 
# Higher score = more deprived -> Alzheimer's sufferers are more deprived on avergae in the sample




# (b): Cleaning phenotypic data through exclusions 

# first, apply exclusion list 
# load in exclusion list
exclusion_ids <- read.table("PRS/exclusion_ids.txt", quote="\"", comment.char="")
colnames(exclusion_ids)[colnames(exclusion_ids)=="V1"] <- "eid" 
exclude <- exclusion_ids$eid

# about 500000 individuals in full dataset
#remove samples that did not pass QC
df2 <- df2[!df2$eid %in% exclude, ]
dim(df2)
# dropped about 150000 participants

# YOB csv-> need to merge to the df to get the age of dementia diagnosis
YOB <- read.csv("YOB.csv")
# combine with rest of dataset by ID using merge: 
df2 <- merge(df2, YOB, by = "eid")
#create dementia diagnosis date variable
df2$dementia_date <- pmin(df2$`Date of alzheimer's disease report.0.0`, 
                          df2$`Date of vascular dementia report.0.0`, 
                          df2$`Date of all cause dementia report.0.0`, 
                          na.rm = TRUE)
summary(df2$dementia_date)
#calculate age dementia diagnosed: Mean dementia age is 73.5 years
df2$dementia_year <- format(df2$dementia_date, format="%Y")
df2$dementia_year <- as.numeric(df2$dementia_year)
df2$dementia_age <- df2$dementia_year - df2$`Year.of.birth.0.0`
summary(df2$dementia_age)
hist(df2$dementia_age)

# reduce down the number of columns to just the essentials:
sdf=df2[,c(1,2,3,7,14,22,30,54,62,66:76,107,108,114,115,116,120)]
# rename some columns:
names(sdf)[names(sdf) == "Sex.0.0"] <- "Sex"
names(sdf)[names(sdf) == "Age at recruitment.0.0"] <- "Age"
colnames(sdf)[11:20]=c("GPC1","GPC2","GPC3","GPC4","GPC5","GPC6","GPC7","GPC8","GPC9","GPC10")
sum(is.na(sdf$WMH))/dim(sdf)[1]
# 91% of WMH data is missing/not available
# 'sdf' is the phenotypic data that we will want to bind with to the genotype (PRS) data in order to create our models




# (c): Merging phenotypic and genotypic data into final dataset 

# Then using the txt Effect files from PRS creation according to the pipeline: https://2cjenn.github.io/PRS_Pipeline/ in BlueCrystal4

# load in these 4 generated PRS scores for each individual:
AD_PRS <- read.delim("PRS/AD_PRS.sscore", header=FALSE, comment.char="#")
DBP_PRS <- read.delim("PRS/DBP_PRS.sscore", header=FALSE, comment.char="#")
SBP_PRS <- read.delim("PRS/SBP_PRS.sscore", header=FALSE, comment.char="#")
WMH_PRS <- read.delim("PRS/WMH_PRS.sscore", header=FALSE, comment.char="#")

# weird oddity where the first 371 individuals have incorrect IDs of -371 to -1
# These individuals will be removed in the merging as don't have a match  
# Merging phenotypic data to the PRS scores:

colnames(AD_PRS)=c("eid","eid2_AD","N_Alleles_AD", "Sum_Allele_Dosage_AD", "PRS_AD_AVG","PRS_AD")
colnames(DBP_PRS)=c("eid","eid2_DBP","N_Alleles_DBP", "Sum_Allele_Dosage_DBP", "PRS_DBP_AVG","PRS_DBP")
colnames(SBP_PRS)=c("eid","eid2_SBP","N_Alleles_SBP", "Sum_Allele_Dosage_SBP", "PRS_SBP_AVG","PRS_SBP")
colnames(WMH_PRS)=c("eid","eid2_WMH","N_Alleles_WMH", "Sum_Allele_Dosage_WMH", "PRS_WMH_AVG","PRS_WMH")
AD_PRS <- AD_PRS[,c(1,5,6)]
DBP_PRS <- DBP_PRS[, c(1,5,6)]
SBP_PRS <- SBP_PRS[, c(1,5,6)]
WMH_PRS <- WMH_PRS[, c(1,5,6)]

# Merge all data frames by 'id':
final <- merge(sdf, AD_PRS, by = "eid")
final <- merge(final, SBP_PRS, by = "eid")
final <- merge(final, DBP_PRS, by = "eid")
final <- merge(final, WMH_PRS, by = "eid")
# remove individuals with a non-positive age of dementia onset (n=4)
final <-  final[final$dementia_age > 0 | is.na(final$dementia_age)]
# save this final cleaned and merged dataset
write.csv(final, "R CODE/clean_merged.csv",row.names=FALSE)

############################ Exclusion list specifics ####################################

# 1. Sex mismatch = 372
sex_mismatch_ids <- fread("sexmismatch.txt")[[1]]
# 2. Putative sex chromosome aneuploidy = 651
aneuploidy_ids <- fread("aneiplody.txt")[[1]]
# 3. Het/missing outliers = 968
het_missing_outliers_ids <- fread("hetoutliers.txt")[[1]]
# 4. Non-White British ancestry = 92,787
non_white_british_ids <- fread("whitebritish.txt")[[1]]
# 5. Relatedness = 81,795
#(remove 1 individual of each related pair up to the third degree)
relatedness_ids <- fread("relateds.txt")[[1]]
# 6. Withdrawn individuals = 174
withdrawn_ids <- fread("withdraw.txt")[[1]]
# Combine all exclusion IDs = 176,747
all_ids <- c(sex_mismatch_ids, aneuploidy_ids, het_missing_outliers_ids, non_white_british_ids, relatedness_ids, withdrawn_ids)
# Remove duplicate IDs = 9,150. New Total = 167,597
unique_ids <- unique(all_ids)
# Create exclusion file
write.table(unique_ids, file = "exclusion_ids.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
#Example of using exclusion IDs on UKB PRS
#load in PRS score = 487,409
snp <- fread("~/Desktop/ResearchFellow/UKBiobank/WMH_PRS.sscore")
summary(snp)
#rename column names
colnames(snp)[colnames(snp)=="IID"] <- "eid" 
colnames(snp)[colnames(snp)=="SCORE1_SUM"] <- "PRS"
summary(snp)
#remove samples that did not pass QC, leaving = 335,130
clean_sscore_data <- snp[!snp$eid %in% unique_ids, ]
summary(clean_sscore_data)


