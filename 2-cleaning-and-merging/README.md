# Data preperation
Prepration is split into three sections in the `exclusions-cleaning-merging.R` file.

First, we read in the phenotypic data from UKB into R, some columns are renamed to move appropriate titles, and some basic exploration is done to observe how some variables vary across those with and without AD. 

Next, individuals were excluded if they had a sex mismatch, putative sex chromosome aneuploidy, outlying heterozygosity, non-white British ancestry, were related to other participants or withdrew participation from the study.  

Finally, PRS scores for AD, SBP, DBP, and WMH are merged with phenotypic data to create the final dataset for analysis.
