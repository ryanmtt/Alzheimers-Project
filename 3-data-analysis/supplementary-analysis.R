# (1) All-cause dementia outcome models
# (2) Models split at age at baseline 
# (3) Binarised PRS scores 
# (4) Poisson models

########################## 1: All-cause dementia outcome models #####################################

# AD and SBP interaction:
modelac1<- glm(DemAllCause ~ PRS_AD_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelac1)
wald_CIt(modelac1)
# AD and DBP interaction:
modelac2 <- glm(DemAllCause ~ PRS_AD_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelac2)
wald_CIt(modelac2)
# WMH and SBP interaction:
modelac3 <- glm(DemAllCause ~ PRS_WMH_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelac3)
wald_CIt(modelac3)
# WMH and DBP interaction:
modelac4 <- glm(DemAllCause ~ PRS_WMH_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelac4)
wald_CIt(modelac4)

########################## 2: Models split at age at baseline  #####################################

# see if those diagnosed in the under 65 category have different relationships
# with AD/dementia outcome going on?

group1 = final[final$Age < 65]
group2 = final[final$Age >= 65] 

# ad on AD:
model_ad_age_young <- glm(DemAlz ~ PRS_AD_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
model_ad_age_old<- glm(DemAlz ~ PRS_AD_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(model_ad_age_young)
wald_CI(model_ad_age_old)
# sbp on AD:
modelsbp_age_young <- glm(DemAlz ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelsbp_age_old <- glm(DemAlz ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(modelsbp_age_young)
wald_CI(modelsbp_age_old)
# dbp on AD:
modeldbp_age_young <- glm(DemAlz ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modeldbp_age_old <- glm(DemAlz ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(modeldbp_age_young)
wald_CI(modeldbp_age_old)
# AD and SBP interaction:
modelint1_age_young<- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelint1_age_old<- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CIt(modelint1_age_young)
wald_CIt(modelint1_age_old)
# AD and DBP interaction:
modelint2_age_young<- glm(DemAlz ~ PRS_AD_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelint2_age_old<- glm(DemAlz ~ PRS_AD_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CIt(modelint2_age_young)
wald_CIt(modelint2_age_old)
# strength of evidence of association is lost due to smaller sample
# but the estimates are pretty similar (specifically of BP on AD)

# Split age group on vascular outcome:
# WMH on VD:
model_wmh_age_young <- glm(DemVas ~ PRS_WMH_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
model_wmh_age_old<- glm(DemVas ~ PRS_WMH_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(model_wmh_age_young)
wald_CI(model_wmh_age_old)
# sbp on VD:
modelvd_sbp_age_young <- glm(DemVas ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelvd_sbp_age_old <- glm(DemVas ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(modelvd_sbp_age_young)
wald_CI(modelvd_sbp_age_old)
# dbp on VD:
modelvd_dbp_age_young <- glm(DemVas ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelvd_dbp_age_old <- glm(DemVas ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CI(modelvd_dbp_age_young)
wald_CI(modelvd_dbp_age_old)
# WMH and SBP interaction:
modelint3_age_young<- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelint3_age_old<- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CIt(modelint3_age_young)
wald_CIt(modelint3_age_old)
# WMH and DBP interaction:
modelint4_age_young<- glm(DemVas ~ PRS_WMH_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group1, family = binomial)
modelint4_age_old<- glm(DemVas ~ PRS_WMH_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = group2, family = binomial)
wald_CIt(modelint4_age_young)
wald_CIt(modelint4_age_old)

# TESTING FOR AN INTERACTION BETWEEN AGE GROUP AND SBP PRS in VD and AD:

final$age_binary <- ifelse(final$Age >= 65, 1, 0)

modelint1_age_tt<- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_STD+Sex+age_binary*PRS_SBP_STD+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
wald_CI(modelint1_age_tt)
modelint3_age_tt<- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_STD+Sex+age_binary*PRS_SBP_STD+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
wald_CI(modelint3_age_tt)

########################## 3: Binarised PRS scores #####################################

final$PRS_AD_b<- ifelse(final$PRS_AD_STD > quantile(final$PRS_AD_STD[1], 0.75), 1, 0)
final$PRS_SBP_b<- ifelse(final$PRS_SBP_STD > quantile(final$PRS_SBP_STD[1], 0.75), 1, 0)
final$PRS_DBP_b<- ifelse(final$PRS_DBP_STD > quantile(final$PRS_DBP_STD[1], 0.75), 1, 0)
final$PRS_WMH_b<- ifelse(final$PRS_WMH_STD > quantile(final$PRS_WMH_STD[1], 0.75), 1, 0)

# (a) Alzheimer's Disease Outcome
# AD and SBP interaction:
modela_ba<- glm(DemAlz ~ PRS_AD_b*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela_ba)
wald_CIt(modela_ba)
modela_bs<- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela_bs)
wald_CIt(modela_bs)
modela_bb<- glm(DemAlz ~ PRS_AD_b*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela_bb)
wald_CIt(modela_bb)
# AD and DBP interaction:
modela2_ba<- glm(DemAlz ~ PRS_AD_b*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela2_ba)
wald_CIt(modela2_ba)
modela2_bd<- glm(DemAlz ~ PRS_AD_STD*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela2_bd)
wald_CIt(modela2_bd)
modela2_bb<- glm(DemAlz ~ PRS_AD_b*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela2_bb)
wald_CIt(modela2_bb)

# (b) Vascular Dementia outcome
# WMH and SBP interaction:
modelv_bw<- glm(DemVas ~ PRS_WMH_b*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv_bw)
wald_CIt(modelv_bw)
modelv_bs<- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv_bs)
wald_CIt(modelv_bs)
modelv_bb<- glm(DemVas ~ PRS_WMH_b*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv_bb)
wald_CIt(modelv_bb)
# WMH and DBP interaction:
modelv2_bw<- glm(DemVas ~ PRS_WMH_b*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv2_bw)
wald_CIt(modelv2_bw)
modelv2_bd<- glm(DemVas ~ PRS_WMH_STD*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv2_bd)
wald_CIt(modelv2_bd)
modelv2_bb<- glm(DemVas ~ PRS_WMH_b*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelv2_bb)
wald_CIt(modelv2_bb)

# (c) All Cause Dementia Outcome
# AD and SBP interaction:
modelall_ba<- glm(DemAllCause ~ PRS_AD_b*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall_ba)
wald_CI(modelall_ba)
modelall_bs<- glm(DemAllCause ~ PRS_AD_STD*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall_bs)
wald_CI(modelall_bs)
modelall_bb<- glm(DemAllCause ~ PRS_AD_b*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall_bb)
wald_CI(modelall_bb)
# AD and DBP interaction:
modelall2_ba<- glm(DemAllCause ~ PRS_AD_b*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall2_ba)
wald_CI(modelall2_ba)
modelall2_bd<- glm(DemAllCause ~ PRS_AD_STD*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall2_bd)
wald_CI(modelall2_bd)
modelall2_bb<- glm(DemAllCause ~ PRS_AD_b*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall2_bb)
wald_CI(modelall2_bb)
# WMH and SBP interaction:
modelall3_bw<- glm(DemAllCause ~ PRS_WMH_b*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall3_bw)
wald_CI(modelall3_bw)
modelall3_bs<- glm(DemAllCause ~ PRS_WMH_STD*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall3_bs)
wald_CI(modelall3_bs)
modelall3_bb<- glm(DemAllCause ~ PRS_WMH_b*PRS_SBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall3_bb)
wald_CI(modelall3_bb)
# WMH and DBP interaction:
modelall4_bw<- glm(DemAllCause ~ PRS_WMH_b*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall4_bw)
wald_CI(modelall4_bw)
modelall4_bd<- glm(DemAllCause ~ PRS_WMH_STD*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall4_bd)
wald_CI(modelall4_bd)
modelall4_bb<- glm(DemAllCause ~ PRS_WMH_b*PRS_DBP_b+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modelall4_bb)
wald_CI(modelall4_bb)

######################## 4: Poisson Models ###################################

poisson_model <- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = poisson())
poisson_model2 <- glm(DemAlz ~ PRS_AD_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = poisson())
poisson_model3 <- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = poisson())
poisson_model4 <- glm(DemVas ~ PRS_WMH_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = poisson())
library(sandwich)  # For robust standard errors
library(lmtest)

# check for overdispersion:
overdispersion_stat <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
print(overdispersion_stat)

# there is overdispersion (overdispersion stat >1), so we use robust se
robust_se <- vcovHC(poisson_model, type = "HC0")
coeftest(poisson_model, vcov = robust_se)
robust_se2 <- vcovHC(poisson_model2, type = "HC0")
coeftest(poisson_model2, vcov = robust_se2)
robust_se3 <- vcovHC(poisson_model3, type = "HC0")
coeftest(poisson_model3, vcov = robust_se3)
robust_se4 <- vcovHC(poisson_model4, type = "HC0")
coeftest(poisson_model4, vcov = robust_se4)

wald_CI2 <- function(model, se, alpha = 0.05) {
  coefs <- coeftest(model, vcov=se)
  estimates <- coefs[, "Estimate"]
  std_errors <- coefs[, "Std. Error"]
  p_values <- coefs[, "Pr(>|z|)"]
  
  # Critical value for the specified confidence level
  z_alpha_2 <- qnorm(1 - alpha / 2)
  
  # Calculate the lower and upper bounds of the confidence intervals
  lower_bound <- estimates - z_alpha_2 * std_errors
  upper_bound <- estimates + z_alpha_2 * std_errors
  
  # Exponentiate estimates and confidence intervals
  exp_estim <- exp(estimates)
  exp_lowerbd <- exp(lower_bound)
  exp_upperbd <- exp(upper_bound)
  
  # Define custom formatting functions
  custom_format <- function(x) {
    if (abs(x) < 0.0000005) {  # Use scientific notation for very small values
      formatC(x, format = "e", digits = 0)
    } else if (abs(x) < 0.000005) {  # Use 6 decimal places
      formatC(x, format = "f", digits = 6)
    } else if (abs(x) < 0.00005) {  # Use 5 decimal places
      formatC(x, format = "f", digits = 5)
    } else if (abs(x) < 0.0005) {  # Use 4 decimal places
      formatC(x, format = "f", digits = 4)
    } else if (abs(x) < 0.005) {  # Use 3 decimal places
      formatC(x, format = "f", digits = 3)
    } else {  # Use 2 decimal places for larger values
      formatC(x, format = "f", digits = 2)
    }
  }
  custom_format2 <- function(x) {
    if (abs(x - 1) < 0.005) {  # Close to 1, use dynamic significant figures
      diff <- abs(x - 1)
      if (diff >= 0.0005) {
        formatted <- formatC(x, format = "f", digits = 3)  # Use 3 significant digits
      } else if (diff >= 0.00005) {
        formatted <- formatC(x, format = "f", digits = 4)  # Use 4 significant digits
      } else if (diff >= 0.000005) {
        formatted <- formatC(x, format = "f", digits = 5)  # Use 5 significant digits
      } else {
        formatted <- formatC(x, format = "f", digits = 6)  # Use 6 significant digits
      }
      return(formatted)
    } else {
      formatC(x, format = "f", digits = 2)  # Use 2 decimal places for other cases
    }
  }
  
  # Format p-values with a special condition
  custom_format_p <- function(p) {
    if (p < 2e-16) {
      "<2e-16"
    } else {
      custom_format(p)
    }
  }
  
  # Format results for all rows
  formatted_results <- data.frame(
    Variable = rownames(coefs),
    RR = sapply(exp_estim, custom_format2),
    SE = sapply(std_errors, custom_format),
    Lower_CI = sapply(exp_lowerbd, custom_format2),
    Upper_CI = sapply(exp_upperbd, custom_format2),
    P_Value = sapply(p_values, custom_format_p),
    stringsAsFactors = FALSE
  )
  
  # Correct formatting for the intercept row
  intercept <- formatted_results[1, ]
  intercept$RR <- custom_format2(exp_estim[1])
  intercept$SE <- custom_format(std_errors[1])
  intercept$Lower_CI <- custom_format2(exp_lowerbd[1])
  intercept$Upper_CI <- custom_format2(exp_upperbd[1])
  intercept$P_Value <- custom_format_p(p_values[1])
  
  # Rebind intercept with formatted results
  formatted_results[1, ] <- intercept
  formatted_results=formatted_results[c(2:3, 16),]
  # Create flextable and add borders
  library(flextable)
  ft <- flextable(formatted_results)
  ft <- border_remove(x = ft)
  big_border <- fp_border_default(color = "black", width = 2)
  small_border <- fp_border_default(color = "gray", width = 1)
  ft <- border_inner_h(ft, part = "all", border = small_border)
  ft <- border_inner_v(ft, part = "all", border = small_border)
  ft <- border_outer(ft, part = "all", border = big_border)
  
  return(ft)
}

wald_CI2(poisson_model,robust_se)
wald_CI2(poisson_model2,robust_se2)
wald_CI2(poisson_model3,robust_se3)
wald_CI2(poisson_model4,robust_se4)

##############################################################################################
