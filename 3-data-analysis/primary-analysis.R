# (a): Alzheimer's disease outcome models
# (b): Vascular dementia outcome models

# (a): Alzheimer's outcome models:
# just AD PRS on AD outcome:
model1a <- glm(DemAlz ~ PRS_AD_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model1a)
exp(coefficients(model1a))
# very strongly associated as we would expect -> one sd movement in PRS is associated with increase in
# odds of Alzheimer's by 116%

plot_CI <- function(model, alpha = 0.05) {
  coefs <- summary(model)$coefficients
  estimates <- coefs[, "Estimate"]
  std_errors <- coefs[, "Std. Error"]
  p_values <- coefs[, "Pr(>|z|)"]
  # Critical value for the specified confidence level
  z_alpha_2 <- qnorm(1 - alpha / 2)
  # Calculate the lower and upper bounds of the confidence intervals
  lower_bound <- estimates - z_alpha_2 * std_errors
  upper_bound <- estimates + z_alpha_2 * std_errors
  # Combine into a data frame for easy viewing
  
  # Exponentiate after formatting
  wald_ci <- data.frame(
    Variable = rownames(coefs),
    OR = (exp(estimates)),  # Apply exp after rounding and formatting
    SE = (std_errors),      # Apply custom formatting for Std Errors
    LowerCI = (exp(lower_bound)),  # Apply exp after rounding and formatting
    UpperCI = (exp(upper_bound)), # Apply exp after rounding and formatting
    P_Value = (p_values)
  )
  rownames(wald_ci) <- NULL
  return(wald_ci)
}


wald_CI <- function(model, alpha = 0.05) {
  coefs <- summary(model)$coefficients
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
    OR = sapply(exp_estim, custom_format2),
    SE = sapply(std_errors, custom_format),
    Lower_CI = sapply(exp_lowerbd, custom_format2),
    Upper_CI = sapply(exp_upperbd, custom_format2),
    P_Value = sapply(p_values, custom_format_p),
    stringsAsFactors = FALSE
  )
  
  # Correct formatting for the intercept row
  intercept <- formatted_results[1, ]
  intercept$OR <- custom_format2(exp_estim[1])
  intercept$SE <- custom_format(std_errors[1])
  intercept$Lower_CI <- custom_format2(exp_lowerbd[1])
  intercept$Upper_CI <- custom_format2(exp_upperbd[1])
  intercept$P_Value <- custom_format_p(p_values[1])
  
  # Rebind intercept with formatted results
  formatted_results[1, ] <- intercept
  formatted_results=formatted_results[c(1:5, 16:17),]
  
  
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
wald_CI(model1a)

# just SBP PRS: 
model2a <- glm(DemAlz ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model2a)
wald_CI(model2a)
# just DBP PRS:
model3a <- glm(DemAlz ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model3a)
wald_CI(model3a)
# AD and SBP interaction:
modela1<- glm(DemAlz ~ PRS_AD_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela1)
wald_CI(modela1)
# AD and DBP interaction:
modela2 <- glm(DemAlz ~ PRS_AD_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modela2)
wald_CI(modela2)

# (b) Vascular dementia outcome models:
# just WMH PRS on VD outcome:
model1b <- glm(DemVas ~ PRS_WMH_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model1b)
wald_CI(model1b)
# just SBP PRS: 
model2b <- glm(DemVas ~ PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model2b)
wald_CI(model2b)
# just DBP PRS:
model3b <- glm(DemVas ~ PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(model3b)
wald_CI(model3b)
# WMH and SBP interaction:
modeld1 <- glm(DemVas ~ PRS_WMH_STD*PRS_SBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modeld1)
wald_CI(modeld1)
# WMH and DBP interaction:
modeld2 <- glm(DemVas ~ PRS_WMH_STD*PRS_DBP_STD+Sex+Age+GPC1+GPC2+GPC3+GPC4+GPC5+GPC6+GPC7+GPC8+GPC9+GPC10, data = final, family = binomial)
summary(modeld2)
wald_CI(modeld2)

wald_CIt <- function(model, alpha = 0.05) {
  coefs <- summary(model)$coefficients
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
    OR = sapply(exp_estim, custom_format2),
    SE = sapply(std_errors, custom_format),
    Lower_CI = sapply(exp_lowerbd, custom_format2),
    Upper_CI = sapply(exp_upperbd, custom_format2),
    P_Value = sapply(p_values, custom_format_p),
    stringsAsFactors = FALSE
  )
  
  # Correct formatting for the intercept row
  intercept <- formatted_results[1, ]
  intercept$OR <- custom_format2(exp_estim[1])
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
