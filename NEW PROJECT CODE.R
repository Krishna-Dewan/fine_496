## clear memory - removes all objects from R environment to ensure clean workspace
rm(list=ls())


# ----------------------------
# 1. DATA LOADING & SETUP
# ----------------------------
```{r}

start_date <- 196401  
end_date <- 199012    

factors <- read.csv("Bond_Return_Forecasting_Factors.csv", header=TRUE)
head(factors)

# REPLACE THE PATH WITH YOUR OWN
ind_ret_12 <- read.csv("30_Industry_Portfolios_EDITED.csv", header=TRUE) 
ind_ret_12[, 1:ncol(ind_ret_12)] <- lapply(ind_ret_12[, 1:ncol(ind_ret_12)], as.numeric)
#head(ind_ret_12)
ind_ret_12[,2:ncol(ind_ret_12)] <- ind_ret_12[,2:ncol(ind_ret_12)]/100
#head(ind_ret_12)
```

# ----------------------------
# 2. DATA PREPARATION
# ----------------------------
```{r}
ind_ret_12_base <- ind_ret_12[which(ind_ret_12[,1] >= start_date & ind_ret_12[,1] <= end_date),] 
row_for_factors <- nrow(ind_ret_12_base) 
#print(row_for_factors) #324
factors_base <- factors[2:(row_for_factors + 1),] 
#print(nrow(factors_base)) ##324
```

# ----------------------------
# 3. IN-SAMPLE REGRESSION
# ----------------------------
```{r}
# In-sample regression analysis with formula:
# industry_return ~ factor1 + factor2 + factor3 + factor4 + factor5 + factor6 + factor7 + factor8 #does not use CP  

sink("ols_summaries_1.txt") #ADDED - to save summary tables in a txt file

# Three frequencies: monthly (1), quarterly (4), annual (12)
for (frequency in c(1,4,12)) {
  
  ###ADDED: will output which factor is significant for which industry 
  significance_table <- matrix("", nrow = 8, ncol = 3)  # 8 factors, 3 significance levels
  rownames(significance_table) <- paste("Factor", 1:8)
  colnames(significance_table) <- c("***", "**", "*")
  ###
  
  # Copy original to avoid overwriting after each iteration
  factors_t <- factors_base
  ind_ret_12_t <- ind_ret_12_base
  length(factors_t)
  length(ind_ret_12_t)
  
  # 1 month lag
  if(frequency == 1){ 
    factors_t <- factors_t[1:(nrow(factors_t)-1),] #CHANGED
    ind_ret_12_t <- ind_ret_12_t[2:nrow(ind_ret_12_t),] #CHANGED
  }
  
  # 1 quarter lag
  if(frequency == 4){ 
    factors_t <- factors_t[1:(nrow(factors_t)-4),] #CHANGED
    ind_ret_12_t <- ind_ret_12_t[5:nrow(ind_ret_12_t),] #CHANGED
    
  }
  # 1 year lag
  if(frequency == 12){ 
    factors_t <- factors_t[1:(nrow(factors_t)-12),] #CHANGED
    ind_ret_12_t <- ind_ret_12_t[13:nrow(ind_ret_12_t),] #CHANGED
  }
  
  for(i in 2:ncol(ind_ret_12_t)) {
    dependent_variable <- ind_ret_12_t[,i] # Current industry returns
    
    independent_variable <- cbind(factors_t[,2], factors_t[,3], factors_t[,4], 
                                  factors_t[,5], factors_t[,6], factors_t[,7],
                                  factors_t[,8], factors_t[,9]) 
    
    ols <- lm(dependent_variable ~ independent_variable)
    
    # Regression results
    cat("\nIndustry:", colnames(ind_ret_12_t)[i], "| Frequency:", frequency, "\n")
    print(summary(ols))
    
    ####ADDED: Below to populate the statistical output table
    # Get the summary of the regression
    regression_summary <- summary(ols)
    
    # Check the p-values for each coefficient and assign industry names based on significance
    for (factor_index in 2:ncol(regression_summary$coefficients)) {  # Skip the intercept
      p_value <- regression_summary$coefficients[factor_index, 4]  # p-value of the current factor
      
      # Determine which significance level the p-value falls under
      if (p_value <= 0.001) {
        significance_table[factor_index - 1, 1] <- paste(significance_table[factor_index - 1, 1], colnames(ind_ret_12_t)[i], sep = ", ")  # *** column
      } else if (p_value <= 0.01) {
        significance_table[factor_index - 1, 2] <- paste(significance_table[factor_index - 1, 2], colnames(ind_ret_12_t)[i], sep = ", ")  # ** column
      } else if (p_value <= 0.05) {
        significance_table[factor_index - 1, 3] <- paste(significance_table[factor_index - 1, 3], colnames(ind_ret_12_t)[i], sep = ", ")  # * column
      }
    ####
  }
  }
  print(significance_table) ##will do the significance table for each period - monthly, quarterly, annually
}

# Print the populated table
sink() #stop writing to txt

```


# --------------------------------------------------------
# 3. OUT-OF-SAMPLE FORECASTING WITH SELECT PREDICTORS
# --------------------------------------------------------
```{r}
# Coefficients selected based on p-value:
# f2, f4, f5

###Coefficients need to be reselected - did not change it down there


##########################
#####     MAIN CODE   ####
##########################
start_date_OOS <- 196401  
end_date_OOS <- 200312 
ind_ret_12_OOS <- ind_ret_12[which(ind_ret_12[,1] >= start_date_OOS & ind_ret_12[,1] <= end_date_OOS),]

start_idx = 205
end_idx = nrow(ind_ret_12_OOS)

fitted_active <- matrix(NA, nrow = end_idx - start_idx + 1, ncol = 30) ###CHANGED ncol
fitted_passive <- matrix(NA, nrow = end_idx - start_idx + 1, ncol = 30) ###CHANGED ncol

predictive_var_main <- cbind(factors[,3], factors[,5], factors[,6])

# Loop over all 12 industries to get returns
for (industry_index in 2:ncol(ind_ret_12_OOS)) {
  ret <- ind_ret_12_OOS[, industry_index]
  #print(ret)
  fitted_industry_active <- c()
  fitted_industry_passive <- c()
  
  # OOS
  for (i in start_idx:end_idx) {
    #print(i)
    y_train <- ret[1:(i - 1)]
    #print(y_train)
    X_train <- predictive_var_main[1:(i - 1), ]
    #print(X_train)
    X_passive <- rep(1, length(y_train))
    
    # Active
    ols <- lsfit(X_train, y_train, intercept = TRUE)
    
    pred <- as.numeric((ols$coef)[1] + ((ols$coef)[2])*predictive_var_main[i,1] + 
                         ((ols$coef)[3])*predictive_var_main[i,2] + ((ols$coef)[4])*predictive_var_main[i,3])
    
    fitted_industry_active <- c(fitted_industry_active, pred)
    
    # Passive 
    ols_passive <- lsfit(X_passive, y_train, intercept = FALSE)
    pred_passive <- as.numeric(ols_passive$coef[1])
    fitted_industry_passive <- c(fitted_industry_passive, pred_passive)
  }
  
  fitted_active[, industry_index - 1] <- fitted_industry_active
  fitted_passive[, industry_index - 1] <- fitted_industry_passive
}
colnames(fitted_active) <- colnames(ind_ret_12_OOS)[2:ncol(ind_ret_12_OOS)]
colnames(fitted_passive) <- colnames(ind_ret_12_OOS)[2:ncol(ind_ret_12_OOS)]

# Prediction Results
cat("ACTIVE predictions:\n")
print(round(fitted_active, 4))
cat("PASSIVE predictions:\n")
print(round(fitted_passive, 4))


##########################
#####    R_SQUARED   #####
##########################
ret_OOS <- ind_ret_12_OOS[start_idx:end_idx, 2:ncol(ind_ret_12_OOS)]

Rsq_OOS <- rep(NA, ncol(fitted_active))
names(Rsq_OOS) <- colnames(fitted_active)

# R-SQUARED for each industry
for (j in 1:ncol(fitted_active)) {
  a <- sum((ret_OOS[, j] - fitted_active[, j])^2)
  b <- sum((ret_OOS[, j] - fitted_passive[, j])^2)
  Rsq_OOS[j] <- 1 - (a / b)
}

# Print results
cat("------------------------------------------------------- \n")
cat("-- OUT-OF-SAMPLE R^2 BY INDUSTRY -- \n")
cat("------------------------------------------------------- \n")
print(round(Rsq_OOS, 4))
cat("------------------------------------------------------- \n")


##########################
########    MSE   ########
##########################

mse_active <- numeric(12)
mse_passive <- numeric(12)
mse_ratio <- numeric(12)

# Loop through each industry
for (j in 1:12) {
  actual <- ret_OOS[, j]
  pred_active <- fitted_active[, j]
  pred_passive <- fitted_passive[, j]
  
  mse_active[j] <- mean((actual - pred_active)^2)
  mse_passive[j] <- mean((actual - pred_passive)^2)
  
  mse_ratio[j] <- mse_active[j] / mse_passive[j]
}
names(mse_active) <- colnames(ind_ret_12_OOS)[2:13]
names(mse_passive) <- colnames(ind_ret_12_OOS)[2:13]
names(mse_ratio) <- colnames(ind_ret_12_OOS)[2:13]

# Print results
cat("\nMSE using active:\n")
print(round(mse_active, 6))

cat("\nMSE using passive:\n")
print(round(mse_passive, 6))

cat("\nMSE ratio (main / full):\n")
print(round(mse_ratio, 4))

##########################
####  SHARPE RATIO   #####
##########################

FF_factors<- read.table("FFfactors.txt",header=TRUE,sep="")
rf_OOS <- FF_factors[FF_factors$date >= start_date_OOS & FF_factors$date <= end_date_OOS, 5]
rf_OOS <- rf_OOS/100
rf_OOS <- rf_OOS[(start_idx):(end_idx)] # Match prediction period

sharpe_active <- numeric(12)
sharpe_passive <- numeric(12)

names(sharpe_active) <- colnames(fitted_active)
names(sharpe_passive) <- colnames(fitted_passive)

# Loop through each industry
for (j in 1:12) {
  # Active 
  excess_active <- fitted_active[, j] - rf_OOS
  sharpe_active[j] <- mean(excess_active) / sd(excess_active)
  
  # Passive
  excess_passive <- fitted_passive[, j] - rf_OOS
  sharpe_passive[j] <- mean(excess_passive) / sd(excess_passive)
}

# Results
cat("\nSharpe Ratios (Active):\n")
print(round(sharpe_active, 4))

cat("\nSharpe Ratios (Passive):\n")
print(round(sharpe_passive, 4))

```


# --------------------------------------------------------
# 3.1 CRISIS PERFORMANCE - DOTCOM
# --------------------------------------------------------
```{r}
dates_crisis <- ind_ret_12_OOS[start_idx:end_idx, 1]

idx_dotcom <- which(dates_crisis >= 200003 & dates_crisis <= 200212)

sharpe_active_dotcom <- numeric(12)
sharpe_passive_dotcom <- numeric(12)
mse_active_dotcom <- numeric(12)
mse_passive_dotcom <- numeric(12)
rsq_dotcom <- numeric(12)

names(sharpe_active_dotcom) <- colnames(fitted_active)
names(sharpe_passive_dotcom) <- colnames(fitted_passive)
names(mse_active_dotcom) <- colnames(fitted_active)
names(mse_passive_dotcom) <- colnames(fitted_passive)
names(rsq_dotcom) <- colnames(fitted_active)

# Loop through each industry
for (j in 1:12) {
  actual_dotcom <- ret_OOS[idx_dotcom, j]
  pred_active_dotcom <- fitted_active[idx_dotcom, j]
  pred_passive_dotcom <- fitted_passive[idx_dotcom, j]
  rf_dotcom <- rf_OOS[idx_dotcom]
  
  # R-squared
  a <- sum((actual_dotcom - pred_active_dotcom)^2)
  b <- sum((actual_dotcom - pred_passive_dotcom)^2)
  rsq_dotcom[j] <- 1 - (a / b)
  
  # MSE
  mse_active_dotcom[j] <- mean((actual_dotcom - pred_active_dotcom)^2)
  mse_passive_dotcom[j] <- mean((actual_dotcom - pred_passive_dotcom)^2)
  
  # Sharpe Ratio
  sharpe_active_dotcom[j] <- mean(pred_active_dotcom - rf_dotcom) / sd(pred_active_dotcom - rf_dotcom)
  sharpe_passive_dotcom[j] <- mean(pred_passive_dotcom - rf_dotcom) / sd(pred_passive_dotcom - rf_dotcom)
}

# Results
cat("\nSharpe Ratios (Active):\n")
print(round(sharpe_active_dotcom, 4))

cat("\nSharpe Ratios (Passive):\n")
print(round(sharpe_passive_dotcom, 4))

cat("\nMSE (Active):\n")
print(round(mse_active_dotcom, 6))

cat("\nMSE (Passive):\n")
print(round(mse_passive_dotcom, 6))

cat("\nR-squared:\n")
print(round(rsq_dotcom, 4))
```

# --------------------------------------------------------
# 4. MEAN_VARIANCE OPTIMIZATION PORTFOLIO
# --------------------------------------------------------
```{r}
# Covariance
cov <- cov(ret_OOS)

# rf
rf_mean <- mean(rf_OOS)

# Average pred returns for each industry
mean_active_ret <- colMeans(fitted_active)
mean_passive_ret <- colMeans(fitted_passive)

# Excess returns
mean_excess_ret_active <- mean_active_ret - mean(rf_OOS)
mean_excess_ret_passive <- mean_passive_ret - mean(rf_OOS)

# Compute z
z_active <- solve(cov) %*% mean_excess_ret_active
z_passive <- solve(cov) %*% mean_excess_ret_passive

# Portfolio weights
weights_active <- z_active / sum(z_active)
weights_passive <- z_passive / sum(z_passive)

# Port returns
port_ret_active <- fitted_active %*% weights_active
port_ret_passive <- fitted_passive %*% weights_passive

# Convert to vector for calculations
port_ret_active <- as.vector(port_ret_active)
port_ret_passive <- as.vector(port_ret_passive)

# Sharpe ratios
sharpe_active_mvo <- (mean(port_ret_active) - rf_mean) / sd(port_ret_active)
sharpe_passive_mvo <- (mean(port_ret_passive) - rf_mean) / sd(port_ret_passive)


# Results
cat("Weights (Active):\n")
print(round(weights_active, 4))
cat("\nWeights (Passive):\n")
print(round(weights_passive, 4))

cat("\nSharpe Ratio (Active Portfolio):", round(sharpe_active_mvo, 4), "\n")
cat("Sharpe Ratio (Passive Portfolio):", round(sharpe_passive_mvo, 4), "\n")
```


#
#
#
#
# Although the code above is with the selected predictors based on p-value, 
# I wanted to see how a model with ALL predictors would perform and it seems like
# it actuallly predicts better looking at the MSE ratio. Not sure if we keep this one
# or stick with the selected predictors model
#
#
#
# --------------------------------------------------------
# 3.1 OUT-OF-SAMPLE FORECASTING WITH ALL SELECT PREDICTORS
# --------------------------------------------------------

```{r}
start_date_OOS <- 196401  
end_date_OOS <- 200312 
ind_ret_12_OOS <- ind_ret_12[which(ind_ret_12[,1] >= start_date_OOS & ind_ret_12[,1] <= end_date_OOS),]

start_idx = 205
end_idx = nrow(ind_ret_12_OOS)

fitted_main_2 <- matrix(NA, nrow = end_idx - start_idx + 1, ncol = 12)

predictive_var_main <- cbind(factors[,2], factors[,3], factors[,4], 
                             factors[,5], factors[,6], factors[,7],
                             factors[,8], factors[,9])

for (industry_index in 2:ncol(ind_ret_12_OOS)) {
  ret <- ind_ret_12_OOS[, industry_index]
  #print(ret)
  fitted_industry <- c()
  
  for (i in start_idx:end_idx) {
    #print(i)
    y_train <- ret[1:(i - 1)]
    #print(y_train)
    X_train <- predictive_var_main[1:(i - 1), ]
    #print(X_train)
    
    ols <- lsfit(X_train, y_train, intercept = TRUE)
    
    pred <- as.numeric((ols$coef)[1] + ((ols$coef)[2])*predictive_var_main[i,1] + 
                         ((ols$coef)[3])*predictive_var_main[i,2] + ((ols$coef)[4])*predictive_var_main[i,3] + ((ols$coef)[5])*predictive_var_main[i,4] + ((ols$coef)[6])*predictive_var_main[i,5] + 
                         ((ols$coef)[7])*predictive_var_main[i,6] + ((ols$coef)[8])*predictive_var_main[i,7] +
                         ((ols$coef)[9])*predictive_var_main[i,8])
    
    fitted_industry <- c(fitted_industry, pred)
  }
  
  fitted_main_2[, industry_index - 1] <- fitted_industry
}
colnames(fitted_main_2) <- colnames(ind_ret_12_OOS)[2:ncol(ind_ret_12_OOS)]

print(round(fitted_main_2, 4))
```

# --------------------------------------------------------
# 3.2 COMPARING MSE FOR BOTH MODELS
# --------------------------------------------------------
```{R}
ret_OOS <- ind_ret_12_OOS[start_idx:end_idx, 2:ncol(ind_ret_12_OOS)]

# Initialize MSE vectors
mse_main <- numeric(12)
mse_full <- numeric(12)
mse_ratio <- numeric(12)

# Loop through each industry
for (j in 1:12) {
  actual <- ret_OOS[, j]
  pred_main <- fitted_main[, j]
  pred_full <- fitted_main_2[, j]
  
  mse_main[j] <- mean((actual - pred_main)^2)
  mse_full[j] <- mean((actual - pred_full)^2)
  
  mse_ratio[j] <- mse_main[j] / mse_full[j]
}
names(mse_main) <- colnames(ind_ret_12_OOS)[2:13]
names(mse_full) <- colnames(ind_ret_12_OOS)[2:13]
names(mse_ratio) <- colnames(ind_ret_12_OOS)[2:13]

# Print results
cat("\nMSE using f2, f4, f5:\n")
print(round(mse_main, 6))

cat("\nMSE using all factors:\n")
print(round(mse_full, 6))

cat("\nMSE ratio (main / full):\n")
print(round(mse_ratio, 4))

```