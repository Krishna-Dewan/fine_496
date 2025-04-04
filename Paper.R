## clear memory - removes all objects from R environment to ensure clean workspace
rm(list=ls())

# ----------------------------
# 1. DATA LOADING & SETUP
# ----------------------------
# Note: Library loading is commented out but would be needed for actual analysis
# Define analysis parameters:
start_date <- 200001  # Start date in YYYYMM format
end_date <- 201012    # End date in YYYYMM format
estimation_window <- 60  # 60-month (5-year) rolling window for regressions

# Load macroeconomic factors data (date column + 8 factors)
factors <- read.csv("Bond_Return_Forecasting_Factors.csv", header=TRUE)
# Factors are loaded but not renamed (comments indicate CP, f1-f8 exist)

# Load industry return data for different classification granularities:
# 12-industry classification
ind_ret_12 <- read.csv("12_Industry_Portfolios.CSV", header=FALSE)
ind_ret_12[,2:ncol(ind_ret_12)] <- ind_ret_12[,2:ncol(ind_ret_12)]/100 # Convert returns from percentages to decimals
head(ind_ret_12) # Preview first few rows

# 17-industry classification  
ind_ret_17 <- read.csv("17_Industry_Portfolios.CSV", header=FALSE)
head(ind_ret_17)

# 30-industry classification (without dividends)
ind_ret_30 <- read.csv("30_Industry_Portfolios_Wout_Div.csv", header=FALSE)
head(ind_ret_30)

# 38-industry classification
ind_ret_38 <- read.csv("38_Industry_Portfolios.CSV", header=FALSE)
head(ind_ret_38)

# ----------------------------
# 2. DATA PREPARATION
# ----------------------------
# Trim all datasets to the analysis period (200001-201012):

# Trim 12-industry data
ind_ret_12_t <- ind_ret_12[which(ind_ret_12[,1] >= start_date & ind_ret_12[,1] <= end_date),]
nrow(ind_ret_12_t) # Verify row count

# Trim 17-industry data
ind_ret_17_t <- ind_ret_17[which(ind_ret_17[,1] >= start_date & ind_ret_17[,1] <= end_date),]
nrow(ind_ret_17_t)

# Trim 30-industry data
ind_ret_30_t <- ind_ret_30[which(ind_ret_30[,1] >= start_date & ind_ret_30[,1] <= end_date),]
nrow(ind_ret_30_t)

# Trim 38-industry data (using ind_ret_30 date column - POTENTIAL ISSUE)
ind_ret_38_t <- ind_ret_38[which(ind_ret_30[,1] >= start_date & ind_ret_30[,1] <= end_date),]
nrow(ind_ret_38_t)

# Trim factors data - hardcoded row selection (2:529) for date alignment
factors_t <- factors[2:529,] # Creates base factors dataset
factors_t_17 <- factors[2:529,] # Duplicate for 17-industry (redundant)
factors_t_30 <- factors[2:529,] # Duplicate for 30-industry (redundant)

# Verify row counts match across all datasets
stopifnot(nrow(factors_t) == nrow(ind_ret_12_t),
          nrow(factors_t) == nrow(ind_ret_17_t),
          nrow(factors_t_30) == nrow(ind_ret_30_t),
          nrow(factors_t_30) == nrow(ind_ret_38_t))

# ----------------------------
# 3. REGRESSION ANALYSIS
# ----------------------------
# In-sample regression analysis with formula:
# industry_return ~ factor1 + I(factor1^3) + factor2 + factor3 + factor4 + factor5 + factor9

# Loop through three frequencies: monthly (1), quarterly (4), annual (12)
for (frequency in c(1,4,12)) {
  print(paste("Processing frequency:", frequency))
  
  # Create appropriate lags based on frequency:
  if(frequency == 1){ # Monthly frequency - 1 month lag
    factors_t <- factors_t[2:nrow(factors_t),] # Lag factors by 1 month
    # Lag all industry returns by 1 month using 12-industry row count as reference
    ind_ret_12_t <- ind_ret_12_t[1:nrow(ind_ret_12_t)-1,]
    ind_ret_17_t <- ind_ret_17_t[1:nrow(ind_ret_12_t)-1,]
    ind_ret_30_t <- ind_ret_30_t[1:nrow(ind_ret_12_t)-1,]
    ind_ret_38_t <- ind_ret_38_t[1:nrow(ind_ret_12_t)-1,]
  }
  if(frequency == 4){ # Quarterly frequency - 1 quarter lag
    factors_t <- factors_t[,5:nrow(factors)] # Lag factors by 1 quarter
    # Lag all industry returns by 1 quarter
    ind_ret_12_t <- ind_ret_12_t[1:nrow(ind_ret_12_t)-4,]
    ind_ret_17_t <- ind_ret_17_t[1:nrow(ind_ret_12_t)-4,]
    ind_ret_30_t <- ind_ret_30_t[1:nrow(ind_ret_12_t)-4,]
    ind_ret_38_t <- ind_ret_38_t[1:nrow(ind_ret_12_t)-4,]
  }
  if(frequency == 12){ # Annual frequency - 1 year lag
    factors_annual <- factors_t[,13:nrow(factors_t)] # Lag factors by 1 year
    # Lag all industry returns by 1 year
    ind_ret_12_t <- ind_ret_12_t[1:nrow(ind_ret_12_t)-12,]
    ind_ret_17_t <- ind_ret_17_t[1:nrow(ind_ret_12_t)-12,]
    ind_ret_30_t <- ind_ret_30_t[1:nrow(ind_ret_12_t)-12,]
    ind_ret_38_t <- ind_ret_38_t[1:nrow(ind_ret_12_t)-12,]
  }
  
  # Process each industry classification granularity
  for(granularity in c(12,17,30,38)) {
    print(paste("Processing granularity:", granularity))
    
    # Select appropriate dataset based on granularity
    if(granularity == 12){ind_data <- ind_ret_12_t}
    if(granularity == 17){ind_data <- ind_ret_17_t}
    if(granularity == 30){ind_data <- ind_ret_30_t}
    if(granularity == 38){ind_data <- ind_ret_38_t}
    
    # Run regression for each industry in the classification
    for(i in 2:ncol(ind_data)) {
      dependent_variable <- ind_data[,i] # Current industry's returns
      length(dependent_variable) # Check vector length (debugging)
      
      # Create independent variables matrix:
      # factor1, factor1^3, factor2, factor3, factor4, factor5, factor9
      independent_variable <- cbind(factors_t[,2], factors_t[,2]^3, 
                                    factors_t[,3], factors_t[,4], 
                                    factors_t[,5], factors_t[,9])
      
      # Run linear regression
      ols <- lm(dependent_variable ~ independent_variable)
      
      # Print regression results
      cat("\nIndustry:", colnames(ind_data)[i], 
          "| Frequency:", frequency, 
          "| Granularity:", granularity, "\n")
      print(summary(ols))
    }
  }
}


########################################################
###########anything after thsi line is not complete yet #################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
    
    #   - Perform rolling window regression:
    #       for each period in estimation_window+1:nperiods
    #           train_data = current_window - estimation_window
    #           fit = lm(formula, data = train_data)
    #           store coefficients and predictions
    #       end
    #   - Save out-of-sample predictions for portfolio construction  
    
    
    # Rolling window regression
    start_idx <- which(unique_months == start_date)
    for(i in (start_idx + estimation_window):length(unique_months)) {
      current_month <- unique_months[i]
      print(current_month)  # Your characteristic progress tracking
      
      # Training window
      train_start <- i - estimation_window
      train_end <- i - 1
      
      # Storage for this window
      coefficients <- array(NA, c(ncol(ind_data)-1, 7))  # 6 factors + intercept
      predictions <- array(NA, c(ncol(ind_data)-1))
      
      # Industry return loop
      for(ind in 2:ncol(ind_data)) {
        # Prepare data using your merge pattern
        y <- ind_data[train_start:train_end, ind]
        x <- cbind(factors_monthly[train_start:train_end, 2], 
                   (factors_monthly[train_start:train_end, 2])^3,
                   factors_monthly[train_start:train_end, 3:6])
        
        # Your exact regression approach
        ols <- lsfit(x, y, intercept=TRUE)
        coefficients[ind-1,] <- ols$coef
        
        # Make prediction
        current_x <- c(1, factors_monthly[i, 2], 
                       (factors_monthly[i, 2])^3,
                       factors_monthly[i, 3:6])
        predictions[ind-1] <- sum(ols$coef * current_x)
      }
      
      # Value-weight portfolios using your exact method
      value_wgt <- predictions / sum(predictions)
      
      # Store results
      ret_month <- rbind(ret_month, value_wgt)
      number_stocks <- rbind(number_stocks, length(value_wgt))
    }
  }
}





# ----------------------------
# 3. REGRESSION ANALYSIS MODULE - OUT OF SAMPLE
# ----------------------------
oos_predictions_12 <- array(NA, c(0,12))
oos_actuals_12 <- array(NA, c(0,12))
rolling_R2_12 <- array(NA, c(0))

# ----------------------------
# OUT-OF-SAMPLE REGRESSION ENGINE
# ----------------------------


# Rolling Window Setup
# ----------------------------
# - Initialize storage matrices:
#   * oos_predictions: matrix[periods × industries]
#   * oos_actuals: matrix[periods × industries]
#   * rolling_R2: array[periods]
# - Define expanding/rolling window parameters:
#   * start_idx = first complete window
#   * window_size = estimation_window (e.g., 60 months)

# 3. Core Regression Loop
# ----------------------------
# For each industry granularity:
#   For each rebalancing frequency:
#     For each time period t in [window_size+1 : nperiods]:
#       
#       a. Training Data:
#           - If monthly: t-window_size to t-1
#           - If quarterly: last 20 quarters
#           - If annual: last 5 years
#       
#       b. Model Specification:
#           - Formula: return ~ factor1_lag + I(factor1_lag^3) + 
#                      factor2_lag + ... + factor6_lag
#       
#       c. Estimate Model:
#           - Fit linear regression on training window
#           - Store coefficients and model statistics
#       
#       d. Make Prediction:
#           - Predict next period return using current factors
#           - Store prediction vs actual realization
#       
#       e. Window Maintenance:
#           - For rolling window: drop oldest observation
#           - For expanding window: retain all history

# 4. Performance Metrics Calculation
# ----------------------------
# For each industry/frequency combination:
#   a. Compute Out-of-Sample R²:
#       R2_OOS = 1 - (SSE_oos / SST_oos)
#       Where:
#       SSE = Σ(actual - predicted)²
#       SST = Σ(actual - mean_actual)²
#   
#   b. Calculate MSE and MAE:
#       MSE = mean((actual - predicted)^2)
#       MAE = mean(abs(actual - predicted))
fitted_active<- array(0,c(0))
fitted_passive<- array(0,c(0))
variance_mkt<- array(0,c(0))

data_1<- data[data[,1]>=193501 & data[,1]<=201012,]
mkt_ret<- data[data[,1]>=193501 & data[,1]<=201012,ncol(data)-1]
predictor_var<- (data[data[,1]>=193412 & data[,1]<=201011,8] - data[data[,1]>=193412 & data[,1]<=201011,7])


for (i in which(data_1[,1]==196501):nrow(data_1)){
  predictor_var_active<- predictor_var[1:(i-1)]
  predictor_var_passive<- array(1,c(length(predictor_var_active),1))
  mkt_ret_i<- mkt_ret[1:(i-1)]
  
  ols_active<-lsfit(predictor_var_active,mkt_ret_i,intercept=TRUE)
  xx<- (ols_active$coef)[1] + ((ols_active$coef)[2])*predictor_var[i]
  fitted_active<- rbind(fitted_active,xx)
  
  ols_passive<-lsfit(predictor_var_passive,mkt_ret_i,intercept=FALSE)
  xx<- (ols_passive$coef)[1]
  fitted_passive<- rbind(fitted_passive,xx)
  
  variance_mkt<- rbind(variance_mkt,var(mkt_ret[(i-60):(i-1)]))	
}


data<- data_1
mkt_ret_OOS<- data[which(data[,1]==196501):nrow(data),ncol(data)-1]
risk_free_OOS<- data[which(data[,1]==196501):nrow(data),ncol(data)-7]

Rsq_OOS<- 1-(sum((mkt_ret_OOS-fitted_active)^2)/sum((mkt_ret_OOS-fitted_passive)^2))

cat("------------------------------------------------------- \n")
cat("-- OUT-OF-SAMPLE REGRESSION -- \n")
cat("------------------------------------------------------- \n")
cat("-- R^2 -- \n")
print(Rsq_OOS)
cat("------------------------------------------------------- \n")

# ----------------------------
# 5. PORTFOLIO CONSTRUCTION
# ----------------------------
# For each granularity/frequency combination:
#   a. Active Strategy:
#       - Use predicted returns from regression
#       - Compute covariance matrix using historical returns
#       - Solve mean-variance optimization with constraints:
#           max w'μ - λw'Σw
#           s.t. sum(w) = 1, w >= 0 (long-only)

#   b. Passive Strategy:
#       - Use historical average returns
#       - Same covariance matrix and optimization

#   c. Calculate portfolio returns:
#       active_ret = sum(w_active * actual_returns)
#       passive_ret = sum(w_passive * actual_returns)



# ----------------------------
# 5. PERFORMANCE EVALUATION
# ----------------------------
# For each strategy combination:
#   - Calculate performance metrics:
#       * Annualized return
#       * Annualized volatility
#       * Sharpe/Sortino ratios
#       * Maximum drawdown
#       * Turnover analysis
#       * Tracking error vs passive
#   - Store results in comparable dataframe

# ----------------------------
# OPTIONAL:  CYCLICALITY ANALYSIS
# ----------------------------
# Load NBER recession dates
# For each strategy:
#   - Split returns into recession/expansion periods
#   - Compare performance characteristics:
#       * Mean returns during recessions vs expansions
#       * Volatility regimes
#       * Risk-adjusted returns
#   - Statistical tests for regime differences

# ----------------------------
# 7. VISUALIZATION & REPORTING
# ----------------------------
# Generate comparative plots:
#   - Cumulative returns by strategy
#   - Rolling Sharpe ratios
#   - Factor exposure over time
#   - Portfolio weights heatmaps
#   - Recession overlays on performance

# Output formatted tables:
#   - Performance metrics by granularity/frequency
#   - Statistical significance tests
#   - Cyclicality analysis results



# ----------------------------
# 3. REGRESSION ANALYSIS
# ----------------------------
# Initialize storage arrays using your pattern


# Main loop for 12-industry analysis
for(granularity in c(12,17,30,38)) {  # Process each granularity separately
  print(paste("Processing granularity:", granularity))
  
  # Select appropriate dataset
  if(granularity == 12) ind_data <- ind_ret_12
  if(granularity == 17) ind_data <- ind_ret_17
  if(granularity == 30) ind_data <- ind_ret_30
  if(granularity == 38) ind_data <- ind_ret_38
  
  # Align dates with your index pattern
  unique_months <- sort(unique(ind_data$yearmonth))
  index <- 1:length(unique_months)
  
  # Initialize storage
  ret_month <- array(NA, c(0, ncol(ind_data)-1))
  number_stocks <- array(NA, c(0))
  
  # Rolling window regression
  start_idx <- which(unique_months == start_date)
  for(i in (start_idx + estimation_window):length(unique_months)) {
    current_month <- unique_months[i]
    print(current_month)  # Your characteristic progress tracking
    
    # Training window
    train_start <- i - estimation_window
    train_end <- i - 1
    
    # Storage for this window
    coefficients <- array(NA, c(ncol(ind_data)-1, 7))  # 6 factors + intercept
    predictions <- array(NA, c(ncol(ind_data)-1))
    
    # Industry return loop
    for(ind in 2:ncol(ind_data)) {
      # Prepare data using your merge pattern
      y <- ind_data[train_start:train_end, ind]
      x <- cbind(factors_monthly[train_start:train_end, 2], 
                 (factors_monthly[train_start:train_end, 2])^3,
                 factors_monthly[train_start:train_end, 3:6])
      
      # Your exact regression approach
      ols <- lsfit(x, y, intercept=TRUE)
      coefficients[ind-1,] <- ols$coef
      
      # Make prediction
      current_x <- c(1, factors_monthly[i, 2], 
                     (factors_monthly[i, 2])^3,
                     factors_monthly[i, 3:6])
      predictions[ind-1] <- sum(ols$coef * current_x)
    }
    
    # Value-weight portfolios using your exact method
    value_wgt <- predictions / sum(predictions)
    
    # Store results
    ret_month <- rbind(ret_month, value_wgt)
    number_stocks <- rbind(number_stocks, length(value_wgt))
  }
  
  # ----------------------------
  # 4. PERFORMANCE METRICS
  # ----------------------------
  # Your exact metric calculations
  ann_return <- apply(ret_month, 2, mean) * 1200
  ann_vol <- apply(ret_month, 2, sd) * sqrt(12) * 100
  sharpe <- ann_return / ann_vol
  
  # Print results in your style
  cat("-------------------------------------------------------\n")
  cat("Granularity:", granularity, "\n")
  cat("Annualized Returns (%):\n")
  print(ann_return)
  cat("Annualized Volatility (%):\n")
  print(ann_vol)
  cat("Sharpe Ratio:\n")
  print(sharpe)
  cat("-------------------------------------------------------\n")
  
  # ----------------------------
  # 5. VISUALIZATION
  # ----------------------------
  # Your exact plotting style
  cum_ret <- array(1, c(nrow(ret_month)+1))
  for(i in 2:(nrow(ret_month)+1)) {
    cum_ret[i] <- cum_ret[i-1] * (mean(ret_month[i-1,]) + 1)
  }
  
  plot(ts(cum_ret, start=start_date, frequency=12),
       main=paste("Cumulative Returns -", granularity, "Industries"),
       ylab="Cumulative Return", col="blue", lwd=2)
}
