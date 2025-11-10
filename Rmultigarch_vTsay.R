# ==============================
# Rmultigarch_v16.R
# Fully explicit output paths with confirmation
# ==============================

library(rmgarch)
library(xtable)
library(MTS)
library(tsDyn)
library(FinTS)
library(broom)
library(dplyr)
library(knitr)
library(rugarch)
library(rugarch)
library(TSA)
library(nonlinearTseries)

# ------------------------------
# 1. Load Data
# ------------------------------
data_path <- "C:/Users/naham/OneDrive/Documents/GARCHfolder/spread.csv"
spread <- read.csv(data_path, stringsAsFactors = FALSE)

# First 5 series (adjust columns if needed)
returns <- spread[, 2:6]        # assuming column 1 is Date
dates <- as.Date(spread[, 1])   # first column is Date

# Convert to numeric matrix
returns_num <- as.matrix(returns)

# ------------------------------
# 2. Clean Data
# ------------------------------
returns_num[returns_num == 0] <- NA
returns_num <- na.omit(returns_num)
dates_clean <- dates[(length(dates) - nrow(returns_num) + 1):length(dates)]

# ------------------------------
# 3. Tsay MTS GARCH Specs
# ------------------------------

# MCHdiag Multivariate Conditional Heteroscedastic Model Checking
# Description
# Apply four portmanteau test statistics to check the validity of a fitted multivariate volatility model
# Usage
# MCHdiag(at, Sigma.t, m = 10)
# Arguments
# at A T-by-k matrix of residuals for a k-dimensional asset return series
# Sigma.t The fitted volatility matrices. The dimension is T-by-k^2 matrix
# m The number of lags used in the tests. Default is 10.
# Details
# The four test statistics are given in Tsay (2014, Chapter 7)
# Value
# Four test statistics and their p-values
# Author(s)
# Ruey S. Tsay
# References
# Tsay (2014, Chapter 7). Multivariate Time Series Analysis with R and Financial Applications. John
# Wiley. Hoboken, NJ.

#Tsay Multivariate Time Series Analysis with R and Financial Applications
#Remark: The volatility test for multivariate time series is carried out by the command MarchTest of the MTS package. The default option uses 10 lags.
#R Demonstration: Multivariate volatility test.
# zt=matrix(rnorm(2000),400,5)
#  MarchTest(zt)
# Q(m) of squared scalar series(LM test):
#   Test statistic: 13.2897 p-value: 0.2079223
# Rank-based Test:
#   Test statistic: 6.753778 p-value: 0.7484673
# Q_k(m) of squared series:
#   Test statistic: 280.1069 p-value: 0.09251779
# Robust Q_k(m) statistic (5% trimming):
#   Test statistics: 261.0443 p-value: 0.3027401
# > da=read.table("m-ibmsp-6111.txt",header=T)
# > rtn=log(da[,2:3]+1)
# > at=scale(rtn,center=T,scale=F) ## remove sample means
# > MarchTest(at)
# Q(m) of squared scalar series(LM test):
#   Test statistic: 38.06663 p-value: 3.695138e-05
# Rank-based Test:
#   Test statistic: 108.3798 p-value: 0
# Q_k(m) of squared series:
#   Test statistic: 109.4194 p-value: 2.276873e-08
# Robust Q_k(m) statistic (5% trimming):
#   Test statistics: 118.7134 p-value: 9.894441e-10


# Tsay test for nonlinearity
# tsay.test(z_t)

# --- Fit GARCH model ---
r=returns_num

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec, data = r)

# --- Residuals ---
z_t <- residuals(fit, standardize = TRUE)

# --- Run tests ---
#library(TSA)
res_tsay1 <- Tsay.test(z_t, order = 5)  # pick an AR order
print(res_tsay1)
# If you omit order, it will try to choose via AIC:
res_tsay2 <- Tsay.test(z_t)

#library(nonlinearTseries)
#res_tsay3 <- tsayTest(z_t, order = 5)
#print(res_tsay3)
#res_tsay3 <- tsayTest(z_t, order = 5)
#print(res_tsay3)


#tsay_res   <- tsayTest(z_t)
tsay_res <- tsayTest(z_t, order = 5)
print(tsay_res)

arch_res   <- ArchTest(z_t, lags = 12)
mcleod_res <- McLeod.Li.test(y = z_t, lags = 12)

# --- Collect results ---
results <- data.frame(
  Test        = c("Tsay Test", "ARCH LM Test", "McLeod-Li Test"),
  Statistic   = c(tsay_res$statistic, arch_res$statistic, mcleod_res$statistic),
  P_Value     = c(tsay_res$p.value, arch_res$p.value, mcleod_res$p.value)
)


# Tests chatper 7 Tsay 7,1
# 1. portmanteau test Q*_k(m)
# null no heteroskedascity
# Distrbuted chiquared(k^2,m)
# asymptotically equivalent generalization of the multivariate Lagrange Multiplier (LM) 
# test of Engle (1982) for multivariate d(k62,)heteroskedacity 
# mail fail in presence of heavy tails
# Table 7.1 Finte sample performancs of Q
#   T ave var q_90  q_95  q_99

# 2. Rank based test
# to overcome heavy tailedness
# Q_R(m)
# distributed Chisquared(m) asymptotically if e_t has not serial correlation 

# Create xtable object
results_xtable <- xtable(results,
                         caption = "Diagnostic Tests on Standardized Residuals",
                         label   = "tab:diagnostics",
                         digits  = c(0, 0, 3, 3))  # round stats/p-values to 3 decimals

# Print LaTeX table
print(results_xtable,
      include.rownames = FALSE,  # hide row numbers
      booktabs = TRUE)           # nice formatting

##

# #2. Create LaTeX-ready table using knitr:
# kable(results, format = "latex", booktabs = TRUE,
#       caption = "Diagnostic Tests on Standardized Residuals")
# 
# # Or with xtable:
# print(xtable(results, 
#              caption = "Diagnostic Tests on Standardized Residuals",
#              digits = c(0,0,3,3)),  # rounding
#       include.rownames = FALSE,
#       booktabs = TRUE)

# 3. Example Output (LaTeX)
# If you knit to PDF, you’ll get something like:
#   
#   latex
# Copy code
# \begin{table}[ht]
# \centering
# \caption{Diagnostic Tests on Standardized Residuals}
# \begin{tabular}{lcc}
# \toprule
# Test & Statistic & P-Value \\
# \midrule
# Tsay Test      & 12.345 & 0.015 \\
# ARCH LM Test   &  5.678 & 0.060 \\
# McLeod-Li Test &  8.901 & 0.032 \\
# \bottomrule
# \end{tabular}
# \end{table}

# ------------------------------
# 3. Univariate GARCH Specs
# ------------------------------

# 3. Univariate GARCH Specs
# ------------------------------
uspec <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  mean.model     = list(armaOrder=c(0,0), include.mean=FALSE),
  distribution.model="norm"
)

mspec <- multispec(replicate(ncol(returns_num), uspec))

# ------------------------------
# 4. DCC Specification
# ------------------------------
dcc_spec <- dccspec(uspec = mspec, dccOrder = c(1,1), distribution = "mvnorm")
dcc_fit <- dccfit(dcc_spec, data = returns_num)

# ------------------------------
# 5. Extract Coefficients
# ------------------------------
matcoef <- dcc_fit@mfit$matcoef
cols <- colnames(matcoef)

dcc_coef_df <- data.frame(
  Parameter = rownames(matcoef),
  Estimate  = round(matcoef[, cols[1]], 4),
  StdError  = round(matcoef[, cols[2]], 4),
  tValue    = round(matcoef[, cols[3]], 4),
  pValue    = signif(matcoef[, cols[4]], 4)
)

#
# source("Rmultigarch_vTsay.R", echo = TRUE)
