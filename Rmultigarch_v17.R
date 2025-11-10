# ==============================
# Rmultigarch_v16.R
# Fully explicit output paths with confirmation
# ==============================

library(rmgarch)
library(xtable)

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
