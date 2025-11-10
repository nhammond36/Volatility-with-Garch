# =========================================================
# Ornstein–Uhlenbeck Estimation for Daily Short Rates
# =========================================================
# Required: data.csv with columns: date, EFFR, OBFR, TGCR, BGSR, SOFR
# =========================================================

library(ggplot2)
library(dplyr)
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

# Tp run whole code:
# source("C:/Users/naham/OneDrive/Documents/CBOE2025/your_script_name.R")
# else line(s) by linw

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

## Chat
# Replace zeros with NA, then remove NA rows
#returns_num[returns_num == 0] <- NA
#returns_num <- na.omit(returns_num)

# Align cleaned dates with the remaining rows
dates_clean <- dates[(length(dates) - nrow(returns_num) + 1):length(dates)]


# Create a cleaned dataframe of rates
rates <- data.frame(date = dates_clean, returns_num) %>%
  select(date, EFFR, OBFR, TGCR, BGCR, SOFR) %>%
  na.omit()

  # Vasicek estimation function (daily data)
  estimate_vasicek <- function(rates, delta = 1/252) {
    r_t <- head(rates, -1)
    r_tp1 <- tail(rates, -1)

    fit <- lm(r_tp1 ~ r_t)
    phi <- coef(fit)[2]
    c <- coef(fit)[1]
    s <- sd(resid(fit))

    kappa <- -log(phi) / delta
    theta <- c / (1 - phi)
    sigma <- s * sqrt(2 * kappa / (1 - phi^2))

    list(kappa = kappa, theta = theta, sigma = sigma, phi = phi, model = summary(fit))
  }

  # Example: using daily EFFR
  # ---- 3. Example: estimate on one series ----
  # Suppose you have daily EFFR data
  effr <- rates$EFFR  # or your chosen rate series
  effr <- na.omit(effr)


  #Step 1: You define ou_loglik().

  #Step 2: You define estimate_vasicek() — which uses ou_loglik() inside optim().

  #Step 3: You run estimate_vasicek() on your rate data (e.g., EFFR).
# ---- 2. Define OU log-likelihood and estimation function
  # NEW Oct 22
  # ---- 1. Define OU log-likelihood ----
  ou_loglik <- function(params, x, dt) {
    kappa <- exp(params[1])   # enforce positivity
    theta <- params[2]
    sigma <- exp(params[3])   # enforce positivity
    n <- length(x)
    ll <- 0
    for (i in 1:(n-1)) {
      expm <- exp(-kappa * dt)
      m <- x[i] * expm + theta * (1 - expm)
      v <- (sigma^2) / (2 * kappa) * (1 - exp(-2 * kappa * dt))
      resid2 <- (x[i+1] - m)^2
      ll <- ll - 0.5 * (log(2 * pi * v) + resid2 / v)
    }
    return(-ll)  # negative log-likelihood (for minimization)
  }

  # ---- 2. Define Vasicek estimation function ----
  estimate_vasicek <- function(x, dt = 1/252) {
    start_vals <- c(log(0.5), mean(x, na.rm = TRUE), log(sd(x, na.rm = TRUE)))

    fit <- optim(par = start_vals,
                 fn = ou_loglik,
                 x = x,
                 dt = dt,
                 method = "BFGS",
                 control = list(maxit = 1000))

    kappa <- exp(fit$par[1])
    theta <- fit$par[2]
    sigma <- exp(fit$par[3])

    list(kappa = kappa,
         theta = theta,
         sigma = sigma,
         logLik = -fit$value,
         convergence = fit$convergence)
  }

  # Assume you have dataframe df with columns: date, EFFR, OBFR, TGCR, BGSR, SOFR
  vasicek_effr <- estimate_vasicek(rates$EFFR)
  print(vasicek_effr)


  #fit_effr <- estimate_vasicek(effr, dt = 1/252)
  print(fit_effr)


  # Example: using daily EFFR
  # Assume you have dataframe df with columns: date, EFFR, OBFR, TGCR, BGSR, SOFR
  vasicek_effr <- estimate_vasicek(rates$EFFR)
  vasicek_effr


  phi <- vasicek_effr$phi
  c <- coef(vasicek_effr$model)[1]
  fitted <- c + phi * head(df$EFFR, -1)

  plot(rates$date[-1], tail(rates$EFFR, -1), type='l', col='black', lwd=2,
       ylab='EFFR', main='Vasicek Model Fit')
  lines(rates$date[-1], fitted, col='red', lwd=2)
  legend('topright', legend=c('Actual', 'Fitted'), col=c('black','red'), lwd=2)

estimate_vasicek <- function(x, dt = 1/252) {
  start <- c(log(0.3), mean(x), log(sd(diff(x))))
  fit <- optim(start, ou_loglik, x = x, dt = dt, method = "BFGS", hessian = TRUE)
  par <- fit$par
  cov <- try(solve(fit$hessian), silent = TRUE)
  se <- if (!inherits(cov, "try-error")) sqrt(diag(cov)) else rep(NA, 3)
  list(
    kappa = exp(par[1]), theta = par[2], sigma = exp(par[3]),
    se_kappa = exp(par[1]) * se[1], se_theta = se[2], se_sigma = exp(par[3]) * se[3],
    logLik = -fit$value, convergence = fit$convergence
  )
}

# ---- 3. Run estimation for each rate ----
rate_names <- c("EFFR", "OBFR", "TGCR", "BGSR", "SOFR")
dt <- 1/252  # daily trading-year step

results <- lapply(rate_names, function(name) {
  x <- rates[[name]]
  fit <- estimate_OU(x, dt)
  data.frame(
    Rate = name,
    kappa = fit$kappa, se_kappa = fit$se_kappa,
    theta = fit$theta, se_theta = fit$se_theta,
    sigma = fit$sigma, se_sigma = fit$se_sigma,
    logLik = fit$logLik,
    convergence = fit$convergence
  )
})
ou_results <- do.call(rbind, results)
#ou_results_2025-10-20.csv)

print("===== OU Parameter Estimates =====")
print(ou_results)

#C:\Users\naham\OneDrive\Documents\StochasticCalculus
#write.csv(ou_results, "C:/Users/naham/OneDrive/Documents/CBOE2025/results/ou_results.csv", row.names = FALSE)

write.csv(ou_results, "C:/Users/naham/OneDrive/Documents/StochasticCalculus/ou_results.csv", row.names = FALSE)
# load(C:/Users/naham/OneDrive/Documents/StochasticCalculus/ou_results.csv("ou_results.RData")




# ---- 4. Plot one example: EFFR actual vs fitted mean path ----
phi <- vasicek_effr$phi
c <- coef(vasicek_effr$model)[1]
fitted <- c + phi * head(df$EFFR, -1)

plot(df$date[-1], tail(df$EFFR, -1), type='l', col='black', lwd=2,
     ylab='EFFR', main='Vasicek Model Fit')
lines(df$date[-1], fitted, col='red', lwd=2)
legend('topright', legend=c('Actual', 'Fitted'), col=c('black','red'), lwd=2)

