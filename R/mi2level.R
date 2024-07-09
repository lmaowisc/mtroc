
## function to estimate model parameters
two_level_mod_fit <- function(dfcc){

  logit_pi <- dfcc$logit_pi
  a0 <- mean(logit_pi)
  v_pi <- var(logit_pi)

  mc <- nrow(dfcc)
  obj_lm <- lm(logit_q ~ logit_p, dfcc)

  beta <- coef(obj_lm)
  V_beta <- vcov(obj_lm) * mc

  return(list(a0 = a0, v_pi = v_pi,
              beta = beta, V_beta = V_beta))
}


## function to impute missing FP/TN (non-cases)
fp_tn_impute <- function(dfmc, mod2_param){


  ## size of incomplete data
  mc <- nrow(dfmc)

  ## get estimated parameters for
  ## imputation model
  a0 <- mod2_param$a0
  v_pi <- mod2_param$v_pi
  beta <- mod2_param$beta
  V_beta <- mod2_param$V_beta

  ## draw random observations
  epsi <- rnorm(n = mc, mean = 0, sd = sqrt(v_pi))
  bi <-  mvrnorm(
    n = mc,
    mu = c(0, 0),
    Sigma = V_beta
  )
  ## impute values of Nb and q
  dfm <- dfmc |>
    mutate(
      pi = expit(a0 + epsi),
      q = expit(beta[1] + bi[, 1] + (beta[2] + bi[, 2]) * logit_p),
      Nb = N * (1 - pi) / pi,
      FP = Nb * (1 - q),
      TN = Nb * q
    )

  dfm |>
    dplyr::select(TP, FN, FP, TN) |>
    drop_na()

}

