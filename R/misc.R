## continuity correction
correct_zero <- function(x, ep = 0.5){
  # x[x == 0] <- ep
  x <- x + ep
  x
}
## logit/expit function
logit <- function(x){
  log(x / (1 - x))
}
expit <- function(x){
  exp(x) / (1 + exp(x))
}
