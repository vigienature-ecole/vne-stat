#' @title Margin error
#'
#' @param x a numeric vector
margin_error <- function(x, alpha = 0.05) {
  sample.mean <- mean(x)
  sample.n <- length(x)
  sample.sd <- sd(x)
  sample.se <- sample.sd/sqrt(sample.n)
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  margin.error <- t.score * sample.se
  return(margin.error)
}
