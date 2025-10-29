n<-100
a<-10000

n1 <- 50
mean1 <- 22
std1 <- 16 

n2 <- 50
mean2 <- 25
std2 <- 15

alpha <- 0.05

cat(paste0("Store A: n=", n1, ", Mean=", mean1, " min, Std Dev=", std1, " min\n"))
cat(paste0("Store B: n=", n2, ", Mean=", mean2, " min, Std Dev=", std2, " min\n"))

numerator <- (mean1 - mean2)
denominatorsquared <- (std1^2 / n1) + (std2^2 / n2)
denominator <- sqrt(denominatorsquared)

tstatistic <- numerator / denominator


dfnumerator <- (std1^2 / n1 + std2^2 / n2)^2
dfdenominatorterm1 <- ((std1^2 / n1)^2) / (n1 - 1)
dfdenominatorterm2 <- ((std2^2 / n2)^2) / (n2 - 1)
df <- dfnumerator / (dfdenominatorterm1 + dfdenominatorterm2)

# Calculate p-value (two-tailed)
pvalue <- 2 * pt(abs(tstatistic), df = df, lower.tail = FALSE)

cat(paste0("Calculated t-statistic: ", round(tstatistic, 3), "\n"))
cat(paste0("Calculated degrees of freedom: ", round(df, 2), "\n"))
cat(paste0("Calculated pvalue: ", round(pvalue, 3), "\n\n"))


if (pvalue < alpha) {
  cat("we reject the null hypothesis.\n")
  cat("There is statistically significant evidence to conclude that the average service times in Store A and Store B are different.\n")
  if (mean1 < mean2) {
    cat(paste0("Specifically, Store A (average ", mean1, " min) appears to be more efficient than Store B (average ", mean2, " min).\n"))
  } else {
    cat(paste0("Specifically, Store B (average ", mean2, " min) appears to be more efficient than Store A (average ", mean1, " min).\n"))
  }
} else {
  cat("we fail to reject the null hypothesis.\n")
  cat("There is not enough statistical evidence to conclude that the average service times in Store A and Store B are different.\n")

}
