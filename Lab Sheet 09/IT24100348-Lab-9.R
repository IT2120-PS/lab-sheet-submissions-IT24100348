 
setwd("C:\\Users\\k_mur\\Desktop\\IT24100348 ")
# Set seed for reproducibility (optional)
set.seed(123)

# i) Generate random sample of size 25 from N(mean=45, sd=2)
sample_size <- 25
true_mean <- 45
true_sd <- 2
bake_times <- rnorm(sample_size, mean=true_mean, sd=true_sd)

# Show the sample
print("Sample of baking times (minutes):")
print(round(bake_times, 4))

# Summary statistics
sample_mean <- mean(bake_times)
sample_sd <- sd(bake_times)       # sample standard deviation (n-1)
cat("\nSample mean:", round(sample_mean,4), "\n")
cat("Sample sd:", round(sample_sd,4), "\n")
cat("n =", sample_size, "\n\n")

# ii) Test H0: mu = 46  vs  Ha: mu < 46 at alpha = 0.05 (one-sided t-test)
test_result <- t.test(bake_times, mu = 46, alternative = "less")
print(test_result)

# Manual calculation of t-statistic and one-sided p-value (for clarity)
t_stat <- (sample_mean - 46) / (sample_sd / sqrt(sample_size))
df <- sample_size - 1
cat("\nManual t-statistic:", round(t_stat,6), "  df =", df, "\n")
# one-sided p-value
p_one_sided <- pt(t_stat, df = df)
cat("One-sided p-value (P(T <= t)):", round(p_one_sided,6), "\n")

# Decision at alpha = 0.05
alpha <- 0.05
if (p_one_sided < alpha) {
  cat("\nConclusion: Reject H0. Evidence suggests average baking time is less than 46 minutes (at 5% level).\n")
} else {
  cat("\nConclusion: Fail to reject H0. Not enough evidence that average baking time is less than 46 minutes (at 5% level).\n")
}

