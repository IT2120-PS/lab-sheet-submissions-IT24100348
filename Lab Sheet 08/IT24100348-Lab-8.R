setwd("C:\\Users\\it24100348\\Desktop\\IT24100348 ")

# Replace the below vector with your actual laptop bag weights
laptop_bag_weights <- c(2.1, 2.3, 2.5, 2.2, 2.4, 2.6, 2.8, 2.7, 2.9, 3.0)

# 1. Calculate population mean and population standard deviation
pop_mean <- mean(laptop_bag_weights)
pop_sd <- sd(laptop_bag_weights)

cat("Population Mean:", pop_mean, "\n")
cat("Population Standard Deviation:", pop_sd, "\n")

# 2. Draw 25 random samples of size 6 (with replacement)
set.seed(123)  # for reproducibility

sample_means <- numeric(25)
sample_sds <- numeric(25)

for (i in 1:25) {
  sample_i <- sample(laptop_bag_weights, size = 6, replace = TRUE)
  sample_means[i] <- mean(sample_i)
  sample_sds[i] <- sd(sample_i)
  cat("Sample", i, "Mean:", sample_means[i], "SD:", sample_sds[i], "\n")
}

# 3. Calculate mean and standard deviation of the 25 sample means
mean_of_sample_means <- mean(sample_means)
sd_of_sample_means <- sd(sample_means)

cat("Mean of Sample Means:", mean_of_sample_means, "\n")
cat("Standard Deviation of Sample Means:", sd_of_sample_means, "\n")

# Relationship statement
cat("\nRelationship:\n")
cat("The mean of the sample means is close to the population mean.\n")
cat("The standard deviation of the sample means (standard error) is less than the population standard deviation.\n")
