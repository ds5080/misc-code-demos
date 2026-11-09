# simulate some data for lung cancer -------------------------------------

# Always set a seed
set.seed(2026 - 11 - 09)

# Sample size
n <- 2000

# Causal SNP coded 0/1/2 (copies of the minor allele), MAF 0.3
snp_causal <- rbinom(n, size = 2, prob = 0.3)

# Null SNP coded the same way
snp_null <- rbinom(n, size = 2, prob = 0.3)
table(snp_null)

# Pack-years: 40% never smokers, the rest right-skewed
pack_years <- ifelse(
  test = runif(n) < 0.4,
  yes = 0,
  no = round(rgamma(n, shape = 2, scale = 12), 1)
)
hist(pack_years)

# Coffee: a covariate with no effect on lung function
coffee_cups <- rpois(n, lambda = 2)
table(coffee_cups)

# Outcome: FEV1 in liters (air forcibly exhaled in one second).
# Only snp_causal and pack_years have an effect.
# Each minor allele lowers FEV1 by 0.1 L; each pack-year lowers it by 0.015 L.
fev1 <- 4 - 0.1 * snp_causal - 0.015 * pack_years + rnorm(n, mean = 0, sd = 0.5)
fev1 <- round(fev1, 2)
hist(fev1)

# Put it all into a data frame
dat <- data.frame(
  fev1,
  snp_causal,
  snp_null,
  pack_years,
  coffee_cups
)
head(dat)

# Visualization ----------------------------------------------------------

ggplot(dat, aes(x = pack_years, y = fev1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(x = "Pack-years", y = "FEV1 (L)")

ggplot(dat, aes(x = coffee_cups, y = fev1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(x = "Coffee cups", y = "FEV1 (L)")

ggplot(dat, aes(x = factor(snp_causal), y = fev1)) +
  geom_boxplot(alpha = 0.5, aes(fill = factor(snp_causal))) +
  labs(x = "snp_causal genotype", y = "FEV1 (L)", fill = "Genotype")

ggplot(dat, aes(x = factor(snp_null), y = fev1)) +
  geom_boxplot(alpha = 0.5, aes(fill = factor(snp_null))) +
  labs(x = "snp_null genotype", y = "FEV1 (L)", fill = "Genotype")


# Statistical analysis with linear regression ----------------------------

# Note the coefficient p-values, and overall model fit p-value
summary(lm(fev1 ~ snp_null, data = dat))
summary(lm(fev1 ~ snp_null + pack_years, data = dat))
summary(lm(fev1 ~ snp_causal + snp_null + pack_years + coffee_cups, data = dat))
