set.seed(2026 - 11 - 09)

p <-
  replicate(1e5, {
    x <- rnorm(100, mean = 0, sd = 1)
    y <- rnorm(100, mean = 0, sd = 1)
    t.test(x, y)$p.value
  })

sum(p < 0.10) / length(p)
sum(p < 0.05) / length(p)
sum(p < 0.01) / length(p)

hist(p, 100)

qqman::qq(p)
