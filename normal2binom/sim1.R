library(tidyverse)

# Simulation approach:

# Step 1: Correlated normal
# To reduce noise, make sure that they are N(0,1) and fixed correlation (r)
# Step 1a: Simulate X1, X2 from N(0,1)
# Step 1b: Center so both have mean 0
# Step 1c: Orthogonalize X2 to X1
# Step 1d: Scale so both have variance 1
# Step 1c: Create X2r for different correlations

# Step 2: Convert to Binomial(2,p)
# Convert X1 and X2r to G1p and G2rp for different p

# Step 3: Correlate
# Calculate the correlation of G1p and G2rp

simulate <- function (
  n = 100,
  r = seq(0.1, 0.9, 0.1),
  p = seq(0.1, 0.5, 0.1)
) {
  X <- rnorm(2*n) %>% matrix(ncol=2) %>% scale
  X <- X %>% var %>% chol %>% solve %>% tcrossprod(X) %>% t

  X2r <- X %*% rbind(r, sqrt(1-r^2))
  colnames(X2r) <- r

  G1p <- sapply(p, qbinom, p = X[,1] %>% pnorm, size = 2)
  G2rp <- sapply(p, qbinom, p = X2r %>% pnorm, size = 2, simplify = "array")

  cbind(
    n = n,
    expand.grid(
      p2 = p,
      p1 = p
    ),
    apply(G2rp, 2, cor, G1p)
  ) %>%
    gather(r, cor, -n, -p1, -p2) %>%
    mutate(r = as.numeric(r))
}
