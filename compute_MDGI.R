# =====================================================================
# Function: compute_MDGI
# Author: Cinzia Di Nuzzo
# Reference: Di Nuzzo, Tomaselli & Torrisi (2025) 
#            "Assessing multilevel spatial inequality in EU regions:
#            a multidimensional approach"
# Description:
#   This function computes the Multidimensional Gini Inequality Index 
#   (both the global MDGI and yearly 3wMDGI) based on three-way data 
#   arrays (Regions x Variables x Time / Objects x Variables x Occasion)
#   using a rank-1 Candecomp/Parafacm tensor decomposition.
#
# Inputs:
#   G3_3W  : 3D array [Regions x Variables x Time]
#   delta  : parameter (δ). Higher δ emphasizes bottom 
#            units in inequality evaluation. Recommended: δ ∈ [1, 2].
#   beta   : parameter (β). Controls substitutability between 
#            well-being dimensions. Recommended: 0 < β < 1.
#
# Output:
#   A list containing:
#     - MDGI_Global : Global multidimensional Gini index
#     - MDGI_Yearly : Vector of yearly 3wMDGI values
#     - Weights_Var : Variable weights from CP decomposition
#     - Weights_Time: Time (occasion) weights from CP decomposition
#     - Scores      : Normalized well-being scores per region
#     - GiniScores  : Inequality contributions by region and year
# =====================================================================

compute_MDGI <- function(G3_3W, delta = 2, beta = 0.005) {
  
  # Load required library
  if (!requireNamespace("multiway", quietly = TRUE)) {
    stop("Please install the 'multiway' package.")
  }
  library(multiway)
  
  # Dimensions: Regions (n), Variables (m), Time (H)
  n <- dim(G3_3W)[1]
  m <- dim(G3_3W)[2]
  H <- dim(G3_3W)[3]
  
  # ------------------------------
  # Step 1: CP decomposition (rank-1 PARAFAC)
  # ------------------------------
  pfac <- parafac(G3_3W, nfac = 1, nstart = 100, const = c(2, 2, 2))
  Chat <- pfac$C   # Time weights
  Ahat <- pfac$A   # Region scores
  Bhat <- pfac$B   # Variable weights
  
  # Normalize variable weights across occasions
  X_3w <- array(0, dim = c(m, H))
  for (h in 1:H) {
    X_3w[, h] <- Bhat * Chat[h]
  }
  
  xn <- apply(X_3w, 2, function(x) x / sum(x))
  
  # ------------------------------
  # Step 2: Compute yearly MDGI
  # ------------------------------
  sum_w   <- array(0, dim = c(n, H))
  mucol   <- array(0, dim = c(m, H))
  weig_mu <- rep(0, H)
  G       <- rep(0, H)
  pesi    <- array(0, dim = c(n, H))
  
  for (h in 1:H) {
    # Regional well-being scores per year
    for (i in 1:n) {
      sum_w[i, h] <- (sum(xn[, h] * (G3_3W[i, , h])^beta))^(1/beta)
    }
    
    # Ranking of regions
    wind <- sort(sum_w[, h], decreasing = TRUE, index.return = TRUE)$ix
    
    # Mean of each variable across regions
    mucol[, h] <- apply(G3_3W[, , h], 2, mean)
    
    # Normalized average well-being score
    weig_mu[h] <- (sum(xn[, h] * (mucol[, h])^beta))^(1/beta)
    
    # Yearly 3wMDGI (Equation (7) in the paper)
    G[h] <- 1 - (sum(((wind/n)^delta - ((wind-1)/n)^delta) * sum_w[, h]) / weig_mu[h])
    
    # Regional weights (ηih in the paper)
    pesi[, h] <- sum_w[, h] / weig_mu[h]
  }
  
  # ------------------------------
  # Step 3: Global MDGI (Equation (8) in the paper)
  # ------------------------------
  Ac <- array(0, dim = c(n, m))
  for (i in 1:n) {
    Ac[i, ] <- G3_3W[i, , ] %*% Chat
  }
  
  xn_global <- Bhat / sum(Bhat)
  mucol_global <- colMeans(Ac)
  weig_mu_global <- (sum(xn_global * (mucol_global)^beta))^(1/beta)
  
  sum_w_global <- rep(0, n)
  for (i in 1:n) {
    sum_w_global[i] <- (sum(xn_global * (Ac[i, ])^beta))^(1/beta)
  }
  
  wind_global <- sort(sum_w_global, decreasing = TRUE, index.return = TRUE)$ix
  MDGI_Global <- 1 - (sum(((wind_global/n)^delta - ((wind_global-1)/n)^delta) * sum_w_global) / weig_mu_global)
  
  # ------------------------------
  # Return results
  # ------------------------------
  return(list(
    MDGI_Global = MDGI_Global,
    MDGI_Yearly = G,
    Weights_Var = Bhat,
    Weights_Time = Chat,
    Scores = sum_w,
    GiniScores = pesi
  ))
}


