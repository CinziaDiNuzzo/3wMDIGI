# 3wMDIGI
Three-way Multidimensional Gini Index (3wMDGI) - R Function - Assessing multilevel spatial inequality in EU regions:  a multidimensional approach
# Three-way Multidimensional Gini Index (3wMDGI) - R Function

## Description
R implementation of the Three-way Multidimensional Gini Index (3wMDGI) 
as proposed by Di Nuzzo, Tomaselli & Torrisi (2025), "Assessing multilevel spatial inequality in EU regions: 
a multidimensional approach". Spatial Economic Analysis.

The function `compute_MDGI()` allows users to compute both:
- The Global MDGI: a single measure of multidimensional inequality across regions, variables, and years.
- The Yearly MDGI: a time-resolved measure of multidimensional inequality per year/occasion.

The method is based on a rank-1 Candecomp/Parafac tensor decomposition, 
which extracts optimal weights for variables and time periods from the data itself.


## Input
- `G3_3W`: A three-way data array of size [Regions x Variables x Time].
- `delta` (δ): Sensitivity parameter (default = 2).  
   Recommended range: 1 ≤ δ ≤ 2.
- `beta` (β): Parameter (default = 0.005).  
   Must satisfy β < 1.



## Output
The function returns a list with:
- `MDGI_Global`: The overall multidimensional inequality index (Equation (8) in the paper).
- `MDGI_Yearly`: A vector of yearly MDGI values (Equation (7)).
- `Weights_Var`: Weights assigned to variables/dimensions by the CP model.
- `Weights_Time`: Weights assigned to years/occasions by the CP model.
- `Scores`: Well-being scores per region and year.
- `GiniScores`: Normalized inequality contributions per region.



## Interpretation
- Weights_Var (Bhat): Show the relative contribution of each dimension 
  (e.g., GDP, education, life expectancy, employment) to overall inequality.
- Weights_Time (Chat): Indicate which years/occasions have more influence in shaping disparities.
- δ (delta): Governs how much the bottom of the distribution (worst-off regions) is emphasized.
- β (beta): Controls the substitutability across dimensions; low β means dimensions are less substitutable.



## Reference
Di Nuzzo, C., Tomaselli, V., & Torrisi, G. (2025).  
Assessing multilevel spatial inequality in EU regions: a multidimensional approach.  
Spatial Economic Analysis



## Example Usage

```r

# ================================================================
# Example: Simulated 3-way dataset [Regions x Variables x Time]
# ================================================================

# Define dimensions
n_regions <- 10
n_vars <- 3
n_years <- 4

# Names for clarity
region_names <- paste0("Country_", 1:n_regions)
var_names    <- paste0("Var_", 1:n_vars)
year_names   <- paste0("Year_", 1:n_years)

# Create synthetic disparities:
# - Regions 1-3: high values (wealthy)
# - Regions 4-7: medium values
# - Regions 8-10: low values (poor)


set.seed(123) # reproducibility

G3_3W <- array(0, dim = c(n_regions, n_vars, n_years),
               dimnames = list(region_names, var_names, year_names))

for (h in 1:n_years) {
  # Increasing trend over time
  growth_factor <- 1 + (h-1) * 0.1
  
  for (i in 1:n_regions) {
    if (i <= 3) {
      # Rich regions
      G3_3W[i, , h] <- rnorm(n_vars, mean = 80, sd = 5) * growth_factor
    } else if (i <= 7) {
      # Middle regions
      G3_3W[i, , h] <- rnorm(n_vars, mean = 50, sd = 5) * growth_factor
    } else {
      # Poor regions
      G3_3W[i, , h] <- rnorm(n_vars, mean = 20, sd = 5) * growth_factor
    }
  }
}

# Quick check
G3_3W[,,1]  # values at Year_1
G3_3W[,,4]  # values at Year_4

# ================================================================
# Run the MDGI function (assumes compute_MDGI() is already loaded)
# ================================================================
result <- compute_MDGI(G3_3W, delta = 2, beta = 0.005)

# Print results
cat("Global Multidimensional Gini Index:\n")
print(result$MDGI_Global)

cat("\nYearly MDGI values:\n")
print(result$MDGI_Yearly)

# Plot yearly inequality trend
plot(result$MDGI_Yearly, type = "b", pch = 19, col = "blue",
     xaxt = "n", xlab = "Year", ylab = "3wMDGI",
     main = "Yearly Multidimensional Gini Index (Simulated Data)")
axis(1, at = 1:n_years, labels = year_names)


# ================================================================
# Additional Plots: Variable Weights, Time Weights, and Region Scores
# ================================================================

# 1. Plot variable weights (Bhat)
plot(result$Weights_Var, pch = 19,
        main = "Variable Weights (Bhat)",
        ylab = "Weight", col = "lightblue")

# 2. Plot time weights (Chat)
plot(result$Weights_Time,  pch = 19,
        main = "Time Weights (Chat)",
        ylab = "Weight", col = "lightgreen")

# 3. Plot region scores (ηih / contributions)
# For readability, let’s use the last year (Year_4) as example
region_scores <- result$GiniScores[,4]

plot(region_scores,las=2, pch = 19,
        main = "Regional Contributions (Year 4)",
        ylab = "Score", col = "salmon")




