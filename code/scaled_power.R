rm(list = ls())

global_scale <- 10

scaled_power <- function(x, 
                         power = 2, 
                         method = c("simple", "precise"),
                         scale = 10,
                         plot = TRUE,
                         ...) {
  if (!is.numeric(x)) {
    stop("Input 'x' must be numeric.", call. = FALSE)
  }
  
  method = match.arg(method)
   # Local variables are isolated within the function environment
  pow_res <- x^power
  
  # If 'global_scale' is not defined locally, R searches the parent/global environment
  final_res <- pow_res * scale
  
  if (method == "simple") {
    final_res = round(final_res)
  }
  if (plot) {
    par(mfrow = c(1, 2))
    plot(x, final_res, xlab = "Input (x)", ...)
    plot(x, final_res, main = "Another Plot", ...)
    
  }
  
  return(final_res)
}

scaled_power(x = c(1, 2, 3))
scaled_power(c(1, 2, 3), power = 0.5, method = "p", scale = 5)
scaled_power(c(1, 2, 3), power = 0.5, method = "s", scale = 5)
scaled_power(c(1, 2, 3), power = 3, scale = 5, pch = 18, cex = 2)
scaled_power(c(1, 2, 3), power = 3, scale = 5, pch = 18, cex = 2, xlab = "My x")
res <- scaled_power(
  x = seq(-3, 3, by = 0.5), 
  power = 2, 
  scale = 5, 
  method = "precise",
  col = "firebrick", 
  pch = 19, 
  type = "b"
)
res