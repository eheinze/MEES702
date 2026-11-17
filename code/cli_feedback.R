library(cli)

# Base R validation reference
is_leap_year_cli <- function(year) {
  if (!is.numeric(year)) {
    cli::cli_abort(c("x" = "Argument {.arg year} must be a numeric vector.",
                     "i" = "You supplied an object of class {.cls {class(year)}}"),
                   call. = FALSE)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

is_leap_year("2024")

scaled_power_cli <- function(x, 
                             power = 2, 
                             scale = 10, 
                             method = c("simple", "precise"), 
                             plot = TRUE, verbose = TRUE, ...) {
  if (!is.numeric(x)) {
    cli::cli_abort(c("x" = "{.arg x} must be a numeric vector.",
                     "i" = "You supplied an object of class {.cls {class(x)}}"), 
                   call. = FALSE)
  }
  if (!is.numeric(power)) {
    cli:cli_abort(c("x" = "{.arg power} must be a numeric vector.",
                    "i" = "You supplied an object of class {.cls {class(power)}}"),
                  call. = FALSE)
  }
  if (!is.logical(plot)) {
    cli::cli_abort(c("x" = "{.arg plot} must be logical.",
                     "i" = "You supplied incorrect parameters for the plot"))
  }
  if (!is.logical(verbose)) {
    cli::cli_abort(c("x" = "{.arg verbose} must be logical.",
                     "i" = "You supplied incorrect parameters for verbose"))
  }
  method <- match.arg(method)
  res <- (x^power) * scale
  if (method == "simple") res <- round(res)
  if (verbose) cli::cli_inform("v" = "Method selected: '", method, "' | Processing ", length(x), " elements.",
                               "i" = "The ", method, "will either round or print the precise calculation")
  if (plot) plot(x, res, ...)
  return(res)
}
scaled_power_cli(x = c(1, 2, 3))
res <- scaled_power_cli(
  x = seq(-3, 3, by = 0.5), 
  power = 2, 
  scale = 5, 
  method = "precise",
  col = "firebrick", 
  pch = 19, 
  type = "b"
)
res
