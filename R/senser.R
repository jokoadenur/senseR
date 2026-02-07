#' senser: Proxy Indicator Diagnostic Tool
#'
#' @description
#' \code{senser()} is a statistical diagnostic function designed to evaluate
#' whether one or more proxy indicators are suitable representations of an
#' underlying construct that cannot be directly observed or measured.
#'
#' The function assesses each proxy based on multiple statistical dimensions:
#' monotonicity, information content, stability, distributional alignment,
#' bias risk, and dynamic range (sensitivity). The output is a concise,
#' interpretable summary printed directly to the console.
#'
#' This tool is intended for analytical diagnostics and policy-oriented
#' indicator assessment. It does not claim causal inference.
#'
#' @param data A \code{data.frame} containing the target construct and proxy variables.
#'
#' @param proxy A character vector specifying one or more proxy indicator
#'   variable names contained in \code{data}.
#'
#' @param target A character string specifying the target construct variable
#'   name contained in \code{data}.
#'
#' @param lang A character string specifying the language of interpretation.
#'   Options are \code{"english"} (default) or \code{"indonesia"}.
#'
#' @details
#' The diagnostic score for each proxy is computed using six normalized components:
#'
#' \enumerate{
#'   \item \strong{Monotonicity}: Spearman rank correlation between proxy and target.
#'   \item \strong{Information content}: Proportion of variance explained (R-squared).
#'   \item \strong{Stability}: Sensitivity of regression coefficients across subsamples.
#'   \item \strong{Distributional alignment}: Similarity of standardized distributions
#'         using the Kolmogorov--Smirnov statistic.
#'   \item \strong{Bias risk}: Penalization for strong nonlinearity indicating
#'         potential proxy distortion.
#'   \item \strong{Dynamic range / Sensitivity}: Penalization if the proxy has
#'         very small changes relative to the target (detects ceiling effect or nearly flat proxies).
#' }
#'
#' The final score is calculated as the \strong{median} of all six components,
#' providing a robust measure less sensitive to extreme values.
#'
#' The score ranges from 0 to 1 and is classified into three categories:
#'
#' \itemize{
#'   \item \strong{Suitable proxy}: score >= 0.70
#'   \item \strong{Conditionally suitable}: 0.40 <= score < 0.70
#'   \item \strong{Not suitable proxy}: score < 0.40
#' }
#'
#' Interpretation is automatically generated in the selected language.
#'
#' @return
#' The function prints a structured diagnostic summary to the console.
#' No object is returned invisibly. This design prioritizes interpretability
#' and ease of use for applied users and policymakers.
#'
#' @author
#' Joko Nursiyono (concept) \cr
#' senseR development team
#'
#' @seealso
#' \code{\link{lm}}, \code{\link{cor}}, \code{\link{ks.test}}
#'
#' @examples
#' ## Example with multiple proxies
#' set.seed(123)
#' df <- data.frame(
#'   gdp = rnorm(100, 10, 2),
#'   ntl = rnorm(100, 50, 10),
#'   road_density = rnorm(100, 3, 0.5),
#'   mobile_signal = rnorm(100, 70, 15)
#' )
#'
#' senser(
#'   data = df,
#'   proxy = c("ntl", "road_density", "mobile_signal"),
#'   target = "gdp",
#'   lang = "english"
#' )
#'
#' @export
senser <- function(data, proxy, target, lang = c("english", "indonesia")) {

  lang <- match.arg(lang)

  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!all(proxy %in% names(data))) stop("one or more proxy variables not found in data")
  if (!target %in% names(data)) stop("target variable not found in data")

  Y <- data[[target]]
  if (!is.numeric(Y)) stop("target variable must be numeric")

  cat("\n============================================\n")
  cat(" senseR - Proxy Indicator Diagnostic\n")
  cat("============================================\n")

  if (lang == "english") cat("Target construct :", target, "\n\n") else cat("Konstruk target  :", target, "\n\n")

  for (p in proxy) {

    X <- data[[p]]
    if (!is.numeric(X)) next

    idx <- complete.cases(X, Y)
    X <- X[idx]; Yc <- Y[idx]

    if (length(X) < 10) next

    # Monotonicity
    mono <- abs(cor(X, Yc, method = "spearman"))

    # Information content (R-squared)
    info <- cor(X, Yc)^2

    # Stability
    half <- floor(length(X)/2)
    b_full <- coef(lm(Yc ~ X))[2]
    b_sub1 <- coef(lm(Yc[1:half] ~ X[1:half]))[2]
    b_sub2 <- coef(lm(Yc[(half+1):length(X)] ~ X[(half+1):length(X)]))[2]
    sens <- 1 - sd(c(b_full, b_sub1, b_sub2), na.rm = TRUE) / max(abs(b_full), 1e-8)
    sens <- max(min(sens, 1), 0)

    # Distributional alignment (KS test)
    ks_val <- suppressWarnings(ks.test(scale(X), scale(Yc))$statistic)
    align <- 1 - ks_val

    # Bias risk: linear vs quadratic
    lin  <- lm(Yc ~ X)
    quad <- lm(Yc ~ poly(X,2))
    bias <- ifelse(summary(quad)$r.squared - summary(lin)$r.squared > 0.1, 0.3, 1)

    # Range / sensitivity
    range_ratio <- diff(range(X)) / (diff(range(Yc)) + 1e-8)
    range_penalty <- ifelse(range_ratio < 0.1, 0.5, 1)

    # Robust score: median dari semua komponen
    score <- median(c(mono, info, sens, align, bias, range_penalty))

    # Classification
    if (score >= 0.7) {
      classif <- if(lang=="english") "Suitable proxy" else "Proxy layak"
    } else if (score >= 0.4) {
      classif <- if(lang=="english") "Conditionally suitable" else "Layak bersyarat"
    } else {
      classif <- if(lang=="english") "Not suitable proxy" else "Proxy tidak layak"
    }

    # Interpretation
    if (lang=="english") {
      interp <- if(score>=0.7) {
        "This proxy reliably represents the target construct and is suitable for analytical and policy use."
      } else if(score>=0.4) {
        "This proxy captures part of the target construct, but its use requires contextual caution."
      } else {
        "This proxy provides a weak and potentially misleading representation of the target construct."
      }
    } else {
      interp <- if(score>=0.7) {
        "Proxy ini secara andal merepresentasikan konstruk target dan layak digunakan untuk analisis maupun kebijakan."
      } else if(score>=0.4) {
        "Proxy ini menangkap sebagian konstruk target, namun penggunaannya perlu kehati-hatian konteks."
      } else {
        "Proxy ini lemah dan berpotensi tidak akurat dalam merepresentasikan konstruk target."
      }
    }

    # Output
    cat("Proxy :", p, "\n")
    cat("Score :", round(score,3), "\n")
    cat("Status:", classif, "\n")
    cat("Interpretation:\n", interp, "\n\n")
  }

  invisible(NULL)
}
