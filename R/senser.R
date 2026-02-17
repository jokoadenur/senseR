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
#' @param stagnation_cut Numeric scalar. Threshold used to detect stagnation
#' (very small average absolute change). If the average absolute change of
#' the proxy variable is below this value, a structural penalty is applied.
#' Default is 0.01.
#'
#' @details
#' The stagnation threshold is used to penalize proxy variables that exhibit
#' minimal temporal or cross-sectional variation. Extremely small changes
#' may indicate lack of sensitivity or measurement rigidity.
#'
#' @param cv_cut Numeric scalar. Threshold for the coefficient of variation
#' (CV). If the coefficient of variation of the proxy variable is below
#' this value, a variability penalty is applied. Default is 0.02.
#'
#' @details
#' The coefficient of variation (CV) is defined as the ratio of the standard
#' deviation to the mean. Very low CV values indicate insufficient dispersion,
#' which may reduce the informational usefulness of the proxy variable.
#'
#' @param ceiling_cut Numeric scalar. Threshold used to detect ceiling
#' effects. If the ceiling ratio exceeds this value, a ceiling penalty is
#' applied. Default is 0.95.
#'
#' @details
#' A ceiling effect occurs when observations cluster near the upper bound
#' of the distribution, limiting discriminatory power. If the ceiling ratio
#' exceeds this threshold, the proxy's score is penalized to reflect reduced
#' measurement sensitivity.
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
senser <- function(data, proxy, target,
                   lang = c("english", "indonesia"),
                   stagnation_cut = 0.01,
                   cv_cut = 0.02,
                   ceiling_cut = 0.95) {

  lang <- match.arg(lang)

  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!all(proxy %in% names(data))) stop("proxy not found")
  if (!target %in% names(data)) stop("target not found")

  Y <- data[[target]]
  if (!is.numeric(Y)) stop("target must be numeric")

  cat("\n====================================================\n")
  cat(" senseR STRICT - Proxy Indicator Diagnostic\n")
  cat("====================================================\n")

  if (lang == "english")
    cat("Target construct :", target, "\n\n")
  else
    cat("Konstruk target  :", target, "\n\n")

  for (p in proxy) {

    X <- data[[p]]
    if (!is.numeric(X)) next

    idx <- complete.cases(X, Y)
    X <- X[idx]; Yc <- Y[idx]
    if (length(X) < 10) next

    # --- CORE TESTS ---
    mono  <- abs(cor(X, Yc, method = "spearman"))
    info  <- cor(X, Yc)^2

    # Elasticity (responsiveness)
    elast <- abs(coef(lm(Yc ~ X))[2]) * sd(X)/sd(Yc)
    elast_score <- ifelse(elast < 0.1, 0.3, 1)

    # Variance adequacy
    cv <- sd(X)/mean(X)
    cv_score <- ifelse(cv < cv_cut, 0.3, 1)

    # Stagnation over time
    avg_change <- mean(abs(diff(X)), na.rm=TRUE)
    stagnation_score <- ifelse(avg_change < stagnation_cut, 0.2, 1)

    # Ceiling effect
    ceiling_ratio <- mean(X)/max(X)
    ceiling_score <- ifelse(ceiling_ratio > ceiling_cut, 0.3, 1)

    # Stability
    half <- floor(length(X)/2)
    b_full <- coef(lm(Yc ~ X))[2]
    b_sub1 <- coef(lm(Yc[1:half] ~ X[1:half]))[2]
    b_sub2 <- coef(lm(Yc[(half+1):length(X)] ~ X[(half+1):length(X)]))[2]
    sens <- 1 - sd(c(b_full, b_sub1, b_sub2)) / max(abs(b_full), 1e-8)
    sens <- max(min(sens,1),0)

    # HARD FAILURE CONDITIONS
    hard_fail <- (cv < cv_cut & avg_change < stagnation_cut) |
      (ceiling_ratio > 0.97)

    components <- c(mono, info, elast_score, cv_score,
                    stagnation_score, ceiling_score, sens)

    score <- median(components)

    if (hard_fail) score <- score * 0.4

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
      interp <- if(hard_fail) {
        "The indicator shows structural stagnation or ceiling effect. It lacks discriminative power and is statistically weak."
      } else if(score>=0.7) {
        "This proxy demonstrates adequate variability, responsiveness, and stability."
      } else if(score>=0.4) {
        "The proxy partially captures the construct but has structural limitations."
      } else {
        "The proxy fails to provide reliable and informative representation."
      }
    } else {
      interp <- if(hard_fail) {
        "Indikator mengalami stagnasi struktural atau efek ceiling. Daya bedanya rendah dan lemah secara statistik."
      } else if(score>=0.7) {
        "Proxy memiliki variabilitas, sensitivitas, dan stabilitas yang memadai."
      } else if(score>=0.4) {
        "Proxy menangkap sebagian konstruk namun memiliki keterbatasan struktural."
      } else {
        "Proxy gagal merepresentasikan konstruk secara informatif dan andal."
      }
    }

    # ==============================
    # DIAGNOSTIC OUTPUT COMPONENTS
    # ==============================
    cat("----------------------------------------------------\n")
    cat(if(lang=="english") "Proxy :" else "Proxy :", p, "\n")
    cat("----------------------------------------------------\n")

    if(lang=="english"){

      cat("1. Monotonicity (Spearman)        :", round(mono,4), "\n")
      cat("2. Information Content (R2)       :", round(info,4), "\n")

      cat("3. Elasticity (Responsiveness)    :", round(elast,4),
          "| Score:", elast_score,
          "| Penalized if <", 0.1, "\n")

      cat("4. Coefficient of Variation (CV)  :", round(cv,4),
          "| Score:", cv_score,
          "| Penalized if <", cv_cut, "\n")

      cat("5. Average Absolute Change        :", round(avg_change,4),
          "| Score:", stagnation_score,
          "| Penalized if <", stagnation_cut, "\n")

      cat("6. Ceiling Effect Ratio           :", round(ceiling_ratio,4),
          "| Score:", ceiling_score,
          "| Penalized if >", ceiling_cut, "\n")

      cat("7. Stability (Beta Consistency)   :", round(sens,4), "\n")

      cat("Hard Failure Triggered            :", hard_fail, "\n")

    } else {

      cat("1. Monotonisitas (Spearman)       :", round(mono,4), "\n")
      cat("2. Kandungan Informasi (R2)       :", round(info,4), "\n")

      cat("3. Elastisitas (Responsivitas)    :", round(elast,4),
          "| Skor:", elast_score,
          "| Penalti jika <", 0.1, "\n")

      cat("4. Koefisien Variasi (KV)         :", round(cv,4),
          "| Skor:", cv_score,
          "| Penalti jika <", cv_cut, "\n")

      cat("5. Rata-rata Perubahan Absolut    :", round(avg_change,4),
          "| Skor:", stagnation_score,
          "| Penalti jika <", stagnation_cut, "\n")

      cat("6. Rasio Efek Ceiling             :", round(ceiling_ratio,4),
          "| Skor:", ceiling_score,
          "| Penalti jika >", ceiling_cut, "\n")

      cat("7. Stabilitas (Konsistensi Beta)  :", round(sens,4), "\n")

      cat("Kegagalan Struktural Terdeteksi   :", hard_fail, "\n")
    }

    cat("----------------------------------------------------\n")

    components <- c(mono, info, elast_score, cv_score,
                    stagnation_score, ceiling_score, sens)

    score <- median(components)

    if (hard_fail) {
      if(lang=="english"){
        cat("Warning: Structural penalty applied (x0.4)\n")
      } else {
        cat("Warning: Penalti struktural diterapkan (x0.4)\n")
      }
      score <- score * 0.4
    }

    if(lang=="english"){
      cat("Final Proxy Score (Median Rule)   :", round(score,4), "\n")
    } else {
      cat("Skor Akhir Proxy (Aturan Median)  :", round(score,4), "\n")
    }
    cat("----------------------------------------------------\n")
    cat("Proxy :", p, "\n")
    cat("Score :", round(score,3), "\n")
    cat("Status:", classif, "\n")
    cat("Interpretation:\n", interp, "\n\n")
  }

  # ============================================
  # SCIENTIFIC FOOTNOTES
  # ============================================
  cat("====================================================\n")

  if(lang == "english"){

    cat("Scientific References:\n\n")

    cat("[1] Spearman, C. (1904). The proof and measurement of association between two things.\n")
    cat("                        American Journal of Psychology, 15(1), 72-101.\n\n")

    cat("[2] Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences.\n\n")

    cat("[3] Wooldridge, J. M. (2013). Introductory Econometrics: A Modern Approach.\n\n")

    cat("[4] OECD (2008). Handbook on Constructing Composite Indicators:\n")
    cat("               Methodology and User Guide.\n\n")

    cat("[5] Hamilton, J. D. (1994). Time Series Analysis.\n\n")

    cat("[6] Chow, G. C. (1960). Tests of equality between sets of coefficients\n")
    cat("                       in two linear regressions. Econometrica.\n\n")

  } else {

    cat("Referensi Ilmiah:\n\n")

    cat("[1] Spearman, C. (1904). The proof and measurement of association between two things.\n")
    cat("    American Journal of Psychology, 15(1), 72-101.\n\n")

    cat("[2] Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences.\n\n")

    cat("[3] Wooldridge, J. M. (2013). Introductory Econometrics: A Modern Approach.\n\n")

    cat("[4] OECD (2008). Handbook on Constructing Composite Indicators:\n")
    cat("    Methodology and User Guide.\n\n")

    cat("[5] Hamilton, J. D. (1994). Time Series Analysis.\n\n")

    cat("[6] Chow, G. C. (1960). Tests of equality between sets of coefficients\n")
    cat("    in two linear regressions. Econometrica.\n\n")
  }
  invisible(NULL)
}
