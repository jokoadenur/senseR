![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)
![CRAN Version](https://img.shields.io/badge/CRAN-0.1.0-brightgreen)
![Open Issues](https://img.shields.io/badge/open%20issues-0-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)

<img width="1024" height="1024" alt="ChatGPT Image Feb 17, 2026, 12_14_55 PM" src="https://github.com/user-attachments/assets/c4488b33-a373-417c-803c-277906fd6dad" />


# senseR

Quality assessment framework for proxy indicators using
monotonicity, information content, responsiveness,
dispersion, stagnation, ceiling effect, and stability metrics.

## Installation

```r
# install.packages("devtools")
devtools::install_github("username/senseR")
```

## Example
```r
library(senseR)

set.seed(42)
df <- data.frame(
  gdp = rnorm(50, 100, 10),
  ntl = rnorm(50, 50, 5) #ntl is nightime light index from Google Earth Engine (GEE)
)
senser(df, proxy = "ntl", target = "gdp") #English explanation by default
```
## Methodological Background


The composite score integrates: (1) Spearman monotonicity; (2) R-squared information content; (3) Elasticity responsiveness; (4) Coefficient of variation; (5) Absolute change; (6) Ceiling effect; and (7) Beta stability.

## Scientific References

The methodological foundation of **senseR** is based on established statistical
and econometric literature:

### 1. Monotonicity (Spearman Rank Correlation)

Spearman, C. (1904). *The proof and measurement of association between two things.*  **American Journal of Psychology**, 15(1), 72–101. https://doi.org/10.2307/1412159

---

### 2. Information Content (R² Interpretation)

Cohen, J. (1988).  *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.).  Lawrence Erlbaum Associates.

---

### 3. Elasticity and Responsiveness

Wooldridge, J. M. (2013).  *Introductory Econometrics: A Modern Approach* (5th ed.).  South-Western Cengage Learning.

---

### 4. Composite Indicator Methodology

OECD (2008).  *Handbook on Constructing Composite Indicators: Methodology and User Guide.*  OECD Publishing. https://doi.org/10.1787/9789264043466-en

---

### 5. Time Series Stability Concepts

Hamilton, J. D. (1994).  *Time Series Analysis.*  Princeton University Press.

---

### 6. Structural Stability (Chow Test)

Chow, G. C. (1960).  Tests of equality between sets of coefficients in two linear regressions.  **Econometrica**, 28(3), 591–605. https://doi.org/10.2307/1910133
