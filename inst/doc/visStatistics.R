## ----setup, include=FALSE-----------------------------------------------------
library(visStatistics)
  knitr::opts_chunk$set(
    fig.width = 7.5,      # Smaller device = smaller text
    fig.height = 5,
    out.width = '100%')

## ----install-github, eval = FALSE---------------------------------------------
# install_github("shhschilling/visStatistics")

## ----load, eval = FALSE-------------------------------------------------------
# library(visStatistics)

## ----npk, fig.show='hide'-----------------------------------------------------
#Standardised form
visstat(npk$block,npk$yield)
# Using formula interface
 visstat(yield~block,data=npk)
#Backward-compatible form
 visstat(npk,"yield","block")

## ----fig-overview, echo=FALSE, results='asis'---------------------------------
if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
  cat('
<div style="border: 1px solid #666; padding: 10px; display: inline-block; text-align: center;">
  <img src="figures/overview.png" width="100%"
       alt="Overview of implemented tests .">
  <p style="font-style: italic; font-size: 90%; margin-top: 0.5em;">
    Overview of the implemented statistical tests based on the class of the variables.
  </p>
</div>
')
} else {
  cat('
\\begin{center}
\\fbox{%
  \\begin{minipage}{0.95\\linewidth}
    \\centering
    \\includegraphics[width=\\linewidth]{figures/overview.png}\\\\
    \\vspace{0.5em}
    \\textit{Decision tree used to select the appropriate statistical test for a categorical predictor and numeric response, based on the number of factor levels, normality, and homoscedasticity.}
  \\end{minipage}
}
\\end{center}
')
}

## ----fig-decision-switch, echo=FALSE, results='asis'--------------------------
if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
  cat('
<div style="border: 1px solid #666; padding: 10px; display: inline-block; text-align: center;">
  <img src="figures/decision_tree.png" width="100%"
       alt="Decision tree used to select the appropriate statistical test.">
  <p style="font-style: italic; font-size: 90%; margin-top: 0.5em;">
    Decision tree used to select the appropriate statistical test for a categorical
    predictor and numeric response, based on the sample sizes, number of factor levels, normality,
    and homoscedasticity.
  </p>
</div>
')
} else {
  cat('
\\begin{center}
\\fbox{%
  \\begin{minipage}{0.95\\linewidth}
    \\centering
    \\includegraphics[width=\\linewidth]{figures/decision_tree.png}\\\\
    \\vspace{0.5em}
    \\textit{Decision tree used to select the appropriate statistical test for a categorical predictor and numeric response, based on the sample sizes, number of factor levels, normality, and homoscedasticity.}
  \\end{minipage}
}
\\end{center}
')
}

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
t_test_statistics <- visstat(mtcars$am, mtcars$mpg)

## -----------------------------------------------------------------------------
grades_gender <- data.frame(
  sex = as.factor(c(rep("girl", 21), rep("boy", 23))),
  grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.0,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6, 17.3, 19.9, 4.4, 2.1
  )
)

wilcoxon_statistics <- visstat(grades_gender$sex, grades_gender$grade)

## ----ordinal, fig.show='hide'-------------------------------------------------
set.seed(123)

# Create predictor: Customer segment (2 groups)
segment <- factor(rep(c("Budget", "Premium"), each = 50))

# Create response: Likert scale ratings (1-5)
satisfaction_numeric <- c(
  sample(1:5, 50, replace = TRUE, prob = c(0.15, 0.25, 0.30, 0.20, 0.10)),  # Budget
  sample(1:5, 50, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))   # Premium
)

# Create dataframe with ORDERED response
survey_data <- data.frame(
  segment = segment,
  satisfaction = ordered(satisfaction_numeric)  # Declare as ordered
)

# triggers warnings and use Wilcoxon test
wilcox_ordered <- visstat(survey_data, "satisfaction", "segment")

## -----------------------------------------------------------------------------
anova_npk <- visstat(npk$block,npk$yield,conf.level=0.95)

## ----results='hide'-----------------------------------------------------------
visstat(iris$Species, iris$Petal.Width)

## ----ordinal-kruskal, fig.show='hide'-----------------------------------------
set.seed(123)

# Predictor: customer segment (3 groups)
segment <- factor(rep(c("Budget", "Standard", "Premium"), each = 50))

# Response: Likert scale ratings (1-5), with a deliberate trend across segments
comfort_numeric <- c(
  sample(1:5, 50, replace = TRUE, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)),  # Budget
  sample(1:5, 50, replace = TRUE, prob = c(0.10, 0.20, 0.40, 0.20, 0.10)),  # Standard
  sample(1:5, 50, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))   # Premium
)

# Dataframe with ORDERED response
survey_data_3 <- data.frame(
  segment = segment,
  comfort = ordered(comfort_numeric)  # Declare as ordered
)

# triggers warning and uses Kruskal-Wallis test
kruskal_ordered <- visstat(survey_data_3, "comfort", "segment")

## -----------------------------------------------------------------------------
linreg_swiss <- visstat(swiss$Examination, swiss$Fertility, conf.level = 0.99)

## ----fig.show='hide'----------------------------------------------------------
result_ozone0 <- visstat(airquality$Wind, airquality$Ozone)

## -----------------------------------------------------------------------------
result_ozone1 <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)

## -----------------------------------------------------------------------------
# Remove zeros to satisfy Gamma requirements
airquality_clean <- subset(airquality, Ozone > 0)
# Gamma model with log mapping
model_gamma <- glm(Ozone ~ Wind, data = airquality_clean, family = Gamma(link = "log"))
summary(model_gamma)

## ----echo=FALSE---------------------------------------------------------------
# Plotting the data with Gamma model overlay
plot(airquality_clean$Wind, airquality_clean$Ozone, log = "y", 
     pch = 19, col = "darkgrey", xlab = "Wind (mph)", ylab = "Ozone (ppb) [log scale]",
     main = "Gamma GLM Fit (Log Link)")
# Generate predictions for the overlay
wind_seq <- seq(min(airquality_clean$Wind), max(airquality_clean$Wind), length.out = 100)
preds <- predict(model_gamma, newdata = data.frame(Wind = wind_seq), type = "response")
lines(wind_seq, preds, col = "red", lwd = 2)

legend("topright", legend = c("Data", "Gamma GLM (log link)"), 
       col = c("darkgrey", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

## -----------------------------------------------------------------------------
# Extract standardised  residuals
std_dev_res <- rstandard(model_gamma, type = "deviance")
# Validate using the Shapiro-Wilk normality test
shapiro.test(std_dev_res)
# Validate using the Anderson-Darling normality test
nortest::ad.test(std_dev_res)

## -----------------------------------------------------------------------------
HairEyeColourDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))

## -----------------------------------------------------------------------------
hair_eye_colour_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_colour_df$Eye, hair_eye_colour_df$Hair)

## -----------------------------------------------------------------------------
hair_black_brown_eyes_brown_blue <- HairEyeColor[1:2, 1:2, ]
# Transform to data frame
hair_black_brown_eyes_brown_blue_df <- counts_to_cases(as.data.frame(hair_black_brown_eyes_brown_blue))
# Chi-squared test with Yates' continuity correction

visstat(hair_black_brown_eyes_brown_blue_df$Eye, hair_black_brown_eyes_brown_blue_df$Hair)

## ----fisher-data-prep---------------------------------------------------------
hair_eye_colour_male <- HairEyeColor[, , 1]
# Slice out a 2 by 2 contingency table
black_brown_hazel_green_male <- hair_eye_colour_male[1:2, 3:4]
# Transform to data frame
black_brown_hazel_green_male <- counts_to_cases(as.data.frame(black_brown_hazel_green_male))
# Fisher test
fisher_stats <- visstat(black_brown_hazel_green_male$Eye, black_brown_hazel_green_male$Hair)

## ----fisher-or----------------------------------------------------------------
fisher_stats$estimate   # odds ratio
fisher_stats$conf.int   # 95% CI

## ----kendall-example----------------------------------------------------------
set.seed(42)
n <- 150
# Latent scores with deliberate negative monotone association:
# higher alcohol consumption (xs) -> lower academic performance (ys)
xs <- sample(1:5, n, replace = TRUE)
ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
likert_levels  <- c("never", "rarely", "sometimes", "often", "always")
likert_levels2 <- c("poor", "fair", "ok", "good", "great")
alcohol     <- ordered(likert_levels[xs],  levels = likert_levels)
performance <- ordered(likert_levels2[ys], levels = likert_levels2)
kendall_result <- visstat(performance, alcohol, correlation = TRUE)

## ----fisher-save-graphics-----------------------------------------------------
# Graphical output written to plotDirectory: In this example
# a single bar chart showing absolute counts.
# Output file: chi_squared_or_fisher_Hair_Eye.png
save_fisher = visstat(black_brown_hazel_green_male$Eye, black_brown_hazel_green_male$Hair,
        graphicsoutput = "png", plotDirectory = tempdir())

## ----show-path2---------------------------------------------------------------
paths <- attr(save_fisher, "plot_paths")
print(paths)

## ----eval=TRUE----------------------------------------------------------------
file.remove(paths)

## ----visstat-methods, fig.show='hide', results='hide'-------------------------
iris_kruskal <- visstat(iris$Species, iris$Petal.Width)

## ----visstat-methods-print-summary--------------------------------------------
print(iris_kruskal)
summary(iris_kruskal)

## ----visstat-methods-plot-paths-----------------------------------------------
iris_kruskal_stored <- visstat(iris$Species, iris$Petal.Width,
                               graphicsoutput = "pdf",
                               plotName = "iris_kruskal",
                               plotDirectory = tempdir())
plot(iris_kruskal_stored)

## ----visstat-methods-plot-replay----------------------------------------------
plot(iris_kruskal)

## ----visstat-methods-cleanup, echo = FALSE------------------------------------
file.remove(attr(iris_kruskal_stored, "plot_paths"))

