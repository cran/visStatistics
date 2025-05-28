## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(visStatistics)

## ----install-github, eval = FALSE---------------------------------------------
# install_github("shhschilling/visStatistics")

## ----load, eval = FALSE-------------------------------------------------------
# library(visStatistics)

## ----fig-decision-switch, echo=FALSE, results='asis'--------------------------
if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
  cat('
<div style="border: 1px solid #666; padding: 10px; display: inline-block; text-align: center;">
  <img src="figures/decision_tree.png" width="100%" 
       alt="Decision tree used to select the appropriate statistical test.">
  <p style="font-style: italic; font-size: 90%; margin-top: 0.5em;">
    Decision tree used to select the appropriate statistical test for a categorical
    predictor and numeric response, based on the number of factor levels, normality,
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
    \\includegraphics[width=\\linewidth]{../man/figures/decision_tree.png}\\\\
    \\vspace{0.5em}
    \\textit{Decision tree used to select the appropriate statistical test for a categorical predictor and numeric response, based on the number of factor levels, normality, and homoscedasticity.}
  \\end{minipage}
}
\\end{center}
')
}

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
t_test_statistics <- visstat(mtcars$am, mtcars$mpg)

## -----------------------------------------------------------------------------
mtcars$am <- as.factor(mtcars$am)
t_test_statistics_99 <- visstat(mtcars$am, mtcars$mpg, conf.level = 0.99)

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

## -----------------------------------------------------------------------------
oneway_npk <- visstat(npk$block,npk$yield,conf.level=0.90)

## -----------------------------------------------------------------------------
insect_sprays_tr <- InsectSprays
insect_sprays_tr$count_sqrt <- sqrt(InsectSprays$count)
test_statistic_anova=visstat(insect_sprays_tr$spray, insect_sprays_tr$count_sqrt)
# test_statistic_anova 

## -----------------------------------------------------------------------------
visstat(iris$Species, iris$Petal.Width)

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars$speed, cars$dist)

## -----------------------------------------------------------------------------
linreg_cars <- visstat(cars$speed,cars$dist, conf.level = 0.99)

## -----------------------------------------------------------------------------
linreg_trees <- visstat(trees$Girth, trees$Volume,conf.level = 0.9)

## -----------------------------------------------------------------------------
linreg_cars <- visstat(trees$Girth, trees$Volume, conf.level = 0.9)

## -----------------------------------------------------------------------------
HairEyeColourDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))

## -----------------------------------------------------------------------------
hair_eye_colour_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_colour_df$Eye, hair_eye_colour_df$Hair)

## -----------------------------------------------------------------------------
hair_black_brown_eyes_brown_blue <- HairEyeColor[1:2, 1:2, ]
# Transform to data frame
hair_black_brown_eyes_brown_blue_df <- counts_to_cases(as.data.frame(hair_black_brown_eyes_brown_blue))
# Chi-squared test
visstat(hair_black_brown_eyes_brown_blue_df$Eye, hair_black_brown_eyes_brown_blue_df$Hair)

## -----------------------------------------------------------------------------
hair_eye_colour_male <- HairEyeColor[, , 1]
# Slice out a 2 by 2 contingency table
black_brown_hazel_green_male <- hair_eye_colour_male[1:2, 3:4]
# Transform to data frame
black_brown_hazel_green_male <- counts_to_cases(as.data.frame(black_brown_hazel_green_male))
# Fisher test
fisher_stats <- visstat(black_brown_hazel_green_male$Eye, black_brown_hazel_green_male$Hair)

## -----------------------------------------------------------------------------
#Graphical output written to plotDirectory: In this example 
# a bar chart to visualise the Chi-squared test and mosaic plot showing
# Pearson's residuals.
#chi_squared_or_fisher_Hair_Eye.png and mosaic_complete_Hair_Eye.png
visstat(black_brown_hazel_green_male, "Hair", "Eye",
        graphicsoutput = "png", plotDirectory = tempdir())

## ----eval=FALSE---------------------------------------------------------------
# file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
# file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))

