% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visstat.R
\name{visstat}
\alias{visstat}
\title{Automated Visualization of Statistical Hypothesis Testing}
\usage{
visstat(
  dataframe,
  varsample,
  varfactor,
  conf.level = 0.95,
  numbers = TRUE,
  minpercent = 0.05,
  graphicsoutput = NULL,
  plotName = NULL,
  plotDirectory = getwd()
)
}
\arguments{
\item{dataframe}{\code{data.frame} containing at least two columns. Data must
be column wise ordered.}

\item{varsample}{column name of the dependent variable (response) in
\code{dataframe}, datatype \code{character}. \code{varsample} must be one
entry of the list \code{names(dataframe)}.}

\item{varfactor}{column name of the independent variable (feature) in
\code{dataframe}, datatype \code{character}.\code{varsample} must be one
entry of the list \code{names(dataframe)}.}

\item{conf.level}{confidence level}

\item{numbers}{a logical indicating whether to show numbers in mosaic count
plots.}

\item{minpercent}{number between 0 and 1 indicating minimal fraction of total
count data of a category to be displayed    in mosaic count plots.}

\item{graphicsoutput}{saves plot(s) of type "png",  "jpg", "tiff" or  "bmp"
in directory specified in \code{plotDirectory}. If graphicsoutput=NULL, no
plots are saved.}

\item{plotName}{graphical output is stored following the naming convention
"plotName.graphicsoutput" in \code{plotDirectory}. Without specifying this
parameter, plotName is automatically generated following the convention
"statisticalTestName_varsample_varfactor".}

\item{plotDirectory}{specifies directory, where generated plots are stored.
Default is current working directory.}
}
\value{
\code{list} containing statistics of automatically selected test
  meeting assumptions. All values are returned as invisible copies.
  Values can be accessed by assigning a return value to \code{visstat}.
}
\description{
\code{visstat()} provides automated visualization and selection
of a statistical hypothesis test between a response and a feature variable in
a given \code{data.frame} named \code{dataframe}, selecting a test that is
appropriate under the data's type, distribution, sample size, and the
specified \code{conf.level}. The data in \code{dataframe} must be structured
column-wise, where \code{varsample} and \code{varfactor} are \code{character}
strings corresponding to the column names of the response and feature
variables, respectively. The automatically generated output figures
illustrate the selected statistical hypothesis test, display the main test
statistics, and include assumption checks and post hoc comparisons when
applicable. The primary test results are returned as a list object.
}
\details{
Decision logic (for more details, please refer to the package's
\code{vignette}).

Throughout, data of class \code{numeric} or \code{integer} are referred to as
numerical, while data of class \code{factor} are referred to as categorical.
The significance level \eqn{\alpha} is defined as one minus the confidence
level, given by the argument \code{conf.level}.' Assumptions of normality and
homoscedasticity are considered met when the corresponding test yields a
p-value greater than alpha = 1 - 'conf.level'.

The choice of statistical tests performed by the function \code{visstat()}
depends on whether the data are numerical or categorical, the number of
levels in the categorical variable, the distribution of the data, and the
chosen \code{conf.level()}.

The function prioritizes interpretable visual output and tests that remain
valid under their assumptions, following the decision logic below:

(1) When the response is numerical and the predictor is categorical, tests of
central tendency are performed. If the categorical predictor has two levels:
- Welch's t-test (\code{t.test()}) is used if both groups have more than 30
observations (Lumley et al. (2002)
<doi:10.1146/annurev.publheath.23.100901.140546>).
- For smaller samples, normality is assessed using \code{shapiro.test()}.
If both groups return p-values greater than \eqn{\alpha}, Welch's t-test is
applied; otherwise, the Wilcoxon rank-sum test (\code{wilcox.test()}) is
used.

For predictors with more than two levels:
- An ANOVA model (\code{aov()}) is initially fitted.
- Residual normality is tested with \code{shapiro.test()} and \code{ad.test()}.
If \eqn{p > \alpha} for either test, normality is assumed.
- Homogeneity of variance is tested with \code{bartlett.test()}:
  - If \eqn{p > \alpha}, use ANOVA with \code{TukeyHSD()}.
  - If \eqn{p \le \alpha}, use \code{oneway.test()} with \code{TukeyHSD()}.
- If residuals are not normal, use \code{kruskal.test()} with
\code{pairwise.wilcox.test()}.

(2) When both the response and predictor are numerical, a linear model
(\code{lm()}) is fitted, with residual diagnostics and a confidence band
plot.

(3) When both variables are categorical, \code{visstat()} uses
\code{chisq.test()} or \code{fisher.test()} depending on expected counts,
following Cochran's rule (Cochran (1954) <doi:10.2307/3001666>).

Implemented main tests: \code{t.test()}, \code{wilcox.test()}, \code{aov()},
\code{oneway.test()}, \code{lm()}, \code{kruskal.test()},
\code{fisher.test()}, \code{chisq.test()}.

Implemented tests for assumptions:
\itemize{
  \item Normality: \code{shapiro.test()} and \code{ad.test()}.
  \item Heteroscedasticity: \code{bartlett.test()}.
}

Implemented post hoc tests:
\itemize{
  \item \code{TukeyHSD()} for \code{aov()} and \code{oneway.test()}.
  \item \code{pairwise.wilcox.test()} for \code{kruskal.test()}.
}
}
\examples{

## Welch Two Sample t-test (calling t.test())
visstat(mtcars, "mpg", "am")

## Wilcoxon rank sum test (calling wilcox.test())
grades_gender <- data.frame(
  Sex = as.factor(c(rep("Girl", 20), rep("Boy", 20))),
  Grade = c(
    19.3, 18.1, 15.2, 18.3, 7.9, 6.2, 19.4,
    20.3, 9.3, 11.3, 18.2, 17.5, 10.2, 20.1, 13.3, 17.2, 15.1, 16.2, 17.3,
    16.5, 5.1, 15.3, 17.1, 14.8, 15.4, 14.4, 7.5, 15.5, 6.0, 17.4,
    7.3, 14.3, 13.5, 8.0, 19.5, 13.4, 17.9, 17.7, 16.4, 15.6
  )
)
visstat(grades_gender, "Grade", "Sex")

## One-way analysis of means (oneway.test())
anova_npk <- visstat(npk, "yield", "block")
anova_npk # prints summary of tests

## Kruskal-Wallis rank sum test (calling kruskal.test())
visstat(iris, "Petal.Width", "Species")
visstat(InsectSprays, "count", "spray")

## Linear regression
visstat(trees, "Girth", "Height", conf.level = 0.99)

## Pearson's Chi-squared test and mosaic plot with Pearson residuals
### Transform array to data.frame
HairEyeColorDataFrame <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(HairEyeColorDataFrame, "Hair", "Eye")

## 2x2 contingency tables with Fisher's exact test and mosaic plot
## with Pearson residuals
HairEyeColorMaleFisher <- HairEyeColor[, , 1]
### slicing out a 2 x2 contingency table
blackBrownHazelGreen <- HairEyeColorMaleFisher[1:2, 3:4]
blackBrownHazelGreen <- counts_to_cases(as.data.frame(blackBrownHazelGreen))
fisher_stats <- visstat(blackBrownHazelGreen, "Hair", "Eye")
fisher_stats # print out summary statistics



## Saving the graphical output in directory plotDirectory
## A) saving graphical output of type "png" in temporary directory tempdir()
##    with default naming convention:
visstat(blackBrownHazelGreen, "Hair", "Eye",
  graphicsoutput = "png",
  plotDirectory = tempdir()
)

## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "chi_squared_or_fisher_Hair_Eye.png"))
file.remove(file.path(tempdir(), "mosaic_complete_Hair_Eye.png"))

## B) Specifying pdf as output type:
visstat(iris, "Petal.Width", "Species",
  graphicsoutput = "pdf",
  plotDirectory = tempdir()
)

## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "kruskal_Petal_Width_Species.pdf"))

## C) Specifiying plotName overwrites default naming convention
visstat(iris, "Petal.Width", "Species",
  graphicsoutput = "pdf",
  plotName = "kruskal_iris", plotDirectory = tempdir()
)
## remove graphical output from plotDirectory
file.remove(file.path(tempdir(), "kruskal_iris.pdf"))

}
\seealso{
\url{https://shhschilling.github.io/visStatistics/}
}
