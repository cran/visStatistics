## ----knit-setup, include=FALSE------------------------------------------------
# The child documents are named relative to this file. Knitting from the
# package root instead (RStudio's "Project Directory" setting) would look for
# them there, so the root directory is moved back to vignettes/ in that case.
if (!file.exists("children/_abstract.Rmd") && dir.exists("vignettes")) {
 knitr::opts_knit$set(root.dir = normalizePath("vignettes"))
}

## ----setup, include=FALSE-----------------------------------------------------
library(visStatistics)
knitr::opts_knit$set(root.dir = normalizePath("."))
knitr::opts_chunk$set(
 fig.width = 7,
 fig.height = 4.5,
 out.width = "100%",
 echo = TRUE
)
example_alpha <- 0.05
options(visStatistics.qq_nsim = 999L)

## ----npk, fig.show='hide', results='hide'-------------------------------------
# Standardised form
visstat(npk$block, npk$yield)

## ----overview, echo=FALSE, fig.cap="Overview of all implemented tests selected based on input class.", fig.alt="Flowchart showing all implemented statistical tests organised by the class of the input vectors."----
knitr::include_graphics("figures/overview.png")

## ----decision-tree, echo=FALSE, fig.cap="Decision tree for the default Route 1 test selection (group\\_test = NULL). Shapiro--Wilk on model residuals determines whether the route remains mean-based or switches to rank-based tests; the Levene test then selects equal-variance or Welch-type procedures.", fig.alt="Decision tree for the default Route 1 test selection among Welch t-test, Student t-test, Wilcoxon, Fisher ANOVA, Welch ANOVA, and Kruskal-Wallis tests, based on the Shapiro-Wilk test on model residuals and the Levene test for variance homogeneity."----
knitr::include_graphics("figures/decision_tree.png")

## ----student-ttest-example, fig.cap="Student's t-test applied to the `ToothGrowth` dataset (`len` vs.\\ `supp`). Assumption diagnostics (Shapiro--Wilk does not reject residual normality; Levene does not reject residual variance homogeneity) select the equal-variance mean-based path, followed by box plots with the Student t-test result.", out.width="48%", fig.height=4.5, fig.show="hold", results='hide'----
student_ttest <- visstat(ToothGrowth$supp, ToothGrowth$len)

## ----anova-example, results='hide', fig.show='hide'---------------------------
anova_plantgrowth <- visstat(PlantGrowth$group, PlantGrowth$weight)

## ----anova-plantgrowth-panels, fig.cap=paste0("`PlantGrowth`data set:  Fisher's one-way ANOVA (`weight` vs.\\ `group`). (a) Assumption-diagnostic panel. (b) Result panel with Tukey HSD significance letters ($\\alpha = ", example_alpha, "$)."), out.width="48%", fig.height=4.5, fig.show="hold"----
plot(anova_plantgrowth, which = 1)
plot(anova_plantgrowth, which = 2)

## ----anova-save-graphics------------------------------------------------------
anova_plantgrowth_stored <- visstat(
 PlantGrowth$group,
 PlantGrowth$weight,
 graphicsoutput = "png",
 plotName = "anova_plantgrowth",
 plotDirectory = tempdir()
)
paths <- attr(anova_plantgrowth_stored, "plot_paths")
print(basename(paths))

## ----anova-print--------------------------------------------------------------
print(anova_plantgrowth)

## ----anova-summary, eval=knitr::is_html_output(), echo=knitr::is_html_output()----
summary(anova_plantgrowth)

## ----cleanup-anova-paths, echo=FALSE, results='hide'--------------------------
file.remove(paths)

## ----ttest-example, fig.cap="Welch's t-test applied to the `mtcars` dataset (`mpg` vs.\\ `am`). Assumption diagnostics (Shapiro--Wilk does not reject residual normality; Levene rejects residual variance homogeneity) select the unequal-variance mean-based path, followed by box plots with the Welch t-test result.", out.width="48%", fig.height=4.5, fig.show="hold"----
mtcars$am <- as.factor(mtcars$am)
t_test_stats <- visstat(mtcars$am, mtcars$mpg)

## ----welch-anova-example, fig.cap=paste0("Welch's heteroscedastic one-way ANOVA applied to the `iris` dataset (`Sepal.Length` vs.\\ `Species`). Assumption diagnostics (Shapiro--Wilk does not reject residual normality; Levene rejects residual variance homogeneity) select the unequal-variance mean-based path, followed by box plots with Games--Howell significance letters ($\\alpha = ", example_alpha, "$)."), out.width="48%", fig.height=4.5, fig.show="hold"----
welch_anova_iris <- visstat(iris$Species, iris$Sepal.Length)

## ----wilcoxon-example, fig.cap="Wilcoxon rank-sum test applied to the `warpbreaks` dataset (`breaks` vs.\\ `wool`). Assumption diagnostics (Shapiro--Wilk rejects residual normality; non-parametric path selected) and box plots with the Wilcoxon test result.", out.width="48%", fig.height=4.5, fig.show="hold"----
wilcoxon_stats <- visstat(warpbreaks$wool, warpbreaks$breaks)

## ----kruskal-example, fig.cap=paste0("Kruskal-Wallis test applied to the `iris` dataset (`Petal.Width` vs.\\ `Species`). Assumption diagnostics (Shapiro--Wilk rejects residual normality; non-parametric path selected) and box plots with Holm-adjusted pairwise Wilcoxon significance letters ($\\alpha = ", example_alpha, "$)."), out.width="48%", fig.height=4.5, fig.show="hold"----
kruskal_iris <- visstat(iris$Species, iris$Petal.Width)

## ----ordinal-wilcoxon-example, fig.show='hide', results='hide'----------------
titanic_df <- counts_to_cases(as.data.frame(Titanic))
titanic_df$Class <- ordered(titanic_df$Class,
 levels = c("1st", "2nd", "3rd", "Crew")
)
wilcox_ordered <- visstat(titanic_df$Sex, titanic_df$Class)

## ----ordinal-kruskal-example, fig.show='hide', results='hide'-----------------
set.seed(123)
market <- factor(rep(c("Europe", "North America", "Asia"), each = 50))
comfort_numeric <- c(
 sample(1:5, 50, replace = TRUE, prob = c(0.30, 0.30, 0.20, 0.15, 0.05)),
 sample(1:5, 50, replace = TRUE, prob = c(0.10, 0.20, 0.40, 0.20, 0.10)),
 sample(1:5, 50, replace = TRUE, prob = c(0.05, 0.10, 0.20, 0.35, 0.30))
)
survey_data_3 <- data.frame(
 market = market,
 comfort = ordered(comfort_numeric)
)
kruskal_ordered <- visstat(comfort ~ market, data = survey_data_3)

## ----ordinal-wilcoxon-kruskal-caption, echo=FALSE-----------------------------
ordinal_wilcoxon_kruskal_cap <- paste0(
 "Wilcoxon rank-sum test for ordered passenger class by sex in the ",
 "expanded `Titanic` data (left) and its multi-group generalisation, ",
 "the Kruskal-Wallis test for ordered car comfort ratings by market ",
 "(right). ",
 "Holm-adjusted pairwise Wilcoxon post-hoc comparisons are shown as ",
 "significance letters for the Kruskal-Wallis example ($\\alpha = ",
 example_alpha, "$)."
)

## ----ordinal-wilcoxon-kruskal-example, echo=FALSE, warning=FALSE, fig.cap=ordinal_wilcoxon_kruskal_cap, out.width="48%", fig.height=4.5, fig.show="hold", results='hide'----
visstat(titanic_df$Sex, titanic_df$Class)
visstat(comfort ~ market, data = survey_data_3)

## ----regression-example, fig.cap="Simple linear regression of `Fertility` on `Examination` for the `swiss` dataset (`conf.level = 0.99`). Left: residual-diagnostic panel with histogram, normal Q-Q plot with simultaneous tolerance band (STB) and point-wise tolerance band (TB), and residuals versus fitted values. Right: scatter plot with fitted regression line, 99\\% prediction interval for an individual response, and 99\\% confidence interval for the mean response.", out.width="48%", fig.height=4.5, fig.show="hold"----
linreg_swiss <- visstat(swiss$Examination, swiss$Fertility, conf.level = 0.99)

## ----ozone-lm-triage, fig.cap="Default simple linear regression for `Ozone` by `Wind` in the `airquality` dataset. Assumption diagnostics flag non-normal model residuals and heteroscedasticity before alternative routes are considered.", fig.height=4.5, fig.show="hold", out.width="48%"----
ozone_lm <- visstat(airquality$Wind, airquality$Ozone)

## -----------------------------------------------------------------------------
# Gamma model with log mapping
model_gamma <- glm(Ozone ~ Wind, data = airquality, family = Gamma(link = "log"))
model_gamma$aic
# Comparison with AIC of simple linear regression
model_lm <- glm(Ozone ~ Wind, data = airquality)
model_lm$aic

## ----gamma-glm-plot, echo=FALSE, fig.cap="Gamma GLM with log link fitted to the `airquality` dataset `Ozone` vs. `Wind`. The red curve shows the fitted Gamma GLM; the y-axis is on a log scale.", out.width="60%", fig.height=4.5, fig.align="center"----
# Plotting the data with Gamma model overlay
plot(airquality$Wind, airquality$Ozone,
 log = "y",
 pch = 1, col = "black", xlab = "Wind (mph)", ylab = "Ozone (ppb) [log scale]",
 main = "Gamma GLM Fit (Log Link)"
)
# Generate predictions for the overlay
wind_seq <- seq(min(airquality$Wind), max(airquality$Wind), length.out = 100)
preds <- predict(model_gamma, newdata = data.frame(Wind = wind_seq), type = "response")
lines(wind_seq, preds, col = "red", lwd = 2)

legend("topright",
 legend = c("Data", "Gamma GLM (log link)"),
 col = c("black", "red"), pch = c(1, NA), lty = c(NA, 1), lwd = c(NA, 2)
)

## -----------------------------------------------------------------------------
# Extract standardised deviance residuals
std_dev_res <- rstandard(model_gamma, type = "deviance")
# Validate using the Shapiro-Wilk normality test
shapiro.test(std_dev_res)
# Validate using the Anderson-Darling normality test
nortest::ad.test(std_dev_res)

## ----chisq-example, fig.cap="Pearson's $\\chi^2$ test applied to the `HairEyeColor` dataset. Grouped bar chart of eye colour by hair colour and mosaic plot with tiles coloured by Pearson residuals (blue: over-represented, red: under-represented).", out.width="48%", fig.height=4.5, fig.show="hold"----
hair_eye_df <- counts_to_cases(as.data.frame(HairEyeColor))
visstat(hair_eye_df$Eye, hair_eye_df$Hair)

## ----yates-example, fig.show='hide', results='hide'---------------------------
hair_bb_eyes_bb <- HairEyeColor[1:2, 1:2, ]
hair_bb_eyes_bb_df <- counts_to_cases(
 as.data.frame(hair_bb_eyes_bb)
)
yates_stats <- visstat(
 hair_bb_eyes_bb_df$Eye,
 hair_bb_eyes_bb_df$Hair
)

## ----yates-effect-size--------------------------------------------------------
yates_stats$effect_size

## ----fisher-example, fig.show='hide', results='hide'--------------------------
hair_eye_male <- HairEyeColor[, , 1]
black_brown_hazel_green <- hair_eye_male[1:2, 3:4]
black_brown_hazel_green_df <- counts_to_cases(
 as.data.frame(black_brown_hazel_green)
)
fisher_stats <- visstat(
 black_brown_hazel_green_df$Eye,
 black_brown_hazel_green_df$Hair
)

## ----yates-fisher-example, echo=FALSE, fig.cap="Two $2 \\times 2$ categorical routes in `HairEyeColor`: Yates-corrected Pearson $\\chi^2$ when Cochran's rule is satisfied (black/brown hair and brown/blue eyes; left), and Fisher's exact test when expected counts are too small (male participants, black/brown hair, hazel/green eyes; right). The Yates-corrected plot shows row percentages; the Fisher plot shows absolute counts.", out.width="48%", fig.height=4.5, fig.show="hold", results='hide'----
plot(yates_stats, which = 1)
plot(fisher_stats, which = 1)

## ----kendall-spearman-example, fig.cap="Rank-based correlations: Left: Kendall's $\\tau_b$ for a hypothetical survey ($n = 150$): alcohol consumption frequency vs.\\ academic performance. Right: Spearman rank correlation of `Wind` and `Ozone` from the `airquality` dataset (`correlation = TRUE`; right). Both plots annotate the corresponding effect measure and $p$\\ value.", out.width="48%", fig.height=4.5, fig.show="hold", fig.crop=FALSE----
set.seed(42)
n <- 150
xs <- sample(1:5, n, replace = TRUE)
ys <- pmin(5, pmax(1, (6 - xs) + sample(-1:1, n, replace = TRUE)))
likert_alc <- c("never", "rarely", "sometimes", "often", "always")
likert_perf <- c("poor", "fair", "ok", "good", "great")
alcohol <- ordered(likert_alc[xs], levels = likert_alc)
performance <- ordered(likert_perf[ys], levels = likert_perf)
kendall_result <- visstat(performance, alcohol, correlation = TRUE)
spearman_air <- visstat(airquality$Wind, airquality$Ozone, correlation = TRUE)

## ----simulation-captions, include=FALSE---------------------------------------
cap_route1_identical_typeI <- paste(
 "Route 1 Type I simulation under identical distributions and identical",
 "means, with group mean 0 and SD = 1 in all four groups.",
 "(A) input distributions, dashed lines mark means and dotted lines mark medians.",
 "(B) balanced design with group sizes, listed from top to bottom, as 10, 20, 50, 100. ",
 "(C) Unbalanced design with group sizes $\\bar{n} \\cdot ",
 "(0.5, 0.8, 1.2, 1.5)$ with the target mean group size for unbalanced designs $\\bar{n} \\in \\{10, 20, 50, 100\\}$ ",
 "rounded up to the next integer. The heatmaps in",
 "(B) and (C) report final-test rejection rates at $\\alpha = 5\\%$.",
 "All heatmap numbers are percentages; the first value is the",
 "final-test rejection rate, and gated rows additionally list route splits after |."
)

cap_route1_unequal_typeI <- paste(
 "Route 1 equal-means simulation with varied group SD",
 "and sample-size pairings.",
 "(A) input distributions",
 "(B) balanced design with group sizes, listed from top to bottom,  as 10, 20, 50, 100.",
 "(C) unbalanced design with larger groups paired with larger SD.",
 "(D) unbalanced design with larger groups paired with smaller SD."
)

## ----route1-identical-typeI, echo=FALSE, fig.cap=cap_route1_identical_typeI, out.width="95%"----
knitr::include_graphics(
 "figures/route1_identical_distributions_typeI_with_kw_fleishman_B50000.png"
)

## ----route1-unequal-typeI, echo=FALSE, fig.cap=cap_route1_unequal_typeI, out.width="94%"----
knitr::include_graphics(
 "figures/route1_equal_means_unequal_distributions_fleishman_B50000.png"
)

## ----route1-power, echo=FALSE, fig.height=7.8, fig.cap="Route 1 power simulation with Fleishman input distributions. (A) Input distributions with group mean and median reference lines. (B) Simulated rejection rates for the six testing strategies."----
knitr::include_graphics(
 "figures/fleishman_4groups_power.png"
)

## ----effect-size-table, echo=FALSE, results='asis'----------------------------
if (knitr::is_latex_output()) {
 cat(r"(
\begin{table}[!htbp]
\caption{Effect sizes returned by \texttt{effect\_size()}.}
\label{tab:effect-size-formulae}
\centering
\begingroup
\scriptsize
\setlength{\tabcolsep}{2pt}
\setlength{\arrayrulewidth}{0.2pt}
\renewcommand{\arraystretch}{1.18}
\newcommand{\tbdoi}[2]{\href{https://doi.org/#1}{#2}}
\begin{tabular}{@{}%
>{\raggedright\arraybackslash}p{0.23\textwidth}%
>{\raggedright\arraybackslash}p{0.17\textwidth}%
>{\raggedright\arraybackslash}p{0.37\textwidth}%
>{\raggedright\arraybackslash}p{0.19\textwidth}@{}}
\hline
\textbf{Analysis} & \textbf{Effect size} & \textbf{Formula} &
\textbf{Source} \\
\hline
\hyperref[sec:tt]{Student's $t$-test} &
Hedges' $g_{s_p}$ (pooled) & $g_{s_p}=J(N-2)\cdot(\bar{x}_1-\bar{x}_2)/s_p$ &
\tbdoi{10.3102/10769986006002107}{Hedges 1981} \\
\hline
\hyperref[sec:welch-tt]{Welch's $t$-test} &
Hedges' $g_{s^{*}}$ (non-pooled) & $g_{s^{*}}=J(\nu^{*})\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$ &
\tbdoi{10.31234/osf.io/tu6mp}{Delacre et al. 2021} \\
\hline
\hyperref[sec:wilc]{Wilcoxon rank-sum} &
rank-biserial $r$ & $r=2\cdot W/(n_1\cdot n_2)-1$ &
\tbdoi{10.2466/11.IT.3.1}{Kerby 2014} \\
\hline
\hyperref[sec:fisher-aov]{Fisher's ANOVA} & $\omega^2$ &
$\nu_1\cdot(F-1)/(\nu_1\cdot F+\nu_2+1)$ &
\tbdoi{10.1016/j.jesp.2017.09.004}{Albers and Lakens 2018, Appendix A} \\
\hline
\hyperref[sec:welch-aov]{Welch's ANOVA} &
$\omega^2$ (approx.) &
$\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W+\nu_2+1)$ &
\tbdoi{10.1016/j.jesp.2017.09.004}{F-form from Albers and Lakens 2018, Appendix A} \\
\hline
\hyperref[sec:kw]{Kruskal--Wallis} &
$\eta_H^2$ & $(H-k+1)/(N-k)$ &
\tbdoi{10.1073/pnas.21.9.554}{Kelley 1935} \\
\hline
\hyperref[sec:lin-reg]{Simple linear regression} &
$R^2$ &
$R^2=1-SS_\text{res}/SS_\text{tot}$ &
\texttt{summary(lm())\$r.squared} \\
\hline
\hyperref[sec:rho]{Spearman} &
$\rho$ &
$\rho=r(\operatorname{rank}(x),\operatorname{rank}(y))$ &
\texttt{cor.test(method = "spearman")\$estimate} \\
\hline
\hyperref[sec:tau]{Kendall} &
$\tau_b$ &
$\tau_b=(n_c-n_d)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$ &
\texttt{cor.test(method = "kendall")\$estimate} \\
\hline
\hyperref[sec:fisher-exact]{Pearson $\chi^2$ ($R\times C$)} &
Cramér's $V$ &
$V_{R\times C}=\sqrt{\chi^2/\left(N\cdot(\min(R,C)-1)\right)}$ &
\tbdoi{10.4324/9780203771587}{Cohen 2013, p. 223} \\
\hline
\hyperref[sec:fisher-exact]{Pearson $\chi^2$ ($2\times2$)} &
$\phi$ & $\phi=\sqrt{\chi^2/N}$ &
\tbdoi{10.4324/9780203771587}{Cohen 2013, p. 223} \\
\hline
\hyperref[sec:fisher-exact]{Fisher's exact ($2\times2$)} &
conditional odds ratio &
\(\hat\theta_{\mathrm{cond}}\) &
\texttt{fisher.test()\$estimate} \\
\hline
\end{tabular}
\endgroup
\end{table}
)")
} else {
 cat(r"(
<table id="tab:effect-size-formulae">
<caption>Effect sizes returned by <code>effect_size()</code>.</caption>
<tr>
<th>Analysis</th>
<th>Effect size</th>
<th>Formula</th>
<th>Source</th>
</tr>
<tr>
<td><a href="#sec:tt">Student's $t$-test</a></td>
<td>Hedges' $g_{s_p}$ (pooled)</td>
<td>$g_{s_p}=J(N-2)\cdot(\bar{x}_1-\bar{x}_2)/s_p$</td>
<td><a href="https://doi.org/10.3102/10769986006002107">Hedges 1981</a></td>
</tr>
<tr>
<td><a href="#sec:welch-tt">Welch's $t$-test</a></td>
<td>Hedges' $g_{s^{*}}$ (non-pooled)</td>
<td>$g_{s^{*}}=J(\nu^{*})\cdot(\bar{x}_1-\bar{x}_2)/s^{*}$</td>
<td><a href="https://doi.org/10.31234/osf.io/tu6mp">Delacre et al. 2021</a></td>
</tr>
<tr>
<td><a href="#sec:wilc">Wilcoxon rank-sum</a></td>
<td>rank-biserial $r$</td>
<td>$r=2\cdot W/(n_1\cdot n_2)-1$</td>
<td><a href="https://doi.org/10.2466/11.IT.3.1">Kerby 2014</a></td>
</tr>
<tr>
<td><a href="#sec:fisher-aov">Fisher's ANOVA</a></td>
<td>$\omega^2$</td>
<td>$\nu_1\cdot(F-1)/(\nu_1\cdot F+\nu_2+1)$</td>
<td><a href="https://doi.org/10.1016/j.jesp.2017.09.004">Albers and Lakens 2018, Appendix A</a></td>
</tr>
<tr>
<td><a href="#sec:welch-aov">Welch's ANOVA</a></td>
<td>$\omega^2$ (approx.)</td>
<td>$\nu_1\cdot(F_W-1)/(\nu_1\cdot F_W+\nu_2+1)$</td>
<td><a href="https://doi.org/10.1016/j.jesp.2017.09.004">F-form from Albers and
Lakens 2018, Appendix A</a></td>
</tr>
<tr>
<td><a href="#sec:kw">Kruskal--Wallis</a></td>
<td>$\eta_H^2$</td>
<td>$(H-k+1)/(N-k)$</td>
<td><a href="https://doi.org/10.1073/pnas.21.9.554">Kelley 1935</a></td>
</tr>
<tr>
<td><a href="#sec:lin-reg">Simple linear regression</a></td>
<td>$R^2$</td>
<td>$R^2=1-SS_\text{res}/SS_\text{tot}$</td>
<td><code>summary(lm())&#36;r.squared</code></td>
</tr>
<tr>
<td><a href="#sec:rho">Spearman</a></td>
<td>$\rho$</td>
<td>$\rho=r(\operatorname{rank}(x),\operatorname{rank}(y))$</td>
<td><code>cor.test(method = "spearman")&#36;estimate</code></td>
</tr>
<tr>
<td><a href="#sec:tau">Kendall</a></td>
<td>$\tau_b$</td>
<td>$\tau_b=(n_c-n_d)/\sqrt{\left(n_0-n_1\right)\left(n_0-n_2\right)}$</td>
<td><code>cor.test(method = "kendall")&#36;estimate</code></td>
</tr>
<tr>
<td><a href="#sec:fisher-exact">Pearson $\chi^2$ ($R\times C$)</a></td>
<td>Cramér's $V$</td>
<td>$V_{R\times C}=\sqrt{\chi^2/\left(N\cdot(\min(R,C)-1)\right)}$</td>
<td><a href="https://doi.org/10.4324/9780203771587">Cohen 2013, p. 223</a></td>
</tr>
<tr>
<td><a href="#sec:fisher-exact">Pearson $\chi^2$ ($2\times 2$)</a></td>
<td>$\phi$</td>
<td>$\phi=\sqrt{\chi^2/N}$</td>
<td><a href="https://doi.org/10.4324/9780203771587">Cohen 2013, p. 223</a></td>
</tr>
<tr>
<td><a href="#sec:fisher-exact">Fisher's exact ($2\times 2$)</a></td>
<td>conditional odds ratio</td>
<td>$\hat\theta_{\mathrm{cond}}$</td>
<td><code>fisher.test()&#36;estimate</code></td>
</tr>
</table>
)")
}

