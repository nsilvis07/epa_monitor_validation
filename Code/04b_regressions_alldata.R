##############################################################################
# FILE NAME: 04b_regressions_alldata
# AUTHOR: Zoe Mitchell 
# PURPOSE: This script runs regressions and creates regression tables for the
# EPA PM2.5 Monitors project (for all data)
# UPDATED: 07-25-2025
##############################################################################

####### Difference in Means Test and Output LaTeX Table ######

# ----- Difference in Means Test -----


# Prepare data
mean_data_alldata <- monitor_satellite_long_alldata %>%
  filter(Source %in% c("Original", "Updated")) %>%
  group_by(x, y, Year) %>%
  summarise(
    pm_orig = Monitor_PM2.5[Source == "Original"],
    pm_updt = Monitor_PM2.5[Source == "Updated"],
    .groups = "drop"
  ) %>%
  mutate(diff = pm_updt - pm_orig)

# Cluster variable
cluster_var <- interaction(mean_data_alldata$x, mean_data_alldata$y)

# Difference in means test
model_diff <- lm(diff ~ 1, data = mean_data_alldata)
robust_cov <- vcovCL(model_diff, cluster = cluster_var, type = "HC1")
robust_se_diff <- sqrt(robust_cov[1, 1])
diff_est <- coef(model_diff)[1]

# Calculate p value
t_stat <- diff_est / robust_se_diff
df <- length(unique(cluster_var)) - 1
pval <- 2 * pt(-abs(t_stat), df = df)

# Group means and SEs (non-clustered SEs) 
mean_orig <- mean(mean_data_alldata$pm_orig, na.rm = TRUE)
mean_updt <- mean(mean_data_alldata$pm_updt, na.rm = TRUE)
se_orig <- sd(mean_data_alldata$pm_orig, na.rm = TRUE) / sqrt(nrow(mean_data_alldata))
se_updt <- sd(mean_data_alldata$pm_updt, na.rm = TRUE) / sqrt(nrow(mean_data_alldata))

# Format values
fmt <- function(x) sprintf("%.3f", x)
mean_orig_fmt <- fmt(mean_orig)
mean_updt_fmt <- fmt(mean_updt)
diff_fmt <- fmt(diff_est)
se_orig_fmt <- sprintf("(%.3f)", se_orig)
se_updt_fmt <- sprintf("(%.3f)", se_updt)
se_diff_fmt <- sprintf("(%.3f)", robust_se_diff)

pval_fmt <- if (pval < 0.0001) {
  "p < 0.0001"
} else {
  sprintf("p = %.4f", pval)
}

stars <- function(p) {
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1) return("$^{*}$")
  return("")
}
diff_fmt_star <- paste0(diff_fmt, stars(pval))

# ------ Output LaTeX Table ------

# Set output location

output_file_mean_alldata <- file.path(output_dir, "means_table_alldata.tex")

# Output file

cat(sprintf('
\\begin{table}[!htbp] \\centering
  \\caption{Mean PM\\textsubscript{2.5} Before and After EPA Correction (All Monitors)}
  \\label{tab:means_alldata}
\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lccc}
\\\\[-1.8ex]\\hline
\\hline \\\\[-1.8ex]
 & Original & Updated & Difference (Updated $-$ Original) \\\\
 & (1) & (2) & (3) \\\\
\\hline \\\\[-1.8ex]
Mean PM\\textsubscript{2.5} & %s & %s & %s \\\\
 & %s & %s & %s \\\\
\\hline \\\\[-1.8ex]
\\multicolumn{4}{l}{\\textit{Difference-in-means test:} %s} \\\\
\\hline
\\hline \\\\[-1.8ex]
\\multicolumn{4}{p{\\textwidth}}{\\textit{Notes:} This table compares the average PM\\textsubscript{2.5} measured by monitors before and after the EPA correction, using data from all monitors. Column (3) shows the difference in means with robust standard errors clustered by monitor location. Significance levels: $^{*}p < 0.1$, $^{**}p < 0.05$, $^{***}p < 0.01$. Source: The EPA\\textquotesingle s Air Quality System.}
\\end{tabular*}
\\end{table}
',
            mean_orig_fmt, mean_updt_fmt, diff_fmt_star,
            se_orig_fmt, se_updt_fmt, se_diff_fmt, pval_fmt),
    file = output_file_mean_alldata)

message("LaTeX table written to: ", output_file_mean_alldata)


  
 ###### SUR Regression and Wald Test ######

# ------ Run Regression ------

# Prepare data
sur_data_impacted <- monitor_satellite_long_impacted %>%
  filter(Source %in% c("Original", "Updated")) %>%
  group_by(x, y, Year) %>%
  summarise(
    Satellite = first(Satellite),
    Monitor_PM2.5_orig = Monitor_PM2.5[Source == "Original"],
    Monitor_PM2.5_updt = Monitor_PM2.5[Source == "Updated"],
    .groups = "drop"
  ) %>%
  na.omit()

# Fit separate regressions with robust SEs
lm_orig <- lm(Monitor_PM2.5_orig ~ Satellite, data = sur_data_impacted)
lm_updt <- lm(Monitor_PM2.5_updt ~ Satellite, data = sur_data_impacted)

# Extract estimates and robust SEs
coef_orig <- coef(lm_orig)["Satellite"]
se_orig <- sqrt(diag(vcovHC(lm_orig, type = "HC1")))["Satellite"]
pval_orig <- coeftest(lm_orig, vcov. = vcovHC(lm_orig, type = "HC1"))["Satellite", "Pr(>|t|)"]

coef_updt <- coef(lm_updt)["Satellite"]
se_updt <- sqrt(diag(vcovHC(lm_updt, type = "HC1")))["Satellite"]
pval_updt <- coeftest(lm_updt, vcov. = vcovHC(lm_updt, type = "HC1"))["Satellite", "Pr(>|t|)"]

# Wald test for equality of slopes
slope_diff <- coef_orig - coef_updt
se_diff <- sqrt(se_orig^2 + se_updt^2)
z_stat <- slope_diff / se_diff
pval_diff <- 2 * (1 - pnorm(abs(z_stat)))


# ------ LaTeX table ------


# Format stars
stars <- function(p) {
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1)  return("$^{*}$")
  return("")
}

# Format LaTeX values
slope_orig_fmt <- sprintf("%.3f%s", coef_orig, stars(pval_orig))
slope_updt_fmt <- sprintf("%.3f%s", coef_updt, stars(pval_updt))
se_orig_fmt <- sprintf("(%.3f)", se_orig)
se_updt_fmt <- sprintf("(%.3f)", se_updt)
wald_fmt <- sprintf("Diff = %.3f%s, p = %.3f", slope_diff, stars(pval_diff), pval_diff)

# Set output location
output_file_regression_alldata <- file.path(output_dir, "regression_table_impacted.tex")

# Write and output LaTeX table
cat(sprintf("
\\begin{table}[!htbp] \\centering
  \\caption{Satellite-Monitor Regression Results (Impacted Monitors Only)} 
  \\label{tab:regression_updated} 
\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lcc}
\\\\[-1.8ex]\\hline 
\\hline \\\\[-1.8ex] 
\\multicolumn{3}{c}{\\textit{Dependent Variables:}} \\\\
[0.5ex]\\hline \\\\[-1.8ex] 
 & Original Monitor PM\\textsubscript{2.5} & Updated Monitor PM\\textsubscript{2.5} \\\\ 
 & (1) & (2)\\\\ 
\\hline \\\\[-1.8ex] 
Satellite PM\\textsubscript{2.5} & %s & %s \\\\
& %s & %s \\\\
\\hline \\\\[-1.8ex]
\\multicolumn{3}{l}{\\textit{Wald Test for Equal Slopes:} %s} \\\\
\\hline
\\hline \\\\[-1.8ex]
\\multicolumn{3}{p{\\textwidth}}{\\textit{Notes:} This table presents results from regressions of monitor PM\\textsubscript{2.5} on satellite PM\\textsubscript{2.5} for original versus updated data, using robust standard errors and a Wald test to evaluate slope differences. Sources: The Washington University in St. Louis\\textquotesingle s Atmospheric Composition Analysis Group and the EPA\\textquotesingle s Air Quality System.}
\\end{tabular*}
\\end{table}
", slope_orig_fmt, slope_updt_fmt, se_orig_fmt, se_updt_fmt, wald_fmt),
    file = output_file_regression_impacted)

# Print confirmation
message("LaTeX table written to: ", output_file_regression_alldata)

