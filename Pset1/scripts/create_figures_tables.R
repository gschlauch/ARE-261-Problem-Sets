# ------------------------------------------------------------------------------
# ARE 261, Reed's Half - Problem Set 1
# ------------------------------------------------------------------------------

# Initialize settings ----------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = F)
options(scipen = 999)
dirpath <- "/Users/garyschlauch/Documents/github/ARE-261-Problem-Sets/Pset1"
dirpath_data <- paste0(dirpath, "/data")
dirpath_output <- paste0(dirpath, "/output")

# Load packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(xtable)

# Define functions -------------------------------------------------------------

format_number <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), x)
}

format_reg_varname <- function(x) {
  x <- str_replace_all(x, "c\\.", "")
  x <- str_replace_all(x, "o\\.", "")
  x <- str_replace_all(x, "#", " $\\\\times$ ")
  return(x)
}

clean_reg_results <- function(df, ndigits, colnum) {
  colnum <- as.character(colnum)
  df <- df %>%
    dplyr::select(var, depvar, coef, stderr, pval, ci_lower, ci_upper, N, r2) %>%
    mutate_at(vars("coef", "stderr"), ~ format_number(., ndigits)) %>%
    mutate(
      var = format_reg_varname(var),
      var = ifelse(var == "_cons", "constant", var),
      N = as.character(N),
      r2 = format_number(r2, 3)
    ) %>%
    rename_at(vars(-var), ~ paste0(.x, "_", colnum))
  return(df)
}

move_reg_constant <- function(df) {
  df <- df %>%
    mutate(
      rownum = row_number(),
      sorter = ifelse(var == "constant", 1, 0)
    ) %>%
    arrange(sorter, rownum) %>%
    dplyr::select(-c(rownum, sorter))
  return(df)
}

fill_na_with_non_na <- function(x) {
  ifelse(is.na(x), na.omit(x), x)
}

make_reg_table <- function(
    reg_results_df,
    ncol,
    title,
    caption,
    label,
    save_file,
    own_file = F) {
  # Open file
  file_con <- file(save_file, "w")

  # Preamble
  if (own_file == T) {
    writeLines("\\documentclass[12pt]{article}", file_con)
    writeLines("\\usepackage{amsmath}", file_con)
    writeLines("\\usepackage{tabularx}", file_con)
    writeLines("\\usepackage{booktabs}", file_con)
    writeLines("\\begin{document}", file_con)
    writeLines("\\pagenumbering{gobble}", file_con)
    writeLines("", file_con)
  }

  cat("", file = file_con)
  writeLines("\\begin{table}[ht]", file_con)
  writeLines(paste0("\\caption{", title, "}"), file_con)
  writeLines("\\centering", file_con)
  writeLines("\\normalsize", file_con)
  col_format <- paste0("l", paste0(rep("c", ncol), collapse = ""))
  writeLines(paste0("\\begin{tabular}{", col_format, "}"), file_con)
  writeLines("\\toprule", file_con)
  writeLines("\\centering", file_con)

  # Column numbers
  col_nums <- ""
  for (i in 1:ncol) {
    col_nums <- paste0(col_nums, " & (", i, ")")
  }
  col_nums <- paste0(col_nums, " \\\\")
  writeLines(col_nums, file_con)
  
  # Column names
  col_names <- ""
  for (i in 1:ncol) {
    varname <- paste0("depvar_", as.character(i))
    col_names <- paste0(col_names, " & ", reg_results_df[[varname]][1])
  }
  col_names <- str_replace_all(col_names, "_", "\\\\_")
  col_names <- paste0(col_names, " \\\\")
  writeLines(col_names, file_con)
  
  # Start body of table
  writeLines("\\midrule", file_con)

  # Write the coefficients and standard errors
  for (i in 1:nrow(reg_results_df)) { # Loop through rows (variable names)
    coefficients <- paste0(reg_results_df$var[i])
    std_errors <- ""
    for (j in 1:ncol) { # Loop through columns
      # Get the coefficients
      varname <- paste0("coef_", as.character(j))
      value <- reg_results_df[[varname]][i]
      if (!is.na(value)) {
        coefficients <- paste0(coefficients, " & ", value)
      } else {
        coefficients <- paste0(coefficients, " & ")
      }
      if (j == ncol) {
        coefficients <- paste0(coefficients, " \\\\")
      }
      # Get the standard errors
      varname <- paste0("stderr_", as.character(j))
      value <- reg_results_df[[varname]][i]
      if (!is.na(value)) {
        std_errors <- paste0(std_errors, " & ", "(", value, ")")
      } else {
        std_errors <- paste0(std_errors, " & ")
      }
      if (j == ncol) {
        std_errors <- paste0(std_errors, " \\\\")
      }
    }
    writeLines(coefficients, file_con)
    writeLines(std_errors, file_con)
  }
  writeLines("\\midrule", file_con)

  # Write the number of observations
  nobs <- "N"
  for (j in 1:ncol) { # Loop through columns
    varname <- paste0("N_", as.character(j))
    value <- reg_results_df[[varname]][1]
    nobs <- paste0(nobs, " & ", value)
    if (j == ncol) {
      nobs <- paste0(nobs, " \\\\")
    }
  }
  writeLines(nobs, file_con)

  # Write the R-squared
  rsquared <- "$R^2$"
  for (j in 1:ncol) { # Loop through columns
    varname <- paste0("r2_", as.character(j))
    value <- reg_results_df[[varname]][1]
    rsquared <- paste0(rsquared, " & ", value)
    if (j == ncol) {
      rsquared <- paste0(rsquared, " \\\\")
    }
  }
  writeLines(rsquared, file_con)

  # Ending
  writeLines("\\bottomrule", file_con)
  writeLines("\\end{tabular}", file_con)
  writeLines(paste0("\\caption*{", caption, "}"), file_con)
  writeLines(paste0("\\label{", label, "}"), file_con)
  writeLines("\\end{table}", file_con)
  if (own_file == T) {
    writeLines("", file_con)
    writeLines("\\end{document}", file_con)
  }
  close(file_con)
}


# ------------------------------------------------------------------------------
# 1.2 Climate Impacts
# ------------------------------------------------------------------------------

# 1 ----------------------------------------------------------------------------
reg_results <- read_csv(paste0(dirpath_data, "/intermediate/reg_climate_impacts_1.csv")) %>%
  filter(var != "_cons")
labels <- c(
  "<0°C", "0-4°C", "4-8°C", "8-12°C", "12-16°C", "16-20°C", "20-24°C",
  "24-28°C", "28-32°C", ">32°C"
)
fig <- ggplot(data = reg_results, aes(x = fct_inorder(var))) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  geom_line(aes(y = coef, group = 1)) +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper, group = 1),
    alpha = 0.2,
    linewidth = 0.01
  ) +
  scale_x_discrete(labels = labels) +
  labs(
    y = "Log farm employment", x = ""
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.25)
  )
file_out <- paste0(dirpath_output, "/figures/Climate_Impacts_Fig1.png")
ggsave(file_out)

# 2 ----------------------------------------------------------------------------
reg_results <- read_csv(paste0(dirpath_data, "/intermediate/reg_climate_impacts_2.csv")) %>%
  filter(var != "_cons")
labels <- c("0-8°C", "8-16°C", "16-24°C", "24-32°C")
ggplot(data = reg_results, aes(x = fct_inorder(var))) +
  geom_hline(aes(yintercept = 0), color = "red", linewidth = 0.25) +
  geom_line(aes(y = coef, group = 1)) +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper, group = 1),
    alpha = 0.2,
    linewidth = 0.01
  ) +
  scale_x_discrete(labels = labels) +
  labs(
    y = "Log per capita farm prop income", x = ""
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.25)
  )
file_out <- paste0(dirpath_output, "/figures/Climate_Impacts_Fig2.png")
ggsave(file_out)

# 3 ----------------------------------------------------------------------------
reg_results_df <- read_csv(paste0(dirpath_data, "/intermediate/reg_climate_impacts_3.csv")) %>%
  clean_reg_results(., 6, 1) %>%
  mutate_at(vars(coef_1, stderr_1), ~ ifelse(var == "temp16to20", NA, .))

make_reg_table(
  reg_results_df,
  ncol = 1,
  title = "Exercise 1.2.3",
  caption = "",
  label = "table_climate_impacts_3",
  save_file = paste0(dirpath_output, "/tables/reg_table_climate_impacts_3.tex"),
  own_file = F
)




# ------------------------------------------------------------------------------
# 2 Hedonic Air Quality Analysis
# ------------------------------------------------------------------------------

# 1 ----------------------------------------------------------------------------
df1 <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_1_1.csv")) %>%
  clean_reg_results(., 5, 1)

df2 <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_1_2.csv")) %>%
  clean_reg_results(., 5, 2)

reg_results_df <- full_join(df1, df2, by = c("var")) %>%
  move_reg_constant() %>%
  mutate_at(vars(depvar_1, depvar_2), ~ fill_na_with_non_na(.))

make_reg_table(
  reg_results_df,
  ncol = 2,
  title = "Exercise 2.1",
  caption = "",
  label = "hedonic_1_1",
  save_file = paste0(dirpath_output, "/tables/hedonic_1_1.tex"),
  own_file = F
)

df3 <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_1_3.csv")) %>%
  clean_reg_results(., 4, 1)

make_reg_table(
  df3,
  ncol = 1,
  title = "Hedonic analysis - Question 1",
  caption = "",
  label = "hedonic_1_2",
  save_file = paste0(dirpath_output, "/tables/hedonic_1_2.tex"),
  own_file = F
)

# 2 ----------------------------------------------------------------------------
df1 <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_2_1.csv")) %>%
  clean_reg_results(., 5, 1)

df2 <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_2_2.csv")) %>%
  clean_reg_results(., 5, 2)

reg_results_df <- full_join(df1, df2, by = c("var")) %>%
  move_reg_constant() %>%
  mutate_at(vars(N_2, r2_2, depvar_1, depvar_2), ~ fill_na_with_non_na(.))

make_reg_table(
  reg_results_df,
  ncol = 2,
  title = "Exercise 2.2",
  caption = "",
  label = "hedonic_2",
  save_file = paste0(dirpath_output, "/tables/hedonic_2.tex"),
  own_file = F
)

# 3 ----------------------------------------------------------------------------
reg_results_df <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_3_1.csv")) %>%
  clean_reg_results(., 5, 1)
for (i in 2:6) {
  df <- read_csv(
    paste0(dirpath_data, "/intermediate/reg_hedonic_3_", as.character(i), ".csv")
  ) %>%
    clean_reg_results(., 5, i)
  reg_results_df <- full_join(reg_results_df, df, by = c("var"))
}
varlist <- names(reg_results_df)[str_detect(names(reg_results_df), "^(depvar|N|r2)")]
reg_results_df <- reg_results_df %>%
  move_reg_constant() %>%
  mutate_at(vars(varlist), ~ fill_na_with_non_na(.))

make_reg_table(
  reg_results_df,
  ncol = 6,
  title = "Exercise 2.3",
  caption = "",
  label = "hedonic_3",
  save_file = paste0(dirpath_output, "/tables/hedonic_3.tex"),
  own_file = F
)

# 4 ----------------------------------------------------------------------------
reg_results_df <- read_csv(paste0(dirpath_data, "/intermediate/reg_hedonic_4_1.csv")) %>%
  clean_reg_results(., 5, 1)
for (i in 2:6) {
  df <- read_csv(
    paste0(dirpath_data, "/intermediate/reg_hedonic_4_", as.character(i), ".csv")
  ) %>%
    clean_reg_results(., 5, i)
  reg_results_df <- full_join(reg_results_df, df, by = c("var"))
}
varlist <- names(reg_results_df)[str_detect(names(reg_results_df), "^(depvar|N|r2)")]
reg_results_df <- reg_results_df %>%
  move_reg_constant() %>%
  mutate_at(vars(varlist), ~ fill_na_with_non_na(.))

make_reg_table(
  reg_results_df,
  ncol = 6,
  title = "Exercise 2.4",
  caption = "",
  label = "hedonic_4",
  save_file = paste0(dirpath_output, "/tables/hedonic_4.tex"),
  own_file = F
)

# 5 ----------------------------------------------------------------------------
df5 <- read_csv(paste0(dirpath_data, "/intermediate/lowess_hedonic_5.csv")) %>%
  mutate(tsp7576 = as.character(tsp7576))

varlist <- c("dlhouse", "dgtsp")
bwidths <- as.character(2:4)

for (var in varlist) {
  if (var == "dlhouse") {
    yaxis_title <- "Change in log housing values"
  } else{
    yaxis_title <- "Change in annual mean TSPs"
  }
  for (bwidth in bwidths) {
    plt <- ggplot() +
      geom_point(
        data = df5,
        aes(x = mtspgm74, y = .data[[paste0(var, "_bwidth", bwidth)]], color = tsp7576),
        size = 0.2
      ) +
      scale_color_manual(
        values = c("0" = "blue", "1" = "red"),
        labels = c("Unregulated", "Regulated")
      ) +
      geom_vline(aes(xintercept = 75), linewidth = 0.3, color = "black") +
      labs(
        x = "TSPs level in 1974",
        y = yaxis_title,
        title = paste0("Bandwidth = ", bwidth)
      ) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(color = "gray", linewidth = 0.25),
        legend.title=element_blank(),
        legend.text = element_text(size = 10)
      ) +
      guides(color = guide_legend(override.aes = list(size = 1.5)))
    file_out <- paste0(dirpath_output, "/figures/lowess_5_", var, "_bwidth", bwidth, ".png")
    ggsave(file_out)
  }
}

# 6 ----------------------------------------------------------------------------
df5 <- df5 %>% mutate(type = "actual")
df6 <- read_csv(paste0(dirpath_data, "/intermediate/lowess_hedonic_6.csv")) %>%
  mutate(
    tsp7576 = as.character(tsp7576),
    type = "index"
  )

df5_6 <- bind_rows(df5, df6) %>%
  mutate(
    var_bwidth2 = ifelse(type == "actual", dlhouse_bwidth2, index_bwidth2),
    var_bwidth3 = ifelse(type == "actual", dlhouse_bwidth3, index_bwidth3),
    var_bwidth4 = ifelse(type == "actual", dlhouse_bwidth4, index_bwidth4),
    description = 
      ifelse(tsp7576 == 1 & type == "actual", "Regulated: actual",
      ifelse(tsp7576 == 1 & type == "index", "Regulated: index",
      ifelse(tsp7576 == 0 & type == "actual", "Unregulated: actual",
      ifelse(tsp7576 == 0 & type == "index", "Unregulated: index",
             NA))))
    ) %>%
  dplyr::select(mtspgm74, starts_with("var_"), tsp7576, type, description)

bwidths <- as.character(2:4)

for (bwidth in bwidths) {
  plt <- ggplot() +
    geom_point(
      data = df5_6,
      aes(
        x = mtspgm74, 
        y = .data[[paste0("var_bwidth", bwidth)]], 
        color = description
        ),
      size = 0.2
    ) +
    scale_shape_manual(values = c("actual" = 19, "index" = 4)) +
    geom_vline(aes(xintercept = 75), linewidth = 0.3, color = "black") +
    labs(
      x = "TSPs level in 1974",
      y = "Predicted change in log housing values",
      title = paste0("Bandwidth = ", bwidth)
    ) +
    theme_classic() +
    theme(
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.25),
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 1.5))
    )
  file_out <- paste0(dirpath_output, "/figures/lowess_6_bwidth", bwidth, ".png")
  ggsave(file_out)
}


# 7 ----------------------------------------------------------------------------
df7 <- read_csv(paste0(dirpath_data, "/intermediate/lowess_hedonic_7.csv")) %>%
  filter(mtspgm74_50to75 == 1) %>%
  mutate(tsp7576 = as.character(tsp7576))

varlist <- c("dlhouse", "dgtsp")
bwidths <- as.character(7:9)

for (var in varlist) {
  if (var == "dlhouse") {
    yaxis_title <- "Change in log housing values"
  } else{
    yaxis_title <- "Change in annual mean TSPs"
  }
  for (bwidth in bwidths) {
    plt <- ggplot() +
      geom_point(
        data = df7,
        aes(x = mtspgm74, y = .data[[paste0(var, "_50to75_bwidth", bwidth)]], color = tsp7576),
        size = 0.2
      ) +
      scale_color_manual(
        values = c("0" = "blue", "1" = "red"),
        labels = c("Unregulated", "Regulated")
      ) +
      geom_vline(aes(xintercept = 75), linewidth = 0.3, color = "black") +
      labs(
        x = "TSPs level in 1974",
        y = yaxis_title,
        title = paste0("Bandwidth = ", bwidth)
      ) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(color = "gray", linewidth = 0.25),
        legend.title=element_blank(),
        legend.text = element_text(size = 10)
      ) +
      guides(color = guide_legend(override.aes = list(size = 1.5)))
    file_out <- paste0(dirpath_output, "/figures/lowess_7_", var, "_bwidth", bwidth, ".png")
    ggsave(file_out)
  }
}

