# =============================================================================
# World Happiness Report (WHR) — Bivariate Statistical Analysis
# Data: 470 country-year observations across 2015, 2016, 2017
# Author: Public Health Data Analysis
# IDE: RStudio (Posit)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. SETUP — Install & Load Packages
# -----------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # data wrangling + ggplot2
  ggcorrplot,   # correlation heatmap
  ggpubr,       # annotated scatter plots (stat_cor)
  rstatix,      # tidy statistical tests
  patchwork,    # multi-panel plot layout
  scales,       # axis formatting
  viridis,      # colour-blind-safe palettes
  broom         # tidy model output
)

# -----------------------------------------------------------------------------
# 1. DATA IMPORT & CLEANING
# -----------------------------------------------------------------------------
whr <- read_csv("whr.csv", show_col_types = FALSE) %>%
  rename(
    gdp          = gdp_pc,
    social       = family,
    life_exp     = health,
    freedom      = freedom,
    trust        = trust_gov_corr,
    generosity   = generosity,
    dystopia     = dystopia_res,
    score        = happy_score,
    rank         = happy_rank
  ) %>%
  mutate(
    year   = factor(year),
    region = factor(region)
  )

glimpse(whr)
summary(whr)

# Numeric predictors of interest
predictors <- c("gdp", "social", "life_exp", "freedom", "trust", "generosity")

# -----------------------------------------------------------------------------
# 2. CORRELATION MATRIX  (Pearson r)
# -----------------------------------------------------------------------------
cor_data <- whr %>%
  select(score, all_of(predictors)) %>%
  drop_na()

cor_mat  <- cor(cor_data, method = "pearson")
cor_pmat <- cor_pmat(cor_data)            # p-value matrix (rstatix)

p_corr <- ggcorrplot(
  cor_mat,
  hc.order  = TRUE,
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3.5,
  p.mat     = cor_pmat,
  sig.level = 0.05,
  insig     = "blank",
  colors    = c("#D55E00", "white", "#0072B2"),
  title     = "Pearson Correlation Matrix — WHR Variables",
  ggtheme   = theme_minimal(base_size = 12)
)
print(p_corr)

# -----------------------------------------------------------------------------
# 3. SCATTERPLOTS WITH REGRESSION LINES
#    Happiness Score ~ each predictor, coloured by Region
# -----------------------------------------------------------------------------
make_scatter <- function(xvar, xlabel) {
  ggplot(whr, aes(x = .data[[xvar]], y = score, colour = region)) +
    geom_point(alpha = 0.55, size = 1.8) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                colour = "black", linewidth = 0.9) +
    stat_cor(aes(group = 1), method = "pearson",
             label.x.npc = "left", label.y.npc = "top",
             size = 3.5, colour = "black") +
    scale_colour_viridis_d(option = "turbo", name = "Region") +
    labs(x = xlabel, y = "Happiness Score",
         title = paste("Happiness Score ~", xlabel)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 10))
}

scatter_list <- list(
  make_scatter("gdp",       "GDP per Capita"),
  make_scatter("social",    "Social Support"),
  make_scatter("life_exp",  "Healthy Life Expectancy"),
  make_scatter("freedom",   "Freedom to Make Life Choices"),
  make_scatter("trust",     "Trust in Government"),
  make_scatter("generosity","Generosity")
)

# Shared legend for region
legend_plot <- ggplot(whr, aes(x = gdp, y = score, colour = region)) +
  geom_point() +
  scale_colour_viridis_d(option = "turbo", name = "Region") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))
shared_legend <- cowplot::get_legend(legend_plot)

p_scatter_grid <- wrap_plots(scatter_list, ncol = 3) +
  plot_annotation(
    title    = "Bivariate Scatter Plots: Happiness Score vs. WHR Predictors",
    subtitle = "n = 470 country-year observations (2015–2017) | Black line = OLS fit with 95% CI | r = Pearson correlation",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10, colour = "grey40"))
  )
print(p_scatter_grid)

# -----------------------------------------------------------------------------
# 4. SIMPLE LINEAR REGRESSIONS — tidy summary table
# -----------------------------------------------------------------------------
slr_results <- map_dfr(predictors, function(xvar) {
  formula <- as.formula(paste("score ~", xvar))
  fit     <- lm(formula, data = whr)
  glance(fit) %>%
    select(r.squared, adj.r.squared, statistic, p.value) %>%
    mutate(
      predictor  = xvar,
      beta       = coef(fit)[2],
      beta_se    = summary(fit)$coefficients[2, 2],
      .before    = r.squared
    )
})

slr_results <- slr_results %>%
  arrange(desc(r.squared)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n===== Simple Linear Regression Summary =====\n")
print(slr_results)

# -----------------------------------------------------------------------------
# 5. REGION × HAPPINESS — One-Way ANOVA + Boxplot
# -----------------------------------------------------------------------------
aov_fit    <- aov(score ~ region, data = whr)
aov_tidy   <- tidy(aov_fit)
cat("\n===== One-Way ANOVA: Happiness Score ~ Region =====\n")
print(aov_tidy)

# Post-hoc Tukey HSD
tukey_res <- TukeyHSD(aov_fit)
tukey_df  <- as.data.frame(tukey_res$region) %>%
  rownames_to_column("comparison") %>%
  filter(`p adj` < 0.05) %>%
  arrange(`p adj`)
cat("\nTukey HSD — significant pairwise comparisons (p < 0.05):\n")
print(tukey_df)

# Boxplot by region
region_order <- whr %>%
  group_by(region) %>%
  summarise(med = median(score, na.rm = TRUE)) %>%
  arrange(desc(med)) %>%
  pull(region)

p_box <- whr %>%
  mutate(region = factor(region, levels = region_order)) %>%
  ggplot(aes(x = region, y = score, fill = region)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               outlier.alpha = 0.6, width = 0.55) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1.2, colour = "grey30") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(
    title    = "Happiness Score Distribution by World Region",
    subtitle = paste0("One-Way ANOVA: F = ", round(aov_tidy$statistic[1], 2),
                      ", p < 0.001"),
    x = NULL, y = "Happiness Score"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40"))
print(p_box)

# -----------------------------------------------------------------------------
# 6. GDP × LIFE EXPECTANCY interaction (bivariate bubble chart)
#    — bubble size = Happiness Score, faceted by year
# -----------------------------------------------------------------------------
p_bubble <- ggplot(whr,
  aes(x = gdp, y = life_exp, size = score, colour = region)) +
  geom_point(alpha = 0.65) +
  scale_size_continuous(range = c(1, 9), name = "Happiness\nScore") +
  scale_colour_viridis_d(option = "turbo", name = "Region") +
  facet_wrap(~ year) +
  labs(
    title    = "GDP per Capita × Healthy Life Expectancy",
    subtitle = "Bubble size = Happiness Score | Faceted by year",
    x = "GDP per Capita (contribution to score)",
    y = "Healthy Life Expectancy (contribution to score)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "bottom")
print(p_bubble)

# -----------------------------------------------------------------------------
# 7. YEAR-OVER-YEAR CHANGE — t-test (2015 vs 2017)
# -----------------------------------------------------------------------------
whr_15 <- whr %>% filter(year == "2015") %>% pull(score)
whr_17 <- whr %>% filter(year == "2017") %>% pull(score)

t_res <- t.test(whr_17, whr_15, alternative = "two.sided")
cat("\n===== Welch Two-Sample t-Test: 2015 vs 2017 Mean Happiness =====\n")
print(t_res)

# Density plot comparing years
p_density <- ggplot(whr, aes(x = score, fill = year, colour = year)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  scale_fill_manual(values   = c("2015" = "#E69F00",
                                 "2016" = "#56B4E9",
                                 "2017" = "#009E73")) +
  scale_colour_manual(values = c("2015" = "#E69F00",
                                 "2016" = "#56B4E9",
                                 "2017" = "#009E73")) +
  labs(
    title    = "Distribution of Happiness Scores by Year",
    subtitle = paste0("Welch t-test (2015 vs 2017): t = ",
                      round(t_res$statistic, 3),
                      ", p = ", round(t_res$p.value, 4)),
    x = "Happiness Score", y = "Density", fill = "Year", colour = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
print(p_density)

# -----------------------------------------------------------------------------
# 8. EXPORT ALL PLOTS
# -----------------------------------------------------------------------------
ggsave("01_correlation_matrix.png",  plot = p_corr,         width = 8,  height = 7,  dpi = 300)
ggsave("02_scatter_grid.png",        plot = p_scatter_grid, width = 14, height = 9,  dpi = 300)
ggsave("03_boxplot_region.png",      plot = p_box,          width = 10, height = 6,  dpi = 300)
ggsave("04_bubble_gdp_lifeexp.png",  plot = p_bubble,       width = 12, height = 5,  dpi = 300)
ggsave("05_density_by_year.png",     plot = p_density,      width = 8,  height = 5,  dpi = 300)

cat("\n All plots saved to working directory.\n")

# =============================================================================
# END OF SCRIPT
# =============================================================================
