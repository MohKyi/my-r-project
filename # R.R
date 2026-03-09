# R
# - remove rows with NA in age/low
# - compute Pearson correlation (r and p)
# - build ggplot scatter + lm smoother and annotate correlation
library(MASS)
lbw <- birthwt  # load the dataset (avoid using 'df' — it clashes with the built-in df() function)

lbw2 <- na.omit(lbw[c("age", "low")])
res <- cor.test(lbw2$age, lbw2$low, method = "pearson")
r_val <- signif(res$estimate, 3)
p_val <- format.pval(res$p.value, digits = 2)
label <- paste0("r = ", r_val, ", p = ", p_val)

library(ggplot2)

# Okabe-Ito color-blind friendly colors
cb_point  <- "#0072B2"   # blue for points
cb_smooth <- "#D55E00"   # vermillion for the lm line

plot <- ggplot(lbw2, aes(x = age, y = low)) +
  geom_point(alpha = 0.6, color = cb_point) +
  geom_smooth(method = "lm", se = TRUE, color = cb_smooth, fill = "#56B4E9") +
  annotate("text",
           x = max(lbw2$age, na.rm = TRUE),
           y = max(lbw2$low, na.rm = TRUE),
           label = label,
           hjust = 1, vjust = 1) +
  labs(x = "Age", y = "Low", title = "Age vs Low (Pearson correlation)") +
  theme_minimal()

plot# R
library(ggplot2)
library(viridis)

# Okabe-Ito (color-blind friendly) for discrete palettes
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Example 1: discrete (use scale_fill_manual or scale_color_manual)
p_discrete <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = okabe_ito) +
  labs(x = "Cylinders", fill = "Gear") +
  theme_minimal()

# Example 2: continuous (use viridis for perceptually-uniform, color-blind friendly)
p_continuous <- ggplot(mtcars, aes(wt, mpg, color = hp)) +
  geom_point(size = 3) 
  scale_color_viridis_c(option = "D") 
  labs(color = "Horsepower") +
  theme_minimal()

# Return both plot objects so you can inspect or print them
list(discrete = p_discrete, continuous = p_continuous)

