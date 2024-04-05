# R stats script

library(dplyr)
library(broom)
library(ggplot2)
library(hrbrthemes)
library(viridis)

# Independent samples t-test ####

#Test assumptions
# 1. dependent variable is continuous
# 2. independent variable contains 2 categorical independent groups
# 3. observations are independent e.g. no relationship between observations within the group and between the groups themselves
# 4. no significant outliers in the data
# box plot to visualise
xyzxyz

# 5. dependent variable data (&residuals) is (approximately) normally distributed for each group
#for both tests - if p>0.05 data is normally distributed, use parametric test. if p<0.05, use non-parametric test.
ggplot(ghaem_df, aes(x = ghaem_df$pyruvate)) +
  geom_histogram(fill = "salmon2", colour = "black") +
  facet_grid(Group ~ ., scales = "free")

# if n<2000 - Shapiro Wilk
sw_normality <- ghaem_df %>%
  group_by(Group) %>%
  do(tidy(shapiro.test(ghaem_df$age)))

#if n>2000 - Kolmogorov-Smirnov
ks_normality <- ghaem_df %>%
  group_by(Group) %>%
  do(tidy(ks.test(ghaem_df$age, "pnorm", 1,2)))

# 6. homogeneity of variance - variance for each group is the same
hom_o_var <- var.test(ghaem_df$age ~ ghaem_df$Group, ghaem_df)

# run t-test (if above assumptions are met)
age_ttest <- t.test (ghaem_df$age ~ ghaem_df$Group, var.equal=TRUE, data = dataframe)

# Mann-whitney (aka Wilcoxon rank sum/unpaired 2-sample wilcoxon) ####
# if non-parametric test
age_mw <- wilcox.test(age ~ Group, data = ghaem_df, exact = FALSE)
