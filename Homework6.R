## Homework 6

conc1 <- rnorm(20, 3, 1)
conc2 <- rnorm(20, 5, 2)

conc_df <- data.frame(concentration = rep(c("1%", "2%"), each=20), rate = c(conc1, conc2))
print(conc_df)

conc_ANOVA <- aov(rate ~ concentration, data=conc_df)
summary(conc_ANOVA)


