## Homework 6

conc1 <- rnorm(20, 1.1, 0.3)
conc5 <- rnorm(20, 1.8, 0.5)

conc_df <- data.frame(concentration = rep(c("1%", "5%"), each=20), rate = c(conc1, conc5))
print(conc_df)

conc_ANOVA <- aov(rate ~ concentration, data=conc_df)
p_value <- summary(conc_ANOVA)[[1]][[1,"Pr(>F)"]]
print (p_value)

ss <- c(5, 10, 20, 40, 80)
for (i in 1:length(ss)){
  conc1 <- rnorm(ss[i], 1.1, 0.3)
  conc5 <- rnorm(ss[i], 1.8, 0.5)
  conc_df <- data.frame(concentration = rep(c("1%", "5%"), each=20), rate = c(conc1, conc5))
  conc_ANOVA <- aov(rate ~ concentration, data=conc_df)
  p_value <- summary(conc_ANOVA)[[1]][[1,"Pr(>F)"]]
  print(p_value)
}

m <- c(1, 2, 3, 4)
for (i in 1:length(m)){
  conc1 <- rnorm(20, 1.1+m[i], 0.3)
  conc5 <- rnorm(20, 1.8+m[i], 0.5)
  conc_df <- data.frame(concentration = rep(c("1%", "5%"), each=20), rate = c(conc1, conc5))
  conc_ANOVA <- aov(rate ~ concentration, data=conc_df)
  p_value <- summary(conc_ANOVA)[[1]][[1,"Pr(>F)"]]
  print(p_value)
}

sd <- c(0.1, 0.2, 0.3, 0.4)
for (i in 1:length(m)){
  conc1 <- rnorm(20, 1.1, 0.3+sd[i])
  conc5 <- rnorm(20, 1.8, 0.5+sd[i])
  conc_df <- data.frame(concentration = rep(c("1%", "5%"), each=20), rate = c(conc1, conc5))
  conc_ANOVA <- aov(rate ~ concentration, data=conc_df)
  p_value <- summary(conc_ANOVA)[[1]][[1,"Pr(>F)"]]
  print(p_value)
}

