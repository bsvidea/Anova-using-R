library(dplyr)
productiondata <- read.csv('shift_production.csv')

groupedmean <- productiondata %>%   group_by(Shift) %>%   summarise(meanvalue = mean(Units_Per_Hour))

print(groupedmean)

productiondata$Shift <- as.factor(productiondata$Shift)

anova_model <- aov(Units_Per_Hour ~ Shift, data = productiondata)

anova_summary <- summary(anova_model)
print(anova_summary)

p_value <- anova_summary[[1]]$'Pr(>F)'[1] 
alpha <- 0.05

options(scipen = 10) 

cat("P-value for 'Shift' factor:", p_value, "\n")

if (p_value < alpha) {
  cat("Conclusion: Reject the Null Hypothesis (H0).\n")
  cat("There is statistically significant evidence that at least one shift's average production efficiency is different from the others.\n")
  
} else {
  cat("Conclusion: Fail to Reject the Null Hypothesis (H0).\n")
  cat("There is no statistically significant evidence to conclude that the average production efficiency differs among the shifts.\n")
}

boxplot(Units_Per_Hour ~ Shift, data = productiondata,
        main = "Production Efficiency by Shift",
        xlab = "Shift",
        ylab = "Units Produced Per Hour",
        col = c("blue", "green","yellow"),
        cex.main = 1.2, cex.lab = 1.1, cex.axis = 1.0)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

