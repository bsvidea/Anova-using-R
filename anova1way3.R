library(readxl) 
library(openxlsx) 

options(width=100)
productiondata <- read_excel('shiftdataalternate1.xlsx')
print(productiondata)
groupedmean <- productiondata %>%   group_by(Shift) %>%   
  summarise(meanvalue = mean(Units_Per_Hour)) %>%
  mutate(meanvalue = format(round(meanvalue, 2), nsmall = 2))

print(groupedmean)

productiondata$Shift <- as.factor(productiondata$Shift)

anova_model <- aov(Units_Per_Hour ~ Shift, data = productiondata)

anova_summary <- summary(anova_model)
print(anova_summary)

p_value <- anova_summary[[1]]$'Pr(>F)'[1] 
alpha <- 0.05

options(scipen = 100) 

cat("P-value for 'Shift' factor:", p_value, "\n")

if (p_value < alpha) {
  cat("Conclusion: Reject the Null Hypothesis (H0).\n")
  cat("There is statistically significant evidence that at least one shift's average production efficiency is different from the others.\n")
  
} else {
  cat("Conclusion: Fail to Reject the Null Hypothesis (H0).\n")
  cat("The means are identical, but rather that there isn't enough evidence to conclude they are truly different.\n")
}

boxplot(Units_Per_Hour ~ Shift, data = productiondata,
        main = "Production Efficiency by Shift",
        xlab = "Shift",
        ylab = "Units Produced Per Hour",
        col = c("blue", "green","yellow"),
        cex.main = 1.2, cex.lab = 1.1, cex.axis = 1.0)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

