install.packages("dplyr")
library(dplyr)
install.packages("infotheo")
library(infotheo)



data <- read.csv("D:/Fall 24-25/DS/FINAL/customer_purchase_data.csv", header = TRUE, sep =",")
`print(data)
cat("Number of missing values:", sum(is.na(data)), "\n")
cat("Number of rows:", nrow(data), "\n")

cat("Number of columns:", ncol(data), "\n")
str(data)


data$PurchaseStatus <- as.factor(data$PurchaseStatus)
data$Gender <- as.factor(data$Gender)
data$ProductCategory <- as.factor(data$ProductCategory)
data$LoyaltyProgram <- as.factor(data$LoyaltyProgram)


anova <- function(independent, dependent, data) {
  formula <- as.formula(paste(independent, "~", dependent))
  anova_method <- aov(formula, data = data)
  return(summary(anova_method))
}

chi_square <- function(independent, dependent, data) {
  contingency_table <- table(data[[independent]], data[[dependent]])
  print(contingency_table)
  return(chisq.test(contingency_table))
}

kendall <- function(independent, dependent, data) {
  numeric_categorical <- as.numeric(data[[dependent]])
  return(cor(data[[independent]], numeric_categorical, method = "kendall"))
}

mutual_method <- function(independent, dependent, data) {
  return(mutinformation(as.factor(data[[independent]]), as.factor(data[[dependent]])))
}


cat("ANOVA: Age ~ PurchaseStatus\n")
print(anova("Age", "PurchaseStatus", data))


cat("Chi-Square Test: Gender vs PurchaseStatus\n")
print(chi_square("Gender", "PurchaseStatus", data))


cat("Kendall's Correlation: AnnualIncome ~ PurchaseStatus\n")
print(kendall("AnnualIncome", "PurchaseStatus", data))


cat("ANOVA: NumberOfPurchases ~ PurchaseStatus\n")
print(anova("NumberOfPurchases", "PurchaseStatus", data))



cat("Chi-Square Test: ProductCategory vs PurchaseStatus\n")
print(chi_square("ProductCategory", "PurchaseStatus", data))



cat("ANOVA: TimeSpentOnWebsite ~ PurchaseStatus\n")
print(anova("TimeSpentOnWebsite", "PurchaseStatus", data))


cat("Mutual Information: LoyaltyProgram ~ PurchaseStatus\n")
print(mutual_method("LoyaltyProgram", "PurchaseStatus", data))



cat("ANOVA: DiscountsAvailed ~ PurchaseStatus\n")
print(anova("DiscountsAvailed", "PurchaseStatus", data))

