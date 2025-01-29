install.packages("dplyr")
library(dplyr)
data <- read.csv("D:/Fall 24-25/DS/FINAL/customer_purchase_data.csv", header = TRUE, sep = ",")
print(data)

cat("Number of missing values:", sum(is.na(data)), "\n")
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

str(data)







install.packages("rcompanion")
library(rcompanion)

hist(data$Age, xlab = "Age", main = "Histogram of Age",col="cyan")
install.packages("e1071")
library(e1071)

skewness_value <- skewness(data$Age)
cat("Skewness of Age:", skewness_value, "\n")


skewness_type <- ifelse(skewness_value > 0, "Positive",
                        ifelse(skewness_value < 0, "Negative", "Zero"))
cat("The data is", skewness_type, "skewed.\n")


plotNormalHistogram(data$Age, prob = FALSE,
                    main = "Normal Distribution Overlay on Age Histogram", 
                    xlab = "Age", length = 1500,col = "cyan")







Gender <- table(data$Gender)
barplot(Gender, xlab = "Gender", ylab = "Frequency", main = "Bar Graph for Gender",col="cyan")









boxplot(data$NumberOfPurchases, main = "Box Plot for Number of Purchases", 
        ylab = "Number of Purchases", col = "Purple", border = "red")







plot(data$Age, data$TimeSpentOnWebsite, 
     main = "Scatterplot of Age vs Time Spent on Website by Purchase Status", 
     xlab = "Age", ylab = "Time Spent on Website", 
     col = ifelse(data$PurchaseStatus == 1, "green", "purple"), pch = 19)
legend("topright", legend = c("Purchase", "No Purchase"), col = c("green", "purple"), pch = 19)









install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x = as.factor(PurchaseStatus), y = NumberOfPurchases, fill = as.factor(PurchaseStatus))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9), outlier.shape = NA) +
  labs(title = "Violin Plot of Number of Purchases by Purchase Status",
       x = "Purchase Status (0 = No, 1 = Yes)", y = "Number of Purchases") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Purchase", "Purchase")) +
  theme_minimal()








`ggplot(data, aes(x = Age, y = AnnualIncome, color = as.factor(PurchaseStatus), group = PurchaseStatus)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Line Plot of Annual Income by Age (Grouped by Purchase Status)",
       x = "Age", y = "Annual Income", color = "Purchase Status") +
  theme_minimal()

ggplot(data, aes(x = Age, y = AnnualIncome)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  labs(title = "Line Plot of Annual Income by Age", x = "Age", y = "Annual Income") +
  theme_minimal()
