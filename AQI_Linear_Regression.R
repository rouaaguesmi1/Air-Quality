#install.packages(c('corrplot','tidyverse','moments', 'ggplot2', 'caret','gridExtra','lattice','ggcorrplot'))
library(corrplot)
library(ggplot2)
library(tidyverse)
library(caret)
library(lattice)
library(gridExtra)
library(dplyr)
library(ggcorrplot)
library(scales)
library(moments)

vehicles_data <- read.csv("C:\\Users\\Orjouwen\\Desktop\\Stats\\Data\\registered-vehicles-per-1000-people.csv")
pollution_data <- read.csv("C:\\Users\\Orjouwen\\Desktop\\Stats\\Data\\global air pollution dataset.csv")

merged_data <- merge(pollution_data, vehicles_data, by.x = "Country", by.y = "Entity")
print("Combined Data Overview")
print(summary(merged_data))
print(str(merged_data))

# Check for missing values
print("Missing Values in Combined Data")
merged_data_missing <- colSums(is.na(merged_data))
print(merged_data_missing)

# Rename column 
colnames(merged_data)[colnames(merged_data) == "Registered.vehicles.per.1.000.people"] <- "Vehicles"

merged_data <- subset(merged_data, select = -Code)
write.csv(merged_data, "C:\\Users\\Orjouwen\\Desktop\\data\\main\\merged_data.csv", row.names = FALSE)
print("Combined Data Overview")
print(summary(merged_data))
colnames(merged_data)

# Check for duplicate rows
print("Duplicate Rows in Merged Data")
merged_data_duplicates <- merged_data[duplicated(merged_data), ]
print(merged_data_duplicates)

#--------------------------------------------------------------
# Select only categorical columns
categorical_data <- merged_data %>%
  select(AQI.Category, CO.AQI.Category, Ozone.AQI.Category, NO2.AQI.Category, PM2.5.AQI.Category)

# Convert to long format for easier plotting
categorical_long <- categorical_data %>%
  gather(key = "Category_Variable", value = "Category")

# Plot bar chart for categorical data distribution
ggplot(categorical_long, aes(x = Category, fill = Category_Variable)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  facet_wrap(~Category_Variable, scales = "free") +
  labs(
    title = "Distribution of Categorical Variables",
    x = "Category",
    y = "Frequency",
    fill = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Select only numerical columns
numerical_data <- merged_data %>%
  select(AQI.Value, CO.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles)

# Convert to long format for easier plotting
numerical_long <- numerical_data %>%
  gather(key = "Numerical_Variable", value = "Value")

# Plot density plot for numerical data distribution
ggplot(numerical_long, aes(x = Value, fill = Numerical_Variable)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~Numerical_Variable, scales = "free") +
  labs(
    title = "Distribution of Numerical Variables",
    x = "Value",
    y = "Density",
    fill = "Variable"
  ) +
  theme_minimal()

#--------------------------------------------------------------

# label encoding 
merged_data <- merged_data %>%
  mutate(
    AQI.Category = as.numeric(factor(AQI.Category)),
    CO.AQI.Category = as.numeric(factor(CO.AQI.Category)),
    Ozone.AQI.Category = as.numeric(factor(Ozone.AQI.Category)),
    NO2.AQI.Category = as.numeric(factor(NO2.AQI.Category)),
    PM2.5.AQI.Category = as.numeric(factor(PM2.5.AQI.Category))
  )
unique(pollution_data$AQI.Category)
print(summary(merged_data))
print(str(merged_data))
write.csv(merged_data, "C:\\Users\\Orjouwen\\Desktop\\data\\main\\afterencoding.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------------------
# Compute correlation matrix
correlation_matrix <- cor(numerical_data, use = "complete.obs")

# Plot the correlation matrix
ggcorrplot(correlation_matrix, 
           method = "square",
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Matrix of Numerical Variables",
           lab_size = 3,
           colors = c("red", "white", "blue"),
           ggtheme = theme_minimal())
#-------------------------------------------------------------------------------------------------------
# Reshape numerical data for plotting
numerical_long <- numerical_data %>%
  gather(key = "Variable", value = "Value")

# Create boxplots for outlier detection
ggplot(numerical_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 2) +
  labs(
    title = "Outlier Detection",
    x = "Variable",
    y = "Value",
    fill = "Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------------------------------------
# Create histograms of the numerical variables before normalization
# Reshape to long format for easier plotting
merged_data_long <- merged_data %>%
  select(AQI.Value, CO.AQI.Value, Ozone.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles) %>%
  gather(key = "Variable", value = "Value")

# Create a facet grid to show histograms for all the variables
ggplot(merged_data_long, aes(x=Value)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") + # separate plots for each variable
  ggtitle("Histograms of AQI Values Before Normalization") + 
  theme_minimal() +
  theme(strip.text = element_text(size = 10))


numeric_features <- merged_data %>%
  select(AQI.Value, CO.AQI.Value, Ozone.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles)

# Reshape data for plotting
numeric_long <- numeric_features %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value")

# Plot
ggplot(numeric_long, aes(x = Value, fill = Feature)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Feature, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Features Before Normalization", x = "Value", y = "Density")

#merged_data <- merged_data %>%
#  mutate(
#    AQI.Value = scale(AQI.Value),
#    CO.AQI.Value = scale(CO.AQI.Value),
#    Ozone.AQI.Value = scale(Ozone.AQI.Value),
#    NO2.AQI.Value = scale(NO2.AQI.Value),
#    PM2.5.AQI.Value = scale(PM2.5.AQI.Value),
#    Vehicles = scale(Vehicles)
#  )
merged_data <- merged_data %>%
  mutate(
    AQI.Value = rescale(AQI.Value, to = c(0, 1)),
    CO.AQI.Value = rescale(CO.AQI.Value, to = c(0, 1)),
    Ozone.AQI.Value = rescale(Ozone.AQI.Value, to = c(0, 1)),
    NO2.AQI.Value = rescale(NO2.AQI.Value, to = c(0, 1)),
    PM2.5.AQI.Value = rescale(PM2.5.AQI.Value, to = c(0, 1)),
    Vehicles = rescale(Vehicles, to = c(0, 1))
  )


# Reshape scaled data for plotting
scaled_numeric <- scaled_data %>%
  select(AQI.Value, CO.AQI.Value, Ozone.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value")

# Plot
ggplot(scaled_numeric, aes(x = Value, fill = Feature)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Feature, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Features After Normalization", x = "Value", y = "Density")

# Combine original and scaled data
comparison_data <- bind_rows(
  numeric_features %>% mutate(Type = "Before Scaling"),
  scaled_numeric %>% mutate(Type = "After Scaling")
)

# Plot
ggplot(comparison_data, aes(x = Feature, y = Value, fill = Type)) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free") +
  theme_minimal() +
  labs(title = "Comparison of Features Before and After Scaling", x = "Feature", y = "Value")
write.csv(merged_data, "C:\\Users\\Orjouwen\\Desktop\\Stats\\Data\\merged_data.csv", row.names = FALSE)
print(summary(merged_data))
print(str(merged_data))

# Remove scaling attributes from the scaled columns
merged_data_cleaned <- merged_data %>%
  mutate(
    AQI.Value = as.numeric(AQI.Value),
    CO.AQI.Value = as.numeric(CO.AQI.Value),
    Ozone.AQI.Value = as.numeric(Ozone.AQI.Value),
    NO2.AQI.Value = as.numeric(NO2.AQI.Value),
    PM2.5.AQI.Value = as.numeric(PM2.5.AQI.Value),
    Vehicles = as.numeric(Vehicles)
  )

# Remove scaling attributes from the scaled columns
merged_data_cleaned <- merged_data %>%
  mutate(
    AQI.Value = as.numeric(AQI.Value),
    CO.AQI.Value = as.numeric(CO.AQI.Value),
    Ozone.AQI.Value = as.numeric(Ozone.AQI.Value),
    NO2.AQI.Value = as.numeric(NO2.AQI.Value),
    PM2.5.AQI.Value = as.numeric(PM2.5.AQI.Value),
    Vehicles = as.numeric(Vehicles)
  )

# Save the cleaned dataset
write.csv(
  merged_data_cleaned,
  "C:\\Users\\Orjouwen\\Desktop\\Stats\\Data\\merged_data_scaled_cleaned.csv",
  row.names = FALSE
)

# Check the result
print(summary(merged_data_cleaned))
print(str(merged_data_cleaned))

# Create histograms of the numerical variables before normalization
# Reshape to long format for easier plotting
merged_data_long <- merged_data_cleaned %>%
  select(AQI.Value, CO.AQI.Value, Ozone.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles) %>%
  gather(key = "Variable", value = "Value")

# Create a facet grid to show histograms for all the variables
ggplot(merged_data_long, aes(x=Value)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") + # separate plots for each variable
  ggtitle("Histograms of AQI Values After Normalization") + 
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

numerical_data <- merged_data_cleaned %>%
  select(AQI.Value, CO.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles)

# Reshape numerical data for plotting
numerical_long <- numerical_data %>%
  gather(key = "Variable", value = "Value")

# Create boxplots for outlier detection
ggplot(numerical_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 2) +
  labs(
    title = "Outlier Detection",
    x = "Variable",
    y = "Value",
    fill = "Variable"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#------------------------------------------------------------------------------------------

# Compute skewness and kurtosis for numerical variables
skewness_kurtosis <- data.frame(
  Variable = c("AQI.Value", "CO.AQI.Value", "Ozone.AQI.Value", "NO2.AQI.Value", "PM2.5.AQI.Value", "Vehicles"),
  Skewness = c(
    skewness(merged_data_cleaned$AQI.Value, na.rm = TRUE),
    skewness(merged_data_cleaned$CO.AQI.Value, na.rm = TRUE),
    skewness(merged_data_cleaned$Ozone.AQI.Value, na.rm = TRUE),
    skewness(merged_data_cleaned$NO2.AQI.Value, na.rm = TRUE),
    skewness(merged_data_cleaned$PM2.5.AQI.Value, na.rm = TRUE),
    skewness(merged_data_cleaned$Vehicles, na.rm = TRUE)
  ),
  Kurtosis = c(
    kurtosis(merged_data_cleaned$AQI.Value, na.rm = TRUE),
    kurtosis(merged_data_cleaned$CO.AQI.Value, na.rm = TRUE),
    kurtosis(merged_data_cleaned$Ozone.AQI.Value, na.rm = TRUE),
    kurtosis(merged_data_cleaned$NO2.AQI.Value, na.rm = TRUE),
    kurtosis(merged_data_cleaned$PM2.5.AQI.Value, na.rm = TRUE),
    kurtosis(merged_data_cleaned$Vehicles, na.rm = TRUE)
  )
)

# Display the results
print(skewness_kurtosis)

print(summary(merged_data_cleaned))
print(str(merged_data_cleaned))
#------------------------------------hypo testing----------------------------------
# Step 1: Binning the vehicle density data based on 'Vehicles'
merged_data_cleaned <- merged_data_cleaned %>%
  mutate(Vehicle_Density_Group = cut(Vehicles, 
                                     breaks = c(-Inf, 0.1, 0.3, 0.6, Inf),  # Adjust bin thresholds as needed
                                     labels = c("Low", "Moderate", "High", "Very High")))

# Verify the distribution of bins
cat("Vehicle Density Group Distribution:\n")
print(table(merged_data_cleaned$Vehicle_Density_Group))

# Visualize group sizes: Check Independence
group_sizes <- table(merged_data_cleaned$Vehicle_Density_Group)
barplot(group_sizes, main = "Observations per Vehicle Density Group",
        col = "lightblue", xlab = "Vehicle Density Group", ylab = "Number of Observations")


ggplot(merged_data_cleaned, aes(x = Vehicle_Density_Group, y = AQI.Value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of AQI by Vehicle Density Group",
       x = "Vehicle Density Group", y = "AQI Value")
# Step 2: Define Hypotheses
# Null Hypothesis (H0): The medians of AQI.Value are the same across all Vehicle Density Groups.
# Alternative Hypothesis (H1): At least one group has a different median AQI.Value.

kruskal_test <- kruskal.test(AQI.Value ~ Vehicle_Density_Group, data = merged_data_cleaned)
cat("Kruskal-Wallis Test Results:\n")
print(kruskal_test)
if (kruskal_test$p.value < 0.05) {
  cat("The Kruskal-Wallis test indicates significant differences in AQI values across vehicle density groups.\n")
  cat("Review the pairwise Wilcoxon test results for detailed group comparisons.\n")
} else {
  cat("No significant difference in AQI values across vehicle density groups.\n")
}
if (kruskal_test$p.value < 0.05) {
  pairwise_test <- pairwise.wilcox.test(merged_data_cleaned$AQI.Value,
                                        merged_data_cleaned$Vehicle_Density_Group,
                                        p.adjust.method = "bonferroni")
  print(pairwise_test)
}

# Boxplot
p <- ggplot(merged_data_cleaned, aes(x = Vehicle_Density_Group, y = AQI.Value)) +
  geom_boxplot(fill = c("skyblue", "lightgreen", "orange", "red"), color = "black") +
  theme_minimal() +
  labs(title = "AQI Value by Vehicle Density Group",
       x = "Vehicle Density Group",
       y = "Normalized AQI Value")

# Adding Significance Levels
p <- p + stat_compare_means(comparisons = list(
  c("Low", "Moderate"),
  c("Low", "High"),
  c("Low", "Very High"),
  c("Moderate", "High"),
  c("Moderate", "Very High"),
  c("High", "Very High")
), method = "wilcox.test", label = "p.signif")

print(p)
#------------------------------------------------------------------------------------------------
#merged_data_cleaned <- merged_data_cleaned %>%
#  mutate(
#    AQI.Value.Log = log(AQI.Value + 1),
#    CO.AQI.Value.Log = log(CO.AQI.Value + 1),
#    Ozone.AQI.Value.Log = log(Ozone.AQI.Value + 1),
#    NO2.AQI.Value.Log = log(NO2.AQI.Value + 1),
#    PM2.5.AQI.Value.Log = log(PM2.5.AQI.Value + 1)
#  )
#numerical_data <- merged_data_cleaned %>%
#  select(AQI.Value, CO.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value, Vehicles)
# Reshape numerical data for plotting
#numerical_long <- numerical_data %>%
#  gather(key = "Variable", value = "Value")
#
# Create boxplots for outlier detection
#ggplot(numerical_long, aes(x = Variable, y = Value, fill = Variable)) +
#  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.size = 2) +
#  labs(
#    title = "Outlier Detection",
#    x = "Variable",
#    y = "Value",
#    fill = "Variable"
#  ) +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------------------

# Linear Regression: AQI vs Vehicles_Per_1000_People
linear_model <- lm(AQI.Value ~ Vehicles, data = merged_data_cleaned)
cat("Linear Regression Summary:\n")
summary(linear_model)

ggplot(merged_data_cleaned, aes(x = Vehicles, y = AQI.Value)) +
  geom_point(color = "blue") +  # Scatter plot for the data points
  geom_smooth(method = "lm", color = "red") +  # Add the regression line
  labs(x = "Number of Vehicles", y = "AQI Value", title = "Linear Regression: AQI Value vs Vehicles") +
  theme_minimal()

# Evaluate Linear Regression
predicted_aqi <- predict(linear_model, merged_data_cleaned)
linear_rmse <- sqrt(mean((merged_data_cleaned$AQI.Value - predicted_aqi)^2))
linear_mae <- mean(abs(merged_data_cleaned$AQI.Value - predicted_aqi))
linear_mape <- mean(abs((merged_data_cleaned$AQI.Value - predicted_aqi) / merged_data_cleaned$AQI.Value)) * 100

cat("Linear Regression RMSE:", linear_rmse, "\n")
cat("Linear Regression MAE:", linear_mae, "\n")
cat("Linear Regression MAPE:", linear_mape, "%\n")

# Assumption Checks for Linear Regression
# 1. Histogram and Normality of Residuals using ggplot2
residuals <- residuals(linear_model)
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# 2. Homoscedasticity using ggplot2
ggplot(data.frame(fitted = fitted(linear_model), residuals = residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Multicollinearity Analysis
cat("Variance Inflation Factor (VIF):\n")
model_for_vif <- lm(AQI.Value ~ ., data = merged_data_cleaned)
library(car)
vif_values <- vif(model_for_vif)
print(vif_values)
#---------------------------------------------------------------------------------

# Multiple Regression
multiple_model <- lm(AQI.Value ~ CO.AQI.Value +PM2.5.AQI.Value+ Ozone.AQI.Value + NO2.AQI.Value + Vehicles, data = merged_data_cleaned)
cat("Multiple Regression Summary:\n")
summary(multiple_model)

ggplot(merged_data_cleaned, aes(x = Vehicles, y = AQI.Value)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Vehicles", y = "AQI Value", title = "Multiple Regression: AQI vs Vehicles") +
  theme_minimal()
ggplot(merged_data_cleaned, aes(x = NO2.AQI.Value, y = AQI.Value)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "NO2 AQI Value", y = "AQI Value", title = "Multiple Regression: AQI vs NO2 AQI") +
  theme_minimal()

ggplot(merged_data_cleaned, aes(x = CO.AQI.Value, y = AQI.Value)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "CO AQI Value", y = "AQI Value", title = "Multiple Regression: AQI vs CO AQI") +
  theme_minimal()

ggplot(merged_data_cleaned, aes(x = Ozone.AQI.Value, y = AQI.Value)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Ozone AQI Value", y = "AQI Value", title = "Multiple Regression: AQI vs Ozone AQI") +
  theme_minimal()
ggplot(merged_data_cleaned, aes(x = Ozone.AQI.Value, y = AQI.Value)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Ozone AQI Value", y = "AQI Value", title = "Multiple Regression: AQI vs Ozone AQI") +
  theme_minimal()
ggplot(merged_data_cleaned, aes(x = PM2.5.AQI.Value, y = AQI.Value)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Ozone AQI Value", y = "AQI Value", title = "Multiple Regression: AQI vs PM2.5.AQI.Value") +
  theme_minimal()

# Evaluate Multiple Regression
predicted_aqi_multiple <- predict(multiple_model, merged_data)
multiple_rmse <- sqrt(mean((merged_data$AQI_Value - predicted_aqi_multiple)^2))
multiple_mae <- mean(abs(merged_data$AQI_Value - predicted_aqi_multiple))
multiple_mape <- mean(abs((merged_data$AQI_Value - predicted_aqi_multiple) / merged_data$AQI_Value)) * 100

cat("Multiple Regression RMSE:", multiple_rmse, "\n")
cat("Multiple Regression MAE:", multiple_mae, "\n")
cat("Multiple Regression MAPE:", multiple_mape, "%\n")

# Visualize Actual vs Predicted for Multiple Regression using ggplot2
ggplot(merged_data, aes(x = AQI_Value, y = predicted_aqi_multiple)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Actual vs Predicted AQI for Multiple Regression", x = "Actual AQI", y = "Predicted AQI")
#---------------------------------------------------------------------------------
# Linear Regression: AQI vs PM2.5.AQI.Value
linear_model <- lm(AQI.Value ~ PM2.5.AQI.Value, data = merged_data_cleaned)
cat("Linear Regression Summary:\n")
summary(linear_model)

ggplot(merged_data_cleaned, aes(x = PM2.5.AQI.Value, y = AQI.Value)) +
  geom_point(color = "blue") +  # Scatter plot for the data points
  geom_smooth(method = "lm", color = "red") +  # Add the regression line
  labs(x = "PM2 5 AQI Value", y = "AQI Value", title = "Linear Regression: AQI Value vs PM2.5.AQI.Value") +
  theme_minimal()

# Evaluate Linear Regression
predicted_aqi <- predict(linear_model, merged_data_cleaned)
linear_rmse <- sqrt(mean((merged_data_cleaned$AQI.Value - predicted_aqi)^2))
linear_mae <- mean(abs(merged_data_cleaned$AQI.Value - predicted_aqi))
linear_mape <- mean(abs((merged_data_cleaned$AQI.Value - predicted_aqi) / merged_data_cleaned$AQI.Value)) * 100

cat("Linear Regression RMSE:", linear_rmse, "\n")
cat("Linear Regression MAE:", linear_mae, "\n")
cat("Linear Regression MAPE:", linear_mape, "%\n")


# Select only the AQI.Value and Country columns
aqi_country_data <- merged_data_cleaned %>% 
  select(Country, AQI.Value)

# Write the data to a CSV file
write.csv(aqi_country_data, "C:/Users/Orjouwen/Desktop/aqi_country_data.csv", row.names = FALSE)
