# Air Quality Prediction 

---

# **Air Quality Prediction Using Linear Regression in R**

## **Overview**
This project aims to predict the **Air Quality Index (AQI)** using **Linear Regression models** in R. By analyzing pollutant data and vehicle density statistics, the project explores the relationship between various environmental factors and AQI levels. The analysis leverages **statistical methods**, **data visualization**, and **model evaluation techniques** to deliver actionable insights.

---

## **Features**
- **Data Cleaning**: Handle missing values, outliers, and inconsistencies in the datasets.
- **Exploratory Data Analysis (EDA)**: Visualize and summarize the data, including correlations between pollutants and AQI.
- **Statistical Assumptions**: Test for normality, variance equality, and independence to validate model assumptions.
- **Linear Regression Models**: 
  - Simple Linear Regression: Predict AQI using a single variable.
  - Multiple Linear Regression: Use multiple predictors, including vehicle density and pollutant AQI values.
- **Non-Parametric Testing**: Assess the relationship between vehicle density and AQI using Kruskal-Wallis and Wilcoxon tests.
- **Model Evaluation**: Evaluate regression models using metrics like RMSE, MAE, and MAPE.
- **Data Visualization**: Generate boxplots, histograms, scatter plots, and maps for actionable insights.

---

## **Datasets**
1. **Global Air Pollution Dataset**  
   - Contains air quality measurements for various pollutants (PM2.5, CO, NO2, Ozone).  
   - Columns include `Country`, `City`, `AQI.Value`, and pollutant-specific AQI values.

2. **Registered Vehicles Dataset**  
   - Provides the number of registered vehicles per 1,000 people across different countries and years.  
   - Columns include `Entity`, `Year`, and `Registered.vehicles.per.1.000.people`.

3. **Merged Dataset**  
   - Combines the above datasets to enable a comprehensive analysis of vehicle density and air quality.

---

## **Installation**
### **Prerequisites**
- **R (>= 4.0)**  
- RStudio (optional but recommended)

### **Required Libraries**
Install the required libraries by running:

```R
required_packages <- c("ggplot2", "dplyr", "corrplot", "VIM", "car", "MASS", "nortest", "scales", "GGally", "tidyr", "magrittr", "sf", "tmap", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)
```

---

## **Project Workflow**
1. **Data Loading and Cleaning**  
   - Import datasets, handle missing values, and merge them for analysis.
2. **Exploratory Data Analysis**  
   - Summarize data statistics and visualize distributions.
   - Analyze correlations between pollutants, AQI, and vehicle density.
3. **Non-Parametric Testing**  
   - Use Kruskal-Wallis and Wilcoxon tests to study AQI differences across vehicle density groups.
4. **Linear Regression Modeling**  
   - Build and evaluate simple and multiple linear regression models.
5. **Assumption Testing**  
   - Validate normality, variance equality, and residual behavior.
6. **Model Evaluation**  
   - Calculate RMSE, MAE, and MAPE to assess model performance.
7. **Visualization**  
   - Generate boxplots, scatter plots, Q-Q plots, and an AQI map.

---

## **Key Code Examples**

### **Simple Linear Regression**
```R
# Fit Simple Linear Regression Model
simple_lm <- lm(AQI.Value ~ PM2.5.AQI.Value, data = merged_data)

# Model Summary
summary(simple_lm)

# Plot Regression Line
ggplot(merged_data, aes(x = PM2.5.AQI.Value, y = AQI.Value)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Simple Linear Regression: AQI vs PM2.5",
       x = "PM2.5 AQI Value", y = "AQI Value")
```

### **Multiple Linear Regression**
```R
# Fit Multiple Linear Regression Model
multiple_lm <- lm(AQI.Value ~ CO.AQI.Value + Ozone.AQI.Value + NO2.AQI.Value + Registered.vehicles.per.1.000.people, data = merged_data)

# Model Summary
summary(multiple_lm)

# Evaluate Model
predicted_aqi <- predict(multiple_lm, merged_data)
rmse <- sqrt(mean((merged_data$AQI.Value - predicted_aqi)^2))
cat("RMSE:", rmse)
```

---

## **Results**
1. **Correlations**:
   - PM2.5 and NO2 showed the strongest correlation with AQI levels.
2. **Model Performance**:
   - Multiple regression improved prediction accuracy compared to simple regression.
3. **Non-Parametric Testing**:
   - Significant differences in AQI were observed across vehicle density groups.

---

## **Visualization**
- **Scatter Plots**: Show relationships between pollutants and AQI.  
- **Boxplots**: Compare AQI distributions across vehicle density groups.  
- **Heatmaps**: Highlight correlations between variables.  
- **Maps**: Visualize AQI variations across countries.

---


## **Future Work**
- Incorporate time-series analysis for AQI prediction.
- Add additional predictors, such as weather data or industrial emissions.
- Experiment with advanced models like Random Forest or Gradient Boosting.

---

## **Contributors**
- [Rouaa Guesmi](https://github.com/yourusername)  
- [Mohamed Amine Ghorbali](https://github.com/yourusername)  

Feel free to contribute to this project by submitting a pull request!

---

## **License**
This project is licensed under the MIT License. See the [LICENSE] file for details.

---

