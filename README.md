# ⚾ MLB Pitcher Strikeout Predictor

## 📝 Project Overview
This project uses **machine learning models in R** to predict a pitcher's **strikeout percentage (K%)** based on historical MLB data. The dataset includes advanced pitching metrics such as velocity, plate discipline stats, and movement data to determine key factors that influence a pitcher's ability to generate strikeouts.

## 🔍 Data Sources
- **FanGraphs**: Pitcher statistics from 2021-2024 (minimum 30 innings pitched).
- **Google Sheets**: Data stored and imported using `gsheet2tbl()`.
- **Lahman Baseball Database**: Additional historical context.

## 🏆 Best Performing Model
The **Ridge Regression Model** was selected as the final model based on its **lowest RMSE and highest R² score**. 

### **🔢 Final Model Predictors**
The following features were used in the final Ridge model:
- `vFA` → Fastball velocity  
- `O-Swing%` → Percentage of pitches batters swing outside the strike zone  
- `Contact%` → Percentage of contact batters make when swinging  
- `Z-Contact%` → Percentage of contact batters make on pitches inside the strike zone  
- `O-Contact%` → Percentage of contact batters make on pitches outside the strike zone  
- `ERA` → Earned Run Average  
- `G` → Games played  

## 📊 Model Performance
| Model                  | RMSE  | R²    | MAE   |
|------------------------|-------|-------|-------|
| **Linear Regression**  | 0.0298 | 0.6462 | 0.0234 |
| **Ridge Regression**   | 0.0287 | 0.6968 | 0.0231 |
| **Lasso Regression**   | 0.0289 | 0.6945 | 0.0252 |
| **Random Forest**      | 0.0322 | 0.5921 | 0.0279 |
| **XGBoost**           | 0.0304 | 0.6543 | 0.0256 |

**Key Takeaway:** Ridge Regression performed **best**, achieving the **highest R² (0.6968) and lowest RMSE (0.0287)**.

## 🛠️ Required R Packages  
To run this project, install the required R packages:
```r
install.packages(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot",
                   "tidymodels", "yardstick", "gsheet", "caret", "randomForest", 
                   "here", "tibble", "dplyr", "tidyr", "baseballr", "Lahman",
                   "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix"))
```

## 📌 Future Improvements

- Test additional ensemble models (e.g., Stacking).
- Tune hyperparameters for XGBoost and Random Forest.
- Explore time-series forecasting techniques for future predictions.

## 🔗 Related Links

- Live Project: [NBA MVP Predictor](https://steven-martinez-colon.github.io/projects/mlb-kpercent.html)
- Data Source: [FanGraphs](https://www.fangraphs.com)

## 📩 Contact

🔗 LinkedIn: [Steven Martinez](https://www.linkedin.com/in/steven-martinez-colon/)

💻 Portfolio: [steven-martinez-colon.github.io](https://steven-martinez-colon.github.io)
