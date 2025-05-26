# 🚦Traffic Detection Based on Environmental Features

This project uses Big Data techniques to analyze and detect traffic conditions by leveraging environmental factors such as **temperature**, **visibility**, and other derived features.

---

## 📁 Dataset: Traffic Dataset with Temperature and Visibility

- **Filename**: `Traffic_dataset_with_temperature_and_visibility.csv`
- **Rows**: 3000 (approximate)
- **Columns**:
  - `ID`: Unique identifier for each record.
  - `Date`: Timestamp or date when the data was recorded.
  - `Temperature(F)`: Ambient temperature in Fahrenheit.
  - `Visibility(mi)`: Visibility at the time of the reading, in miles.
  - `Traffic Condition`: Target variable representing traffic level (e.g., Light, Moderate, Heavy).
  - **Additional derived or cleaned features** may include:
    - `DayOfWeek`
    - `Hour`
    - `IsWeekend`
    - `Temp_Category` 
    - `Visibility_Level` 

### 🧼 Preprocessing
- **Missing value handling** for temperature and visibility.
- **Categorical encoding** for traffic levels.
- **Feature binning** to convert continuous values into categorical groups.
- **Normalization/Scaling** applied in relevant models.

---

## 🧪 Analysis & Features

- **Exploratory Data Analysis (EDA)**:
  - Visualizations exploring relationships between temperature, visibility, and traffic.
  - Stored in `Plots/` (e.g., `all eda.pdf`).

- **Feature Engineering**:
  - Performed via `bigdataproject_addingvars.R` script.
  - Extracts day, hour, temperature category, and more.

- **Machine Learning Models**:
  - **Decision Tree** (`DecisionTree.R`)
  - **Naive Bayes** (`naiveBayes.R`)
  - **Support Vector Machine (SVM)** (`svm.R`)
  - **K-Means Clustering** (`k_means.R`)
  - **Linear Regression** (`regression.R`)
  - **Hypothesis Testing** (`hypothesis.R`)

---

## 📊 Output

- **Model predictions and evaluation visuals**:
  - Decision Tree output in `decisionTree.pdf`
  - Naive Bayes prediction in `naiveBayesPrediction.png`
  - Regression plots, clustering graphs, and SVM decision boundaries.

---

## 🛠️ Tools & Technologies

- **Language**: R
- **IDE**: RStudio
- **Libraries**:
  - `ggplot2` for visualizations
  - `dplyr` for data manipulation
  - `caret`, `e1071`, `rpart`, `class` for machine learning models

---

## 🚀 Project Goals

- Detect traffic patterns based on environmental features.
- Compare different machine learning models for classification and prediction.
- Utilize Big Data workflows for processing and analyzing high-volume sensor-like data.
