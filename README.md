# 📉 Bank Customer Churn Prediction

> A machine learning project using Logistic Regression, Decision Tree, and SVM to predict customer churn in the banking sector.
## 📌 Overview
This project analyzes bank customer data to predict churn using statistical and machine learning models. The goal is to help banks identify at-risk customers and improve retention strategies.

---

## 🧰 Tools & Technologies
- **Language**: R  
- **Libraries**: `caret`, `e1071`, `rpart`, `ggplot2`, `pROC`, `ROCR`, `corrplot`, `dplyr`, `MASS`
- **Models Used**: Logistic Regression, Decision Tree, Support Vector Machine (Polynomial Kernel)
- **Evaluation Metrics**: Accuracy, Sensitivity, Specificity, AUC, ROC

---

## 📊 Key Features
- Cleaned and prepared data from the [Kaggle dataset](https://www.kaggle.com/datasets/shantanudhakadd/bank-customer-churn-prediction)
- Performed Chi-Square tests and correlation analysis for feature selection
- Implemented:
  - **Logistic Regression** (AUC: 0.76)
  - **Decision Tree** (AUC: 0.77)
  - **Polynomial SVM** (AUC: **0.85**, Sensitivity: **59%** after threshold tuning)
- Visualized performance through confusion matrices and ROC curves
- Generated actionable insights on churn based on features like Geography, Gender, Product Usage, and Activity Level

---

## 📈 Model Comparison

| Model              | Accuracy | Sensitivity | Specificity | AUC    |
|--------------------|----------|-------------|-------------|--------|
| Logistic Regression| 80.29%   | 20.13%      | 96.33%      | 0.7625 |
| Decision Tree      | 86.10%   | 46.75%      | 96.58%      | 0.7688 |
| SVM (Polynomial)   | 84.73%   | **59.11%**  | 91.55%      | **0.8531** |

---

## 📁 Files Included
- `Customer_churn_Code.R`: Full analysis and modeling pipeline
- `Customer_Churn_Report.pdf`: Final written report with visuals

---

## 📬 Contact
📫 [LinkedIn](https://linkedin.com/in/sruthi-kondra-5773981a1)

---

## 📜 License
This project is open-source under the MIT License.
