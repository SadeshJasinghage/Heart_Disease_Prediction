# Heart Disease Prediction using Logistic Regression in R

## Overview
This project predicts heart disease using logistic regression. The dataset is from the UCI Machine Learning Repository, and the model is evaluated using McFadden’s Pseudo R² and accuracy.

## Dataset
- **Source:** [UCI Heart Disease Dataset](http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/)
- **Target Variable (`hd`)**:  
  - "Healthy" (0) - No heart disease  
  - "Unhealthy" (1) - Presence of heart disease  

## Model Used
- **Logistic Regression (`glm()`)**
- **Evaluation Metrics:**  
  - Confusion Matrix  
  - Accuracy Score  
  - McFadden’s Pseudo R²

## Results
- Accuracy: ~83.5% (varies depending on train/test split)
- McFadden’s Pseudo R²: ~0.55
  

## Files
- `heart_disease.R` - Main R script for data preprocessing, model training, and evaluation.
- `heart_disease_test_probabilities.pdf` - Visualization of predicted probabilities.

## How to Run
### **1️ Install Required Packages**
Run this in R:
```r
install.packages(c("ggplot2", "cowplot", "caTools", "caret"))



