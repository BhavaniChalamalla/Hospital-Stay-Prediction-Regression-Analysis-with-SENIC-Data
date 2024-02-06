# Hospital-Stay-Prediction-Regression-Analysis-with-SENIC-Data
## Project Overview
This project focuses on the analysis of a dataset containing characteristics of hospitals participating in the SENIC project. The SENIC data comprises 113 samples and 11 variables. The primary objective is to fit a Multiple Linear Regression (MLR) model to predict the average length of stay for all patients in a hospital. The project involves rigorous hypothesis testing using R to arrive at the most appropriate and robust model.

## Project Goals
The project aimed to achieve the following results:

**Model Fitting and Justification:** Fit an MLR model and justify the chosen model. Conduct assumptions checking and remediation.

**Regression Coefficients:** Compute point estimates of regression coefficients using a design matrix and normal equations. Interpret adjusted 𝑅² (𝑎𝑑𝑗.𝑅²).

**Model Adequacy Testing:** Conduct an overall F-test for model adequacy.

**Regression Coefficient Hypothesis Testing:** Perform multiple hypothesis tests for regression coefficients based on the t-test.

**Confidence Intervals:** Obtain confidence intervals for individual regression coefficients.

**Conclusion Making:** Make conclusions based on hypothesis tests and confidence intervals.

**Model Diagnostics and Transformation:** Perform model diagnostics. Apply transformations if required based on diagnostic results. Project Progress

The project was initiated by importing the dataset and conducting exploratory data analysis, providing initial insights. However, with only 19 observations, the dataset posed a challenge for building a robust MLR model. The initial attempt with a full model revealed violations of various assumptions.

## Model Refinement Steps
Multicollinearity Check: Identified and addressed multicollinearity issues in the model. Removed predictors responsible for multicollinearity.

**Stepwise Regression:**
Applied stepwise regression to iteratively select predictors based on predefined criteria. Assumption Diagnostics:

Conducted thorough assumption diagnostics. Identified and addressed violations of normality assumption. Box Cox Transformation:

Applied the Box Cox transformation to address normality issues in the model. Next Steps The project is currently at the transformed model stage, and further analyses, such as additional assumption checking and performance evaluation, will be conducted.

## Code and Files
code/: Contains R scripts for data importing, exploratory data analysis, and model fitting. results/: Stores outputs, diagnostic plots, and the final transformed model. docs/: Documentation files, including this README.

## Conclusion
The project progresses towards building a reliable MLR model for predicting hospital stay duration. Continuous refinement and thorough analysis contribute to the development of a robust model, addressing challenges encountered during the process.
