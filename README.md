# covid-90data
COVID-19 patients data and the R script for the analysis (Chen X. et al, Research on Influencing Factors and Classification of patients with mild and severe COVID-19 Symptoms)

## 90original.csv
complete data from the original 90 patients after interpolation
## smotedata.csv
data used for Lasso and logistic regression analysis.
## smote_data_train.csv and smote_data_test.csv
The data set was divided into two sets, smote_data_train set and smote_data_test set. The ratio of people in the two sets was 7:3. These data are used for random forest analysis.
## Descriptive statistical compilation of raw data.R
R script used for descriptive analysis.
## lasso_logistics_code.R
R script used for Lasso and logistic regression analysis.
## randforest_code.R
R script used for random forest analysis.
