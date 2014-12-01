ADAM
====

# Automatic DAta Modeling #


This is a project utilze R for automatic data modeling, specifically, for supervized data learning.

# Input and Output #
The program accept a CSV data file as input, which includes single or multi-column labels (response variable), and multi-column features (explanatory variables).

Performace of classification or regression model, can be formated as LaTex file automatically, hence a report for the data analsis can be gernrated automatically.


# Procedure #
1. The program read data from CSV file into dataframe.
2. The program conduct preprocess on data, for both response vaibales and explanatory variable.
3. The modeling process, which applied caret package in R, can use various algorithm in both classification and regression, like SVM, Random Forest, LASSO, etc. 
4. The program will build prediction model for each respone variable, and evaluate the performance of the model via cross validation.

# Future Work #
This project is still under work. Purpose of this project is to build a general frame work for classfication or regression modeling procedure.