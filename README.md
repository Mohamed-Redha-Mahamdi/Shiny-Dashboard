# Shiny-Dashboard
An R Shiny web application, a Dashboard  capable of doing preprocessing, visualization (EDA) and creating machine learning models  

Shiny Data Exploration Dashboard
This Shiny application is designed to facilitate comprehensive data exploration, analysis, and modeling within an interactive web interface. It allows users to upload datasets, perform various statistical analyses, and build predictive models using machine learning algorithms.

Features
Data Upload: Users can upload their own CSV files for analysis.
Data Preprocessing: Includes options for handling missing values, converting data types, and selecting features for analysis.
Descriptive Statistics: Provides an overview of the data with univariate and bivariate analysis.
Data Visualization: Generates histograms, box plots, density plots, and correlation matrices.
Predictive Modeling: Supports training and evaluation of SVM and Random Forest models.
Model Evaluation: Displays performance metrics such as confusion matrices, ROC curves, and feature importances.
Installation
To run this Shiny application on your local machine, you will need to install R and the following R packages:

install.packages(c("shiny", "DT", "dplyr", "ggplot2", "plotly", "e1071", "rpart", "psych", "corrplot", "caret", "pROC", "ROCR", "pdp", "randomForest", "shinyjs", "shinyWidgets", "shinydashboard"))
Usage
Once you have installed R and the necessary packages, you can run the application by setting your working directory to the folder containing the app files and then running the following command in R:

shiny::runApp()
Contributing
Contributions to this project are welcome. You can contribute by:

Reporting a bug
Discussing the current state of the code
Submitting a fix
Proposing new features
