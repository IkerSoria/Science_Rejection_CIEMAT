----------------------------------------------------------------------------------------------------------------------------------------------------------------
# LAIC_CTS_CIEMAT
Online repository of research based on the LAIC questionnaire
----------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------

# BBDD_LAIC.RData file
This is the initial raw data file from which all of LAIC's analysis are based.

# Data_cleansing_code.R file
This script loads a dataset from a file and processes it to create a new data frame "LAIC". The input data is stored in "BBDD_LAIC.RData", which is loaded using the "load" function. The library "dplyr" is also loaded using the "library" function.

Two functions are defined: "reorder" and "cambia99". The "reorder" function is used to replace integer values from 0 to 9 with integer values from 10 to 1. The "cambia99" function replaces the value 99 with 0.

The input data is then processed to create two new data frames: "nucleo_neg_rev" and "nucleo_pos". The "nucleo_neg_rev" data frame is created by selecting specific columns from "BBDD_LAIC", applying the "reorder" function to each column using the "apply" function, and renaming the columns. The "nucleo_pos" data frame is created by selecting specific columns from "BBDD_LAIC".

The "LAIC" data frame is created by combining the "nucleo_pos" and "nucleo_neg_rev" data frames with additional columns selected from "BBDD_LAIC". Values of 99 in columns 2 to 117 are replaced with NA. Rows with NA values are removed using "na.omit". Columns 36, 43, and 51 are removed using "select". Values of 99 in rows 131 to 150 are replaced with 0 using the "cambia99" function. The "Nu_45" column is created by applying the "reorder" function to the "Nu_45r" column. Finally, the columns of "LAIC" are reordered and renamed.

# LAIC.RData file
This is the file that is generated after running the Data_cleansing_code.R script on the raw database, BBDD_LAIC.RData, and is then taken as the basis for carrying out the statistical analysis.

# Manipulated_Science_Statistical_Analysis_Code.R file
The code loads data from a file "LAIC.RData" using the load() function. The following libraries are loaded with the library() function: dplyr, Hmisc, scales, psych, lavaan, ltm, QuantPsyc, energy, semPlot, WRS2, and semptools.

Two functions are defined: reorder11() and flattenCorrMatrix(). reorder11() maps values of a variable to a new set of values between 0 and 10. The flattenCorrMatrix() function takes a correlation matrix and a p-value matrix as inputs and outputs a data frame with three columns: row, column, and correlation. This function is used to convert a correlation matrix to a tidy data frame.

The code processes the data by manipulating some variables and creating new variables. First, the code replaces all values of 99 in the column "Ideología" with NA. Next, rows with Ideología >= 99 are removed. Then, the function reorder11() is applied to four variables ("Nu_15r", "Nu_16r", "Nu_36r", "Nu_44r") and the resulting data frame is bound to the original data frame LAIC. The new variables are named "Nu_15", "Nu_16", "Nu_36", and "Nu_44". The same procedure is repeated for five variables ("npe3", "npe2", "Universalismo", "idprogre2", "idprogre3"), and the resulting data frame is bound to LAIC as well. These new variables are named "npe3r", "npe2r", "Universalismor", "idprogre2r", and "idprogre3r".

Several variables are then created by summing up the values of several variables: "manipula", "polarisation", "antiintel", "conspira_cionismo", "creencias", "worldview", and "ideologia_multidimensional".

Then the code creates two dataframes called model_descriptives and worldview_descriptives which contain the trimmed mean (trim parameter set to 0.2), median absolute deviation, minimum, and maximum values for the variables of the model and the variables of Worldview.

Also the code calculates Cronbach's alpha and omega reliability coefficients for the scales in the model. The check.keys parameter is set to TRUE for the alpha function to check the factor loadings to see if they are all positive.

The third block of code calculates the Kendall rank coefficient correlation and performs multiple comparisons correction using the false discovery rate (FDR) method. The output data frame df contains the pairwise p-values and correlation coefficients between each column in df_scales. The cor column is rounded to two decimal places, and a new column cor_1 is created based on the p-value and correlation value. If the p-value is greater than 0.01 and less than or equal to 0.05 and the correlation value is less than 1, * is added to the end of cor. If the p-value is greater than 0.001 and less than or equal to 0.01 and the correlation value is less than 1, ** is added to the end of cor. If the p-value is less than or equal to 0.001 and the correlation value is less than 1, *** is added to the end of cor.

Then comes the execution of the tests for multivariate normality using the mult.norm() and mvnorm.etest() functions on a data frame called df_model_items.

The penultimate code block, contains a bootstrapped structural equation model using the lavaan package. The model uses as latent variables manipulated science, polarization, ideology, anti-intellectualism, conspiracism, and beliefs. The second block creates a path from the three latent variables, ideology, polarization, and conspiracy to the latent variable manipulated science. The third block specifies the covariance between some of the exogenous variables. Then the script calls the sem() function from the lavaan package to estimate the model parameters, and it then prints several summary statistics and fit measures using parameterEstimates(), summary(), and fitmeasures().

Finally, the code defines a matrix m and a set of labels labels. The matrix is a 34 by 10 matrix with some elements replaced by character strings corresponding to the labels in labels. The semPaths() function is then called with various arguments to create a path diagram using the manipulated_science_model. The path diagram is drawn using the layout defined in the m matrix and the node labels defined in the labels vector. Various arguments are used to adjust the appearance of the diagram, including the color and width of edges, the size and shape of nodes, and the size and position of latent variables. The resulting diagram visualizes the relationships between the variables in the manipulated_science_model.
