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

# Important note
The documentation found in this README file was partially generated using the chatbot ChatGPT. No other neural network generated content was employed in this repository.
