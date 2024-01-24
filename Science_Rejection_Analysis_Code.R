"Data and library load"
load('/LAIC.RData') # Add path here.
"
install.packages(c('dplyr','Hmisc','scales','psych','lavaan','ltm','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','writexl','doParallel'))
"
lapply(c('dplyr','Hmisc','scales','psych','GPArotation','lavaan','ltm','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','writexl','doParallel'), 
       require, character.only = TRUE)
options(max.print=999999)
set.seed(2024)
"Functions"
#CLICK TO FOLD/UNFOLD#####################################################################################################
reorder11 <- function(x) {ifelse(x == 0, 10, ifelse(x == 1, 9, 
                                                    ifelse(x == 2, 8, 
                                                           ifelse(x == 3, 7, 
                                                                  ifelse(x == 4, 6, 
                                                                         ifelse(x == 5, 5, 
                                                                                ifelse(x == 6, 4, 
                                                                                       ifelse(x == 7, 3, 
                                                                                              ifelse(x == 8, 2, 
                                                                                                     ifelse(x == 9, 1, 0))))))))))}
##########################################################################################################################

"Data processing"
#CLICK TO FOLD/UNFOLD#####################################################################################################
LAIC$critical_thinking <- rowSums(LAIC[c('pensacrit2','pensacrit3','pensacrit4','menteab3','pensalog1','necontrol1')])
LAIC$conspiracionism <- rowSums(LAIC[c('conspiragen1','conspiragen2','conspiracien1','conspiracien2')])
LAIC$progressive <- rowSums(LAIC[c('Universalismo','npe3','npe2','idprogre2','idprogre3')])
LAIC$conservative <- rowSums(LAIC[c('idconserva1','idconserva2','idconserva4','npe1')])
LAIC$manichaeism <- rowSums(LAIC[c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')])
df_scales <- LAIC[c('critical_thinking','conspiracionism','progressive','conservative','manichaeism')]

LAIC$age6 <- ifelse((LAIC$Edad == 1 | LAIC$Edad == 2), 1,
                     ifelse((LAIC$Edad == 3 | LAIC$Edad == 4), 2,
                            ifelse((LAIC$Edad == 5 | LAIC$Edad == 6), 3,
                                   ifelse((LAIC$Edad == 7 | LAIC$Edad == 8), 4,
                                          ifelse((LAIC$Edad == 9 | LAIC$Edad == 10), 5, 6)))))

LAIC$education5 <- ifelse((LAIC$Nivel_Estudio == 0 | LAIC$Nivel_Estudio == 1 | LAIC$Nivel_Estudio == 2), 1,
                        ifelse((LAIC$Nivel_Estudio == 3 | LAIC$Nivel_Estudio == 4 | LAIC$Nivel_Estudio == 5), 2,
                               ifelse((LAIC$Nivel_Estudio == 6 | LAIC$Nivel_Estudio == 7), 3,
                                      ifelse(LAIC$Nivel_Estudio == 8, 4,
                                             ifelse((LAIC$Nivel_Estudio == 9 | LAIC$Nivel_Estudio == 10), 5, 99)))))

science_reject_variables <- as.data.frame(apply(LAIC[c('Nu_1','Nu_8','Nu_13','Nu_25','Nu_26','Nu_34')], 2, reorder11))
colnames(science_reject_variables) <- c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')
LAIC <- cbind(LAIC, science_reject_variables)

df_model_items <- LAIC[c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r','pensacrit2','pensacrit3','pensacrit4','menteab3','necontrol1',
                         'conspiragen1','conspiragen2','conspiracien1','conspiracien2','Universalismo','npe3','npe2','idprogre2','idprogre3','idconserva1',
                         'idconserva2','idconserva4','npe1','polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')]
##########################################################################################################################

"Descriptive statistics"
#CLICK TO FOLD/UNFOLD#####################################################################################################

# Distribution and age in the sample
age_education_table <- data.frame(sapply(LAIC[, c(132:133)], table))

# Mean, NMAD, Min and Max
model_descriptives <- data.frame(sapply(df_model_items, function(x) mean(x, trim=0.2)), sapply(df_model_items, mad), sapply(df_model_items, min),
                                 sapply(df_model_items, max))
model_descriptives <- cbind(index = row.names(model_descriptives), model_descriptives)
names(model_descriptives) <- c('Items','Trimmed mean*','NMAD','Min','Max')
row.names(model_descriptives) <- NULL

# Alpha and Omega
alpha(LAIC[, c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')], check.keys=TRUE)
omega(LAIC[, c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')])

alpha(LAIC[, c('pensacrit2','pensacrit3','pensacrit4','menteab3','necontrol1')], check.keys=TRUE)
omega(LAIC[, c('pensacrit2','pensacrit3','pensacrit4','menteab3','necontrol1')])

alpha(LAIC[, c('conspiragen1','conspiragen2','conspiracien1','conspiracien2')], check.keys=TRUE)
omega(LAIC[, c('conspiragen1','conspiragen2','conspiracien1','conspiracien2')],rotate='simplimax')

alpha(LAIC[, c('Universalismo','npe3','npe2','idprogre2','idprogre3')], check.keys=TRUE)
omega(LAIC[, c('Universalismo','npe3','npe2','idprogre2','idprogre3')],rotate='Promax')

alpha(LAIC[, c('idconserva1','idconserva2','idconserva4','npe1')], check.keys=TRUE)
omega(LAIC[, c('idconserva1','idconserva2','idconserva4','npe1')])

alpha(LAIC[, c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')], check.keys=TRUE)
omega(LAIC[, c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')])

# Normality and outlier detection
univariate_normality <- data.frame(sapply(df_model_items, shapiro.test)[1:2,])
outlier_detection <- data.frame(sapply(df_model_items, rosnerTest)[13,])
##########################################################################################################################

"Correlations (Kendall rank correlation coefficient)"
#CLICK TO FOLD/UNFOLD#####################################################################################################
# Variables correlations
# Get the number of columns in the dataframe
num_vars <- ncol(df_model_items)
# Create an empty matrix to store the correlation coefficients
cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars)
# Create vectors to store variable names
var_names <- colnames(df_model_items)
num_cores <- 4  # Set the number of cores to use, adjust this parameter based on your machine.
# Register a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)
# Use foreach to parallelize the loop
cor_matrix <- foreach(i = 1:num_vars, .combine = 'rbind') %dopar% {
  cor_row <- numeric(num_vars)
  for (j in 1:i) {
    cor_test_result <- cor.test(df_model_items[, i], df_model_items[, j], method = "kendall")
    cor_row[j] <- ifelse(cor_test_result$p.value <= 0.001, sprintf("%.3f***", cor_test_result$estimate),
                         ifelse(cor_test_result$p.value <= 0.01, sprintf("%.3f**", cor_test_result$estimate),
                                ifelse(cor_test_result$p.value <= 0.05, sprintf("%.3f*", cor_test_result$estimate),
                                       sprintf("%.3f", cor_test_result$estimate))))
  }
  return(cor_row)
}
# Stop the parallel backend
stopCluster(cl)
# Create a new dataframe with the correlation coefficients and significance markings
correlations <- as.data.frame(cor_matrix)
correlations[upper.tri(correlations, diag = T)] <- NA
# Set column and row names
colnames(correlations) <- var_names
rownames(correlations) <- var_names
correlations <- cbind(index = row.names(correlations), correlations)

# Scales correlations
# Get the number of columns in the dataframe
num_vars <- ncol(df_scales)
# Create an empty matrix to store the correlation coefficients
cor_matrix <- matrix(NA, nrow = num_vars, ncol = num_vars)
# Create vectors to store variable names
var_names <- colnames(df_scales)
num_cores <- 4  # Set the number of cores to use, adjust this parameter based on your machine.
# Register a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)
# Use foreach to parallelize the loop
cor_matrix <- foreach(i = 1:num_vars, .combine = 'rbind') %dopar% {
  cor_row <- numeric(num_vars)
  for (j in 1:i) {
    cor_test_result <- cor.test(df_scales[, i], df_scales[, j], method = "kendall")
    cor_row[j] <- ifelse(cor_test_result$p.value <= 0.001, sprintf("%.3f***", cor_test_result$estimate),
                         ifelse(cor_test_result$p.value <= 0.01, sprintf("%.3f**", cor_test_result$estimate),
                                ifelse(cor_test_result$p.value <= 0.05, sprintf("%.3f*", cor_test_result$estimate),
                                       sprintf("%.3f", cor_test_result$estimate))))
  }
  return(cor_row)
}
# Stop the parallel backend
stopCluster(cl)
# Create a new dataframe with the correlation coefficients and significance markings
correlations <- as.data.frame(cor_matrix)
correlations[upper.tri(correlations, diag = T)] <- NA
# Set column and row names
colnames(correlations) <- var_names
rownames(correlations) <- var_names
correlations <- cbind(index = row.names(correlations), correlations)
##########################################################################################################################

"SEM assumptions"
#CLICK TO FOLD/UNFOLD#####################################################################################################
# Multivariate normality tests
mult.norm(df_model_items)$mult.test
mvnorm.etest(df_model_items, R=10000) # Depending on your computer this line may take several minutes to run.

# Multicolinearity testing with random binary variable and regressions
LAIC$binary <- rbinom(nrow(LAIC), 1, 0.5)
vif_science_rejection <- lm(binary ~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r, data=LAIC)
vif(vif_science_rejection)
vif_critical_thinking <- lm(binary ~ pensacrit2+pensacrit3+pensacrit4+menteab3+necontrol1, data=LAIC)
vif(vif_critical_thinking)
vif_conspiracionism <- lm(binary ~ conspiragen1+conspiragen2+conspiracien1+conspiracien2, data=LAIC)
vif(vif_conspiracionism)
vif_progressive <- lm(binary ~ Universalismo+npe3+npe2+idprogre2+idprogre3, data=LAIC)
vif(vif_progressive)
vif_conservative <- lm(binary ~ idconserva1+idconserva2+idconserva4+npe1, data=LAIC)
vif(vif_conservative)
vif_manichaeism <- lm(binary ~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3, data=LAIC)
vif(vif_manichaeism)

# Outlier testing
outlier_IDs <- c(149,225,463,163,304,589,136,150,551,124,237,515,530,605,964,236,248,530,75,82,350)
LAIC_no_outliers <- subset(LAIC, !ID %in% outlier_IDs)

"Model 1 no outliers"
science_rejection_no_outliers_v1 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3+necontrol1
Conspiracionism =~ conspiragen1+conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3
Conservative =~ idconserva1+idconserva2+idconserva4+npe1
Manichaeism =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive + Conservative + Manichaeism
'
science_rejection_no_outliers_v1 <- sem(science_rejection_no_outliers_v1, LAIC_no_outliers, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_no_outliers_v1, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_no_outliers_v1, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_no_outliers_v1, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

"Model 2"
science_rejection_no_outliers_v2 <- '
# 1º Latent variables
science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3+necontrol1
Conspiracionism =~ conspiragen1+conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
science_rejection ~ Progressive + Conspiracionism + Critical_thinking

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
conspiragen1~~conspiragen2
Nu_25r~~Nu_26r
menteab3~~Universalismo
pensacrit3~~Universalismo
pensacrit2~~menteab3
Nu_8r~~Nu_13r
'
science_rejection_no_outliers_v2 <- sem(science_rejection_no_outliers_v2, LAIC_no_outliers, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_no_outliers_v2, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_no_outliers_v2, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_no_outliers_v2, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
##########################################################################################################################

"Science Rejection SEM Models"
rm(df_scales,science_reject_variables,df_model_items,univariate_normality,outlier_detection,model_descriptives,age_education_table)
#CLICK TO FOLD/UNFOLD#####################################################################################################
"Model 1"
science_rejection_v1 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3+necontrol1
Conspiracionism =~ conspiragen1+conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3
Conservative =~ idconserva1+idconserva2+idconserva4+npe1
Manichaeism =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive + Conservative + Manichaeism
'
science_rejection_v1 <- sem(science_rejection_v1, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v1, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v1, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v1, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

labels <- c(Nu_1r = "SR1",
            Nu_8r = "SR2",
            Nu_13r = "SR3",
            Nu_25r = "SR4",
            Nu_26r = "SR5",
            Nu_34r = "SR6",
            pensacrit2 = "CT1",
            pensacrit3 = "CT2",
            pensacrit4 = "CT3",
            menteab3 = "CT4",
            necontrol1 = "CT5",
            conspiragen1 = "Cons1",
            conspiragen2 = "Cons2",
            conspiracien1 = "Cons3",
            conspiracien2 = "Cons4",
            Universalismo = "P1",
            npe3 = "P2",
            npe2 = "P3",
            idprogre2 = "P4",
            idprogre3 = "P5",
            idconserva1 = "C1",
            idconserva2 = "C2",
            idconserva4 = "C3",
            npe1 = "C4",
            polariza1 = "M1",
            polariza2 = "M2",
            polariza3 = "M3",
            dogmat1 = "M4",
            dogmat2 = "M5",
            dogmat3 = "M6",
            Science_rejection='Science Rejection',
            Critical_thinking='Critical Thinking',
            Conspiracionism='Conspiracionism',
            Progressive='Progressive',
            Conservative='Conservative',
            Manichaeism='Manichaeism')

path<- semPaths(science_rejection_v1, what = "std",
                residuals = FALSE, 
                edge.color = "black", 
                fade = FALSE, 
                nCharNodes = 0, 
                sizeMan = 4, 
                sizeMan2 = 1.5,
                sizeLat = 7.5,
                esize = 2, 
                edge.label.cex = 0.6,
                label.cex = 1,
                label.prop = 0.9,
                exoCov = F,
                edge.width = 0.8,
                rotation = 2)
path_2 <- mark_sig(path, science_rejection_v1)
path_3 <- change_node_label(path_2, labels)

curve_list_1 <- c('Critical Thinking ~~ Conspiracionism' = 15)
path_4 <- set_curve(path_3, curve_list_1)
plot(path_4)
legend("bottomleft",
       legend = "*=p<0.05 **=p<0.01 ***=p<0.001",
       bty = "n",
       cex = 1,
       text.font = 6)
legend("topleft",
       legend = "Figure 1. Science Rejection SEM model 1 (Theoretical model)",
       bty = "n",
       text.font = 6,
       cex = 1.5)

"Model 2"
science_rejection_v2 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3+necontrol1
Conspiracionism =~ conspiragen1+conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Progressive + Conspiracionism + Critical_thinking

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
conspiragen1~~conspiragen2
Nu_25r~~Nu_26r
menteab3~~Universalismo
pensacrit3~~Universalismo
pensacrit2~~menteab3
Nu_8r~~Nu_13r
'
science_rejection_v2 <- sem(science_rejection_v2, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v2, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v2, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v2, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

labels <- c(Nu_1r = 'SR1',
            Nu_8r = 'SR2',
            Nu_13r = 'SR3',
            Nu_25r = 'SR4',
            Nu_26r = 'SR5',
            Nu_34r = 'SR6',
            pensacrit2 = 'CT1',
            pensacrit3 = 'CT2',
            pensacrit4 = 'CT3',
            menteab3 = 'CT4',
            necontrol1 = 'CT5',
            conspiragen1 = 'Cons1',
            conspiragen2 = 'Cons2',
            conspiracien1 = 'Cons3',
            conspiracien2 = 'Cons4',
            Universalismo = 'P1',
            npe3 = 'P2',
            npe2 = 'P3',
            idprogre2 = 'P4',
            idprogre3 = 'P5',
            Science_rejection='Science Rejection',
            Critical_thinking='Critical Thinking',
            Conspiracionism='Conspiracionism',
            Progressive='Progressive')

path<- semPaths(science_rejection_v2, what = 'std',
                residuals = FALSE, 
                edge.color = 'black', 
                fade = FALSE, 
                nCharNodes = 0, 
                sizeMan = 7, 
                sizeMan2 = 2.3,
                sizeLat = 7,
                esize = 2, 
                edge.label.cex = 0.6,
                label.cex = 1,
                label.prop = 0.9,
                exoCov = F,
                edge.width = 0.8,
                rotation = 2)
path_2 <- mark_sig(path, science_rejection_v2)
path_3 <- change_node_label(path_2, labels)

curve_list_1 <- c('CT1 ~~ CT4' = 2.8,'CT1 ~~ CT3' = 1.7,'P2 ~~ P3' = 1.7,'P1 ~~ P5' = 3,
                  'P1 ~~ CT2' = 4,'P1 ~~ CT4' = 3,'Cons1 ~~ Cons2' = 1.7)
path_4 <- set_curve(path_3, curve_list_1)
plot(path_4)
legend("bottomleft",
       legend = "*=p<0.05 **=p<0.01 ***=p<0.001",
       bty = "n",
       cex = 1,
       text.font = 6)
legend("topleft",
       legend = "Figure 2. Science Rejection SEM model 2",
       bty = "n",
       text.font = 6,
       cex = 1.5)
##########################################################################################################################
