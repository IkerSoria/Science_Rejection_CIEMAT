"Data and library load"
#CLICK TO FOLD/UNFOLD#####################################################################################################
load('/LAIC.RData') # Add path here.
"
install.packages(c('Hmisc','scales','psych','lavaan','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','doParallel'))
"
lapply(c('Hmisc','scales','psych','lavaan','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','doParallel'), 
       require, character.only = TRUE)
options(max.print=999999)
set.seed(2024)
"Functions"
reorder11 <- function(x) {
  lookup_table <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  lookup_table[x + 1]
}
##########################################################################################################################

"Data processing"
#CLICK TO FOLD/UNFOLD#####################################################################################################
LAIC$critical_thinking <- rowSums(LAIC[c('pensacrit2','pensacrit3','pensacrit4','menteab3','pensalog1')])
LAIC$conspiracionism <- rowSums(LAIC[c('conspiragen2','conspiracien1','conspiracien2')])
LAIC$progressive <- rowSums(LAIC[c('Universalismo','npe3','npe2','idprogre2','idprogre3')])
LAIC$conservative <- rowSums(LAIC[c('idconserva1','idconserva2','idconserva4','npe1')])
LAIC$polarized_thinking <- rowSums(LAIC[c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')])
df_scales <- LAIC[c('critical_thinking','conspiracionism','progressive','conservative','polarized_thinking')]

LAIC$age6 <- cut(LAIC$Edad, breaks = c(0, 2, 4, 6, 8, 10, Inf), labels = c(1, 2, 3, 4, 5, 6), include.lowest = TRUE)
LAIC$education5 <- cut(LAIC$Nivel_Estudio,breaks = c(-Inf, 2, 5, 7, 8, 10, Inf),labels = c(1, 2, 3, 4, 5, 99),include.lowest = TRUE)

science_reject_variables <- as.data.frame(apply(LAIC[c('Nu_1','Nu_8','Nu_13','Nu_25','Nu_26','Nu_34')], 2, reorder11))
colnames(science_reject_variables) <- c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')
LAIC <- cbind(LAIC, science_reject_variables)

df_model_items <- LAIC[c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r','pensacrit2','pensacrit3','pensacrit4','menteab3','conspiragen2',
                         'conspiracien1','conspiracien2','Universalismo','npe3','npe2','idprogre2','idprogre3','idconserva1','idconserva2',
                         'idconserva4','npe1','polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')]
##########################################################################################################################

"Descriptive statistics"
#CLICK TO FOLD/UNFOLD#####################################################################################################
# Distribution of age and education level in the sample
age_education_table <- data.frame(sapply(LAIC[, c(132:133)], table))
# Mean, NMAD, Min and Max
model_descriptives <- data.frame(sapply(df_model_items, function(x) mean(x, trim=0.2)), sapply(df_model_items, mad), sapply(df_model_items, min),
                                 sapply(df_model_items, max))
model_descriptives <- cbind(index = row.names(model_descriptives), model_descriptives)
names(model_descriptives) <- c('Items','Trimmed mean*','MAD','Min','Max')
row.names(model_descriptives) <- NULL
# Distribution of the Science Rejection indicator
Science_rejection <- data.frame(sapply(LAIC[, c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')],table))
# Alpha and Omega
Alpha_omega <- data.frame()
Alpha_omega[1,1] <- round(psych::alpha(LAIC[, c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[1,2] <- round(omega(LAIC[, c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r')])[['omega.tot']], digits = 2)
Alpha_omega[2,1] <- round(psych::alpha(LAIC[, c('pensacrit2','pensacrit3','pensacrit4','menteab3')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[2,2] <- round(omega(LAIC[, c('pensacrit2','pensacrit3','pensacrit4','menteab3')])[['omega.tot']], digits = 2)
Alpha_omega[3,1] <- round(psych::alpha(LAIC[, c('conspiragen2','conspiracien1','conspiracien2')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[3,2] <- round(omega(LAIC[, c('conspiragen2','conspiracien1','conspiracien2')])[['omega.tot']], digits = 2)
Alpha_omega[4,1] <- round(psych::alpha(LAIC[, c('Universalismo','npe3','npe2','idprogre2','idprogre3')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[4,2] <- round(omega(LAIC[, c('Universalismo','npe3','npe2','idprogre2','idprogre3')],rotate='Promax')[['omega.tot']], digits = 2)
Alpha_omega[5,1] <- round(psych::alpha(LAIC[, c('idconserva1','idconserva2','idconserva4','npe1')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[5,2] <- round(omega(LAIC[, c('idconserva1','idconserva2','idconserva4','npe1')])[['omega.tot']], digits = 2)
Alpha_omega[6,1] <- round(psych::alpha(LAIC[, c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')], check.keys=TRUE)[['total']][['std.alpha']], digits = 2)
Alpha_omega[6,2] <- round(omega(LAIC[, c('polariza1','polariza2','polariza3','dogmat1','dogmat2','dogmat3')])[['omega.tot']], digits = 2)
colnames(Alpha_omega) <- c('Alpha', 'Omega')
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
    cor_test_result <- cor.test(df_model_items[, i], df_model_items[, j], method = 'kendall')
    cor_row[j] <- ifelse(cor_test_result$p.value <= 0.001, sprintf('%.3f***', cor_test_result$estimate),
                         ifelse(cor_test_result$p.value <= 0.01, sprintf('%.3f**', cor_test_result$estimate),
                                ifelse(cor_test_result$p.value <= 0.05, sprintf('%.3f*', cor_test_result$estimate),
                                       sprintf('%.3f', cor_test_result$estimate))))
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
    cor_test_result <- cor.test(df_scales[, i], df_scales[, j], method = 'kendall')
    cor_row[j] <- ifelse(cor_test_result$p.value <= 0.001, sprintf('%.3f***', cor_test_result$estimate),
                         ifelse(cor_test_result$p.value <= 0.01, sprintf('%.3f**', cor_test_result$estimate),
                                ifelse(cor_test_result$p.value <= 0.05, sprintf('%.3f*', cor_test_result$estimate),
                                       sprintf('%.3f', cor_test_result$estimate))))
  }
  return(cor_row)
}
# Stop the parallel backend
stopCluster(cl)
# Create a new dataframe with the correlation coefficients and significance markings
correlations_scales <- as.data.frame(cor_matrix)
correlations_scales[upper.tri(correlations_scales, diag = T)] <- NA
# Set column and row names
colnames(correlations_scales) <- var_names
rownames(correlations_scales) <- var_names
correlations_scales <- cbind(index = row.names(correlations_scales), correlations_scales)
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
vif_critical_thinking <- lm(binary ~ pensacrit2+pensacrit3+pensacrit4+menteab3, data=LAIC)
vif(vif_critical_thinking)
vif_conspiracionism <- lm(binary ~ conspiragen2+conspiracien1+conspiracien2, data=LAIC)
vif(vif_conspiracionism)
vif_progressive <- lm(binary ~ Universalismo+npe3+npe2+idprogre2+idprogre3, data=LAIC)
vif(vif_progressive)
vif_conservative <- lm(binary ~ idconserva1+idconserva2+idconserva4+npe1, data=LAIC)
vif(vif_conservative)
vif_polarized_thinking <- lm(binary ~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3, data=LAIC)
vif(vif_polarized_thinking)

# Outlier testing
outlier_IDs <- c(149,225,463,163,304,589,136,150,551,124,237,515,530,605,964,236,248,530,75,82,350)
LAIC_no_outliers <- subset(LAIC, !ID %in% outlier_IDs)

"Model 1 no outliers"
science_rejection_no_outliers_v1 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3
Conservative =~ idconserva1+idconserva2+idconserva4+npe1
Polarized_thinking =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive + Conservative + Polarized_thinking
'
science_rejection_no_outliers_v1 <- sem(science_rejection_no_outliers_v1, LAIC_no_outliers, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_no_outliers_v1, ci=T, level=0.95, boot.ci.type='perc', standardized=T)
summary(science_rejection_no_outliers_v1, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_no_outliers_v1, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

"Model 2"
science_rejection_no_outliers_v2 <- '
# 1º Latent variables
science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
science_rejection ~ Progressive + Conspiracionism + Critical_thinking

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
Nu_25r~~Nu_26r
pensacrit2~~menteab3
Nu_8r~~Nu_13r
Nu_8r~~Nu_26r
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
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3
Conservative =~ idconserva1+idconserva2+idconserva4+npe1
Polarized_thinking =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive + Conservative + Polarized_thinking
'
science_rejection_v1 <- sem(science_rejection_v1, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v1, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v1, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v1, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

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
            conspiragen2 = 'Cons1',
            conspiracien1 = 'Cons2',
            conspiracien2 = 'Cons3',
            Universalismo = 'P1',
            npe3 = 'P2',
            npe2 = 'P3',
            idprogre2 = 'P4',
            idprogre3 = 'P5',
            idconserva1 = 'C1',
            idconserva2 = 'C2',
            idconserva4 = 'C3',
            npe1 = 'C4',
            polariza1 = 'PT1',
            polariza2 = 'PT2',
            polariza3 = 'PT3',
            dogmat1 = 'PT4',
            dogmat2 = 'PT5',
            dogmat3 = 'PT6',
            Science_rejection='Science Rejection',
            Critical_thinking='Critical Thinking',
            Conspiracionism='Conspiracy Thinking',
            Progressive='Progressive',
            Conservative='Conservative',
            Polarized_thinking='Polarized Thinking')

m<-matrix(c('PT1',	NA,	NA,	NA,	NA,	NA,	NA,
            'PT2',	NA,	NA,	NA,	NA,	NA,	NA,
            'PT3',	NA,	NA,	NA,	NA,	NA,	NA,
            NA,	NA,	'Polarized Thinking',	NA,	NA,	NA,	NA,
            'PT4',	NA,	NA,	NA,	NA,	NA,	NA,
            'PT5',	NA,	NA,	NA,	NA,	NA,	NA,
            'PT6',	NA,	NA,	NA,	NA,	NA,	NA,
            'C1',	NA,	NA,	NA,	NA,	NA,	'SR1',
            'C2',	NA,	NA,	NA,	NA,	NA,	NA,
            NA,	NA,	'Conservative',	NA,	NA,	NA,	'SR2',
            'C3',	NA,	NA,	NA,	NA,	NA,	NA,
            'C4',	NA,	NA,	NA,	NA,	NA,	'SR3',
            'P1',	NA,	NA,	NA,	NA,	NA,	NA,
            'P2',	NA,	NA,	NA,	'Science Rejection',	NA,	NA,
            'P3',	NA,	'Progressive',	NA,	NA,	NA,	NA,
            'P4',	NA,	NA,	NA,	NA,	NA,	'SR4',
            'P5',	NA,	NA,	NA,	NA,	NA,	NA,
            'Cons1',	NA,	NA,	NA,	NA,	NA,	'SR5',
            'Cons2',	NA,	'Conspiracy Thinking',	NA,	NA,	NA,	NA,
            'Cons3',	NA,	NA,	NA,	NA,	NA,	'SR6',
            'CT1',	NA,	NA,	NA,	NA,	NA,	NA,
            'CT2',	NA,	NA,	NA,	NA,	NA,	NA,
            'CT3',	NA,	'Critical Thinking',	NA,	NA,	NA,	NA,
            'CT4',	NA,	NA,	NA,	NA,	NA,	NA),byrow=TRUE,24,7)

path<- semPaths(science_rejection_v1, what = 'std',
                nodeLabels = labels,
                layout = m,
                residuals = FALSE, 
                edge.color = 'black', 
                fade = FALSE, 
                nCharNodes = 0, 
                sizeMan = 4, 
                sizeMan2 = 1.5,
                sizeLat = 8.5,
                esize = 2, 
                edge.label.cex = 0.6,
                label.cex = 1,
                label.prop = 0.9,
                exoCov = F,
                edge.width = 0.8,
                rotation = 2)
path_2 <- mark_sig(path, science_rejection_v1)
plot(path_2)
legend('bottomleft',
       legend = '*=p<0.05 **=p<0.01 ***=p<0.001',
       bty = 'n',
       cex = 1,
       text.font = 6)
legend('topleft',
       legend = 'Figure 1. Science Rejection SEM model 1 (Theoretical model)',
       bty = 'n',
       text.font = 6,
       cex = 1.5)


"Iterations"
# First we drop the non significant variables
science_rejection_v2 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive
'
science_rejection_v2 <- sem(science_rejection_v2, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v2, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v2, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v2, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))


# Suggested correlations
ideas <- data.frame(modindices(science_rejection_v2)[order(modindices(science_rejection_v2)$mi, decreasing = TRUE), ])


# Add correlations
science_rejection_v3 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive

# Covariances
npe3~~npe2
'
science_rejection_v3 <- sem(science_rejection_v3, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v3, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v3, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v3, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
ideas <- data.frame(modindices(science_rejection_v3)[order(modindices(science_rejection_v3)$mi, decreasing = TRUE), ])


science_rejection_v4 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
'
science_rejection_v4 <- sem(science_rejection_v4, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v4, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v4, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v4, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
ideas <- data.frame(modindices(science_rejection_v4)[order(modindices(science_rejection_v4)$mi, decreasing = TRUE), ])


science_rejection_v5 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
'
science_rejection_v5 <- sem(science_rejection_v5, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v5, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v5, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v5, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
ideas <- data.frame(modindices(science_rejection_v5)[order(modindices(science_rejection_v5)$mi, decreasing = TRUE), ])


science_rejection_v6 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3

pensacrit2~~menteab3
###Nu_25r~~Nu_26r
'
science_rejection_v6 <- sem(science_rejection_v6, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v6, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v6, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v6, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
ideas <- data.frame(modindices(science_rejection_v6)[order(modindices(science_rejection_v6)$mi, decreasing = TRUE), ])


science_rejection_v7 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Critical_thinking + Conspiracionism + Progressive

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
pensacrit2~~menteab3
Nu_25r~~Nu_26r
'
science_rejection_v7 <- sem(science_rejection_v7, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v7, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v7, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v7, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))
ideas <- data.frame(modindices(science_rejection_v7)[order(modindices(science_rejection_v7)$mi, decreasing = TRUE), ])


"Final model"
science_rejection_v8 <- '
# 1º Latent variables
Science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
Critical_thinking =~ pensacrit2+pensacrit3+pensacrit4+menteab3
Conspiracionism =~ conspiragen2+conspiracien1+conspiracien2
Progressive =~ Universalismo+npe3+npe2+idprogre2+idprogre3

# 2º Path
Science_rejection ~ Progressive + Conspiracionism + Critical_thinking

# Covariances
npe3~~npe2
pensacrit2~~pensacrit4
Universalismo~~idprogre3
Nu_25r~~Nu_26r
pensacrit2~~menteab3
Nu_8r~~Nu_13r
Nu_8r~~Nu_26r
'
science_rejection_v8 <- sem(science_rejection_v8, LAIC, se='bootstrap', bootstrap=10000)
parameterEstimates(science_rejection_v8, ci=TRUE, level=0.95, boot.ci.type='perc', standardized = TRUE)
summary(science_rejection_v8, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_v8, c('cfi', 'tli', 'RMSEA', 'srmr', 'gfi', 'agfi'))

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
            conspiragen2 = 'Cons1',
            conspiracien1 = 'Cons2',
            conspiracien2 = 'Cons3',
            Universalismo = 'P1',
            npe3 = 'P2',
            npe2 = 'P3',
            idprogre2 = 'P4',
            idprogre3 = 'P5',
            Science_rejection='Science Rejection',
            Critical_thinking='Critical Thinking',
            Conspiracionism='Conspiracy Thinking',
            Progressive='Progressive')

path<- semPaths(science_rejection_v8, what = 'std',
                residuals = FALSE, 
                edge.color = 'black', 
                fade = FALSE, 
                nCharNodes = 0, 
                sizeMan = 7, 
                sizeMan2 = 2.3,
                sizeLat = 12,
                esize = 2, 
                edge.label.cex = 0.6,
                label.cex = 1,
                label.prop = 0.9,
                exoCov = F,
                edge.width = 0.8,
                rotation = 2)
path_2 <- mark_sig(path, science_rejection_v8)
path_3 <- change_node_label(path_2, labels)

curve_list_1 <- c('P2 ~~ P3'=1.7,'CT1 ~~ CT3'=3,'P1 ~~ P5'=3,'SR4 ~~ SR5'=-2,
                  'CT1 ~~ CT4'=1.7,'SR2 ~~ SR3'=-2)
path_4 <- set_curve(path_3, curve_list_1)
plot(path_4)
legend('bottomleft',
       legend = '*=p<0.05 **=p<0.01 ***=p<0.001',
       bty = 'n',
       cex = 1,
       text.font = 6)
legend('topleft',
       legend = 'Figure 2. Science Rejection SEM model 2',
       bty = 'n',
       text.font = 6,
       cex = 1.5)
##########################################################################################################################