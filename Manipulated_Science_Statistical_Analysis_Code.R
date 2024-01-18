# Data load
load("//cendat/u7443/w2000/Escritorio/CIEMAT/Artículo_Unai/Archivos_gitHub/LAIC.RData")
"
install.packages(c('dplyr','Hmisc','scales','psych','lavaan','ltm','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','writexl'))
"

# Libraries
lapply(c('dplyr','Hmisc','scales','psych','lavaan','ltm','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car','writexl'), 
       require, character.only = TRUE)

# Functions
#CLICK TO FOLD/UNFOLD#####################################################################################################
reorder11 <- function(x) {ifelse(x == 0, 10, 
                                 ifelse(x == 1, 9, 
                                        ifelse(x == 2, 8,
                                               ifelse(x == 3, 7, 
                                                      ifelse(x == 4, 6, 
                                                             ifelse(x == 5, 5, 
                                                                    ifelse(x == 6, 4, 
                                                                           ifelse(x == 7, 3, 
                                                                                  ifelse(x == 8, 2, 
                                                                                         ifelse(x == 9, 1, 0))))))))))}

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

"A very handy function to transpose the results of the correlation:"
transpose_and_customize <- function(df) {
  # Initialize a new dataframe to store the transposed data
  transposed_df <- data.frame()
  # Set the number of rows to transpose at a time (48 in this case)
  rows_per_transpose <- 35
  # Calculate the number of iterations required
  num_iterations <- ceiling(nrow(df) / rows_per_transpose)
  # Loop through the dataframe and transpose the required rows at a time
  for (i in 1:num_iterations) {
    start_row <- (i - 1) * rows_per_transpose + 1
    end_row <- min(i * rows_per_transpose, nrow(df))
    # Extract the specified rows from the third column and transpose
    transposed_chunk <- t(df[start_row:end_row, "cor_1"])
    # Convert the transposed chunk into a dataframe and append to the result dataframe
    transposed_df <- rbind(transposed_df, transposed_chunk)
  }
  
  # Add column names to the transposed dataframe
  col_names <- as.character(df[1:35, "Var1"])
  colnames(transposed_df) <- col_names
  return(transposed_df)
}

##########################################################################################################################

# Data processing
#CLICK TO FOLD/UNFOLD#####################################################################################################
LAIC["Ideología"][LAIC["Ideología"] == 99] <- NA
LAIC <- LAIC %>% filter(Ideología < 99)

manipula_variables <- as.data.frame(apply(LAIC[c("Nu_15r", "Nu_16r", "Nu_36r", "Nu_44r")], 2, reorder11))
colnames(manipula_variables) <- c("Nu_15", "Nu_16", "Nu_36", "Nu_44")
LAIC <- cbind(LAIC, manipula_variables)

worldview_variables <- as.data.frame(apply(LAIC[c("npe3","npe2","Universalismo","idprogre2","idprogre3")], 2, reorder11))
colnames(worldview_variables) <- c("npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")
LAIC <- cbind(LAIC, worldview_variables)

LAIC$manipula <- rowSums(LAIC[c("Nu_15", "Nu_16", "Nu_36", "Nu_44")])
LAIC$dogmatism <- rowSums(LAIC[c("polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3")])
LAIC$conspira_cionismo <- rowSums(LAIC[c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")])
LAIC$worldview <- scales::rescale(rowSums(data.frame(LAIC[c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")])), to = c(0, 100))
LAIC$ideologia_multidimensional <- rowSums(LAIC[c("worldview", "Ideología")])
LAIC$critical_thinking <- rowSums(LAIC[c("pensacrit2","pensacrit3","pensacrit4","menteab3","pensalog1","necontrol1")])

df_scales <- LAIC[c("manipula","dogmatism","conspira_cionismo","ideologia_multidimensional")]
df_worldview <- LAIC[c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")]

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

science_reject_variables <- as.data.frame(apply(LAIC[c('Nu_1','Nu_8','Nu_13','Nu_20r','Nu_21r','Nu_25','Nu_26','Nu_34','Nu_43r','Nu_45r','Nu_53r')], 2, reorder11))
colnames(science_reject_variables) <- c('Nu_1r','Nu_8r','Nu_13r','Nu_20','Nu_21','Nu_25r','Nu_26r','Nu_34r','Nu_43','Nu_45','Nu_53')
LAIC <- cbind(LAIC, science_reject_variables)
df_model_items <- LAIC[c('Nu_1r','Nu_8r','Nu_13r','Nu_25r','Nu_26r','Nu_34r','pensacrit2','pensacrit3','pensacrit4','menteab3','pensalog1',
                         'necontrol1','conspiragen1','conspiragen2','conspiracien1','conspiracien2','idconserva2','npe1','idconserva1',
                         'idconserva2','idconserva4','npe1','npe3r','npe2r','Universalismor','idprogre2r','idprogre3r','dogmat2',
                         'polariza1','polariza2','polariza3','dogmat1','dogmat3','idconserva1','idconserva4')]
rm(manipula_variables,worldview_variables,science_reject_variables,df_scales,df_worldview)
##########################################################################################################################

# Descriptive statistics
#CLICK TO FOLD/UNFOLD#####################################################################################################
univariate_normality <- data.frame(sapply(df_model_items, shapiro.test)[1:2,])
outlier_detection <- data.frame(sapply(df_model_items, rosnerTest)[13,])

model_descriptives <- data.frame(sapply(df_model_items, function(x) mean(x, trim=0.2)), sapply(df_model_items, mad), sapply(df_model_items, min),
                                 sapply(df_model_items, max))
model_descriptives <- cbind(index = row.names(model_descriptives), model_descriptives)
names(model_descriptives) <- c("Items","Trimmed mean*","NMAD","Min","Max")
row.names(model_descriptives) <- NULL
worldview_descriptives <- data.frame(sapply(df_worldview[], function(x) mean(x, trim=0.2)), sapply(df_worldview, mad), 
                                     sapply(df_worldview, min),sapply(df_worldview, max))
worldview_descriptives <- cbind(index = row.names(worldview_descriptives), worldview_descriptives)
names(worldview_descriptives) <- c("Items","Trimmed mean*","NMAD","Min","Max")
row.names(worldview_descriptives) <- NULL

age_table <- data.frame(table(LAIC$edad6), prop.table(table(LAIC$edad6)))
education_table <- data.frame(table(LAIC$education5), prop.table(table(LAIC$education5)))
##########################################################################################################################



"SEM Models"

"
Mirar de sacar los supuestos del SEM para este modelo.
Mirar justificaciones como se hace en Nature Human Behaviour

Mirar de sacar el modelo con conservadurismo por un lado y progresismo por otro.
"

#CLICK TO FOLD/UNFOLD#####################################################################################################
# SCIENCE REJECTION MODEL FINAL
science_rejection_model_v3 <- '
# 1º Latent variables Rejection of Science,
science_rejection =~ Nu_1r+Nu_8r+Nu_13r+Nu_25r+Nu_26r+Nu_34r
critical_thinking_lat =~ pensacrit2+pensacrit3+pensacrit4+menteab3+pensalog1+necontrol1
conspiracionism =~ conspiragen1+conspiragen2+conspiracien1+conspiracien2+idconserva2+npe1
ideology_lat =~ idconserva1+idconserva2+idconserva4+npe1+npe3r+npe2r+Universalismor+idprogre2r+idprogre3r+dogmat2
dogmatism_lat =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3+idconserva1+idconserva4

# 2º Path
science_rejection ~ critical_thinking_lat + conspiracionism + ideology_lat + dogmatism_lat

# Covariances
npe3r ~~ npe2r
polariza2 ~~ dogmat1
conspiragen1 ~~ conspiragen2
pensacrit2 ~~ pensacrit4
npe1 ~~ npe2r
npe1 ~~ npe3r
polariza1 ~~ polariza3
Universalismor ~~ idprogre3r
pensacrit2 ~~ menteab3
dogmat2 ~~ dogmat1
'
science_rejection_model_v3 <- sem(science_rejection_model_v3, LAIC, se="bootstrap", bootstrap=100)
#parameterEstimates(science_rejection_model_v2, ci=TRUE, level=0.95, boot.ci.type="perc", standardized = TRUE)
summary(science_rejection_model_v3, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(science_rejection_model_v3, c("cfi", "tli", "RMSEA", "srmr", "gfi", "agfi"))
ideas <- data.frame(modindices(science_rejection_model_v3)[order(modindices(science_rejection_model_v3)$mi, decreasing = TRUE), ])

# Este modelo no tiene sentido, hemos metido dos variables de ideología en el dogmatismo y sencillamente no funcionan.

# Para justificar: science_rejection_model_v3 el "dogmatismo" explica la actitud militante
# hacia la ciencia en el modelo de abajo.

militant_attitude_model <-"
militant_attitude=~Nu_18+Nu_46+Nu_50+Nu_55
critical_thinking_lat =~ pensacrit2+pensacrit3+pensacrit4+menteab3+pensalog1+necontrol1
dogmatism_lat =~ polariza1+polariza2+polariza3+dogmat1+dogmat2+dogmat3

# 2º Path
militant_attitude ~ critical_thinking_lat + dogmatism_lat

# 3º Covariances
Nu_50~~Nu_55
polariza1~~polariza3
menteab3~~dogmat2
pensacrit4~~menteab3
polariza2~~dogmat2
pensalog1~~necontrol1
polariza3~~dogmat3
pensacrit3~~dogmat2
menteab3~~dogmat1
polariza1~~dogmat3
pensacrit2~~dogmat2
pensacrit4~~dogmat2
"
militant_attitude_model <- sem(militant_attitude_model, LAIC)
#parameterEstimates(science_rejection_model_v3, ci=TRUE, level=0.95, boot.ci.type="perc", standardized = TRUE)
summary(militant_attitude_model, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(militant_attitude_model, c("cfi", "tli", "RMSEA", "srmr", "gfi", "agfi"))
ideas <- data.frame(modindices(militant_attitude_model)[order(modindices(militant_attitude_model)$mi, decreasing = TRUE), ])
##########################################################################################################################
