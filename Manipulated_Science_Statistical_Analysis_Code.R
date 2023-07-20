# Data load
load("//cendat/u7443/w2000/Escritorio/CIEMAT/Artículo_Unai/Archivos_gitHub/LAIC.RData")

# Libraries
lapply(c('dplyr','Hmisc','scales','psych','lavaan','ltm','QuantPsyc','energy','semPlot','WRS2','semptools','EnvStats','car'), 
       require, character.only = TRUE)

# Functions
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

# Data processing
LAIC["Ideología"][LAIC["Ideología"] == 99] <- NA
LAIC <- LAIC %>% filter(Ideología < 99)

manipula_variables <- as.data.frame(apply(LAIC[c("Nu_15r", "Nu_16r", "Nu_36r", "Nu_44r")], 2, reorder11))
colnames(manipula_variables) <- c("Nu_15", "Nu_16", "Nu_36", "Nu_44")
LAIC <- cbind(LAIC, manipula_variables)

worldview_variables <- as.data.frame(apply(LAIC[c("npe3","npe2","Universalismo","idprogre2","idprogre3")], 2, reorder11))
colnames(worldview_variables) <- c("npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")
LAIC <- cbind(LAIC, worldview_variables)

LAIC$manipula <- rowSums(LAIC[c("Nu_15", "Nu_16", "Nu_36", "Nu_44")])
LAIC$polarisation <- rowSums(LAIC[c("polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3")])
LAIC$conspira_cionismo <- rowSums(LAIC[c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")])
LAIC$worldview <- scales::rescale(rowSums(data.frame(LAIC[c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")])), to = c(0, 100))
LAIC$ideologia_multidimensional <- rowSums(LAIC[c("worldview", "Ideología")])

df_model_items <- LAIC[c("Nu_15", "Nu_16", "Nu_36", "Nu_44","polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3",
                         "conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2","worldview", "Ideología")]
df_scales <- LAIC[c("manipula","polarisation","conspira_cionismo","ideologia_multidimensional")]
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

# Descriptive statistics
univariate_normality <- data.frame(sapply(df_model_items, shapiro.test)[1:2,],sapply(df_worldview, shapiro.test)[1:2,])
outlier_detection <- data.frame(sapply(df_model_items, rosnerTest)[13,],sapply(df_worldview, rosnerTest)[13,])

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

alpha(LAIC[, c("Nu_15", "Nu_16", "Nu_36", "Nu_44")])
omega(LAIC[, c("Nu_15", "Nu_16", "Nu_36", "Nu_44")])

alpha(LAIC[, c("polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3")])
omega(LAIC[, c("polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3")])

alpha(LAIC[, c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")], check.keys=TRUE)
omega(LAIC[, c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")])

alpha(LAIC[, c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")])
omega(LAIC[, c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")],rotate="simplimax")

alpha(LAIC[, c("worldview", "Ideología")])
omega(LAIC[, c("worldview", "Ideología")], nfactors=1)

# Kendall Rank Coefficient Correlation of the scales
idx <- expand.grid(colnames(df_scales), colnames(df_scales))
pvals <- apply(idx, 1, function(i){
  x <- df_scales[,i[[1]]]
  y <- df_scales[,i[[2]]]
  cor.test(x, y, method = "kendall")$p.value
})
pvals <- cbind.data.frame(idx, pvals = p.adjust(pvals, "fdr"))
cors <- cor(df_scales, method = "kendall")
cors <- reshape2::melt(cors)

if (identical(cors[,1:2], pvals[,1:2])) {
  df <- cbind.data.frame(pvals, cor = cors[,3])
}

df$cor <- as.character(round(df$cor ,digit=2))

df <- df %>% mutate(cor_1=case_when(pvals >0.05 & cor<1 ~ paste(cor, "",sep = ""),
                                    pvals >0.01 & pvals<=0.05 & cor<1 ~ paste(cor, "*",sep = ""),
                                    pvals >0.001 & pvals<=0.01 & cor<1 ~ paste(cor, "**",sep = ""),
                                    pvals <=0.001 & cor<1 ~ paste(cor, "***",sep = "")))

"SEM"
set.seed(2022)

# Multivariate normality tests
mult.norm(df_model_items)$mult.test
mvnorm.etest(df_model_items, R=10000)

# Multicolinearity testing with correlations between variables using the Kendall Rank Coefficient Correlation
idx <- expand.grid(colnames(df_model_items), colnames(df_model_items))
pvals <- apply(idx, 1, function(i){
  x <- df_model_items[,i[[1]]]
  y <- df_model_items[,i[[2]]]
  cor.test(x, y, method = "kendall")$p.value
})
pvals <- cbind.data.frame(idx, pvals = p.adjust(pvals, "fdr"))
cors <- cor(df_model_items, method = "kendall")
cors <- reshape2::melt(cors)

if (identical(cors[,1:2], pvals[,1:2])) {
  df <- cbind.data.frame(pvals, cor = cors[,3])
}

df$cor <- as.character(round(df$cor ,digit=2))

df <- df %>% mutate(cor_1=case_when(pvals >0.05 & cor<1 ~ paste(cor, "",sep = ""),
                                    pvals >0.01 & pvals<=0.05 & cor<1 ~ paste(cor, "*",sep = ""),
                                    pvals >0.001 & pvals<=0.01 & cor<1 ~ paste(cor, "**",sep = ""),
                                    pvals <=0.001 & cor<1 ~ paste(cor, "***",sep = "")))

# Multicolinearity testing with random binary variable and regressions
LAIC$binary <- rbinom(nrow(LAIC), 1, 0.5)
vif_model_manipula <- lm(binary ~ Nu_15 + Nu_16 + Nu_36 + Nu_44, data=LAIC)
vif(vif_model_manipula)
vif_model_polarizacion <- lm(binary ~ polariza1 + polariza2 + polariza3 + dogmat1 + dogmat2 + dogmat3, data=LAIC)
vif(vif_model_polarizacion)
vif_model_ideologia <- lm(binary ~ worldview + Ideología, data=LAIC)
vif(vif_model_ideologia)
vif_model_conspiracionismo <- lm(binary ~ conspiragen1 + conspiragen2 + conspiracien1 + conspiracien2, data=LAIC)
vif(vif_model_conspiracionismo)

# Outlier testing
outlier_IDs <- c(68,75,221,233,300,324,328,362,498,572,904)
LAIC_no_outliers <- subset(LAIC, !ID %in% outlier_IDs)

manipulated_science_outlier_test <- '
# 1º Latent variables Manipulated Science, Polarisation, Ideology and Conspiracism
Manipula_lat =~ Nu_15 + Nu_16 + Nu_36 + Nu_44
Polarizacion_lat =~ polariza1 + polariza2 + polariza3 + dogmat1 + dogmat2 + dogmat3
Ideologia_lat =~ worldview + Ideología
Conspiracionismo_lat =~ conspiragen1 + conspiragen2 + conspiracien1 + conspiracien2

# 2º Path from Polarisation, Ideology and Conspiracy to Manipulated Science
Manipula_lat ~ Ideologia_lat + Polarizacion_lat + Conspiracionismo_lat

# 3º Covariations
Nu_15 ~~ Nu_44 + Nu_16
polariza2 ~~ dogmat1
worldview ~~ dogmat2
polariza1 ~~  conspiragen1
conspiragen1 ~~ conspiragen2
dogmat1 ~~ dogmat2
worldview ~~ conspiracien1
Ideología ~~  conspiragen1
'
manipulated_science_model <- sem(manipulated_science, LAIC, se="bootstrap", bootstrap=10000)
parameterEstimates(manipulated_science_model, ci=TRUE, level=0.95, boot.ci.type="perc", standardized = TRUE)
summary(manipulated_science_model, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(manipulated_science_model, c("cfi", "tli", "RMSEA", "srmr", "gfi", "agfi"))


"Manipulated science"
manipulated_science <- '
# 1º Latent variables Manipulated Science, Polarisation, Ideology and Conspiracism
Manipula_lat =~ Nu_15 + Nu_16 + Nu_36 + Nu_44
Polarizacion_lat =~ polariza1 + polariza2 + polariza3 + dogmat1 + dogmat2 + dogmat3
Ideologia_lat =~ worldview + Ideología
Conspiracionismo_lat =~ conspiragen1 + conspiragen2 + conspiracien1 + conspiracien2

# 2º Path from Polarisation, Ideology and Conspiracy to Manipulated Science
Manipula_lat ~ Ideologia_lat + Polarizacion_lat + Conspiracionismo_lat

# 3º Covariations
Nu_15 ~~ Nu_44 + Nu_16
polariza2 ~~ dogmat1
worldview ~~ dogmat2
polariza1 ~~  conspiragen1
conspiragen1 ~~ conspiragen2
dogmat1 ~~ dogmat2
worldview ~~ conspiracien1
Ideología ~~  conspiragen1
'
manipulated_science_model <- sem(manipulated_science, LAIC, se="bootstrap", bootstrap=10000)
parameterEstimates(manipulated_science_model, ci=TRUE, level=0.95, boot.ci.type="perc", standardized = TRUE)
summary(manipulated_science_model, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
fitmeasures(manipulated_science_model, c("cfi", "tli", "RMSEA", "srmr", "gfi", "agfi"))

labels <- c(Nu_15 = "Nu15",
            Nu_16 = "Nu16",
            Nu_36 = "Nu36",
            Nu_44 = "Nu44",
            polariza1 = "P1",
            polariza2 = "P2",
            polariza3 = "P3",
            dogmat1 = "D1",
            dogmat2 = "D2",
            dogmat3 = "D3",
            worldview = "Worldview",
            Ideología = "Selfpos",
            conspiragen1 = "Cons1",
            conspiragen2 = "Cons2",
            conspiracien1 = "Cons3",
            conspiracien2 = "Cons4",
            Manipula_lat = "Manipulated",
            Polarizacion_lat = "Dogmatism",
            Ideologia_lat = "Ideology",
            Conspiracionismo_lat = "Conspiracy")

path<- semPaths(manipulated_science_model, what = "std",
                residuals = FALSE, 
                edge.color = "black", 
                fade = FALSE, 
                nCharNodes = 0, 
                sizeMan = 7, 
                sizeMan2 = 2.3,
                sizeLat = 7,
                esize = 2, 
                edge.label.cex = 0.6,
                label.cex = 1,
                label.prop = 0.9,
                exoCov = T,
                edge.width = 0.8)
path_2 <- mark_sig(path, manipulated_science_model)
path_3 <- change_node_label(path_2, labels)
plot(path_3)
legend("bottomleft",
       legend = "*=p<0.05 **=p<0.01 ***=p<0.001",
       bty = "n",
       cex = 0.8,
       text.font = 6)
legend("topleft",
       legend = "Figure 1. Structural equation model predicting manipulated science",
       bty = "n",
       text.font = 6)