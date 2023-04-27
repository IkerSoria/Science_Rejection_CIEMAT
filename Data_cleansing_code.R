# Data load
load("//cendat/u7443/w2000/Escritorio/CIEMAT/Artículo_Unai/Archivos_gitHub/LAIC.RData")

# Libraries
library(dplyr)
library("Hmisc")
library(scales)
library(psych)
library(lavaan)
library(ltm)
library(QuantPsyc)
library(energy)
library(semPlot)
library(WRS2)
library(semptools)
library("EnvStats")

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
LAIC$antiintel <- rowSums(LAIC[c("popul3", "Nu_45")])
LAIC$conspira_cionismo <- rowSums(LAIC[c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")])
LAIC$creencias <- rowSums(LAIC[c("paranorm1", "paranorm2", "paranorm3", "pseudocien1", "pseudocien2", "pseudocien3")])
LAIC$worldview <- scales::rescale(rowSums(data.frame(LAIC[c("idconserva1", "idconserva2", "idconserva3", "idconserva4","npe1","npe3r","npe2r","Universalismor","idprogre2r","idprogre3r")])), to = c(0, 100))
LAIC$ideologia_multidimensional <- rowSums(LAIC[c("worldview", "Ideología")])

df_model_items <- LAIC[c("Nu_15", "Nu_16", "Nu_36", "Nu_44","polariza1", "polariza2", "polariza3", "dogmat1", "dogmat2", "dogmat3",
                         "popul3", "Nu_45","conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2","paranorm1", "paranorm2", 
                         "paranorm3", "pseudocien1", "pseudocien2", "pseudocien3","worldview", "Ideología")]
df_scales <- LAIC[c("manipula","polarisation","antiintel","conspira_cionismo","creencias","ideologia_multidimensional")]
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

alpha(LAIC[, c("popul3", "Nu_45")])
omega(LAIC[, c("popul3", "Nu_45")], nfactors=1)

alpha(LAIC[, c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")])
omega(LAIC[, c("conspiragen1", "conspiragen2", "conspiracien1", "conspiracien2")],rotate="simplimax")

alpha(LAIC[, c("paranorm1", "paranorm2", "paranorm3", "pseudocien1", "pseudocien2", "pseudocien3")])
omega(LAIC[, c("paranorm1", "paranorm2", "paranorm3", "pseudocien1", "pseudocien2", "pseudocien3")])

alpha(LAIC[, c("worldview", "Ideología")])
omega(LAIC[, c("worldview", "Ideología")], nfactors=1)

# Kendall Rank Coefficient Correlation
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

df <- df %>% mutate(cor_1=case_when(pvals >0.01 & pvals<=0.05 & cor<1 ~ paste(cor, "*",sep = ""),
                                    pvals >0.001 & pvals<=0.01 & cor<1 ~ paste(cor, "**",sep = ""),
                                    pvals <=0.001 & cor<1 ~ paste(cor, "***",sep = "")))

"SEM"
set.seed(2022)

# Multivariate normality tests
mult.norm(df_model_items)$mult.test
mvnorm.etest(df_model_items, R=10000)

"Manipulated science"
manipulated_science <- '
# 1º Latent variables Manipulated Science, Polarisation, Ideology, Anti-intellectualism, Conspiracism and Beliefs
Manipula_lat =~ Nu_15 + Nu_16 + Nu_36 + Nu_44
Polarizacion_lat =~ polariza1 + polariza2 + polariza3 + dogmat1 + dogmat2 + dogmat3
Ideologia_lat =~ worldview + Ideología
Antiintelectualismo_lat =~ Nu_45 + popul3
Conspiracionismo_lat =~ conspiragen1 + conspiragen2 + conspiracien1 + conspiracien2
Creencias_lat =~ pseudocien1 + pseudocien2 + pseudocien3 + paranorm1 + paranorm2 + paranorm3

# 2º Path from Polarisation, Ideology and Conspiracy to Manipulated Science
Manipula_lat ~ Ideologia_lat + Polarizacion_lat + Conspiracionismo_lat

# 3º Covariations
Nu_15 ~~ Nu_44 + Nu_16
polariza2 ~~ dogmat1
polariza1 ~~ polariza3
pseudocien1 ~~ pseudocien3
paranorm1 ~~ paranorm3
pseudocien2 ~~ paranorm2
dogmat2 ~~ popul3
worldview ~~  pseudocien1 + pseudocien3 + dogmat2 + popul3
dogmat3 ~~ paranorm3
polariza1 ~~  conspiragen1
conspiragen2 ~~ paranorm1
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
            Nu_45 = "Nu45",
            popul3 = "P13",
            conspiragen1 = "Cons1",
            conspiragen2 = "Cons2",
            conspiracien1 = "Cons3",
            conspiracien2 = "Cons4",
            pseudocien1 = "Pseudo1",
            pseudocien2 = "Pseudo2",
            pseudocien3 = "Pseudo3",
            paranorm1 = "Para1",
            paranorm2 = "Para2",
            paranorm3 = "Para3",
            Manipula_lat = "Manipulated",
            Polarizacion_lat = "Polarisation",
            Ideologia_lat = "Ideology",
            Antiintelectualismo_lat = "Antiintel",
            Conspiracionismo_lat = "Conspiracy",
            Creencias_lat = "Beliefs")

m <- matrix(c(NA,NA,"Para2",NA,"Cons3",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,"Pseudo2",NA,"Cons4",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,"Para1",NA,"Cons2",NA,NA,NA,"Nu16",NA,
              NA,NA,NA,NA,NA,NA,"Conspiracy",NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"Cons1",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"P1",NA,NA,NA,NA,"Nu15",
              "Beliefs",NA,"Para3",NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"P3",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"D3",NA,NA,NA,NA,NA,
              NA,NA,"Pseudo3",NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"D1",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,"Polarisation",NA,"Manipulated",NA,
              NA,NA,NA,NA,"P2",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,"D2",NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,"Pseudo1",NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,"Nu44",
              NA,NA,"P13",NA,"Worldview",NA,NA,NA,NA,NA,
              "Antiintel",NA,NA,NA,NA,NA,"Ideology",NA,NA,NA,
              NA,NA,NA,NA,NA,NA,NA,NA,"Nu36",NA,
              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
              NA,NA,"Nu45",NA,"Selfpos",NA,NA,NA,NA,NA),
            byrow = TRUE, 34, 10)

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
                layout = m,
                nodeLabels = labels,
                exoCov = F,
                edge.width = 0.8)
path_2 <- mark_sig(path, manipulated_science_model)

my_curve_list_1 <- c("Cons1 ~~ P1" = 6.3)
p_pa3 <- set_curve(path_2, my_curve_list_1)

my_curve_list_2 <- c("Nu_16 ~~ Nu_15" = -1)
p_pa4 <- set_curve(p_pa3, my_curve_list_2)

my_curve_list_3 <- c("Para2 ~~ Pseudo2" = -6)
p_pa5 <- set_curve(p_pa4, my_curve_list_3)

my_curve_list_4 <- c("Pseudo3 ~~ Pseudo1" = 0)
p_pa6 <- set_curve(p_pa5, my_curve_list_4)

my_curve_list_5 <- c("P3 ~~ P1" = -9.4)
p_pa7 <- set_curve(p_pa6, my_curve_list_5)

my_curve_list_6 <- c("D1 ~~ P2" = 9.4)
p_pa8 <- set_curve(p_pa7, my_curve_list_6)
plot(p_pa8)
