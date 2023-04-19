# Load of raw data
load("//cendat/u7443/w2000/Escritorio/CIEMAT/Artículo_Unai/Archivos_gitHub/BBDD_LAIC.RData")
 
# Libraries
library(dplyr)

# Functions
reorder <- function(x) {ifelse(x == 0, 10, 
                                 ifelse(x == 1, 9, 
                                        ifelse(x == 2, 8, 
                                               ifelse(x == 3, 7, 
                                                      ifelse(x == 4, 6, 
                                                             ifelse(x == 5, 5, 
                                                                    ifelse(x == 6, 4, 
                                                                           ifelse(x == 7, 3, 
                                                                                  ifelse(x == 8, 2, 
                                                                                         ifelse(x == 9, 1, 0))))))))))}
cambia99 <- function(x) {ifelse(x == 99, 0, x)}

# Data processing
nucleo_neg_rev <- as.data.frame(apply(select(BBDD_LAIC, 5, 7, 8, 11, 16:18, 21, 22, 24, 30, 31, 33, 37, 44:46, 52, 54, 57, 58), 2, reorder))
names(nucleo_neg_rev) <- c("Nu_4r", "Nu_6r", "Nu_7r", "Nu_10r", "Nu_15r", "Nu_16r", "Nu_17r", "Nu_20r", "Nu_21r", "Nu_23r", "Nu_29r", "Nu_30r", 
                           "Nu_32r", "Nu_36r", "Nu_43r", "Nu_44r", "Nu_45r", "Nu_51r", "Nu_53r", "Nu_56r", "Nu_57r")
nucleo_pos <- select(BBDD_LAIC, 2, 4, 6, 9, 10, 12, 14, 19, 20, 23, 25:29, 32, 34:36, 38:43, 47, 48, 50, 51, 53, 55, 56)
LAIC <- data.frame(BBDD_LAIC[1], nucleo_pos, nucleo_neg_rev, BBDD_LAIC[c(59:157)])

LAIC[c(2:117)][LAIC[c(2:117)] == 99] <- NA
LAIC <- na.omit(LAIC)
LAIC <- select(LAIC, -(c(36, 43, 51)))
LAIC[131:150] <- apply(LAIC[131:150], 2, cambia99)

LAIC$Nu_45 <- reorder(LAIC$Nu_45r)

LAIC <- data.frame(LAIC[c(1:51, 56:60, 64:70, 80:82, 96:98, 105, 52, 61:63, 99:101, 109:111, 104, 94, 72, 77:79, 84, 85, 113, 114, 90, 91, 71, 75)],
                   LAIC[112],LAIC[86:89],LAIC[115:120], LAIC[126:133], LAIC[135:136], LAIC[139:143], LAIC[,c(144,145,146,149,150)], LAIC[151])
names(LAIC)[c(83,84,85,86,93,94,121:125)] <- c("Conformidad", "Benevolencia", "Universalismo", "Seguridad", "Poder", "Logro","Racional1", "Racional2", 
                                               "Racional3", "Racional4", "Racional5")
save(LAIC, file = "//cendat/u7443/w2000/Escritorio/CIEMAT/Artículo_Unai/Archivos_gitHub/LAIC.RData")