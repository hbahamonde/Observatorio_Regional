cat("\014")
rm(list=ls())
graphics.off()


if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(foreign)
dat <- data.frame(read.spss("/Users/hectorbahamonde/RU/research/Observatorio_Regional/data.sav"))



# Optica
## Tratar de encontrar asociacion estadistica para "apoyo a la democracia".
## La idea es tratar de explicar apoyo a quiebre democratico ante crisis.
## Es decir, una vez explota una crisis social, cual es el apoyo irrestricto
## a las instituciones democraticas? 


# Data Prep


# p36: demo/dicta
dat$p36 = as.character(dat$p36)
dat$p36[dat$p36 == "NO SABE "] <- NA
dat$p36[dat$p36 == "NO RESPONDE"] <- NA
dat$p36 = as.factor(dat$p36)

# p25: aporte de partidos a la region
dat$T_p25_5 = as.character(dat$T_p25_5)
dat$T_p25_5[dat$T_p25_5 == "NO SABE (NO LEER)"] <- NA
dat$T_p25_5[dat$T_p25_5 == "NO RESPONDE (NO LEER)"] <- NA
dat$T_p25_5 = as.numeric(as.factor(dat$T_p25_5))

# p37: confianza en las personas
dat$p37 = as.character(dat$p37)
dat$p37[dat$p37 == "NO SABE / NO RESPONDE"] <- NA
dat$p37 = as.factor(dat$p37)

# p44 : edad
dat$p44

# p3 : cambiarse a otro lugar
dat$p3

# p23_1 : estado/mercado
dat$p23_1 = as.character(dat$p23_1)
dat$p23_1[dat$p23_1 == "NO SABE (NO LEER)"] <- NA
dat$p23_1[dat$p23_1 == "NO RESPONDE (NO LEER)"] <- NA
dat$p23_1[dat$p23_1 == as.factor(dat$p23_1)





