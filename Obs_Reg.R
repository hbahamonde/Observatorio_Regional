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
dat$p36 = factor(dat$p36,levels(dat$p36)[c(3,2,1)])



# p25: aporte de partidos a la region
dat$T_p25_5 = as.character(dat$T_p25_5)
dat$T_p25_5[dat$T_p25_5 == "NO SABE (NO LEER)"] <- NA
dat$T_p25_5[dat$T_p25_5 == "NO RESPONDE (NO LEER)"] <- NA
dat$T_p25_5 = as.factor(dat$T_p25_5)
dat$T_p25_5 = c(3,4,1,2)[as.numeric(dat$T_p25_5)]


# p37: confianza en las personas (0: no; 1 = si)
dat$p37 = as.character(dat$p37)
dat$p37[dat$p37 == "NO SABE / NO RESPONDE"] <- NA
dat$p37 = as.factor(dat$p37)
dat$p37 = c(0,1)[as.numeric(dat$p37)]



# p44 : edad
dat$p44

# p3 : cambiarse a otro lugar (0: no; 1: si)
dat$p3
dat$p3 = c(1,0)[as.numeric(dat$p3)]


# p23_1 : estado/mercado (redistributivo: 0 NO, 1 SI)
dat$p23_1 = as.character(dat$p23_1)
dat$p23_1[dat$p23_1 == "NO SABE (NO LEER)"] <- NA
dat$p23_1[dat$p23_1 == "NO RESPONDE (NO LEER)"] <- NA
dat$p23_1 = factor(dat$p23_1)
dat$p23_1 = c(0,1)[as.numeric(dat$p23_1)]


          
# p16 : conflicto = dejar que pase / evitarlos (evitar conflictos: 0 NO, 1 SI)
dat$p16 = as.character(dat$p16)
dat$p16[dat$p16 == "NO SABE (NO LEER)"] <- NA
dat$p16[dat$p16 == "NO RESPONDE  (NO LEER)"] <- NA
dat$p16 = factor(dat$p16)
dat$p16 = c(0,1)[as.numeric(dat$p16)]


# GSE : GSE (riqueza)
dat$GSE
dat$GSE = c(5:1)[as.numeric(dat$GSE)]


# Modelo
p_load(zeligverse)

z.out1 <- zelig(as.factor(p36) ~ # demo/dicta
                  T_p25_5 + # aporte de partidos a la region (poco a mucho)
                  p37 + # confianza en las personas (0: no; 1 = si)
                  p3 + # cambiarse a otro lugar 0: no; 1 = si)
                  p23_1 + # redistributivo (0: no; 1 = si)
                  p16 + # conflicto = evitar conflictos: 0 NO, 1 SI
                  GSE + # riqueza
                  p44, # edad
                model = "mlogit", 
                data = dat, 
                cite = FALSE)

summary(z.out1)

# Sims

set.seed(2207)
n.sim = 5

# aporte de partidos a la region
x.partido.aporta.mucho <- setx(z.out1, T_p25_5 = max(dat$T_p25_5, na.rm=T))
x.partido.aporta.poco <- setx(z.out1, T_p25_5 = min(dat$T_p25_5, na.rm=T))
s.out.aporte.partido <- sim(z.out1, x =  x.partido.aporta.poco, x1 =  x.partido.aporta.mucho, num = n.sim)
par(mar=c(1,1,1,1));plot(s.out.aporte.partido)
# comment: la democracia es preferible cuando los partidos aportan poco. La dictadura es preferible cuando los partidos aportan mucho.
# Es decir, la sociedad (1) quiere a los partidos lejos de la politica, (2) no asocia partidos con democracia, (3) no cree que los partidos sean buenos para la democracia.
# Esto es de las barras.


aporte.partidos.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.aporte.partido$get_qi(qi ="ev", xvalue = "x1")[,3]
    ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Aporte.Partidos = c(
    rep("Nada",n.sim*3),
    rep("Mucho",n.sim*3)
    )
  )

# HERE

p_load(ggplot2)

ggplot(data=aporte.partidos.sim.d) + 
  geom_density(aes(x=Valor.Esperado, group=Region, fill=Region), 
                        alpha=0.5, adjust=2)
gg <- gg + facet_grid(~Region)
gg <- gg + labs("MEI", "Density")
gg <- gg + theme_bw()



ggplot(aporte.partidos.sim.d, aes(Valor.Esperado, fill = Aporte.Partidos)) +
  geom_density(alpha = 0.1) 


# confianza en las personas (0: no; 1 = si)
x.confianza <- setx(z.out1, p37 = max(dat$p37, na.rm=T))
x.desconfianza <- setx(z.out1, p37 = min(dat$p37, na.rm=T))
s.out.confianza <- sim(z.out1, x =  x.desconfianza, x1 =  x.confianza)
par(mar=c(1,1,1,1));plot(s.out.confianza)
# comment: (distribution plots) la confianza sube las preferencias en la democracia. la desconfianza sube la indiferencia y actitudes no-democraticas.
# Esto es especialmente importante donde la gran mayoria no confia en las personas.
table(dat$p37)


# cambiarse (0: no; 1 = si)
x.cambiarse <- setx(z.out1, p3 = max(dat$p3, na.rm=T))
x.no.cambiarse <- setx(z.out1, p3 = min(dat$p3, na.rm=T))
s.out.cambiarse <- sim(z.out1, x =  x.no.cambiarse, x1 =  x.cambiarse)
par(mar=c(1,1,1,1));plot(s.out.cambiarse)
# comment: no hay diferencia. Las gente de todas maneras siente un arraigo con su region, que esta dispuesto a vivir bajo cualquier regimen, pero en su region.

# redistributivo (0: no; 1 = si)
x.redistributivo <- setx(z.out1, p23_1 = max(dat$p23_1, na.rm=T))
x.no.redistributivo <- setx(z.out1, p23_1 = min(dat$p23_1, na.rm=T))
s.out.redistribucion <- sim(z.out1, x =  x.no.redistributivo, x1 =  x.redistributivo)
par(mar=c(1,1,1,1));plot(s.out.redistribucion)
# comment: tener preferencias redistributivas no esta asociado a ser sistematicamente mas democratico. esto es peligroso, en el sentido de que pueda abrir la posibildiad de que candidatos populistas y militaristas ofreciendio redistribucion, podrian ser votados/electos. Tradicionaomente, la democracia ha sido siempre un mecanismo de redistribiccion, pero no en la sexta region.

# conflicto = evitar conflictos: 0 NO, 1 SI
x.permitir.conflicto <- setx(z.out1, p16 = max(dat$p16, na.rm=T))
x.evitar.conflicto <- setx(z.out1, p16 = min(dat$p16, na.rm=T))
s.out.conflicto <- sim(z.out1, x =  x.evitar.conflicto, x1 =  x.permitir.conflicto)
par(mar=c(1,1,1,1));plot(s.out.conflicto)
# predisposicion al conflicto: no te hace ser ni mas autoritario ni mas democratico. esto significa que los rancaguinos no asocian el conflicto social con algun regimen en particular. el conflicto y la paz, puede darse en ambos, lo que indica que la democracia si es un lugar donde se PUEDAN exhibir fuertes tensiones sociales. Los rancaguinos, en este sentido no son conservadores en el sentido de que la democracia deba estar exenta de conflictos. 

# GSE
x.riqueza <- setx(z.out1, GSE = max(dat$GSE, na.rm=T))
x.pobreza <- setx(z.out1, GSE = min(dat$GSE, na.rm=T))
s.out.GSE <- sim(z.out1, x =  x.pobreza, x1 =  x.riqueza)
par(mar=c(1,1,1,1));plot(s.out.GSE)
# la pobreza sube la indiferencia al regimen politico y baja las preferencias a favor de la democracia.
# Esto habla basicamente de una falta en la confianza de que la democracia es un mecanismo de 
# distribucion economica. Muy por el contrario, los rancaguinos mas ricos tienden a preferir la
# democracia como mecanismo para proteger la riqueza acumulada. Esto podria ser importante para
# poder explicar la contigencia. Sobre todo en un contexto donde la mayoria de los rancaguinos
# no se identifica con el eje clasico de izq/derecha.

# Edad
x.viejo <- setx(z.out1, p44 = max(dat$p44, na.rm=T))
x.joven <- setx(z.out1, p44 = min(dat$p44, na.rm=T))
s.edad <- sim(z.out1, x =  x.joven, x1 =  x.viejo)
par(mar=c(1,1,1,1));plot(s.edad)
# la edad aumenta la indiferencia.

# PANORAMA NO ES ALENTADOR.
