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
levels(dat$p36) <- c("Indiferente", "Dictadura es preferible", "Democracia es preferible")
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
# dat$p44

# p3 : cambiarse a otro lugar (0: no; 1: si)
# dat$p3
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
# dat$GSE
dat$GSE = c(5:1)[as.numeric(dat$GSE)]


####################################################################################
# Modelo
####################################################################################

p_load(zeligverse,MCMCpack)

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

####################################################################################
# Sims
####################################################################################

set.seed(2207)
n.sim = 10000

# Fx to capitalize first letter
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


####################################################################################
# Describir VD
####################################################################################

p_load(ggplot2)
par(mar=c(1,1,1,1))
ggplot(dat, aes(x=dat$p36)) + 
  geom_bar(stat = "count") +
  coord_flip() +
  xlab("") + 
  ylab("Frecuencia") + 
  theme_bw() + 
  ggtitle("¿Con cuál de las siguientes frases acerca de la democracia\nusted está más de acuerdo?") +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20)) +   
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/vd.pdf")

####################################################################################
# aporte de partidos a la region
####################################################################################

x.partido.aporta.mucho <- setx(z.out1, T_p25_5 = max(dat$T_p25_5, na.rm=T))
x.partido.aporta.poco <- setx(z.out1, T_p25_5 = min(dat$T_p25_5, na.rm=T))
s.out.aporte.partido <- sim(z.out1, x =  x.partido.aporta.poco, x1 =  x.partido.aporta.mucho, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.aporte.partido)
# comment: la democracia es preferible cuando los partidos aportan poco. La dictadura es preferible cuando los partidos aportan mucho.
# Es decir, la sociedad (1) quiere a los partidos lejos de la politica, (2) no asocia partidos con democracia, (3) no cree que los partidos sean buenos para la democracia.


# ds
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


# plot
p_load(ggplot2)
ggplot(data=aporte.partidos.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Aporte.Partidos), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +   
  labs(title="¿Cuánto aporta al desarrollo de esta región los partidos políticos?") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/aporte_partidos.pdf")
  

####################################################################################
# confianza en las personas (0: no; 1 = si)
####################################################################################

x.confianza <- setx(z.out1, p37 = max(dat$p37, na.rm=T))
x.desconfianza <- setx(z.out1, p37 = min(dat$p37, na.rm=T))
s.out.confianza <- sim(z.out1, x =  x.desconfianza, x1 =  x.confianza, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.confianza)
# comment: (distribution plots) la confianza sube las preferencias en la democracia. la desconfianza sube la indiferencia y actitudes no-democraticas.
# Esto es especialmente importante donde la gran mayoria no confia en las personas.
table(dat$p37)


# ds
confianza.personas.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.confianza$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.confianza$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.confianza$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.confianza$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.confianza$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.confianza$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Nivel.Confianza = c(
    rep("Desconfianza",n.sim*3),
    rep("Confianza",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=confianza.personas.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Nivel.Confianza), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +  
  labs(title="Ud. diría que en general, se puede/no se puede confiar en las personas.") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/confianza_interpersonal.pdf")

####################################################################################
# cambiarse (0: no; 1 = si)
####################################################################################

x.cambiarse <- setx(z.out1, p3 = max(dat$p3, na.rm=T))
x.no.cambiarse <- setx(z.out1, p3 = min(dat$p3, na.rm=T))
s.out.cambiarse <- sim(z.out1, x =  x.no.cambiarse, x1 =  x.cambiarse, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.cambiarse)
# comment: no hay diferencia. Las gente de todas maneras siente un arraigo con su region, que esta dispuesto a vivir bajo cualquier regimen, pero en su region.


# ds
cambiarse.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.cambiarse$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Tendencia.Cambiarse = c(
    rep("No se cambiaría",n.sim*3),
    rep("Sí se cambiaría",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=cambiarse.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Tendencia.Cambiarse), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +   
  labs(title="¿Le gustaría cambiarse o irse a vivir a otro lugar?") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/cambiarse.pdf")
  

####################################################################################
# redistributivo (0: no; 1 = si)
####################################################################################

x.redistributivo <- setx(z.out1, p23_1 = max(dat$p23_1, na.rm=T))
x.no.redistributivo <- setx(z.out1, p23_1 = min(dat$p23_1, na.rm=T))
s.out.redistribucion <- sim(z.out1, x =  x.no.redistributivo, x1 =  x.redistributivo, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.redistribucion)
# comment: tener preferencias redistributivas no esta asociado a ser sistematicamente mas democratico. esto es peligroso, en el sentido de que pueda abrir la posibildiad de que candidatos populistas y militaristas ofreciendio redistribucion, podrian ser votados/electos. Tradicionaomente, la democracia ha sido siempre un mecanismo de redistribiccion, pero no en la sexta region.

# ds
redistribucion.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.redistribucion$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Tendencia.Redistribuir = c(
    rep("No distribuir",n.sim*3),
    rep("Sí redistribuir",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=redistribucion.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Tendencia.Redistribuir), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +  
  labs(title="¿Cree usted que debiera ser prioritario impulsar el crecimiento económico o reducir las desigualdades?") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/redistribucion.pdf")
  

####################################################################################
# conflicto = evitar conflictos: 0 NO, 1 SI
####################################################################################

x.permitir.conflicto <- setx(z.out1, p16 = max(dat$p16, na.rm=T))
x.evitar.conflicto <- setx(z.out1, p16 = min(dat$p16, na.rm=T))
s.out.conflicto <- sim(z.out1, x =  x.evitar.conflicto, x1 =  x.permitir.conflicto, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.conflicto)
# predisposicion al conflicto: no te hace ser ni mas autoritario ni mas democratico. esto significa que los rancaguinos no asocian el conflicto social con algun regimen en particular. el conflicto y la paz, puede darse en ambos, lo que indica que la democracia si es un lugar donde se PUEDAN exhibir fuertes tensiones sociales. Los rancaguinos, en este sentido no son conservadores en el sentido de que la democracia deba estar exenta de conflictos. 

# ds
conflicto.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.conflicto$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Evitar.Conflictos = c(
    rep("No evitar conflictos",n.sim*3),
    rep("Sí evitar conflictos",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=conflicto.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Evitar.Conflictos), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) + 
  labs(title="Cuando se producen conflictos, ¿se debiera dejar que se muestren o tratar de eviarlos?") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/conflicto.pdf")


####################################################################################
# GSE
####################################################################################

x.riqueza <- setx(z.out1, GSE = max(dat$GSE, na.rm=T))
x.pobreza <- setx(z.out1, GSE = min(dat$GSE, na.rm=T))
s.out.GSE <- sim(z.out1, x =  x.pobreza, x1 =  x.riqueza, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.out.GSE)
# la pobreza sube la indiferencia al regimen politico y baja las preferencias a favor de la democracia.
# Esto habla basicamente de una falta en la confianza de que la democracia es un mecanismo de 
# distribucion economica. Muy por el contrario, los rancaguinos mas ricos tienden a preferir la
# democracia como mecanismo para proteger la riqueza acumulada. Esto podria ser importante para
# poder explicar la contigencia. Sobre todo en un contexto donde la mayoria de los rancaguinos
# no se identifica con el eje clasico de izq/derecha.



# ds
GSE.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.GSE$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.GSE$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.GSE$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.GSE$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.GSE$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.GSE$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  GSE = c(
    rep("Pobre",n.sim*3),
    rep("Rico",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=GSE.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=GSE), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +  
  labs(title="Grupo Socio Económico") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/gse.pdf")


####################################################################################
# Edad
####################################################################################

x.viejo <- setx(z.out1, p44 = max(dat$p44, na.rm=T))
x.joven <- setx(z.out1, p44 = min(dat$p44, na.rm=T))
s.out.edad <- sim(z.out1, x =  x.joven, x1 =  x.viejo, num = n.sim)
# par(mar=c(1,1,1,1));plot(s.edad)
# la edad aumenta la indiferencia.

# ds
edad.sim.d = data.frame(
  Valor.Esperado = c(
    s.out.edad$get_qi(qi ="ev", xvalue = "x")[,1],
    s.out.edad$get_qi(qi ="ev", xvalue = "x")[,2],
    s.out.edad$get_qi(qi ="ev", xvalue = "x")[,3],
    s.out.edad$get_qi(qi ="ev", xvalue = "x1")[,1],
    s.out.edad$get_qi(qi ="ev", xvalue = "x1")[,2],
    s.out.edad$get_qi(qi ="ev", xvalue = "x1")[,3]
  ),
  Tendencia = c(
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim),
    rep("Democratica",n.sim),
    rep("Autoritaria",n.sim),
    rep("Indiferente",n.sim)
  ),
  Edad = c(
    rep("Joven",n.sim*3),
    rep("Viejo",n.sim*3)
  )
)


# plot
p_load(ggplot2)
ggplot(data=edad.sim.d) + 
  geom_density(aes(x=Valor.Esperado, fill=Edad), alpha=0.5) + 
  facet_grid(~Tendencia) + 
  theme_bw() +
  theme(axis.text.y = element_text(size=25), 
        axis.text.x = element_text(size=13), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=0), 
        legend.position = "bottom",
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.spacing.x = unit(1.0, 'cm')) +   
  labs(title="Grupo Etáreo") +
  ggsave("/Users/hectorbahamonde/RU/research/Observatorio_Regional/edad.pdf")


####################################################################################
# PANORAMA NO ES ALENTADOR.

# Bastian
# 1. Ver posibles faltas de ortografia.
# 2. Anteponer signo de pregunta al ppio de cada pregunta (cuando corresponda).
# 3. 
