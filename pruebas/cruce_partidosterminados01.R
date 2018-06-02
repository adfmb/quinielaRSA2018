library(dplyr)
alldocs<-readRDS("data/alldocs.rds")

alldocs_selecF01<-alldocs%>%
  select(nomconcursante,folio,Grupo,Partido,E1E2,GolesFavor,Ganado,Perdido,Empate)%>%
  filter(Grupo!="W")

resultados_reales<-read.csv("data/resultados_reales.csv",header=T)
resultados_reales$status_juego[1:2]<-"terminado"
resultados_reales_F01<-resultados_reales%>%
  filter(Grupo!="W" & status_juego!="por_jugar")




View(doc%>%
       arrange(Grupo,Partido,E1E2))

names(doc)