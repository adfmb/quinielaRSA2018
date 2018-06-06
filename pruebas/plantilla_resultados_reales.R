library(dplyr)
to_zero<-function(x){
  
  return(0)
}
docsnames<-list.files("data/")
docsnames<-docsnames[grep("@",docsnames)]
doc<-read.csv(paste0("data/",docsnames[1]),header=T)
doc<-doc%>%
  mutate_at(vars(Grupo,Partido,E1E2,Equipo,Equipo_gsub),.funs = funs(as.character))

# doc$Grupo<-as.character(doc$Grupo)
# doc$Partido<-as.character(doc$Partido)
# doc$E1E2<-as.character(doc$E1E2)
# doc$Equipo<-as.character(doc$Equipo)

cols_ids<-c("Codigo","Grupo","Partido","E1E2","Equipo","Equipo_gsub")
cols_resultados<-c("GolesFavor","GolesContra","Pts","DiferenciaGoles","Ganado","Perdido","Empate")
resultados_reales<-doc%>%
  select(one_of(c(cols_ids,cols_resultados)))%>%
  mutate_at(vars(one_of(cols_resultados)),to_zero)%>%
  arrange(Grupo,Partido,E1E2)%>%
  mutate(Equipo=replace(Equipo, Grupo=="W", "por_definir"),
         Equipo_gsub=replace(Equipo_gsub, Grupo=="W", "por_definir"),
         status_juego="por_jugar")%>%
  rename(GolesFavor_real=GolesFavor,
         GolesContra_real=GolesContra,
         Pts_real=Pts,
         DiferenciaGoles_real=DiferenciaGoles,
         Ganado_real=Ganado,
         Perdido_real=Perdido,
         Empate_real=Empate)

View(resultados_reales)
write.csv(resultados_reales,"data/resultados_reales.csv",row.names = F)
# resultados_reales<-read.csv("data/resultados_reales.csv",header=T)
