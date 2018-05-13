docsnames<-list.files("data/")
alldocs<-data.frame()

for(docname in docsnames){
  
  doc<-read.csv(paste0("data/",docname),header=T)
  alldocs<-rbind(alldocs,doc)
  
}

View(alldocs)
alldocs$Grupo<-as.character(alldocs$Grupo)
alldocs$Equipo<-as.character(alldocs$Equipo)

library(dplyr)
View(doc%>%
arrange(Grupo,Partido,E1E2))

names(alldocs)

to_zero<-function(x){
  
  return(0)
}

cols_ids<-c("Codigo","Grupo","Partido","E1E2","Equipo")
cols_resultados<-c("GolesFavor","GolesContra","Pts","DiferenciaGoles","Ganado","Perdido","Empate")
resultados_reales<-alldocs%>%
  select(one_of(c(cols_ids,cols_resultados)))%>%
  mutate_at(vars(one_of(cols_resultados)),to_zero)%>%
  arrange(Grupo,Partido,E1E2)%>%
  mutate(Equipo=replace(Equipo, Grupo=="W", "por_definir"))

View(resultados_reales)
write.csv(resultados_reales,"data/resultados_reales.csv",row.names = F)
