docsnames<-list.files("data/")
docsnames<-docsnames[grep("@",docsnames)]
alldocs<-data.frame()

for(docname in docsnames){
  print(docname)
  doc<-read.csv(paste0("data/",docname),header=T)
  alldocs<-rbind(alldocs,doc)
  
}

View(alldocs)
alldocs$Grupo<-as.character(alldocs$Grupo)
alldocs$Partido<-as.character(alldocs$Partido)
alldocs$E1E2<-as.character(alldocs$E1E2)
alldocs$Equipo<-as.character(alldocs$Equipo)
alldocs$nomconcursante<-as.character(alldocs$nomconcursante)
alldocs$folio<-as.character(alldocs$folio)

alldocs<-alldocs%>%
  arrange(Grupo,Partido,E1E2)
