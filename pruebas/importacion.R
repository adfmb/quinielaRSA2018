docsnames<-list.files("data/")
docsnames<-docsnames[grep("@",docsnames)]
alldocs<-data.frame()

for(docname in docsnames){
  print(docname)
  doc<-read.csv(paste0("data/",docname),header=T)
  alldocs<-rbind(alldocs,doc)
  
}

View(alldocs)
doc$Grupo<-as.character(doc$Grupo)
doc$Equipo<-as.character(doc$Equipo)

library(dplyr)
View(doc%>%
arrange(Grupo,Partido,E1E2))

names(doc)