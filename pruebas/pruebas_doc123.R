library(dplyr)
to_zero<-function(x){
  
  return(0)
}
docsnames<-list.files("data/")
docsnames<-docsnames[grep("@",docsnames)]

doc1<-read.csv(paste0("data/",docsnames[1]),header=T)
doc1<-doc1%>%
  select(Grupo,Partido,E1E2,Codigo,Equipo,Equipo_gsub)%>%
  mutate_at(vars(Grupo,Partido,E1E2,Equipo,Equipo_gsub),.funs = funs(as.character))%>%
  # rename(Partido1=Partido)%>%
  # rename(E1E21=E1E2)%>%
  rename(Codigo1=Codigo)%>%
  rename(Equipo1=Equipo)%>%
  rename(Equipo_gsub1=Equipo_gsub)

doc2<-read.csv(paste0("data/",docsnames[2]),header=T)
doc2<-doc2%>%
  select(Grupo,Partido,E1E2,Codigo,Equipo,Equipo_gsub)%>%
  mutate_at(vars(Grupo,Partido,E1E2,Equipo,Equipo_gsub),.funs = funs(as.character))%>%
  # rename(Partido2=Partido)%>%
  # rename(E1E22=E1E2)%>%
  rename(Codigo2=Codigo)%>%
  rename(Equipo2=Equipo)%>%
  rename(Equipo_gsub2=Equipo_gsub)

doc3<-read.csv(paste0("data/",docsnames[3]),header=T)
doc3<-doc3%>%
  select(Grupo,Partido,E1E2,Codigo,Equipo,Equipo_gsub)%>%
  mutate_at(vars(Grupo,Partido,E1E2,Equipo,Equipo_gsub),.funs = funs(as.character))%>%
  # rename(Partido3=Partido)%>%
  # rename(E1E23=E1E2)%>%
  rename(Codigo3=Codigo)%>%
  rename(Equipo3=Equipo)%>%
  rename(Equipo_gsub3=Equipo_gsub)

docsi_F01<-doc1%>%
  left_join(doc2)%>%
  left_join(doc3)%>%
  mutate_all(funs(as.character))%>%
  filter(Grupo!="W")

sum(docsi$Codigo1!=docsi$Codigo2)
docsi[docsi$Codigo1!=docsi$Codigo2,]

sum(docsi$Equipo_gsub1!=docsi$Equipo_gsub2)
docsi[docsi$Equipo_gsub1!=docsi$Equipo_gsub2,]

sum(docsi_F01$Codigo1!=docsi_F01$Codigo2)
sum(docsi_F01$Equipo1!=docsi_F01$Equipo2)
sum(docsi_F01$Equipo_gsub1!=docsi_F01$Equipo_gsub2)

sum(docsi_F01$Codigo1!=docsi_F01$Codigo3)
docsi_F01[docsi_F01$Codigo1!=docsi_F01$Codigo3,]
sum(docsi_F01$Equipo1!=docsi_F01$Equipo3)
sum(docsi_F01$Equipo_gsub1!=docsi_F01$Equipo_gsub3)

sum(docsi_F01$Equipo_gsub1!=docsi_F01$Equipo_gsub2)
sum(docsi_F01$Equipo_gsub1!=docsi_F01$Equipo_gsub3)
sum(docsi_F01$Equipo_gsub2!=docsi_F01$Equipo_gsub3)

