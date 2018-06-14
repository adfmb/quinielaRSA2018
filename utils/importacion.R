importa_alldocs<-function(overwrite=F){
  
  if(overwrite){
    
    library(dplyr)
    docsnames<-list.files("Quinielas_Grupo_A/")
    docsnames<-docsnames[grep("@",docsnames)]
    alldocs<-data.frame()
    # options(warn=1)
    for(docname in docsnames){
      print(docname)
      doc<-read.csv(paste0("Quinielas_Grupo_A/",docname),header=T)
      doc<-doc%>%
        mutate_at(.vars = c("Codigo","Grupo","Partido","E1E2","Equipo",
                            "Equipo_gsub","nomequipo",
                            "nomconcursante","folio","correo"),.funs = as.character)
      alldocs<-bind_rows(alldocs,doc)#rbind(alldocs,doc)
      
    }
    
    # View(alldocs)
    alldocs$Grupo<-as.character(alldocs$Grupo)
    alldocs$Partido<-as.character(alldocs$Partido)
    alldocs$E1E2<-as.character(alldocs$E1E2)
    alldocs$Equipo<-as.character(alldocs$Equipo)
    alldocs$nomconcursante<-as.character(alldocs$nomconcursante)
    alldocs$folio<-as.character(alldocs$folio)
    
    alldocs<-alldocs%>%
      arrange(Grupo,Partido,E1E2)
    
    saveRDS(alldocs,"data/alldocs.rds")
    
  }else{
    alldocs<-readRDS("data/alldocs.rds")
  }
  
  return(alldocs)
  
}
# saveRDS(alldocs,"data/alldocs.rds")