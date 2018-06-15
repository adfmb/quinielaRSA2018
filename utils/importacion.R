importa_alldocs<-function(overwrite=F){
  
  if(overwrite){
    
    library(dplyr)
    docsnames<-list.files("Quinielas_Grupo_A/")
    docsnames<-docsnames[grep("@",docsnames)]
    alldocs<-data.frame()
    # options(warn=1)
    for(docname in docsnames){
      #2018-06-12 00_39_59_o.lopezarellanos@gmail.com_A035.csv
      #2018-06-12 01_13_16_rios.fabiola@hotmail.com_A065.csv
      #Rusia 2018 - Quiniela ANnie@.csv
      
      ##quiniela mundial FOLIO A016.csv
      ##2018-06-14 17_21_56__.csv
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