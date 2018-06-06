source("utils/generate_gbs_F01.R")
library(dplyr)
library(tidyr)
streamingmatch<-function(Grupo_sm="A",Partido_sm="p1",alldocs=alldocs,resultados_reales=resultados_reales){
  
  prediccones_sm00<-alldocs%>%
    filter(Grupo==Grupo_sm & Partido==Partido_sm)
    
  reales_sm<-resultados_reales%>%
    filter(Grupo==Grupo_sm & Partido==Partido_sm)
  
  gbs_sm00<-generate_gbs_F01(prediccones_sm00,reales_sm)
    
  prediccones_sm01<-prediccones_sm00%>%
    select(nomconcursante,folio,Equipo_gsub,GolesFavor)%>%
    spread(Equipo_gsub,GolesFavor,fill = 0)
  
  pts_sm00<-gbs_sm00$gb_nomb_grupopartido%>%
    ungroup()%>%
    select(nomconcursante,folio,Puntos_Resultado,Puntos_Marcador,Puntos_Partido)
  
  sm<-prediccones_sm01%>%
    left_join(pts_sm00)
  
    
  
}
