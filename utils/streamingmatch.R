source("utils/generate_gbs_F01.R")
library(dplyr)
library(tidyr)
streamingmatch<-function(alldocs=alldocs,resultados_reales=resultados_reales){
  
  reales_sm<-resultados_reales%>%
    # filter(Grupo==Grupo_sm & Partido==Partido_sm)
    filter(Grupo!="W" & status_juego=="jugando")
  
  prediccones_sm00<-alldocs%>%
    # filter(Grupo==Grupo_sm & Partido==Partido_sm)  
    right_join(reales_sm%>%
                 select(Grupo,Partido,E1E2))
  
  gbs_sm00<-generate_gbs_F01(prediccones_sm00,reales_sm)
    
  prediccones_sm01<-prediccones_sm00%>%
    select(nomconcursante,folio,Grupo,Partido,Equipo_gsub,GolesFavor)%>%
    spread(Equipo_gsub,GolesFavor,fill = "-")
  
  pts_sm00<-gbs_sm00$gb_nomb_grupopartido%>%
    ungroup()%>%
    select(nomconcursante,folio,Grupo,Partido,Puntos_Resultado,Puntos_Marcador,Puntos_Partido)
  
  sm<-prediccones_sm01%>%
    left_join(pts_sm00)
  
  return(list("sm"=sm,"E1"=names(sm)[3],"E2"=names(sm)[4]))
  
}
