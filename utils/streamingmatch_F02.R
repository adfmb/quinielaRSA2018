source("utils/generate_gbs_F01.R")
library(dplyr)
library(tidyr)
streamingmatch_F02<-function(alldocs=alldocs,resultados_reales=resultados_reales){
  
  reales_sm<-resultados_reales%>%
    # filter(Grupo==Grupo_sm & Partido==Partido_sm)
    filter(Grupo=="W" & status_juego=="jugando")
  
  show_tblsm<-reales_sm%>%select(Codigo2,Partido)%>%distinct()%>%
    # mutate(Goles_e1=0)%>%
    left_join(reales_sm%>%
                filter(E1E2=="e1" | E1E2=="E1")%>%
                select(Partido,GolesFavor_real,Equipo_gsub)%>%
                rename(Equipo1=Equipo_gsub)%>%
                rename(Goles_e1=GolesFavor_real))%>%
    mutate(vs="vs")%>%
    left_join(reales_sm%>%
                filter(E1E2=="e2" | E1E2=="E2")%>%
                select(Partido,Equipo_gsub,GolesFavor_real)%>%
                rename(Equipo2=Equipo_gsub)%>%
                rename(Goles_e2=GolesFavor_real))#%>%
    # mutate(Goles_e2=0)
  
  prediccones_sm00<-alldocs%>%
    # filter(Grupo==Grupo_sm & Partido==Partido_sm)  
    right_join(reales_sm%>%
                 select(Codigo2,Equipo_gsub))
  
  gbs_sm00<-generate_gbs_F02(prediccones_sm00,reales_sm)
  
  equipos_orden<-as.character(as.data.frame(reales_sm%>%arrange(Grupo,Partido,E1E2))$Equipo_gsub)
  predicciones_sm01<-prediccones_sm00%>%
    select(nomconcursante,folio,Grupo,Partido,Equipo_gsub,GolesFavor)%>%
    spread(Equipo_gsub,GolesFavor,fill = "-")%>%
    select(nomconcursante,folio,Grupo,Partido,one_of(equipos_orden))%>%
    arrange(Grupo,Partido,nomconcursante,folio)
  
  pts_sm00<-gbs_sm00$gb_nomb_grupopartido%>%
    ungroup()%>%
    select(nomconcursante,folio,Grupo,Partido,Puntos_Resultado,Puntos_Marcador,Puntos_Partido)
  
  sm<-predicciones_sm01%>%
    left_join(pts_sm00)
  
  return(list("sm"=sm,"show_tblsm"=show_tblsm))
  
}
