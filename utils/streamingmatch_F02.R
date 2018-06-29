source("utils/generate_gbs_F02.R")
library(dplyr)
library(tidyr)
streamingmatch_F02<-function(alldocs=alldocs,resultados_reales=resultados_reales,fase="Octavos"){
  
  reales_sm<-resultados_reales%>%
    # filter(Grupo==Grupo_sm & Partido==Partido_sm)
    filter(Grupo=="W" & status_juego=="jugando" & Codigo2%in%fase)
  
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
  
  #### AQU? HAY QUE CAMBIAR LOS PRON?STICOS EN GOLES, POR LLENARLOS CON "GANA"/"PIERDE" O ALGO PARECIDO
  predicciones_sm01<-prediccones_sm00%>%
    mutate(Resultado=if_else(Ganado==1,"Gana","Pierde"))%>%
    select(nomconcursante,folio,Grupo,Codigo2,Partido,Equipo_gsub,Resultado)%>%
    spread(Equipo_gsub,Resultado,fill = "-")%>%
    select(nomconcursante,folio,Grupo,Codigo2,Partido,one_of(equipos_orden))%>%
    arrange(Grupo,Partido,nomconcursante,folio)%>%
    select(-Partido)%>%
    group_by(nomconcursante,folio,Codigo2)%>%
    mutate_at(.vars = vars(equipos_orden),max)%>%
    filter(row_number()==1)
  
  pts_sm00<-gbs_sm00$gb_nomb_codigo2%>%
    ungroup()#%>%
    # select(nomconcursante,folio,Codigo2,Partido,Ganadores_Fase_acertados,Puntos_Marcador,Puntos_Partido)
  
  sm<-predicciones_sm01%>%
    left_join(pts_sm00)
  
  
  return(list("sm"=sm,"show_tblsm"=show_tblsm))
  
}
