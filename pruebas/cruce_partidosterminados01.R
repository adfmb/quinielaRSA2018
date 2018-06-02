library(dplyr)
alldocs<-readRDS("data/alldocs.rds")

alldocs_selecF01<-alldocs%>%
  select(nomconcursante,folio,Grupo,Partido,E1E2,GolesFavor,Ganado,Perdido,Empate)%>%
  filter(Grupo!="W")%>%
  mutate_at(vars(Grupo,Partido,E1E2),.funs = funs(as.character))

resultados_reales<-read.csv("data/resultados_reales.csv",header=T)
resultados_reales$Empate_real[1:2]<-1
resultados_reales$status_juego<-as.character(resultados_reales$status_juego)
resultados_reales$status_juego[1:2]<-"terminado"

resultados_reales_F01_pterminados<-resultados_reales%>%
  filter(Grupo!="W" & status_juego!="por_jugar")

alldocs_selecF01_pterminados<-alldocs_selecF01%>%
  right_join(resultados_reales_F01_pterminados)%>%
  arrange(Grupo,Partido,nomconcursante,E1E2)%>%
  mutate(Dif_GF=GolesFavor-GolesFavor_real,
         I_Ganados=Ganado*Ganado_real,
         I_Perdido=Perdido*Perdido_real,
         I_Empate=Empate*Empate_real)%>%
  rowwise()%>%
  mutate(Resultado_correcto_equipo=sum(I_Ganados,I_Perdido,I_Empate))%>%
  ungroup()

View(alldocs_selecF01_pterminados)

gb_nomb_grupopartido<-alldocs_selecF01_pterminados%>%
  group_by(nomconcursante,folio,Grupo,Partido)%>%
  summarise(Resultado_correcto_partido=as.integer(sum(Resultado_correcto_equipo)/2),
            min_Difgolesfavor=min(Dif_GF),
            max_Difgolesfavor=max(Dif_GF),
            Marcador_correcto_partido=if_else(min_Difgolesfavor==0 & max_Difgolesfavor==0,1,0),
            Puntos_Resultado=if_else(Resultado_correcto_partido==1))

names(doc)