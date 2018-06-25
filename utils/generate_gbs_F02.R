generate_gbs_F02<-function(alldocs=alldocs,resultados_reales=resultados_reales,matches=c(49,50,51,52,53,54,55,56)){#,
  # Fase="Grupos"){
  
  alldocs_selecF02<-alldocs%>%
    select(nomconcursante,folio,Codigo2,Grupo,Partido,E1E2,Equipo_gsub,GolesFavor,Ganado,Perdido,Empate)%>%
    mutate_at(vars(Grupo,Partido,E1E2),.funs = funs(as.character))%>%
    filter(Grupo=="W" & Ganado==1)
  
  resultados_reales_F02_pterminados<-resultados_reales%>%
    mutate_at(vars(Grupo,Partido,E1E2,Equipo_gsub,status_juego,Codigo2),.funs = funs(as.character))%>%
    filter(Grupo=="W" & status_juego!="por_jugar")# & Ganado_real==1)
  
  ### Aquí se generará la tabla con la Indicadora de si se le atinó al equipo concursante como ganador -> I_Ganados
  alldocs_selecF02_pterminados<-alldocs_selecF02%>%
    right_join(resultados_reales_F02_pterminados%>%select(-Codigo,-Grupo,-Partido,-E1E2))%>% #Joining, by = c("Codigo2", "Equipo_gsub") -> Para considerar a todos los que están en juego
    arrange(Grupo,Partido,nomconcursante,E1E2)%>%
    mutate(I_Ganados=Ganado*Ganado_real,#%>%,
           I_Perdido=Perdido*Perdido_real)#,
           # I_Empate=Empate*Empate_real
           
    # rowwise()%>%
    # mutate(
      # Resultado_correcto_equipo=sum(I_Ganados,I_Perdido,I_Empate),
           # Resultado_correcto_equipo_Ganador=Resultado_correcto_equipo*sum(I_Ganados,I_Perdido),
           # Resultado_correcto_equipo_Empate=Resultado_correcto_equipo*sum(I_Empate)
           # )%>%
    # ungroup()
  
  
  ###  Agrupando por nomconcursante, folio & Codigo2 (Octavos, Cuartos, Semifinales, Tercer, Final)   -------------------------------FALTA EL IF_ELSE Fase="Grupos") ---- Por ahora esta funcion será solo para Fase de Grupos
  gb_nomb_codigo2<-alldocs_selecF02_pterminados%>%
    group_by(nomconcursante,folio,Codigo2)%>%
    summarise(Resultado_correcto_partido_Ganador=as.integer(sum(I_Ganados)),#/2),
              Resultado_correcto_partido_Perdedor=as.integer(sum(I_Perdido)))%>%#,
              # Resultado_correcto_partido_Empate=as.integer(sum(Resultado_correcto_equipo_Empate)/2),
              # min_Difgolesfavor=as.integer(min(Dif_GF)),
              # max_Difgolesfavor=as.integer(max(Dif_GF)),
    ungroup()%>%
    mutate(
              Puntos_primlugar=if_else(Codigo2=="Final",8*Resultado_correcto_partido_Ganador,0),
              Puntos_seglugar=if_else(Codigo2=="Final",4*Resultado_correcto_partido_Perdedor,0),
              Puntos_terlugar=if_else(Codigo2=="Tercer",2*Resultado_correcto_partido_Ganador,0),
              # Puntos_Resultado = 3*Resultado_correcto_partido_Ganador + 2*Resultado_correcto_partido_Empate,
              # Puntos_Marcador = if_else(Puntos_Resultado>0 & Marcador_correcto_partido==1,5,0),
              Puntos_Partido = 5*Resultado_correcto_partido_Ganador + Puntos_primlugar+Puntos_seglugar+Puntos_terlugar
              )
  
  gb_nomb<-gb_nomb_codigo2%>%
    # ungroup()%>%
    group_by(nomconcursante,folio)%>%
    summarise(Suma_Puntos_Resultado=sum(Resultado_correcto_partido_Ganador),
              Suma_Puntos_1erL=sum(Puntos_primlugar),
              Suma_Puntos_2doL=sum(Puntos_seglugar),
              Suma_Puntos_3erL=sum(Puntos_terlugar),
              Suma_Puntos_Partido=sum(Puntos_Partido))%>%
    arrange(desc(Suma_Puntos_Partido))
  
  return(list("gb_nomb_codigo2"=gb_nomb_codigo2,
              "gb_nomb"=gb_nomb,
              "alldocs_selecF02_pterminados"=alldocs_selecF02_pterminados))
}