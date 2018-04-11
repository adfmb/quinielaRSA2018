calcula_Goles<-function(vectorgoles,data){
  
  if(sum(unlist(lapply(vectorgoles,is.na)))==0){
    golesA<-vectorgoles
    data2<-data
    data2$Goles<-golesA
    
    dfllaves<-data.frame(do.call('rbind', strsplit(as.character(data2$Codigo),'_',fixed=TRUE)))
    data2llaves<-data.frame(do.call('rbind', strsplit(as.character(data2$Codigo),'_',fixed=TRUE)))
    names(data2llaves)<-c("Grupo","Partido","E1E2")
    data2llaves$Grupo<-as.character(data2llaves$Grupo)
    data2llaves$Partido<-as.character(data2llaves$Partido)
    data2llaves$E1E2<-as.character(data2llaves$E1E2)
    
    data2<-cbind(data2,data2llaves)
    
    # datagolesmax<-data2 %>% group_by(Grupo,Partido) %>% summarise(GolesGanador=max(Goles),
    #                                                               GolesPerdedor=min(Goles))
    
    dataganadores<- merge(aggregate(Goles~Partido,data2,max),data2)
    dataperdedores<- merge(aggregate(Goles~Partido,data2,min),data2)
    names(dataganadores)[match("E1E2",names(dataganadores))]<-"E1E2Ganador"
    names(dataperdedores)[match("E1E2",names(dataperdedores))]<-"E1E2Perdedor"
    names(dataganadores)[match("Equipo",names(dataganadores))]<-"EquipoGanador"
    names(dataperdedores)[match("Equipo",names(dataperdedores))]<-"EquipoPerdedor"
    names(dataganadores)[match("Goles",names(dataganadores))]<-"GolesGanador"
    names(dataperdedores)[match("Goles",names(dataperdedores))]<-"GolesPerdedor"
    
    ################################################################################
    #####Para quitar duplicados en los empates manteniendo ambos equipos#####
    dataganadores<-dataganadores[order(dataganadores$E1E2Ganador),]
    dataperdedores<-dataperdedores[order(dataperdedores$E1E2Perdedor),]
    
    dataganadores<-dataganadores[!duplicated(dataganadores$Partido,fromLast = T),]
    dataperdedores<-dataperdedores[!duplicated(dataperdedores$Partido,fromLast = F),]
    ################################################################################
    
    data_resP<-dataganadores %>% 
      left_join(
        dataperdedores[c("Grupo","Partido","GolesPerdedor","EquipoPerdedor","E1E2Perdedor")])
    data_resP<-data_resP[order(data_resP$Partido),]
    data_resP<-data_resP[c("Codigo","Grupo","Partido","E1E2Ganador","E1E2Perdedor",
                           "EquipoGanador","GolesGanador","EquipoPerdedor","GolesPerdedor")]
    data_resP$ptsG<-rep(1,nrow(data_resP))
    data_resP$ptsG[data_resP$GolesGanador>data_resP$GolesPerdedor]<-3
    data_resP$ptsP<-rep(1,nrow(data_resP))
    data_resP$ptsP[data_resP$GolesGanador>data_resP$GolesPerdedor]<-0
    
    data_resEqG<- data_resP[c("Codigo","Grupo","Partido","E1E2Ganador",
                              "EquipoGanador","GolesGanador","GolesPerdedor","ptsG")]
    names(data_resEqG)[match("E1E2Ganador",names(data_resEqG))]<-"E1E2"
    names(data_resEqG)[match("EquipoGanador",names(data_resEqG))]<-"Equipo"
    names(data_resEqG)[match("GolesGanador",names(data_resEqG))]<-"GolesFavor"
    names(data_resEqG)[match("GolesPerdedor",names(data_resEqG))]<-"GolesContra"
    names(data_resEqG)[match("ptsG",names(data_resEqG))]<-"Pts"
    
    data_resEqP<- data_resP[c("Codigo","Grupo","Partido","E1E2Perdedor",
                              "EquipoPerdedor","GolesGanador","GolesPerdedor","ptsP")]
    names(data_resEqP)[match("E1E2Perdedor",names(data_resEqP))]<-"E1E2"
    names(data_resEqP)[match("EquipoPerdedor",names(data_resEqP))]<-"Equipo"
    names(data_resEqP)[match("GolesGanador",names(data_resEqP))]<-"GolesContra"
    names(data_resEqP)[match("GolesPerdedor",names(data_resEqP))]<-"GolesFavor"
    names(data_resEqP)[match("ptsP",names(data_resEqP))]<-"Pts"
    
    data_EquipoPartido<-rbind(data_resEqG,data_resEqP)
    data_EquipoPartido<-data_EquipoPartido[order(
      data_EquipoPartido$Partido,
      -data_EquipoPartido$Pts
    ),]
    
    data_EquipoPartido$DiferenciaGoles<-data_EquipoPartido$GolesFavor-data_EquipoPartido$GolesContra
    
    data_EquipoPartido$Ganado<-0
    data_EquipoPartido$Perdido<-0
    data_EquipoPartido$Empate<-1
    
    data_EquipoPartido$Ganado[data_EquipoPartido$Pts==3]<-1
    data_EquipoPartido$Perdido[data_EquipoPartido$Pts==0]<-1
    data_EquipoPartido$Empate[data_EquipoPartido$Pts!=1]<-0
    
    data_resumenEquipos<-data_EquipoPartido[
      c("Equipo",
        "Ganado","Perdido","Empate",
        "GolesFavor","GolesContra","DiferenciaGoles",
        "Pts")] %>% 
      group_by(Equipo) %>% 
      summarise(
        G=sum(Ganado),
        P=sum(Perdido),
        E=sum(Empate),
        GF=sum(GolesFavor),
        GC=sum(GolesContra),
        D=sum(DiferenciaGoles),
        Pts=sum(Pts)
      ) %>% arrange(desc(Pts),desc(D),desc(GF))
    data_resumenEquipos<-as.data.frame(data_resumenEquipos)
    
    
    
  }else{
    data2<-NULL
    data_EquipoPartido<-NULL
    data_resumenEquipos<-NULL}
  
  return(list("data2"=data2,
              # "vectorgoles"=vectorgoles,
              "data_EquipoPartido"=data_EquipoPartido,
              "data_resumenEquipos"=data_resumenEquipos)
  )
}
