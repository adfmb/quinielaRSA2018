v1<-rbind(c("F_p1_e1", "Alemania"),
          c("F_p1_e2", "México"),
          c("F_p2_e1", "Suecia"),
          c("F_p2_e2", "Corea S"),
          c("F_p3_e1", "Corea S"),
          c("F_p3_e2", "México"),
          c("F_p4_e1", "Alemania"),
          c("F_p4_e2", "Suecia"),
          c("F_p5_e1", "México"),
          c("F_p5_e2", "Suecia"),
          c("F_p6_e1", "Corea S"),
          c("F_p6_e2", "Alemania")
)
colnames(v1)<-c("Codigo","Equipo")
dataF<-data.frame(as.data.frame(v1))
dataF$Codigo<-as.character(dataF$Codigo)
dataF$Equipo<-as.character(dataF$Equipo)
dataF$Equipo<-iconv(dataF$Equipo, to = "UTF-8")
