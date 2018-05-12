pegar1Fase<-function(g1,g2,g3,g4,g5,g6,g7,g8,
                     w1,w2,w3,w4,
                     nomequipo,
                     nomconcursante,
                     folio,
                     correo){
  data<-rbind(g1,g2,g3,g4,g5,g6,g7,g8,w1,w2,w3,w4)
  x1<-gsub(pattern ="á","a",x = data$Equipo)
  x2<-gsub(pattern ="é","e",x = x1)
  x3<-gsub(pattern ="í","i",x = x2)
  x4<-gsub(pattern ="ó","o",x = x3)
  x5<-gsub(pattern ="ú","u",x = x4)
  x6<-gsub(pattern ="ñ","n",x = x5)
  data$Equipo_gsub<-x6
  data$nomequipo<-nomequipo
  data$nomconcursante<-nomconcursante
  data$folio<-folio
  data$correo<-correo
  return(data)
}