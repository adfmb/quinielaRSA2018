docsnames<-list.files("data/")
alldocs<-data.frame()

for(docname in docsnames){
  
  doc<-read.csv(paste0("data/",docname),header=T)
  alldocs<-rbind(alldocs,doc)
  
}