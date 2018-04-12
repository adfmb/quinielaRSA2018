Grupo<-"A"
v1<-rbind(c(paste0(Grupo,"_p1_e1"), "Rusia"),
          c(paste0(Grupo,"_p1_e2"), "Arabia S"),
          c(paste0(Grupo,"_p2_e1"), "Egipto"),
          c(paste0(Grupo,"_p2_e2"), "Uruguay"),
          c(paste0(Grupo,"_p3_e1"), "Rusia"),
          c(paste0(Grupo,"_p3_e2"), "Egipto"),
          c(paste0(Grupo,"_p4_e1"), "Uruguay"),
          c(paste0(Grupo,"_p4_e2"), "Arabia S"),
          c(paste0(Grupo,"_p5_e1"), "Uruguay"),
          c(paste0(Grupo,"_p5_e2"), "Rusia"),
          c(paste0(Grupo,"_p6_e1"), "Arabia S"),
          c(paste0(Grupo,"_p6_e2"), "Egipto")
)
colnames(v1)<-c("Codigo","Equipo")
dataA<-data.frame(as.data.frame(v1))
dataA$Codigo<-as.character(dataA$Codigo)
dataA$Equipo<-as.character(dataA$Equipo)

lista2A<-eventReactive(input$buttonA, {
  
  calcula_Goles(vectorgoles=c(input$A_p1_e1,input$A_p1_e2,
                              input$A_p2_e1,input$A_p2_e2,
                              input$A_p3_e1,input$A_p3_e2,
                              input$A_p4_e1,input$A_p4_e2,
                              input$A_p5_e1,input$A_p5_e2,
                              input$A_p6_e1,input$A_p6_e2
  ),
  data=dataA)
  
})

observeEvent(input$buttonA, {
  # Change the following line for more examples
  toggleState("element")
})

sidebarLayout(position = "right",
              sidebarPanel(
                
                # renderDataTable(data.frame("numeros"=rnorm(6),"num2"=rnorm(6))),
                # renderTable(as.matrix(data.frame("Equipo"=unique(data$Equipo),
                #                                  "G"=input$A_p1_e1,
                #                                  "P"=input$A_p1_e2,
                #                                  "E"=rnorm(4),
                #                                  "GF"=rnorm(4),
                #                                  "GC"=rnorm(4),
                #                                  "D"=rnorm(4),
                #                                  "Pts"=input$A_p6_e2))),
                
                renderTable({
                  if(!is.null(lista2A()$data_EquipoPartido)){
                    as.matrix(lista2A()$data_EquipoPartido)
                    
                  }else{
                    m<-matrix("Por favor revisa que las cantidades ingresadas sean correctas")
                    colnames(m)<-"¡¡¡AGUAS!!!!"
                    m
                  }
                  
                }),
                
                width = 4
                
              ),
              mainPanel(
                fluidRow(
                  column(2, 
                         numericInput(paste0(Grupo,"_p1_e1"), dataA$Equipo[1], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"),tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p1_e2"), dataA$Equipo[2], min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput(paste0(Grupo,"_p2_e1"), dataA$Equipo[3], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"),tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p2_e2"), dataA$Equipo[4], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  )
                ),
                
                
                fluidRow(
                  column(2, 
                         numericInput(paste0(Grupo,"_p3_e1"), dataA$Equipo[5], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p3_e2"), dataA$Equipo[6], min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput(paste0(Grupo,"_p4_e1"), dataA$Equipo[7], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p4_e2"), dataA$Equipo[8], min=0, max=100, value=0,step=1,
                                      width = '65px'))
                  
                  
                ),
                
                fluidRow(
                  column(2, 
                         numericInput(paste0(Grupo,"_p5_e1"), dataA$Equipo[9], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p5_e2"), dataA$Equipo[10], min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput(paste0(Grupo,"_p6_e1"), dataA$Equipo[11], min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput(paste0(Grupo,"_p6_e2"), dataA$Equipo[12], min=0, max=100, value=0,step=1,
                                      width = '65px'))
                  
                  
                ),
                width = 8
              )
)

fluidRow(
  column(2, actionButton("buttonA", "Actualizar")
  ))