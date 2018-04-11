f_GA<-function(){
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
                  if(!is.null(lista2A()$data_resumenEquipos)){
                    as.matrix(lista2A()$data_resumenEquipos)
                    
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
                         numericInput("A_p1_e1", "Rusia", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"),tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p1_e2", "Arabia S", min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput("A_p2_e1", "Rusia", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"),tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p2_e2", "Egipto", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  )
                ),
                
                
                fluidRow(
                  column(2, 
                         numericInput("A_p3_e1", "Uruguay", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p3_e2", "Rusia", min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput("A_p4_e1", "Uruguay", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p4_e2", "Arabia S", min=0, max=100, value=0,step=1,
                                      width = '65px'))
                  
                  
                ),
                
                fluidRow(
                  column(2, 
                         numericInput("A_p5_e1", "Arabia S", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p5_e2", "Egipto", min=0, max=100, value=0,step=1,
                                      width = '65px')),
                  
                  
                  column(2, 
                         numericInput("A_p6_e1", "Egipto", min=0, max=100, value=0,step=1,
                                      width = '65px')
                  ),
                  column(2,
                         tags$div(
                           HTML(paste(tags$span(style="color:white", "----"), tags$span(style="color:red", "vs"), sep = ""))
                         )),
                  column(2,
                         numericInput("A_p6_e2", "Uruguay", min=0, max=100, value=0,step=1,
                                      width = '65px'))
                  
                  
                ),
                width = 8
              )
)

fluidRow(
  column(2, actionButton("buttonA", "Actualizar")
  ))
}
# renderTable({
#      if(!is.null(lista2A()$data2)){
#        lista2A()$data2
#      }else{
#          m<-matrix("Por favor revisa que las cantidades ingresadas sean correctas")
#          colnames(m)<-"¡¡¡AGUAS!!!!"
#          m
#        }
#           
#      })


