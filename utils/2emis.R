Semifinales
=======================================================================
  
```{r}
e57<-reactive({#input$buttonA, {
  etopn(lista4s()$data_EquipoPartido[lista4s()$data_EquipoPartido$Partido==57,],n=1)
})
e58<-reactive({#input$buttonA, {
  etopn(lista4s()$data_EquipoPartido[lista4s()$data_EquipoPartido$Partido==58,],n=1)
})
e59<-reactive({#input$buttonA, {
  etopn(lista4s()$data_EquipoPartido[lista4s()$data_EquipoPartido$Partido==59,],n=1)
})
e60<-reactive({#input$buttonA, {
  etopn(lista4s()$data_EquipoPartido[lista4s()$data_EquipoPartido$Partido==60,],n=1)
})

vSemis <- reactiveValues(OK = c(F,F))

observeEvent(input$button4s, {
  if(lista4s()=="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
    vSemis$OK[1] <- F
    vSemis$OK[2] <- F
  }else{
    vSemis$OK[1] <- lista4s()$marcaBoton
    vSemis$OK[2] <- sum(lista4s()$data_EquipoPartido$Empate)==0
  }
})

alarmaSemis<-paste0("¡¡O.O Terminemos con las fases anteriores :D!!")
```

Row
-------------------------------------
  
### [W61]
  
```{r}

partido1<-57
partido2<-58

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e57()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarmaSemis##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
fluidRow(
  
  column(4,
         tags$div(
           HTML(paste(tags$span(style="color:white", "----   "), sep = ""))
         )),
  
  column(4,
         tags$div(
           HTML(paste(tags$span(style="color:white", "----"),tags$h3(style="color:red", "vs"), sep = ""))
         ))
  
)

fluidRow(
  column(4,
         h2(paste0("W",partido2))
  ),
  
  column(4,
         numericInput(paste0("W",partido2),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e58()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarmaSemis##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
```

Row
-------------------------------------
  
### [W62]
  
```{r}
partido1<-59
partido2<-60

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e59()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarmaSemis##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
fluidRow(
  
  column(4,
         tags$div(
           HTML(paste(tags$span(style="color:white", "----   "), sep = ""))
         )),
  
  column(4,
         tags$div(
           HTML(paste(tags$span(style="color:white", "----"),tags$h3(style="color:red", "vs"), sep = ""))
         ))
  
)

fluidRow(
  column(4,
         h2(paste0("W",partido2))
  ),
  
  column(4,
         numericInput(paste0("W",partido2),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e60()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarmaSemis##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
```


Row {data-height=100}
-------------------------------------
  
  
```{r}
library(shinyjs)

dataW2s<-reactive({data.frame("Codigo"=c("W_61_E1","W_61_E2",
                                         "W_62_E1","W_62_E2"),
                              "Equipo"=c(paste0(e57()[1]),paste0(e58()[1]),
                                         paste0(e59()[1]),paste0(e60()[1]))
)
  # eventReactive({ input$buttonA input$buttonB input$buttonC input$buttonD input$buttonE input$buttonF input$buttonG input$buttonH}
})


listaSemis<-eventReactive(input$buttonSemis,{
  
  if(vSemis$OK[1] & vSemis$OK[2]){
    # print("holi")
    calcula_Goles(vectorgoles=c(input[["W57"]],input[["W58"]],#input[[paste0("E1",Grupo1)]],
                                input[["W59"]],input[["W60"]]),
                  data=dataW2s()
    )
  }else{
    "MENSAJE SIN CONCLUIR FASE DE CUARTOS"
  }
})
fluidRow(
  
  column(7,
         actionButton("buttonSemis","  NO TE VAYAS SIN ACTUALIZARME    :P")
  ),
  
  column(7,
         renderTable({
           if(listaSemis()=="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
             m<-matrix("FASE DE CUARTOS SIN CONCLUIR CORRECTAMENTE")
             colnames(m)<-"¡¡¡O.O!!!!"
             m
           }else if(!is.null(listaSemis()$data_EquipoPartido)){
             if(lista2A()$marcaBoton & lista2B()$marcaBoton){
               if(listaSemis()$marcaBoton){
                 if(sum(listaSemis()$data_EquipoPartido$Empate)==0){
                   as.matrix(listaSemis()$data_EquipoPartido[1:4,c("Grupo","Partido","Equipo")])
                 }else{
                   m<-matrix("</3 Cámbialo porfiiiiis")
                   colnames(m)<-"¡¡¡NO MAMAR QUE PUSISTE EMPATES!!!!"
                   m
                 }
                 
               }else{
                 m<-matrix("Por favor revisa que las cantidades de la Fase de Cuartos sean correctas :O")
                 colnames(m)<-"¡¡¡AGUAS!!!!"
                 m
               }
             }else{
               m<-matrix("Por favor revisa que las cantidades de Primera Fase sean correctas :O")
               colnames(m)<-"¡¡¡AGUAS!!!!"
               m
             }
             
           }else{
             m<-matrix("Por favor revisa que las cantidades ingresadas sean correctas")
             colnames(m)<-"¡¡¡AGUAS!!!!"
             m
           }
           
         })
         
  )
)
```