Cuartos de Final
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
  
### [W57]
  
```{r}

partido1<-49
partido2<-50

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e49()[1])}
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
                            paste0(e50()[1])}
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
  
### [W58]
  
```{r}
partido1<-53
partido2<-54

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista4s()!="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
                          if(!is.null(lista4s()$data_resumenEquipos)){
                            paste0(e53()[1])}
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
                            paste0(e54()[1])}
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

dataW4s<-reactive({data.frame("Codigo"=c("W_57_E1","W_57_E2",
                                         "W_58_E1","W_58_E2",
                                         "W_59_E1","W_59_E2",
                                         "W_60_E1","W_60_E2"),
                              "Equipo"=c(paste0(e49()[1]),paste0(e50()[1]),
                                         paste0(e53()[1]),paste0(e54()[1]),
                                         paste0(e51()[1]),paste0(e52()[1]),
                                         paste0(e55()[1]),paste0(e56()[1]))
)
  # eventReactive({ input$buttonA input$buttonB input$buttonC input$buttonD input$buttonE input$buttonF input$buttonG input$buttonH}
})


listaSemis<-eventReactive(input$buttonSemis,{
  
  if(vSemis$OK[1] & vSemis$OK[2]){
    # print("holi")
    calcula_Goles(vectorgoles=c(input[["W49"]],input[["W50"]],#input[[paste0("E1",Grupo1)]],
                                input[["W53"]],input[["W54"]],
                                input[["W51"]],input[["W52"]],
                                input[["W55"]],input[["W56"]]),
                  data=dataW4s()
    )
  }else{
    "MENSAJE SIN CONCLUIR SEGUNDA FASE"
  }
})
fluidRow(
  
  column(7,
         actionButton("buttonSemis","  NO TE VAYAS SIN ACTUALIZARME    :P")
  ),
  
  column(7,
         renderTable({
           if(listaSemis()=="MENSAJE SIN CONCLUIR SEGUNDA FASE"){
             m<-matrix("SEGUNDA FASE SIN CONCLUIR CORRECTAMENTE")
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
                 m<-matrix("Por favor revisa que las cantidades de Segunda Fase sean correctas :O")
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