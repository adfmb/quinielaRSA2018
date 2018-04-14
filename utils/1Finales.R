Finales {data-orientation=columns}
=======================================================================
  
  Column
-------------------------------------
  
  ```{r}
e61<-reactive({#input$buttonA, {
  etopn(listaSemis()$data_EquipoPartido[listaSemis()$data_EquipoPartido$Partido==61,],n=2)
})
e62<-reactive({#input$buttonA, {
  etopn(listaSemis()$data_EquipoPartido[listaSemis()$data_EquipoPartido$Partido==62,],n=2)
})

vFinales <- reactiveValues(OK = c(F,F))

observeEvent(input$buttonSemis, {
  if(listaSemis()=="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
    vFinales$OK[1] <- F
    vFinales$OK[2] <- F
  }else{
    vFinales$OK[1] <- listaSemis()$marcaBoton
    vFinales$OK[2] <- sum(listaSemis()$data_EquipoPartido$Empate)==0
  }
})

alarmaSemis<-paste0("¡¡O.O Chequemos las fases anteriores DX DX!!")
```

<!-- Row -->
  <!-- ------------------------------------- -->
  
  ### FINAL
  
  ```{r}

partido1<-61
partido2<-62

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1,"E1"),
                      renderUI(
                        if(listaSemis()!="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
                          if(!is.null(listaSemis()$data_resumenEquipos)){
                            paste0(e61()[1])}
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
         numericInput(paste0("W",partido2,"E1"),
                      renderUI(
                        if(listaSemis()!="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
                          if(!is.null(listaSemis()$data_resumenEquipos)){
                            paste0(e62()[1])}
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

<!-- Row -->
  <!-- ------------------------------------- -->
  
  ### TERCER LUGAR
  
  ```{r}
partido1<-61
partido2<-62

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1,"E2"),
                      renderUI(
                        if(listaSemis()!="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
                          if(!is.null(listaSemis()$data_resumenEquipos)){
                            paste0(e61()[2])}
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
         numericInput(paste0("W",partido2,"E2"),
                      renderUI(
                        if(listaSemis()!="MENSAJE SIN CONCLUIR FASE DE CUARTOS"){
                          if(!is.null(listaSemis()$data_resumenEquipos)){
                            paste0(e62()[2])}
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


Column {data-width=150}
-------------------------------------
  
  ### G A N A D O R
  ```{r}
```  
```{r}
library(shinyjs)

dataW1s<-reactive({data.frame("Codigo"=c("W_64_E1","W_64_E2",
                                         "W_63_E1","W_63_E2"),
                              "Equipo"=c(paste0(e61()[1]),paste0(e62()[1]),
                                         paste0(e61()[2]),paste0(e62()[2]))
)
  # eventReactive({ input$buttonA input$buttonB input$buttonC input$buttonD input$buttonE input$buttonF input$buttonG input$buttonH}
})


listaFinales<-eventReactive(input$buttonFinales,{
  
  if(vFinales$OK[1] & vFinales$OK[2]){
    # print("holi")
    calcula_Goles(vectorgoles=c(input[["W61E1"]],input[["W62E1"]],#input[[paste0("E1",Grupo1)]],
                                input[["W61E2"]],input[["W62E2"]]),
                  data=dataW1s()
    )
  }else{
    "MENSAJE SIN CONCLUIR FASE DE SEMIS"
  }
})
fluidRow(
  
  column(7,
         actionButton("buttonFinales","  NO TE VAYAS SIN ACTUALIZARME    :P")
  ),
  
  column(7,
         renderTable({
           if(listaFinales()=="MENSAJE SIN CONCLUIR FASE DE SEMIS"){
             m<-matrix("FASE DE SEMIFINALES SIN CONCLUIR CORRECTAMENTE")
             colnames(m)<-"¡¡¡O.O!!!!"
             m
           }else if(!is.null(listaFinales()$data_EquipoPartido)){
             if(lista2A()$marcaBoton & lista2B()$marcaBoton){
               if(listaFinales()$marcaBoton){
                 if(sum(listaFinales()$data_EquipoPartido$Empate)==0){
                   pais<-listaFinales()$data_EquipoPartido[1:2,c("Grupo","Partido","Equipo")]
                   as.matrix(pais()$Equipo[pais()$Partido==64])
                   #[,listaFinales()$data_EquipoPartido$Partido==64])
                 }else{
                   m<-matrix("</3 Cámbialo porfiiiiis")
                   colnames(m)<-"¡¡¡NO MAMAR QUE PUSISTE EMPATES!!!!"
                   m
                 }
                 
               }else{
                 m<-matrix("Por favor revisa que las cantidades de sean correctas :O")
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