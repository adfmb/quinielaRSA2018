Cuartos de Final
=======================================================================
  ```{r}
e49<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==49],n=1)
})
e50<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==50],n=1)
})
e51<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==51],n=1)
})
e52<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==52],n=1)
})
e53<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==53],n=1)
})
e54<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==54],n=1)
})
e55<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==55],n=1)
})
e56<-reactive({#input$buttonA, {
  etopn(lista8vos()$data_resumenEquipos[lista8vos()$data_resumenEquipos$Partido==56],n=1)
})

v4s <- reactiveValues(OK = c(F,F))

observeEvent(input$button8vos, {
  if(lista8vos()=="MENSAJE SIN CONCLUIR PRIMERA FASE"){
    v4s$OK[1] <- F
    v4s$OK[2] <- F
  }else{
    v4s$OK[1] <- lista8vos()$marcaBoton
    v4s$OK[2] <- sum(lista8vos()$data_EquipoPartido$Empate)==0
  }
})

alarma4s<-paste0("¡O.O Nos caemos a pedazos!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e49()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e50()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
```

### [W59]

```{r}

partido1<-51
partido2<-52

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e51()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e52()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e53()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e54()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
``` 

### [W60]

```{r}
partido1<-55
partido2<-56

fluidRow(
  
  column(4,
         h2(paste0("W",partido1))
  ),
  
  column(4,
         numericInput(paste0("W",partido1),
                      renderUI(
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e55()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
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
                        if(lista8vos()!="MENSAJE SIN CONCLUIR PRIMERA FASE"){
                          if(!is.null(lista8vos()$data_resumenEquipos)){
                            paste0(e56()[1])}
                        }else{
                          tags$span(style="color:red",
                                    alarma4s##,unique(lista2A()$data_EquipoPartido$Grupo),"!")
                          )
                        }
                      ),
                      min=0, max=100, value=0,step=1,
                      width = '100px')
  )
)
``` 
