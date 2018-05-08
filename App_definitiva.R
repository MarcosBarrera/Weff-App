library(shiny)
library(shinydashboard)
library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)



datos <- read.table("Dataset_Wefferent_Card_Analytics_Completo.txt",
                    header = TRUE,
                    sep="|",
                    dec = ",")

datos$IMPORTE <- as.numeric(paste(datos$IMPORTE))
datos$DIA <- as.Date(datos$DIA)
datos$CP_COMERCIO <- as.factor(datos$CP_COMERCIO)
datos$CP_CLIENTE <- as.factor(datos$CP_CLIENTE)

datos$DIA_LAB <- ordered(datos$DIA_LAB,levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", 
                                                "Friday", "Saturday"))
datos$MES <- factor(datos$MES,levels=c(1:12),labels=c("January","February","March","April","May","June",
                                                      "July","August","September","October","November","December"))

ui <- dashboardPage(
  dashboardHeader(title="Análisis Descriptivo Cajamar"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Analisis Sectorial", tabName = "Analisis_Sectorial"),
      menuItem("Analisis Temporal", tabName = "Analisis_Temporal"),
      menuItem("Base de datos", tabName = "base_datos")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "base_datos",
              sidebarLayout(position = "left",fluid= TRUE,
                            sidebarPanel(selectizeInput(inputId = "tabla_cp_cliente",
                                                        label = "Seleccione Código Postal Cliente",
                                                        choices = unique(datos$CP_CLIENTE),
                                                        multiple = TRUE,
                                                        selected= c("30000")),
                                         selectizeInput(inputId = "tabla_cp_comercio",
                                                        label = "Seleccione Código Postal Comercio",
                                                        choices = unique(datos$CP_COMERCIO),
                                                        multiple = TRUE,
                                                        selected= c("30001")),
                                         selectizeInput(inputId = "tabla_sector",
                                                        label = "Seleccionar sectores:",
                                                        choices = unique(datos$SECTOR),
                                                        multiple = TRUE,
                                                        selected=unique(datos$SECTOR)),
                                         dateRangeInput("tabla_fechas", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                         selectizeInput(inputId = "tabla_franjas",
                                                        label = "Seleccionar franjas:",
                                                        choices = unique(datos$FRANJA_HORARIA),
                                                        multiple = TRUE,
                                                        selected= unique(datos$FRANJA_HORARIA)),
                                         selectizeInput(inputId = "tabla_dia_semana",
                                                        label = "Seleccionar días:",
                                                        choices = unique(datos$DIA_LAB),
                                                        multiple = TRUE,
                                                        selected= unique(datos$DIA_LAB)),
                                         selectizeInput(inputId = "tabla_renta",
                                                        label = "Seleccionar nivel de renta",
                                                        choices = unique(datos$CATEGORIA_RENTA),
                                                        multiple = TRUE,
                                                        selected = unique(datos$CATEGORIA_RENTA)),
                                         selectizeInput(inputId = "tabla_edad",
                                                        label = "Seleccionar rango de edad",
                                                        choices = unique(datos$CATEGORIA_EDAD),
                                                        multiple = TRUE,
                                                        selected = unique(datos$CATEGORIA_EDAD))
                              
                            ),
                            mainPanel(
                              column(6, HTML("Seleccione el tipo de archivo y las variables, después pulse 'Download data'."),
                                     radioButtons(inputId = "filetype",
                                                  label = "Tipo de archivo",
                                                  choices = c("csv", "xlsx"),
                                                  selected = "csv")),
                              column(6,downloadButton("download_data", "Download data")),
                              column(12,dataTableOutput(outputId = "tabla_general"))
                                     )       
                                      
                            )),

             
             
             
      
      
      tabItem(tabName = "Analisis_Sectorial",
              navbarPage("", 
                         tabPanel("Por dia",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  dateRangeInput("dates_dias", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                                  selectizeInput(inputId = "dia_semana",
                                                                 label = "Seleccionar días:",
                                                                 choices = unique(datos$DIA_LAB),
                                                                 multiple = TRUE,
                                                                 selected= c("Monday","Tuesday","Wednesday")),
                                                  
                                                  selectizeInput(inputId = "sectores_dias",
                                                                 label = "Seleccionar sectores:",
                                                                 choices = unique(datos$SECTOR),
                                                                 multiple = TRUE,
                                                                 selected=c("ALIMENTACION","RESTAURACION","MODA Y COMPLEMENTOS",
                                                                            "HOGAR","OCIO Y TIEMPO LIBRE","OTROS","BELLEZA",
                                                                            "SALUD","TECNOLOGIA","HIPERMERCADOS","AUTO"))),
                                                mainPanel(
                                                  plotlyOutput("grafico_sector_dia"))
                                                
                                  ))
                         ,
                         
                         tabPanel("Por franja horaria",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  dateRangeInput("dates_franjas", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                                  selectizeInput(inputId = "franjas",
                                                                 label = "Seleccionar franjas horarias:",
                                                                 choices = unique(datos$FRANJA_HORARIA),
                                                                 multiple = TRUE,
                                                                 selected= c("10-12",
                                                                             "12-14","14-16")),
                                                  selectizeInput(inputId = "sectores_franjas",
                                                                 label = "Seleccionar sectores:",
                                                                 choices = unique(datos$SECTOR),
                                                                 multiple = TRUE,
                                                                 selected=c("ALIMENTACION","RESTAURACION","MODA Y COMPLEMENTOS",
                                                                            "HOGAR","OCIO Y TIEMPO LIBRE","OTROS","BELLEZA",
                                                                            "SALUD","TECNOLOGIA","HIPERMERCADOS","AUTO"))),
                                                mainPanel(
                                                  plotlyOutput("grafico_sector_franja"))
                                                
                                  )),
                         
                         tabPanel("Por nivel de renta",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  
                                                  selectizeInput(inputId = "rentas",
                                                                 label = "Seleccionar nivel de renta",
                                                                 choices = unique(datos$CATEGORIA_RENTA),
                                                                 multiple = TRUE,
                                                                 selected = c("Renta_Baja")),
                                                  selectizeInput(inputId = "sectores_renta",
                                                                 label = "Seleccionar sectores:",
                                                                 choices = unique(datos$SECTOR),
                                                                 multiple = TRUE,
                                                                 selected=c("ALIMENTACION","RESTAURACION","MODA Y COMPLEMENTOS",
                                                                            "HOGAR","OCIO Y TIEMPO LIBRE","OTROS","BELLEZA",
                                                                            "SALUD","TECNOLOGIA","HIPERMERCADOS","AUTO"))),
                                                mainPanel(
                                                  plotlyOutput("grafico_sector_renta")))),
                         tabPanel("Por grupo de edad",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  
                                                  selectizeInput(inputId = "edades",
                                                                 label = "Seleccione rando de edad",
                                                                 choices = unique(datos$CATEGORIA_EDAD),
                                                                 multiple = TRUE,
                                                                 selected=c("Joven")),
                                                  selectizeInput(inputId = "sectores_edad",
                                                                 label = "Seleccionar sectores:",
                                                                 choices = unique(datos$SECTOR),
                                                                 multiple = TRUE,
                                                                 selected=c("ALIMENTACION","RESTAURACION","MODA Y COMPLEMENTOS",
                                                                            "HOGAR","OCIO Y TIEMPO LIBRE","OTROS","BELLEZA",
                                                                            "SALUD","TECNOLOGIA","HIPERMERCADOS","AUTO"))),
                                                mainPanel(plotlyOutput("grafico_sector_edades")))),
                         tabPanel("Por Código Postal Comercio",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  
                                                  selectizeInput(inputId = "cp_comercio",
                                                                 label = "Seleccione Código Postal Comercio",
                                                                 choices = unique(datos$CP_COMERCIO),
                                                                 multiple = TRUE,
                                                                 selected= c("30001")),
                                                  selectizeInput(inputId = "sectores_cp",
                                                                 label = "Seleccionar sectores:",
                                                                 choices = unique(datos$SECTOR),
                                                                 multiple = TRUE,
                                                                 selected=c("ALIMENTACION","RESTAURACION","MODA Y COMPLEMENTOS",
                                                                            "HOGAR","OCIO Y TIEMPO LIBRE","OTROS","BELLEZA",
                                                                            "SALUD","TECNOLOGIA","HIPERMERCADOS","AUTO"))),
                                                mainPanel(plotlyOutput("grafico_sector_cp"))))
              )),
      
      
      tabItem(tabName = "Analisis_Temporal",
              navbarPage("", 
                         tabPanel("Franja Horaria y Dia",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  dateRangeInput("fecha_franja_dia", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                                  selectizeInput(inputId = "franja_dia",
                                                                 label = "Seleccionar franjas:",
                                                                 choices = unique(datos$FRANJA_HORARIA),
                                                                 multiple = TRUE,
                                                                 selected= c("10-12",
                                                                             "12-14","14-16")),
                                                  selectizeInput(inputId = "dia_franja",
                                                                 label = "Seleccionar días:",
                                                                 choices = unique(datos$DIA_LAB),
                                                                 multiple = TRUE,
                                                                 selected= c("Monday","Tuesday","Wednesday"))),
                                                mainPanel(
                                                  plotlyOutput("grafico_franja_dia")))),
                         tabPanel("Franja Horaria y Mes",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  dateRangeInput("fecha_franja_mes", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                                  selectizeInput(inputId = "franja_mes",
                                                                 label = "Seleccionar franjas:",
                                                                 choices = unique(datos$FRANJA_HORARIA),
                                                                 multiple = TRUE,
                                                                 selected= c("10-12",
                                                                             "12-14","14-16")),
                                                  selectizeInput(inputId = "mes_franja",
                                                                 label = "Seleccionar meses:",
                                                                 choices = unique(datos$MES),
                                                                 multiple = TRUE,
                                                                 selected= c(1,2,3,4,5,6))),
                                                mainPanel(
                                                  plotlyOutput("grafico_franja_mes")))),
                         tabPanel("Dia y Mes",
                                  sidebarLayout(position = "left",fluid= TRUE,
                                                sidebarPanel(
                                                  dateRangeInput("fecha_dia_mes", label = "Fecha", start = "2015-10-01", end = "2017-09-30"),
                                                  selectizeInput(inputId = "dia_mes",
                                                                 label = "Seleccionar dia:",
                                                                 choices = unique(datos$DIA_LAB),
                                                                 multiple = TRUE,
                                                                 selected= unique(datos$DIA_LAB)),
                                                  selectizeInput(inputId = "mes_dia",
                                                                 label = "Seleccionar meses:",
                                                                 choices = unique(datos$MES),
                                                                 multiple = TRUE,
                                                                 selected= unique(datos$MES))),
                                                mainPanel(
                                                  plotlyOutput("grafico_dia_mes"))))
                         
              ))
    )
  ))





server <- function(input, output) {
  
  df_sector_dia <- eventReactive(c(input$sectores_dias,input$dia_semana),
                                 datos %>% filter(SECTOR %in% input$sectores_dias & 
                                                    DIA_LAB %in% input$dia_semana))
  
  df_sector_franja <- eventReactive(c(input$sectores_franjas,input$franjas),
                                    datos %>% filter(SECTOR %in% input$sectores_franjas & 
                                                       FRANJA_HORARIA %in% input$franjas))
  
  df_sector_renta <- eventReactive(c(input$sectores_renta,input$rentas),
                                   datos %>% filter(SECTOR %in% input$sectores_renta & 
                                                      CATEGORIA_RENTA %in% input$rentas))
  df_sector_edad <- eventReactive(c(input$sectores_edad,input$edades),
                                  datos %>% filter(SECTOR %in% input$sectores_edad & 
                                                     CATEGORIA_EDAD %in% input$edades))
  df_sector_cp <- eventReactive(c(input$sectores_cp,input$cp_comercio),
                                datos %>% filter(SECTOR %in% input$sectores_cp &
                                                   CP_COMERCIO %in% input$cp_comercio))
  
  df_franja_dia <- eventReactive(c(input$dia_franja,input$franja_dia),
                                 datos %>% filter(DIA_LAB %in% input$dia_franja & FRANJA_HORARIA %in% input$franja_dia))
  df_franja_mes <- eventReactive(c(input$mes_franja,input$franja_mes),
                                 datos %>% filter(MES %in% input$mes_franja &
                                                    FRANJA_HORARIA %in% input$franja_mes))
  
  df_dia_mes <- eventReactive(c(input$dia_mes,input$mes_dia),
                                 datos %>% filter(DIA_LAB %in% input$dia_mes & MES %in% input$mes_dia))
  
  
  df_tabla = eventReactive(c(input$tabla_sector,input$tabla_cp_cliente,input$tabla_cp_comercio,
                             input$tabla_dia_semana,input$tabla_dia_semana,input$tabla_renta,
                             input$tabla_edad,input$tabla_franjas),
                           datos %>% filter(SECTOR %in% input$tabla_sector &
                                              CP_CLIENTE %in% input$tabla_cp_cliente & 
                                              CP_COMERCIO %in% input$tabla_cp_comercio & 
                                              DIA_LAB %in% input$tabla_dia_semana &
                                              CATEGORIA_RENTA %in% input$tabla_renta &
                                              CATEGORIA_EDAD %in% input$tabla_edad & 
                                              FRANJA_HORARIA %in% input$tabla_franjas))
  
  
  output$grafico_sector_dia <- renderPlotly({
    datos_sector_dia <- 
      df_sector_dia() %>% 
      group_by(DIA_LAB,SECTOR) %>% 
      summarise_if(is.numeric,sum)  %>% 
      select(DIA_LAB,SECTOR,IMPORTE)
    
    p1 <- ggplot(data= datos_sector_dia,aes(x=DIA_LAB, y= IMPORTE, colour= SECTOR,group= SECTOR)) +
      geom_line()
    
    ggplotly(p1)
    
  })
  
  output$grafico_sector_franja <- renderPlotly({
    datos_sector_franja <- 
      df_sector_franja() %>% 
      group_by(FRANJA_HORARIA,SECTOR) %>% 
      summarise_if(is.numeric,sum)  %>% 
      select(FRANJA_HORARIA,SECTOR,IMPORTE)
    
    p2 <- ggplot(data=datos_sector_franja,aes(x=FRANJA_HORARIA, y=IMPORTE, colour=SECTOR,group=SECTOR)) +
      geom_line()
    
    ggplotly(p2)
    
  })
  
  output$grafico_sector_renta <- renderPlotly({
    
    datos_sector_renta <- 
      df_sector_renta() %>% 
      group_by(CATEGORIA_RENTA,SECTOR) %>% 
      summarise_if(is.numeric,sum)  %>% 
      select(CATEGORIA_RENTA,SECTOR,IMPORTE) %>%
      mutate(TOTAL_RENTA = ave(IMPORTE,CATEGORIA_RENTA,FUN=sum)) %>% 
      mutate(PORCENTAJE_RENTA = IMPORTE/TOTAL_RENTA)
    
    p3 <- ggplot(data=datos_sector_renta,aes(x=CATEGORIA_RENTA, y=PORCENTAJE_RENTA, fill=CATEGORIA_RENTA)) +
      geom_bar(stat = "identity") + facet_wrap(~SECTOR)
    
    ggplotly(p3)
    
    
  })
  
  output$grafico_sector_edades <- renderPlotly({
    datos_sector_edades <- 
      df_sector_edad() %>% 
      group_by(CATEGORIA_EDAD,SECTOR) %>% 
      summarise_if(is.numeric,sum)  %>% 
      select(CATEGORIA_EDAD,SECTOR,IMPORTE) %>% 
      mutate(TOTAL_EDAD = ave(IMPORTE,CATEGORIA_EDAD,FUN=sum)) %>% 
      mutate(PORCENTAJE_EDAD = IMPORTE/TOTAL_EDAD)
    
    p4 <- ggplot(data=datos_sector_edades,aes(x=CATEGORIA_EDAD, y=PORCENTAJE_EDAD, fill=CATEGORIA_EDAD)) +
      geom_bar(stat = "identity") + facet_wrap(~SECTOR)
    
    ggplotly(p4)
    
  })
  
  output$grafico_sector_cp <- renderPlotly({
    datos_sector_cp <- 
      df_sector_cp() %>% 
      group_by(CP_COMERCIO ,SECTOR) %>% 
      summarise_if(is.numeric,sum)  %>% 
      select(CP_COMERCIO ,SECTOR,IMPORTE) %>% 
      mutate(TOTAL_CP = ave(IMPORTE,CP_COMERCIO,FUN=sum)) %>% 
      mutate(PORCENTAJE_CP = IMPORTE/TOTAL_CP)
    
    p5 <- ggplot(data=datos_sector_cp,aes(x= CP_COMERCIO, y= PORCENTAJE_CP, fill= CP_COMERCIO)) +
      geom_bar(stat = "identity") + facet_wrap(~SECTOR)
    
    ggplotly(p5)
    
  })
  
  
  output$grafico_franja_dia <- renderPlotly({
    datos_franja_dia <- 
      df_franja_dia() %>% 
      group_by(DIA_LAB,FRANJA_HORARIA) %>% 
      summarise_if(is.numeric,sum) %>% 
      select(DIA_LAB,FRANJA_HORARIA,IMPORTE) 
    
    p6 <- ggplot(data=datos_franja_dia,aes(x= FRANJA_HORARIA, y= IMPORTE,colour=DIA_LAB,group=DIA_LAB)) +
      geom_line()
    
    ggplotly(p6)
  }
  )
  
  output$grafico_franja_mes <- renderPlotly({
    datos_franja_mes <- 
      df_franja_mes() %>%
      group_by(MES,FRANJA_HORARIA) %>% 
      summarise_if(is.numeric,sum) %>% 
      select(MES,FRANJA_HORARIA,IMPORTE) 
    
    p7 <- ggplot(data=datos_franja_mes,aes(x= FRANJA_HORARIA, y= IMPORTE,colour=MES,group=MES)) +
      geom_line()
    
    ggplotly(p7)
  }
  )
  
  output$grafico_dia_mes <- renderPlotly({
    datos_dia_mes <- 
      df_dia_mes() %>%
      group_by(MES,DIA_LAB) %>% 
      summarise_if(is.numeric,sum) %>% 
      select(MES,DIA_LAB,IMPORTE) 
    
    p8 <- ggplot(data=datos_dia_mes,aes(x= MES, y= IMPORTE,colour=DIA_LAB,group=DIA_LAB)) +
      geom_line()
    
    ggplotly(p8)
  }
  )
  
  
  output$tabla_general = renderDataTable({
    df_tabla()
  },options = list(scrollX = TRUE,lengthMenu = c(5,10),pageLength=5))
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("WefferentCards.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write.csv(df_tabla(), file) 
      }
      if(input$filetype == "xlsx"){ 
        write.xlsx(df_tabla(), file) 
      }
    }
  )
  
  
  
}

shinyApp(ui, server)


