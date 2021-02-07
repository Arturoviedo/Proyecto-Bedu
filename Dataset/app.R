library("readxl")
library("rAmCharts")
library("shiny")
library("greybox")
library("shinydashboard")
library("smooth")
library("forecast")
library("data.table")
library("dplyr")
library("lubridate")

#setwd("C:/Users/Dell/Desktop/BEDUfinal/PFinal")

ui = dashboardPage(skin = "black",
                   dashboardHeader(title = "Tablero"),
                   dashboardSidebar(
                     sidebarMenuOutput("menu")
                   ),
                   dashboardBody(
                     
                     tabItems(
                       tabItem(tabName = "dashboard1",
                               fluidRow(
                                 column(8, box(
                                   plotOutput(outputId = "promediomes"), solidHeader = FALSE, height = "auto", width = "100%")
                                 ),
                                 
                                 column(4, box(
                                   tableOutput(outputId = "pronostico"), solidHeader = FALSE, height = "auto", width = "100%")
                                 ),
                                 
                               ),
                               
                               fluidRow(
                                 column(12, box(
                                   dataTableOutput(outputId = "tabla"), solidHeader = FALSE, height = "auto", width = "100%")
                                 )
                               )
                       )
                     )
                   )
)

server = function(input, output, session) {
  
  output$menu = renderMenu({
    
    sidebarMenu(
      menuItem("Analisis de Criptomonedas", tabName = "dashboard1", icon = icon("bar-chart-o")),
      menuItem("Controles", tabName = "controles", icon = icon("bar-char-o"),
               
               selectInput(inputId = "criptomoneda",
                           label = HTML("Seleccione una criptomoneda:"),
                           choices = c("Seleccione una criptomoneda")),
               actionButton("go", "Actualizar", icon("refresh")))
    )
  })
  
  observe({  z<-read.csv("Base.csv")
  updateSelectInput(session,inputId = "criptomoneda",label = "Seleccione una criptomoneda:",
                    choices = c("Seleccione una criptomoneda:",unique(as.character(z$CRIPTO))))

  })
  
  base<-eventReactive(input$go,{
    datos<-read.csv("Base.csv")
    datos<-datos %>% 
      rename(Fecha = Date, Cierre=Close)
    
    ventas<-as.data.frame(datos%>%
                            filter(CRIPTO==input$criptomoneda) %>%
                            group_by(CRIPTO,Fecha) %>%
                            summarise(Cierre=sum(Cierre,na.rm = TRUE)))
    
#    ventas=ventas[,3:5]
 #   for(i in 1:nrow(ventas)){
  #    if(ventas$MES[i]=="ENERO"||ventas$MES[i]=="MARZO"||ventas$MES[i]=="MAYO"||ventas$MES[i]=="JULIO"||ventas$MES[i]=="AGOSTO"||ventas$MES[i]=="OCTUBRE"||ventas$MES[i]=="DICIEMBRE"){
   #     ventas$Dia[i]=31
    #  }else if(ventas$MES[i]=="FEBRERO"){
     #   ventas$Dia[i]=28
#      }else{
 #       ventas$Dia[i]=30
  #    }
   # }
    
    
   # ventas$Fecha<-as.Date(paste(ventas$Dia,"-",ventas$MES,"-","2018",sep=""),format= "%d-%B-%Y")
    ventas$MesN<-month(ventas$Fecha)
    ventas<-select(ventas,Cierre,MesN,Fecha) 
    
    return(ventas)
  })
  
  output$promediomes = renderPlot({
    datos = base()
    graph = forecast(sma(datos$Cierre), h = 12)
    plot(graph)
  })
  
  output$pronostico = renderTable({
    datos = base()
    tab = forecast(sma(datos$Cierre), h = 12)
    return(tab)
  })
  
  output$tabla = renderDataTable({
    datos = base()
    return(datos)
  })
}

shinyApp(ui = ui, server = server)