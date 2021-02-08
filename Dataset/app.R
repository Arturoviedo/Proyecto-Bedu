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
library('tseries')
library('lmtest')


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
                            #filter(Fecha>="2015-02-20")%>%
                            group_by(CRIPTO,Fecha) %>%
                            summarise(Cierre=sum(Cierre,na.rm = TRUE)))
     return(ventas)
  })
  
  output$promediomes = renderPlot({
    datos = base()
    datosts <- ts(data = datos$Cierre)
    arima<-auto.arima(datosts)
    graph = forecast(arima,h=12)
    plot(graph)
  })
  
  output$pronostico = renderTable({
    datos = base()
    datosts <- ts(datos$Cierre)
    arima<-auto.arima(datosts)
    tab = forecast(arima, h=12)
    return(tab)
  })
  
  output$tabla = renderDataTable({
    datos = base()
    return(datos)
  })
}

shinyApp(ui = ui, server = server)