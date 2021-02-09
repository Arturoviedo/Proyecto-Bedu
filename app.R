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
library('plotly')



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
                                   plotlyOutput(outputId = "promediomes"), solidHeader = FALSE, height = "auto", width = "100%")
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
               sliderInput(inputId = "predict_days",
                           label = "Numero de días a predecir:",
                           min = 1,
                           max = 50,
                           value = 30),
               sliderInput(inputId = "plot_points",
                           label = "Cuántos puntos de la grafica deseas saltar:",
                           min = 0,
                           max = 1500,
                           value = 450),
               actionButton("go", "Actualizar", icon("refresh")))
    )
  })
  
  observe({  z<-read.csv("Dataset/Base.csv")
  updateSelectInput(session,inputId = "criptomoneda",label = "Seleccione una criptomoneda:",
                    choices = c(unique(as.character(z$CRIPTO))))
  
  })
  
  base<-eventReactive(input$go,{
    datos<-read.csv("Dataset/Base.csv")
    datos<-datos %>% 
      rename(Fecha = Date, Cierre=Close)
    ventas<-as.data.frame(datos%>%
                            filter(CRIPTO==input$criptomoneda) %>%
                            #filter(Fecha>="2015-02-20")%>%
                            group_by(CRIPTO,Fecha) %>%
                            summarise(Cierre=sum(Cierre,na.rm = TRUE)))
    return(ventas)
  })
  
  output$promediomes = renderPlotly({
    datos = base()
    datosts <- ts(data = datos$Cierre)
    arima<-auto.arima(datosts)
    fore = forecast(arima,h=input$predict_days)
    plot <-plot_ly() %>%
      add_lines(x = time(datosts), y = datosts,
                color = I("black"), name = "datos conocidos")%>%
      add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
                  color = I("gray95"), name = "95% confidence") %>%
      add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
                  color = I("gray80"), name = "80% confidence") %>%
      add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")%>%
      layout(xaxis = list(range=c(input$plot_points,length(datosts)+input$predict_days)),
             title=paste("Precio del ",input$criptomoneda),xaxis=list(title='Día'),yaxis=list(title='USD'))
      
  })
  
  output$pronostico = renderTable({
    datos = base()
    datosts <- ts(datos$Cierre)
    arima<-auto.arima(datosts)
    tab = forecast(arima, h=input$predict_days)
    return(tab)
  })
  
  output$tabla = renderDataTable({
    datos = base()
    return(datos)
  })
}

shinyApp(ui = ui, server = server)
