# Carregar as bibliotecas necessárias
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, leaflet, shiny, sf, geojsonio, leaflet.extras, lubridate, DT)

load("del.Rda") #Dados das delegacias
load("rota.Rda") #Dados das rotas
load("ponto.Rda") #Dados dos pontos

# Carregar e preparar os dados
banco <- read_csv2("dados-abertosmai2022.csv") %>%
  mutate(auinf_local_latitude = as.numeric(auinf_local_latitude), 
         auinf_local_longitude = as.numeric(auinf_local_longitude)) %>%
  filter(!(is.na(auinf_local_latitude))) %>%
  filter(!(auinf_local_latitude %in% 0))

banco$datetime <- parse_date_time(paste(banco$cometimento, banco$hora_cometimento), orders = "dmy HMS")

# Top 10 infrações
top10_infra <- banco %>%
  group_by(descricao) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:10)

banco <- banco %>%
  filter(descricao %in% top10_infra$descricao)

cores = colorFactor("Paired", banco$descricao)


# Definição da interface do usuário
ui <- fluidPage(
  titlePanel("Infrações de Trânsito no DF"),
  theme = shinythemes::shinytheme('darkly'),
  sidebarLayout(
    sidebarPanel(
      numericInput("inicio", "Hora de Início:", value = 0),
      numericInput("fim", "Hora de Término:", value = 24),
      shiny::hr(),
      checkboxInput("grupo", "Agrupar", value = TRUE),
      checkboxInput('point', 'Ponto', value = TRUE),
      checkboxInput('heat', 'Calor', value = FALSE),
      hr(),
      checkboxGroupInput('infracao', 'Infração', top10_infra$descricao, selected = top10_infra$descricao),
      hr(),
      print("Questão Bonus"),
      checkboxInput("del", "Delegacias", value = FALSE),
      checkboxInput("predio_est", "Rota até CIC/EST", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa", leafletOutput('plot'))
      )
    )
  )
)

# Definição da lógica do servidor
server <- function(input, output) {
  
    
  dataset <- reactive({
    banco %>%
      filter(descricao %in% input$infracao) %>%
      filter(input$fim >= hour(datetime) | hour(datetime) >= input$inicio)
  })
  
  
  
  output$plot <- renderLeaflet({
    
    p <- leaflet(dataset()) %>%
      addTiles() %>%
      setView(lng = -47.93835, lat = -15.80875, zoom = 10)
    

    if (input$inicio >= 18) 
      p <- p %>% addProviderTiles(providers$Jawg.Matrix,
                                  options = providerTileOptions(
                                    accessToken = "lMjUumQsRRrpg4Ot8AD6Kn2gukW5iHZoy86bWRdNFsyjO9J3y0aqZ1AI6AHsflgP"))


    if (input$inicio < 18) 
      p <- p %>% addProviderTiles(providers$Jawg.Streets,
                                  options = providerTileOptions(
                                    accessToken = "lMjUumQsRRrpg4Ot8AD6Kn2gukW5iHZoy86bWRdNFsyjO9J3y0aqZ1AI6AHsflgP"))

    
    if (input$point)
      if(input$grupo)
        p <- p %>% 
          addCircleMarkers(lng = ~auinf_local_longitude, 
                           lat = ~auinf_local_latitude,
                           color = ~cores(descricao),
                           stroke = FALSE, fillOpacity = 0.5,
                           clusterOptions = markerClusterOptions())
    else
      p <- p %>% 
        addCircleMarkers(lng = ~auinf_local_longitude, 
                         lat = ~auinf_local_latitude,
                         color = ~cores(descricao),
                         stroke = FALSE, fillOpacity = 0.5)
    
    if(input$heat)
      p <- p %>%
        addHeatmap(lng = ~auinf_local_longitude, 
                   lat = ~auinf_local_latitude, 
                   blur = 25, max = 0.05, radius = 15)
    
    if(input$del)
      p <- p %>%
        addMarkers(lng = ~resultados$geometry$location$lng,
                   lat = ~resultados$geometry$location$lat,
                   popup = ~resultados$name,
                   label = ~resultados$name)
    
    if(input$predio_est)
      p <- p %>%
        addPolylines(data = rota, lat = ~lat, lng = ~lon, color = "darkred", weight = 5) %>%
        addMarkers(data = pontos, lat = ~lat, lng = ~lon, label = ~label, popup = ~popup)
    
    p
  })
  

}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
