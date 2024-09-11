suppressPackageStartupMessages({
  library(pacman)
  p_load(shiny, plotly, shinydashboard, dplyr, readxl, tidyr, ggplot2, data.table, shinyWidgets, DT, htmlwidgets, reshape2, RColorBrewer,
         tidyverse, lubridate, scales, rhandsontable, shinythemes, bslib, vistime, shinyscreenshot, readxl, bizdays, kableExtra)
})




extrair_top_materias <- function(dataframe, gerente_da_hora){
  ordem <- dataframe%>%
    mutate(gerente = str_split(gerente, ",\\s*")) %>%
    unnest(gerente) %>%
    mutate(assuntos = str_split(materias, ",\\s*")) %>%
    unnest(assuntos) %>%
    filter(gerente == gerente_da_hora) %>%
    group_by(assuntos) %>%
    summarise(frequencia = n()) %>%
    arrange(desc(frequencia))
    
  
  
  
  x <- c(gerente_da_hora, ordem$assuntos)
  
  while(length(x) < 4){
    x <- c(x, NA)
  }
  
  while(length(x) > 4){
    x <- x[-length(x)]
  }
  return(x)
}


#setwd("~/Documents/ESTAT/Controle de Atrasos/dash")
#source("~/Documents/ESTAT/Controle de Atrasos/pegando_dados_pipefy.R") # Extraindo o bd do pipefy, mas tenho medo de na hora de postar isso não funcionar, de qualuqer maneira, podemos rodar isso antes de postar o site



banco_recem_atualizado <- read_excel("banco_controle_de_atrasos.xlsx")
# names(banco_recem_atualizado) <- c("nome_projeto", "fase_projeto", "etiquetas", "data_vencimento", "ods", "gerente", "comercial", "gestor", "valor_projeto", "materias", "inicio_execucao", "fim_execucao", "tempo_total_execucao")
# 
# 
# 
# banco_recem_atualizado <- banco_recem_atualizado %>%
#   mutate(pf_pj = str_extract(nome_projeto, "\\w{2}"), 
#          atraso = bizdays(data_vencimento, fim_execucao)) #atraso é positivo, numeros negativos sao datas adiantadas
#          







nome_projetos<-unique(banco_recem_atualizado$`Título`) %>%
  sort()

Sys.getlocale()


ui = function(request){
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "Controle de Projetos ESTAT", titleWidth = 430
    ),
    
    dashboardSidebar(sidebarMenu(
      menuItem("Geral", tabName = "geral", icon = icon("house")),
      menuItem("Progresso", tabName = "progresso", icon = icon("bars-progress")),
      menuItem("Estudo", icon = icon("chart-line"),
               menuSubItem("Projetistas", tabName = "estudo-projetistas"),
               menuSubItem("Projetos", tabName = "estudo-projetos")),
      menuItem("Upload banco", icon = icon("upload"),
               fileInput("upload", "Upload banco", accept = c(".csv", ".xlsx"), width = "100%")
      ),
      menuItem("Sobre esse aplicativo shiny", tabName = "info-geral", icon = icon("circle-info")))),
    
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "geral",
                
                fluidRow(
                  valueBoxOutput("completos", width = 3),
                  valueBoxOutput("em_progresso", width = 3),
                  valueBoxOutput("atrasados", width = 3),
                  valueBoxOutput("nps", width = 3)
                ),
                
                fluidRow(
                  box(downloadButton("downloadData", "Download"),DTOutput("tbl_geral"), width = 12, solidHeader = TRUE, status = "info", title = "Planilha dos Projetos", collapsible = TRUE),
                  box(plotlyOutput("fase_do_projeto"), width = 4,solidHeader = TRUE, status = "success", title = "Fase de Projeto", collapsible = TRUE),  
                  box(plotlyOutput("pj_pf_projeto"), width = 4,solidHeader = TRUE, status = "success", title = "Tipo de Projeto", collapsible = TRUE),  
                  box(plotlyOutput("tipos_de_projetos"), width = 12,solidHeader = TRUE, status = "primary", title = "Matérias", collapsible = TRUE),
                  box(DTOutput("proximos_prazos"), width = 6,solidHeader = TRUE, status = "danger", title = "Próximos Prazos", collapsible = TRUE),
                  box(DTOutput("projetos_atrasados"), width = 6,solidHeader = TRUE, status = "danger", title = "Projetos Atrasados", collapsible = TRUE)
                  
                  
                ) ),
        
        tabItem(tabName = "progresso",
                fluidRow(box(width = 20,textOutput(outputId = "gg1"),
                             downloadButton('downloadPlot1', 'Download'),
                             plotlyOutput(outputId = "gantt1", height = 600, width="100%")),
                         
                         
                         box(width = 20,textOutput(outputId = "gg2"),
                             downloadButton('downloadPlot2', 'Download'),
                             plotlyOutput(outputId = "gantt2", height = 600, width="100%"))
                         
                         )),

        #Alterar essa aba para ser referente a cada projeto em específico com infos do pipefy sobre ele ou filtragem de gerentes/areas/projetos de x maneira
        tabItem(tabName = "estudo-projetistas",
                
                fluidRow(
                  box(DTOutput("principais_materias"), width = 12, status = "danger", title = "Principais Matérias"),
                  box(plotlyOutput("frequencia_projeto_x_gerente"), width = 12, status = "success", title = "Frequência de Projetos"),
                  box(plotlyOutput("linha_temporal_projetos_gerenciados"), width = 12 , status = "primary", title = "Linha Temporal Gerenciamento")
                         
                ) ),
        
        tabItem(tabName = "estudo-projetos",
                tabsetPanel(
                  tabPanel("Card do Pipefy", 
                           fluidRow(
                             box(
                               width = 12, status = "primary",
                               selectInput("filter_pipefy", "Projeto", choices = nome_projetos), 
                               htmlOutput("card_pipefy"), 
                               plotlyOutput("timeline_unico_projeto"))
                           )),
                  tabPanel("Projetos", 
                           fluidRow(
                             box(plotlyOutput("atraso_projeto", height = "450px"), width = 12, status = "warning")
                           )),
                  tabPanel("ODS", 
                           fluidRow(
                             box(plotlyOutput("ods_barras", height = "450px"), width = 12, status = "warning")
                           ))
                  )),
                  
        tabItem(tabName = "info-geral",
                fluidRow(
                  box(width = 20,h2("Objetivo"),
                      h4(p(
                        "A diretoria de projetos da ESTAT trabalha com muitas demandas ao mesmo tempo. No interesse de implementar métricas e
                         fazer melhores análises dos próprios projetos, como também acessar principais informações sobre eles na fase de gerenciamento,
                         tornou-se necessário a criação desse aplicativo Shiny." ),

                        p("Fazem semestres que a diretoria anseia pelo desenvolvimento deste dashboard. Dessa maneira, foi no semestre de 2024.2 que finalmente
                        houve recursos(tempo) para começar a desenvolver melhores ferramentas, cujo intuito é agilizar e aprimorar as principais operações da diretoria."),
                        
                        p("Nesse aplicativo shiny, tem-se como objetivo utilizar de incríveis ferramentos do R shiny como também demonstrar a praticidade de múltiplos pacotes e implementações
                        para contabilizar o progresso de projetos, fazer planejamentos e também ótimas visualizações dos dados disponibilizados."))
                  )
                ),
                br(),
                fluidRow(
                  box(width = 20,h2("Código"),
                      h4(p("O código feito para o desenvolvimento desse aplicativo, como também o primeiro banco teste estão disponíveis no ",tags$a(href="https://github.com/gahenvi1/dash","Github.")))
                  )
                ),
                br(),
                fluidRow(
                  box(width = 20,h2("Fontes"),
                      h4( p("Esse projeto foi grandemente inspirado pelo aplicativo desenvolvido pela Dr Yuanyuan Zhang e Lucy Njoki Njuki, ambas da Universidade de Manchester, no Confy de 2023, 
                      'Making Project Management Seamless using Automated Gantt Charts Tool'."),
                          
                          p("A seguir vem as referências utilizada por tais:"),
                          
                          p("Um blog postado sobre Crindo uma linda Gantt chart com ggplot2 por Andrew Heiss.",tags$a(href="https://stats.andrewheiss.com/misc/gantt.html","Github.")),
                          p("Um aplicativo shiny ",tags$a(href="https://github.com/AABoyles/ShinyGanttCharts","ShinyGanttCharts"),"desenvolvido por Anthony A. Boyles."),
                          p(tags$a(href="https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html","A rhandsontable"),"introdução do blog postado por Jonathan Owen"),
                          p("Um blog postado sobre",tags$a(href="https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html","gerenciamento de Projetos"),"por Amit Kulkarni")
                      )
                  )),
                br(),
                fluidRow(
                  box(width = 20,h2("Criador"),
                      h4(p("Gabriel Henrique Ovídio de Araújo, Graduando em Estatística, Departamento de Estatística, Instituto de Ciências Exatas, Universidade de Brasília")))
                ),
                
                fluidRow(
                  box(width = 20,h2("Contato"),
                      h4(p("Gabriel Henrique Ovídio de Araújo (gabriel.henrique.ovidio@gmail.com)"),
                         p("ESTAT (https://estat.com.br)")))
                ),
                br(),
                br(),
                tags$img( height = 200, width = 200, src= "https://media.licdn.com/dms/image/C4E0BAQEPOt7N9K8hhQ/company-logo_200_200/0/1638486169145/estat_consultoria_logo?e=2147483647&v=beta&t=lvbmod9aRikwxQtMlq8bvwFwXknEKppmJv6yu0V4qgA") #adicionar imagem da estat??
                
        )
        
      )
    ))
  
}



server <- function(input, output, session){
  
  
  df_projetos <- reactive({
    if (is.null(input$upload)) {
      # No file uploaded, use default dataset
      df <- banco_recem_atualizado
    } else {
      # File uploaded, read and use that data
      file <- input$upload
      ext <- tools::file_ext(file$name)
      
      if (ext == "csv") {
        df <- read.csv(file$datapath)
      } else if (ext == "xlsx") {
        df <- read_excel(file$datapath)
      } else {
        showNotification("Unsupported file type", type = "error")
        return(NULL)
      }
    }
    
    
    names(df) <- c("nome_projeto", "fase_projeto", "etiquetas", "data_vencimento", "ods", "gerente", "comercial", "gestor", "valor_projeto", "materias", "inicio_execucao", "fim_execucao", "tempo_total_execucao")
    
    
    
    df <- df %>%
      mutate(pf_pj = str_extract(nome_projeto, "\\w{2}"), 
             atraso = bizdays(data_vencimento, fim_execucao)) %>%#atraso é positivo, numeros negativos sao datas adiantadas
      arrange(-desc(fase_projeto))  
    
    
    df  
    
    
  })
  
  
  bd_gerentes <- reactive({
    df_projetos() %>%
      mutate(gerente = str_split(gerente, ",\\s*")) %>%
      unnest(gerente)  
    
  })
  
  
  
  output$completos <- renderValueBox({
    
    projetos_executados <- df_projetos() %>%
      filter(fase_projeto == "Finalizados™") 
    
    valueBox(value = tags$p(NROW(projetos_executados), style = "font-size: 150%;"), 
             "Finalizados", icon = icon("stack-overflow"), color = "fuchsia")
  })
    
  output$em_progresso <- renderValueBox({
    
    projetos_executando <- df_projetos() %>%
      filter(fase_projeto == "Execução")
    
    valueBox(value = tags$p(NROW(projetos_executando), style = "font-size: 150%;"),  
             "Em execução ", icon = icon("spinner"), color = "green")
  })
  
  output$atrasados <- renderValueBox({
    
    projetos_atrasados <- df_projetos() %>%
      filter(fase_projeto == "Execução") %>%
      mutate(atraso = bizdays(data_vencimento, today())) %>%
      filter(atraso > 0) 
    
    valueBox(value = tags$p(NROW(projetos_atrasados), style = "font-size: 150%;"),  
             "Atrasando ", icon = icon("fire"), color = "red")
    
  })
  
  output$nps <- renderValueBox({
    
    no_aguardo_projetos <- df_projetos() %>%
      filter(fase_projeto %in% c("NPS", "Forms de feedback")) 
    
    valueBox(value = tags$p(NROW(no_aguardo_projetos), style = "font-size: 150%;"),   
             "NPS/Feedback", icon = icon("comment"), color = "blue")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dt_projects, file)
    }
  )
  
  output$tbl_geral <- renderDT({
    
    banco <- df_projetos() %>%
      select(c(nome_projeto, inicio_execucao, data_vencimento, fim_execucao, gerente, pf_pj, materias, fase_projeto, atraso)) %>%
      mutate(
        data_vencimento =  format(ymd_hms(data_vencimento), "%d-%m-%Y"),
        inicio_execucao = format(ymd_hms(inicio_execucao), "%d-%m-%Y"),
        fim_execucao = format(ymd_hms(fim_execucao), "%d-%m-%Y")) 
    
    names(banco) <- c("Projeto", "Data de Início", "Data de Fim(Esperado)", "Data de Fim(Observado)", "Gerentes", "PF/PJ", "Matérias", "Fase", "Atraso(dias úteis)*")
    
    datatable(banco, options = list(pageLength = 12, language = list(url = "https://cdn.datatables.net/plug-ins/2.1.3/i18n/pt-BR.json")))
    
  })
  
  
  
  output$fase_do_projeto <- renderPlotly({
    
    df_fases <- df_projetos() %>% 
      group_by(fase_projeto) %>% 
      summarise("Quantidade" = n()) %>%
      filter(`Quantidade` != 0)
    
    fig <- plot_ly(type='pie', labels=df_fases$fase_projeto, values=df_fases$Quantidade, 
                   textinfo='label+percent',insidetextorientation='radial')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig
    
  })
    
  output$pj_pf_projeto <- renderPlotly({
    
    df_fases <- df_projetos() %>% 
      group_by(pf_pj) %>% 
      summarise("Quantidade" = n()) %>%
      filter(`Quantidade` != 0)
    
    fig <- plot_ly(type='pie', labels=df_fases$pf_pj, values=df_fases$Quantidade, 
                   textinfo='label+percent',insidetextorientation='radial')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig
    
  })
  
  output$tipos_de_projetos <- renderPlotly({
    
   df_projetos() %>%
      mutate(topicos = str_split(materias, ",\\s*")) %>%
      unnest(topicos) %>%
      group_by(topicos) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
    
    ggplot() +
      aes(x = fct_reorder(topicos, n, .desc=F), y = n) +
      geom_col(fill = "steelblue") +
      scale_fill_hue() +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") %>%
      labs(y = "Quantidade", x = "", fill = "")
    
  })
  
  output$proximos_prazos <- renderDT({
    
    df_prazo_chegando <- df_projetos() %>%
      filter(fase_projeto %in% c("Caixa de entrada", "Execução")) %>%
      mutate(dias_uteis_faltando = bizdays(today(), data_vencimento)) %>%  
      select(nome_projeto, data_vencimento, dias_uteis_faltando, gerente) %>%
      arrange(-desc(dias_uteis_faltando)) %>%
      mutate(data_vencimento =  format(ymd_hms(data_vencimento), "%d-%m-%Y"))
    
    names(df_prazo_chegando) <- c("Nome", "Entrega Esperada", "Dias Úteis Restantes", "Gerente")
    
    datatable(df_prazo_chegando,options = list(pageLength = 5, language = list(url = "https://cdn.datatables.net/plug-ins/2.1.3/i18n/pt-BR.json")))
    
  })
  
  output$projetos_atrasados <- renderDT({
    
    df_atrasados <- df_projetos() %>%
      filter(fase_projeto == "Execução") %>%
      mutate(atraso = bizdays(data_vencimento, today())) %>%
      filter(atraso > 0)  %>%
      select(nome_projeto, atraso, gerente) %>%
      arrange(desc(atraso))
    
    names(df_atrasados) <- c("Nome", "Dias Úteis Atrasados", "Gerente")
    
    datatable(df_atrasados,options = list(pageLength = 5, language = list(url = "https://cdn.datatables.net/plug-ins/2.1.3/i18n/pt-BR.json")))
    
  })
  
  
  
  output$gg1 <- renderText({ paste("A seguinte Gantt Chart feita com ggplot2 para os projetos já finalizados(datas observadas): ")})
  
  createPlot1 <- reactive({
    # req(input$hot)
    # 
    # tasks <- hot_to_r(input$hot)
    tasks <- df_projetos() %>% select("nome_projeto","inicio_execucao","fim_execucao", "gerente") %>%
      mutate(gerente = str_split(gerente, ",\\s*")) %>%
      unnest(gerente) %>%
      na.omit() 
      
    
    
    
    tasks.long <- tasks %>%
      gather(date.type, task.date, -c(nome_projeto, gerente)) %>%
      arrange(date.type, task.date) %>%
      mutate(nome_projeto = factor(nome_projeto, levels=rev(unique(nome_projeto)), ordered=TRUE))
    
    # Calculate where to put the dotted lines that show up every three entries
    x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=3)
    
    theme_gantt <- function(base_size=11) {
      theme_bw(base_size) %+replace%
        theme(panel.background = element_rect(fill="#ffffff", colour=NA),
              axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
              title=element_text(vjust=1.2),
              panel.border = element_blank(), axis.line=element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(size=0.5, colour="grey80"),
              axis.ticks = element_blank(),
              legend.position = "bottom", 
              axis.title = element_text(size=rel(0.8)),
              strip.text = element_text(size=rel(1)),
              strip.background = element_rect(fill="#ffffff", colour=NA),
              panel.spacing.y = unit(1.5, "lines"),
              legend.key = element_blank())
    }
    
    
    tasks.long <- na.omit(tasks.long)
    plot1 <- ggplot(tasks.long) +
      aes(x=nome_projeto, y=task.date, colour=gerente) +
      geom_line(linewidth=6) +
      geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
      guides(colour=guide_legend(title=NULL)) +
      coord_flip() +
      labs(y = "Datas de começo e fim", x = "Projeto")+
      theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))
    
    ggplotly(plot1)
    
  })
  
  output$gantt1 <- renderPlotly({
    print(createPlot1())
  })
  
  output$downloadPlot1 <- downloadHandler(
    filename = "ShinyGanttChart1.html",
    content = function(file){
      htmlwidgets::saveWidget(as_widget(createPlot1()), file)
    }
  )
  
  output$gg2 <- renderText({ paste("A seguinte Gantt Chart feita com ggplot2 para os projetos por vir(datas esperadas): ")})
  
  createPlot2 <- reactive({

    tasks <- df_projetos() %>% 
      filter(fase_projeto %in% c("Caixa de entrada", "Execução", "Fase de espera")) %>%
      select("nome_projeto","inicio_execucao","data_vencimento", "gerente") %>%
      mutate(gerente = str_split(gerente, ",\\s*")) %>%
      unnest(gerente) %>%
      na.omit() 
    
    
    
    
    tasks.long <- tasks %>%
      gather(date.type, task.date, -c(nome_projeto, gerente)) %>%
      arrange(date.type, task.date) %>%
      mutate(nome_projeto = factor(nome_projeto, levels=rev(unique(nome_projeto)), ordered=TRUE))
    
    # Calculate where to put the dotted lines that show up every three entries
    x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=3)
    
    theme_gantt <- function(base_size=11) {
      theme_bw(base_size) %+replace%
        theme(panel.background = element_rect(fill="#ffffff", colour=NA),
              axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
              title=element_text(vjust=1.2),
              panel.border = element_blank(), axis.line=element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(size=0.5, colour="grey80"),
              axis.ticks = element_blank(),
              legend.position = "bottom", 
              axis.title = element_text(size=rel(0.8)),
              strip.text = element_text(size=rel(1)),
              strip.background = element_rect(fill="#ffffff", colour=NA),
              panel.spacing.y = unit(1.5, "lines"),
              legend.key = element_blank())
    }
    
    
    tasks.long <- na.omit(tasks.long)
    plot2 <- ggplot(tasks.long) +
      aes(x=nome_projeto, y=task.date, colour=gerente) +
      geom_line(linewidth=6) +
      geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
      guides(colour=guide_legend(title=NULL)) +
      coord_flip() +
      labs(y = "Datas de começo e fim", x = "Projeto")+
      theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))
    
    ggplotly(plot2)
    
  })
  
  output$gantt2 <- renderPlotly({
    print(createPlot2())
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = "ShinyGanttChart2.html",
    content = function(file){
      htmlwidgets::saveWidget(as_widget(createPlot2()), file)
    }
  )
  
  
  
  
  
  output$principais_materias <- renderDT({

    
    gerentes <- unique(bd_gerentes()$gerente)
    
    
    df_ordem_materias <- map(gerentes, function(x) extrair_top_materias(df_projetos(), x))  
    
    df_ordem_materias <- do.call(rbind, lapply(df_ordem_materias, `length<-`, max(lengths(df_ordem_materias)))) %>%
      as.data.frame(stringAsFactors = FALSE) %>%
      arrange(-desc(V1))
                             
    
    
    names(df_ordem_materias) <- c("Gerente", "1", "2", "3")
    
    
    datatable(df_ordem_materias,options = list(pageLength = length(gerentes), language = list(url = "https://cdn.datatables.net/plug-ins/2.1.3/i18n/pt-BR.json"), na = "--"), filter = "top")
    
  })
  
  
  output$frequencia_projeto_x_gerente <- renderPlotly({
  
  df_freq_projetista <- df_projetos() %>%
    mutate(gerente = str_split(gerente, ",\\s*")) %>%
    unnest(gerente) %>%
    group_by(gerente) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n/sum(n), 2)) %>%
    arrange(desc(n))
  
  
  fig <- plot_ly(
    df_freq_projetista,
    y = ~n,
    x = ~reorder(gerente, n),
    color = ~gerente,
    text = ~freq,
    type = "bar",
    marker = list(line = list(color = "black", width = 1.5))
  )
  
  
  fig <- fig %>% layout(title = "",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")) 
  
  fig
  
  })
  
  
  output$linha_temporal_projetos_gerenciados <- renderPlotly({
    
    # Expandir as datas de início e fim para obter uma sequência de datas
    projetos_expandidos <- bd_gerentes() %>%
      filter(!is.na(inicio_execucao) & !is.na(fim_execucao)) %>%
      rowwise() %>%
      mutate(periodo = list(seq(inicio_execucao, fim_execucao, by = "day"))) %>%
      unnest(periodo)
    
    # Calcular o número de projetos por gerente ao longo do tempo
    projetos_por_gerente <- projetos_expandidos %>%
      group_by(gerente, periodo) %>%
      summarise(qtd_projetos = n(), .groups = 'drop')
    
    # Criar um gráfico de linha para visualizar as mudanças ao longo do tempo
    ggplot(projetos_por_gerente, aes(x = periodo, y = qtd_projetos, color = gerente, group = gerente)) +
      geom_line(size = 1) +
      labs(title = "Quantidade de Projetos por Gerente ao Longo do Tempo",
           x = "Data",
           y = "Quantidade de Projetos") +
      theme_minimal()
    
    
    
  }) 
  
  
  output$card_pipefy <- renderUI({
    card_pipefy <- df_projetos() %>%
      filter(nome_projeto == input$filter_pipefy) %>%
      setNames(c("Nome", "Fase", "Etiquetas", "Vencimento", "ODS", 
                 "Gerente de Projetos", "Gerente de Comercial", "Acessor de Gestão", "Valor", "Matérias", "Execução(início)", "Execução(fim)", 
                 "Duração da Execução", "Tipo", "Atraso")) %>%
      t() %>%
      as.data.frame()
    
    names(card_pipefy) <- NULL
    
    HTML(kable(card_pipefy, format = "html") %>%
           kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
           column_spec(1, bold = TRUE)
    )
    
    
  })
  
  output$timeline_unico_projeto <- renderPlotly({
    vistime_data <- df_projetos() %>%
      filter(nome_projeto == input$filter_pipefy) 
    
    
    timeline_data <- data.frame(event = c("Observado", "Esperado"),
                                start = c(vistime_data$inicio_execucao, vistime_data$inicio_execucao), 
                                end = c(vistime_data$fim_execucao, vistime_data$data_vencimento),
                                color = c("#ff7f0e", "#1f77b4")) %>%
      na.omit()
    
    
    
    vistime(timeline_data)
  })
  
  
  output$atraso_projeto <- renderPlotly({
    banco_graf_atrasos <- df_projetos() 
    
    banco_graf_atrasos$nome_projeto <- reorder(banco_graf_atrasos$nome_projeto, banco_graf_atrasos$atraso)
    
    
    plot_ly(data = banco_graf_atrasos, 
            x = ~nome_projeto, 
            y = ~atraso,
            color = ~gerente,
            text = ~paste(nome_projeto, "<br>Gerente: ", gerente, "<br>Atraso: ", atraso),
            hoverinfo = "text",
            type = 'bar') %>%
      layout(title = "Atraso dos projetos com execução finalizada",
             xaxis = list(title = "Nome do Projeto"),
             yaxis = list(title = "Dias de Atraso"),
             showlegend = FALSE,
             annotations = list(
               x = 0.5,
               y = 1.05,
               text = "Valores positivos é atraso, enquanto valores negativos são entregas adiantadas",
               showarrow = FALSE,
               xref = "paper",
               yref = "paper",
               xanchor = "center",
               yanchor = "top",
               font = list(size = 12, color = "gray")
             ))
  })
  
  
  output$ods_barras <- renderPlotly({
    banco_graf_ods <- df_projetos() %>%
      group_by(ods) %>%
      summarise(qtd = n())
    
    banco_graf_ods$ods <- reorder(banco_graf_ods$ods, banco_graf_ods$qtd)
    
    
    plot_ly(data = banco_graf_ods, 
            x = ~ods, 
            y = ~qtd,
            color = ~ods,
            type = 'bar') %>%
      layout(title = "Frequência de cada ODS",
             xaxis = list(title = "ODS"),
             yaxis = list(title = "Frequência Absoluta"),
             showlegend = FALSE)
  })
  
  
  
  
  
}




shinyApp(ui = ui, server = server, enableBookmarking = "url")






