library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Análise de Regressão Múltipla"),
  
  tabsetPanel(
    # Aba 1: Upload do arquivo e seleção de variáveis
    tabPanel("Carregar Dados e Selecionar Variáveis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Faça o upload do arquivo CSV", accept = ".csv"),
                 uiOutput("var_response"),
                 uiOutput("var_predictors"),
                 actionButton("run_analysis", "Executar Regressão")
               ),
               mainPanel(
                 verbatimTextOutput("data_preview")
               )
             )
    ),
    
    # Aba 2: Análise Exploratória
    tabPanel("Análise Exploratória",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("show_summary", "Mostrar Resumo dos Dados", value = TRUE),
                 checkboxInput("show_structure", "Mostrar Estrutura dos Dados", value = FALSE)
               ),
               mainPanel(
                 verbatimTextOutput("exploratory_analysis")
               )
             )
    ),
    
    # Aba 3: Resultados da Regressão
    tabPanel("Resultados da Regressão",
             sidebarLayout(
               sidebarPanel(
                 h4("Resultados do Modelo")
               ),
               mainPanel(
                 verbatimTextOutput("regression_output")
               )
             )
    ),
    
    # Aba 4: Análise de Resíduos
    tabPanel("Análise de Resíduos",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("download_residual_plot", "Baixar Gráfico de Resíduos"),
                 downloadButton("download_qq_plot", "Baixar Gráfico Q-Q")
               ),
               mainPanel(
                 plotOutput("residual_plot"),
                 plotOutput("qq_plot"),
                 verbatimTextOutput("residual_analysis")
               )
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Leitura do arquivo .csv
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Pré-visualização dos dados carregados
  output$data_preview <- renderPrint({
    req(data())
    head(data())
  })
  
  # Criação das opções de variáveis após o upload do arquivo
  output$var_response <- renderUI({
    req(data())
    selectInput("response", "Selecione a variável resposta:", choices = names(data()))
  })
  
  output$var_predictors <- renderUI({
    req(data())
    selectInput("predictors", "Selecione as variáveis preditoras:",
                choices = names(data()), multiple = TRUE)
  })
  
  # Reativo para resultados de regressão
  regression_result <- reactiveVal()
  
  observeEvent(input$run_analysis, {
    req(input$response, input$predictors)
    df <- data()
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+")))
    model <- lm(formula, data = df)
    regression_result(model)
  })
  
  # Mostrar os resultados da regressão
  output$regression_output <- renderPrint({
    req(regression_result())
    regression_result()
  })
  
  # Análise exploratória
  output$exploratory_analysis <- renderPrint({
    req(data())
    if (input$show_summary) {
      print(summary(data()))
    }
    if (input$show_structure) {
      print(str(data()))
    }
  })
  
  # Análise de resíduos
  output$residual_analysis <- renderPrint({
    req(regression_result())
    summary(resid(regression_result()))
  })
  
  # Gráfico de resíduos
  output$residual_plot <- renderPlot({
    req(regression_result())
    model <- regression_result()
    ggplot(data = data.frame(
      Fitted = fitted(model), 
      Residuals = resid(model)), 
      aes(x = Fitted, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Gráfico de Resíduos", x = "Valores Ajustados", y = "Resíduos")
  })
  
  # Q-Q Plot
  output$qq_plot <- renderPlot({
    req(regression_result())
    model <- regression_result()
    qqnorm(resid(model), main = "Q-Q Plot dos Resíduos")
    qqline(resid(model), col = "red")
  })
  
  # Download do gráfico de resíduos
  output$download_residual_plot <- downloadHandler(
    filename = function() { "grafico_residuos.png" },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  # Download do gráfico Q-Q
  output$download_qq_plot <- downloadHandler(
    filename = function() { "qq_plot.png" },
    content = function(file) {
      png(file)
      qqnorm(resid(regression_result()), main = "Q-Q Plot dos Resíduos")
      qqline(resid(regression_result()), col = "red")
      dev.off()
    }
  )
}

# Executa o app
shinyApp(ui = ui, server = server)
