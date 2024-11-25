library(shiny)
library(ggplot2)
library(shinyjs)
library(DT)

# Define UI
ui <- fluidPage(
  useShinyjs(), 
  tags$style(HTML("h4 { color: #2E86C1; } .btn { background-color: #2E86C1; color: white; }")),
  
  titlePanel("Aplicativo de Regressão Múltipla - ME918"),
  
  tabsetPanel(
    tabPanel("1. Carregar Dados",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Faça o upload do arquivo CSV", accept = ".csv"),
                 uiOutput("var_response"),
                 uiOutput("var_predictors"),
                 actionButton("run_analysis", "Executar Regressão")
               ),
               mainPanel(
                 h4("Pré-visualização dos Dados:"),
                 textOutput("data_warning"), 
                 verbatimTextOutput("data_preview")
               )
             )
    ),
    
    tabPanel("2. Análise Exploratória",
             sidebarLayout(
               sidebarPanel(
                 h4("Opções de Análise:"),
                 sliderInput("num_obs", "Número de Observações:", min = 5, max = 50, value = 10, step = 5),
                 checkboxInput("show_summary", "Mostrar Resumo dos Dados", value = TRUE),
                 checkboxInput("show_correlation", "Mostrar Matriz de Correlação", value = FALSE),
                 checkboxInput("show_plots", "Mostrar Gráficos", value = FALSE)
               ),
               mainPanel(
                 h4("Resumo:"),
                 verbatimTextOutput("exploratory_analysis"),
                 h4("Matriz de Correlação:"),
                 DTOutput("correlation_matrix"),
                 h4("Gráficos:"),
                 plotOutput("exploratory_histogram"),
                 plotOutput("exploratory_scatter")
               )
             )
    ),
    
    tabPanel("3. Resultados da Regressão",
             sidebarLayout(
               sidebarPanel(
                 h4("Modelo Gerado:")
               ),
               mainPanel(
                 verbatimTextOutput("regression_output")
               )
             )
    ),
    
    tabPanel("4. Análise de Resíduos",
             sidebarLayout(
               sidebarPanel(
                 h4("Baixar Gráficos:"),
                 downloadButton("download_residual_plot", "Gráfico de Resíduos"),
                 downloadButton("download_qq_plot", "Gráfico Q-Q")
               ),
               mainPanel(
                 h4("Gráfico de Resíduos:"),
                 plotOutput("residual_plot"),
                 h4("Gráfico de Normalidade (Q-Q):"),
                 plotOutput("qq_plot"),
                 h4("Resumo dos Resíduos:"),
                 verbatimTextOutput("residual_analysis")
               )
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Leitura do arquivo .csv com validações
  data <- reactive({
    req(input$file)
    
    validate(
      need(tools::file_ext(input$file$name) == "csv", "Por favor, envie um arquivo no formato CSV.")
    )
    
    df <- read.csv(input$file$datapath)
    
    validate(
      need(nrow(df) > 0, "O arquivo CSV está vazio.")
    )
    
    return(df)
  })
  
  # Atualização dinâmica na interface
  observe({
    if (is.null(input$file)) {
      shinyjs::hide("var_response")
      shinyjs::hide("var_predictors")
      shinyjs::hide("run_analysis")
    } else {
      shinyjs::show("var_response")
      shinyjs::show("var_predictors")
      shinyjs::show("run_analysis")
    }
  })
  
  # Pré-visualização dos dados carregados
  output$data_preview <- renderPrint({
    req(data())
    head(data(), input$num_obs)
  })
  
  # Criação de campos dinâmicos para selecionar variáveis
  output$var_response <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]
    selectInput("response", "Selecione a variável resposta:", choices = numeric_vars)
  })
  
  output$var_predictors <- renderUI({
    req(data())
    req(input$response)
    predictors <- setdiff(names(data()), input$response)
    selectInput("predictors", "Selecione as variáveis preditoras:", choices = predictors, multiple = TRUE)
  })
  
  # Variável reativa para resultados da regressão
  regression_result <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    req(input$response, input$predictors)
    
    validate(
      need(!is.null(input$response), "Selecione uma variável resposta."),
      need(length(input$predictors) > 0, "Selecione pelo menos uma variável preditora.")
    )
    
    # Criação do modelo de regressão
    df <- data()
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+")))
    model <- lm(formula, data = df)
    
    # Atualiza o modelo na variável reativa
    regression_result(model)
    showNotification("Regressão executada com sucesso!", type = "message", duration = 5)
  })
  
  # Exibição dos resultados da regressão
  output$regression_output <- renderPrint({
    req(regression_result())
    summary(regression_result())
  })
  
  # Análise exploratória
  output$exploratory_analysis <- renderPrint({
    req(data())
    if (input$show_summary) print(summary(data()[1:input$num_obs, ]))
  })
  
  output$correlation_matrix <- renderDT({
    req(data(), input$show_correlation)
    numeric_data <- data()[, sapply(data(), is.numeric), drop = FALSE]
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    datatable(cor_matrix, options = list(scrollX = TRUE, paging = FALSE))
  })
  
  output$exploratory_histogram <- renderPlot({
    req(data(), input$show_plots)
    numeric_data <- data()[, sapply(data(), is.numeric), drop = FALSE]
    ggplot2::ggplot(data = reshape2::melt(numeric_data), aes(x = value)) +
      geom_histogram(bins = 30, fill = "#2E86C1", color = "white") +
      facet_wrap(~variable, scales = "free") +
      labs(title = "Histogramas das Variáveis Numéricas", x = "Valor", y = "Frequência")
  })
  
  output$exploratory_scatter <- renderPlot({
    req(data(), input$response, input$predictors, input$show_plots)
    ggplot(data(), aes_string(x = input$predictors[1], y = input$response)) +
      geom_point(color = "#2E86C1") +
      labs(title = "Dispersão entre Preditora e Resposta", x = input$predictors[1], y = input$response)
  })
  
  # --- Adicionando as análises de resíduos ---
  
  # Resumo dos resíduos
  output$residual_analysis <- renderPrint({
    req(regression_result())
    summary(resid(regression_result()))
  })
  
  # Gráfico de resíduos
  output$residual_plot <- renderPlot({
    req(regression_result())
    model <- regression_result()
    ggplot(data = data.frame(Fitted = fitted(model), Residuals = resid(model)), aes(x = Fitted, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Gráfico de Resíduos", x = "Valores Ajustados", y = "Resíduos")
  })
  
  # Gráfico Q-Q
  output$qq_plot <- renderPlot({
    req(regression_result())
    model <- regression_result()
    qqnorm(resid(model), main = "Q-Q Plot dos Resíduos")
    qqline(resid(model), col = "red")
  })
  
  # Download dos gráficos
  output$download_residual_plot <- downloadHandler(
    filename = function() { "grafico_residuos.png" },
    content = function(file) {
      png(file)
      model <- regression_result()
      plot_data <- data.frame(Fitted = fitted(model), Residuals = resid(model))
      p <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Gráfico de Resíduos", x = "Valores Ajustados", y = "Resíduos")
      print(p)
      dev.off()
    }
  )
  
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
