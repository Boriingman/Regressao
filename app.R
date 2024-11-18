# Instale o pacote shiny se ainda não tiver
# install.packages("shiny")

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Regressão Simples"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Faça o upload do arquivo CSV", accept = ".csv"),
      uiOutput("var_response"),
      uiOutput("var_predictor"),
      actionButton("run_analysis", "Executar Regressão")
    ),
    
    mainPanel(
      verbatimTextOutput("regression_output")
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
  
  # Criação das opções de variáveis após o upload do arquivo
  output$var_response <- renderUI({
    req(data())
    selectInput("response", "Selecione a variável resposta:", choices = names(data()))
  })
  
  output$var_predictor <- renderUI({
    req(data())
    selectInput("predictor", "Selecione a variável preditora:", choices = names(data()))
  })
  
  # Executa a análise de regressão
  regression_result <- eventReactive(input$run_analysis, {
    req(input$response, input$predictor)
    df <- data()
    lm(as.formula(paste(input$response, "~", input$predictor)), data = df)
  })
  
  # Apresenta os resultados
  output$regression_output <- renderPrint({
    req(regression_result())
    summary(regression_result())
  })
}

# Executa o app
shinyApp(ui = ui, server = server)