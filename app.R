library(shiny)

ui <- fluidPage(
  
  titlePanel("Split and rename pdfs based on QR content"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "pdf for splitting",
        accept = ".pdf"
      ),
      downloadButton("dl")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  split_rename <- reactive({
    req(input$file)
    
    split_then_qr_scan(input$file$datapath)
  })
  
  output$table <- renderTable(split_rename())
  
  output$dl <- downloadHandler(
    filename = function() {
      paste0( tools::file_path_sans_ext(input$file$name), ".zip")
    },
    content = function(file) {
      zip(
        file,
        split_rename()$new_path,
        flags = "-j"
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
