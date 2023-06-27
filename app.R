library(shiny)
# up to 100mb files
options(shiny.maxRequestSize = 100*1024^2)
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
      input$file$datapath |>
      split_then_qr_scan() |>
      combine_by_student_id() 
      })
  
  output$table <- renderTable({
    split_rename() %>%
      select(student_id)
    })
  
  output$dl <- downloadHandler(
    filename = function() {
      paste0( tools::file_path_sans_ext(input$file$name), ".zip")
    },
    content = function(file) {
      zip(
        file,
        split_rename()$combined,
        flags = "-j"
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
