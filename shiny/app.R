library(shiny)
library(RNifti)
library(shinythemes)
library(shinyWidgets)
library(markdown)
source("lazyr.R")
source("animation.R")

read_img_as_array <- function(path) {
  img_raw <- RNifti::readNifti(path)
  if (length(dim(img_raw)) == 3) return(img_raw[,,])
  return(img_raw[,,,])
}

# Create an empty data.frame to store Q&A data
qa_df <- data.frame(ImageID = character(),
                    Question = character(), 
                    Answer = character(),
                    stringsAsFactors = FALSE)

# Predefined questions and answer choices
questions_answers <- data.frame(
  question = c("Are there any T2 hyperintense white matter lesions periventricularly?", 
               "Are there any juxtacortical/cortical T2 hyperintense white matter lesions?", 
               "Are there any infratentorial T2 hyperintense white matter lesions?"),
  answers = I(list(c('No', '1-2', '3 or more'), 
                   c('None', 'Present'),
                   c('None', 'Present')))
)

ui <- navbarPage(
  "Shiny MRI",
  theme = shinytheme("cyborg"),
  tabPanel(
    "View Mode",
    fluidRow(
      column(4, fileInput("your_dt", "Upload a .nii .nii.gz")),
      column(1, h2("|"), class = "text-center", 
             style = "margin-top: -5px; ")
    ),
    uiOutput("raster_panel_view")
  ),
  tabPanel(
    "Answer Mode",
    fluidRow(
      column(4, fileInput("your_dt_ans", "Upload a .nii .nii.gz")),
      column(1, h2("|"), class = "text-center", 
             style = "margin-top: -5px; ")
    ),
    fluidRow(
      column(4, selectInput("question", "Select a question", 
                            choices = questions_answers$question)),
      column(4, selectInput("answer", "Select your answer", 
                            choices = NULL)),
      column(4, actionButton("save_pair", "Save Question-Answer Pair")),
      column(4, downloadButton("download_data", "Download Q&A Data")),
      column(12, dataTableOutput("saved_pairs"))
    ),
    uiOutput("raster_panel_ans")
  ),
  tabPanel(
    "About",
    includeMarkdown("about.md")
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500*1024^2)
  
  # Reactive value for storing Q&A data
  rv <- reactiveValues(df = qa_df)
  
  app_dt_view <- reactive({
    if (is.null(input$your_dt)) {
      return(NULL)
    }
    datapath <- input$your_dt$datapath
    if (tools::file_ext(datapath) == "gz") {
      datapath <- sub("gz$", "nii.gz", datapath)
      file.rename(input$your_dt$datapath, datapath)
    }
    out <- read_img_as_array(datapath)
    return(out)
  })
  
  app_dt_ans <- reactive({
    if (is.null(input$your_dt_ans)) {
      return(NULL)
    }
    datapath <- input$your_dt_ans$datapath
    if (tools::file_ext(datapath) == "gz") {
      datapath <- sub("gz$", "nii.gz", datapath)
      file.rename(input$your_dt_ans$datapath, datapath)
    }
    out <- read_img_as_array(datapath)
    return(out)
  })
  
  output$raster_panel_view <- renderUI({
    callModule(raster3d_animation_Module, "mri_3d_view", im = app_dt_view)
    raster3d_animation_UI("mri_3d_view")
  })
  
  output$raster_panel_ans <- renderUI({
    callModule(raster3d_animation_Module, "mri_3d_ans", im = app_dt_ans)
    raster3d_animation_UI("mri_3d_ans")
  })
  
  observe({
    if (!is.null(input$question)) {
      question_choices <- questions_answers$answers[[which(questions_answers$question == input$question)]]
      updateSelectInput(session, "answer", choices = question_choices)
    }
  })
  
  output$saved_pairs <- renderDataTable({
    rv$df
  })
  
  observeEvent(input$save_pair, {
    if (!is.null(input$your_dt_ans) && !is.null(input$question) && !is.null(input$answer)) {
      image_id <- basename(input$your_dt_ans$name) # Update this line
      question <- input$question
      answer <- input$answer
      rv$df <- rbind(rv$df, data.frame(ImageID = image_id, 
                                       Question = question, 
                                       Answer = answer,
                                       stringsAsFactors = FALSE))
    }
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      "QA_data.csv"
    },
    content = function(file) {
      write.csv(rv$df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
