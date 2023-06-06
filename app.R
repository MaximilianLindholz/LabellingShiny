library(shiny)
library(RNifti)
library(shinythemes)
library(shinyWidgets)
library(rsconnect)
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
  question = c(
    "Is there any abnormal signal intensity in the brain parenchyma?",
    "Are there any focal lesions present?",
    "Is there any evidence of brain atrophy?",
    "Are there any abnormalities in the ventricles?",
    "Is there any abnormal enhancement after contrast administration?",
    "Are there any abnormalities in the skull or calvarium?",
    "Is there any abnormal signal intensity in the meninges?",
    "Are there any abnormalities in the cranial nerves?",
    "Is there any evidence of vascular abnormalities or malformations?",
    "Are there any abnormalities in the pituitary gland?"
  ),
  answers = I(list(
    c('No', 'Yes'),
    c('None', 'Present'),
    c('None', 'Mild', 'Moderate', 'Severe'),
    c('No', 'Yes'),
    c('No', 'Minimal/mild', 'Marked/avid', 'Not applicable'),
    c('No', 'Yes'),
    c('No', 'Present'),
    c('No', 'Yes'),
    c('No', 'Yes'),
    c('No', 'Yes')
  ))
)



ui <- navbarPage(
  "MRI Labeller",
  theme = shinytheme("cyborg"),
  tabPanel(
    "Answer Mode",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(12, tags$h4("How to:", style = "color: white; margin-top: 30px;")),
          column(12, tags$div("The labelling is quite easy, upload an nifti image via the browse button. Then choose a question and answer and click on save. Congrats, now you have done your first labelling. Check the results tab to see the labellings you have done so far. Once you are done with your session you can download the annotations via the download button.                                                                          ", 
                              style = "color: white; margin-top: 20px;")),
          tags$br(),
          tags$br(),
          column(12, fileInput("your_dt_ans", "Upload a .nii .nii.gz")),
          column(12, selectInput("question", "Select a question", 
                                 choices = questions_answers$question)),
          column(12, selectInput("answer", "Select your answer", 
                                 choices = NULL)),
          column(12, actionButton("save_pair", "Save Question-Answer Pair")),
          column(12, downloadButton("download_data", "Download Q&A Data"))
        )
      ),
      mainPanel(
        uiOutput("raster_panel_ans")
      )
    )
  ),
  tabPanel(
    "Results",
    fluidRow(
      column(12, dataTableOutput("saved_pairs"))
    )
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
