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
    "Whats the imaging modality?",
    "Whats the dominant finding?",
    "Are there any focal lesions present?",
    "Is there any evidence of brain atrophy?",
    "Are there any abnormalities in the ventricles?",
    "Are there any T2 hyperintense white matter lesions periventricularly?", 
    "Are there any juxtacortical/cortical T2 hyperintense white matter lesions?", 
    "Are there any infratentorial T2 hyperintense white matter lesions?",
    "Describe the brain parenchyma:",
    "Is there any infartction?",
    "Is there any herniation?",
    "Is there any mass effect?",
    "Is there any edema?",
    "Is there any tumor or tumor residual (e.g. postoperative)?",
    "Where is the largest component of the tumor?"
  ),
  answers = I(list(
    c('MRI T1', 'MRI T2', 'Flair', 'CT'),
    c("Normal Brain", "Multiple Sclerosis/Inflammatory", "Tumor/Post-Operative","Tumor/Pre-Operative", "Stroke/Haemorrhage"),
    c('None', 'Present'),
    c('None', 'Mild', 'Moderate', 'Severe'),
    c('None', 'Present'),
    c('No', '1-2', '3 or more'), 
    c('None', 'Present'),
    c('None', 'Present'),
    c('Signal intensities are within normal limits for age', 'Mild white matter chronic small vessel ischemic changes', 'Moderate white matter chronic small vessel ischemic changes', 'Severe white matter chronic small vessel ischemic changes'),
    c('None', 'Present'),
    c('None', 'Present'),
    c('None', 'Present'),
    c('None', 'Present'),
    c('None', 'Present'),
    c('There is no tumor', 'Right', 'Center/bilateral', 'Left', 'No Tumor present')
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
