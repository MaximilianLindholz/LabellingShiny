library(shiny)
library(RNifti)
library(png)

ui <- fluidPage(
  titlePanel("Medical Image Labeling App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("nifti_file", "Upload NIfTI Image:", accept = c(".nii", ".nii.gz")),
      actionButton("load_image", "Load Image"),
      hr(),
      sliderInput("slice_slider", "Slice:", min = 1, max = 1, value = 1),
      sliderInput("brightness_slider", "Brightness:", min = -100, max = 100, value = 0),
      sliderInput("contrast_slider", "Contrast:", min = 0, max = 200, value = 100),
      hr(),
      uiOutput("question_output"),
      downloadButton("download_data", "Download Answers")
    ),
    mainPanel(
      imageOutput("image_plot")
    )
  )
)

server <- function(input, output, session) {
  loaded_image <- reactiveVal(NULL)
  labels <- reactiveValues()
  answers <- reactiveValues()
  
  questions_answers <- data.frame(
    question = c("Are there any T2 hyperintense white matter lesions periventricularly?", 
                 "Are there any juxtacortical/cortical T2 hyperintense white matter lesions?", 
                 "Are there any infratentorial T2 hyperintense white matter lesions?",
                 "Are there any T2 hyperintense white matter lesions on the optic nerve?",
                 "Are there any T2 hyperintense white matter lesions at the cervicomedullary junction?",
                 "What is the overall disease burden based on the number of lesions?",
                 "What is the extent of parenchymal atrophy?",
                 "What was the impression from the MRI Brain scan?",
                 "Is there a mass effect?",
                 "Is there any herniation?",
                 "Is there a venous thrombus?",
                 "Where is the sinus location?",
                 "Is there hydrocephalus?",
                 "Is there a microhemorrhage?",
                 "What is the microhemorrhage location?",
                 "Describe the brain parenchyma:",
                 "Hydrocephalus?"),
    answers = I(list(c('No', '1-2', '3 or more'), 
                     c('None', 'Present'),
                     c('None', 'Present'),
                     c('None', 'Present'),
                     c('None', 'Present'),
                     c('None', '<10 lesions', '10-20 lesions', '>20 lesions'),
                     c('None', 'Mild', 'Moderate', 'Severe'),
                     c('Normal MRI brain', 'Multiple white matter lesions consistent with demyelinating disease'),
                     c('yes', 'no', 'not applicable'),
                     c('no', 'subfalcine', 'transtentorial', 'not applicable'),
                     c('yes', 'no', 'not applicable'),
                     c('dural venous sinus', 'cortical vein', 'not applicable'),
                     c('yes', 'no', 'not applicable'),
                     c('absent', 'one', 'two to five', 'more than five', 'not applicable'),
                     c('absent', 'cortical', 'subcortical', 'not applicable'),
                     c('Signal intensities are within normal limits for age', 'Mild white matter chronic small vessel ischemic changes', 'Moderate white matter chronic small vessel ischemic changes', 'Severe white matter chronic small vessel ischemic changes'),
                     c('present', 'absent', 'not applicable')))
  )
  
  observeEvent(input$load_image, {
    req(input$nifti_file)
    nifti_path <- input$nifti_file$datapath
    image <- tryCatch(
      {
        readNifti(nifti_path)
      },
      error = function(e) {
        print(paste("Error reading NIfTI image:", e$message))
        return(NULL)
      }
    )
    if (is.null(image)) {
      loaded_image(NULL)
    } else {
      loaded_image(image)
      labels[[nifti_path]] <- NULL
      updateSliderInput(session, "slice_slider", max = dim(image)[3])
    }
  })
  
  output$question_output <- renderUI({
    question_index <- reactiveVal(1)
    questions <- questions_answers$question
    answers <- questions_answers$answers
    
    observeEvent(input$load_image, {
      question_index(1)
    })
    
    observeEvent(input$save_label, {
      if (question_index() < nrow(questions_answers)) {
        question_index(question_index() + 1)
      }
    })
    
    output_list <- lapply(1:nrow(questions_answers), function(i) {
      if (i == question_index()) {
        list(
          h3(questions[i]),
          selectInput(inputId = paste0("question_", i), 
                      label = NULL,
                      choices = answers[[i]]),
          actionButton(inputId = "save_label", label = "Next Question")
        )
      }
    })
    
    if (is.null(output_list)) {
      list()
    } else {
      output_list
    }
  })
  
  output$image_plot <- renderImage({
    image <- loaded_image()
    slice <- input$slice_slider
    brightness <- input$brightness_slider
    contrast <- input$contrast_slider
    if (!is.null(image)) {
      png_filename <- tempfile(fileext = ".png")
      png(png_filename, width = 800, height = 800)  # Adjust the width and height as needed
      adjusted_image <- (image[,,slice] + brightness) * (contrast / 100)
      adjusted_image <- pmax(adjusted_image, 0)
      adjusted_image <- pmin(adjusted_image, 255)
      image(adjusted_image, col = gray.colors(256))  # Use grayscale colors
      dev.off()
      list(src = png_filename, contentType = 'image/png', alt = "Image Slice")
    } else {
      list()
    }
  }, deleteFile = TRUE)
  
  observe({
    req(input$nifti_file)
    nifti_path <- input$nifti_file$datapath
    if (!is.null(labels[[nifti_path]])) {
      updateTextInput(session, "label_input", value = labels[[nifti_path]])
    } else {
      updateTextInput(session, "label_input", value = "")
    }
  })
  
  observeEvent(input$save_label, {
    req(input$nifti_file, input$label_input)
    nifti_path <- input$nifti_file$datapath
    label <- input$label_input
    labels[[nifti_path]] <- label
    for (i in 1:nrow(questions_answers)) {
      answers[[paste0(nifti_path, "_Q", i)]] <- input[[paste0("question_", i)]]
    }
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("answers-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(answers()), file, row.names = FALSE)
    }
  )
}

# Set the maximum request size option
options(shiny.maxRequestSize = 50*1024^2)  # 50MB

# Run the Shiny app
shinyApp(ui, server)
