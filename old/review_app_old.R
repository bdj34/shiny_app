library(shiny)
library(shinyjs)

# EDIT these fields only:
reviewer <- "brian"
# Optionally change these:
optional_blacklisted_pids <- "" # Will also blacklist previously annotated within outdir
N_annotate <- 100
N_lines_before <- 20
N_lines_after <- 20

# No need to change these
annotation_type <- "crc"
rootDir <- "P:/ORD_Curtius_202210036D/chartReview_May2025"
rootDir <- "~/VA_IBD/"
outDir <- paste0(rootDir, "/output_csvs_", annotation_type, "_", reviewer)  # Change this to your desired folder
if (!dir.exists(outDir)) dir.create(outDir)

session_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_file <- file.path(outDir, paste0(annotation_type, "_", reviewer, "_reviewed_", session_stamp, ".csv"))

# REPLACE WITH YOUR SQL-LOADED DATAFRAME
patient_data <- data.frame(
  ID = c("123", "456", "789"),
  Text = c(
    "Patient has adenocarcinoma invading the submucosa. Tumor seen in rectum.",
    "No dysplasia seen. Inflammation only.",
    "Confirmed CRC with lymph node involvement and high grade dysplasia."
  ),
  stringsAsFactors = FALSE
)

# Filter for CRC-relevant cases
crc_regex <- "(?i)(crc|adenocarcinoma|invasi\\w*|tumor\\w*|cancer)"
#patient_data <- subset(patient_data, grepl(crc_regex, patient_data$Text, perl = TRUE))

annotations <- data.frame(
  ID = character(),
  Diagnosis = character(),
  DifficultCase = logical(),
  DxYear = character(),
  DxMonth = character(),
  T = character(),
  N = character(),
  M = character(),
  stringsAsFactors = FALSE
)


review_ids <- setdiff(patient_data$ID, annotations$ID)

ui <- fluidPage(
  titlePanel("CRC Chart Review"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      htmlOutput("patient_id"),
      radioButtons("diagnosis", "Diagnosis:", choices = c("HGD", "CRC", "Neither"), selected=character(0)),
      checkboxInput("difficult", "Difficult CRC diagnosis", FALSE),
      selectInput("dx_y", "Diagnosis Year", choices = c("Unknown", rev(format(seq.Date(as.Date("1920-01-01"), Sys.Date(), , by = "year"), "%Y")))),
      selectInput("dx_m", "Diagnosis Month", choices = c("Unknown", month.name)),
      selectInput("t_stage", "T Stage", choices = c("Unknown", "Tis/T0", "T1", "T1+", paste0("T", 2:4))),
      selectInput("n_stage", "N Stage", choices = c("Unknown", "N0", "N1", "N2")),
      selectInput("m_stage", "M Stage", choices = c("Unknown", "M0", "M1")),
      actionButton("back", "⬅ Previous"),
      actionButton("submit", "Submit & Next", class = "btn-primary", disabled = "disabled"),
      textOutput("remaining")
    ),

    mainPanel(
      tags$style(HTML(".scroll-box {
        height: 400px;
        overflow-y: scroll;
        background-color: #f9f9f9;
        border: 1px solid #ccc;
        padding: 10px;
        white-space: pre-wrap;
      }")),
      uiOutput("patient_text", container = div, class = "scroll-box")
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (!is.null(input$diagnosis) && input$diagnosis != "") {
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })

  current_index <- reactiveVal(1)

  reviewed_ids <- reactiveVal(annotations$ID)
  idx <- reactiveVal(1)

  get_remaining <- reactive({
    setdiff(patient_data$ID, reviewed_ids())
  })

  # current_id <- reactive({
  #   remaining <- get_remaining()
  #   if (length(remaining) >= idx()) {
  #     remaining[idx()]
  #   } else {
  #     NULL
  #   }
  # })

  current_id <- reactive({
    if (length(patient_data$ID) >= current_index()) {
      patient_data$ID[current_index()]
    } else {
      NULL
    }
  })


  output$patient_id <- renderUI({
    pid <- current_id()
    if (is.null(pid)) return(h4("All Done"))
    h4(paste("Patient ID:", pid))
  })

  output$remaining <- renderText({
    remaining <- get_remaining()
    sprintf("Remaining: %d of %d", length(remaining), nrow(patient_data))
  })

  output$patient_text <- renderUI({
    pid <- current_id()
    if (is.null(pid)) return("")
    text <- patient_data$Text[patient_data$ID == pid]
    highlighted <- gsub(
      "(?i)(crc|adenocarcinoma|invasi\\w*|tumor\\w*|cancer)",
      "<span style='color: red; font-weight: bold;'>\\1</span>",
      text,
      perl = TRUE
    )
    HTML(highlighted)
  })

  reset_inputs <- function() {
    updateRadioButtons(session, "diagnosis", selected = character(0))
    updateCheckboxInput(session, "difficult", value = FALSE)
    updateSelectInput(session, "dx_ym", selected = "Unknown")
    updateSelectInput(session, "t_stage", selected = "Unknown")
    updateSelectInput(session, "n_stage", selected = "Unknown")
    updateSelectInput(session, "m_stage", selected = "Unknown")
  }

  observeEvent(input$submit, {
    pid <- current_id()
    if (is.null(pid)) return()

    if (input$diagnosis == "") {
      showModal(modalDialog("❗ Please select a diagnosis.", easyClose = TRUE))
      return()
    }

    new_entry <- data.frame(
      ID = pid,
      Diagnosis = input$diagnosis,
      DifficultCase = input$difficult,
      DxYear = input$dx_y,
      DxMonth = input$dx_m,
      T = input$t_stage,
      N = input$n_stage,
      M = input$m_stage,
      stringsAsFactors = FALSE
    )

    # Replace or add row
    annotations <<- annotations[annotations$ID != pid, ]
    annotations <<- rbind(annotations, new_entry)
    write.csv(annotations, out_file, row.names = FALSE)

    if (current_index() < nrow(patient_data)) {
      current_index(current_index() + 1)
    }
    reset_inputs()
  })

  observeEvent(input$back, {
    if (current_index() > 1) {
      current_index(current_index() - 1)

      pid <- current_id()
      row <- annotations[annotations$ID == pid, ]
      if (nrow(row) == 1) {
        updateRadioButtons(session, "diagnosis", selected = row$Diagnosis)
        updateCheckboxInput(session, "difficult", value = row$DifficultCase)
        updateSelectInput(session, "dx_y", selected = row$DxYear)
        updateSelectInput(session, "dx_m", selected = row$DxMonth)
        updateSelectInput(session, "t_stage", selected = row$T)
        updateSelectInput(session, "n_stage", selected = row$N)
        updateSelectInput(session, "m_stage", selected = row$M)
      }
    }
  })


}

shinyApp(ui = ui, server = server)
