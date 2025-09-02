# Load necessary libraries
library(shiny)
library(shinyjs)
library(purrr) # For map_dfr

# --- Configuration --- EDIT THESE FIELDS ---
reviewer <- "brian_uccare"
optional_blacklisted_pids <- ""

highlight_morphology_regex <- "(?i)(polypoid|flat|depressed|invisible|random|non-polypoid)"
highlight_lesion_type_regex <- "(?i)(adenocarcinoma|dysplasia|tubulovillous adenoma|villous adenoma|tubular adenoma|sessile serrated|ssl|ssa|tsa|hyperplastic|adenoma)"
highlight_dysplasia_grade_regex <- "(?i)(low grade dysplasia|high grade dysplasia|lgd|hgd|indefinite for dysplasia|no dysplasia)"
highlight_resection_regex <- "(?i)(complete resection|incomplete resection|resection margin|piecemeal|en bloc|R0|R1|R2)"
highlight_colitis_terms_regex <- "(?i)(colitis|proctitis|inflammation|cryptitis|crypt abscess)"
highlight_location_regex <- "(?i)(rectum|rectosigmoid|sigmoid|descending|splenic|transverse|hepatic|ascending|cecum|appendix|colon|ileum)"
highlight_size_regex <- "(?i)(\\d+(\\.\\d+)?\\s?(cm|mm))"
highlight_procedure_regex <- "(?i)(polypectomy|biopsy|EMR|ESD|colectomy)"
# --- End Configuration ---

# --- Setup ---
annotation_type <- "uccare_review"
rootDir <- "~/VA_IBD/"
outDir <- file.path(rootDir, paste0("output_csvs_", annotation_type, "_", reviewer))

if (!dir.exists(outDir)) {
  message("Creating output directory: ", outDir)
  tryCatch({ dir.create(outDir, recursive = TRUE); message("Output directory created successfully.") },
           error = function(e) { stop("Failed to create output directory: ", outDir, " Error: ", e$message) })
} else { message("Using existing output directory: ", outDir) }

session_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_file <- file.path(outDir, paste0(annotation_type, "_", reviewer, "_reviewed_", session_stamp, ".csv"))
message("Output file for this session: ", out_file)

# --- Data Loading ---
all_potential_patient_data <- data.frame(
  ID = c("PAT001", "PAT002", "PAT003", "PAT004", "PAT005", "PAT006", "PAT007", "PAT008"),
  ColoReportText = c(
    "Patient PAT001: Colonoscopy on January 10, 2023. Findings: Sigmoid colon shows a 1.5 cm polypoid lesion. Resection attempted. Background mucosa shows mild active colitis, proctitis only. Procedure: Polypectomy.",
    "Patient PAT002: Cecum biopsy performed 2022-05-18 during colonoscopy for evaluation of extensive chronic colitis. One 0.8 cm adenoma seen and biopsied. Another lesion of 12mm also noted.",
    "Patient PAT003: Flexible sigmoidoscopy to rectum on April 2024. Multiple small polyps found, not removed. No evidence of colitis. Some invisible changes noted, random biopsies taken.",
    "Patient PAT004: Colectomy performed July 1, 2021 for a large mass. Pre-op colonoscopy noted a 3.0 cm mass.",
    "Patient PAT005: EMR of a 2.5cm flat lesion in the ascending colon. Procedure successful.",
    "Patient PAT006: Surveillance colonoscopy. Random biopsies from transverse colon taken due to history of colitis.",
    "Patient PAT007: Sigmoidoscopy. One 0.5cm polypoid lesion, morphology described as unusual, biopsied. Another 1.2cm non-polypoid lesion, attempted EMR.",
    "Patient PAT008: Report states 'three polyps in transverse colon, all 0.5cm tubular adenomas, LGD, removed by snare polypectomy'."
  ),
  PathReportText = c(
    "PAT001 Pathology: Sigmoid polyp: tubular adenoma with low grade dysplasia. Size 1.5cm. Resection margins appear clear (R0). Rectal biopsies: mild active proctitis.",
    "PAT002 Pathology: Cecum biopsy: Tubular adenoma, 0.8 cm, with high grade dysplasia. Margins involved (R1). Background: extensive chronic colitis. Second lesion 12 mm, also adenoma.",
    "PAT003 Pathology: Rectal biopsies: Hyperplastic polyps. Random biopsies: non-specific mild inflammation.",
    "PAT004 Pathology: Colectomy specimen: Invasive adenocarcinoma, 3.0 cm, arising in a tubulovillous adenoma. Margins clear. T3N1M0. Background: Pancolitis with moderate activity.",
    "PAT005 Pathology: EMR specimen, ascending colon: Sessile serrated lesion with dysplasia. Measures 2.5 cm. Resection piecemeal, margins uncertain.",
    "PAT006 Pathology: Transverse colon biopsies show focal active colitis, mild. No dysplasia.",
    "PAT007 Pathology: Sigmoid polyp biopsy: high grade dysplasia. Ascending colon EMR: intramucosal adenocarcinoma, 1.2 cm.",
    "PAT008 Pathology: A) Transverse colon polyp: tubular adenoma, LGD. B) Transverse colon polyp: tubular adenoma, LGD. C) Transverse colon polyp: tubular adenoma, LGD. All measure approx 0.5 cm."
  ),
  stringsAsFactors = FALSE
)
all_potential_patient_data$ID <- as.character(all_potential_patient_data$ID)
message("Loaded ", nrow(all_potential_patient_data), " total potential patient records.")

# --- Identify Previously Reviewed IDs ---
existing_files <- list.files(path = outDir, pattern = paste0("^", annotation_type, "_", reviewer, "_reviewed_\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.csv$"), full.names = TRUE)
previously_reviewed_ids <- character(0)
if (length(existing_files) > 0) {
  all_prev_annotations <- purrr::map_dfr(existing_files, function(f) { tryCatch({ df <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE, fill = TRUE, header = TRUE); if ("ID" %in% names(df)) { valid_ids <- df$ID[!is.na(df$ID) & nzchar(df$ID)]; if(length(valid_ids) > 0) { data.frame(ID = valid_ids, stringsAsFactors = FALSE) } else { NULL } } else { NULL } }, error = function(e) { NULL }) })
  if (!is.null(all_prev_annotations) && nrow(all_prev_annotations) > 0 && "ID" %in% names(all_prev_annotations)) { previously_reviewed_ids <- unique(all_prev_annotations$ID) }
}
if (exists("optional_blacklisted_pids") && nzchar(optional_blacklisted_pids)) { manual_blacklist <- trimws(unlist(strsplit(optional_blacklisted_pids, ",|;|\n"))); manual_blacklist <- manual_blacklist[nzchar(manual_blacklist)]; if(length(manual_blacklist) > 0) { previously_reviewed_ids <- unique(c(previously_reviewed_ids, manual_blacklist)) } }
patient_data_for_session <- all_potential_patient_data[!all_potential_patient_data$ID %in% previously_reviewed_ids, ]
message("Filtered patient list for this session. ", nrow(patient_data_for_session), " patients remaining to be reviewed.")
if(nrow(patient_data_for_session) == 0) { message("WARNING: No unreviewed patients remaining for this session.") }

# Base schema
base_schema_cols <- list(
  ID = character(),  UCCARE = logical(),  Multifocal = logical(),
  Large = logical(), Incomplete = logical(), Invisible = logical(), EndoModSev = logical()
)
annotations_schema <- as.data.frame(base_schema_cols, stringsAsFactors = FALSE)


# --- UI Definition ---
ui <- fluidPage(
  titlePanel(paste("UC-CaRE Review:", annotation_type, "-", reviewer)),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(style="max-height: 85vh; overflow-y: auto;",
               h4(textOutput("patient_id_display")),
               textOutput("progress_display"),
               hr(),
               h4("UC-CaRE"),
               # *** FIX 1: Correctly defined checkboxInput ***
               checkboxInput("UCCARE", "Does this meet UC-CaRE inclusion criteria?", FALSE),
               h4("Multifocal"),
               checkboxInput("Multifocal", "Is there multifocal colitis-associated LGD?", FALSE),
               h4("Large"),
               checkboxInput("Large", "Is there a colitis-associated LGD >= 1cm?", FALSE),
               h4("Incomplete"),
               checkboxInput("Incomplete", "Are any LGD incompletely resected?", FALSE),
               h4("Invisible"),
               checkboxInput("Invisible", "Are any LGD invisible?", FALSE),
               h4("Moderate / severe inflammation"),
               checkboxInput("EndoModSev", "Is there moderate or severe endoscopic inflammation (current exam)?", FALSE),
               hr(),
               sliderInput("fontSize", "Text Font Size (px):", min = 10, max = 20, value = 14, step = 1),
               hr(),
               div(style="display: flex; justify-content: space-between;",
                   actionButton("back", "⬅ Previous"),
                   # *** FIX 2: Removed 'disabled' attribute from UI ***
                   actionButton("submit", "Submit & Next ➡", class = "btn-primary")
               ),
               br(), br()
      )
    ),
    mainPanel(
      width = 9,
      tags$style(HTML(paste0("
        .scroll-box { height: 85vh; overflow-y: scroll; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.25rem; padding: 15px; white-space: pre-wrap; font-family: monospace; line-height: 1.5; margin-bottom: 20px;}
        .highlight-morph { background-color: #E6E6FA; font-weight: bold;}
        .highlight-ltype { background-color: #FFDAB9; font-weight: bold;}
        .highlight-dysp { background-color: #dda0dd; font-weight: bold;}
        .highlight-resect { background-color: #b0e0e6; font-weight: bold;}
        .highlight-colitis { background-color: #98fb98; font-weight: bold;}
        .highlight-loc { background-color: #add8e6; font-weight: bold;}
        .highlight-size { background-color: #ffcccb; font-weight: bold;}
        .highlight-proc { background-color: #E0FFFF; font-weight: bold;}
      "))),
      fluidRow(
        column(width = 6, uiOutput("path_report_text_output", class = "scroll-box")),
        column(width = 6, uiOutput("colo_report_text_output", class = "scroll-box"))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {

  current_index <- reactiveVal(1)
  session_annotations <- reactiveVal(annotations_schema)
  patient_ids_this_session <- reactiveVal(as.character(patient_data_for_session$ID))

  # *** IMPROVEMENT: Helper function for highlighting to avoid duplication ***
  highlight_text <- function(text) {
    if (is.null(text) || !nzchar(text)) return("")
    text <- gsub(highlight_morphology_regex, "<span class='highlight-morph'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_lesion_type_regex, "<span class='highlight-ltype'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_dysplasia_grade_regex, "<span class='highlight-dysp'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_resection_regex, "<span class='highlight-resect'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_colitis_terms_regex, "<span class='highlight-colitis'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_location_regex, "<span class='highlight-loc'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_size_regex, "<span class='highlight-size'>\\1</span>", text, perl = TRUE)
    text <- gsub(highlight_procedure_regex, "<span class='highlight-proc'>\\1</span>", text, perl = TRUE)
    return(text)
  }

  reset_inputs <- function() {
    message("Resetting inputs")
    # *** FIX 1 (cont.): Use correct ID in reset function ***
    updateCheckboxInput(session, "UCCARE", value = FALSE)
    updateCheckboxInput(session, "Multifocal", value = FALSE)
    updateCheckboxInput(session, "Large", value = FALSE)
    updateCheckboxInput(session, "Incomplete", value = FALSE)
    updateCheckboxInput(session, "Invisible", value = FALSE)
    updateCheckboxInput(session, "EndoModSev", value = FALSE)
  }

  load_session_annotation_to_ui <- function(pid) {
    req(pid)
    message("Loading session annotation UI for ID: ", pid)
    annotations_df <- session_annotations()
    record <- annotations_df[as.character(annotations_df$ID) == as.character(pid), ]

    if (nrow(record) == 1) {
      message("Found existing annotation. Populating inputs.")
      # *** FIX 3: Removed incorrect logic referencing non-existent columns and UI elements ***
      # The remaining logic correctly updates the UI from the saved data.
      shinyjs::delay(50, {
        isolate({
          updateCheckboxInput(session, "UCCARE", value = as.logical(record$UCCARE %||% FALSE))
          updateCheckboxInput(session, "Multifocal", value = as.logical(record$Multifocal %||% FALSE))
          updateCheckboxInput(session, "Large", value = as.logical(record$Large %||% FALSE))
          updateCheckboxInput(session, "Incomplete", value = as.logical(record$Incomplete %||% FALSE))
          updateCheckboxInput(session, "Invisible", value = as.logical(record$Invisible %||% FALSE))
          updateCheckboxInput(session, "EndoModSev", value = as.logical(record$EndoModSev %||% FALSE))
        })
      })
    } else {
      message("No existing annotation found. Resetting inputs.")
      reset_inputs()
    }
  }

  update_ui_for_index <- function(index) {
    session_ids <- patient_ids_this_session()
    total_session_patients <- length(session_ids)

    if(total_session_patients == 0) {
      reset_inputs()
      output$patient_id_display <- renderText("No patients to review")
      output$progress_display <- renderText("0 / 0")
      output$path_report_text_output <- renderUI(HTML("All potential patients have been reviewed."))
      output$colo_report_text_output <- renderUI(HTML(""))
      shinyjs::disable("back")
      shinyjs::disable("submit")
      return()
    }

    req(index > 0 && index <= total_session_patients)
    pid <- session_ids[index]
    req(pid)
    message("Updating UI for session index: ", index, ", ID: ", pid)

    output$patient_id_display <- renderText({ paste("Patient ID:", pid) })
    output$progress_display <- renderText({
      reviewed_count <- nrow(session_annotations())
      sprintf("Session Progress: %d / %d", reviewed_count, total_session_patients)
    })

    current_patient_row <- all_potential_patient_data[as.character(all_potential_patient_data$ID) == as.character(pid), ]

    output$path_report_text_output <- renderUI({
      if(nrow(current_patient_row) == 1 && "PathReportText" %in% names(current_patient_row)) {
        text <- current_patient_row$PathReportText %||% "Pathology report not available."
        HTML(paste0("<h5>Pathology Report</h5><hr>", highlight_text(text)))
      } else {
        HTML("<h5>Pathology Report</h5><p>Error: Could not find Path Report for Patient ID ", pid, "</p>")
      }
    })

    output$colo_report_text_output <- renderUI({
      if(nrow(current_patient_row) == 1 && "ColoReportText" %in% names(current_patient_row)) {
        text <- current_patient_row$ColoReportText %||% "Colonoscopy report not available."
        HTML(paste0("<h5>Colonoscopy Report(s)</h5><hr>", highlight_text(text)))
      } else {
        HTML("<h5>Colonoscopy Report(s)</h5><p>Error: Could not find Colonoscopy Report for Patient ID ", pid, "</p>")
      }
    })

    load_session_annotation_to_ui(pid)

    # Correctly enable/disable navigation buttons
    shinyjs::toggleState("back", condition = index > 1)
    shinyjs::toggleState("submit", condition = index <= total_session_patients)
  }

  observeEvent(input$fontSize, {
    shinyjs::runjs(sprintf("document.querySelectorAll('.scroll-box').forEach(el => el.style.fontSize = '%dpx');", input$fontSize))
  })

  observeEvent(input$submit, {
    message("Submit button clicked.")
    isolate({
      pid <- patient_ids_this_session()[current_index()]

      # *** FIX 1 (cont.): input$UCCARE now reads from the corrected checkbox ***
      new_entry_list <- list(
        ID = as.character(pid),
        UCCARE = input$UCCARE,
        Multifocal = input$Multifocal,
        Large = input$Large,
        Incomplete = input$Incomplete,
        Invisible = input$Invisible,
        EndoModSev = input$EndoModSev
      )

      new_entry <- as.data.frame(new_entry_list, stringsAsFactors = FALSE)
      message("Created new entry for ID: ", pid)

      anno_df <- session_annotations()
      anno_df <- anno_df[as.character(anno_df$ID) != as.character(pid), ] # Remove old entry if it exists
      anno_df <- rbind(anno_df, new_entry)
      session_annotations(anno_df)

      message("Session annotations updated. Total this session: ", nrow(anno_df))
      tryCatch({
        write.csv(session_annotations(), out_file, row.names = FALSE, quote = TRUE)
        message("Successfully wrote to: ", out_file)
      }, error = function(e) {
        warning("FAILED to write: ", e$message)
        showModal(modalDialog(title = "Error Saving", paste("Could not write:", out_file, e$message)))
      })

      if (current_index() < length(patient_ids_this_session())) {
        current_index(current_index() + 1)
        update_ui_for_index(current_index())
      } else {
        output$patient_id_display <- renderText("All Done!")
        output$progress_display <- renderText(sprintf("Completed: %d / %d", length(patient_ids_this_session()), length(patient_ids_this_session())))
        output$path_report_text_output <- renderUI(HTML("Session complete. You may close this window."))
        output$colo_report_text_output <- renderUI(HTML(""))
        reset_inputs()
        shinyjs::disable("submit")
        showModal(modalDialog(title = "Session Complete", "All patients reviewed.", easyClose = TRUE))
      }
    })
  })

  observeEvent(input$back, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
      update_ui_for_index(current_index())
    }
  })

  # Initial UI Load
  observeEvent(session, {
    message("Initial UI load.")
    # *** FIX 4: Removed hide calls for non-existent UI elements ***
    update_ui_for_index(current_index())
  }, once = TRUE)
}

`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) { b } else { a } }

shinyApp(ui = ui, server = server)
