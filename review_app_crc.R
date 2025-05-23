# Load necessary libraries
library(shiny)
library(shinyjs)
library(purrr) # Added for map_dfr

# --- Configuration --- EDIT THESE FIELDS ---
reviewer <- "brian"
# Optionally change these:
optional_blacklisted_pids <- ""#head(readLines(), 100) # Can be used to manually blacklist additional IDs if needed
N_annotate <- 100 # Not currently used, represents a potential target number
N_lines_before <- 20 # Not currently used in text display logic
N_lines_after <- 20 # Not currently used in text display logic

# ** Optional: Modify highlighting regular expressions **
# Case-insensitive by default due to (?i) prefix
highlight_crc_regex <- "(?i)(crc|adenocarcinoma|invasi\\w*|tumor\\w*|cancer|malignan\\w*|dysplasia)"
highlight_loc_regex <- "(?i)(rectum|rectosigmoid|sigmoid|descending|splenic|transverse|hepatic|ascending|cecum|appendix)"
# ** Added Date Regex **
highlight_date_regex <- "(?i)\\b(19\\d{2}|20\\d{2}|January|February|March|April|May|June|July|August|September|October|November|December)\\b"
# --- End Configuration ---


# --- Setup --- No need to change these usually ---
annotation_type <- "crc"
# IMPORTANT: Adjust rootDir to your actual project location
# rootDir <- "P:/ORD_Curtius_202210036D/chartReview_May2025" # Example Windows path
rootDir <- "~/VA_IBD/" # Example Linux/macOS path - Using ~ might be unreliable in non-interactive sessions, prefer absolute paths if issues arise.
outDir <- file.path(rootDir, paste0("output_csvs_", annotation_type, "_", reviewer))

# Create output directory if it doesn't exist
if (!dir.exists(outDir)) {
  message("Creating output directory: ", outDir)
  tryCatch({
    dir.create(outDir, recursive = TRUE)
    message("Output directory created successfully.")
  }, error = function(e) {
    stop("Failed to create output directory: ", outDir, " Error: ", e$message) # Stop if directory can't be created
  })
} else {
  message("Using existing output directory: ", outDir)
}

# Define the output file path (timestamped for each session)
session_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_file <- file.path(outDir, paste0(annotation_type, "_", reviewer, "_reviewed_", session_stamp, ".csv"))
message("Output file for this session: ", out_file)

# --- Data Loading ---
# !!! REPLACE THIS with your actual data loading logic (e.g., from SQL or a file) !!!
all_potential_patient_data <- data.frame(
  ID = c("123", "456", "789", "101", "112", "205", "315"), # Added more examples
  Text = c(
    "Patient ID 123: Biopsy confirmed adenocarcinoma invading the submucosa on June 1, 2022. Primary tumor located in the rectum.", # Assume 123 was reviewed
    "Patient ID 456: Colonoscopy showed inflammation consistent with IBD. No dysplasia or malignancy identified.", # Assume 456 was reviewed
    "Patient ID 789: Pathology report from 2024 confirms CRC stage T3N1M0. High grade dysplasia noted in surrounding tissue. No mention of cancer elsewhere. Location: Descending colon.", # Unreviewed
    "Patient ID 101: History of diverticulosis. Screening colonoscopy negative for polyps or tumor.", # Unreviewed
    "Patient ID 112: Invasive adenocarcinoma found in the sigmoid colon. Patient referred to oncology. Metastasis suspected. Dx November 2021.", # Assume 112 was reviewed
    "Patient ID 205: New patient. Colonoscopy pending. Family history positive for CRC. Lesion seen in transverse colon on imaging.", # Unreviewed
    "Patient ID 315: Follow-up shows resolution of previous polyps. No malignancy found." # Unreviewed
  ),
  stringsAsFactors = FALSE
)
all_potential_patient_data$ID <- as.character(all_potential_patient_data$ID)
message("Loaded ", nrow(all_potential_patient_data), " total potential patient records.")

# --- Identify Previously Reviewed IDs ---
existing_files <- list.files(
  path = outDir,
  pattern = paste0("^", annotation_type, "_", reviewer, "_reviewed_\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.csv$"),
  full.names = TRUE
)
previously_reviewed_ids <- character(0)
if (length(existing_files) > 0) {
  message("Found ", length(existing_files), " previous annotation files. Reading IDs...")
  all_prev_annotations <- purrr::map_dfr(existing_files, function(f) {
    tryCatch({
      df <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
      if ("ID" %in% names(df)) {
        valid_ids <- df$ID[!is.na(df$ID) & nzchar(df$ID)]
        if(length(valid_ids) > 0) { data.frame(ID = valid_ids, stringsAsFactors = FALSE) } else { NULL }
      } else { warning("File ", f, " does not contain an 'ID' column."); NULL }
    }, error = function(e) { warning("Could not read or parse file: ", f, ". Error: ", e$message); NULL })
  })
  if (!is.null(all_prev_annotations) && nrow(all_prev_annotations) > 0 && "ID" %in% names(all_prev_annotations)) {
    previously_reviewed_ids <- unique(all_prev_annotations$ID)
    message("Identified ", length(previously_reviewed_ids), " unique previously reviewed patient IDs.")
  } else { message("No valid IDs found in previous annotation files or files could not be read.") }
} else { message("No previous annotation files found for this reviewer in ", outDir) }
if (exists("optional_blacklisted_pids") && nzchar(optional_blacklisted_pids)) {
  manual_blacklist <- trimws(unlist(strsplit(optional_blacklisted_pids, ",|;|\n")))
  manual_blacklist <- manual_blacklist[nzchar(manual_blacklist)]
  if(length(manual_blacklist) > 0) {
    message("Adding ", length(manual_blacklist), " manually blacklisted IDs.")
    previously_reviewed_ids <- unique(c(previously_reviewed_ids, manual_blacklist))
  }
}

# --- Filter Patient Data for Current Session ---
patient_data_for_session <- all_potential_patient_data[!all_potential_patient_data$ID %in% previously_reviewed_ids, ]
message("Filtered patient list for this session. ", nrow(patient_data_for_session), " patients remaining to be reviewed.")
if(nrow(patient_data_for_session) == 0) { message("WARNING: No unreviewed patients remaining for this session.") }


# Define the structure for the annotations data frame
annotations_schema <- data.frame(
  ID = character(),
  Diagnosis = character(),
  DifficultCase = logical(),
  Location = character(),
  DxYear = character(),
  DifficultDxYear = logical(),
  DxMonth = character(),
  T = character(),
  N = character(),
  M = character(),
  stringsAsFactors = FALSE
)

# --- UI Definition ---
year_choices <- c("Select..." = "", "Unknown", rev(format(seq.Date(as.Date("1920-01-01"), Sys.Date(), by = "year"), "%Y")))
month_choices <- c("Select..." = "", "Unknown", month.name)
t_stage_choices <- c("Select..." = "", "Unknown", "Tis/T0", "T1", "T1+", paste0("T", 2:4))
n_stage_choices <- c("Select..." = "", "Unknown", "N0", "N1", "N2")
m_stage_choices <- c("Select..." = "", "Unknown", "M0", "M1")
location_choices <- c("Select..." = "", "Unknown", "Rectum", "Rectosigmoid", "Sigmoid", "Descending Colon", "Splenic Flexure", "Transverse Colon", "Hepatic Flexure", "Ascending Colon", "Cecum", "Appendix", "Multiple Locations", "Other/Not Specified")

ui <- fluidPage(
  titlePanel(paste("CRC Chart Review:", annotation_type, "-", reviewer)),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4(textOutput("patient_id_display")),
      textOutput("progress_display"),
      hr(),
      radioButtons("diagnosis", "Diagnosis:", choices = c("HGD", "Adenocarcinoma", "Neither"), selected = character(0)),
      h5("Difficulty"),
      checkboxInput("difficult", "Difficult diagnosis?", FALSE),

      # Div for fields shown for HGD or Adenocarcinoma
      div(id = "hgd_crc_fields", style = "display: none;",
          selectInput("location", "Anatomical Location:", choices = location_choices, selected = ""),
          selectInput("dx_y", "Diagnosis Year:", choices = year_choices, selected = ""),
          checkboxInput("difficult_year", "Difficult Dx Year?", FALSE),
          # Wrap Dx Month in its own conditional div
          div(id = "dx_month_div", style = "display: none;",
              selectInput("dx_m", "Diagnosis Month:", choices = month_choices, selected = "")
          )
      ),
      # Div for fields shown only for Adenocarcinoma
      div(id = "crc_only_fields", style = "display: none;",
          selectInput("t_stage", "T Stage:", choices = t_stage_choices, selected = ""),
          selectInput("n_stage", "N Stage:", choices = n_stage_choices, selected = ""),
          selectInput("m_stage", "M Stage:", choices = m_stage_choices, selected = "")
      ),

      hr(),
      sliderInput("fontSize", "Text Font Size (px):", min = 10, max = 18, value = 14, step = 1),
      hr(),
      div(style="display: flex; justify-content: space-between;",
          actionButton("back", "⬅ Previous"),
          actionButton("submit", "Submit & Next ➡", class = "btn-primary", disabled = "disabled")
      ),
      br(), br()
    ),
    mainPanel(
      width = 9,
      tags$style(HTML("
        .scroll-box { height: 75vh; overflow-y: scroll; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.25rem; padding: 15px; white-space: pre-wrap; font-family: monospace; /* font-size controlled by JS */ line-height: 1.5; }
        .highlight-crc { background-color: #ffff99; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
        .highlight-loc { background-color: #add8e6; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
        .highlight-date { background-color: #ffcccb; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
      ")),
      uiOutput("patient_text_display", container = div, class = "scroll-box")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {

  current_index <- reactiveVal(1)
  session_annotations <- reactiveVal(annotations_schema)
  patient_ids_this_session <- reactiveVal(as.character(patient_data_for_session$ID))

  observeEvent(session, {
    initial_font_size <- 14
    shinyjs::runjs(sprintf("document.querySelector('.scroll-box').style.fontSize = '%dpx';", initial_font_size))
  }, once = TRUE)

  reset_inputs <- function() {
    message("Resetting inputs")
    updateRadioButtons(session, "diagnosis", selected = character(0))
    updateCheckboxInput(session, "difficult", value = FALSE)
    updateSelectInput(session, "location", selected = "")
    updateSelectInput(session, "dx_y", selected = "")
    updateCheckboxInput(session, "difficult_year", value = FALSE)
    updateSelectInput(session, "dx_m", selected = "")
    updateSelectInput(session, "t_stage", selected = "")
    updateSelectInput(session, "n_stage", selected = "")
    updateSelectInput(session, "m_stage", selected = "")
    shinyjs::hide("hgd_crc_fields")
    shinyjs::hide("dx_month_div")
    shinyjs::hide("crc_only_fields")
    shinyjs::disable("submit")
  }

  load_session_annotation_to_ui <- function(pid) {
    req(pid)
    message("Loading session annotation UI for ID: ", pid)
    annotations_df <- session_annotations()
    record <- annotations_df[as.character(annotations_df$ID) == as.character(pid), ]

    if (nrow(record) == 1) {
      message("Found existing annotation from this session. Populating inputs.")
      diagnosis_value <- record$Diagnosis %||% ""
      dx_year_value <- record$DxYear %||% ""
      show_hgd_crc <- diagnosis_value %in% c("HGD", "Adenocarcinoma")
      show_crc_only <- diagnosis_value == "Adenocarcinoma"
      show_dx_month <- show_hgd_crc && dx_year_value != "" && dx_year_value != "Unknown"

      shinyjs::toggle(id = "hgd_crc_fields", condition = show_hgd_crc)
      shinyjs::toggle(id = "crc_only_fields", condition = show_crc_only)
      shinyjs::toggle(id = "dx_month_div", condition = show_dx_month)

      isolate({
        updateRadioButtons(session, "diagnosis", selected = diagnosis_value)
        updateCheckboxInput(session, "difficult", value = as.logical(record$DifficultCase %||% FALSE))
        updateSelectInput(session, "location", selected = record$Location %||% "")
        updateSelectInput(session, "dx_y", selected = dx_year_value)
        updateCheckboxInput(session, "difficult_year", value = as.logical(record$DifficultDxYear %||% FALSE))
        updateSelectInput(session, "dx_m", selected = record$DxMonth %||% "")
        updateSelectInput(session, "t_stage", selected = record$T %||% "")
        updateSelectInput(session, "n_stage", selected = record$N %||% "")
        updateSelectInput(session, "m_stage", selected = record$M %||% "")
      })
      # Trigger validity check AFTER values and visibility are set
      # Use a slight delay to ensure inputs have updated in the browser session? No, check_all_fields should work.
      check_all_fields_validity()

    } else {
      message("No existing annotation found from this session. Resetting inputs.")
      reset_inputs()
    }
  }

  # --- Conditional UI Logic ---
  toggle_conditional_crc_fields <- function(diagnosis_value, dx_year_value) {
    show_hgd_crc <- !is.null(diagnosis_value) && diagnosis_value %in% c("HGD", "Adenocarcinoma")
    shinyjs::toggle(id = "hgd_crc_fields", condition = show_hgd_crc)
    show_crc_only <- !is.null(diagnosis_value) && diagnosis_value == "Adenocarcinoma"
    shinyjs::toggle(id = "crc_only_fields", condition = show_crc_only)
    show_dx_month <- show_hgd_crc && !is.null(dx_year_value) && dx_year_value != "" && dx_year_value != "Unknown"
    shinyjs::toggle(id = "dx_month_div", condition = show_dx_month)

    isolate({
      if (!show_hgd_crc) {
        updateSelectInput(session, "location", selected = ""); updateSelectInput(session, "dx_y", selected = ""); updateCheckboxInput(session, "difficult_year", value = FALSE); updateSelectInput(session, "dx_m", selected = "")
      }
      if (!show_dx_month && show_hgd_crc) { updateSelectInput(session, "dx_m", selected = "") }
      if (!show_crc_only) { updateSelectInput(session, "t_stage", selected = ""); updateSelectInput(session, "n_stage", selected = ""); updateSelectInput(session, "m_stage", selected = "") }
    })
  }

  observeEvent(input$diagnosis, {
    req(input$diagnosis)
    isolate({dx_y_val <- input$dx_y})
    toggle_conditional_crc_fields(input$diagnosis, dx_y_val)
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$dx_y, {
    isolate({diag_val <- input$diagnosis})
    if (!is.null(diag_val) && diag_val %in% c("HGD", "Adenocarcinoma")) {
      show_dx_month <- !is.null(input$dx_y) && input$dx_y != "" && input$dx_y != "Unknown"
      shinyjs::toggle(id = "dx_month_div", condition = show_dx_month)
      if(!show_dx_month) { isolate(updateSelectInput(session, "dx_m", selected = "")) }
    } else {
      shinyjs::hide("dx_month_div"); isolate(updateSelectInput(session, "dx_m", selected = ""))
    }
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Add observers for other required inputs to trigger validity check
  observeEvent(input$location, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dx_m, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$t_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$n_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$m_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # --- Input Validation Logic ---
  # ** Updated validity check - removed req() **
  all_fields_valid <- reactive({
    diag_selection <- input$diagnosis
    year_selection <- input$dx_y

    if (is.null(diag_selection) || !nzchar(diag_selection)) { return(FALSE) }
    if (diag_selection == "Neither") { return(TRUE) }

    # Check fields required for HGD and Adenocarcinoma
    # Ensure inputs exist before checking their values if they *should* be visible
    if (diag_selection %in% c("HGD", "Adenocarcinoma")) {
      # Check if inputs are non-NULL (they should be if visible) before checking != ""
      loc_valid <- !is.null(input$location) && input$location != ""
      year_valid <- !is.null(year_selection) && year_selection != ""
      month_required <- year_valid && year_selection != "Unknown"
      # Month is valid if not required OR if required and selected
      month_valid <- !month_required || (!is.null(input$dx_m) && input$dx_m != "")
    } else {
      # Should not happen if diagnosis is HGD/Adeno, but set defaults
      loc_valid <- FALSE
      year_valid <- FALSE
      month_valid <- FALSE
    }


    if (diag_selection == "HGD") {
      hgd_valid <- loc_valid && year_valid && month_valid
      # message("Checking field validity for HGD: ", hgd_valid, "...") # Reduced logging
      return(hgd_valid)
    }

    if (diag_selection == "Adenocarcinoma") {
      # Check TNM validity
      t_valid <- !is.null(input$t_stage) && input$t_stage != ""
      n_valid <- !is.null(input$n_stage) && input$n_stage != ""
      m_valid <- !is.null(input$m_stage) && input$m_stage != ""
      crc_valid <- loc_valid && year_valid && month_valid && t_valid && n_valid && m_valid
      # message("Checking field validity for Adenocarcinoma: ", crc_valid, "...") # Reduced logging
      return(crc_valid)
    }

    # message("Checking field validity: FALSE (Unknown state)") # Reduced logging
    return(FALSE) # Default
  })

  # Observer to enable/disable submit based on validity
  observe({
    is_valid <- all_fields_valid()
    if (is_valid) {
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })
  check_all_fields_validity <- function() { all_fields_valid() } # Keep helper

  # Update UI for index
  update_ui_for_index <- function(index) {
    session_ids <- patient_ids_this_session()
    total_session_patients <- length(session_ids)

    if(total_session_patients == 0) {
      message("No patients to review in this session.")
      output$patient_id_display <- renderText({ "No patients to review" })
      output$progress_display <- renderText({ "Progress: 0 / 0" })
      output$patient_text_display <- renderUI({ HTML("All potential patients have been previously reviewed.") })
      reset_inputs(); shinyjs::disable("back"); shinyjs::disable("submit")
      return()
    }

    req(index > 0 && index <= total_session_patients)
    pid <- session_ids[index]
    req(pid)
    message("Updating UI for session index: ", index, ", ID: ", pid)

    output$patient_id_display <- renderText({ paste("Patient ID:", pid) })
    output$progress_display <- renderText({
      reviewed_in_session_count <- length(intersect(session_ids, unique(as.character(session_annotations()$ID))))
      sprintf("Session Progress: %d / %d (Viewing record %d)", reviewed_in_session_count, total_session_patients, index)
    })

    # Text Highlighting Logic
    output$patient_text_display <- renderUI({
      text_row <- all_potential_patient_data[as.character(all_potential_patient_data$ID) == as.character(pid), ]
      if(nrow(text_row) == 1) {
        text <- text_row$Text
        highlighted_text <- text
        highlighted_text <- gsub(highlight_crc_regex, "<span class='highlight-crc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_loc_regex, "<span class='highlight-loc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_date_regex, "<span class='highlight-date'>\\1</span>", highlighted_text, perl = TRUE)
        HTML(highlighted_text)
      } else { HTML(paste("Error: Could not find text for Patient ID", pid)) }
    })

    load_session_annotation_to_ui(pid) # Handles conditional fields and validity

    if (index == 1) { shinyjs::disable("back") } else { shinyjs::enable("back") }
    if (index > total_session_patients) { shinyjs::disable("submit") }
  }

  # Observer for Font Size Slider
  observeEvent(input$fontSize, {
    new_size <- input$fontSize
    message("Font size changed to: ", new_size, "px")
    shinyjs::runjs(sprintf("document.querySelector('.scroll-box').style.fontSize = '%dpx';", new_size))
  })

  # Submit button observer
  observeEvent(input$submit, {
    message("Submit button clicked.")
    # Use isolate() to read inputs safely within observer
    isolate({
      diag_selection <- input$diagnosis
      year_selection <- input$dx_y
      month_selection <- input$dx_m

      # Re-run validation check before proceeding
      if (!all_fields_valid()) {
        error_message <- "❗ Please ensure all required fields have a valid selection before submitting."
        # Generate more specific error message based on current state
        if (is.null(diag_selection) || !nzchar(diag_selection)) { error_message <- "❗ Please select a Diagnosis."}
        else if (diag_selection %in% c("HGD", "Adenocarcinoma")) {
          if (is.null(input$location) || input$location == "") { error_message <- "❗ Anatomical Location is required."}
          else if (is.null(year_selection) || year_selection == "") { error_message <- "❗ Diagnosis Year is required."}
          else if (year_selection != "Unknown" && (is.null(month_selection) || month_selection == "")) { error_message <- "❗ Diagnosis Month is required when Diagnosis Year is not 'Unknown'."}
          else if (diag_selection == "Adenocarcinoma") {
            if (is.null(input$t_stage) || input$t_stage == "" || is.null(input$n_stage) || input$n_stage == "" || is.null(input$m_stage) || input$m_stage == "") { error_message <- "❗ T, N, and M Stages are required for Adenocarcinoma."}
          }
        }
        showModal(modalDialog(title = "Missing Information", error_message, easyClose = TRUE, footer = modalButton("OK")))
        message("Submission failed: Validation check failed inside submit observer.")
        return() # Stop submission
      }

      session_ids <- patient_ids_this_session(); total_session_patients <- length(session_ids); current_idx <- current_index()
      if (total_session_patients == 0 || current_idx > total_session_patients) { message("Submit clicked when no patients or past the end."); return() }
      pid <- session_ids[current_idx]; message("Processing submission for session index: ", current_idx, ", ID: ", pid)

      new_entry <- data.frame(
        ID = as.character(pid), Diagnosis = diag_selection, DifficultCase = input$difficult,
        Location = ifelse(diag_selection %in% c("HGD", "Adenocarcinoma"), input$location, ""),
        DxYear = ifelse(diag_selection %in% c("HGD", "Adenocarcinoma"), year_selection, ""),
        DifficultDxYear = ifelse(diag_selection %in% c("HGD", "Adenocarcinoma"), input$difficult_year, FALSE),
        DxMonth = ifelse(diag_selection %in% c("HGD", "Adenocarcinoma") && year_selection != "Unknown", month_selection, ""),
        T = ifelse(diag_selection == "Adenocarcinoma", input$t_stage, ""),
        N = ifelse(diag_selection == "Adenocarcinoma", input$n_stage, ""),
        M = ifelse(diag_selection == "Adenocarcinoma", input$m_stage, ""),
        stringsAsFactors = FALSE )
      message("Created new entry for ID: ", pid)

      anno_df <- session_annotations(); anno_df <- anno_df[as.character(anno_df$ID) != as.character(pid), ]; anno_df <- rbind(anno_df, new_entry); session_annotations(anno_df)
      message("Session annotations updated. Total this session: ", nrow(anno_df))
      tryCatch({ write.csv(session_annotations(), out_file, row.names = FALSE, quote = TRUE); message("Successfully wrote session annotations to: ", out_file) },
               error = function(e) { warning("!!! FAILED to write annotations to CSV: ", e$message); showModal(modalDialog(title = "Error Saving Data", paste("Could not write annotations to file:", out_file, "Error:", e$message), footer = modalButton("OK"))) })

      if (current_idx < total_session_patients) {
        next_idx <- current_idx + 1; message("Moving to next session index: ", next_idx); current_index(next_idx); update_ui_for_index(next_idx)
      } else {
        message("Reached the end of the session list."); output$patient_id_display <- renderText({ "All Done for this session!" }); output$progress_display <- renderText({ sprintf("Session Completed: %d / %d", total_session_patients, total_session_patients) })
        output$patient_text_display <- renderUI({ "" }); reset_inputs(); shinyjs::disable("submit")
        showModal(modalDialog(title = "Session Complete", "You have reviewed all available patients for this session.", easyClose = TRUE, footer = modalButton("OK")))
      }
    })
  })

  # Back button observer
  observeEvent(input$back, {
    message("Back button clicked."); current_idx <- current_index()
    if (current_idx > 1) { prev_idx <- current_idx - 1; message("Moving to previous session index: ", prev_idx); current_index(prev_idx); update_ui_for_index(prev_idx)
    } else { message("Already at the first record for this session.") }
  })

  # Initial UI Load
  observeEvent(session, {
    message("Initial UI load observer triggered.");
    shinyjs::hide("hgd_crc_fields"); shinyjs::hide("dx_month_div"); shinyjs::hide("crc_only_fields")
    initial_index <- current_index();
    message("Performing initial UI update for session index: ", initial_index);
    update_ui_for_index(initial_index)
  }, once = TRUE) # Runs after the font size observer

}

# Helper function for safe defaulting
`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) { b } else { a } }

shinyApp(ui = ui, server = server)
