# Load necessary libraries
library(shiny)
library(shinyjs)
library(purrr) # For map_dfr

# --- Configuration --- EDIT THESE FIELDS ---
reviewer <- "brian_ibd" # Use a distinct reviewer name if needed, or keep the same
# Optionally change these:
optional_blacklisted_pids <- "" # Manually blacklist additional IDs if needed

# ** Optional: Modify highlighting regular expressions **
# Case-insensitive by default due to (?i) prefix
highlight_ibd_regex <- "(?i)(crohn'?s|ulcerative colitis|uc|ibd|colitis|proctitis|ileitis|dysplasia|inflammation)"
highlight_loc_regex <- "(?i)(rectum|rectosigmoid|sigmoid|descending|splenic flexure|transverse|hepatic flexure|ascending|cecum|appendix|colon|colonic|ileum|ileal)"
highlight_proc_regex <- "(?i)(colonoscopy|colectomy|sigmoidoscopy|endoscopy|scope|biopsies|biopsy)"
highlight_date_regex <- "(?i)\\b(19\\d{2}|20\\d{2}|January|February|March|April|May|June|July|August|September|October|November|December)\\b"
# --- End Configuration ---

# --- Setup --- No need to change these usually ---
annotation_type <- "ibd"
# IMPORTANT: Adjust rootDir to your actual project location
# rootDir <- "P:/ORD_Curtius_202210036D/chartReview_May2025" # Example Windows path
rootDir <- "~/" # Example Linux/macOS path
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
# !!! REPLACE THIS with your actual data loading logic (e.g., from SQL or a file) !!!
all_potential_patient_data <- data.frame(
  ID = c("123", "456", "789", "101", "112", "205", "315", "400", "501"), # Example IDs
  Text = c(
    "Patient ID 123: Colonoscopy on January 15, 2023 showed continuous inflammation from rectum to splenic flexure, consistent with ulcerative colitis. Biopsies confirm.", # Assume 123 reviewed
    "Patient ID 456: History of Crohn's disease since 2010, currently asymptomatic. Last colonoscopy showed mild ileitis.", # Assume 456 reviewed
    "Patient ID 789: Presented with abdominal pain and diarrhea in March 2024. CT showed terminal ileum thickening. Colonoscopy revealed skip lesions and cobblestoning. Dx: Crohn's colitis.", # Unreviewed
    "Patient ID 101: Referred for positive fecal immunochemical test. Colonoscopy normal.", # Unreviewed
    "Patient ID 112: Long history of IBD-U, managed with mesalamine. Recent flare after colectomy in 1999.", # Assume 112 reviewed
    "Patient ID 205: Patient reports bloody diarrhea. Scope limited to sigmoid due to poor prep, showed proctitis. Flexible sigmoidoscopy recommended for August.", # Unreviewed
    "Patient ID 315: Known Crohn's disease involving the small bowel only since May 2015. No colonic involvement seen on scope.", # Unreviewed
    "Patient ID 400: Endoscopy report mentions 'colitis, type unspecified'. Biopsy results pending. Seen October 2022.", # Unreviewed
    "Patient ID 501: Iron deficiency anemia workup. Colonoscopy showed diverticulosis only." # Unreviewed
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
  message("Found ", length(existing_files), " previous IBD annotation files. Reading IDs...")
  all_prev_annotations <- purrr::map_dfr(existing_files, function(f) {
    tryCatch({
      df <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
      if ("ID" %in% names(df)) {
        valid_ids <- df$ID[!is.na(df$ID) & nzchar(df$ID)]; if(length(valid_ids) > 0) { data.frame(ID = valid_ids, stringsAsFactors = FALSE) } else { NULL }
      } else { warning("File ", f, " does not contain an 'ID' column."); NULL }
    }, error = function(e) { warning("Could not read or parse file: ", f, ". Error: ", e$message); NULL })
  })
  if (!is.null(all_prev_annotations) && nrow(all_prev_annotations) > 0 && "ID" %in% names(all_prev_annotations)) {
    previously_reviewed_ids <- unique(all_prev_annotations$ID); message("Identified ", length(previously_reviewed_ids), " unique previously reviewed patient IDs.")
  } else { message("No valid IDs found in previous annotation files or files could not be read.") }
} else { message("No previous IBD annotation files found for this reviewer in ", outDir) }
if (exists("optional_blacklisted_pids") && nzchar(optional_blacklisted_pids)) {
  manual_blacklist <- trimws(unlist(strsplit(optional_blacklisted_pids, ",|;|\n"))); manual_blacklist <- manual_blacklist[nzchar(manual_blacklist)]
  if(length(manual_blacklist) > 0) { message("Adding ", length(manual_blacklist), " manually blacklisted IDs."); previously_reviewed_ids <- unique(c(previously_reviewed_ids, manual_blacklist)) }
}

# --- Filter Patient Data for Current Session ---
patient_data_for_session <- all_potential_patient_data[!all_potential_patient_data$ID %in% previously_reviewed_ids, ]
message("Filtered patient list for this session. ", nrow(patient_data_for_session), " patients remaining to be reviewed.")
if(nrow(patient_data_for_session) == 0) { message("WARNING: No unreviewed patients remaining for this session.") }


# Define the structure for the IBD annotations data frame
annotations_schema <- data.frame(
  ID = character(),
  Diagnosis = character(),
  DifficultCase = logical(),
  DxYear = character(),
  DifficultDate = logical(),
  PathConfirmed = logical(),
  stringsAsFactors = FALSE
)

# --- UI Definition ---
diagnosis_choices <- c("Crohn's colitis", "UC", "IBD-colitis (unspecified)", "Ulcerative proctitis", "Crohn's, unknown colitis", "Crohn's without colitis", "None of the above", "Unknown")
year_choices <- c("Select..." = "", "Unknown", rev(format(seq.Date(as.Date("1930-01-01"), Sys.Date(), by = "year"), "%Y")))

ui <- fluidPage(
  titlePanel(paste("IBD Chart Review:", annotation_type, "-", reviewer)),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4(textOutput("patient_id_display")),
      textOutput("progress_display"),
      hr(),
      radioButtons("diagnosis", "Diagnosis:", choices = diagnosis_choices, selected = character(0)),
      h5("Difficulty"),
      checkboxInput("difficult", "Difficult diagnosis?", FALSE),
      # Dx Year section (conditionally shown)
      div(id = "dx_year_section", style = "display: none;",
          selectInput("dx_y", "Diagnosis Year:", choices = year_choices, selected = ""),
          # Difficult Date checkbox (conditionally shown *within* year section)
          div(id = "difficult_date_div", style = "display: none;",
              checkboxInput("difficult_date", "Difficult date?", FALSE)
          )
      ),
      # Path Confirmed section (conditionally shown)
      div(id = "path_confirmed_div", style = "display: none;",
          checkboxInput("path_confirmed", "Pathology confirmed?", FALSE)
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
        .highlight-ibd { background-color: #ffff99; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
        .highlight-loc { background-color: #add8e6; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
        .highlight-proc { background-color: #90ee90; font-weight: bold; padding: 0.1em 0.2em; border-radius: 0.2em; }
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
    updateSelectInput(session, "dx_y", selected = "")
    updateCheckboxInput(session, "difficult_date", value = FALSE)
    updateCheckboxInput(session, "path_confirmed", value = FALSE)
    shinyjs::hide("dx_year_section")
    shinyjs::hide("difficult_date_div")
    shinyjs::hide("path_confirmed_div")
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
      show_dx_year_section <- !is.null(diagnosis_value) && diagnosis_value != "" && !diagnosis_value %in% c("None of the above")
      show_difficult_date <- show_dx_year_section && !is.null(dx_year_value) && dx_year_value != "" && dx_year_value != "Unknown"
      show_path_confirmed <- !is.null(diagnosis_value) && diagnosis_value != "" && !diagnosis_value %in% c("None of the above", "Unknown")

      shinyjs::toggle(id = "dx_year_section", condition = show_dx_year_section)
      shinyjs::toggle(id = "difficult_date_div", condition = show_difficult_date)
      shinyjs::toggle(id = "path_confirmed_div", condition = show_path_confirmed)

      isolate({
        updateRadioButtons(session, "diagnosis", selected = diagnosis_value)
        updateCheckboxInput(session, "difficult", value = as.logical(record$DifficultCase %||% FALSE))
        updateSelectInput(session, "dx_y", selected = dx_year_value)
        updateCheckboxInput(session, "difficult_date", value = as.logical(record$DifficultDate %||% FALSE))
        updateCheckboxInput(session, "path_confirmed", value = as.logical(record$PathConfirmed %||% FALSE))
      })
      # Trigger validity check AFTER values and visibility are set
      # Use req(input$diagnosis) to ensure it exists before checking validity
      req(input$diagnosis)
      check_all_fields_validity()

    } else {
      message("No existing annotation found from this session. Resetting inputs.")
      reset_inputs()
    }
  }

  # --- Conditional UI Logic ---
  toggle_conditional_fields <- function(diagnosis_value, dx_year_value) {
    show_dx_year_section <- !is.null(diagnosis_value) && diagnosis_value != "" && !diagnosis_value %in% c("None of the above")
    shinyjs::toggle(id = "dx_year_section", condition = show_dx_year_section)

    show_difficult_date <- show_dx_year_section && !is.null(dx_year_value) && dx_year_value != "" && dx_year_value != "Unknown"
    shinyjs::toggle(id = "difficult_date_div", condition = show_difficult_date)

    show_path_confirmed <- !is.null(diagnosis_value) && diagnosis_value != "" && !diagnosis_value %in% c("None of the above", "Unknown")
    shinyjs::toggle(id = "path_confirmed_div", condition = show_path_confirmed)

    isolate({
      if (!show_dx_year_section) {
        updateSelectInput(session, "dx_y", selected = "")
        updateCheckboxInput(session, "difficult_date", value = FALSE)
      }
      if (!show_difficult_date && show_dx_year_section) {
        updateCheckboxInput(session, "difficult_date", value = FALSE)
      }
      if (!show_path_confirmed) {
        updateCheckboxInput(session, "path_confirmed", value = FALSE)
      }
    })
  }

  observeEvent(input$diagnosis, {
    isolate({ dx_y_val <- input$dx_y })
    toggle_conditional_fields(input$diagnosis, dx_y_val)
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$dx_y, {
    isolate({ diag_val <- input$diagnosis })
    if (!is.null(diag_val) && diag_val != "" && !diag_val %in% c("None of the above")) {
      show_difficult_date <- !is.null(input$dx_y) && input$dx_y != "" && input$dx_y != "Unknown"
      shinyjs::toggle(id = "difficult_date_div", condition = show_difficult_date)
      if (!show_difficult_date) { isolate(updateCheckboxInput(session, "difficult_date", value = FALSE)) }
    } else {
      shinyjs::hide("difficult_date_div"); isolate(updateCheckboxInput(session, "difficult_date", value = FALSE))
    }
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # --- Input Validation Logic ---
  # ** Updated validity check based on new rules and removed req() **
  all_fields_valid <- reactive({
    diag_selection <- input$diagnosis
    year_selection <- input$dx_y

    # 1. Diagnosis must be selected
    if (is.null(diag_selection) || !nzchar(diag_selection)) {
      message("Checking field validity: FALSE (Diagnosis not selected)"); return(FALSE)
    }

    # 2. Check if year is required for the selected diagnosis
    # Year is NOT required for "None of the above", "Crohn's without colitis", "Unknown"
    year_required <- !diag_selection %in% c("None of the above", "Crohn's without colitis", "Unknown")

    if (year_required) {
      # 3. If year is required, check if it has a valid selection ("" is invalid)
      # Check for non-NULL first, as input might be NULL briefly when hidden/shown
      valid <- !is.null(year_selection) && year_selection != ""
      message("Checking field validity: ", valid, " (Diagnosis '", diag_selection, "' requires year selection: ", valid, ")")
      return(valid)
    } else {
      # 4. If year is not required, the form is valid based on diagnosis selection alone
      message("Checking field validity: TRUE (Diagnosis is '", diag_selection, "', year not required)")
      return(TRUE)
    }
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
        highlighted_text <- gsub(highlight_ibd_regex, "<span class='highlight-ibd'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_loc_regex, "<span class='highlight-loc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_proc_regex, "<span class='highlight-proc'>\\1</span>", highlighted_text, perl = TRUE)
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
    isolate({
      diag_selection <- input$diagnosis
      year_selection <- input$dx_y

      # Re-run validation check before proceeding
      if (!all_fields_valid()) {
        error_message <- "❗ Please make a selection for Diagnosis."
        # More specific error message based on current state
        year_required <- !is.null(diag_selection) && diag_selection != "" &&
          !diag_selection %in% c("None of the above", "Crohn's without colitis", "Unknown")
        if (year_required && (is.null(year_selection) || year_selection == "")) {
          error_message <- "❗ Diagnosis Year is required for the selected Diagnosis." }
        showModal(modalDialog(title = "Missing Information", error_message, easyClose = TRUE, footer = modalButton("OK")))
        message("Submission failed: Validation check failed inside submit observer."); return()
      }

      session_ids <- patient_ids_this_session(); total_session_patients <- length(session_ids); current_idx <- current_index()
      if (total_session_patients == 0 || current_idx > total_session_patients) { message("Submit clicked when no patients or past the end."); return() }
      pid <- session_ids[current_idx]; message("Processing submission for session index: ", current_idx, ", ID: ", pid)

      new_entry <- data.frame(
        ID = as.character(pid), Diagnosis = diag_selection, DifficultCase = input$difficult,
        DxYear = ifelse(!diag_selection %in% c("None of the above"), year_selection, ""),
        DifficultDate = ifelse(!diag_selection %in% c("None of the above") && !is.null(year_selection) && year_selection != "" && year_selection != "Unknown", input$difficult_date, FALSE),
        PathConfirmed = ifelse(!diag_selection %in% c("None of the above", "Unknown"), input$path_confirmed, FALSE),
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
    shinyjs::hide("dx_year_section"); shinyjs::hide("difficult_date_div"); shinyjs::hide("path_confirmed_div")
    initial_index <- current_index();
    message("Performing initial UI update for session index: ", initial_index);
    update_ui_for_index(initial_index)
  }, once = TRUE) # Runs after the font size observer

}

# Helper function for safe defaulting
`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) { b } else { a } }

shinyApp(ui = ui, server = server)
