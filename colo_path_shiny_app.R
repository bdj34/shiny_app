# Load necessary libraries
library(shiny)
library(shinyjs)
library(purrr) # For map_dfr

# --- Configuration --- EDIT THESE FIELDS ---
reviewer <- "brian_path_colo"
optional_blacklisted_pids <- ""

highlight_morphology_regex <- "(?i)(polypoid|flat|depressed|invisible|random|non-polypoid)"
highlight_lesion_type_regex <- "(?i)(adenoma|serrated|ssl|ssa|tsa|hyperplastic|adenocarcinoma|dysplasia|tubular|villous|tubulovillous)"
highlight_dysplasia_grade_regex <- "(?i)(low grade dysplasia|high grade dysplasia|lgd|hgd|indefinite for dysplasia|no dysplasia)"
highlight_resection_regex <- "(?i)(complete resection|incomplete resection|resection margin|piecemeal|en bloc)" # R0-R2 removed from here as they are in choices
highlight_colitis_terms_regex <- "(?i)(colitis|proctitis|inflammation|cryptitis|crypt abscess)"
highlight_location_regex <- "(?i)(rectum|rectosigmoid|sigmoid|descending|splenic|transverse|hepatic|ascending|cecum|appendix|colon|ileum)"
highlight_date_regex <- "(?i)\\b(19\\d{2}|20\\d{2}|January|February|March|April|May|June|July|August|September|October|November|December)\\b"
highlight_procedure_regex <- "(?i)(polypectomy|biopsy|EMR|ESD|colectomy)"
# --- End Configuration ---

# --- Setup ---
annotation_type <- "path_colo_review"
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
  ID = c("PAT001", "PAT002", "PAT003", "PAT004", "PAT005", "PAT006"),
  Text = c(
    "Patient PAT001: Colonoscopy on January 10, 2023. Findings: Sigmoid colon shows a 1.5 cm polypoid adenoma with low grade dysplasia. Resection appears complete. Background mucosa shows mild active colitis, proctitis only. Procedure: Polypectomy.",
    "Patient PAT002: Pathology report from 2022-05-20. Specimen: Cecum biopsy. Diagnosis: Tubular adenoma, 0.8 cm, with high grade dysplasia. Margins involved. Extensive chronic colitis noted.",
    "Patient PAT003: Report date: April 2024. Flexible sigmoidoscopy to rectum. Multiple small hyperplastic polyps found, not removed. No evidence of colitis. No definite lesion concerning for malignancy. Some invisible changes noted, random biopsies taken.",
    "Patient PAT004: Colectomy specimen from July 1, 2021. Invasive adenocarcinoma, 3.0 cm, arising in a tubulovillous adenoma. Margins clear. Pancolitis with moderate activity. T3N1M0.",
    "Patient PAT005: EMR of a 2.5cm flat lesion in the ascending colon. Pathology: Sessile serrated lesion with dysplasia. Resection piecemeal, margins uncertain.",
    "Patient PAT006: Random biopsies from transverse colon show focal active colitis, mild. No polyps seen."
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

# ** UPDATED annotations_schema for colitis extent **
annotations_schema <- data.frame(
  ID = character(),
  DifficultCase = logical(),
  ProcedureType = character(),
  LesionPresent = character(),
  LesionType = character(),
  Morphology = character(),
  DysplasiaGrade = character(),
  SizeCM = character(),
  LocationLesion = character(),
  ResectionCompleteness = character(),
  LesionTstage = character(),
  LesionNstage = character(),
  LesionMstage = character(),
  ColitisPresent = character(),
  ColitisRectum = logical(),      # New
  ColitisSigmoid = logical(),     # New
  ColitisDescending = logical(),  # New
  ColitisTransverse = logical(),  # New
  ColitisAscendingCecum = logical(),# New
  ColitisPancolitis = logical(),  # New
  ColitisSeverity = character(),
  stringsAsFactors = FALSE
)

# --- UI Definition ---
# ** UPDATED Choices **
lesion_type_choices <- c("Select..." = "", "Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Hyperplastic Polyp", "Adenocarcinoma", "Dysplasia", "Other", "Not Applicable", "Unknown")
morphology_choices <- c("Select..." = "", "Polypoid", "Non-polypoid (Flat, Depressed)", "Invisible/Random Biopsies", "Unknown")
dysplasia_grade_choices <- c("Select..." = "", "Low-grade Dysplasia (LGD)", "High-grade Dysplasia (HGD)", "Indefinite for Dysplasia", "No Dysplasia", "Not Applicable", "Unknown")
location_choices <- c("Select..." = "", "Unknown", "Rectum", "Rectosigmoid", "Sigmoid Colon", "Descending Colon", "Splenic Flexure", "Transverse Colon", "Hepatic Flexure", "Ascending Colon", "Cecum", "Ileocecal Valve", "Terminal Ileum", "Appendix", "Multiple Locations", "Diffuse", "Other") # Removed "Not Specified"
resection_choices <- c("Select..." = "", "Complete", "Incomplete - Microscopic", "Incomplete - Macroscopic", "Piecemeal", "Uncertain", "Not Resected", "Not Applicable")
colitis_severity_choices <- c("Select..." = "", "Quiescent / Inactive", "Mild", "Moderate", "Severe", "Unknown") # Removed "Not Applicable"
size_cm_values <- c(0, round(seq(0.1, 10, by = 0.1), 1))
size_cm_choices <- c("Select..." = "", "Unknown", "0 cm (Biopsy only?)" = "0", setNames(as.character(size_cm_values[size_cm_values > 0]), paste0(sprintf("%.1f", size_cm_values[size_cm_values > 0]), " cm")), "10+ cm" = "10+") # Removed "Not Specified"
procedure_choices <- c("Select..." = "", "Polypectomy/Biopsy", "EMR (Endoscopic Mucosal Resection)", "ESD (Endoscopic Submucosal Dissection)", "Colectomy") # Removed Other/Unknown
t_stage_choices <- c("Select..." = "", "Unknown", "Tis", "T1", "T2", "T3", "T4a", "T4b", "TX", "Not Applicable")
n_stage_choices <- c("Select..." = "", "Unknown", "N0", "N1", "N1a", "N1b", "N1c", "N2", "N2a", "N2b", "NX", "Not Applicable")
m_stage_choices <- c("Select..." = "", "Unknown", "M0", "M1", "M1a", "M1b", "M1c", "MX", "Not Applicable")


ui <- fluidPage(
  titlePanel(paste("Path/Colo Report Review:", annotation_type, "-", reviewer)),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(textOutput("patient_id_display")),
      textOutput("progress_display"),
      hr(),
      h5("General Information"),
      checkboxInput("difficult_case", "Difficult Case (Overall)?", FALSE),
      selectInput("procedure_type", "Procedure Type:", choices = procedure_choices, selected = ""),
      hr(),
      h5("Lesion Findings"),
      radioButtons("lesion_present", "Lesions of interest present?", choices = c("Yes", "No", "Uncertain"), selected = character(0), inline = TRUE),
      div(id = "lesion_details_div", style = "display: none;",
          selectInput("lesion_type", "Primary Lesion Type:", choices = lesion_type_choices, selected = ""),
          selectInput("morphology", "Morphology:", choices = morphology_choices, selected = ""),
          div(id = "dysplasia_grade_div", style = "display:none;",
              selectInput("dysplasia_grade", "Dysplasia Grade:", choices = dysplasia_grade_choices, selected = "")
          ),
          selectInput("size_cm", "Size (cm, largest lesion):", choices = size_cm_choices, selected = ""),
          selectInput("location_lesion", "Lesion Location:", choices = location_choices, selected = ""),
          selectInput("resection_completeness", "Completeness of Resection:", choices = resection_choices, selected = ""),
          div(id = "tnm_staging_div", style = "display: none;",
              h6("Adenocarcinoma Staging (if applicable)"),
              selectInput("lesion_t_stage", "T Stage:", choices = t_stage_choices, selected = ""),
              selectInput("lesion_n_stage", "N Stage:", choices = n_stage_choices, selected = ""),
              selectInput("lesion_m_stage", "M Stage:", choices = m_stage_choices, selected = "")
          )
      ),
      hr(),
      h5("Colitis Findings"),
      radioButtons("colitis_present", "Colitis Present?", choices = c("Yes", "No", "Uncertain"), selected = character(0), inline = TRUE),
      div(id = "colitis_details_div", style = "display: none;",
          h6("Colitis Extent:"),
          checkboxInput("colitis_rectum", "Rectum", FALSE),
          checkboxInput("colitis_sigmoid", "Sigmoid", FALSE),
          checkboxInput("colitis_descending", "Descending Colon", FALSE),
          checkboxInput("colitis_transverse", "Transverse Colon", FALSE),
          checkboxInput("colitis_ascending_cecum", "Ascending Colon/Cecum", FALSE),
          checkboxInput("colitis_pancolitis", "Pancolitis (All of the above)", FALSE),
          selectInput("colitis_severity", "Colitis Severity:", choices = colitis_severity_choices, selected = "")
      ),
      hr(),
      sliderInput("fontSize", "Text Font Size (px):", min = 10, max = 20, value = 14, step = 1),
      hr(),
      div(style="display: flex; justify-content: space-between;",
          actionButton("back", "⬅ Previous"),
          actionButton("submit", "Submit & Next ➡", class = "btn-primary", disabled = "disabled")
      ),
      br(), br()
    ),
    mainPanel(
      width = 8,
      tags$style(HTML("
        .scroll-box { height: 80vh; overflow-y: scroll; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.25rem; padding: 15px; white-space: pre-wrap; font-family: monospace; line-height: 1.5; }
        .highlight-morph { background-color: #E6E6FA; font-weight: bold;}
        .highlight-ltype { background-color: #FFDAB9; font-weight: bold;}
        .highlight-dysp { background-color: #dda0dd; font-weight: bold;}
        .highlight-resect { background-color: #b0e0e6; font-weight: bold;}
        .highlight-colitis { background-color: #98fb98; font-weight: bold;}
        .highlight-loc { background-color: #add8e6; font-weight: bold;}
        .highlight-date { background-color: #ffcccb; font-weight: bold;}
        .highlight-proc { background-color: #E0FFFF; font-weight: bold;}
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
    updateCheckboxInput(session, "difficult_case", value = FALSE)
    updateSelectInput(session, "procedure_type", selected = "")
    updateRadioButtons(session, "lesion_present", selected = character(0))
    updateSelectInput(session, "lesion_type", selected = "")
    updateSelectInput(session, "morphology", selected = "")
    updateSelectInput(session, "dysplasia_grade", selected = "")
    updateSelectInput(session, "size_cm", selected = "")
    updateSelectInput(session, "location_lesion", selected = "")
    updateSelectInput(session, "resection_completeness", selected = "")
    updateSelectInput(session, "lesion_t_stage", selected = "")
    updateSelectInput(session, "lesion_n_stage", selected = "")
    updateSelectInput(session, "lesion_m_stage", selected = "")
    updateRadioButtons(session, "colitis_present", selected = character(0))
    # Reset colitis extent checkboxes
    updateCheckboxInput(session, "colitis_rectum", value = FALSE)
    updateCheckboxInput(session, "colitis_sigmoid", value = FALSE)
    updateCheckboxInput(session, "colitis_descending", value = FALSE)
    updateCheckboxInput(session, "colitis_transverse", value = FALSE)
    updateCheckboxInput(session, "colitis_ascending_cecum", value = FALSE)
    updateCheckboxInput(session, "colitis_pancolitis", value = FALSE)
    shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum") # Ensure enabled
    updateSelectInput(session, "colitis_severity", selected = "")

    shinyjs::hide("lesion_details_div"); shinyjs::hide("dysplasia_grade_div"); shinyjs::hide("tnm_staging_div"); shinyjs::hide("colitis_details_div")
    shinyjs::disable("submit")
  }

  load_session_annotation_to_ui <- function(pid) {
    req(pid)
    message("Loading session annotation UI for ID: ", pid)
    annotations_df <- session_annotations()
    record <- annotations_df[as.character(annotations_df$ID) == as.character(pid), ]

    if (nrow(record) == 1) {
      message("Found existing annotation from this session. Populating inputs.")
      lesion_present_val <- record$LesionPresent %||% ""
      lesion_type_val <- record$LesionType %||% ""
      colitis_present_val <- record$ColitisPresent %||% ""

      show_lesion_details <- lesion_present_val == "Yes"
      show_dysplasia <- show_lesion_details && lesion_type_val %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia")
      show_tnm <- show_lesion_details && lesion_type_val == "Adenocarcinoma"
      show_colitis_details <- colitis_present_val == "Yes"

      shinyjs::toggle(id = "lesion_details_div", condition = show_lesion_details)
      shinyjs::toggle(id = "dysplasia_grade_div", condition = show_dysplasia)
      shinyjs::toggle(id = "tnm_staging_div", condition = show_tnm)
      shinyjs::toggle(id = "colitis_details_div", condition = show_colitis_details)

      isolate({
        updateCheckboxInput(session, "difficult_case", value = as.logical(record$DifficultCase %||% FALSE))
        updateSelectInput(session, "procedure_type", selected = record$ProcedureType %||% "")
        updateRadioButtons(session, "lesion_present", selected = lesion_present_val)
        updateSelectInput(session, "lesion_type", selected = lesion_type_val)
        updateSelectInput(session, "morphology", selected = record$Morphology %||% "")
        updateSelectInput(session, "dysplasia_grade", selected = record$DysplasiaGrade %||% "")
        updateSelectInput(session, "size_cm", selected = record$SizeCM %||% "")
        updateSelectInput(session, "location_lesion", selected = record$LocationLesion %||% "")
        updateSelectInput(session, "resection_completeness", selected = record$ResectionCompleteness %||% "")
        updateSelectInput(session, "lesion_t_stage", selected = record$LesionTstage %||% "")
        updateSelectInput(session, "lesion_n_stage", selected = record$LesionNstage %||% "")
        updateSelectInput(session, "lesion_m_stage", selected = record$LesionMstage %||% "")
        updateRadioButtons(session, "colitis_present", selected = colitis_present_val)
        # Load colitis extent checkboxes
        updateCheckboxInput(session, "colitis_rectum", value = as.logical(record$ColitisRectum %||% FALSE))
        updateCheckboxInput(session, "colitis_sigmoid", value = as.logical(record$ColitisSigmoid %||% FALSE))
        updateCheckboxInput(session, "colitis_descending", value = as.logical(record$ColitisDescending %||% FALSE))
        updateCheckboxInput(session, "colitis_transverse", value = as.logical(record$ColitisTransverse %||% FALSE))
        updateCheckboxInput(session, "colitis_ascending_cecum", value = as.logical(record$ColitisAscendingCecum %||% FALSE))
        updateCheckboxInput(session, "colitis_pancolitis", value = as.logical(record$ColitisPancolitis %||% FALSE))
        updateSelectInput(session, "colitis_severity", selected = record$ColitisSeverity %||% "")
      })
      # Manually trigger pancolitis observer if it was loaded as TRUE to disable others
      if(as.logical(record$ColitisPancolitis %||% FALSE)){
        shinyjs::runjs("Shiny.setInputValue('colitis_pancolitis', true, {priority: 'event'});") # Trigger observer
      }
      check_all_fields_validity()
    } else {
      message("No existing annotation found. Resetting inputs.")
      reset_inputs()
    }
  }

  # --- Conditional UI Logic ---
  toggle_conditional_fields_path_colo <- function() {
    lesion_present_val <- input$lesion_present
    lesion_type_val <- input$lesion_type
    colitis_present_val <- input$colitis_present

    show_lesion_details <- !is.null(lesion_present_val) && lesion_present_val == "Yes"
    shinyjs::toggle(id = "lesion_details_div", condition = show_lesion_details)

    show_dysplasia <- show_lesion_details && !is.null(lesion_type_val) &&
      lesion_type_val %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia")
    shinyjs::toggle(id = "dysplasia_grade_div", condition = show_dysplasia)

    show_tnm <- show_lesion_details && !is.null(lesion_type_val) && lesion_type_val == "Adenocarcinoma"
    shinyjs::toggle(id = "tnm_staging_div", condition = show_tnm)

    show_colitis_details <- !is.null(colitis_present_val) && colitis_present_val == "Yes"
    shinyjs::toggle(id = "colitis_details_div", condition = show_colitis_details)

    isolate({
      if (!show_lesion_details) {
        updateSelectInput(session, "lesion_type", selected = ""); updateSelectInput(session, "morphology", selected = "")
        updateSelectInput(session, "dysplasia_grade", selected = ""); updateSelectInput(session, "size_cm", selected = "")
        updateSelectInput(session, "location_lesion", selected = ""); updateSelectInput(session, "resection_completeness", selected = "")
        updateSelectInput(session, "lesion_t_stage", selected = ""); updateSelectInput(session, "lesion_n_stage", selected = ""); updateSelectInput(session, "lesion_m_stage", selected = "")
      }
      if (!show_dysplasia && show_lesion_details) { updateSelectInput(session, "dysplasia_grade", selected = "") }
      if (!show_tnm && show_lesion_details) { updateSelectInput(session, "lesion_t_stage", selected = ""); updateSelectInput(session, "lesion_n_stage", selected = ""); updateSelectInput(session, "lesion_m_stage", selected = "")}
      if (!show_colitis_details) {
        updateCheckboxInput(session, "colitis_rectum", value = FALSE); updateCheckboxInput(session, "colitis_sigmoid", value = FALSE)
        updateCheckboxInput(session, "colitis_descending", value = FALSE); updateCheckboxInput(session, "colitis_transverse", value = FALSE)
        updateCheckboxInput(session, "colitis_ascending_cecum", value = FALSE); updateCheckboxInput(session, "colitis_pancolitis", value = FALSE)
        shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum")
        updateSelectInput(session, "colitis_severity", selected = "")
      }
    })
  }

  # Observers for primary radio buttons and lesion type
  observeEvent(input$lesion_present, { toggle_conditional_fields_path_colo(); check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$lesion_type, { toggle_conditional_fields_path_colo(); check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_present, { toggle_conditional_fields_path_colo(); check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Observer for Pancolitis checkbox
  observeEvent(input$colitis_pancolitis, {
    if (input$colitis_pancolitis) {
      updateCheckboxInput(session, "colitis_rectum", value = TRUE); shinyjs::disable("colitis_rectum")
      updateCheckboxInput(session, "colitis_sigmoid", value = TRUE); shinyjs::disable("colitis_sigmoid")
      updateCheckboxInput(session, "colitis_descending", value = TRUE); shinyjs::disable("colitis_descending")
      updateCheckboxInput(session, "colitis_transverse", value = TRUE); shinyjs::disable("colitis_transverse")
      updateCheckboxInput(session, "colitis_ascending_cecum", value = TRUE); shinyjs::disable("colitis_ascending_cecum")
    } else {
      # Only re-enable if not also individually checked - user might uncheck pancolitis to specify segments
      # This part might need refinement based on desired interaction if pancolitis is unchecked.
      # For now, just enable them. User can then uncheck.
      shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum")
    }
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Observers for individual colitis extent checkboxes to trigger validity
  observeEvent(input$colitis_rectum, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_sigmoid, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_descending, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_transverse, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_ascending_cecum, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # Observers for all other fields that might affect validity
  observeEvent(input$procedure_type, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$morphology, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$dysplasia_grade, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$size_cm, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$location_lesion, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$resection_completeness, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$lesion_t_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$lesion_n_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$lesion_m_stage, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_severity, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # --- Input Validation Logic ---
  all_fields_valid <- reactive({
    if (is.null(input$procedure_type) || input$procedure_type == "") return(FALSE)
    if (is.null(input$lesion_present) || !nzchar(input$lesion_present)) return(FALSE)
    if (is.null(input$colitis_present) || !nzchar(input$colitis_present)) return(FALSE)

    if (input$lesion_present == "Yes") {
      if (is.null(input$lesion_type) || input$lesion_type == "") return(FALSE)
      if (is.null(input$morphology) || input$morphology == "") return(FALSE)
      if (input$lesion_type %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia") &&
          (is.null(input$dysplasia_grade) || input$dysplasia_grade == "")) return(FALSE)
      if (is.null(input$size_cm) || input$size_cm == "") return(FALSE)
      if (is.null(input$location_lesion) || input$location_lesion == "") return(FALSE)
      if (is.null(input$resection_completeness) || input$resection_completeness == "") return(FALSE)
      if (input$lesion_type == "Adenocarcinoma") {
        if (is.null(input$lesion_t_stage) || input$lesion_t_stage == "") return(FALSE)
        if (is.null(input$lesion_n_stage) || input$lesion_n_stage == "") return(FALSE)
        if (is.null(input$lesion_m_stage) || input$lesion_m_stage == "") return(FALSE)
      }
    }

    if (input$colitis_present == "Yes") {
      # At least one extent checkbox must be checked OR pancolitis
      any_extent_checked <- input$colitis_rectum || input$colitis_sigmoid || input$colitis_descending ||
        input$colitis_transverse || input$colitis_ascending_cecum || input$colitis_pancolitis
      if (!any_extent_checked) return(FALSE)
      if (is.null(input$colitis_severity) || input$colitis_severity == "") return(FALSE)
    }
    return(TRUE)
  })

  observe({ if (all_fields_valid()) { shinyjs::enable("submit") } else { shinyjs::disable("submit") } })
  check_all_fields_validity <- function() { all_fields_valid() }

  # Update UI for index
  update_ui_for_index <- function(index) {
    session_ids <- patient_ids_this_session()
    total_session_patients <- length(session_ids)
    if(total_session_patients == 0) { reset_inputs(); output$patient_id_display <- renderText("No patients to review"); output$progress_display <- renderText("0 / 0"); output$patient_text_display <- renderUI(HTML("All potential patients reviewed.")); shinyjs::disable("back"); shinyjs::disable("submit"); return() }
    req(index > 0 && index <= total_session_patients); pid <- session_ids[index]; req(pid)
    message("Updating UI for session index: ", index, ", ID: ", pid)
    output$patient_id_display <- renderText({ paste("Patient ID:", pid) })
    output$progress_display <- renderText({ sprintf("Session Progress: %d / %d (Record %d)", length(intersect(session_ids, unique(as.character(session_annotations()$ID)))), total_session_patients, index) })
    output$patient_text_display <- renderUI({
      text_row <- all_potential_patient_data[as.character(all_potential_patient_data$ID) == as.character(pid), ]
      if(nrow(text_row) == 1) {
        text <- text_row$Text; highlighted_text <- text
        highlighted_text <- gsub(highlight_morphology_regex, "<span class='highlight-morph'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_lesion_type_regex, "<span class='highlight-ltype'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_dysplasia_grade_regex, "<span class='highlight-dysp'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_resection_regex, "<span class='highlight-resect'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_colitis_terms_regex, "<span class='highlight-colitis'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_location_regex, "<span class='highlight-loc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_date_regex, "<span class='highlight-date'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_procedure_regex, "<span class='highlight-proc'>\\1</span>", highlighted_text, perl = TRUE)
        HTML(highlighted_text)
      } else { HTML(paste("Error: Could not find text for Patient ID", pid)) }
    })
    load_session_annotation_to_ui(pid)
    if (index == 1) { shinyjs::disable("back") } else { shinyjs::enable("back") }
    if (index > total_session_patients) { shinyjs::disable("submit") }
  }

  observeEvent(input$fontSize, { shinyjs::runjs(sprintf("document.querySelector('.scroll-box').style.fontSize = '%dpx';", input$fontSize)) })

  # Submit button observer
  observeEvent(input$submit, {
    message("Submit button clicked.")
    isolate({
      if (!all_fields_valid()) {
        showModal(modalDialog(title = "Missing Information", "Please ensure all required fields are completed based on your selections.", easyClose = TRUE, footer = modalButton("OK")))
        message("Submission failed: Validation check failed."); return()
      }
      pid <- patient_ids_this_session()[current_index()]
      new_entry <- data.frame(
        ID = as.character(pid),
        DifficultCase = input$difficult_case,
        ProcedureType = input$procedure_type,
        LesionPresent = input$lesion_present,
        LesionType = ifelse(input$lesion_present == "Yes", input$lesion_type, ""),
        Morphology = ifelse(input$lesion_present == "Yes", input$morphology, ""),
        DysplasiaGrade = ifelse(input$lesion_present == "Yes" && input$lesion_type %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia"), input$dysplasia_grade, ""),
        SizeCM = ifelse(input$lesion_present == "Yes", input$size_cm, ""),
        LocationLesion = ifelse(input$lesion_present == "Yes", input$location_lesion, ""),
        ResectionCompleteness = ifelse(input$lesion_present == "Yes", input$resection_completeness, ""),
        LesionTstage = ifelse(input$lesion_present == "Yes" && input$lesion_type == "Adenocarcinoma", input$lesion_t_stage, ""),
        LesionNstage = ifelse(input$lesion_present == "Yes" && input$lesion_type == "Adenocarcinoma", input$lesion_n_stage, ""),
        LesionMstage = ifelse(input$lesion_present == "Yes" && input$lesion_type == "Adenocarcinoma", input$lesion_m_stage, ""),
        ColitisPresent = input$colitis_present,
        ColitisRectum = ifelse(input$colitis_present == "Yes", input$colitis_rectum, FALSE),
        ColitisSigmoid = ifelse(input$colitis_present == "Yes", input$colitis_sigmoid, FALSE),
        ColitisDescending = ifelse(input$colitis_present == "Yes", input$colitis_descending, FALSE),
        ColitisTransverse = ifelse(input$colitis_present == "Yes", input$colitis_transverse, FALSE),
        ColitisAscendingCecum = ifelse(input$colitis_present == "Yes", input$colitis_ascending_cecum, FALSE),
        ColitisPancolitis = ifelse(input$colitis_present == "Yes", input$colitis_pancolitis, FALSE),
        ColitisSeverity = ifelse(input$colitis_present == "Yes", input$colitis_severity, ""),
        stringsAsFactors = FALSE
      )
      message("Created new entry for ID: ", pid)
      anno_df <- session_annotations(); anno_df <- anno_df[as.character(anno_df$ID) != as.character(pid), ]; anno_df <- rbind(anno_df, new_entry); session_annotations(anno_df)
      message("Session annotations updated. Total this session: ", nrow(anno_df))
      tryCatch({ write.csv(session_annotations(), out_file, row.names = FALSE, quote = TRUE); message("Successfully wrote to: ", out_file) }, error = function(e) { warning("FAILED to write: ", e$message); showModal(modalDialog(title = "Error Saving", paste("Could not write:", out_file, e$message))) })
      if (current_index() < length(patient_ids_this_session())) {
        current_index(current_index() + 1); update_ui_for_index(current_index())
      } else {
        output$patient_id_display <- renderText("All Done!"); output$progress_display <- renderText(sprintf("Completed: %d / %d", length(patient_ids_this_session()), length(patient_ids_this_session()))); output$patient_text_display <- renderUI(""); reset_inputs(); shinyjs::disable("submit")
        showModal(modalDialog(title = "Session Complete", "All patients reviewed.", easyClose = TRUE))
      }
    })
  })

  observeEvent(input$back, { if (current_index() > 1) { current_index(current_index() - 1); update_ui_for_index(current_index()) } })
  observeEvent(session, { message("Initial UI load."); shinyjs::hide("lesion_details_div"); shinyjs::hide("dysplasia_grade_div"); shinyjs::hide("tnm_staging_div"); shinyjs::hide("colitis_details_div"); update_ui_for_index(current_index()) }, once = TRUE)
}

`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) { b } else { a } }
shinyApp(ui = ui, server = server)
