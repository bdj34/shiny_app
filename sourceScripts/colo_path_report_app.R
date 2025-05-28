# Load necessary libraries
library(shiny)
library(shinyjs)
library(purrr) # For map_dfr

# --- PLEASE READ ---
# Make a copy of this script in the reviewScripts_UseThese folder and add your name to the end of the script name
# For example, rename as: "PATH_TO_DIR/shiny_app/reviewScripts_UseThese/colo_path_report_app_Brian.R"
# I may have already done this for you.

# -- In your copied script, ADD YOUR NAME HERE:
# This app will save your reviews as a csv associated with your name.
reviewer <- "NAME_HERE"
# I may have already done this for you also.

# Run the app with the following R command:
# shiny::runApp("PATH_TO_DIR/shiny_app/reviewScripts_UseThese/colo_path_report_app_<YOUR_NAME>.R")
# --- END ---

# Optionally edit the regular expressions you want to highlight
highlight_morphology_regex <- "(?i)(polypoid|flat|depressed|elevated|ulcerated|dalm|invisible|random|non-polypoid|pedunculated|sessile|mass)"
highlight_lesion_type_regex <- paste0("(?i)(adenocarcinoma|low grade dysplasia|low-grade dysplasia|high grade dysplasia|high-grade dysplasia|",
                              "lgd|hgd|indefinite for dysplasia|no dysplasia|",
                              "dysplasia|dysplastic|tubulovillous adenoma|villous adenoma|tubular adenoma|",
                              "sessile serrated|traditional serrated adenoma|ssl|ssa|tsa|hyperplastic|adenoma)")
highlight_resection_regex <- "(?i)(complete resection|incomplete resection|resected|margin|piecemeal|en bloc|R0|R1|R2|incompletely|completely)"
highlight_colitis_terms_regex <- "(?i)(colitis|proctitis|inflammation|inflammatory|inflam|cryptitis|crypt abscess|mild|moderate|severe|acute|chronic)"
highlight_location_regex <- "(?i)(rectum|rectosigmoid|sigmoid\\b|descending|splenic|transverse|hepatic|ascending|cecum|appendix|colon\\b|ileum)"
highlight_procedure_regex <- "(?i)(polypectomy|biopsy|EMR|ESD|colectomy|endoscopic mucosal|endoscopic submucosal|ectomy|sigmoidoscopy)"
highlight_size_regex <- "(?i)(\\d+(\\.\\d+)?\\s?(cm|mm))"
highlight_break_regex <- "(>{2,}\\s*\\n+\\s*<{2,})"
# --- End Configuration. Should not need to edit anything beyond here ---

# --- One time setup ---
rootOutDir <- "~/shiny_app_output/" #P:/ORD_Curtius_202210036D/shiny_app/shiny_app_output/
max_lesions_to_capture <- 5
annotation_type <- "path_colo_review"
outDir <- file.path(rootOutDir, paste0("output_csvs_", annotation_type, "_", reviewer))

if (!dir.exists(outDir)) {
  message("Creating output directory: ", outDir)
  tryCatch({ dir.create(outDir, recursive = TRUE); message("Output directory created successfully.") },
           error = function(e) { stop("Failed to create output directory: ", outDir, " Error: ", e$message) })
} else { message("Using existing output directory: ", outDir) }

session_stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
out_file <- file.path(outDir, paste0(annotation_type, "_", reviewer, "_reviewed_", session_stamp, ".csv"))
message("Output file for this session: ", out_file)

# --- Data Loading ---
# !!! REPLACE THIS with your actual data loading logic !!!
# ** MODIFIED to include PathReportText and ColoReportText **
all_potential_patient_data <- data.frame(
  ID = c("PAT001", "PAT002", "PAT003", "PAT004", "PAT005", "PAT006", "PAT007"),
  ColoReportText = c( # Example Colonoscopy Reports
    "Patient PAT001: Colonoscopy on January 10, 2023. Findings: Sigmoid colon shows a 1.5 cm polypoid lesion. Resection attempted. Background mucosa shows mild active colitis, proctitis only. Procedure: Polypectomy.",
    "Patient PAT002: Cecum biopsy performed 2022-05-18 during colonoscopy for evaluation of extensive chronic colitis. One 0.8 cm adenoma seen and biopsied.",
    "Patient PAT003: Flexible sigmoidoscopy to rectum on April 2024. Multiple small polyps found, not removed. No evidence of colitis. Some invisible changes noted, random biopsies taken.",
    "Patient PAT004: Colectomy performed July 1, 2021 for a large mass. Pre-op colonoscopy noted a 3.0 cm mass.",
    "Patient PAT005: EMR of a 2.5cm flat lesion in the ascending colon. Procedure successful.",
    "Patient PAT006: Surveillance colonoscopy. Random biopsies from transverse colon taken due to history of colitis.",
    "Patient PAT007: Sigmoidoscopy. One 0.5cm polypoid lesion, morphology described as unusual, biopsied. Another 1.2cm non-polypoid lesion, attempted EMR."
  ),
  PathReportText = c( # Example Pathology Reports
    "PAT001 Pathology: Sigmoid polyp: tubular adenoma with low grade dysplasia. Resection margins appear clear (R0). Rectal biopsies: mild active proctitis.",
    "PAT002 Pathology: Cecum biopsy: Tubular adenoma, 0.8 cm, with high grade dysplasia. Margins involved (R1). Background: extensive chronic colitis.",
    "PAT003 Pathology: Rectal biopsies: Hyperplastic polyps. Random biopsies: non-specific mild inflammation.",
    "PAT004 Pathology: Colectomy specimen: Invasive adenocarcinoma, 3.0 cm, arising in a tubulovillous adenoma. Margins clear. T3N1M0. Background: Pancolitis with moderate activity.",
    "PAT005 Pathology: EMR specimen, ascending colon: Sessile serrated lesion with dysplasia. Resection piecemeal, margins uncertain.",
    "PAT006 Pathology: Transverse colon biopsies show focal active colitis, mild. No dysplasia.",
    "PAT007 Pathology: Sigmoid polyp biopsy: high grade dysplasia. Ascending colon EMR: intramucosal adenocarcinoma."
  ),
  stringsAsFactors = FALSE
)
all_potential_patient_data$ID <- as.character(all_potential_patient_data$ID)
message("Loaded ", nrow(all_potential_patient_data), " total potential patient records.")

# --- Identify Previously Reviewed IDs --- (Same logic as before)
existing_files <- list.files(path = outDir, pattern = paste0("^", annotation_type, "_", reviewer, "_reviewed_\\d{4}-\\d{2}-\\d{2}_\\d{6}\\.csv$"), full.names = TRUE)
previously_reviewed_ids <- character(0)
if (length(existing_files) > 0) {
  all_prev_annotations <- purrr::map_dfr(existing_files, function(f) { tryCatch({ df <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE, fill = TRUE, header = TRUE); if ("ID" %in% names(df)) { valid_ids <- df$ID[!is.na(df$ID) & nzchar(df$ID)]; if(length(valid_ids) > 0) { data.frame(ID = valid_ids, stringsAsFactors = FALSE) } else { NULL } } else { NULL } }, error = function(e) { NULL }) })
  if (!is.null(all_prev_annotations) && nrow(all_prev_annotations) > 0 && "ID" %in% names(all_prev_annotations)) { previously_reviewed_ids <- unique(all_prev_annotations$ID) }
}
patient_data_for_session <- all_potential_patient_data[!all_potential_patient_data$ID %in% previously_reviewed_ids, ]
message("Filtered patient list for this session. ", nrow(patient_data_for_session), " patients remaining to be reviewed.")
if(nrow(patient_data_for_session) == 0) { message("WARNING: No unreviewed patients remaining for this session.") }

# Annotations schema remains the same
annotations_schema <- data.frame(
  ID = character(), DifficultCase = logical(), ProcedureType = character(),
  LesionPresent = character(), NumLesions = character(),
  Lesion1_Type = character(), Lesion1_Morphology = character(), Lesion1_DysplasiaGrade = character(), Lesion1_SizeCM = character(), Lesion1_LocationLesion = character(), Lesion1_ResectionCompleteness = character(), Lesion1_EMRperformed = logical(), Lesion1_ESDperformed = logical(), Lesion1_Tstage = character(), Lesion1_Nstage = character(), Lesion1_Mstage = character(),
  Lesion2_Type = character(), Lesion2_Morphology = character(), Lesion2_DysplasiaGrade = character(), Lesion2_SizeCM = character(), Lesion2_LocationLesion = character(), Lesion2_ResectionCompleteness = character(), Lesion2_EMRperformed = logical(), Lesion2_ESDperformed = logical(), Lesion2_Tstage = character(), Lesion2_Nstage = character(), Lesion2_Mstage = character(),
  Lesion3_Type = character(), Lesion3_Morphology = character(), Lesion3_DysplasiaGrade = character(), Lesion3_SizeCM = character(), Lesion3_LocationLesion = character(), Lesion3_ResectionCompleteness = character(), Lesion3_EMRperformed = logical(), Lesion3_ESDperformed = logical(), Lesion3_Tstage = character(), Lesion3_Nstage = character(), Lesion3_Mstage = character(),
  ColitisPresent = character(), ColitisRectum = logical(), ColitisSigmoid = logical(), ColitisDescending = logical(), ColitisTransverse = logical(), ColitisAscendingCecum = logical(), ColitisPancolitis = logical(), ColitisSeverity = character(),
  stringsAsFactors = FALSE
)

# --- UI Definition ---
lesion_type_choices <- c("Select..." = "", "Adenocarcinoma", "Dysplasia", "Tubulovillous Adenoma", "Villous Adenoma", "Tubular Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Hyperplastic Polyp", "Adenoma", "Other", "Unknown")
morphology_choices <- c("Select..." = "", "Polypoid", "Non-polypoid", "Invisible/Random Biopsies", "Unknown")
dysplasia_grade_choices <- c("Select..." = "", "Low-grade Dysplasia (LGD)", "High-grade Dysplasia (HGD)", "Indefinite for Dysplasia", "No Dysplasia", "Not Applicable", "Unknown")
location_choices <- c("Select..." = "", "Unknown", "Rectum", "Rectosigmoid", "Sigmoid Colon", "Descending Colon", "Splenic Flexure", "Transverse Colon", "Hepatic Flexure", "Ascending Colon", "Cecum", "Ileocecal Valve", "Terminal Ileum", "Appendix", "Multiple Locations", "Diffuse", "Other")
resection_choices <- c("Select..." = "", "Complete (R0)", "Incomplete - Microscopic (R1)", "Incomplete - Macroscopic (R2)", "Piecemeal", "Uncertain", "Not Resected", "Not Applicable")
colitis_severity_choices <- c("Select..." = "", "Quiescent / Inactive", "Mild", "Moderate", "Severe", "Unknown")
size_cm_values <- c(0, round(seq(0.1, 10, by = 0.1), 1))
size_cm_choices <- c("Select..." = "", "Unknown", "Biopsy only" = "Biopsy only", setNames(as.character(size_cm_values[size_cm_values > 0]), paste0(sprintf("%.1f", size_cm_values[size_cm_values > 0]), " cm")), "10+ cm" = "10+")
procedure_choices <- c("Select..." = "", "Colonoscopy", "Sigmoidoscopy", "Colectomy")
t_stage_choices <- c("Select..." = "", "Unknown", "Tis", "T1", "T2", "T3", "T4a", "T4b", "TX", "Not Applicable")
n_stage_choices <- c("Select..." = "", "Unknown", "N0", "N1", "N1a", "N1b", "N1c", "N2", "N2a", "N2b", "NX", "Not Applicable")
m_stage_choices <- c("Select..." = "", "Unknown", "M0", "M1", "M1a", "M1b", "M1c", "MX", "Not Applicable")
num_lesions_choices <- c("Select..." = "", "1", "2", "3", "4", "5+")

create_lesion_ui <- function(lesion_number) {
  ns_prefix <- paste0("lesion", lesion_number, "_")
  tagList(
    h6(paste("Lesion", lesion_number, "Details:")),
    selectInput(paste0(ns_prefix, "type"), "Lesion Type:", choices = lesion_type_choices, selected = ""),
    selectInput(paste0(ns_prefix, "morphology"), "Morphology:", choices = morphology_choices, selected = ""),
    div(id = paste0(ns_prefix, "dysplasia_grade_div"), style = "display:none;",
        selectInput(paste0(ns_prefix, "dysplasia_grade"), "Dysplasia Grade:", choices = dysplasia_grade_choices, selected = "")
    ),
    selectInput(paste0(ns_prefix, "size_cm"), "Size (cm):", choices = size_cm_choices, selected = ""),
    selectInput(paste0(ns_prefix, "location"), "Location:", choices = location_choices, selected = ""),
    selectInput(paste0(ns_prefix, "resection"), "Completeness of Resection:", choices = resection_choices, selected = ""),
    checkboxInput(paste0(ns_prefix, "emr"), "EMR performed?", FALSE),
    checkboxInput(paste0(ns_prefix, "esd"), "ESD performed?", FALSE),
    div(id = paste0(ns_prefix, "tnm_div"), style = "display: none;",
        h6(paste("Lesion", lesion_number, "Adenocarcinoma Staging:")),
        selectInput(paste0(ns_prefix, "t_stage"), "T Stage:", choices = t_stage_choices, selected = ""),
        selectInput(paste0(ns_prefix, "n_stage"), "N Stage:", choices = n_stage_choices, selected = ""),
        selectInput(paste0(ns_prefix, "m_stage"), "M Stage:", choices = m_stage_choices, selected = "")
    ),
    hr()
  )
}

ui <- fluidPage(
  titlePanel(paste("Path/Colo Report Review:", annotation_type, "-", reviewer)),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 4, # Keep sidebar width reasonable
      h4(textOutput("patient_id_display")),
      textOutput("progress_display"),
      hr(),
      h5("General Information"),
      checkboxInput("difficult_case", "Difficult Case (Overall)?", FALSE),
      selectInput("procedure_type", "Procedure Type:", choices = procedure_choices, selected = ""),
      hr(),
      h5("Lesion Findings"),
      radioButtons("lesion_present", "Lesions of interest present?", choices = c("Yes", "No", "Uncertain"), selected = character(0), inline = TRUE),
      div(id = "num_lesions_div", style = "display: none;",
          selectInput("num_lesions", "Number of Lesions to Detail:", choices = num_lesions_choices, selected = "")
      ),
      uiOutput("dynamic_lesion_fields_ui"),
      hr(),
      h5("Colitis Findings"),
      radioButtons("colitis_present", "Colitis Present?", choices = c("Yes", "No", "Uncertain"), selected = character(0), inline = TRUE),
      div(id = "colitis_details_div", style = "display: none;",
          h6("Colitis Extent:"),
          checkboxInput("colitis_rectum", "Rectum", FALSE), checkboxInput("colitis_sigmoid", "Sigmoid", FALSE),
          checkboxInput("colitis_descending", "Descending Colon", FALSE), checkboxInput("colitis_transverse", "Transverse Colon", FALSE),
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
    # ** MainPanel now uses fluidRow for two columns **
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
        .highlight-size { background-color: #ffcccb; font-weight: bold;}
        .highlight-break { background-color: red; font-weight: bold;}
        .highlight-proc { background-color: #E0FFFF; font-weight: bold;}
      ")),
      fluidRow(
        column(width = 6,
               h5("Pathology Report"),
               uiOutput("path_report_text_output", class = "scroll-box") # Apply class
        ),
        column(width = 6,
               h5("Colonoscopy Report(s)"),
               uiOutput("colo_report_text_output", class = "scroll-box") # Apply class
        )
      )
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
    # This will now apply to both scroll boxes if they have the class '.scroll-box'
    shinyjs::runjs(sprintf("document.querySelectorAll('.scroll-box').forEach(el => el.style.fontSize = '%dpx');", initial_font_size))
  }, once = TRUE)

  # Render dynamic lesion fields
  output$dynamic_lesion_fields_ui <- renderUI({
    num_to_show <- input$num_lesions
    if (is.null(num_to_show) || num_to_show == "" || input$lesion_present != "Yes") {
      return(NULL)
    }
    num_to_show_actual <- if (grepl("\\+", num_to_show)) max_lesions_to_capture else min(as.integer(num_to_show), max_lesions_to_capture)
    if (is.na(num_to_show_actual) || num_to_show_actual < 1) return(NULL)
    lesion_uis <- lapply(1:num_to_show_actual, create_lesion_ui)
    do.call(tagList, lesion_uis)
  })


  reset_inputs <- function() {
    message("Resetting inputs")
    updateCheckboxInput(session, "difficult_case", value = FALSE)
    updateSelectInput(session, "procedure_type", selected = "")
    updateRadioButtons(session, "lesion_present", selected = character(0))
    updateSelectInput(session, "num_lesions", selected = "")

    for (i in 1:max_lesions_to_capture) {
      ns_prefix <- paste0("lesion", i, "_")
      updateSelectInput(session, paste0(ns_prefix, "type"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "morphology"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "dysplasia_grade"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "size_cm"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "location"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "resection"), selected = "")
      updateCheckboxInput(session, paste0(ns_prefix, "emr"), value = FALSE)
      updateCheckboxInput(session, paste0(ns_prefix, "esd"), value = FALSE)
      updateSelectInput(session, paste0(ns_prefix, "t_stage"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "n_stage"), selected = "")
      updateSelectInput(session, paste0(ns_prefix, "m_stage"), selected = "")
      shinyjs::hide(paste0(ns_prefix, "dysplasia_grade_div"))
      shinyjs::hide(paste0(ns_prefix, "tnm_div"))
      shinyjs::enable(paste0(ns_prefix, "size_cm")); shinyjs::enable(paste0(ns_prefix, "resection"))
    }

    updateRadioButtons(session, "colitis_present", selected = character(0))
    updateCheckboxInput(session, "colitis_rectum", value = FALSE); updateCheckboxInput(session, "colitis_sigmoid", value = FALSE)
    updateCheckboxInput(session, "colitis_descending", value = FALSE); updateCheckboxInput(session, "colitis_transverse", value = FALSE)
    updateCheckboxInput(session, "colitis_ascending_cecum", value = FALSE); updateCheckboxInput(session, "colitis_pancolitis", value = FALSE)
    shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum")
    updateSelectInput(session, "colitis_severity", selected = "")

    shinyjs::hide("num_lesions_div"); shinyjs::hide("colitis_details_div")
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
      colitis_present_val <- record$ColitisPresent %||% ""
      num_lesions_val <- record$NumLesions %||% ""

      show_num_lesions_div <- lesion_present_val == "Yes"
      shinyjs::toggle(id = "num_lesions_div", condition = show_num_lesions_div)
      updateSelectInput(session, "num_lesions", selected = num_lesions_val) # This triggers UI render

      shinyjs::delay(50, { # Increased delay slightly to ensure dynamic UI elements are available
        isolate({
          updateCheckboxInput(session, "difficult_case", value = as.logical(record$DifficultCase %||% FALSE))
          updateSelectInput(session, "procedure_type", selected = record$ProcedureType %||% "")
          updateRadioButtons(session, "lesion_present", selected = lesion_present_val)
          show_colitis_details <- colitis_present_val == "Yes"
          shinyjs::toggle(id = "colitis_details_div", condition = show_colitis_details)
          updateRadioButtons(session, "colitis_present", selected = colitis_present_val)
          updateCheckboxInput(session, "colitis_rectum", value = as.logical(record$ColitisRectum %||% FALSE))
          updateCheckboxInput(session, "colitis_sigmoid", value = as.logical(record$ColitisSigmoid %||% FALSE))
          updateCheckboxInput(session, "colitis_descending", value = as.logical(record$ColitisDescending %||% FALSE))
          updateCheckboxInput(session, "colitis_transverse", value = as.logical(record$ColitisTransverse %||% FALSE))
          updateCheckboxInput(session, "colitis_ascending_cecum", value = as.logical(record$ColitisAscendingCecum %||% FALSE))
          updateCheckboxInput(session, "colitis_pancolitis", value = as.logical(record$ColitisPancolitis %||% FALSE))
          updateSelectInput(session, "colitis_severity", selected = record$ColitisSeverity %||% "")
        })

        num_lesions_to_load <- if (grepl("\\+", num_lesions_val)) max_lesions_to_capture else as.integer(num_lesions_val)
        if (!is.na(num_lesions_to_load) && num_lesions_to_load > 0 && show_num_lesions_div) { # Check if lesion section should be shown
          for (i in 1:min(num_lesions_to_load, max_lesions_to_capture)) {
            ns_prefix <- paste0("lesion", i, "_")
            lesion_type_val_i <- record[[paste0("Lesion", i, "_Type")]] %||% ""
            morphology_val_i <- record[[paste0("Lesion", i, "_Morphology")]] %||% ""

            show_dysplasia_i <- lesion_type_val_i %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia")
            show_tnm_i <- lesion_type_val_i == "Adenocarcinoma"

            shinyjs::toggle(id = paste0(ns_prefix, "dysplasia_grade_div"), condition = show_dysplasia_i)
            shinyjs::toggle(id = paste0(ns_prefix, "tnm_div"), condition = show_tnm_i)

            isolate({
              updateSelectInput(session, paste0(ns_prefix, "type"), selected = lesion_type_val_i)
              updateSelectInput(session, paste0(ns_prefix, "morphology"), selected = morphology_val_i)
              updateSelectInput(session, paste0(ns_prefix, "dysplasia_grade"), selected = record[[paste0("Lesion", i, "_DysplasiaGrade")]] %||% "")
              updateSelectInput(session, paste0(ns_prefix, "size_cm"), selected = record[[paste0("Lesion", i, "_SizeCM")]] %||% "")
              updateSelectInput(session, paste0(ns_prefix, "location"), selected = record[[paste0("Lesion", i, "_LocationLesion")]] %||% "")
              updateSelectInput(session, paste0(ns_prefix, "resection"), selected = record[[paste0("Lesion", i, "_ResectionCompleteness")]] %||% "")
              updateCheckboxInput(session, paste0(ns_prefix, "emr"), value = as.logical(record[[paste0("Lesion", i, "_EMRperformed")]]) %||% FALSE)
              updateCheckboxInput(session, paste0(ns_prefix, "esd"), value = as.logical(record[[paste0("Lesion", i, "_ESDperformed")]]) %||% FALSE)
              updateSelectInput(session, paste0(ns_prefix, "t_stage"), selected = record[[paste0("Lesion", i, "_Tstage")]] %||% "")
              updateSelectInput(session, paste0(ns_prefix, "n_stage"), selected = record[[paste0("Lesion", i, "_Nstage")]] %||% "")
              updateSelectInput(session, paste0(ns_prefix, "m_stage"), selected = record[[paste0("Lesion", i, "_Mstage")]] %||% "")
            })
            if(morphology_val_i == "Invisible/Random Biopsies"){
              isolate({
                updateSelectInput(session, paste0(ns_prefix, "size_cm"), selected = "Biopsy only")
                updateSelectInput(session, paste0(ns_prefix, "resection"), selected = "Not Applicable")
              })
              shinyjs::disable(paste0(ns_prefix, "size_cm"))
              shinyjs::disable(paste0(ns_prefix, "resection"))
            } else {
              shinyjs::enable(paste0(ns_prefix, "size_cm"))
              shinyjs::enable(paste0(ns_prefix, "resection"))
            }
          }
        }
        if(as.logical(record$ColitisPancolitis %||% FALSE)){
          # Use JS to ensure observer fires correctly after updates
          shinyjs::runjs(paste0("Shiny.setInputValue('colitis_pancolitis', true, {priority: 'event'});"))
        }
        check_all_fields_validity()
      })
    } else {
      message("No existing annotation found. Resetting inputs.")
      reset_inputs()
    }
  }


  # --- Conditional UI Logic ---
  toggle_conditional_fields_path_colo <- function() {
    lesion_present_val <- input$lesion_present
    colitis_present_val <- input$colitis_present

    show_num_lesions_div <- !is.null(lesion_present_val) && lesion_present_val == "Yes"
    shinyjs::toggle(id = "num_lesions_div", condition = show_num_lesions_div)
    if (!show_num_lesions_div) { isolate(updateSelectInput(session, "num_lesions", selected = "")) }

    # Lesion details div itself is implicitly handled by dynamic_lesion_fields_ui outputting NULL
    # Sub-parts of lesion details (dysplasia, TNM) are handled by their specific lesion_type observers

    show_colitis_details <- !is.null(colitis_present_val) && colitis_present_val == "Yes"
    shinyjs::toggle(id = "colitis_details_div", condition = show_colitis_details)
    if (!show_colitis_details) {
      isolate({
        updateCheckboxInput(session, "colitis_rectum", value = FALSE); updateCheckboxInput(session, "colitis_sigmoid", value = FALSE)
        updateCheckboxInput(session, "colitis_descending", value = FALSE); updateCheckboxInput(session, "colitis_transverse", value = FALSE)
        updateCheckboxInput(session, "colitis_ascending_cecum", value = FALSE); updateCheckboxInput(session, "colitis_pancolitis", value = FALSE)
        shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum")
        updateSelectInput(session, "colitis_severity", selected = "")
      })
    }
  }

  observeEvent(input$lesion_present, { toggle_conditional_fields_path_colo(); check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_present, { toggle_conditional_fields_path_colo(); check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$num_lesions, {
    # This event will trigger re-rendering of dynamic_lesion_fields_ui
    # Also need to re-check validity
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Observers for dynamic lesion type fields and morphology
  lapply(1:max_lesions_to_capture, function(i) {
    ns_prefix <- paste0("lesion", i, "_")
    observeEvent(input[[paste0(ns_prefix, "type")]], {
      lesion_type_val_i <- input[[paste0(ns_prefix, "type")]]
      show_dysplasia_i <- !is.null(lesion_type_val_i) && lesion_type_val_i %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia")
      shinyjs::toggle(id = paste0(ns_prefix, "dysplasia_grade_div"), condition = show_dysplasia_i)
      if (!show_dysplasia_i) { isolate(updateSelectInput(session, paste0(ns_prefix, "dysplasia_grade"), selected = "")) }

      show_tnm_i <- !is.null(lesion_type_val_i) && lesion_type_val_i == "Adenocarcinoma"
      shinyjs::toggle(id = paste0(ns_prefix, "tnm_div"), condition = show_tnm_i)
      if (!show_tnm_i) { isolate({ updateSelectInput(session, paste0(ns_prefix, "t_stage"), selected = ""); updateSelectInput(session, paste0(ns_prefix, "n_stage"), selected = ""); updateSelectInput(session, paste0(ns_prefix, "m_stage"), selected = "") })}
      check_all_fields_validity()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input[[paste0(ns_prefix, "morphology")]], {
      morphology_val_i <- input[[paste0(ns_prefix, "morphology")]]
      if (!is.null(morphology_val_i) && morphology_val_i == "Invisible/Random Biopsies") {
        isolate({
          updateSelectInput(session, paste0(ns_prefix, "size_cm"), selected = "Biopsy only")
          updateSelectInput(session, paste0(ns_prefix, "resection"), selected = "Not Applicable")
        })
        shinyjs::disable(paste0(ns_prefix, "size_cm"))
        shinyjs::disable(paste0(ns_prefix, "resection"))
      } else {
        shinyjs::enable(paste0(ns_prefix, "size_cm"))
        shinyjs::enable(paste0(ns_prefix, "resection"))
      }
      check_all_fields_validity()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })


  # Observer for Pancolitis checkbox
  observeEvent(input$colitis_pancolitis, {
    if (input$colitis_pancolitis) {
      updateCheckboxInput(session, "colitis_rectum", value = TRUE); shinyjs::disable("colitis_rectum")
      updateCheckboxInput(session, "colitis_sigmoid", value = TRUE); shinyjs::disable("colitis_sigmoid")
      updateCheckboxInput(session, "colitis_descending", value = TRUE); shinyjs::disable("colitis_descending")
      updateCheckboxInput(session, "colitis_transverse", value = TRUE); shinyjs::disable("colitis_transverse")
      updateCheckboxInput(session, "colitis_ascending_cecum", value = TRUE); shinyjs::disable("colitis_ascending_cecum")
    } else {
      shinyjs::enable("colitis_rectum"); shinyjs::enable("colitis_sigmoid"); shinyjs::enable("colitis_descending"); shinyjs::enable("colitis_transverse"); shinyjs::enable("colitis_ascending_cecum")
    }
    check_all_fields_validity()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Observers for all other fields that might affect validity
  observeEvent(input$procedure_type, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  lapply(1:max_lesions_to_capture, function(i) {
    ns_prefix <- paste0("lesion", i, "_")
    observeEvent(input[[paste0(ns_prefix, "dysplasia_grade")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "size_cm")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "location")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "resection")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "emr")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "esd")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "t_stage")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "n_stage")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input[[paste0(ns_prefix, "m_stage")]], { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })
  observeEvent(input$colitis_rectum, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_sigmoid, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_descending, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_transverse, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_ascending_cecum, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$colitis_severity, { check_all_fields_validity() }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # --- Input Validation Logic ---
  all_fields_valid <- reactive({
    if (is.null(input$procedure_type) || input$procedure_type == "") return(FALSE)
    if (is.null(input$lesion_present) || !nzchar(input$lesion_present)) return(FALSE)
    if (is.null(input$colitis_present) || !nzchar(input$colitis_present)) return(FALSE)

    if (input$lesion_present == "Yes") {
      if (is.null(input$num_lesions) || input$num_lesions == "") return(FALSE)
      num_to_validate <- if (grepl("\\+", input$num_lesions)) max_lesions_to_capture else as.integer(input$num_lesions)
      if(is.na(num_to_validate) || num_to_validate < 1) return(FALSE)

      for (i in 1:min(num_to_validate, max_lesions_to_capture)) {
        ns_prefix <- paste0("lesion", i, "_")
        lesion_type_i <- input[[paste0(ns_prefix, "type")]]
        morphology_i <- input[[paste0(ns_prefix, "morphology")]]

        if (is.null(lesion_type_i) || lesion_type_i == "") return(FALSE)
        if (is.null(morphology_i) || morphology_i == "") return(FALSE)

        if (lesion_type_i %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia") &&
            (is.null(input[[paste0(ns_prefix, "dysplasia_grade")]]) || input[[paste0(ns_prefix, "dysplasia_grade")]] == "")) return(FALSE)

        if (morphology_i != "Invisible/Random Biopsies") {
          if (is.null(input[[paste0(ns_prefix, "size_cm")]]) || input[[paste0(ns_prefix, "size_cm")]] == "") return(FALSE)
          if (is.null(input[[paste0(ns_prefix, "resection")]]) || input[[paste0(ns_prefix, "resection")]] == "") return(FALSE)
        } else { # For Invisible/Random, ensure they are set to the auto-filled values
          if (is.null(input[[paste0(ns_prefix, "size_cm")]]) || input[[paste0(ns_prefix, "size_cm")]] != "Biopsy only") return(FALSE)
          if (is.null(input[[paste0(ns_prefix, "resection")]]) || input[[paste0(ns_prefix, "resection")]] != "Not Applicable") return(FALSE)
        }


        if (is.null(input[[paste0(ns_prefix, "location")]]) || input[[paste0(ns_prefix, "location")]] == "") return(FALSE)
        # EMR/ESD are optional, not validated for submit button

        if (lesion_type_i == "Adenocarcinoma") {
          if (is.null(input[[paste0(ns_prefix, "t_stage")]]) || input[[paste0(ns_prefix, "t_stage")]] == "") return(FALSE)
          if (is.null(input[[paste0(ns_prefix, "n_stage")]]) || input[[paste0(ns_prefix, "n_stage")]] == "") return(FALSE)
          if (is.null(input[[paste0(ns_prefix, "m_stage")]]) || input[[paste0(ns_prefix, "m_stage")]] == "") return(FALSE)
        }
      }
    }

    if (input$colitis_present == "Yes") {
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
    output$path_report_text_output <- renderUI({ # Path report
      text_row <- all_potential_patient_data[as.character(all_potential_patient_data$ID) == as.character(pid), ]
      if(nrow(text_row) == 1 && "PathReportText" %in% names(text_row)) {
        text <- text_row$PathReportText %||% "Pathology report not available."
        highlighted_text <- text
        highlighted_text <- gsub(highlight_morphology_regex, "<span class='highlight-morph'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_lesion_type_regex, "<span class='highlight-ltype'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_resection_regex, "<span class='highlight-resect'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_colitis_terms_regex, "<span class='highlight-colitis'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_location_regex, "<span class='highlight-loc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_size_regex, "<span class='highlight-size'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_break_regex, "<span class='highlight-break'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_procedure_regex, "<span class='highlight-proc'>\\1</span>", highlighted_text, perl = TRUE)
        HTML(highlighted_text)
      } else { HTML(paste("Error: Could not find Path Report for Patient ID", pid)) }
    })
    output$colo_report_text_output <- renderUI({ # Colonoscopy report
      text_row <- all_potential_patient_data[as.character(all_potential_patient_data$ID) == as.character(pid), ]
      if(nrow(text_row) == 1 && "ColoReportText" %in% names(text_row)) {
        text <- text_row$ColoReportText %||% "Colonoscopy report not available."
        highlighted_text <- text
        highlighted_text <- gsub(highlight_morphology_regex, "<span class='highlight-morph'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_lesion_type_regex, "<span class='highlight-ltype'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_resection_regex, "<span class='highlight-resect'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_colitis_terms_regex, "<span class='highlight-colitis'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_location_regex, "<span class='highlight-loc'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_size_regex, "<span class='highlight-size'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_break_regex, "<span class='highlight-break'>\\1</span>", highlighted_text, perl = TRUE)
        highlighted_text <- gsub(highlight_procedure_regex, "<span class='highlight-proc'>\\1</span>", highlighted_text, perl = TRUE)
        HTML(highlighted_text)
      } else { HTML(paste("Error: Could not find Colonoscopy Report for Patient ID", pid)) }
    })
    load_session_annotation_to_ui(pid)
    if (index == 1) { shinyjs::disable("back") } else { shinyjs::enable("back") }
    if (index > total_session_patients) { shinyjs::disable("submit") }
  }

  observeEvent(input$fontSize, { shinyjs::runjs(sprintf("document.querySelectorAll('.scroll-box').forEach(el => el.style.fontSize = '%dpx');", input$fontSize)) })

  # Submit button observer
  observeEvent(input$submit, {
    message("Submit button clicked.")
    isolate({
      if (!all_fields_valid()) {
        showModal(modalDialog(title = "Missing Information", "Please ensure all required fields are completed based on your selections.", easyClose = TRUE, footer = modalButton("OK")))
        message("Submission failed: Validation check failed."); return()
      }
      pid <- patient_ids_this_session()[current_index()]
      new_entry_list <- list(
        ID = as.character(pid), DifficultCase = input$difficult_case, ProcedureType = input$procedure_type,
        LesionPresent = input$lesion_present, NumLesions = ifelse(input$lesion_present == "Yes", input$num_lesions, ""),
        ColitisPresent = input$colitis_present,
        ColitisRectum = ifelse(input$colitis_present == "Yes", input$colitis_rectum, FALSE), ColitisSigmoid = ifelse(input$colitis_present == "Yes", input$colitis_sigmoid, FALSE),
        ColitisDescending = ifelse(input$colitis_present == "Yes", input$colitis_descending, FALSE), ColitisTransverse = ifelse(input$colitis_present == "Yes", input$colitis_transverse, FALSE),
        ColitisAscendingCecum = ifelse(input$colitis_present == "Yes", input$colitis_ascending_cecum, FALSE), ColitisPancolitis = ifelse(input$colitis_present == "Yes", input$colitis_pancolitis, FALSE),
        ColitisSeverity = ifelse(input$colitis_present == "Yes", input$colitis_severity, "")
      )
      num_lesions_to_save <- 0
      if (input$lesion_present == "Yes" && !is.null(input$num_lesions) && input$num_lesions != "") {
        num_lesions_to_save <- if (grepl("\\+", input$num_lesions)) max_lesions_to_capture else as.integer(input$num_lesions)
        num_lesions_to_save <- min(num_lesions_to_save, max_lesions_to_capture)
      }
      for (i in 1:max_lesions_to_capture) {
        ns_prefix <- paste0("lesion", i, "_"); is_lesion_active <- i <= num_lesions_to_save && input$lesion_present == "Yes"
        lesion_type_i <- if(is_lesion_active) input[[paste0(ns_prefix, "type")]] else ""; morphology_i <- if(is_lesion_active) input[[paste0(ns_prefix, "morphology")]] else ""
        new_entry_list[[paste0("Lesion", i, "_Type")]] <- lesion_type_i
        new_entry_list[[paste0("Lesion", i, "_Morphology")]] <- morphology_i
        new_entry_list[[paste0("Lesion", i, "_DysplasiaGrade")]] <- if(is_lesion_active && lesion_type_i %in% c("Adenoma", "Tubular Adenoma", "Villous Adenoma", "Tubulovillous Adenoma", "Sessile Serrated Lesion/Polyp (SSL/P)", "Traditional Serrated Adenoma (TSA)", "Dysplasia")) input[[paste0(ns_prefix, "dysplasia_grade")]] else ""
        new_entry_list[[paste0("Lesion", i, "_SizeCM")]] <- if(is_lesion_active && morphology_i == "Invisible/Random Biopsies") "Biopsy only" else if(is_lesion_active) input[[paste0(ns_prefix, "size_cm")]] else ""
        new_entry_list[[paste0("Lesion", i, "_LocationLesion")]] <- if(is_lesion_active) input[[paste0(ns_prefix, "location")]] else ""
        new_entry_list[[paste0("Lesion", i, "_ResectionCompleteness")]] <- if(is_lesion_active && morphology_i == "Invisible/Random Biopsies") "Not Applicable" else if(is_lesion_active) input[[paste0(ns_prefix, "resection")]] else ""
        new_entry_list[[paste0("Lesion", i, "_EMRperformed")]] <- if(is_lesion_active) input[[paste0(ns_prefix, "emr")]] else FALSE
        new_entry_list[[paste0("Lesion", i, "_ESDperformed")]] <- if(is_lesion_active) input[[paste0(ns_prefix, "esd")]] else FALSE
        new_entry_list[[paste0("Lesion", i, "_Tstage")]] <- if(is_lesion_active && lesion_type_i == "Adenocarcinoma") input[[paste0(ns_prefix, "t_stage")]] else ""
        new_entry_list[[paste0("Lesion", i, "_Nstage")]] <- if(is_lesion_active && lesion_type_i == "Adenocarcinoma") input[[paste0(ns_prefix, "n_stage")]] else ""
        new_entry_list[[paste0("Lesion", i, "_Mstage")]] <- if(is_lesion_active && lesion_type_i == "Adenocarcinoma") input[[paste0(ns_prefix, "m_stage")]] else ""
      }
      new_entry <- as.data.frame(new_entry_list, stringsAsFactors = FALSE)
      message("Created new entry for ID: ", pid); anno_df <- session_annotations(); anno_df <- anno_df[as.character(anno_df$ID) != as.character(pid), ]; anno_df <- rbind(anno_df, new_entry); session_annotations(anno_df)
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
  observeEvent(session, { message("Initial UI load."); shinyjs::hide("num_lesions_div"); shinyjs::hide("colitis_details_div"); update_ui_for_index(current_index()) }, once = TRUE)
}

`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) { b } else { a } }
shinyApp(ui = ui, server = server)
