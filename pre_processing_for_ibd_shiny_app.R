# Load necessary libraries
library(stringr) # For string manipulation and regex
library(dplyr)   # For data manipulation verbs
library(purrr)   # For functional programming tools like map
library(lubridate) # For date parsing

# --- Configuration --- EDIT THESE FIELDS ---

# Regex for finding IBD mentions near a potential year (case-insensitive)
# Note: R requires escaping backslashes in strings, so \b becomes \\b
myregex <- "(?i)((crohn|ulcerative\\s+colitis|\\buc\\b|\\bu\\.c\\.\\b|\\bcuc\\b|\\bc\\.u\\.c\\.\\b|inflammatory\\s+bowel\\s+disease|\\bibd\\b|ulcerative\\s+proct|chronic\\s+colitis|chronic\\s+proct).{0,30}?((\\b\\d{2}\\b)|(\\b\\d{4}\\b)))|(((\\b\\d{2}\\b)|(\\b\\d{4}\\b)).{0,30}?(crohn|ulcerative\\s+colitis|\\buc\\b|\\bu\\.c\\.\\b|\\bc\\.u\\.c\\.\\b|\\bcuc\\b|inflammatory\\s+bowel\\s+disease|\\bibd\\b|ulcerative\\s+proct|chronic\\s+colitis|chronic\\s+proct))"

# Regex for finding IBD mentions near a 4-digit year (priority matches)
priorityregex <- "(?i)((crohn|ulcerative\\s+colitis|\\buc\\b|\\bu\\.c\\.\\b|\\bcuc\\b|\\bc\\.u\\.c\\.\\b|inflammatory\\s+bowel\\s+disease|\\bibd\\b|ulcerative\\s+proct|chronic\\s+colitis|chronic\\s+proct).{0,30}?((19|20)\\d{2}))|(((19|20)\\d{2}).{0,30}?(crohn|ulcerative\\s+colitis|\\buc\\b|\\bu\\.c\\.\\b|\\bc\\.u\\.c\\.\\b|\\bcuc\\b|inflammatory\\s+bowel\\s+disease|\\bibd\\b|ulcerative\\s+proct|chronic\\s+colitis|chronic\\s+proct))"

# Regex for ignoring excerpts containing ICD/SNOMED codes (case-insensitive)
icdIgnore <- "(?i)(556|555|K52|K51|K50|ICD|snomed)"

# Context extraction parameters
lines_before <- 2
lines_after <- 2
excerpt_limit <- 30        # Max excerpts to include per patient in the final output
n_most_recent <- 3        # Number of most recent non-priority excerpts to guarantee inclusion
n_most_distant <- 15       # Number of earliest non-priority excerpts to guarantee inclusion
max_excerpts_per_note <- 10 # Max excerpts to extract from a single note

# --- Data Loading ---
# !!! REPLACE THIS SECTION with your actual SQL data loading logic !!!
# Example using a placeholder dataframe:
# Ensure your loaded data has columns like PatientICN, EntryDateTime, ReportText
# notes_data <- dbGetQuery(con, "SELECT PatientICN, EntryDateTime, ReportText FROM YourNotesTable WHERE ...")
notes_data <- data.frame(
  PatientICN = c("PAT1", "PAT1", "PAT2", "PAT3", "PAT4", "PAT4"),
  EntryDateTime = c("2023-01-15 10:00:00", "2024-05-20 14:30:00", "2022-11-01 08:15:00", "2024-02-10 11:00:00", "2010-06-01 09:00:00", "2011-08-15 13:00:00"),
  ReportText = c(
    "Patient seen for follow-up.\nHistory of UC diagnosed 2021.\nColonoscopy showed active proctitis.\nPlan medication adjustment.\nNo family history.",
    "Progress note.\nPatient denies symptoms.\nPrevious ulcerative colitis dx 2021 noted.\nContinue surveillance.",
    "New patient visit.\nScreening colonoscopy recommended.\nNo personal history of ibd.",
    "Consult note.\nDiscussion regarding treatment options.\nBiopsy confirmed Crohn's disease in 2023.",
    "Initial consult. Patient reports symptoms starting approx 2009. Diagnosis of Crohn's confirmed today.",
    "Follow up note. Patient doing well on medication. IBD stable since 2010 diagnosis."
  ),
  stringsAsFactors = FALSE
)

# Ensure correct data types (example, adjust as needed)
notes_data <- notes_data %>%
  mutate(
    PatientICN = as.character(PatientICN),
    EntryDateTime = suppressWarnings(ymd_hms(EntryDateTime, quiet = TRUE)), # Use lubridate
    ReportText = as.character(ReportText)
  ) %>%
  filter(!is.na(EntryDateTime)) # Remove rows with invalid dates

message("Loaded ", nrow(notes_data), " notes.")

# --- Helper Function: Merge Overlapping/Adjacent Line Ranges ---
merge_line_ranges <- function(lines_indices, last_line_index, expand_before, expand_after) {
  if (length(lines_indices) == 0) { return(list()) }
  lines_indices <- sort(unique(lines_indices))
  merged <- list(c(lines_indices[1] - expand_before, lines_indices[1] + expand_after))
  if (length(lines_indices) > 1) {
    for (i in 2:length(lines_indices)) {
      current_start <- lines_indices[i] - expand_before
      current_end <- lines_indices[i] + expand_after
      last_merged_range <- merged[[length(merged)]]
      if (current_start <= last_merged_range[2] + 1) {
        merged[[length(merged)]][2] <- max(last_merged_range[2], current_end)
      } else {
        merged <- c(merged, list(c(current_start, current_end)))
      }
    }
  }
  merged <- lapply(merged, function(range) { c(max(0, range[1]), min(last_line_index, range[2])) })
  return(merged)
}


# --- Main Processing Logic ---
excerpts_list <- list() # Stores list(date=..., text=..., priority=...) for each patient
seen_text_list <- list() # Tracks unique excerpt texts per patient

start_time <- Sys.time()
processed_count <- 0

for (i in 1:nrow(notes_data)) {
  note <- notes_data[i, ]
  patient_icn <- note$PatientICN
  report_text <- note$ReportText
  entry_date <- note$EntryDateTime

  report_text <- gsub("\r\n", "\n", report_text, fixed = TRUE)
  report_text <- gsub("\r", "\n", report_text, fixed = TRUE)

  matches_loc <- str_locate_all(report_text, myregex)[[1]]

  if (nrow(matches_loc) == 0) {
    processed_count <- processed_count + 1
    next
  }

  lines <- str_split(report_text, "\n")[[1]]
  n_lines <- length(lines)
  if (n_lines == 0) { processed_count <- processed_count + 1; next }

  line_lengths <- nchar(lines)
  line_offsets <- c(0, cumsum(line_lengths[-n_lines] + 1))

  match_lines_indices <- integer(0)
  for (j in 1:nrow(matches_loc)) {
    match_start <- matches_loc[j, "start"]
    match_end <- matches_loc[j, "end"]
    start_line_idx <- max(which(line_offsets <= match_start)) - 1
    end_line_search <- which(line_offsets >= match_end)
    end_line_idx <- if(length(end_line_search) == 0) n_lines - 1 else min(end_line_search) - 1
    if (start_line_idx <= end_line_idx) {
      match_lines_indices <- c(match_lines_indices, start_line_idx:end_line_idx)
    }
  }
  match_lines_indices <- unique(match_lines_indices)

  if (length(match_lines_indices) == 0) { processed_count <- processed_count + 1; next }

  blocks <- merge_line_ranges(match_lines_indices, n_lines - 1, lines_before, lines_after)

  if (!patient_icn %in% names(excerpts_list)) {
    excerpts_list[[patient_icn]] <- list()
    seen_text_list[[patient_icn]] <- character(0)
  }

  note_excerpts_count <- 0
  for (block in blocks) {
    if (note_excerpts_count >= max_excerpts_per_note) { break }

    start_line <- block[1] + 1
    end_line <- block[2] + 1
    excerpt_text <- paste(lines[start_line:end_line], collapse = "\n")
    excerpt_text_trimmed_lower <- tolower(trimws(excerpt_text))

    excludeMatch <- str_detect(excerpt_text, icdIgnore)
    is_duplicate <- excerpt_text_trimmed_lower %in% seen_text_list[[patient_icn]]

    if (!excludeMatch && !is_duplicate) {
      seen_text_list[[patient_icn]] <- c(seen_text_list[[patient_icn]], excerpt_text_trimmed_lower)
      priorityMatch <- str_detect(excerpt_text, priorityregex)

      # Store excerpt info including priority status
      excerpts_list[[patient_icn]] <- c(excerpts_list[[patient_icn]],
                                        list(list(date = entry_date,
                                                  text = excerpt_text,
                                                  priority = priorityMatch)))
      note_excerpts_count <- note_excerpts_count + 1
    }
  }

  processed_count <- processed_count + 1
  if (processed_count %% 1000 == 0) {
    current_time <- Sys.time()
    elapsed <- difftime(current_time, start_time, units = "secs")
    message(sprintf("Processed %d notes... Time elapsed: %.2f seconds", processed_count, elapsed))
  }
}

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")
message(sprintf("Finished processing all notes. Total time: %.2f seconds", total_time))


# --- Aggregate Excerpts and Create Final Dataframe ---

output_list <- list()

for (patient_icn in names(excerpts_list)) {
  patient_excerpts_info <- excerpts_list[[patient_icn]]
  n_total_excerpts <- length(patient_excerpts_info)

  if (n_total_excerpts == 0) { next }

  # Separate priority and non-priority excerpts
  is_priority <- sapply(patient_excerpts_info, `[[`, "priority")
  priority_excerpts <- patient_excerpts_info[is_priority]
  non_priority_excerpts <- patient_excerpts_info[!is_priority]
  n_priority <- length(priority_excerpts)
  n_non_priority <- length(non_priority_excerpts)

  # Sort non-priority by date (most recent first for sampling)
  if(n_non_priority > 0) {
    non_priority_excerpts <- non_priority_excerpts[order(sapply(non_priority_excerpts, `[[`, "date"), decreasing = TRUE)]
  }


  final_selected_excerpts <- list()

  if (n_total_excerpts <= excerpt_limit) {
    # Keep all excerpts if under limit
    final_selected_excerpts <- patient_excerpts_info
  } else {
    # Apply sampling logic

    # 1. Start with priority excerpts (up to the limit)
    n_priority_to_take <- min(n_priority, excerpt_limit)
    final_selected_excerpts <- priority_excerpts[1:n_priority_to_take] # Assumes priority list isn't pre-sorted if n_priority > excerpt_limit
    n_remaining_slots <- excerpt_limit - length(final_selected_excerpts)

    # 2. Add non-priority (recent, distant, random) if slots remain
    if (n_remaining_slots > 0 && n_non_priority > 0) {
      # Determine indices for recent and distant non-priority
      recent_indices <- if(n_non_priority >= n_most_recent) 1:n_most_recent else 1:n_non_priority
      distant_indices <- if(n_non_priority >= n_most_distant) (n_non_priority - n_most_distant + 1):n_non_priority else integer(0) # Handle case where n_non_priority < n_most_distant

      # Ensure distant indices are valid if list is short
      distant_indices <- distant_indices[distant_indices <= n_non_priority & distant_indices > 0]

      # Combine recent and distant, ensuring uniqueness
      guaranteed_non_priority_indices <- unique(c(recent_indices, distant_indices))
      n_guaranteed_non_priority <- length(guaranteed_non_priority_indices)

      # How many non-priority can we actually take?
      n_non_priority_to_take <- min(n_remaining_slots, n_non_priority)

      if (n_guaranteed_non_priority >= n_non_priority_to_take) {
        # If guaranteed are enough (or more than enough), take subset of guaranteed
        indices_to_take <- guaranteed_non_priority_indices[1:n_non_priority_to_take]
        final_selected_excerpts <- c(final_selected_excerpts, non_priority_excerpts[indices_to_take])
      } else {
        # If guaranteed are not enough, take all guaranteed and sample randomly from middle
        final_selected_excerpts <- c(final_selected_excerpts, non_priority_excerpts[guaranteed_non_priority_indices])
        n_random_needed <- n_non_priority_to_take - n_guaranteed_non_priority

        middle_indices <- setdiff(1:n_non_priority, guaranteed_non_priority_indices)

        if (length(middle_indices) > 0 && n_random_needed > 0) {
          n_random_to_sample <- min(n_random_needed, length(middle_indices))
          random_indices_sampled <- sample(middle_indices, n_random_to_sample)
          final_selected_excerpts <- c(final_selected_excerpts, non_priority_excerpts[random_indices_sampled])
        }
      }
    }
    # Ensure we didn't accidentally exceed the limit (shouldn't happen with min())
    if(length(final_selected_excerpts) > excerpt_limit) {
      final_selected_excerpts <- final_selected_excerpts[1:excerpt_limit]
    }
  }

  # Sort the final selection by date (oldest first)
  final_selected_excerpts_sorted <- final_selected_excerpts[order(sapply(final_selected_excerpts, `[[`, "date"))]

  # Format excerpts
  formatted_excerpts_text <- sapply(final_selected_excerpts_sorted, function(ex) {
    formatted_date <- format(ex$date, "%Y-%m")
    paste0("\n<<<\nNote date (YYYY-MM): ", formatted_date, "\nNote text:\n", ex$text, "\n>>>\n")
  })

  # Combine into final string
  patient_string <- paste(formatted_excerpts_text, collapse = "")
  patient_string <- paste0(patient_string, "\nQuestion: When was this patient originally diagnosed with IBD (Ulcerative colitis or Crohn's disease)?\n")

  # Add to output list
  output_list[[patient_icn]] <- list(PatientICN = patient_icn, AggregatedExcerpts = patient_string)
}

# Convert the list to a dataframe
output_df <- bind_rows(output_list)

# --- View Output Dataframe ---
message("Created final dataframe with ", nrow(output_df), " patients.")
if (nrow(output_df) > 0) {
  print(head(output_df))
  # write.csv(output_df, "aggregated_ibd_year_excerpts.csv", row.names = FALSE, quote = TRUE)
}

# --- Optional: Write separate ID and input files ---
# if (nrow(output_df) > 0) {
#   writeLines(output_df$PatientICN, "ptIDs_ibd_year_R.txt")
#   message("Wrote patient IDs to ptIDs_ibd_year_R.txt")
#   formatted_inputs_for_file <- gsub("\n", "\\\\n", output_df$AggregatedExcerpts)
#   writeLines(formatted_inputs_for_file, "input_ibd_year_R.txt")
#   message("Wrote formatted inputs to input_ibd_year_R.txt")
# }
