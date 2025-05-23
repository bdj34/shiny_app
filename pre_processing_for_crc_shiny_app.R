# Load necessary libraries
library(stringr) # For string manipulation and regex
library(dplyr)   # For data manipulation verbs
library(purrr)   # For functional programming tools like map
library(lubridate) # Potentially needed if more complex date handling is required

# --- Configuration --- EDIT THESE FIELDS ---

# Regex for finding relevant mentions (case-insensitive)
# Note: R uses slightly different regex syntax sometimes, but PCRE should work with stringr
myregex <- "(?i)((colon|colorectal|rectal)\\s+(adenocarcinoma|carcinoma|cancer))|crc"

# Regex for ignoring excerpts containing these terms (case-insensitive)
regexIgnore <- "(?i)(surveillance|screen|detection|prevent|family\\s+(history|hx))"

# Context extraction parameters
lines_before <- 5
lines_after <- 5
excerpt_limit <- 50        # Max excerpts to include per patient in the final output
n_most_recent <- 15       # Number of most recent excerpts to guarantee inclusion
n_most_distant <- 10      # Number of earliest excerpts to guarantee inclusion
max_excerpts_per_note <- 10 # Max excerpts to extract from a single note

# --- Data Loading ---
# !!! REPLACE THIS SECTION with your actual SQL data loading logic !!!
# Example using a placeholder dataframe:
# Ensure your loaded data has columns like PatientICN, EntryDateTime, ReportText
# notes_data <- dbGetQuery(con, "SELECT PatientICN, EntryDateTime, ReportText FROM YourNotesTable WHERE ...")
notes_data <- data.frame(
  PatientICN = c("PAT1", "PAT1", "PAT2", "PAT3"),
  EntryDateTime = c("2023-01-15 10:00:00", "2023-05-20 14:30:00", "2022-11-01 08:15:00", "2024-02-10 11:00:00"),
  ReportText = c(
    "Patient seen for follow-up.\nHistory of CRC diagnosed 2021.\nColonoscopy showed recurrence in the descending colon.\nPlan for surgery.\nNo family history.",
    "Progress note.\nPatient denies symptoms.\nPrevious adenocarcinoma noted.\nContinue surveillance.",
    "New patient visit.\nScreening colonoscopy recommended.\nNo personal history of crc.",
    "Consult note.\nDiscussion regarding colorectal cancer treatment options.\nBiopsy confirmed adenocarcinoma."
  ),
  stringsAsFactors = FALSE
)

# Ensure correct data types (example, adjust as needed)
notes_data <- notes_data %>%
  mutate(
    PatientICN = as.character(PatientICN),
    # Convert EntryDateTime to POSIXct if it's not already
    # Handle potential parsing errors gracefully
    EntryDateTime = suppressWarnings(ymd_hms(EntryDateTime, quiet = TRUE)),
    ReportText = as.character(ReportText)
  ) %>%
  filter(!is.na(EntryDateTime)) # Remove rows with invalid dates

message("Loaded ", nrow(notes_data), " notes.")

# --- Helper Function: Merge Overlapping/Adjacent Line Ranges ---
# Equivalent to Python's merge_indices
merge_line_ranges <- function(lines_indices, last_line_index, expand_before, expand_after) {
  if (length(lines_indices) == 0) {
    return(list())
  }
  lines_indices <- sort(unique(lines_indices))

  # Initial range
  merged <- list(c(lines_indices[1] - expand_before, lines_indices[1] + expand_after))

  if (length(lines_indices) > 1) {
    for (i in 2:length(lines_indices)) {
      current_start <- lines_indices[i] - expand_before
      current_end <- lines_indices[i] + expand_after
      last_merged_range <- merged[[length(merged)]]

      # Check if current range overlaps or is adjacent to the last merged range
      # Condition: current start is less than or equal to last end + 1
      # (We add 1 because lines_after on the previous and lines_before on the current might touch)
      if (current_start <= last_merged_range[2] + 1) {
        # Merge: Update the end of the last merged range
        merged[[length(merged)]][2] <- max(last_merged_range[2], current_end)
      } else {
        # No overlap: Start a new range
        merged <- c(merged, list(c(current_start, current_end)))
      }
    }
  }

  # Adjust bounds to be within 0 and last_line_index
  merged <- lapply(merged, function(range) {
    c(max(0, range[1]), min(last_line_index, range[2]))
  })

  return(merged)
}


# --- Main Processing Logic ---

# Use a list to store excerpts for each patient
excerpts_list <- list()
seen_text_list <- list() # To track unique excerpts per patient

start_time <- Sys.time()
processed_count <- 0

# Iterate through each note (row in the dataframe)
for (i in 1:nrow(notes_data)) {
  note <- notes_data[i, ]
  patient_icn <- note$PatientICN
  report_text <- note$ReportText
  entry_date <- note$EntryDateTime # Keep as POSIXct for sorting

  # Basic cleaning
  report_text <- stringr::str_replace_all(report_text, "\r\n", "\n")
  report_text <- stringr::str_replace_all(report_text, "\r", "\n")

  # Find matches using stringr
  matches_loc <- str_locate_all(report_text, myregex)[[1]] # Returns matrix of start/end positions

  if (nrow(matches_loc) == 0) {
    processed_count <- processed_count + 1
    next # Skip note if no primary matches
  }

  # Split text into lines
  lines <- str_split(report_text, "\n")[[1]]
  n_lines <- length(lines)
  if (n_lines == 0) {
    processed_count <- processed_count + 1
    next # Skip if note is empty after splitting
  }

  # Calculate character offsets for each line start
  line_lengths <- nchar(lines)
  # Offset is 0 for first line, then cumulative sum of lengths + 1 (for newline char)
  line_offsets <- c(0, cumsum(line_lengths[-n_lines] + 1))

  # Find which lines contain parts of any match
  match_lines_indices <- integer(0)
  for (j in 1:nrow(matches_loc)) {
    match_start <- matches_loc[j, "start"]
    match_end <- matches_loc[j, "end"]

    # Find line index where match starts (line number is index + 1)
    start_line_idx <- max(which(line_offsets <= match_start)) - 1 # 0-based index
    # Find line index where match ends
    # Need to handle case where match ends exactly at end of line or file
    end_line_search <- which(line_offsets >= match_end)
    if(length(end_line_search) == 0){
      # Match ends after the start of the last line
      end_line_idx <- n_lines -1
    } else {
      end_line_idx <- min(end_line_search) -1
    }


    # Add all lines from start_line to end_line to the set
    if (start_line_idx <= end_line_idx) {
      match_lines_indices <- c(match_lines_indices, start_line_idx:end_line_idx)
    }
  }
  match_lines_indices <- unique(match_lines_indices)

  if (length(match_lines_indices) == 0) {
    processed_count <- processed_count + 1
    next # Should not happen if matches_loc had rows, but safety check
  }

  # Get merged blocks of lines to extract
  # Use n_lines - 1 as the last line index (0-based)
  blocks <- merge_line_ranges(match_lines_indices, n_lines - 1, lines_before, lines_after)

  # Initialize patient in lists if first time seen
  if (!patient_icn %in% names(excerpts_list)) {
    excerpts_list[[patient_icn]] <- list()
    seen_text_list[[patient_icn]] <- character(0) # Use character vector as a set
  }

  # Extract excerpts for the current note
  note_excerpts_count <- 0
  for (block in blocks) {
    if (note_excerpts_count >= max_excerpts_per_note) {
      break # Stop if max excerpts for this note is reached
    }

    start_line <- block[1] + 1 # Convert back to 1-based index for subsetting lines
    end_line <- block[2] + 1
    excerpt_text <- paste(lines[start_line:end_line], collapse = "\n")
    excerpt_text_trimmed_lower <- tolower(trimws(excerpt_text))

    # Check if excerpt should be ignored based on regexIgnore
    excludeMatch <- str_detect(excerpt_text, regexIgnore)

    # Check if excerpt is duplicate for this patient
    is_duplicate <- excerpt_text_trimmed_lower %in% seen_text_list[[patient_icn]]

    if (!excludeMatch && !is_duplicate) {
      # Add to seen text
      seen_text_list[[patient_icn]] <- c(seen_text_list[[patient_icn]], excerpt_text_trimmed_lower)

      # Store excerpt with its date
      excerpts_list[[patient_icn]] <- c(excerpts_list[[patient_icn]],
                                        list(list(date = entry_date, text = excerpt_text)))
      note_excerpts_count <- note_excerpts_count + 1
    }
  }

  processed_count <- processed_count + 1
  if (processed_count %% 1000 == 0) { # Print progress less frequently
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

  if (length(patient_excerpts_info) == 0) {
    next # Skip patients with no valid excerpts
  }

  # Sort excerpts by date (most recent first for potential sampling)
  # Order by POSIXct date objects
  patient_excerpts_info <- patient_excerpts_info[order(sapply(patient_excerpts_info, `[[`, "date"), decreasing = TRUE)]

  selected_excerpts_text <- character(length(patient_excerpts_info)) # Preallocate character vector

  if (length(patient_excerpts_info) <= excerpt_limit) {
    # Include all excerpts if below limit, sort by date (oldest first for final output)
    patient_excerpts_info_sorted <- patient_excerpts_info[order(sapply(patient_excerpts_info, `[[`, "date"))]
    # Format each excerpt
    selected_excerpts_text <- sapply(patient_excerpts_info_sorted, function(ex) {
      formatted_date <- format(ex$date, "%Y-%m") # Format date as YYYY-MM
      paste0("\n<<<\nNote date (YYYY-MM): ", formatted_date, "\nNote text:\n", ex$text, "\n>>>\n")
    })

  } else {
    # Apply sampling logic: n most recent, n most distant, random sample from middle
    n_total <- length(patient_excerpts_info)
    most_recent_indices <- 1:n_most_recent
    most_distant_indices <- (n_total - n_most_distant + 1):n_total
    middle_indices <- setdiff(1:n_total, c(most_recent_indices, most_distant_indices))

    n_needed_random <- excerpt_limit - n_most_recent - n_most_distant
    # Ensure we don't request more random samples than available
    n_to_sample <- min(n_needed_random, length(middle_indices))

    random_indices_sampled <- if(n_to_sample > 0) {
      sample(middle_indices, n_to_sample)
    } else {
      integer(0)
    }


    final_indices <- unique(c(most_recent_indices, most_distant_indices, random_indices_sampled))

    # Get the selected excerpts and sort them by date (oldest first for final output)
    selected_excerpts_info_sorted <- patient_excerpts_info[final_indices]
    selected_excerpts_info_sorted <- selected_excerpts_info_sorted[order(sapply(selected_excerpts_info_sorted, `[[`, "date"))]

    # Format each selected excerpt
    selected_excerpts_text <- sapply(selected_excerpts_info_sorted, function(ex) {
      formatted_date <- format(ex$date, "%Y-%m") # Format date as YYYY-MM
      paste0("\n<<<\nNote date (YYYY-MM): ", formatted_date, "\nNote text:\n", ex$text, "\n>>>\n")
    })
  }

  # Combine excerpts into a single string for the patient
  patient_string <- paste(selected_excerpts_text, collapse = "")
  # Add the final question
  patient_string <- paste0(patient_string, "\nQuestion: Has this patient been diagnosed with colorectal cancer (CRC)? If so, what was the year and month of CRC diagnosis?\n")

  # Add to output list
  output_list[[patient_icn]] <- list(PatientICN = patient_icn, AggregatedExcerpts = patient_string)

}

# Convert the list to a dataframe
output_df <- bind_rows(output_list)

# --- View Output Dataframe ---
message("Created final dataframe with ", nrow(output_df), " patients.")
if (nrow(output_df) > 0) {
  print(head(output_df))
  # You can now write this dataframe to a file if needed, e.g.:
  # write.csv(output_df, "aggregated_excerpts.csv", row.names = FALSE, quote = TRUE)
}

# --- Optional: Write separate ID and input files (like Python script) ---
# if (nrow(output_df) > 0) {
#   # Write PatientICNs
#   writeLines(output_df$PatientICN, "ptIDs_R.txt")
#   message("Wrote patient IDs to ptIDs_R.txt")
#
#   # Write aggregated excerpts (replace internal newlines for file output if needed)
#   # The Python script replaced \n with \\n, which might be needed for specific downstream tools.
#   # If writing to a text file where each line is one patient's data:
#   formatted_inputs_for_file <- gsub("\n", "\\\\n", output_df$AggregatedExcerpts) # Escape newlines
#   writeLines(formatted_inputs_for_file, "input_R.txt")
#   message("Wrote formatted inputs to input_R.txt")
# }

