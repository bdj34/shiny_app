# Shiny Applications for Chart Review of LLM-Generated IBD and CRC Phenotypes

This repository contains R Shiny applications designed to facilitate the chart review of large language model (LLM)-generated phenotypes for Inflammatory Bowel Disease (IBD) and Colorectal Cancer (CRC). These applications assist in the structured review of pathology and colonoscopy reports, as well as free-text diagnoses extracted from clinical notes.

## Features

- **Interactive Review Interface**: User-friendly Shiny apps for reviewing and annotating clinical text data.
- **Annotation Tools**: Capabilities to label phenotypes, lesions, dysplasia, and diagnostic features.
- **Data Export**: Export annotated data to CSV files for downstream analysis.

## Repository Contents

- `colo_path_report_app.R`: Main Shiny application script for reviewing colonoscopy and pathology reports.
- `review_app_crc.R`: Shiny app tailored for reviewing CRC-related data.
- `review_app_ibd.R`: Shiny app tailored for reviewing IBD-related data.

## Installation and Usage

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/bdj34/shiny_app.git
   cd shiny_app
   ```

2. **Install Required R Packages**:

   Ensure you have the necessary R packages installed:

   ```r
   install.packages(c("shiny", "DT", "readr", "dplyr", "stringr", "shinydashboard"))
   ```

3. **Run the Shiny Application**:

   Depending on the review task, run the appropriate Shiny app:

   - For colonoscopy and pathology review:

     ```r
     shiny::runApp("colo_path_report_app.R")
     ```

   - For CRC data review:

     ```r
     shiny::runApp("review_app_crc.R")
     ```

   - For IBD data review:

     ```r
     shiny::runApp("review_app_ibd.R")
     ```

## Contributing

Contributions are welcome! If you have suggestions for improvements or encounter any issues, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License.
