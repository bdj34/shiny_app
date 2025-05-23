# run shiny app

# Select the type of annotation. Options:
# crc: Colorectal Cancer using all sources (free text and path)
# ibd: IBD (free text and path domain)

# dysClass: dysplasiaClassifier (path reports)
# coloReport: colonoscopy reports (judge level of inflammation etc.)
# colonoscopyTiming: colonoscopy timing (to nearest month if possible)
# colectomy: colectomy timing and sections removed

# Answer here
annotation_type <- "crc"
reviewer <- "brian"
outDir <- "PATH_TO_OUT_DIR"

# Optionally change these
optional_blacklisted_pids <- "Optional: default is first 100 of input txt files" # Will also blacklist previously annotated
N_annotate <- 100
N_lines_before <- 20
N_lines_after <- 20

