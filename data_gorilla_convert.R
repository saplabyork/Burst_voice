# Configuration
input_file <- read.csv("/Users/chandan/Desktop/GitHub/Burst_voice/Data/full_data.csv", header=TRUE)  # Change this to your CSV filename
output_file <- "final_data.csv"    # Change this to desired output filename

# Specify columns to REMOVE using Excel-style letters (A, B, C, etc.)
# For columns A-L, N-AE, AG-AJ, AL-AR, AT-BA, BJ-BQ
columns_to_remove <- c(
  LETTERS[1:12],           # A-L
  "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",  # N-Z
  "AA", "AB", "AC", "AD", "AE",  # AA-AE
  "AG", "AH", "AI", "AJ",        # AG-AJ
  "AL", "AM", "AN", "AO", "AP", "AQ", "AR",  # AL-AR
  "AT", "AU", "AV", "AW", "AX", "AY", "AZ", "BA",  # AT-BA
  "BK", "BL", "BM", "BN", "BO", "BP", "BQ"   # BK-BQ
)
# Function to convert Excel column letters to column indices (A=1, B=2, C=3, etc.)
excel_col_to_index <- function(col_letter) {
  result <- 0
  for (char in strsplit(toupper(col_letter), "")[[1]]) {
    result <- result * 26 + (utf8ToInt(char) - utf8ToInt("A") + 1)
  }
  return(result)
}

# Read the CSV file
df <- input_file

# Get the indices of columns to remove
indices_to_remove <- sapply(columns_to_remove, excel_col_to_index)

# Remove the columns by index
df <- df[, -indices_to_remove]

# Save to new CSV file
write.csv(df, output_file, row.names = FALSE)
