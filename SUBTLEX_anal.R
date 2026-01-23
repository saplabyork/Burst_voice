# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

# Read the SUBTLEX Excel file
# Note: Replace 'subtlex.xlsx' with the actual path to your file
subtlex <- read_excel("/Users/chandannarayan/Desktop/SUBTLEXusExcel2007.xlsx")

# Step 1: Create initial corpus - words beginning with p,t,k,c followed by vowel
# Exclude words where c/C is followed by i
initial_corpus <- subtlex %>%
  filter(
    # Words starting with p,t,k,c (case insensitive) followed by vowel
    str_detect(Word, "^[PpTtKkCc][aeiouAEIOU]") &
      # Exclude c/C followed by i
      !str_detect(Word, "^[Cc][iI]")
  )

# Step 2: Filter for CVC words with specific criteria
cvc_corpus <- initial_corpus %>%
  filter(
    # CVC pattern where:
    # C1: p,k,c (case insensitive) - removed t
    # V: a,i,e (but not ee)
    # C2: p,t,k,b,d,g,ck
    str_detect(Word, "^[PpKkCc][aieAIE]([PpTtKkBbDdGg]|[Cc][Kk])$") &
      # Exclude words with "ee"
      !str_detect(Word, "[Ee][Ee]")
  ) %>%
  # Add columns for analysis
  mutate(
    # Extract onset consonant (first letter, lowercase)
    onset = str_to_lower(str_sub(Word, 1, 1)),
    # Extract vowel (second letter, lowercase)
    vowel = str_to_lower(str_sub(Word, 2, 2)),
    # Extract final consonant(s) for minimal pair detection
    final_consonant = case_when(
      str_detect(Word, "[Cc][Kk]$") ~ "ck",
      TRUE ~ str_to_lower(str_sub(Word, -1, -1))
    ),
    # Create stem for minimal pair detection (onset + vowel)
    stem = paste0(onset, vowel),
    # Determine place of articulation (only p and k/c now)
    place_of_articulation = case_when(
      onset == "p" ~ "Bilabial (p)",
      onset %in% c("k", "c") ~ "Velar (k/c)"
    ),
    # Convert frequency to percentage (per million to percentage)
    freq_percent = SUBTLWF / 10000  # Convert per million to percentage
  ) %>%
  # Only keep words that match our criteria
  filter(!is.na(place_of_articulation))

# Step 3: Identify voiced codas and prepare for plotting
cvc_corpus <- cvc_corpus %>%
  mutate(
    # Identify voiced codas
    has_voiced_coda = final_consonant %in% c("b", "d", "g"),
    # Create labels with colored final consonants
    display_word = Word,
    # Categorize by frequency for y-axis scaling
    freq_category = ifelse(freq_percent <= 0.01, "low", "high"),
    # Adjust high frequencies for compressed display
    display_freq = ifelse(freq_percent > 0.01, 
                          0.01 + (freq_percent - 0.01) * 0.1,  # Compress high frequencies
                          freq_percent)
  )

# Check if we have data
if(nrow(cvc_corpus) == 0) {
  stop("No words found matching the criteria. Please check the data and filtering conditions.")
}

# Print summary of corpus including voiced codas
cat("CVC Corpus Summary:\n")
cat("Total words found:", nrow(cvc_corpus), "\n")
cat("Words by place of articulation:\n")
print(table(cvc_corpus$place_of_articulation))
cat("\nWords by vowel:\n")
print(table(cvc_corpus$vowel))
cat("\nWords with voiced codas (b,d,g):\n")
print(table(cvc_corpus$has_voiced_coda))
cat("\nFrequency distribution:\n")
cat("Words with freq <= 0.01%:", sum(cvc_corpus$freq_percent <= 0.01), "\n")
cat("Words with freq > 0.01%:", sum(cvc_corpus$freq_percent > 0.01), "\n")

# Show examples of voiced coda words
voiced_coda_words <- cvc_corpus %>%
  filter(has_voiced_coda) %>%
  arrange(desc(freq_percent))
cat("\nExamples of voiced coda words:\n")
print(head(voiced_coda_words %>% select(Word, final_consonant, freq_percent), 10))

# Create the plot with voiced codas highlighted and broken y-axis
plot <- ggplot(cvc_corpus, aes(x = vowel, y = display_freq)) +
  # Create separate panels for each place of articulation
  facet_wrap(~ place_of_articulation, scales = "free_y") +
  # Add text labels for each word
  # Color words with voiced codas (b,d,g) in red, others in black
  geom_text(aes(label = Word, color = has_voiced_coda), 
            position = position_jitter(width = 0.2, height = 0),
            size = 4,  # Increased size
            alpha = 0.8) +
  # Set colors: red for voiced codas, black for others
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("TRUE" = "Voiced coda (b,d,g)", "FALSE" = "Voiceless coda"),
                     name = "Coda type") +
  # Custom y-axis with break
  scale_y_continuous(
    breaks = c(0, 0.005, 0.01, 0.015, 0.02),
    labels = function(x) {
      ifelse(x <= 0.01, 
             sprintf("%.3f", x),
             sprintf("%.3f", 0.01 + (x - 0.01) / 0.1))  # Convert back to original scale for labels
    },
    limits = c(0, max(cvc_corpus$display_freq) * 1.1)
  ) +
  # Add a visual break indicator
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # Customize the plot
  labs(
    title = "CVC Words: P- and K/C-initial with Voiced Codas Highlighted",
    subtitle = "Voiced codas (b,d,g) in red. Y-axis compressed above 0.01% (dashed line)",
    x = "Vowel",
    y = "Frequency (%)",
    caption = "Data from SUBTLEX corpus. Y-axis scale: 0-0.01% proportional, >0.01% compressed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size = 9)
  ) +
  # Set x-axis to show all vowels
  scale_x_discrete(limits = c("a", "i", "e"))

# Display the plot
print(plot)

# Create detailed summary table including voiced coda information
summary_table <- cvc_corpus %>%
  group_by(place_of_articulation, vowel, has_voiced_coda) %>%
  summarise(
    word_count = n(),
    words = paste(Word, collapse = ", "),
    avg_frequency = round(mean(freq_percent), 4),
    total_frequency = round(sum(freq_percent), 4),
    .groups = "drop"
  ) %>%
  arrange(place_of_articulation, vowel, desc(has_voiced_coda))

cat("\nDetailed Summary by Place of Articulation, Vowel, and Coda Type:\n")
print(summary_table)

# Summary of voiced vs voiceless codas
coda_summary <- cvc_corpus %>%
  group_by(has_voiced_coda) %>%
  summarise(
    count = n(),
    avg_freq = round(mean(freq_percent), 4),
    median_freq = round(median(freq_percent), 4),
    max_freq = round(max(freq_percent), 4),
    .groups = "drop"
  ) %>%
  mutate(coda_type = ifelse(has_voiced_coda, "Voiced (b,d,g)", "Voiceless (p,t,k,ck)"))

cat("\nVoiced vs Voiceless Coda Summary:\n")
print(coda_summary)

# Optional: Save the plot
# Uncomment the line below to save the plot
# ggsave("cvc_words_plot.png", plot, width = 12, height = 8, dpi = 300)

# Optional: Save the filtered corpus
# Uncomment the line below to save the corpus as CSV
# write.csv(cvc_corpus, "cvc_corpus.csv", row.names = FALSE)