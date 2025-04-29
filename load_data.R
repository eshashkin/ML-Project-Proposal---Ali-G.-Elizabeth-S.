# -------------------------------
# 1. INSTALL + LOAD LIBRARIES
# -------------------------------
install.packages(c("ipumsr", "dplyr", "sf", "tidyr", "ggplot2", "readr"))
library(ipumsr)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(readr)

# -------------------------------
# 2. LOAD IPUMS MICRODATA
# -------------------------------
setwd("~/Desktop/spring 2025/ml/ml_group_project/data/raw/acs_pums")
ddi <- read_ipums_ddi("usa_00004.xml")
data <- read_ipums_micro(ddi)
write.csv(data, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/data_original.csv", row.names = FALSE)

# -------------------------------
# 3. FILTER FOR FOREIGN-BORN ADULTS
# -------------------------------
data_filtered <- data %>%
  filter(BPL >= 100, AGE >= 18, AGE <= 64, !is.na(PUMA), !is.na(PERWT)) %>%
  mutate(PUMA = as.numeric(PUMA), STATEFIP = as.numeric(STATEFIP))
write.csv(data_filtered, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/data_filtered.csv", row.names = FALSE)

# -------------------------------
# 4. DEFINE EXTENDED BPL → COUNTRY → LANGUAGE MAPPING
# -------------------------------
bpl_language_map <- tibble::tibble(
  BPL = c(200, 210, 250, 300, 310, 325, 350, 400, 500, 600, 640, 651, 711, 830, 850, 862, 865, 867, 870, 900, 910, 915,
          920, 930, 950, 951, 960, 970, 99999, 260, 515, 518, 521, 110, 511, 514, 513, 522, 523, 524, 525),
  Country = c("Afghanistan", "Iran", "India", "China", "Hong Kong", "Mexico", "France", "United Kingdom", "Pakistan", "Egypt",
              "Nigeria", "Ghana", "Somalia", "Vietnam", "Philippines", "Myanmar", "Thailand", "Bangladesh", "Nepal", "Russia",
              "Ukraine", "Poland", "Germany", "Portugal", "Ethiopia", "Eritrea", "Congo", "Afghanistan", "Other",
              "Colombia", "El Salvador", "Cuba", "Guatemala", "Dominican Republic", "Honduras", "Nicaragua", "Mexico (Other)",
              "Haiti", "Brazil", "Jamaica", "Trinidad and Tobago"),
  Language = c("Dari", "Persian", "Hindi", "Mandarin", "Cantonese", "Spanish", "French", "English", "Urdu", "Arabic",
               "Yoruba", "Akan", "Somali", "Vietnamese", "Tagalog", "Burmese", "Thai", "Bengali", "Nepali", "Russian",
               "Ukrainian", "Polish", "German", "Portuguese", "Amharic", "Tigrinya", "Swahili", "Pashto", "Other",
               "Spanish", "Spanish", "Spanish", "Spanish", "Spanish", "Spanish", "Spanish", "Spanish",
               "Haitian Creole", "Portuguese", "English", "English")
)
write.csv(bpl_language_map, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/bpl_list_filled.csv", row.names = FALSE)

# -------------------------------
# 5. JOIN TO MAIN DATA
# -------------------------------
data_lang <- data_filtered %>%
  left_join(bpl_language_map, by = "BPL") %>%
  filter(!is.na(Language))

# -------------------------------
# 6. EXTRACT USED LANGUAGES
# -------------------------------
languages_used <- data_lang %>% distinct(Language) %>% arrange(Language)
write.csv(languages_used, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/used_languages.csv", row.names = FALSE)

# -------------------------------
# 7. BUILD LANGUAGE SIMILARITY MATRIX
# -------------------------------
unique_langs <- languages_used$Language
lang_sim_df <- expand.grid(Language_A = unique_langs, Language_B = unique_langs) %>%
  mutate(Similarity = ifelse(Language_A == Language_B, 1,
                             case_when(
                               Language_A %in% c("Hindi", "Urdu") & Language_B %in% c("Hindi", "Urdu") ~ 0.85,
                               Language_A %in% c("Arabic", "Dari", "Pashto") & Language_B %in% c("Arabic", "Dari", "Pashto") ~ 0.6,
                               Language_A %in% c("Amharic", "Tigrinya", "Swahili") & Language_B %in% c("Amharic", "Tigrinya", "Swahili") ~ 0.6,
                               Language_A %in% c("Russian", "Ukrainian", "Polish") & Language_B %in% c("Russian", "Ukrainian", "Polish") ~ 0.5,
                               Language_A %in% c("French", "Spanish", "Portuguese") & Language_B %in% c("French", "Spanish", "Portuguese") ~ 0.5,
                               TRUE ~ 0.3)))
write.csv(lang_sim_df, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/language_similarity_matrix_strict.csv", row.names = FALSE)

# -------------------------------
# 8. CALCULATE GROUP SHARES
# -------------------------------
group_counts <- data_lang %>%
  group_by(PUMA, STATEFIP, Language) %>%
  summarise(N_bc = sum(PERWT), .groups = "drop")

total_counts <- data_lang %>%
  group_by(PUMA, STATEFIP) %>%
  summarise(N_c = sum(PERWT), .groups = "drop")

group_shares <- group_counts %>%
  left_join(total_counts, by = c("PUMA", "STATEFIP")) %>%
  mutate(p_bc = N_bc / N_c)
write.csv(group_shares, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/group_shares.csv", row.names = FALSE)

# -------------------------------
# 9. LANGUAGE PROXIMITY INDEX
# -------------------------------
lang_prox_raw <- data_lang %>%
  select(PERWT, PUMA, STATEFIP, Language_A = Language) %>%
  left_join(group_shares, by = c("PUMA", "STATEFIP")) %>%
  rename(Language_B = Language) %>%
  left_join(lang_sim_df, by = c("Language_A", "Language_B")) %>%
  mutate(weighted = p_bc * Similarity)

lang_prox_score <- lang_prox_raw %>%
  group_by(PERWT, STATEFIP, PUMA, Language_A) %>%
  summarise(LanguageProximity = sum(weighted, na.rm = TRUE), .groups = "drop")

data_final <- data_lang %>%
  left_join(lang_prox_score, by = c("PERWT", "STATEFIP", "PUMA", "Language" = "Language_A"))
write.csv(data_final, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/data_final.csv", row.names = FALSE)

# -------------------------------
# 10. AGGREGATE TO PUMA LEVEL
# -------------------------------
puma_summary_data <- data_final %>%
  group_by(STATEFIP, PUMA) %>%
  summarise(avg_langprox = weighted.mean(LanguageProximity, PERWT, na.rm = TRUE), .groups = "drop")
write.csv(puma_summary_data, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/puma_language_prox_summary.csv", row.names = FALSE)

# -------------------------------
# 11. TOP LANGUAGES BY PUMA
# -------------------------------
top_langs <- data_lang %>%
  group_by(STATEFIP, PUMA, Language) %>%
  summarise(N = sum(PERWT), .groups = "drop") %>%
  group_by(STATEFIP, PUMA) %>%
  slice_max(N, n = 4) %>%
  mutate(rank = row_number()) %>%
  pivot_wider(names_from = rank, values_from = Language, names_prefix = "TopLang_")
write.csv(top_langs, "~/Desktop/spring 2025/ml/ml_group_project/data/processed/puma_top_languages.csv", row.names = FALSE)

