# ============================================================
# SECTION 1: LIBRARIES AND PATHS
# ============================================================
install.packages("ggrepel")
library(readxl)
library(ggrepel)
library(tidyverse)
base <- "C:/Users/HP/Downloads/AEI_Research"

# ============================================================
# SECTION 2: LOAD ALL RAW FILES
# ============================================================
# We load everything up front so the rest of the script is
# pure transformation with no hidden I/O.

# O*NET task statements — the master task-occupation universe.
# This is our denominator source. Used from Release 1 because
# the O*NET taxonomy itself doesn't change between releases.
onet_tasks <- read_csv(
  file.path(base, "data/release_1_feb2025/onet_task_statements.csv"),
  show_col_types = FALSE
)

# Release 4 API data — our primary adoption signal.
# Date window: Nov 13–20, 2025.
r4_api <- read_csv(
  file.path(base, "data/release_4_jan2026/aei_raw_1p_api_2025-11-13_to_2025-11-20.csv"),
  show_col_types = FALSE
)

# Release 4 Claude.ai — kept for robustness check only.
# Not used in primary adoption measure.
r4_claude <- read_csv(
  file.path(base, "data/release_4_jan2026/aei_raw_claude_ai_2025-11-13_to_2025-11-20.csv"),
  show_col_types = FALSE
)

# O*NET Consequence of Error. 
# Columns: Code (O*NET-SOC code), Context (COE score 1-100).
coe_raw <- read_csv(
  file.path(base, "data/onet/consequence_of_error.csv"),
  show_col_types = FALSE
)

# BLS employment — used only for bubble sizing in scatter plots.
# skip=1 drops the header row that has no SOC code.
bls_raw <- read_csv(
  file.path(base, "data/release_1_feb2025/bls_employment_may_2023.csv"),
  col_names = c("occupation", "bls_employment"),
  skip = 1,
  show_col_types = FALSE
) %>%
  mutate(bls_employment = as.numeric(bls_employment))

# SOC structure — needed to attach major group codes to BLS names.
soc_structure <- read_csv(
  file.path(base, "data/release_1_feb2025/SOC_Structure.csv"),
  show_col_types = FALSE
)

# Verification prints
cat("ONET tasks rows:", nrow(onet_tasks), "\n")
cat("R4 API rows:", nrow(r4_api), "\n")
cat("R4 Claude rows:", nrow(r4_claude), "\n")
cat("COE rows:", nrow(coe_raw), "\n")
cat("BLS rows:", nrow(bls_raw), "\n")

# ============================================================
# SECTION 3: BUILD O*NET TASK UNIVERSE — THE DENOMINATOR
# ============================================================
# For each 6-digit SOC code, we count how many tasks O*NET
# assigns to it. This count is our denominator — it is 
# entirely independent of Anthropic data, which eliminates
# the selection bias in the old api_share measure.
#
# O*NET codes are 8-digit format: "11-1011.00"
# 6-digit SOC strips the decimal suffix:  "11-1011"
# We do this with str_remove(`O*NET-SOC Code`, "\\.\\d+$")

onet_universe <- onet_tasks %>%
  filter(!is.na(`Task Type`)) %>%          # drop the 658 NA rows only
  mutate(
    task_clean = tolower(str_trim(Task)),
    soc_6digit = str_remove(`O*NET-SOC Code`, "\\.\\d+$")
  ) %>%
  select(soc_6digit, task_clean)

onet_task_counts <- onet_universe %>%
  group_by(soc_6digit) %>%
  summarise(
    total_onet_tasks = n_distinct(task_clean),
    .groups = "drop"
  )

cat("6-digit SOC codes in O*NET:", nrow(onet_task_counts), "\n")
cat("Unique tasks in O*NET:", n_distinct(onet_universe$task_clean), "\n")
cat("Avg tasks per occupation:", round(mean(onet_task_counts$total_onet_tasks), 1), "\n")
cat("Range of task counts:", min(onet_task_counts$total_onet_tasks),
    "to", max(onet_task_counts$total_onet_tasks), "\n")

# Find the occupation with 265 tasks — just for awareness
onet_task_counts %>%
  slice_max(total_onet_tasks, n = 5) %>%
  left_join(
    onet_tasks %>%
      mutate(soc_6digit = str_remove(`O*NET-SOC Code`, "\\.\\d+$")) %>%
      distinct(soc_6digit, Title),
    by = "soc_6digit"
  )
# That makes complete sense and is not a problem. 15-1199 is "Computer 
# Occupations, All Other" — O*NET's catch-all code for tech roles that 
# don't fit a specific 6-digit bucket.

# ============================================================
# SECTION 4: EXTRACT RELEASE 4 API TASK LIST
# ============================================================
# We pull the task-level usage from the API data.
# Each row is a task name and its share of total API traffic.
# We keep the pct for later use but our adoption measure
# will be based on task presence, not pct magnitude.
# "none" and "not_classified" are Anthropic's null categories
# — tasks the classifier couldn't assign to an O*NET task.

r4_api_tasks <- r4_api %>%
  filter(
    facet      == "onet_task",
    variable   == "onet_task_pct",
    geography  == "global"
  ) %>%
  select(task_name = cluster_name, api_pct = value) %>%
  filter(!task_name %in% c("none", "not_classified")) %>%
  mutate(task_name = tolower(str_trim(task_name)))

cat("API tasks in Release 4:", nrow(r4_api_tasks), "\n")
cat("API pct sum:", round(sum(r4_api_tasks$api_pct), 3), "\n")
cat("Sample tasks:\n")
print(head(r4_api_tasks, 5))

r4_claude_tasks <- r4_claude %>%
  filter(
    facet      == "onet_task",
    variable   == "onet_task_pct",
    geography  == "global"
  ) %>%
  select(task_name = cluster_name, claude_pct = value) %>%
  filter(!task_name %in% c("none", "not_classified")) %>%
  mutate(task_name = tolower(str_trim(task_name)))

cat("Claude tasks in Release 4:", nrow(r4_claude_tasks), "\n")

# ============================================================
# SECTION 5: BRIDGE, EQUAL WEIGHTING, ADOPTION RATE
# ============================================================

# Step 1: Bridge — match API tasks to all SOC codes via O*NET
api_onet_bridge <- r4_api_tasks %>%
  inner_join(onet_universe, by = c("task_name" = "task_clean"))

matched_tasks   <- n_distinct(api_onet_bridge$task_name)
total_api_tasks <- n_distinct(r4_api_tasks$task_name)

cat("API tasks matched to O*NET:", matched_tasks,
    "of", total_api_tasks,
    "(", round(matched_tasks / total_api_tasks * 100, 1), "%)\n")
cat("SOC codes touched by API tasks:", n_distinct(api_onet_bridge$soc_6digit), "\n")

# Step 2: Equal weighting
# Each task contributes 1/N to each of the N occupations it appears in
api_onet_weighted <- api_onet_bridge %>%
  group_by(task_name) %>%
  mutate(
    n_occupations = n_distinct(soc_6digit),
    task_weight   = 1 / n_occupations
  ) %>%
  ungroup()

cat("Avg occupations per task:",
    round(mean(
      api_onet_weighted %>%
        distinct(task_name, n_occupations) %>%
        pull(n_occupations)
    ), 2), "\n")

cat("Tasks appearing in only 1 occupation:",
    sum(api_onet_weighted %>%
          distinct(task_name, n_occupations) %>%
          pull(n_occupations) == 1), "\n")

cat("Tasks appearing in 10+ occupations:",
    sum(api_onet_weighted %>%
          distinct(task_name, n_occupations) %>%
          pull(n_occupations) >= 10), "\n")

# Step 3: Aggregate to occupation level
occ_adoption <- api_onet_weighted %>%
  group_by(soc_6digit) %>%
  summarise(
    api_tasks_weighted = sum(task_weight),
    n_matched_tasks    = n_distinct(task_name),
    .groups = "drop"
  ) %>%
  inner_join(onet_task_counts, by = "soc_6digit") %>%
  mutate(
    adoption_rate = api_tasks_weighted / total_onet_tasks
  )

cat("\nOccupations with adoption data:", nrow(occ_adoption), "\n")
cat("Adoption rate — mean:", round(mean(occ_adoption$adoption_rate), 3), "\n")
cat("Adoption rate — median:", round(median(occ_adoption$adoption_rate), 3), "\n")
cat("Adoption rate — range:",
    round(min(occ_adoption$adoption_rate), 3), "to",
    round(max(occ_adoption$adoption_rate), 3), "\n")

# Distribution check
cat("\nAdoption rate distribution:\n")
print(quantile(occ_adoption$adoption_rate, 
               probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

# Add zero-adoption occupations back in
# These are O*NET occupations with no API task matches in R4
# They are real data points, not missing data

occ_adoption_full <- onet_task_counts %>%
  left_join(
    occ_adoption %>% select(soc_6digit, api_tasks_weighted, 
                            n_matched_tasks, adoption_rate),
    by = "soc_6digit"
  ) %>%
  mutate(
    api_tasks_weighted = replace_na(api_tasks_weighted, 0),
    n_matched_tasks    = replace_na(n_matched_tasks, 0),
    adoption_rate      = replace_na(adoption_rate, 0)
  )

cat("Total occupations (including zeros):", nrow(occ_adoption_full), "\n")
cat("Occupations with zero adoption:", 
    sum(occ_adoption_full$adoption_rate == 0), "\n")
cat("Occupations with any adoption:", 
    sum(occ_adoption_full$adoption_rate > 0), "\n")

cat("\nUpdated adoption rate distribution:\n")
print(quantile(occ_adoption_full$adoption_rate,
               probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)))

occ_adoption_full %>%
  filter(adoption_rate == 0) %>%
  mutate(soc_major = str_sub(soc_6digit, 1, 2)) %>%
  count(soc_major) %>%
  arrange(desc(n)) %>%
  left_join(
    onet_tasks %>%
      mutate(soc_major = str_sub(`O*NET-SOC Code`, 1, 2)) %>%
      distinct(soc_major, Title) %>%
      group_by(soc_major) %>%
      slice(1),
    by = "soc_major"
  )

# ============================================================
# SECTION 6: PROCESS CONSEQUENCE OF ERROR (COE)
# ============================================================

coe_clean <- coe_raw %>%
  rename(coe_score = Context, onet_code = Code) %>%
  mutate(
    coe_score  = as.numeric(coe_score),
    soc_6digit = str_remove(onet_code, "\\.\\d+$")
  ) %>%
  filter(!is.na(coe_score))

# Aggregate to 6-digit SOC — average across O*NET sub-codes
coe_6digit <- coe_clean %>%
  group_by(soc_6digit) %>%
  summarise(
    coe_score    = mean(coe_score, na.rm = TRUE),
    n_onet_codes = n(),
    .groups = "drop"
  )

cat("6-digit SOC codes with COE:", nrow(coe_6digit), "\n")
cat("COE range:", round(min(coe_6digit$coe_score), 1),
    "to", round(max(coe_6digit$coe_score), 1), "\n")
cat("COE mean:", round(mean(coe_6digit$coe_score), 1), "\n")
cat("COE median:", round(median(coe_6digit$coe_score), 1), "\n")

# Distribution of COE
print(quantile(coe_6digit$coe_score,
               probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

# Check overlap with our adoption data
overlap <- sum(coe_6digit$soc_6digit %in% occ_adoption_full$soc_6digit)
cat("\nSOC codes in both COE and adoption data:", overlap, "\n")
cat("COE codes not in adoption data:", 
    sum(!coe_6digit$soc_6digit %in% occ_adoption_full$soc_6digit), "\n")
cat("Adoption codes not in COE:", 
    sum(!occ_adoption_full$soc_6digit %in% coe_6digit$soc_6digit), "\n")

# Are the 90 adoption codes missing COE random or systematic?
occ_adoption_full %>%
  filter(!soc_6digit %in% coe_6digit$soc_6digit) %>%
  mutate(soc_major = str_sub(soc_6digit, 1, 2)) %>%
  summarise(
    n = n(),
    mean_adoption = round(mean(adoption_rate), 3),
    pct_zero      = round(mean(adoption_rate == 0) * 100, 1),
    .groups = "drop"
  )

# And by major group
occ_adoption_full %>%
  filter(!soc_6digit %in% coe_6digit$soc_6digit) %>%
  mutate(soc_major = str_sub(soc_6digit, 1, 2)) %>%
  count(soc_major) %>%
  arrange(desc(n))

# ============================================================
# SECTION 7: BLS EMPLOYMENT FOR BUBBLE SIZING
# ============================================================

soc_major <- soc_structure %>%
  filter(!is.na(`Major Group`)) %>%
  mutate(soc_major_group = str_sub(`Major Group`, 1, 2)) %>%
  select(soc_major_group, 
         occupation = `SOC or O*NET-SOC 2019 Title`)

bls_coded <- soc_major %>%
  left_join(bls_raw, by = "occupation") %>%
  select(soc_major_group, bls_employment) %>%
  filter(!is.na(bls_employment))

cat("Major groups with employment data:", nrow(bls_coded), "\n")
cat("Total employment covered:", 
    format(sum(bls_coded$bls_employment), big.mark=","), "\n")
cat("Employment range:", 
    format(min(bls_coded$bls_employment), big.mark=","),
    "to", format(max(bls_coded$bls_employment), big.mark=","), "\n")
print(bls_coded %>% arrange(desc(bls_employment)))

# ============================================================
# SECTION 7B: CROSSWALK 2010 SOC TO 2018 COE SCORES
# ============================================================
# Problem: O*NET task statements use 2010 SOC codes
#          O*NET COE file uses 2018 SOC codes
# Solution: Use BLS 2010-to-2018 crosswalk to map COE scores
#           back to 2010 codes. For 1-to-many mappings, we
#           average COE scores across target 2018 codes.

crosswalk <- read_xlsx(
  file.path(base, "data/release_1_feb2025/soc_2010_to_2018_crosswalk.xlsx"),
  skip = 7,
  col_names = c("soc_2010", "title_2010", "soc_2018", "title_2018")
) %>%
  filter(!is.na(soc_2010), !is.na(soc_2018)) %>%
  # Strip to 6-digit format — crosswalk may have dashes already
  mutate(
    soc_2010 = str_trim(soc_2010),
    soc_2018 = str_trim(soc_2018)
  )

cat("Crosswalk rows:", nrow(crosswalk), "\n")
cat("Unique 2010 codes:", n_distinct(crosswalk$soc_2010), "\n")
cat("Unique 2018 codes:", n_distinct(crosswalk$soc_2018), "\n")

# Check software developers specifically
crosswalk %>%
  filter(str_starts(soc_2010, "15-1132") | 
           str_starts(soc_2018, "15-1251"))

# Check if mapping is 1-to-1 or 1-to-many
crosswalk %>%
  group_by(soc_2010) %>%
  summarise(n_2018 = n_distinct(soc_2018)) %>%
  count(n_2018) %>%
  arrange(desc(n_2018))

# Step 1: Join crosswalk to COE scores on 2018 codes
coe_crosswalked <- crosswalk %>%
  inner_join(
    coe_6digit %>% select(soc_6digit, coe_score),
    by = c("soc_2018" = "soc_6digit")
  )

cat("2010 codes matched through crosswalk:", 
    n_distinct(coe_crosswalked$soc_2010), "\n")
cat("2010 codes with no COE match via crosswalk:",
    n_distinct(crosswalk$soc_2010) - 
      n_distinct(coe_crosswalked$soc_2010), "\n")

# Step 2: For 1-to-many mappings average the COE scores
# e.g. 15-1132 → mean(COE 15-1252, COE 15-1253)
coe_2010 <- coe_crosswalked %>%
  group_by(soc_2010) %>%
  summarise(
    coe_score    = mean(coe_score, na.rm = TRUE),
    n_2018_codes = n_distinct(soc_2018),
    .groups = "drop"
  )

cat("Final 2010 SOC codes with COE:", nrow(coe_2010), "\n")

# Step 3: Verify software developers came through
coe_2010 %>%
  filter(str_starts(soc_2010, "15-113")) %>%
  print()

# Step 4: Check how this compares to original direct COE match
# Some 2010 codes may exist in both — direct match takes priority
# as it means O*NET actually surveyed that exact occupation
coe_combined <- occ_adoption_full %>%
  # First try direct 2010 code match in COE
  left_join(
    coe_6digit %>% select(soc_6digit, coe_score) %>%
      rename(coe_direct = coe_score),
    by = "soc_6digit"
  ) %>%
  # Then try crosswalked COE as fallback
  left_join(
    coe_2010 %>% select(soc_2010, coe_crosswalk = coe_score),
    by = c("soc_6digit" = "soc_2010")
  ) %>%
  mutate(
    # Direct match takes priority, crosswalk fills gaps
    coe_score  = coalesce(coe_direct, coe_crosswalk),
    coe_source = case_when(
      !is.na(coe_direct)    ~ "direct",
      !is.na(coe_crosswalk) ~ "crosswalk",
      TRUE                  ~ "missing"
    )
  )

cat("\nCOE source breakdown:\n")
print(table(coe_combined$coe_source))

cat("\nAdoption rate by COE source:\n")
coe_combined %>%
  group_by(coe_source) %>%
  summarise(
    n             = n(),
    mean_adoption = round(mean(adoption_rate), 3),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# SECTION 8: ASSEMBLE FINAL ANALYTICAL DATASET
# ============================================================
# Unit of observation: 6-digit SOC occupation
# Key variables:
#   adoption_rate   — share of O*NET tasks in R4 API (0-1)
#   coe_score       — O*NET consequence of error (1-100)
#   bls_employment  — major group employment for bubble sizing
#   soc_major_group — for grouping and labeling in charts
# One representative title per 6-digit SOC — take first alphabetically

onet_titles <- onet_tasks %>%
  mutate(soc_6digit = str_remove(`O*NET-SOC Code`, "\\.\\d+$")) %>%
  distinct(soc_6digit, Title) %>%
  group_by(soc_6digit) %>%
  slice_min(order_by = Title, n = 1) %>%
  ungroup()

final_data <- coe_combined %>%
  filter(coe_source != "missing") %>%
  mutate(
    soc_major_group = str_sub(soc_6digit, 1, 2)
  ) %>%
  left_join(bls_coded, by = "soc_major_group") %>%
  left_join(onet_titles, by = "soc_6digit") %>%
  select(
    soc_6digit, soc_major_group, Title,
    adoption_rate, total_onet_tasks,
    api_tasks_weighted, n_matched_tasks,
    coe_score, coe_source,
    bls_employment
  )

cat("=== FINAL DATASET (REVISED) ===\n")
cat("Observations:", nrow(final_data), "\n")
cat("Duplicate SOC codes:", sum(duplicated(final_data$soc_6digit)), "\n")
cat("COE source breakdown:\n")
print(table(final_data$coe_source))

cat("\nAdoption rate — mean:", round(mean(final_data$adoption_rate), 3),
    "| median:", round(median(final_data$adoption_rate), 3), "\n")
cat("Adoption rate — range:", round(min(final_data$adoption_rate), 3),
    "to", round(max(final_data$adoption_rate), 3), "\n")
cat("COE score — mean:", round(mean(final_data$coe_score), 1),
    "| median:", round(median(final_data$coe_score), 1), "\n")
cat("COE score — range:", round(min(final_data$coe_score), 1),
    "to", round(max(final_data$coe_score), 1), "\n")
cat("Missing title:", sum(is.na(final_data$Title)), "\n")

# Core correlation with revised dataset
cat("\nCore correlation (adoption vs COE):",
    round(cor(final_data$adoption_rate,
              final_data$coe_score), 3), "\n")

# Where does 15-1132 sit now?
cat("\nSoftware Developers in final data:\n")
final_data %>%
  filter(str_starts(soc_6digit, "15-113")) %>%
  mutate(rank = rank(-final_data$adoption_rate)[
    match(soc_6digit, final_data$soc_6digit)
  ]) %>%
  select(soc_6digit, Title, adoption_rate, 
         coe_score, coe_source, rank) %>%
  print()

# Full SOC 15 picture
cat("\nAll SOC 15 occupations ranked:\n")
final_data %>%
  filter(soc_major_group == "15") %>%
  arrange(desc(adoption_rate)) %>%
  select(soc_6digit, Title, adoption_rate, 
         coe_score, n_matched_tasks) %>%
  print(n = 20)

final_data %>%
  arrange(desc(adoption_rate)) %>%
  select(soc_6digit, Title, adoption_rate, coe_score) %>%
  slice_head(n = 10)

# ============================================================
# SECTION 9: AEI VISUAL THEME
# ============================================================

library(ggplot2)
library(ggrepel)
library(scales)

aei_theme <- function() {
  theme_minimal(base_family = "Georgia") +
    theme(
      # Background
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
      # Titles
      plot.title    = element_text(size = 14, face = "bold",
                                   color = "#1a1a1a", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 10, color = "#444444",
                                   margin = margin(b = 12)),
      plot.caption  = element_text(size = 7.5, color = "#888888",
                                   hjust = 0, margin = margin(t = 10)),
      # Axes
      axis.title   = element_text(size = 9,  color = "#333333"),
      axis.text    = element_text(size = 8.5, color = "#333333"),
      axis.ticks   = element_blank(),
      axis.line.x  = element_line(color = "#cccccc", linewidth = 0.4),
      # Legend
      legend.position    = "bottom",
      legend.title       = element_text(size = 8),
      legend.text        = element_text(size = 8),
      legend.key.size    = unit(0.4, "cm"),
      # Margins
      plot.margin = margin(16, 16, 16, 16)
    )
}

# AEI color palette
aei_navy  <- "#1b3a6b"
aei_red   <- "#c0392b"
aei_grey  <- "#95a5a6"
aei_light <- "#d6e4f0"

# Quintile color ramp — light to dark navy
aei_quintile_colors <- c(
  "#d6e4f9", "#8ab4d4", "#4a86b8", "#1b5c9e", "#1b3a6b"
)

cat("Theme loaded.\n")

# ============================================================
# SECTION 10: VISUAL 1 — COE QUINTILE BAR CHART
# ============================================================
# Headline visual. Bins occupations into COE quintiles and
# shows mean adoption rate per bin with confidence intervals.
# This is the simplest, most direct test of the hypothesis.

# Create quintile bins
final_data_viz <- final_data %>%
  mutate(
    coe_quintile = ntile(coe_score, 5),
    coe_label = case_when(
      coe_quintile == 1 ~ "Q1\nLowest\nConsequence",
      coe_quintile == 2 ~ "Q2",
      coe_quintile == 3 ~ "Q3",
      coe_quintile == 4 ~ "Q4",
      coe_quintile == 5 ~ "Q5\nHighest\nConsequence"
    ),
    coe_label = factor(coe_label, levels = c(
      "Q1\nLowest\nConsequence", "Q2", "Q3", "Q4",
      "Q5\nHighest\nConsequence"
    ))
  )

# Compute mean and SE per quintile
quintile_summary <- final_data_viz %>%
  group_by(coe_quintile, coe_label) %>%
  summarise(
    mean_adoption = mean(adoption_rate),
    se            = sd(adoption_rate) / sqrt(n()),
    n             = n(),
    coe_range     = paste0(round(min(coe_score)), "–",
                           round(max(coe_score))),
    .groups = "drop"
  )

# Print summary for reference
print(quintile_summary %>%
        select(coe_quintile, coe_range, n, mean_adoption, se))

p1 <- ggplot(quintile_summary,
             aes(x = coe_label, y = mean_adoption,
                 fill = factor(coe_quintile))) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = mean_adoption - 1.96 * se,
        ymax = mean_adoption + 1.96 * se),
    width = 0.18, color = "#555555", linewidth = 0.5
  ) +
  geom_text(
    aes(label = paste0(round(mean_adoption * 100, 1), "%")),
    vjust = -0.8, size = 3.2, fontface = "bold",
    color = "#1a1a1a", family = "Georgia"
  ) +
  scale_fill_manual(values = aei_quintile_colors) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.25),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Higher Consequence of Error Is Associated with API Adoption",
    subtitle = "Mean share of O*NET tasks observed in Anthropic API traffic, by COE quintile",
    x        = "Consequence of Error Quintile (O*NET, 1–100 scale)",
    y        = "Mean Task Adoption Rate",
    caption  = paste0(
      "Note: Adoption measured as share of O*NET-defined tasks per occupation observed in ",
      "Anthropic API traffic (Release 4, Nov 2025).\n",
      "Error bars show 95% confidence intervals. N = 680 6-digit SOC occupations. ",
      "Source: Anthropic AEI data; O*NET."
    )
  ) +
  aei_theme() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

ggsave("visual1_coe_quintile_bar.png", p1,
       width = 7, height = 5, dpi = 300, bg = "white")

print(p1)

# ============================================================
# SECTION 11: VISUAL 2 — SCATTER PLOT COE VS ADOPTION
# ============================================================

# ---- UPDATED LABEL SELECTION ----
# Based on Figure 3 findings:
# - Computer Programmers: #1 adoption, moderate COE — the expected hero
# - Travel Agents: #2 adoption, high COE — the outlier to explain
# - Information Security Analysts: high adoption AND high COE — 
#   interesting tension, appears in top 12
# - Market Research Analysts: high adoption, low COE — clean example
# - Aviation Inspectors: low adoption, high COE — anchors the 
#   bottom left of the story
# - Operations Research Analysts: high adoption, low COE — 
#   analytical/quant work being automated

label_these <- c(
  "Computer Programmers",
  "Travel Agents",
  "Information Security Analysts",
  "Market Research Analysts and Marketing Specialists",
  "Aviation Inspectors",
  "Operations Research Analysts"
)

label_data <- final_data %>%
  filter(Title %in% label_these)

cat("Labels found:", nrow(label_data), "of", length(label_these), "\n")
print(label_data %>% select(Title, adoption_rate, coe_score))

# ---- DYNAMIC TRAVEL AGENTS ANNOTATION ----
# Read coordinates from data so arrow always points correctly
travel_agent <- final_data %>%
  filter(Title == "Travel Agents") %>%
  select(coe_score, adoption_rate)

cat("Travel Agents — COE:", travel_agent$coe_score,
    "| Adoption:", round(travel_agent$adoption_rate, 3), "\n")

# ---- REBUILD PLOT ----
set.seed(42)
plot_data <- final_data %>%
  mutate(
    adoption_plot = if_else(
      adoption_rate == 0,
      runif(n(), -0.005, 0.005),
      adoption_rate
    ),
    adoption_plot = pmax(adoption_plot, 0),
    is_zero = adoption_rate == 0
  )

p2 <- ggplot() +
  # Zero adoption points — faded behind
  geom_point(
    data  = plot_data %>% filter(is_zero),
    aes(x = coe_score, y = adoption_plot,
        size = bls_employment),
    color = "#bbcde0",
    alpha = 0.3,
    shape = 16
  ) +
  # Non-zero adoption points — primary layer
  geom_point(
    data  = plot_data %>% filter(!is_zero),
    aes(x = coe_score, y = adoption_plot,
        size = bls_employment),
    color = aei_navy,
    alpha = 0.35,
    shape = 16
  ) +
  # Trend line on non-zero only
  geom_smooth(
    data      = plot_data %>% filter(!is_zero),
    aes(x = coe_score, y = adoption_plot),
    method    = "lm",
    se        = TRUE,
    color     = aei_red,
    fill      = "#f5c6c2",
    linewidth = 0.9,
    alpha     = 0.25
  ) +
  # Travel Agents annotation — dynamic coordinates
  annotate(
    "text",
    x        = travel_agent$coe_score + 6,
    y        = travel_agent$adoption_rate - 0.07,
    label    = "Travel Agents: high-COE outlier\nlikely driven by AI agent pipelines",
    size     = 2.5,
    family   = "Georgia",
    color    = "#888888",
    hjust    = 0,
    fontface = "italic"
  ) +
  annotate(
    "segment",
    x         = travel_agent$coe_score + 5,
    xend      = travel_agent$coe_score + 1,
    y         = travel_agent$adoption_rate - 0.055,
    yend      = travel_agent$adoption_rate - 0.015,
    color     = "#aaaaaa",
    linewidth = 0.4,
    arrow     = arrow(length = unit(0.08, "inches"))
  ) +
  # Labels — exclude Travel Agents since it has annotation
  geom_label_repel(
    data          = label_data %>% filter(Title != "Travel Agents"),
    aes(x = coe_score, y = adoption_rate,
        label = str_wrap(Title, 22)),
    size          = 2.6,
    family        = "Georgia",
    color         = "#1a1a1a",
    fill          = "white",
    label.padding = unit(0.15, "lines"),
    label.size    = 0.2,
    box.padding   = unit(0.6, "lines"),
    max.overlaps  = 20,
    segment.color = "#aaaaaa",
    segment.size  = 0.3,
    force         = 3
  ) +
  scale_size_continuous(
    range  = c(1, 9),
    breaks = c(5000000, 15000000),
    labels = c("5M workers", "15M workers"),
    name   = "Major Group\nEmployment"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(-0.01, 0.80),
    breaks = seq(0, 0.80, 0.20),
    expand = c(0.01, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title    = "Automation Penetration Falls as Error Stakes Rise",
    subtitle = paste0(
      "Each point is a 6-digit SOC occupation. ",
      "Bubble size = major group employment.\n",
      "Faded points at zero have no matched API tasks in Release 4."
    ),
    x       = "Consequence of Error Score (O*NET, 1–100)",
    y       = "Task Automation Penetration Rate",
    caption = paste0(
      "Note: Penetration measured as share of O*NET-defined tasks ",
      "per occupation observed in Anthropic API traffic ",
      "(Release 4, Nov 13–20 2025).\n",
      "Trend line from OLS regression on non-zero occupations. ",
      "N = 762 occupations (495 non-zero). ",
      "Source: Anthropic AEI data; O*NET; BLS."
    )
  ) +
  aei_theme() +
  theme(legend.position = "right")

ggsave("visual2_scatter_final.png", p2,
       width = 8, height = 6, dpi = 300, bg = "white")

print(p2)

# ============================================================
# SECTION 12: VISUAL 3 — TOP AND BOTTOM OCCUPATIONS
# ============================================================

# Fix 1: Deduplicate postsecondary teachers in lowest adoption
# Keep only one representative, fill rest with genuinely 
# different low-adoption occupations

# Flag postsecondary teachers
final_data <- final_data %>%
  mutate(
    is_postsec_teacher = str_detect(
      Title, 
      regex("Teachers?, Postsecondary|Instructors?, Postsecondary", 
            ignore_case = TRUE)
    )
  )

# Top 12 by adoption — no deduplication needed here
top_occ <- final_data %>%
  arrange(desc(adoption_rate)) %>%
  slice_head(n = 12) %>%
  mutate(group = "Highest Adoption")

# Bottom 12 — keep max 1 postsecondary teacher
# First pull non-teachers with any adoption
bottom_non_teacher <- final_data %>%
  filter(adoption_rate > 0, !is_postsec_teacher) %>%
  arrange(adoption_rate) %>%
  slice_head(n = 11)

# Add one postsecondary teacher as representative
bottom_teacher <- final_data %>%
  filter(adoption_rate > 0, is_postsec_teacher) %>%
  arrange(adoption_rate) %>%
  slice_head(n = 1)

bottom_occ <- bind_rows(bottom_non_teacher, bottom_teacher) %>%
  arrange(adoption_rate) %>%
  slice_head(n = 12) %>%
  mutate(group = "Lowest Adoption")

# Check what we got
cat("Top 12 occupations:\n")
print(top_occ %>% select(Title, adoption_rate, coe_score))
cat("\nBottom 12 occupations:\n")
print(bottom_occ %>% select(Title, adoption_rate, coe_score))

# Build plot data
bar_data <- bind_rows(top_occ, bottom_occ) %>%
  mutate(
    # Clean up long titles
    Title_clean = str_wrap(Title, 38),
    Title_clean = fct_reorder(Title_clean, adoption_rate)
  )

p3 <- ggplot(bar_data,
             aes(x = adoption_rate, 
                 y = Title_clean,
                 fill = coe_score)) +
  geom_col(width = 0.72) +
  # Fix 4: right-align labels flush to bar end
  geom_text(
    aes(label = paste0(round(adoption_rate * 100, 1), "%")),
    hjust  = -0.12,
    size   = 2.9,
    color  = "#1a1a1a",
    family = "Georgia"
  ) +
  # Fix 3: stronger color contrast — widen midpoint spread
  scale_fill_gradient2(
    low      = "#d6e4f0",   # light blue — low COE
    mid      = "#1b5c9e",   # mid blue
    high     = "#8b0000",   # deep red — high COE
    midpoint = 50,
    name     = "Consequence\nof Error",
    limits   = c(0, 100),
    breaks   = c(0, 25, 50, 75, 100),
    labels   = c("0", "25", "50", "75", "100")
  ) +
  # Fix 1: extend x-axis to prevent label truncation
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.90),
    breaks = seq(0, 0.80, 0.20),
    expand = c(0, 0)
  ) +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  labs(
    title    = "Which Occupations' Task Structures Are Being Automated",
    subtitle = "Top and bottom 12 occupations by task automation penetration rate, colored by consequence of error",
    x        = "Task Automation Penetration Rate",
    y        = NULL,
    caption  = paste0(
      "Note: Penetration measured as share of O*NET-defined tasks per occupation ",
      "observed in Anthropic API traffic (Release 4, Nov 2025).\n",
      "Lowest adoption panel excludes zero-adoption occupations and collapses ",
      "postsecondary teacher sub-specializations to one representative. ",
      "Source: Anthropic AEI data; O*NET."
    )
  ) +
  aei_theme() +
  theme(
    strip.text = element_text(
      size     = 10,
      face     = "bold",
      color    = "#1a1a1a",
      hjust    = 0           # left-align panel titles
    ),
    # Fix 4: right-align y-axis labels flush to bars
    axis.text.y        = element_text(hjust = 1, size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e8e8e8", 
                                      linewidth = 0.4),
    legend.position    = "right",
    legend.key.height  = unit(1.2, "cm"),
    legend.key.width   = unit(0.35, "cm")
  )

ggsave("visual3_top_bottom_revised.png", p3,
       width = 13, height = 8.5, dpi = 300, bg = "white")

print(p3)

# ============================================================
# ROBUSTNESS CHECK: CLAUDE.AI ADOPTION VS COE
# ============================================================
# Mirror the exact same pipeline but using r4_claude_tasks
# as the traffic source instead of r4_api_tasks.
# Denominator stays identical — same O*NET task universe.

# Step 1: Match Claude.ai tasks to O*NET
claude_onet_bridge <- r4_claude_tasks %>%
  inner_join(onet_universe, by = c("task_name" = "task_clean"))

claude_matched    <- n_distinct(claude_onet_bridge$task_name)
claude_total      <- n_distinct(r4_claude_tasks$task_name)

cat("Claude tasks matched to O*NET:", claude_matched,
    "of", claude_total,
    "(", round(claude_matched / claude_total * 100, 1), "%)\n")

# Step 2: Equal weighting
claude_onet_weighted <- claude_onet_bridge %>%
  group_by(task_name) %>%
  mutate(
    n_occupations = n_distinct(soc_6digit),
    task_weight   = 1 / n_occupations
  ) %>%
  ungroup()

# Step 3: Aggregate to occupation level
claude_adoption <- claude_onet_weighted %>%
  group_by(soc_6digit) %>%
  summarise(
    claude_tasks_weighted = sum(task_weight),
    n_matched_tasks_claude = n_distinct(task_name),
    .groups = "drop"
  )

# Step 4: Add zeros for unmatched occupations
claude_adoption_full <- onet_task_counts %>%
  left_join(
    claude_adoption,
    by = "soc_6digit"
  ) %>%
  mutate(
    claude_tasks_weighted  = replace_na(claude_tasks_weighted, 0),
    n_matched_tasks_claude = replace_na(n_matched_tasks_claude, 0),
    claude_adoption_rate   = claude_tasks_weighted / total_onet_tasks
  )

cat("Occupations with Claude adoption data:",
    sum(claude_adoption_full$claude_adoption_rate > 0), "\n")
cat("Claude adoption rate — mean:",
    round(mean(claude_adoption_full$claude_adoption_rate), 3), "\n")
cat("Claude adoption rate — range:",
    round(min(claude_adoption_full$claude_adoption_rate), 3), "to",
    round(max(claude_adoption_full$claude_adoption_rate), 3), "\n")

# Step 5: Merge into final_data for comparison
robustness_data <- final_data %>%
  left_join(
    claude_adoption_full %>%
      select(soc_6digit, claude_adoption_rate),
    by = "soc_6digit"
  ) %>%
  mutate(claude_adoption_rate = replace_na(claude_adoption_rate, 0))

# Step 6: Core correlations side by side
cat("\n=== ROBUSTNESS CHECK CORRELATIONS ===\n")
cat("API adoption vs COE:   ",
    round(cor(robustness_data$adoption_rate,
              robustness_data$coe_score), 3), "\n")
cat("Claude adoption vs COE:",
    round(cor(robustness_data$claude_adoption_rate,
              robustness_data$coe_score), 3), "\n")

# Step 7: Quintile comparison — does gradient hold in Claude data?
robustness_data %>%
  mutate(coe_quintile = ntile(coe_score, 5)) %>%
  group_by(coe_quintile) %>%
  summarise(
    coe_range      = paste0(round(min(coe_score)), "-",
                            round(max(coe_score))),
    api_adoption   = round(mean(adoption_rate), 3),
    claude_adoption = round(mean(claude_adoption_rate), 3),
    n              = n(),
    .groups = "drop"
  ) %>%
  print()

# Step 8: Scatter comparison — do occupations rank similarly?
cat("\nRank correlation (Spearman) between API and Claude adoption:\n")
cat(round(cor(robustness_data$adoption_rate,
              robustness_data$claude_adoption_rate,
              method = "spearman"), 3), "\n")

# Step 9: Which occupations differ most between platforms?
cat("\nLargest gaps — higher in API than Claude:\n")
robustness_data %>%
  mutate(gap = adoption_rate - claude_adoption_rate) %>%
  arrange(desc(gap)) %>%
  select(Title, adoption_rate, claude_adoption_rate, 
         gap, coe_score) %>%
  slice_head(n = 10) %>%
  print()

cat("\nLargest gaps — higher in Claude than API:\n")
robustness_data %>%
  mutate(gap = claude_adoption_rate - adoption_rate) %>%
  arrange(desc(gap)) %>%
  select(Title, adoption_rate, claude_adoption_rate, 
         gap, coe_score) %>%
  slice_head(n = 10) %>%
  print()

# ============================================================
# SECTION 13: VISUAL 4 — API VS CLAUDE QUINTILE COMPARISON
# ============================================================
# Robustness visual. Shows the COE gradient holds across both
# platforms — API (firm automation) and Claude.ai (human use).
# Side by side bars per quintile, two colors for each platform.

# Build quintile summary for both platforms
robustness_quintile <- robustness_data %>%
  mutate(
    coe_quintile = ntile(coe_score, 5),
    coe_label = case_when(
      coe_quintile == 1 ~ "Q1\nLowest\nConsequence",
      coe_quintile == 2 ~ "Q2",
      coe_quintile == 3 ~ "Q3",
      coe_quintile == 4 ~ "Q4",
      coe_quintile == 5 ~ "Q5\nHighest\nConsequence"
    ),
    coe_label = factor(coe_label, levels = c(
      "Q1\nLowest\nConsequence", "Q2", "Q3", "Q4",
      "Q5\nHighest\nConsequence"
    ))
  ) %>%
  group_by(coe_quintile, coe_label) %>%
  summarise(
    api_mean    = mean(adoption_rate),
    api_se      = sd(adoption_rate) / sqrt(n()),
    claude_mean = mean(claude_adoption_rate),
    claude_se   = sd(claude_adoption_rate) / sqrt(n()),
    coe_range   = paste0(round(min(coe_score)), "–",
                         round(max(coe_score))),
    n           = n(),
    .groups     = "drop"
  )

# Pivot to long format for grouped bar chart
robustness_long <- robustness_quintile %>%
  select(coe_quintile, coe_label, coe_range,
         api_mean, api_se, claude_mean, claude_se) %>%
  pivot_longer(
    cols      = c(api_mean, claude_mean),
    names_to  = "platform",
    values_to = "mean_adoption"
  ) %>%
  mutate(
    se = if_else(platform == "api_mean", api_se, claude_se),
    platform = if_else(platform == "api_mean",
                       "API (Firm Automation)",
                       "Claude.ai (Human Use)"),
    platform = factor(platform,
                      levels = c("API (Firm Automation)",
                                 "Claude.ai (Human Use)"))
  )

# Correlation annotations for subtitle
api_cor    <- round(cor(robustness_data$adoption_rate,
                        robustness_data$coe_score), 3)
claude_cor <- round(cor(robustness_data$claude_adoption_rate,
                        robustness_data$coe_score), 3)

p4 <- ggplot(robustness_long,
             aes(x = coe_label, y = mean_adoption,
                 fill = platform, group = platform)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width    = 0.65
  ) +
  geom_errorbar(
    aes(ymin = mean_adoption - 1.96 * se,
        ymax = mean_adoption + 1.96 * se),
    position  = position_dodge(width = 0.72),
    width     = 0.18,
    color     = "#555555",
    linewidth = 0.45
  ) +
  geom_text(
    aes(label = paste0(round(mean_adoption * 100, 1), "%")),
    position = position_dodge(width = 0.72),
    vjust    = -0.7,
    size     = 2.8,
    fontface = "bold",
    color    = "#1a1a1a",
    family   = "Georgia"
  ) +
  # Annotation — correlation for each platform
  annotate(
    "text",
    x = 4.8, y = 0.235,
    label    = paste0("API r = ", api_cor),
    size     = 3,
    family   = "Georgia",
    color    = aei_navy,
    fontface = "italic",
    hjust    = 1
  ) +
  annotate(
    "text",
    x = 4.8, y = 0.220,
    label    = paste0("Claude r = ", claude_cor),
    size     = 3,
    family   = "Georgia",
    color    = "#555555",
    fontface = "italic",
    hjust    = 1
  ) +
  scale_fill_manual(
    values = c(
      "API (Firm Automation)"   = "#1b5c9e",
      "Claude.ai (Human Use)"   = "#4a86b8"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.26),
    expand = c(0, 0)
  ) +
  labs(
    title    = "The Error Constraint Holds for Both Firms and Individual Users",
    subtitle = paste0(
      "Mean task automation penetration by COE quintile - ",
      "API traffic (firm pipelines) vs Claude.ai (human use)"
    ),
    x       = "Consequence of Error Quintile (O*NET, 1–100 scale)",
    y       = "Mean Task Adoption Rate",
    caption = paste0(
      "Note: Both measures use identical O*NET task denominators. ",
      "API traffic reflects firm-level automated workflows; ",
      "Claude.ai reflects direct human usage.\n",
      "Error bars show 95% confidence intervals. ",
      "N = 762 6-digit SOC occupations. ",
      "Spearman rank correlation between platforms = 0.821. ",
      "Source: Anthropic AEI data; O*NET."
    )
  ) +
  aei_theme() +
  theme(
    legend.position  = "bottom",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 9)
  )

ggsave("visual4_robustness_quintile.png", p4,
       width = 7.5, height = 5.5, dpi = 300, bg = "white")

print(p4)

# Sensitivity Analysis
final_data %>%
  filter(coe_source == "direct") %>%
  summarise(
    n = n(),
    cor_direct = round(cor(adoption_rate, coe_score), 3)
  )