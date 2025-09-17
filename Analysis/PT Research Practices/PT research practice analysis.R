library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(scales)
library(stringr) 
library(purrr)
library(knitr)
library(tidyr)
library(writexl)

options(scipen = 999)

setwd() # SET YOUR WORKING DIRECTORY AS THE FOLDER WHERE YOU HAVE PUT THE "Analysis" FOLDER IN 

df<- read_excel("Analysis/PT Research Practices/PT-research practice validated.xlsx", sheet = "Data 2022+2023+2024")
View(df)

# ─────────────────────────────────────────────────────────────────────────────
#                 #### Define useful variable ####
# ─────────────────────────────────────────────────────────────────────────────
# Dataframe with only Original articles
val_orig <- df %>%
  filter(type == "Original")

# ─────────────────────────────────────────────────────────────────────────────
#                   #### Type of paper published ####
# ─────────────────────────────────────────────────────────────────────────────

# Settings to adjust
bar_height  <- 0.8  
bar_spacing <- 1.2   
aspect_ratio <- 0.5  

# check the unique values and their frequency of type
table(df$'type')

# Count per category
data_counts <- df %>%
  count(type) %>%
  arrange(n) %>%
  mutate(
    type      = factor(type, levels = type),
    highlight = ifelse(type == "Original", "Original", "Other")
  )

# Sort by year
year_counts <- df %>%
  count(type, year, name = "n_year") %>%
  mutate(type = factor(type, levels = levels(data_counts$type))) %>%
  arrange(type, year) %>%
  group_by(type) %>%
  mutate(
    cum_n   = cumsum(n_year),                 
    prev_n  = dplyr::lag(cum_n, default = 0), 
    mid_x   = (prev_n + cum_n)/2,             
    total_n = sum(n_year),
    y_num   = as.numeric(type)                
  ) %>%
  ungroup()

year_split_lines    <- dplyr::filter(year_counts, cum_n < total_n)
year_labels_original<- dplyr::filter(year_counts, as.character(type) == "Original")

# Plot 
plot_type_article <- ggplot(data_counts, aes(x = n, y = type, fill = highlight)) +
  geom_col(width = bar_height) +
  geom_segment(
    data = year_split_lines,
    aes(x = cum_n, xend = cum_n,
        y = y_num - bar_height/2, yend = y_num + bar_height/2),
    inherit.aes = FALSE, color = "white", linewidth = 0.8
  ) +
  geom_text(
    data = year_labels_original,
    aes(x = mid_x, y = type, label = year),
    inherit.aes = FALSE, color = "white", fontface = "bold", size = 4
  ) +
  scale_fill_manual(values = c(Original = "orange", Other = "grey"), guide = FALSE) +
  scale_x_continuous(breaks = pretty(data_counts$n),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(expand = expansion(add = bar_height/2)) +
  labs(
    title = "Distribution of Articles",
    x     = "Number of articles",
    y     = NULL
  ) +
  theme_minimal() +
  theme(    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)),
            axis.line.x        = element_line(color = "black"),
            axis.line.y        = element_line(color = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title.x       = element_text(face = "bold", color = "black", size = 16),
            axis.text.x        = element_text(size = 14, color = "black", face = "bold"),
            axis.text.y        = element_text(size = 16, color = "black", face = "bold"),
            aspect.ratio       = aspect_ratio
  )

print(plot_type_article)

# ─────────────────────────────────────────────────────────────────────────────
#       #### Proportion of systematic review and meta analysis ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(df$`systematic review`)
table(df$`meta-analysis`)

# Settings to adjust
bar_height   <- 0.8  
bar_spacing  <- 1.2  
aspect_ratio <- 0.7  

# Filter review article
df_rev <- df %>% 
  filter(type == "Review")

# Calculation of the distribution of review type 
props_df <- df_rev %>%
  summarise(
    narrative     = mean(`systematic review` == "No"  & `meta-analysis` == "No",  na.rm = TRUE),
    sys_only      = mean(`systematic review` == "Yes" & `meta-analysis` == "No",  na.rm = TRUE),
    meta_only     = mean(`systematic review` == "No"  & `meta-analysis` == "Yes", na.rm = TRUE),
    sys_with_meta = mean(`systematic review` == "Yes" & `meta-analysis` == "Yes", na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "review_type",
    values_to = "proportion"
  ) %>%
  mutate(
    review_type = recode(
      review_type,
      narrative     = "Narrative review",
      sys_only      = "Systematic review",
      meta_only     = "Meta-analysis",
      sys_with_meta = "Systematic review\n+ meta-analysis"
    ),
    review_type = factor(
      review_type,
      levels = c(
        "Narrative review",
        "Systematic review",
        "Meta-analysis",
        "Systematic review\n+ meta-analysis"
      )
    )
  )

# Display proportion table
print(props_df)

# Plot data
plot_review_distr <- ggplot(props_df, aes(
  x    = review_type,
  y    = proportion,
  fill = review_type
)) +
  geom_col(width = bar_height) +
  geom_text(aes(label = percent(proportion, accuracy = 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, .1))
  ) +
  scale_fill_viridis_d(guide = FALSE) +
  labs(
    title = "Distribution of review type",
    x     = NULL,
    y     = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line       = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks      = element_line(color = "#000000"),
    axis.text.x     = element_text(size = 11, face = "bold", colour = "#000000"),
    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 12.5, face = "bold", colour = "#000000", margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )

plot_review_distr

# ─────────────────────────────────────────────────────────────────────────────
#                     #### Hypothesis test rate ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(val_orig$`hypo_tested`)

# Calculation of the number of original article that include hypothesis statement
tab_orig <- table(tolower(val_orig$hypo_tested), useNA = "no")
taux_yes <- tab_orig["yes"] / sum(tab_orig[c("yes","no")])
sprintf("Hypothesis test rate: %.1f%%", taux_yes * 100)


# ─────────────────────────────────────────────────────────────────────────────
#         #### Positive result rate & Level of hypothesis support ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(val_orig$support_hyp1)

# Calculation of distribution
df_support <- val_orig %>%
  mutate(
    support_clean = case_when(
      str_detect(support_hyp1, regex("full",    ignore_case = TRUE)) ~ "Full support",
      str_detect(support_hyp1, regex("partial", ignore_case = TRUE)) ~ "Partial support",
      str_detect(support_hyp1, regex("not",     ignore_case = TRUE)) ~ "Not supported",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(support_clean)) %>%
  count(support_clean) %>%
  mutate(pct = n / sum(n) * 100)

# Specify order on the chart 
levels_sup <- c("Full support", "Partial support", "Not supported")

# Plot bar chart
ggplot(df_support, aes(
  x    = factor(1),                                    
  y    = pct,                                          
  fill = factor(support_clean, levels = levels_sup)    
)) +
  geom_col(
    position = position_stack(reverse = TRUE),      
    width    = 0.5,
    color    = "black",
    size     = 0.1
  ) +
  scale_y_continuous(
    name   = "Relative Frequency",
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  scale_x_discrete(NULL, breaks = NULL) +
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  scale_fill_viridis_d(
    name   = NULL,    
    option = "D",     
    begin  = 1,     
    end    = 0,
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.line.x = element_blank()
  )

# Display value in %
df_support

# Plot pie chart
my_colors <- c(
  "Full support" = "#3B0055",   
  "Partial support" = "#404789",   
  "Not supported" = "#84C8E0"   
)

support_plot <- ggplot(df_support, aes(
  x    = factor(1),                                    
  y    = pct,                                          
  fill = factor(support_clean, levels = levels_sup)    
)) +
  geom_col(
    position = position_stack(reverse = TRUE),      
    width    = 1,
    color    = "white",
    size     = 2
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(
    values = my_colors,
    name   = NULL
  ) +
  geom_text(
    aes(label = paste0(round(pct), "%")), 
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white",
    fontface = "bold",
    size     = 10
  ) +
  theme_void(base_size = 20) +
  theme(
    legend.position = "bottom"
  )
print(support_plot)

# ─────────────────────────────────────────────────────────────────────────────
#                        #### Preregistration ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(df$prereg)

# Check the Websites used 
table(val_orig$prereg_link)

# ────────────────────────────    
       #### Overall ####
# ────────────────────────────

# Proportion of "Yes" among Original article
pct_yes <- df %>%
  filter(type == "Original", !is.na(prereg)) %>%
  summarise(p = sum(prereg == "Yes") / n()) %>%
  pull(p)

# Global rate
pct_global <- val_orig %>%
  summarise(p = sum(prereg == "Yes") / n()) %>%
  pull(p)
# Rate per year
pct_par_annee <- val_orig %>%
  group_by(year) %>%
  summarise(p = sum(prereg == "Yes") / n()) %>%
  arrange(year)
# Display
cat("Overall pre-registration rate : ",
    percent(pct_global, accuracy = 0.1), "\n\n",
    sep = "")

for(i in seq_len(nrow(pct_par_annee))) {
  cat("Pre-registration rate by year :\n")
  cat("  ", pct_par_annee$year[i], " : ",
      percent(pct_par_annee$p[i], accuracy = 0.1), "\n", sep = "")
}

# Plot data
plot_df <- tibble(
  Status = "Pre-registration",
  prop   = pct_yes
)

ggplot(plot_df, aes(x = prop, y = Status)) +
  geom_col(
    fill   = "gold",
    color  = "black",
    width  = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    labels = percent_format(accuracy = 1)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x     = NULL,
    y     = NULL,
    title = "Overall pre-registration rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border     = element_rect(color = "black", fill = NA, size = 1),
    panel.grid       = element_blank(),
    axis.line.x      = element_line(),
    axis.ticks.x     = element_line(),
    axis.text.x      = element_text(vjust = 0.5),
    axis.text.y      = element_text(face = "bold"),
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.margin      = margin(5, 20, 5, 5)
  )

# ─────────────────────────────────────────
          #### By year ####
# ─────────────────────────────────────────

# Aggregation by year
prereg_summary_year <- df %>%
  filter(type == "Original") %>%
  group_by(year) %>%
  summarise(
    total   = n(),
    yes     = sum(tolower(prereg) == "yes"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_yes = yes / total,
    year    = factor(year)
  )

# Plot the data
ggplot(prereg_summary_year, aes(x = year, y = pct_yes, fill = year)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    name   = "Proportion",
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.4),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    name   = "Année",
    values = c("2022" = "#FBBE2EFF",       
               "2023" = "#DD5C31FF",
               "2024" = "#483C32")
  ) +
  labs(
    x     = NULL,
    y     = NULL,
    title = "Percentage of Preregistration by Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.text.x        = element_text(face = "bold"),
    axis.text.y        = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_text(
      margin = margin(r = 10),  
      face   = "bold"
    ),
    legend.position    = "none"
  )

# ─────────────────────────────────────────────────────────────────────────────
#                     #### Sample size justification ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(val_orig$n_justification)

# ────────────────────────────    
       #### Overall ####
# ────────────────────────────

# Proportion of "Yes" among Original article
pct_yes_justif <- val_orig %>%
  summarise(p = sum(n_justification == "Yes") / n()) %>%
  pull(p)

# Sample size justification rate
# Global rate
pct_global_nj <- val_orig  %>%
  summarise(p = sum(n_justification == "Yes") / n()) %>%
  pull(p)

# Rate per year
pct_par_annee_nj <- val_orig  %>%
  group_by(year) %>%
  summarise(p = sum(n_justification == "Yes") / n()) %>%
  arrange(year)

# Display
cat("Overall sample size justification rate : ",
    percent(pct_global_nj, accuracy = 0.1), "\n\n", sep = "")

for(i in seq_len(nrow(pct_par_annee_nj))) {
  cat("Sample size justification rate by year :\n")
  cat("  ", pct_par_annee_nj$year[i], " : ",
      percent(pct_par_annee_nj$p[i], accuracy = 0.1), "\n", sep = "")
}

# Plot data
plot_df_nj <- tibble(
  Status = "",
  prop   = pct_yes_justif
)

ggplot(plot_df_nj, aes(x = prop, y = Status)) +
  geom_col(
    fill   = "gold",
    color  = "black",
    width  = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    labels = percent_format(accuracy = 1)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x     = NULL,
    y     = NULL,
    title = "Overall Sample Size Justification rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border       = element_rect(color = "black", fill = NA, size = 1),
    panel.grid         = element_blank(),
    axis.line.x        = element_line(),
    axis.ticks.x       = element_line(),
    axis.text.x        = element_text(vjust = 0.5),
    axis.text.y        = element_text(face = "bold"),
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    plot.margin        = margin(5, 20, 5, 5)
  )

# ─────────────────────────────────────────
              #### By year ####
# ─────────────────────────────────────────
# Aggregation by year
justif_summary_year <- df %>%
  filter(type == "Original", !is.na(n_justification)) %>%
  group_by(year) %>%
  summarise(
    total = n(),
    yes   = sum(tolower(n_justification) == "yes"),
    .groups = "drop"
  ) %>%
  mutate(pct_yes = yes / total,
         year = factor(year))

# Plot the data
ggplot(justif_summary_year, aes(x = year, y = pct_yes, fill = year)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    name   = "Proportion",
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.8, by = 0.1),
    limits = c(0, 0.4),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    name   = "Année",
    values = c("2022" = "darkred",
               "2023" = "darkorange",
               "2024" = "#483C32")
  ) +
  labs(
    x     = NULL,
    title = "Percentage of Sample Size Justifications by Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.text.x      = element_text(face = "bold"),
    axis.text.y      = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    legend.text      = element_text(face = "bold")
  )


# ───────────────────────────────────  
      #### Justification type ####
# ───────────────────────────────────
# check the unique values and their frequency 
table(val_orig$justification_type)

pie_data <- val_orig %>%
  filter(n_justification == "Yes") %>%
  count(justification_type) %>%
  mutate(
    justification_type = factor(
      justification_type,
      levels = c("Previous Study", "Pilot Study", "Benchmark")
    ),
    prop  = n / sum(n),
    label = scales::percent(prop)
  )

n_just_plot <- ggplot(data = pie_data, aes(x = "", y = prop, fill = justification_type)) +
  geom_col(width = 1, color = "white", size = 2) +
  coord_polar(theta = "y", direction = -1) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            color = "black", fontface = "bold", size = 8) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Pilot Study"         = "#C2DB1C",
      "Previous Study"     = "#9CD94C",
      "Benchmark"           = "#FFE631" 
          )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 13, face = "bold")
  )

plot(n_just_plot)

pie_data

# ─────────────────────────────────────────────────────────────────────────────
#  #### Distribution of n_justification by type of hypothesis support ####
# ─────────────────────────────────────────────────────────────────────────────

# Calculation of distribution
pct_support_nj <- val_orig %>%
  filter(
    hypo_tested   == "Yes",
    !is.na(n_justification),
    !is.na(support_hyp1),
  ) %>%
  filter(!is.na(support_hyp1)) %>%
  group_by(support_hyp1) %>%
  summarise(
    total   = n(),
    yes     = sum(tolower(n_justification) == "yes"),
    pct_yes = yes / total,
    .groups = "drop"
  ) %>%
  mutate(
    support_hyp1 = factor(
      support_hyp1,
      levels = c("Full support", "Partial support", "Not supported")
    )
  )
pct_support_nj

# Set the color
couleurs_support <- c(
  "Full support" = "#3B0055",   
  "Partial support" = "#404789",   
  "Not supported" = "#84C8E0"   
)

# Display
for(i in seq_len(nrow(pct_support_nj))) {
  cat("Percentage of sample size justification :\n")
  cat(
    sprintf("  %s : %s\n",
            levels(pct_support_nj$support_hyp1)[i],
            percent(pct_support_nj$pct_yes[i], accuracy = 0.1))
  )
}

# Plot the data
plot_just_by_support <- ggplot(pct_support_nj,
       aes(x = support_hyp1, y = pct_yes, fill = support_hyp1)) +
  geom_col(color = "black", width = 0.7) +       
  scale_x_discrete(drop = TRUE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = couleurs_support,
    guide  = FALSE
  ) +
  labs(
    x     = NULL,
    y     = "Sample size justification",
    title = "Proportion of sample size justification by type of hypothesis support"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border       = element_blank(),                
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank(),
     panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),  
    axis.line.y        = element_line(color = "black"),
    axis.text.x     = element_text(color = "#000000", face = "bold"),
    axis.title.y     = element_text(face = "bold", color = "#000000", size = 16, margin = margin(r = 15)),
    axis.text.y     = element_text(size = 10, color = "#000000", face = "bold"),
    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15))
  )

# ─────────────────────────────────────────────────────────────────────────────
#                     #### n across topic table ####
# ─────────────────────────────────────────────────────────────────────────────

# Adding topic names
topic_map <- tibble::tibble(
  category = 1:25,
  topic    = c(
    "Acute Care", "Balance Falls", "Cardiovascular Pulmonary", "Covid", "Diabetes",
    "Education", "Geriatrics", "Health Policy", "Implementation Science",
    "Integumentary", "Knowledge Translation", "LEAP", "Measurement",
    "Musculoskeletal", "Neurology", "Obesity", "Oncology", "Pain Management",
    "Pediatrics", "Pharmacology", "Prevention and Health Promotion",
    "Professional Issues", "Psychosocial", "Women Health", "Other"
  )
)

# Filter original article
df_orig <- df %>%
  left_join(topic_map, by = "category") %>%
  filter(type == "Original", topic != "Other") %>%
  mutate(across(c(n, women, men), ~ {
    x <- str_trim(as.character(.))             
    x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA  
    suppressWarnings(as.numeric(x))            
  }))

# Summary of information by topic 
  summary_by_topic <- df_orig %>%
  group_by(topic) %>%
  summarise(
    k         = n(),  
    Median      = median(n, na.rm = TRUE),
    SD        = sd(n, na.rm = TRUE),
    Min.      = min(n, na.rm = TRUE),
    Max.      = max(n, na.rm = TRUE),
    `% Women` = sum(women, na.rm = TRUE) / sum(n[!is.na(women)], na.rm = TRUE) * 100,
    `% Men`   = sum(men, na.rm = TRUE) / sum(n[!is.na(men)], na.rm = TRUE) * 100,
    .groups   = "drop"
  )

# Adding a Total line 
  total_row <- df_orig %>%
    summarise(
      topic = "Total",
      k     = n(),
      Median  = median(n, na.rm = TRUE),
      SD    = sd(n,   na.rm = TRUE),
      Min.  = min(n,  na.rm = TRUE),
      Max.  = max(n,  na.rm = TRUE),
      
      `% Women` = {
        df_gender <- df_orig %>% filter(topic != "Women Health")
        sum(df_gender$women, na.rm = TRUE) / sum(df_gender$n[!is.na(df_gender$women)], na.rm = TRUE) * 100
      },
      
      `% Men` = {
        df_gender <- df_orig %>% filter(topic != "Women Health")
        sum(df_gender$men, na.rm = TRUE) / sum(df_gender$n[!is.na(df_gender$men)], na.rm = TRUE) * 100
      }
    )

# Table creation
final_table <- bind_rows(summary_by_topic, total_row) %>%
  mutate(across(c(Median, SD, Min., Max., `% Women`, `% Men`), ~ round(.x, 1)))

kable(
  final_table,
  col.names = c("Topic", "k", "Median", "SD", "Min.", "Max.", "% Women", "% Men"),
  align     = c("l", rep("r", 7))
)

# Table exportation
write_xlsx(final_table,
           path = "...") # REPLACE BY YOUR PATH

# ─────────────────────────────────────────────────────────────────────────────
#               #### Sex distribution by research category ####
# ─────────────────────────────────────────────────────────────────────────────
# Convert Sex columns to long format and exclude women health
plot_data <- summary_by_topic %>%
  filter(topic != "Women Health") %>%
  pivot_longer(cols = c(`% Women`, `% Men`), 
               names_to = "Sex", 
               values_to = "Percentage") %>%
  mutate(Sex = factor(Sex, levels = c("% Women", "% Men")))

# Plot the data
sex_data <-  ggplot(plot_data, aes(x = topic, y = Percentage / 100, fill = Sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", size = 0.2) +
  scale_fill_manual(
    values = c("% Women" = "#D0F000", "% Men" = "#203174"),
    labels = c("Female", "Male")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x     = NULL,
    y     = "Participants",
    title = "Sex Distribution by Research Topic"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),    
    axis.line.x        = element_line(color = "#000000"),     
    axis.ticks.x       = element_line(color = "#000000"),
    axis.text.x     = element_text(angle = 40, color = "#000000", face = "bold", hjust = 1),
    axis.title.y     = element_text(face = "bold", color = "#000000", size = 16),
    axis.text.y     = element_text(size = 10, color = "#000000", face = "bold"),
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    legend.title    = element_blank(),
    legend.text      = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot(sex_data)

# ─────────────────────────────────────────────────────────────────────────────
#              #### Hypothesis testing & Support by category ####
# ─────────────────────────────────────────────────────────────────────────────

# Remove "Other" category and filter by Original article's categories  
topic_map2 <- topic_map %>%
  slice(-25) %>%
  filter(category %in% (df %>% 
                          filter(type == "Original") %>% 
                          pull(category)
  )
  )

# Label construction
df2 <- val_orig %>%
  inner_join(topic_map2, by = "category") %>%
  mutate(
    topic_label = factor(
      topic,
      levels = topic_map2 %>%
        arrange(category) %>%
        pull(topic)
    )
  ) %>%
  filter(!is.na(topic_label))


# Chart A: Hypothesis tested
p1_counts <- df2 %>%
  count(topic_label, hypo_tested) %>%
  group_by(topic_label) %>%
  mutate(
    freq = n / sum(n),
    percent = freq * 100,
    total_n = sum(n)             
  ) %>%
  ungroup()

labels_with_n <- p1_counts %>%
  distinct(topic_label, total_n) %>%
  mutate(label = paste0(topic_label, " (k = ", total_n, ")")) %>%
  arrange(topic_label)

p1 <- ggplot(p1_counts, aes(x = topic_label, y = freq, fill = hypo_tested)) +
  geom_col(position = "fill", width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0),                
    breaks = seq(0, 1, 0.25)         
  ) +
  scale_x_discrete(labels = labels_with_n$label) +
  scale_fill_manual(
    values = c("Yes" = "#3B0055", "No" = "#9D9D9D"),
    labels = c("Yes" = "Hypothesis", "No" = "No hypothesis"),
    name = NULL,
    guide  = guide_legend(reverse = TRUE) 
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y = element_line(color = "black"),     
    axis.ticks.y = element_line(color = "black"),    
    axis.text.y  = element_text(size = 9, face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold"),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text  = element_text(size = 12, color = "black", face = "bold")
  )

print(p1)

# Chart B : Hypothesis support (Full / Partial / Not)
totals_p1 <- p1_counts %>%
  group_by(topic_label) %>%
  summarise(total_all = sum(n), .groups = "drop")

labels_with_n <- totals_p1 %>%
  mutate(label = paste0(topic_label, " (k = ", total_all, ")"))

p2_counts <- df2 %>%
  filter(hypo_tested == "Yes") %>%
  count(topic_label, support_hyp1) %>%
  group_by(topic_label) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    support_hyp1 = factor(
      support_hyp1,
      levels = c("Full support", "Partial support", "Not supported")
    )
  ) %>%
  left_join(totals_p1, by = "topic_label")

lab_map <- setNames(labels_with_n$label, labels_with_n$topic_label)

p2 <- ggplot(p2_counts, aes(x = topic_label, y = freq, fill = support_hyp1)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_x_discrete(labels = lab_map) +
  scale_fill_manual(
    values = c(
      "Full support"    = "#3B0055",
      "Partial support" = "#406199",
      "Not supported"   = "#84C8E0" 
    ),
    name = NULL
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y  = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text.y  = element_text(size = 9, face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(color = "black", size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text  = element_text(size = 12, color = "black", face = "bold")
  )


print(p2)

# ─────────────────────────────────────────────────────────────────────────────
#                         #### Effect size ####
# ─────────────────────────────────────────────────────────────────────────────

# Overall reported effect size 
summary_yes <- df_quant %>%
  summarise(
    total       = n(),
    yes_count   = sum(effect_size == "Yes", na.rm = TRUE),
    prop_yes    = yes_count / total
  )
summary_yes %>%
  mutate(prop_yes = percent(prop_yes, accuracy = 0.1))


# ─────────────────────────────────────────────────────────────────────────────
#                  #### Statistical reporting ####
# ─────────────────────────────────────────────────────────────────────────────

# df_quant excludes qualitative studies
# check the unique values of sig_test and their frequency 
table(df_quant$sig_test)
table(df_quant$pval_type)
table(df_quant$pval_type)

# Proportion of quantitative original article with a hypothesis 
df_quant %>%
  summarise(
    n_total = n(),
    n_yes   = sum(hypo_tested == "Yes", na.rm = TRUE),
    prop_yes = mean(hypo_tested == "Yes", na.rm = TRUE),
    perc_yes = 100 * prop_yes
  )

# Proportion of positive results for quantitative original articles with a hypothesis
df_quant %>%
  filter(
    hypo_tested == "Yes",             
    !is.na(support_hyp1)              
  ) %>%
  count(support_hyp1) %>%
  mutate(
    proportion = n / sum(n),
    pourcentage = proportion * 100
  )

# Proportion of quantitative original article using significance testing 
df_quant %>%
  summarise(
    prop_yes = mean(sig_test == "Yes"),      
    perc_yes = mean(sig_test == "Yes") * 100  
  )

# Proportion of quantitative original article without hypothesis stated using sig testing
df_quant %>%
  filter(hypo_tested == "No") %>%
  summarise(
    prop_yes = mean(sig_test == "Yes"),       
    perc_yes = mean(sig_test == "Yes") * 100   
  )

# Proportion of pvalue type: exact, relative, mix of original article using significance testing
df_props <- df_quant %>%
  filter(
    sig_test == "Yes",
    !is.na(pval_type),
    pval_type %in% c("Exact", "Relative", "Mix")
  )

total_yes <- nrow(df_props)


df_props <- df_props %>%
  count(pval_type) %>%
  mutate(
    proportion = n / total_yes,
    pval_type = factor(pval_type, levels = c("Exact", "Relative", "Mix"))
  )

df_props

# Plot the data
plot_pval_type <- ggplot(df_props, aes(x = pval_type, y = proportion, fill = pval_type)) +
  geom_col(width = 0.6) +
  scale_fill_viridis_d(option = "D", name = "pval_type") +
  geom_text(aes(label = percent(proportion, accuracy = 1)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, .1))
  ) +
  labs(
    x     = NULL,
    y     = "Proportion",
    title = "Distribution of pvalue types"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line       = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks      = element_line(color = "#000000"),
    axis.text.x     = element_text(size = 11, face = "bold", colour = "#000000"),
    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 12.5, face = "bold", colour = "#000000", margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )

plot_pval_type

# ─────────────────────────────────────────────────────────────────────────────
#                      #### Data Sharing ####
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────
      #### Data availability statement ####
# ─────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(val_orig$data_state)

# Calculation of the proportion among original articles
avail <- val_orig %>%
  summarise(
    total       = n(),                               
    yes_count   = sum(tolower(data_state) == "yes"), 
    proportion  = yes_count / total                  
  )

print(avail)

# ─────────────────────────────────
       #### Open data ####
# ─────────────────────────────────

# check the unique values and their frequency 
table(val_orig$open_data)

# Calculation of overall open data proportion
open_data <- val_orig %>%
  filter(!is.na(open_data)) %>%                             
  summarise(
    total      = n(),                                       
    yes_count  = sum(tolower(open_data) == "yes"),          
    proportion = yes_count / total                          
  )

open_data

# Calculation of the proportion among original articles by year
open_data_year <- val_orig %>%
  filter(!is.na(open_data)) %>%                             
  group_by(year) %>%                                        
  summarise(
    total      = n(),                                       
    yes_count  = sum(tolower(open_data) == "yes"),          
    proportion = yes_count / total                          
  ) %>%
  ungroup()

# Plot the data
ggplot(open_data_year, aes(
  x    = factor(year),
  y    = proportion,
  fill = factor(year)                                     
)) +
  geom_col(color = "black", size = 0.2, width = 0.6) +        
  scale_fill_viridis_d(option = "D", name = "Année") +       
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.2),
    expand = expansion(c(0, 0.05))
  ) +
  labs(
    title = "Proportion of Open Data by year",
    x     = "Year",
    y     = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),    
    panel.grid.minor.x = element_blank(),    
    axis.title.x        = element_text(face = "bold"),
    axis.text.x        = element_text(face = "bold"),
    axis.title.y        = element_text(face = "bold"),
    legend.position    = "none",
    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10))
  )

résumé_data

# ─────────────────────────────────────────────────────────────────────────────
#                           #### Preprint ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(val_orig$preprint)

# Calculation of preprint proportion 
preprint <- val_orig %>%
  filter(!is.na(preprint)) %>%                            
  summarise(
    total             = n(),                              
    yes_preprint      = sum(tolower(preprint) == "yes"),  
    prop_yes_preprint = yes_preprint / total              
  )

print(preprint)

# ─────────────────────────────────────────────────────────────────────────────
#    #### Days elapsed between preprint and final publication ####
# ─────────────────────────────────────────────────────────────────────────────

days <- as.numeric(val_orig$`days from preprint`[val_orig$preprint == "Yes"],
                   units = "days")

mean_days <- mean(days, na.rm = TRUE)
sd_days   <- sd(days,   na.rm = TRUE)
n         <- sum(!is.na(days))

cat(sprintf("Preprint → publication : mean = %.1f days, SD = %.1f (n = %d)\n",
            mean_days, sd_days, n))

# ─────────────────────────────────────────────────────────────────────────────
#                  #### Average publication delay ####
# ─────────────────────────────────────────────────────────────────────────────
# Conversion into days and filtering of NA
val_orig3 <- val_orig %>%
  mutate(
    days = as.numeric(`days from submission`, units = "days")
  ) %>%
  filter(!is.na(days))


# Overall mean and standard deviation
overall_stats <- val_orig3 %>%
  summarise(
    avg_days = mean(days, na.rm = TRUE),
    sd_days  = sd(days,   na.rm = TRUE)
  )

# Display
cat("Average overall publication time:", round(overall_stats$avg_days, 1), "days\n")
cat("Standard deviation:", round(overall_stats$sd_days, 1), "days\n")


# ─────────────────────────────────────────────────────────────────────────────
#                  #### Summary figure ####
# ─────────────────────────────────────────────────────────────────────────────

# Add the combined variable "data_upon_request"
val_mod <- val_orig %>%
  mutate(
    data_upon_request = ifelse(
      data_state == "Yes" & open_data != "Yes", "Yes", "No"
    )
  )

practice_cols <- c(
  "data_upon_request",       
  "open_data", 
  "replication", 
  "preprint",
  "registered_report",
  "prereg", 
  "n_justification", 
  "hypo_tested"
)

df_long <- val_mod %>%
  select(any_of(practice_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Practice", values_to = "Response") %>%
  mutate(yes = ifelse(tolower(Response) == "yes", 1, 0)) %>%
  group_by(Practice) %>%
  summarise(n = sum(yes)) %>%
  mutate(Proportion = n / nrow(val_mod))

# Add missing categories (set to 0 if absent)
expected_practices <- tibble(Practice = practice_cols)
df_long <- expected_practices %>%
  left_join(df_long, by = "Practice") %>%
  mutate(
    n = replace_na(n, 0),
    Proportion = replace_na(Proportion, 0)
  )

# Create formatted labels
df_long <- df_long %>%
  mutate(
    Practice_label = factor(Practice,
                            levels = practice_cols,
                            labels = c(
                              "Data\nupon request",        
                              "Open data",
                              "Replication",
                              "Preprint",
                              "Registered\nreport",
                              "Pre-registration",
                              "Sample size\njustification",
                              "Hypotheses"
                            )
    ),
    Percent = paste0(round(Proportion * 100), "%")
  )

# Plot data
plot_sum <- ggplot(df_long, aes(x = Practice_label, y = Proportion, fill = Practice_label)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Percent), vjust = -0.5, fontface = "bold", size = 6, color = "black") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.4),
    breaks = seq(0, 0.5, by = 0.25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = c(
    "Data\nupon request"            = "#2c5d9f",
    "Open data"                     = "#177199",
    "Replication"                   = "#999999",
    "Preprint"                      = "#555555",
    "Registered\nreport"            = "#777777",
    "Pre-registration"              = "#1AA2A3",
    "Sample size\njustification"    = "#9CD94C",
    "Hypotheses"                    = "#3B0A4F"
  )) +
  labs(
    x = NULL,
    y = "Original articles",
    title = "Research Practices"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line       = element_line(color = "#000000", size = 0.5),
    axis.ticks      = element_line(color = "#000000"),
    axis.text.x     = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 13.5, face = "bold", colour = "#000000", margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )

print(plot_sum)

ggsave("C:/Users/franc/OneDrive - University of Ottawa/Doctorat Ottawa/3 - Article editing/Research practices' paper/Manuscript/Figures/summary research practice.png", plot = plot_sum, width = 12, height = 4, dpi = 600)
