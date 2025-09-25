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

original_counts <- df %>%
  filter(trimws(type) == "Original") %>%
  mutate(
    qual_group = ifelse(tolower(trimws(qualitative)) == "yes", "Qualitative", "Quantitative")
  ) %>%
  count(qual_group, name = "n") %>%
  mutate(type = factor("Original", levels = levels(data_counts$type)))

base_bars <- data_counts %>% filter(type != "Original")

# plot
plot_type_article <- ggplot() +
  geom_col(
    data = base_bars,
    aes(x = n, y = type, fill = "Other"),
    width = bar_height
  ) +
  geom_col(
    data = original_counts,
    aes(x = n, y = type, fill = qual_group),
    width = bar_height
  ) +
  scale_fill_manual(
    values = c(Other = "grey", Quantitative = "orange", Qualitative = "steelblue"),
    guide = FALSE
  ) +
  scale_x_continuous(breaks = pretty(data_counts$n),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(expand = expansion(add = bar_height/2)) +
  labs(
    title = "Distribution of Articles",
    x     = "Number of articles",
    y     = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)),
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
#                   #### Number of paper per journal ####
# ─────────────────────────────────────────────────────────────────────────────
journal_counts <- df %>%
  group_by(journal) %>%
  summarise(n_articles = n()) %>%
  arrange(desc(n_articles))

print(journal_counts)

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

# Keep original studies and remove qualitative studies
df_quant <- df %>%
  filter(
    trimws(type) == "Original",
    qualitative != "Yes"
  )

# check the unique values and their frequency 
table(df_quant$`hypo_tested`)

# Calculation of the number of original article that include hypothesis statement
tab_orig <- table(tolower(df_quant$hypo_tested), useNA = "no")
taux_yes <- tab_orig["yes"] / sum(tab_orig[c("yes","no")])
sprintf("Hypothesis test rate: %.1f%%", taux_yes * 100)


# ─────────────────────────────────────────────────────────────────────────────
#         #### Positive result rate & Level of hypothesis support ####
# ─────────────────────────────────────────────────────────────────────────────

# check the unique values and their frequency 
table(df_quant$support_hyp1)

# Calculation of distribution
df_support <- df_quant %>%
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
table(val_orig$justification_type)
table(val_orig$qualitative)

# ────────────────────────────    
  #### Overall proportion ####
# ────────────────────────────
# Proportion of "Yes" among Original articles
yes_justif_orig <- mean(val_orig$n_justification == "Yes", na.rm = TRUE)
yes_justif_orig

# Rate per year 
justif_orig_year <- val_orig |>
  group_by(year) |>
  summarise(p = mean(n_justification == "Yes", na.rm = TRUE),
            .groups = "drop") |>
  arrange(year)
justif_orig_year

# ────────────────────────────    
  #### Qualitative studies ####
# ────────────────────────────
# Proportion "Yes" among qualitative articles
yes_justif_quali <- mean(val_orig$n_justification[val_orig$qualitative == "Yes"] == "Yes", na.rm = TRUE)
yes_justif_quali
tab <- table(trimws(val_orig$justification_type[val_orig$qualitative == "Yes"]),
             useNA = "ifany")
tab

# Rate per year 
justif_quali_year <- val_orig |>
  filter(tolower(trimws(qualitative)) == "yes") |>
  group_by(year) |>
  summarise(
    p = mean(tolower(trimws(n_justification)) == "yes", na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  arrange(year)

justif_quali_year

# ────────────────────────────
 #### Quantitative studies ####
# ────────────────────────────
# df_quant include only quantitative studies
# check the unique values and their frequency 
table(df_quant$n_justification)
table(df_quant$justification_type)

# Proportion "Yes" among quantitative articles
yes_justif_quanti <- mean(df_quant$n_justification == "Yes", na.rm = TRUE)
yes_justif_quanti

# Rate per year 
justif_quanti_year <- df_quant |>
  group_by(year) |>
  summarise(p = mean(n_justification == "Yes", na.rm = TRUE),
            .groups = "drop") |>
  arrange(year)
justif_quanti_year


# ─────────────────────────────────────────────────────────────────
       #### Justification type for qualitative studies ####
# ─────────────────────────────────────────────────────────────────

fixed_cols <- c(
  "Information Power"     = "#1F9E89",
  "Data Saturation"   = "#4CC26C",
  "Data Adequacy" = "#90D743",
  "Heuristic"         = "#E5E419"
)

core_levels <- c("Information Power", "Data Saturation", "Data Adequacy", "Heuristic")

bar_data_quali <- val_orig %>%
  filter(tolower(trimws(qualitative)) == "yes",
         tolower(trimws(n_justification)) == "yes") %>%
  mutate(justification_type = str_squish(justification_type)) %>%
  filter(!is.na(justification_type), justification_type != "") %>%
  count(justification_type, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(
    prop  = n / sum(n),
    label = scales::percent(prop)
  )

present_levels <- unique(bar_data_quali$justification_type)
fixed_present  <- intersect(names(fixed_cols), present_levels)
other_levels   <- setdiff(present_levels, fixed_present)

other_cols <- if (length(other_levels) > 0) {
  viridisLite::viridis(length(other_levels), option = "viridis", begin = 0.55, end = 0.98)
} else character(0)

palette_map <- c(setNames(fixed_cols[fixed_present], fixed_present),
                 setNames(other_cols, other_levels))

all_levels <- c(core_levels, setdiff(sort(other_levels), core_levels))
bar_data_quali <- bar_data_quali %>%
  mutate(justification_type = factor(justification_type, levels = all_levels))

# Plot
ggplot(bar_data_quali, aes(x = justification_type, y = prop, fill = justification_type)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = label), vjust = -0.4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = palette_map, guide = "none") +
  labs(x = NULL, y = "Qualitative studies",
       title = "Sample size justifications (Qualitative)") +
  theme_classic(base_size = 14) +
  theme(
    axis.line       = element_line(color = "#000000", size = 0.5),
    axis.ticks      = element_line(color = "#000000"),
    axis.text.x     = element_text(size = 12, face = "bold", colour = "#000000",
                                   angle = 45, hjust = 1, vjust = 1),
    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 13.5, face = "bold", colour = "#000000",
                                   margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )



# ─────────────────────────────────────────────────────────────────
      #### Justification type for quantitative studies ####
# ─────────────────────────────────────────────────────────────────
# check the unique values and their frequency 
table(df_quant$justification_type)


core_levels <- c("Power analysis", "Accuracy", "Heuristic", "Resource Constraints")
fixed_cols <- c(
  "Power analysis"       = "#FFEE58",  
  "Accuracy"             = "#FFD54F",  
  "Heuristic"            = "#FFB74D",  
  "Resource Constraints" = "#FFA100"   
)

bar_data_quanti <- df_quant %>%
  filter(tolower(trimws(n_justification)) == "yes") %>%            
  mutate(jt = tolower(trimws(justification_type))) %>%
  mutate(justif4 = dplyr::case_when(
    jt %in% c("previous study", "pilot study", "benchmark") ~ "Power analysis",
    jt == "accuracy" ~ "Accuracy",
    jt == "heuristic" ~ "Heuristic",
    jt %in% c("resource constraints", "resource constraint", "resources") ~ "Resource Constraints",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(justif4)) %>%                                      
  count(justif4, name = "n") %>%
  mutate(
    prop  = n / sum(n),
    label = scales::percent(prop),
    justif4 = factor(justif4, levels = core_levels)                
  )

# Bar plot
ggplot(bar_data_quanti, aes(x = justif4, y = prop, fill = justif4)) +
  geom_col(width = 0.75, color = NA) +
  geom_text(aes(label = label), vjust = -0.4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = fixed_cols, guide = "none") +
  labs(x = NULL, y = "Proportion of quantitative studies",
       title = "Sample size justifications (Quantitative)") +
  theme_classic(base_size = 14) +
  theme(
    axis.line       = element_line(color = "#000000", size = 0.5),
    axis.ticks      = element_line(color = "#000000"),
    axis.text.x     = element_text(size = 12, face = "bold", colour = "#000000",
                                   angle = 45, hjust = 1, vjust = 1),
    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 13.5, face = "bold", colour = "#000000",
                                   margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )


# ─────────────────────────────────────────────────────────────────
    #### Sub-justification type for a-priori power analysis ####
# ─────────────────────────────────────────────────────────────────

keep_levels <- c("Previous Study", "Pilot Study", "Benchmark")

pie_data <- df_quant %>%
  filter(tolower(trimws(n_justification)) == "yes") %>%              
  mutate(justification_type = str_squish(justification_type)) %>%    
  filter(justification_type %in% keep_levels) %>%                    
  count(justification_type, name = "n") %>%
  right_join(tibble(justification_type = keep_levels), by = "justification_type") %>%
  mutate(
    n = tidyr::replace_na(n, 0L),
    justification_type = factor(justification_type, levels = keep_levels),
    prop  = if (sum(n) > 0) n / sum(n) else 0,
    label = if (sum(n) > 0) scales::percent(prop) else "0%"
  )

n_just_plot <- ggplot(pie_data, aes(x = "", y = prop, fill = justification_type)) +
  geom_col(width = 1, color = "white", size = 2) +
  coord_polar(theta = "y", direction = -1) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", fontface = "bold", size = 8) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Pilot Study"    = "#C2DB1C",
      "Previous Study" = "#9CD94C",
      "Benchmark"      = "#FFE631"
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 20)
  )

print(n_just_plot)

pie_data

# ─────────────────────────────────────────────────────────────────────────────
#  #### Distribution of n_justification by type of hypothesis support ####
# ─────────────────────────────────────────────────────────────────────────────

# Calculation of distribution
pct_support_nj <- df_quant %>%
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
    "Acute Care", "Balance & Falls", "Cardiovascular & Pulmonary", "Covid", "Diabetes",
    "Education", "Geriatrics", "Health Policy", "Implementation Science",
    "Integumentary", "Knowledge Translation", "LEAP", "Measurement",
    "Musculoskeletal", "Neurology", "Obesity", "Oncology", "Pain Management",
    "Pediatrics", "Pharmacology", "Prevention & Health Promotion",
    "Professional Issues", "Psychosocial", "Women's Health", "Other"
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
  })) %>%
  filter(if_all(c(n, women, men), ~ !is.na(.)))


## Summary of information by topic 
summary_by_topic <- df_orig %>%
  group_by(topic) %>%
  summarise(
    k         = n(),
    Mean      = mean(n,    na.rm = TRUE),
    SD        = sd(n,      na.rm = TRUE),
    Min.      = min(n,     na.rm = TRUE),
    Max.      = max(n,     na.rm = TRUE),
    Total_n   = sum(n,     na.rm = TRUE),           
    `% Women` = sum(women, na.rm = TRUE) / sum(n, na.rm = TRUE) * 100,
    `% Men`   = sum(men,   na.rm = TRUE) / sum(n, na.rm = TRUE) * 100,
    .groups   = "drop"
  ) %>%
  arrange(topic)

# Adding a Total line 
total_row <- summary_by_topic %>%
  summarise(
    topic     = "Total",
    k         = sum(k),
    Mean      = mean(df_orig$n,    na.rm = TRUE),
    SD        = sd(  df_orig$n,    na.rm = TRUE),
    Min.      = min( df_orig$n,    na.rm = TRUE),
    Max.      = max( df_orig$n,    na.rm = TRUE),
    Total_n   = sum( df_orig$n,    na.rm = TRUE),   
    `% Women` = sum(df_orig$women, na.rm = TRUE) / sum(df_orig$n, na.rm = TRUE) * 100,
    `% Men`   = 100 - `% Women`
  )

# Table creation
final_table <- bind_rows(summary_by_topic, total_row) %>%
  mutate(across(c(Mean, SD, Min., Max., `% Women`, `% Men`), ~ round(.x, 0)))

kable(
  final_table,
  col.names = c("Topic", "k", "Mean", "SD", "Min.", "Max.", "n", "% Women", "% Men"),
  align     = c("l", rep("r", 8))
)

total_femmes <- sum(df_orig$women, na.rm = TRUE)
total_hommes <- sum(df_orig$men,   na.rm = TRUE)

total_femmes
total_hommes

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
df2 <- df_quant %>%
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
#                  #### Summary figure for all articles ####
# ─────────────────────────────────────────────────────────────────────────────

# Add the combined variable "data_upon_request"
val_mod <- val_orig %>%
  mutate(
    data_upon_request = ifelse(
      data_state == "Yes" & open_data != "Yes", "Yes", "No"
    )
  )

# Keep only the requested practices
practice_cols <- c(
  "replication",
  "prereg",
  "registered_report",
  "data_upon_request",
  "open_data",
  "preprint"
)

df_long <- val_mod %>%
  select(any_of(practice_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Practice", values_to = "Response") %>%
  mutate(yes = ifelse(tolower(Response) == "yes", 1, 0)) %>%
  group_by(Practice) %>%
  summarise(n = sum(yes), .groups = "drop") %>%
  mutate(Proportion = n / nrow(val_mod))

# Add missing categories (set to 0 if absent)
expected_practices <- tibble(Practice = practice_cols)
df_long <- expected_practices %>%
  left_join(df_long, by = "Practice") %>%
  mutate(
    n = replace_na(n, 0),
    Proportion = replace_na(Proportion, 0)
  )

# Labels only for the 6 kept practices
df_long <- df_long %>%
  mutate(
    Practice_label = factor(
      Practice,
      levels = practice_cols,
      labels = c(
        "Replication",
        "Pre-registration",
        "Registered report",
        "Data upon request",
        "Open data",
        "Preprint"
      )
    ),
    Percent = paste0(round(Proportion * 100), "%")
  )

# Plot
plot_sum <- ggplot(df_long, aes(x = Practice_label, y = Proportion, fill = Practice_label)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = Percent), vjust = -0.5, fontface = "bold", size = 6, color = "black") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.9),
    breaks = seq(0, 0.75, by = 0.25),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(expand = c(0.1, 0.02)) +
  scale_fill_manual(values = c(
    "Replication"        = "#999999",
    "Pre-registration"   = "#1f9e89",
    "Registered report" = "#777777",
    "Data upon request" = "#2c5d9f",
    "Open data"          = "#2d708e",
    "Preprint"           = "#555555"
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
    axis.text.x      = element_text(size = 12, face = "bold", colour = "#000000",
                                    angle = 45, hjust = 1, vjust = 1),    axis.text.y     = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y    = element_text(size = 13.5, face = "bold", colour = "#000000", margin = margin(r = 12)),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )

print(plot_sum)

# ─────────────────────────────────────────────────────────────────────────────
#                  #### Summary figure for quantitative articles ####
# ─────────────────────────────────────────────────────────────────────────────

practice_cols2 <- c(
  "n_justification",  
  "hypo_tested",      
  "sig_test",      
  "support_hyp1",      
  "effect_size"       
)

df_long2 <- df_quant %>%
  select(any_of(practice_cols2)) %>%
  pivot_longer(cols = everything(), names_to = "Practice", values_to = "Response") %>%
  mutate(yes = ifelse(tolower(Response) == "yes", 1, 0)) %>%
  group_by(Practice) %>%
  summarise(n = sum(yes), .groups = "drop") %>%
  mutate(Proportion = n / nrow(df_quant))

denom_hyp <- sum(tolower(trimws(df_quant$hypo_tested)) == "yes", na.rm = TRUE)
num_hyp <- sum(
  trimws(df_quant$support_hyp1) %in% c("Full support","Partial support"),
  na.rm = TRUE
)

df_long2 <- df_long2 %>%
  mutate(
    n = ifelse(Practice == "support_hyp1", num_hyp, n),
    Proportion = ifelse(Practice == "support_hyp1",
                        ifelse(denom_hyp > 0, num_hyp/denom_hyp, 0),
                        Proportion)
  )

expected_practices2 <- tibble(Practice = practice_cols2)
df_long2 <- expected_practices2 %>%
  left_join(df_long2, by = "Practice") %>%
  mutate(
    n = replace_na(n, 0),
    Proportion = replace_na(Proportion, 0)
  )

df_long2 <- df_long2 %>%
  mutate(
    Practice_label = factor(
      Practice,
      levels = practice_cols2,
      labels = c(
        "Sample size justification",
        "Hypothesis statement",
        "Significance testing",
        "Hypothesis support",
        "Effect size"
      )
    ),
    Percent = paste0(round(Proportion * 100), "%")
  )

plot_quant <- ggplot(df_long2, aes(x = Practice_label, y = Proportion, fill = Practice_label)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = Percent), vjust = -0.5, fontface = "bold", size = 6, color = "black") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.9),
    breaks = seq(0, 0.75, by = 0.25),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(expand = c(0.12, 0.02)) +
  scale_fill_manual(values = c(
    "Sample size justification"    = "#9CD94C",
    "Hypothesis statement"       = "#3B0A4F",
    "Significance testing"       = "#21918c",
    "Hypothesis support"         = "#2a788e",
    "Effect size"                = "#3b528b"
  )) +
  labs(
    x = NULL,
    y = "Quantitative articles",
    title = "Research Practices (Quantitative)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line        = element_line(color = "#000000", size = 0.5),
    axis.ticks       = element_line(color = "#000000"),
    axis.text.x      = element_text(size = 12, face = "bold", colour = "#000000",
                                    angle = 45, hjust = 1, vjust = 1),
    axis.text.y      = element_text(size = 10, face = "bold", colour = "#000000"),
    axis.title.y     = element_text(size = 13.5, face = "bold", colour = "#000000", margin = margin(r = 12)),
    legend.position  = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 16, colour = "#000000")
  )

print(plot_quant)
