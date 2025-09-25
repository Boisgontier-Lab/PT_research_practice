library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(scales)
library(purrr)
library(knitr)
library(tidyr)
library(writexl)
library(stringr)
library(readr)
library(reticulate)

options(scipen = 999)

setwd() # SET YOUR WORKING DIRECTORY AS THE FOLDER WHERE YOU HAVE PUT THE "Analysis" FOLDER IN 
initial_wd <- getwd()

df<- read_excel("Analysis/PT-research practice validated.xlsx", sheet = "Data 2022+2023+2024")

# ─────────────────────────────────────────────────────────────────────────────
#                   #### PYTHON CONFIGURATION ####
# ─────────────────────────────────────────────────────────────────────────────

# Set the path to your Python executable
python_path <- # SET THE PATH TO YOUR PYTHON.EXE FILE
use_python(python_path, required = TRUE)

# Name of the virtual environment
venv_name <- "ptchange-env"

# Activate the virtual environment
use_virtualenv(venv_name, required = TRUE)

virtualenv_install("ptchange-env", packages = c("pandas","numpy","scipy","tqdm","requests","beautifulsoup4","lxml",
    "selenium","undetected-chromedriver","colorama","pyautogui"))

            
# ─────────────────────────────────────────────────────────────────────────────
#             #### RETRIEVE ARTICLES WITH SIGNIFICANCE TESTING #### 
# ─────────────────────────────────────────────────────────────────────────────

# Make Dataframe with only the 246 articles that have done significance testing
val_orig_quant <- df %>%
  filter(type == "Original" & sig_test == "Yes") %>%
  mutate(
    doi = str_remove(doi, "^https?://(doi\\.org|doi-org\\.proxy\\.bib\\.uottawa\\.ca)/"),
    doi_str = doi %>%
      str_replace_all("/", "-") %>%
      str_replace_all("\\.", "-") %>%
      str_replace_all("\\(", "-") %>%
      str_replace_all("\\)", "-")
  )

# Write the CSV file for the Python script
write.csv(val_orig_quant, "Analysis/PTChange-main/dataframes/val_orig_quant.csv", row.names = FALSE)


# ─────────────────────────────────────────────────────────────────────────────
#                  #### DOWNLOADING FULLTEXTS ARTICLES ####  
# ─────────────────────────────────────────────────────────────────────────────

use_python(python_path, required = TRUE)
setwd("Analysis/PTChange-main/prepare_fulltexts")

# Launch download_fulltexts_from_doi: gather .html version of articles available in open access by clicking on the DOI link
system2(python_path, args = "download_fulltexts_from_doi.py", wait = TRUE)

# Retrieve articles not found #1
setwd(initial_wd)

html_dir <- "Analysis/PTChange-main/doi_tree_html"

existing_files <- list.files(html_dir, pattern = "\\.html$", full.names = FALSE)
existing_dois <- tools::file_path_sans_ext(existing_files)  

val_missing1 <- val_orig_quant %>%
  filter(!(doi_str %in% existing_dois))

write.csv(val_missing1, "Analysis/PTChange-main/dataframes/val_missing1.csv", row.names = FALSE)

# Launch download_fulltexts_from_pmcid: gather .html articles available from pubmed PMCID
setwd("Analysis/PTChange-main/prepare_fulltexts")

system2(python_path, args = "get_pmcids_from_dois.py", wait = TRUE)

system2(python_path, args = "download_fulltexts_from_pmcid.py", wait = TRUE)

# Retrieve articles not found #2
setwd(initial_wd)

existing_files <- list.files(html_dir, pattern = "\\.html$", full.names = FALSE)
existing_dois <- tools::file_path_sans_ext(existing_files)  

val_missing2 <- val_orig_quant %>%
  filter(!(doi_str %in% existing_dois))

write.csv(val_missing2["doi"], "Analysis/PTChange-main/dataframes/val_missing2.csv", row.names = FALSE)

# Launch download_fulltexts_from_scidirect: gather .mhtml articles available from  science-direct 
# NOTE: requires connection to the university Wi-Fi network to access science-direct

setwd("Analysis/PTChange-main/prepare_fulltexts")

system2(python_path, args = "get_pii_from_dois.py", wait = TRUE)

system2(python_path, args = "download_fulltexts_from_scidirect.py", wait = TRUE)

# NOTE 2: articles are saved in your download folder, you have to move them to "doi_tree_mhtml" folder afterwards

# # Retrieve manually articles not found #3 
setwd(initial_wd)

html_files <- list.files(path_html, pattern = "\\.html$", full.names = FALSE)
mhtml_files <- list.files(path_mhtml, pattern = "\\.mhtml$", full.names = FALSE)

# Delete extension to obtain DOIs
html_dois <- tools::file_path_sans_ext(html_files)
mhtml_dois <- tools::file_path_sans_ext(mhtml_files)

# Combine both lists
existing_dois3 <- unique(c(html_dois, mhtml_dois))

# Retrieve DOI not found yet
missing_dois3 <- setdiff(all_dois, existing_dois3)

val_missing3 <- val_orig_quant[val_orig_quant$doi_str %in% missing_dois3, ]

write.csv(val_missing3["doi"], "PTChange-main/dataframes/val_missing3.csv", row.names = FALSE)

# Find the latest articles manually, save as mhtml (from page internet save as "Web page, single file") 
# and put in folder "doi_tree_mhtml" with as a name: the DOI by replacing all special characters with “-”)
# Example: Doi = 10.1177/25152459251323480 ; Name = 10-1177-25152459251323480


# ─────────────────────────────────────────────────────────────────────────────
#        #### EXPORT .MHTML AND .HTML RESULTS SECTIONS INTO .TXT ####
# ─────────────────────────────────────────────────────────────────────────────
        
setwd("Analysis/PTChange-main/prepare_fulltexts")

system2(python_path, args = "make_plaintexts_from_mhtml.py", wait = TRUE)

system2(python_path, args = "make_plaintexts_from_html.py", wait = TRUE)


# ─────────────────────────────────────────────────────────────────────────────
#                     #### Fragile p-value calculation #### 
# ─────────────────────────────────────────────────────────────────────────────
              
# Set directories
setwd(initial_wd)
text_dir_html_txt <- normalizePath(file.path(getwd(), "Analysis/PTChange-main/doi_tree_txt"))
text_dir_mhtml_txt <- normalizePath(file.path(getwd(), "Analysis/PTChange-main/doi_tree_mhtml_txt"))
dataframes_dir <- normalizePath(file.path(getwd(), "Analysis/PTChange-main/dataframes"))

# Launch make_p_z_df.py
system2(python_path,
        args = c("Analysis/PTChange-main/make_dataset/make_p_z_df.py",
                 "--text_dirs", shQuote(text_dir_html_txt), shQuote(text_dir_mhtml_txt),
                 "--save_dir", shQuote(dataframes_dir)),
        wait = TRUE)

# Launch make_processed_p_ds.py
system2(python_path,
        args = c("Analysis/PTChange-main/make_dataset/make_processed_p_df.py"),
        wait = TRUE)


# Upload the final file
df_final <- read_csv("Analysis/PTChange-main/dataframes/val_orig_quant_process.csv")

# Calculation of the average of the proportions p_fragile
mean_p_fragile <- mean(df_final$p_fragile, na.rm = TRUE)

print(mean_p_fragile)

# ─────────────────────────────────────────────────────────────────────────────
#                    #### Fragile p-value calculation by year #### 
# ─────────────────────────────────────────────────────────────────────────────

df_final <- df_final %>%
  left_join(val_orig_quant %>% select(doi_str, year) %>% distinct() %>%
              mutate(year = suppressWarnings(as.integer(year))),
            by = "doi_str") %>%
  mutate(p_use = coalesce(p_fragile_prop_adj, p_fragile_prop_raw, p_fragile))


mean_p_fragile_by_year <- df_final %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    mean_p_fragile = mean(p_use, na.rm = TRUE),
    n_articles     = sum(!is.na(p_use)),
    .groups = "drop"
  ) %>%
  arrange(year)
print(mean_p_fragile_by_year)


# ─────────────────────────────────────────────────────────────────────────────
#                    #### Fragile p-value plot by year #### 
# ─────────────────────────────────────────────────────────────────────────────


base_dir  <- "Analysis/PTChange-main/dataframes"
proc_path <- file.path(base_dir, "val_orig_quant_process.csv")
val_path  <- file.path(base_dir, "val_orig_quant.csv")

val_year <- read_csv(val_path, show_col_types = FALSE) |>
  distinct(doi_str, year) |>
  mutate(year = as.integer(year))

yearly <- read_csv(proc_path, show_col_types = FALSE) |>
  transmute(doi_str, p_use = p_fragile_prop_adj) |>
  left_join(val_year, by = "doi_str") |>
  filter(!is.na(year), !is.na(p_use)) |>
  summarise(
    n      = dplyr::n(),
    mean_p = mean(p_use),
    sd     = sd(p_use),
    se     = sd / sqrt(n),
    .by    = year
  ) |>
  arrange(year)


yr_min <- 2022L
yr_max <- 2024L                     
x_mid  <- mean(c(yr_min, yr_max))   

to_percent <- max(yearly$mean_p, na.rm = TRUE) <= 1 + 1e-6
yearly2 <- yearly %>%
  mutate(
    y_raw   = mean_p,
    se_raw  = dplyr::coalesce(se, sd / sqrt(pmax(n, 1)), 0),
    y       = if (to_percent) y_raw  * 100 else y_raw,
    se_plot = if (to_percent) se_raw * 100 else se_raw
  )


plot_pfragile <- ggplot(yearly2, aes(x = year, y = y)) +
  geom_ribbon(aes(ymin = pmax(0, y - se_plot),
                  ymax = pmin(100, y + se_plot)),
              fill = "blue", alpha = 0.12, colour = NA) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 3.2) +
  
  geom_hline(yintercept = 26, linetype = "dashed",
             colour = "black", linewidth = 1) +
  
  annotate("text", x = x_mid, y = 26 + 1,
           label = "(Expected if 80% Power)",
           hjust = 0.5, vjust = 0, size = 5) +
  
  scale_x_continuous(
    limits = c(yr_min, yr_max),
    breaks = seq(yr_min, yr_max, by = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  scale_y_continuous(
    breaks  = c(26, 30, 40),
    labels  = c("26%", "30%", "40%"),
    expand  = c(0, 0)
  ) +
  
  coord_cartesian(ylim = c(24.5, NA), clip = "off") +
  
  labs(x = NULL, y = "Mean p-fragile (%)") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks       = element_line(color = "#000000"),
    axis.text.x      = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.text.y      = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.title.y     = element_text(size = 16, face = "bold", colour = "#000000",
                                    margin = margin(r = 12)),
    plot.margin      = margin(t = 10, r = 24, b = 10, l = 10)
  )

print(plot_pfragile)

ggsave("../Manuscript/Figures/mean p-fragile.png",
       plot = plot_pfragile, width = 6, height = 4, dpi = 600)

# ─────────────────────────────────────────────────────────────────────────────
#              #### Distribution of z-transformed p-value #### 
# ─────────────────────────────────────────────────────────────────────────────


df_pval <- read_csv("Analysis/PTChange-main/dataframes/val_orig_quant_process.csv")

df_z <- df_pval %>%
  transmute(
    sign  = str_trim(as.character(sign)),
    p_chr = str_replace_all(as.character(p_val), ",", "."),
    p_num = readr::parse_number(p_chr)
  ) %>%
  filter(sign == "=", !is.na(p_num), p_num > 0, p_num <= 1) %>%
  mutate(z = qnorm(1 - p_num/2))

bin_width <- 0.245
z_max     <- 9.80
z_edges   <- seq(0, z_max, by = bin_width)
breaks    <- c(z_edges, Inf)

bin_map <- tibble(
  bin_idx = seq_along(z_edges),
  left    = z_edges,
  right   = c(z_edges[-1], Inf)
)

df_binned <- df_z %>%
  mutate(bin = cut(z, breaks = breaks, include.lowest = TRUE, right = TRUE))

counts <- df_binned %>%
  mutate(bin_idx = as.integer(bin)) %>%
  count(bin_idx, name = "Frequency")

counts_full <- bin_map %>%
  left_join(counts, by = "bin_idx") %>%
  mutate(
    Frequency = replace_na(Frequency, 0L),
    highlight = case_when(
      right <= 1.96 & right > 1.96 - bin_width ~ "ref",
      left  >= 1.96 & left  < 1.96 + bin_width ~ "ref",
      TRUE                                    ~ "other"
    )
  )

x_stop <- 8.82

counts_full <- counts_full %>%
  filter(left < x_stop) %>%
  mutate(right_plot = pmin(right, x_stop))

major_breaks <- seq(0, x_stop, by = 0.98)
major_labels <- format(round(major_breaks, 2), nsmall = 2)

z_plot <- ggplot(counts_full, aes(y = Frequency)) +
  geom_rect(aes(xmin = left, xmax = right_plot, ymin = 0, ymax = Frequency, fill = highlight),
            color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c(ref = "grey70", other = "white"), guide = "none") +
  geom_vline(xintercept = 1.96, linetype = "dashed", linewidth = 1.2) +
  scale_x_continuous(breaks = major_breaks, labels = major_labels,
                     expand = c(0, 0), limits = c(0, x_stop)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Z statistic", y = "Frequency",
       title = NULL) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks       = element_line(color = "#000000"),
    axis.text.x      = element_text(size = 16, face = "bold", colour = "#000000"),
    axis.text.y      = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.title.y     = element_text(size = 16, face = "bold", colour = "#000000",
                                    margin = margin(r = 12)),
    axis.title.x     = element_text(size = 16, face = "bold", colour = "#000000"),
    plot.margin      = margin(t = 10, r = 24, b = 10, l = 10)
  )
z_plot

# ─────────────────────────────────────────────────────────────────────────────
#              #### Comparison p < .05 & p > .05 #### 
# ─────────────────────────────────────────────────────────────────────────────
# This comparison is made on p-value "<" or ">"

df_comp <- df_pval %>%
  transmute(
    sign  = str_trim(as.character(sign)),
    p_chr = str_replace_all(as.character(p_val), ",", "."),
    p_num = readr::parse_number(p_chr)
  ) %>%
  filter(sign %in% c("<", ">"), !is.na(p_num), p_num >= 0, p_num <= 1)

summary_df <- tibble(
  Category = c("p < 0.05", "p > 0.05"),
  n = c(sum(df_comp$sign == "<" & df_comp$p_num <= 0.05, na.rm = TRUE),
        sum(df_comp$sign == ">" & df_comp$p_num >= 0.05, na.rm = TRUE))
)

plot_comp <- ggplot(summary_df, aes(x = Category, y = n)) +
  geom_col(width = 0.7, fill = "grey70", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, fontface = "bold") +
  labs(x = NULL, y = "Fréquence",
       title = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line        = element_line(color = "#000000", linewidth = 0.5),
    axis.ticks       = element_line(color = "#000000"),
    axis.text.x      = element_text(size = 16, face = "bold", colour = "#000000"),
    axis.text.y      = element_text(size = 12, face = "bold", colour = "#000000"),
    axis.title.y     = element_text(size = 16, face = "bold", colour = "#000000",
                                    margin = margin(r = 12)),
    axis.title.x     = element_text(size = 16, face = "bold", colour = "#000000"),
    plot.margin      = margin(t = 10, r = 24, b = 10, l = 10)
  )
plot_comp
