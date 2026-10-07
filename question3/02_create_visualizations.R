# ==============================================================================
# Question 3 - Task 2: Plot 1 - AE Severity Distribution
# File: question_3_tlg/02_create_visualizations.R (Parte 1)
# ==============================================================================

library(pharmaverseadam)
library(ggplot2)
library(dplyr)

# Caricamento dati
data("adae", package = "pharmaverseadam")

# Assicuriamoci che la cartella di output esista
dir.create("question3", showWarnings = FALSE)

# Preparazione dei dati per il Plot 1
ae_sev_data <- adae %>%
  filter(!is.na(AESEV) & !is.na(ACTARM)) %>%
  mutate(
    # Ordinamento dei livelli di gravità come mostrato nel mockup (MILD, MODERATE, SEVERE)
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")),
    ACTARM = factor(ACTARM)
  )

# Creazione del grafico a barre
plot1 <- ggplot(ae_sev_data, aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack", width = 0.55) +
  scale_fill_manual(
    values = c("MILD" = "#F8766D", "MODERATE" = "#00BA38", "SEVERE" = "#619CFF"),
    name = "Severity/Intensity"
  ) +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_grey(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    legend.position = "right",
    legend.title = element_text(face = "plain"),
    axis.text = element_text(color = "black")
  )

# Salvataggio in formato PNG
ggsave("question3/plot1_ae_severity.png", plot = plot1, width = 8, height = 6, dpi = 300)

print("Plot 1 creato con successo!")


# ==============================================================================
# Question 3 - Task 2: Plot 2 - Top 10 Most Frequent AEs with 95% CI
# File: question_3_tlg/02_create_visualizations.R (Parte 2)
# ==============================================================================

library(pharmaverseadam)
library(ggplot2)
library(dplyr)
library(stats)

# Caricamento dei dataset
data("adae", package = "pharmaverseadam")
data("adsl", package = "pharmaverseadam")

# Numero totale di soggetti unici nel dataset ADSL
N_subjects <- n_distinct(adsl$USUBJID)

# Estrazione dei Top 10 AETERM e calcolo intervalli di confidenza Clopper-Pearson
top10_ae <- adae %>%
  filter(!is.na(AETERM)) %>%
  group_by(AETERM) %>%
  summarise(
    n_pts = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  slice_max(order_by = n_pts, n = 10, with_ties = FALSE) %>%
  rowwise() %>%
  mutate(
    # Calcolo intervallo di confidenza 95% Clopper-Pearson
    ci_res = list(binom.test(n_pts, N_subjects, conf.level = 0.95)),
    pct = (n_pts / N_subjects) * 100,
    ci_low = ci_res$conf.int[1] * 100,
    ci_high = ci_res$conf.int[2] * 100
  ) %>%
  ungroup() %>%
  mutate(AETERM = reorder(AETERM, pct)) # Ordina l'asse Y per percentuale crescente

# Creazione del grafico con intervalli di confidenza
plot2 <- ggplot(top10_ae, aes(x = pct, y = AETERM)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(top10_ae$ci_high) + 5)
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray20", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black")
  )

# Salvataggio in formato PNG
ggsave("question3/plot2_top10_ae.png", plot = plot2, width = 8, height = 6, dpi = 300)

print("Plot 2 creato con successo!")
