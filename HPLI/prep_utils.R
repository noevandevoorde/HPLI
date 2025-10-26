# prep_utils.R

# Noé Vandevoorde
# octobre 2025


#### Metric set ################################################################

# Data are extracted from the PPDB (version May 2024), completed with proxy data
# as indicated in Vandevoorde et al. (2026). See Vandevoorde (2025) for more details.

# Environmental fate metrics
metrics_envfate <- c(
  "Soil persistence (DT50 soil)"   = "soil_dt50_completed"
  ,"Water persistence (DT50 water)" = "water_dt50"
  ,"Surface water transfer (Kfoc)"  = "kfoc_completed"
  ,"Groundwater transfer (GUS)"     = "gus"
  ,"Aquatic biome transfer (BCF)"   = "bcf_completed"
  ## terrestrial bioaccumulation
)

# Ecotoxicity (terrestrial) metrics
metrics_ecoterr <- c(
  "Birds (acute oral)"                   = "bird_ld50"
  # ,"Birds (chronic oral)"                 = "bird_noel"
  ## bumble bees acute
  , "Earthworms (acute soil)"             = "earthworm_lc50"
  # ,"Earthworms (chronic soil)"            = "earthworm_noec"
  ,"Honeybees (acute oral/contact/other)" = "honeybees_ld50_all"
  ## honeybees chronic
  ## lacewings acute
  ## ladybird chronic
  ,"Mammals (acute oral)"                 = "mammals_ld50_oral"
  # ,"Mammals (chronic oral)"               = "mammals_noael"
  ## mason bee acute
  # ,"Parasitic wasps (acute contact)"      = "parasiticwasps"
  # ,"Predatory mites (acute contact)"      = "predatorymites"
  ## soil microorganisms
  ## springtales acute
  ## springtales chronic
  ## terrestrial plants
)

# Ecotoxicity (aquatic) metrics
metrics_ecoaqua <- c(
  "Algae (acute aqueous)"               = "algae_ec50"
  # ,"Aquatic plants (acute aqueous)"      = "algae_noec"
  # ,"aquaticplants_ec50"
  ,"Aquatic invertebrates (acute aq.)"   = "aquaticinvertebrates_ec50"
  ,"Aquatic invertebrates (chronic aq.)" = "aquaticinvertebrates_noec"
  ,"Fish (acute aqueous)"                = "fish_lc50"
  ,"Fish (chronic aqueous)"              = "fish_noec"
  ## sediment dwelling organisms acute
  ## sediment dwelling organisms chronic
)

# Human health metrics
metrics_humheal <- c(
  "Mammals (acute dermal)"             = "mammals_ld50_dermal"
  ,"Mammals (acute inhalation)"         = "mammals_lc50_inhalation"
  ,"Humans (carcinogenicity)"           = "carcinogenicity"
  ,"Humans (cholinesterase inhibition)" = "cholinesteraseinhibition"
  # ,"endocrinedisruption"
  # ,"Humans (genotoxicity)"              = "genotoxicity_worst"
  ,"Humans (neurotoxicity)"             = "neurotoxicity"
  ,"Humans (reprotoxicity)"             = "reprotoxicity"
)

metrics <- c(
  metrics_envfate,
  metrics_ecoterr,
  metrics_ecoaqua,
  metrics_humheal
)

metric_names <- names(metrics)


#### Color palette #############################################################

metric_colors <- c(
  # Environmental fate
  "Soil persistence (DT50 soil)"         = "#ffffcc",
  "Water persistence (DT50 water)"       = "#c2e699",
  "Surface water transfer (Kfoc)"        = "#78c679",
  "Groundwater transfer (GUS)"           = "#31a354",
  "Aquatic biome transfer (BCF)"         = "#006837",
  # Ecotoxicity (terrestrial)
  "Birds (acute oral)"                   = "#feedde",
  "Earthworms (acute soil)"              = "#fdbe85",
  "Honeybees (acute oral/contact/other)" = "#fd8d3c",
  "Mammals (acute oral)"                 = "#d94701",
  # Ecotoxicity (aquatic)
  "Algae (acute aqueous)"                = "#eff3ff",
  "Aquatic invertebrates (acute aq.)"    = "#bdd7e7",
  "Aquatic invertebrates (chronic aq.)"  = "#6baed6",
  "Fish (acute aqueous)"                 = "#3182bd",
  "Fish (chronic aqueous)"               = "#08519c",
  # Human health
  "Mammals (acute dermal)"               = "#feebe2",
  "Mammals (acute inhalation)"           = "#fcc5c0",
  "Humans (carcinogenicity)"             = "#fa9fb5",
  "Humans (cholinesterase inhibition)"   = "#f768a1",
  "Humans (neurotoxicity)"               = "#c51b8a",
  "Humans (reprotoxicity)"               = "#7a0177"
)


#### Data loading and cleaning #################################################

###### HPL data loading and cleaning ######

data <- readxl::read_excel("HPLI/data.xlsx", sheet = "HPLI_load",
                           col_types = c(rep("text", 8), "numeric", "text", rep("numeric", 4))) |>
  left_join(readxl::read_excel("HPLI/data.xlsx", sheet = "HPLI_detail")) |>
  left_join(readxl::read_excel("HPLI/data.xlsx", sheet = "HPLI_info"),
            relationship = "many-to-many") |>
  mutate(
    main_compound_type = str_replace(
      str_trim(compound_type), "^([^,]+),\\s*(.+)$", "\\1"),
    sub_compound_type = if_else(
      str_detect(compound_type, ","),                                           # only if comma exists
      str_replace(str_trim(compound_type), "^([^,]+),\\s*(.+)$", "\\2"),        # empty string if no comma
      ""),
    compound_origin = ifelse(
      compound_origin == "Natural; Mixture",
      "Natural (mixture)",
      compound_origin))

###### ShinyApp filters ######

keywords_to_remove <- unique(c(
  data$main_compound_type,
  "PGR", "IRG",
  "biocide", "pesticide", "compound", "substance",
  "herbicie", "herbicde", "insecticids", "insectide")) |>
  tolower()

main_compound_group <- data |>
  select(compound_group) |>
  mutate(main_compound_group = str_split(compound_group, ";\\s*")) |>
  unnest(main_compound_group) |>
  select(-compound_group) |>
  mutate(main_compound_group = str_remove_all(
    tolower(main_compound_group),
    paste0("\\b(", paste(keywords_to_remove, collapse = "|"), ")\\b"))) |>
  mutate(main_compound_group = str_trim(main_compound_group)) |>
  unique() |>
  arrange(main_compound_group)

substance_types <- str_extract(data$compound_type, "^[^,]+") |>
  unique()

substance_groups <- str_extract(data$compound_group, "^[^,]+") |>
  unique()

#### Diagram function ##########################################################

trunk <- 1.5
x_max <- 1

make_rose_plot2 <- function(compound_name, data) {
  
  # Data to plot
  plot_data <- data |>
    filter(compound == compound_name) |>
    mutate(attribute = factor(attribute, levels = metric_names)) |>
    mutate(attribute_num = as.numeric(factor(attribute, levels = metric_names)))
  
  # Dummy data for background concentric circles
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(c("Low to moderate", "Moderate to high", "High to very high"),
                  levels = c("Low to moderate", "Moderate to high", "High to very high")))
  
  # Plot
  ggplot(
    plot_data,
    aes(x = 0, #attribute,
        y = truncated,
        fill = attribute)) +
    # Concentric circles
    geom_rect(
      data = background,
      aes(xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = band),
      alpha = 0.5,
      inherit.aes = FALSE) +
    scale_fill_manual(
      name = "Load",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c("Low to moderate" = "white",
                 "Moderate to high" = "gray85",
                 "High to very high" = "gray70"),
      guide = guide_legend(
        override.aes = list(
          color = "gray70",
          size  = 0.5))) +
    # Compartment divisions
    geom_segment(
      data = data.frame(x = c(0, 1/3, 1/2, 2/3)),
      aes(x = x,
          xend = x,
          y = 0,
          yend = 1.5),
      colour = "gray65",
      linewidth = 0.5,
      inherit.aes = FALSE) +
    # New fill layer for the metrics
    ggnewscale::new_scale_fill() +
    # Metrics (existing values under 1.5)
    geom_rect(
      aes(xmin = xmin,
          xmax = xmax,
          ymin = 0,
          ymax = truncated,
          fill = attribute),
      color = "black",
      inherit.aes = FALSE) +
    # Metrics (existing values over 1.5)
    geom_rect(
      data = plot_data |>
        filter(is_truncated),
      aes(xmin = xmin,
          xmax = xmax,
          ymin = trunk,
          ymax = trunk + 0.075),
      fill = "red",
      inherit.aes = FALSE) +
    geom_text(
      aes(x = xmid,
          y = trunk-.2,
          label = label),
      size = 3.5,
      color = "#8B0000",
      fontface = "bold") +
    # Metrics (missing data)
    ggpattern::geom_rect_pattern(
      data = plot_data |>
        filter(missing == "*"),
      aes(xmin = xmin,
          xmax = xmax,
          ymin = 0,
          ymax = truncated),
      fill = "transparent",
      color = NA,
      pattern_fill = "black",
      pattern_density = 0.025,
      pattern_spacing = 0.02,
      pattern_angle = 60,
      pattern = "stripe",
      inherit.aes = FALSE) +
    # Data quality
    # Plain text: qualifier (<,=,>)
    geom_text(
      data = plot_data |>
        filter(!is.na(qualifier)),
      aes(x = xmid,
          y = truncated + 0.1,
          label = qualifier),
      size = 3.5,
      color = "black") +
    # Plain text: data quality (1 to 5)
    geom_text(
      data = plot_data |>
        filter(!is.na(quality)),
      aes(x = xmid,
          y = truncated + 0.25,
          label = quality),
      size = 3,
      color = "black") +
    # Phantom text to keep graph size and alignment
    geom_text(
      aes(x = xmid,
          y = trunk + 0.2,
          label = if_else(is.na(label), "0", "")),
      size = 3.5,
      color = "white",
      fontface = "bold") +
    # Legend
    scale_fill_manual(
      values = metric_colors,
      guide = guide_legend(ncol = 1)) +
    labs(title = NULL,
         x = NULL,
         y = NULL,
         fill = "Metrics") +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0)
}
