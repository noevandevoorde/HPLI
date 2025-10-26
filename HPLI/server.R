# server.R

# Noé Vandevoorde
# octobre 2025


#### Server ####################################################################

server <- function(input, output, session) {
  
  df <- data
  
  ###### Populate filter lists (runs once at app startup) ######
  
  observeEvent(TRUE, {
    # Substance origin filter
    updateSelectInput(
      session,
      "substance_origins",
      choices = unique(df$compound_origin) |>
        sort())
    # Substance type filter
    updateSelectInput(
      session,
      "substance_types",
      choices = unique(df$main_compound_type) |>
        sort())
    # Substance family filter
    updateSelectInput(
      session,
      "substance_groups",
      choices = unique(main_compound_group) |>
        pull())
  },
  once = TRUE)
  
  ###### Populate list of substance (reacts on filters) ######
  
  substance_choices <- reactive({
    df_filtered <- df
    
    # Filter by origin only if an origin is selected
    if (!is.null(input$substance_origins) && length(input$substance_origins) > 0) {
      df_filtered <- df_filtered |>
        filter(compound_origin %in% input$substance_origins)
    }
    
    # Filter by type only if a type is selected
    if (!is.null(input$substance_types) && length(input$substance_types) > 0) {
      df_filtered <- df_filtered |>
        filter(main_compound_type %in% input$substance_types)
    }
    
    # Filter by family only if a family is selected
    if (!is.null(input$substance_groups) && length(input$substance_groups) > 0) {
      df_filtered <- df_filtered |>
        filter(str_detect(tolower(compound_group), input$substance_groups))
    }
    
    # Format final substance list
    df_filtered |>
      pull(compound) |>
      unique() |>
      sort()
  })
  
  ###### Selected substance based on user choice ######
  observe({
    choices <- substance_choices()
    selected <- isolate(input$substance_single)
    if (!is.null(selected)) selected <- selected[selected %in% choices]
    updateSelectInput(session, "substance_single",
                      choices = choices,
                      selected = selected)
    updateSelectInput(session, "substances_compare", choices = choices)
  })
  
  # If current selection is no longer valid (e.g. after a new filter is applied), clear it
  observe({
    valid_choices <- substance_choices()
    current <- input$substance_single
    if (!is.null(current) && !current %in% valid_choices) {
      updateSelectInput(session, "substance_single", selected = "")
    }
  })
  
  ###### Reduce data based on selected substance ######
  single_substance_data <- reactive({
    req(input$substance_single)
    df <- data
    df[df$compound == input$substance_single, ]
    
    plot_data <- data[data$compound == input$substance_single, ] |>
      mutate(attribute = factor(attribute, levels = metric_names)) |>
      arrange(attribute) |>
      mutate(
        truncated = if_else(index_value > trunk, trunk, index_value),  # Cap at 1.6
        is_truncated = index_value > trunk,                         # Flag truncated points (shortcut)
        label = if_else(is_truncated, as.character(round(index_value, 1)), NA_character_),  # Full value label
        label_with_zeros = if_else(is_truncated, label, "0"))
  })
  
  ###### Display substance data ######
  output$substance_info <- renderText({
    # Make it reactive to both inputs
    choices <- substance_choices()
    selected <- input$substance_single
    # Clear out if nothing selected or selection invalid
    if (is.null(selected) || selected == "" || !selected %in% choices) {
      return("")
    }
    # Normal case (if a substance is selected)
    data_sub <- single_substance_data()
    if(nrow(data_sub) > 0) {
      paste0(
        "Substance: ", input$substance_single, "\n\n",
        "      CAS: ", unique(data_sub$cas), "\n",
        "Main type: ", unique(data_sub$compound_origin), " ", unique(data_sub$main_compound_type), "\n",
        " Sub type: ", unique(data_sub$sub_compound_type), "\n",
        "   Family: ", unique(data_sub$compound_group), "\n\n",
        "     Load: ", round(unique(data_sub$sum), 3)
      )
    }
  })
  
  ###### Display HPL visualisation graphh ######
  output$rose_plot <- renderPlot({
    req(input$substance_single)
    make_rose_plot2(input$substance_single, single_substance_data())
  })
  
  ###### Display HPL data table ######
  output$score_details <- DT::renderDataTable({
    req(input$substance_single)
    data_sub <- single_substance_data()
    display_data <- data_sub |>
      mutate(quality = ifelse(!is.na(missing), missing, quality)) |>
      select(sub_compartment, attribute, value_chr, quality, index_value, weighted_index) |>
      rename(compartment = sub_compartment,
             metric = attribute,
             `metric value` = value_chr,
             `data quality` = quality,
             `metric load (unweighted)` = index_value,
             `metric load (weighted)` = weighted_index)
    DT::datatable(
      display_data,
      options = list(scrollX = TRUE,
                     pageLength = 20,
                     dom = 't')) # only show the table
  })
}
