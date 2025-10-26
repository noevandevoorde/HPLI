# ui.R

# Noé Vandevoorde
# octobre 2025


#### User Interface ############################################################

ui <- dashboardPage(
  
  
  ###### Header ##################################################################
  
  dashboardHeader(title = "Harmonised PLI"),
  
  
  ###### Sidebar #################################################################
  
  dashboardSidebar(
    
    ### Menu ###
    
    sidebarMenu(
      menuItem("  Single Substance View", tabName = "single", icon = icon("chart-pie"))
      ## Other tabs could be added, such as a comparison View, detailed data table,
      ## product level visualisation/info, data sources (PPDB) and link to the publication.
      ## E.g.: (idea to develop, or not … not linked to any further UI or Server code)
      # ,menuItem("  Comparison View", tabName = "compare", icon = icon("balance-scale"))
      # ,menuItem("  Data Table", tabName = "data", icon = icon("table"))
      # ,menuItem("  Sources", tabName = "source", icon = icon("magnifying-glass-plus"))
    ),
    
    ### Credit info ###
    
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      HTML("<a href='https://sytra.be' target='_blank'>sytra.be</a><br>
            © Sytra, Noé Vandevoorde (2025)<br>
            Last updated: Sept 2025<br>
            <a href='mailto:noe.vandevoorde@uclouvain.be'>noe.vandevoorde@uclouvain.be</a>")
    )
  ),
  
  
  ###### Body ####################################################################
  
  dashboardBody(
    tabItems(
      
      ###### Body: Single Substance Tab ######
      
      tabItem(tabName = "single",
              fluidRow(
                
                # Substance selection
                box(title = "Substance Selection",
                    status = "primary", # "info",
                    solidHeader = TRUE,
                    width = 4,
                    
                    # Filter options
                    selectizeInput("substance_origins",
                                   label = NULL,
                                   choices = NULL, # populated from data in the server
                                   multiple = TRUE,
                                   selected = NULL,
                                   options = list(placeholder = "Filter by origin")),
                    selectizeInput("substance_types",
                                   label = NULL,
                                   choices = NULL,  # populated from data in the server
                                   multiple = TRUE,
                                   selected = NULL,
                                   options = list(placeholder = "Filter by type")),
                    selectizeInput("substance_groups",
                                   label = NULL,
                                   choices = NULL,  # populated from data in the server
                                   multiple = TRUE,
                                   selected = NULL,
                                   options = list(placeholder = "Filter by family")),
                    # Substance selection
                    selectInput("substance_single",
                                "Select Substance:",
                                choices = NULL, # populated from data in the server
                                selected = NULL)
                ),
                
                # Substance information
                box(title = "Substance Information",
                    status = "primary", # "info",
                    solidHeader = TRUE,
                    width = 8,
                    verbatimTextOutput("substance_info")
                )
              ),
              
              # HPL graph
              fluidRow(
                box(title = "Harmonised Pesticide Load Score",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("rose_plot",
                               height = "500px")
                )
              ),
              
              # Data table
              fluidRow(
                box(title = "Load Score Details",
                    status = "primary", # "info", "success", "warning",
                    solidHeader = TRUE,
                    width = 12,
                    DT::dataTableOutput("score_details")
                )
              )
      )
      
      ###### Body: Other Tab ######
      
      # Other tabs to be added here, e.g.:
      
    )
  )
)
