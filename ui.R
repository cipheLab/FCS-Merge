
############################################################
# FCS-Merge Shiny Application
# User Interface
############################################################

library(shiny)
library(shinyFiles)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shinyjs)

############################################################
# Shiny global options
############################################################

options(
  expressions = 5e5,
  shiny.maxRequestSize = 100 * 1024^3
)

useShinyjs()

app_mode <- Sys.getenv("FCS_MERGE_MODE", "upload")

############################################################
# User Interface
############################################################

ui <- navbarPage(
  
  ##########################################################
  # Application title and theme
  ##########################################################
  
  title = "FCS-Merge",
  
  id = "main_navbar",
  
  theme = shinytheme("flatly"),
  
  ##########################################################
  # Custom JavaScript and CSS
  ##########################################################
  
  header = tagList(
    
    # Retrieve checked parameters from DataTable
    tags$script(HTML("
      Shiny.addCustomMessageHandler(
        'get_checked_parameters',
        function(message){

          var values = [];

          $('.param_check:checked').each(function(){

            values.push(
              $(this).closest('tr')
              .find('td:eq(1)')
              .text()
              .trim()
            );

          });

          Shiny.setInputValue(
            'checked_parameters',
            values,
            {priority:'event'}
          );

        }
      );
    ")),
    
    # Loading spinner overlay
    tags$div(
      id = "loading-overlay",
      tags$div(class = "loader-spinner")
    ),
    
    # Loading events
    tags$script(HTML("
      $(document).on('shiny:busy', function(){

        $('#loading-overlay')
          .css('display','flex');

      });

      $(document).on('shiny:idle', function(){

        $('#loading-overlay')
          .fadeOut(200);

      });
    ")),
    
    # Report download spinner
    tags$script(HTML("
      Shiny.addCustomMessageHandler(
        'start_report_download',
        function(message){

          $('#report_spinner').show();

          setTimeout(function(){

            $('#report_spinner').hide();

          },5000);

        }
      );
    ")),
    
    # Move Download button to navbar
    tags$script(HTML("
      function moveDownloadButton(){

        var download = $('#download_fcs_ui');
        var navbar = $('#main_navbar .navbar-collapse');

        if(download.length && navbar.length){

          download.appendTo(navbar);

        }

      }

      $(document).on('shiny:connected', function(){

        setTimeout(moveDownloadButton, 500);

      });

      $(document).on('shiny:value', function(){

        setTimeout(moveDownloadButton, 100);

      });
    ")),
    
    # Global application styling
    tags$style(HTML("

      /* ===========================
         GLOBAL STYLE
      =========================== */

      body {
        background-color:#f7f9fc;
        font-family:'Segoe UI', sans-serif;
      }

      /* ===========================
         NAVBAR STYLE
      =========================== */

      .navbar {
        background-color:#243447 !important;
        border:none;
        box-shadow:0 2px 8px rgba(0,0,0,0.15);
        min-height:50px !important;
        margin-bottom:20px;
      }

      .navbar-brand {
        font-weight:600;
        font-size:22px;
        color:white !important;
      }

      .navbar-nav > li > a {
        color:#e8edf3 !important;
      }

      .navbar-nav > li.active > a {
        background-color:#3b82f6 !important;
        color:white !important;
      }

      /* ===========================
         NAVBAR DOWNLOAD BUTTON
      =========================== */

      #main_navbar .navbar-collapse {
        position:relative;
      }

      #download_fcs_ui {
        position:absolute;
        right:15px;
        top:0;
        height:50px;
        display:flex;
        align-items:center;
        margin:0;
        z-index:1000;
      }

      #download_fcs_ui .btn {
        background-color:#10b981 !important;
        color:white !important;
        border:none !important;
        border-radius:8px !important;
        padding:7px 16px;
        font-weight:600;
        margin:0;
      }

      #download_fcs_ui .btn:hover {
        background-color:#059669 !important;
      }

      /* Reserve space for Download */
      @media (min-width:768px) {

        #main_navbar .navbar-nav {
          margin-right:130px;
        }

      }

      /* ===========================
         BOX COMPONENTS
      =========================== */

      .box {
        border-radius:14px !important;
        border:none !important;
        box-shadow:0 4px 12px rgba(0,0,0,0.08);
      }

      .box-header {
        border-radius:14px 14px 0 0 !important;
        font-weight:600;
      }

      .box-body {
        padding:22px !important;
      }

      /* ===========================
         BUTTON STYLE
      =========================== */

      .btn {
        border-radius:10px !important;
        font-weight:500;
        transition:0.2s;
      }

      .btn:hover {
        transform:translateY(-1px);
      }

      .btn-success {
        background-color:#10b981 !important;
        border:none !important;
      }

      .btn-primary {
        background-color:#3b82f6 !important;
        border:none !important;
      }

      /* ===========================
         DATATABLE STYLE
      =========================== */

      .dataTables_wrapper {
        font-size:13px;
      }

      table.dataTable {
        table-layout:fixed !important;
      }

      table.dataTable th,
      table.dataTable td {
        overflow:hidden;
        text-overflow:ellipsis;
        white-space:nowrap;
      }

      .param_check {
        transform:scale(1.25);
        cursor:pointer;
      }

      /* ===========================
         TEXT STYLE
      =========================== */

      h3 {
        color:#243447;
        font-weight:600;
      }

      h4 {
        color:#374151;
      }

      /* ===========================
         STATUS PANEL
      =========================== */

      .merge-status {
        background:#eef6ff;
        padding:15px;
        border-radius:12px;
      }

      .btn-raw {
        background-color:#e5e7eb !important;
        border-color:#d1d5db !important;
        color:#374151 !important;
      }

      .btn-raw:hover {
        background-color:#d1d5db !important;
        color:#111827 !important;
      }

      /* ===========================
         GLOBAL LOADING SPINNER
      =========================== */

      #loading-overlay {
        position:fixed;
        top:0;
        left:0;
        width:100%;
        height:100%;
        background:rgba(255,255,255,0.65);
        z-index:99999;
        display:none;
        align-items:center;
        justify-content:center;
      }

      .loader-spinner {
        width:70px;
        height:70px;
        border:8px solid #e5e7eb;
        border-top:8px solid #3b82f6;
        border-radius:50%;
        animation:spin-loader 0.8s linear infinite;
      }

      @keyframes spin-loader {
        0% {
          transform:rotate(0deg);
        }

        100% {
          transform:rotate(360deg);
        }
      }

      /* ===========================
         RESPONSIVE NAVBAR
      =========================== */

      @media (max-width:767px) {

        #download_fcs_ui {
          position:relative;
          right:auto;
          top:auto;
          height:auto;
          margin:8px 15px;
        }

        #main_navbar .navbar-nav {
          margin-right:0;
        }

      }

    ")),
    
    # Download UI placeholder
    uiOutput("download_fcs_ui")
    
  ),
  
  ##########################################################
  # TAB 1 : Upload FCS files
  ##########################################################
  
  tabPanel(
    
    title = "Upload FCS",
    icon = icon("folder-open"),
    
    br(),
    
    fluidRow(
      
      column(
        12,
        
        box(
          width = 12,
          title = "Raw FCS files",
          status = "primary",
          solidHeader = TRUE,
          
          if (identical(app_mode, "local")) {
            
            tagList(
              
              shinyDirButton(
                "raw_dir",
                "Choose RAW folder",
                "Select",
                class = "btn-raw"
              ),
              
              br(),
              br(),
              
              verbatimTextOutput("raw_path")
              
            )
            
          } else {
            
            tagList(
              
              fileInput(
                "raw_files",
                "Choose RAW FCS files",
                multiple = TRUE,
                accept = ".fcs"
              ),
              
              verbatimTextOutput("raw_path")
              
            )
            
          }
          
        )
        
      )
      
    ),
    
    fluidRow(
      
      column(
        
        12,
        
        box(
          
          width = 12,
          
          title = "Processed FCS folder",
          status = "info",
          solidHeader = TRUE,
          
          actionButton(
            "add_method",
            "Add process",
            icon = icon("plus"),
            class = "btn-success"
          ),
          
          br(),
          br(),
          
          uiOutput("methods_container")
          
        )
        
      )
      
    ),
    
    fluidRow(
      
      column(
        width = 12,
        style = "margin-top:15px;",
        uiOutput("validate_button")
      )
      
    )
    
  ),
  
  ##########################################################
  # TAB 2 : Parameter selection
  ##########################################################
  
  tabPanel(
    
    title = "Parameters",
    icon = icon("sliders"),
    
    fluidRow(
      
      column(
        12,
        
        uiOutput("validate_parameters_button")
        
      )
      
    )
    
  ),
  
  ##########################################################
  # TAB 3 : Merge
  ##########################################################
  
  tabPanel(
    
    title = "Merge",
    icon = icon("code-merge"),
    
    br(),
    
    uiOutput("analysis_summary"),
    
    br(),
    
    uiOutput("merge_button_ui"),
    
    br(),
    
    uiOutput("merge_button_ui"),
    
    br()
    
  ),
  
  ##########################################################
  # TAB 4 : Visualization
  ##########################################################
  
  tabPanel(
    
    title = "Visualization",
    icon = icon("chart-line"),
    
    tags$head(
      
      tags$style(HTML("

        .viz-header {
          padding:22px 26px;
          margin-bottom:22px;
          background:linear-gradient(120deg, #17365D, #2874A6);
          color:white;
          border-radius:12px;
          box-shadow:0 3px 12px rgba(0,0,0,0.10);
        }

        .viz-header h3 {
          margin-top:0;
          font-weight:600;
          color:white;
        }

        .viz-box {
          border-radius:10px !important;
          box-shadow:0 2px 10px rgba(0,0,0,0.07);
          border:1px solid #e8edf2;
        }

        .viz-box .box-header {
          border-radius:10px 10px 0 0;
          padding:13px 16px;
        }

        .viz-box .box-title {
          font-weight:600;
        }

        .viz-box .box-body {
          padding:20px;
        }

        .viz-section-title {
          color:#34495e;
          font-weight:600;
          font-size:14px;
          margin:12px 0 15px 0;
        }

        .viz-download {
          width:100%;
          margin-top:10px;
          padding:10px;
          font-weight:600;
          border-radius:7px;
        }

        .viz-plot {
          background:#ffffff;
          border-radius:8px;
          padding:8px;
        }

        .viz-help {
          color:#8492a6;
          font-size:12px;
          margin-top:-5px;
          margin-bottom:12px;
        }

        .viz-box .form-group {
          margin-bottom:16px;
        }

      "))
      
    ),
    
    # Visualization header
    fluidRow(
      
      column(
        width = 12,
        
        div(
          class = "viz-header",
          
          h3(
            icon("chart-line"),
            " FCS Data Visualization"
          )
          
        )
        
      )
      
    ),
    
    # Visualization settings
    fluidRow(
      
      column(
        
        width = 12,
        
        box(
          
          width = 12,
          
          title = tagList(
            icon("sliders-h"),
            " Visualization settings"
          ),
          
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          class = "viz-box",
          
          fluidRow(
            
            column(
              
              width = 5,
              
              selectInput(
                "viz_fcs",
                "Select merged FCS file",
                choices = NULL,
                width = "100%"
              )
              
            ),
            
            column(
              
              width = 3,
              
              checkboxInput(
                "apply_arcsinh",
                "Apply arcsinh transformation",
                value = FALSE
              )
              
            ),
            
            column(
              
              width = 2,
              
              numericInput(
                "arcsinh_cofactor",
                "Arcsinh cofactor",
                value = 150,
                min = 0.001,
                step = 1,
                width = "100%"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    # Plots
    fluidRow(
      
      # Histogram
      column(
        
        width = 6,
        
        box(
          
          width = 12,
          
          title = tagList(
            icon("chart-bar"),
            " Histogram"
          ),
          
          status = "primary",
          solidHeader = TRUE,
          class = "viz-box",
          
          p(
            class = "viz-help",
            "Compare fluorescence intensity distributions across methods."
          ),
          
          uiOutput("viz_parameters"),
          
          div(
            class = "viz-plot",
            
            plotOutput(
              "overlay_hist",
              height = "auto",
              width = "100%"
            )
            
          )
          
        )
        
      ),
      
      # Density plot
      column(
        
        width = 6,
        
        box(
          
          width = 12,
          
          title = tagList(
            icon("braille"),
            " 2D Density Plot"
          ),
          
          status = "info",
          solidHeader = TRUE,
          class = "viz-box",
          
          p(
            class = "viz-help",
            "Explore the relationship between two parameters."
          ),
          
          uiOutput("density_axes"),
          
          tags$hr(),
          
          h5(
            class = "viz-section-title",
            icon("expand-arrows-alt"),
            " Axis limits"
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              numericInput(
                "xlim_min",
                "X-axis minimum",
                value = 0,
                width = "100%"
              )
              
            ),
            
            column(
              width = 6,
              
              numericInput(
                "xlim_max",
                "X-axis maximum",
                value = 100000,
                width = "100%"
              )
              
            ),
            
            column(
              width = 6,
              
              numericInput(
                "ylim_min",
                "Y-axis minimum",
                value = 0,
                width = "100%"
              )
              
            ),
            
            column(
              width = 6,
              
              numericInput(
                "ylim_max",
                "Y-axis maximum",
                value = 100000,
                width = "100%"
              )
              
            )
            
          ),
          
          checkboxInput(
            "density",
            "Color points by density",
            value = TRUE
          ),
          
          div(
            class = "viz-plot",
            
            plotOutput(
              "plot_dens",
              width = "500px",
              height = "500px"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)
