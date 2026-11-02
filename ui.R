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

############################################################
# User Interface
############################################################

ui <- navbarPage(

  ##########################################################
  # Application title and theme
  ##########################################################
  
  title = "FCS-Merge",
  
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
      tags$div(
        class = "loader-spinner"
      )
    ),
  
    
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
    tags$script("
Shiny.addCustomMessageHandler(
 'start_report_download',
 function(message){

   $('#report_spinner').show();

   setTimeout(function(){

     $('#report_spinner').hide();

   },5000);

 });
"),
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
    table-layout: fixed !important;
  }

  table.dataTable th,
  table.dataTable td {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
    

    
    
    .param_check {
      transform:scale(1.25);
      cursor:pointer;
    }
    
    
    0
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
  background-color: #e5e7eb !important;
  border-color: #d1d5db !important;
  color: #374151 !important;
}

.btn-raw:hover {
  background-color: #d1d5db !important;
  color: #111827 !important;
}
    
    /* ===========================
   GLOBAL LOADING SPINNER
=========================== */

#loading-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(255,255,255,0.65);
  z-index: 99999;
  display: none;
  align-items: center;
  justify-content: center;
}


.loader-spinner {
  width: 70px;
  height: 70px;
  border: 8px solid #e5e7eb;
  border-top: 8px solid #3b82f6;
  border-radius: 50%;
  animation: spin-loader 0.8s linear infinite;
}


@keyframes spin-loader {
  0% {
    transform: rotate(0deg);
  }

  100% {
    transform: rotate(360deg);
  }
}
    
    
    "))
    
  ),
  
  
  
  
  ##########################################################
  # TAB 1 : Upload FCS files
  ##########################################################
  
  
  tabPanel(
    
    title = "Upload FCS",
    icon = icon("folder-open"),
    
    
    br(),
    
    
    # RAW folder selection
    fluidRow(
      
      column(
        
        12,
        
        box(
          
          width = 12,
          
          title = "Raw FCS folder",
          status = "primary",
          solidHeader = TRUE,
          
          
          shinyDirButton(
            "raw_dir",
            "Choose RAW folder",
            "Select",
            class = "btn-raw"
          ),
          
          
          br(),
          br(),
          
          
          verbatimTextOutput(
            "raw_path"
          )
          
        )
        
      )
      
    ),
    
    
    
    # Correction methods selection
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
          
          
          uiOutput(
            "methods_ui"
          )
          
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
  
  
  tabPanel(
    title = "Merge",
    icon = icon("code-merge"),
    
    br(),
    
    uiOutput("analysis_summary"),
    
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
    
    fluidRow(
      column(
        width = 4,
        
        box(
          width = 12,
          title = "Merged FCS file",
          status = "primary",
          solidHeader = TRUE,
          
          uiOutput("merge_report_ui"),
          
          selectInput(
            "viz_fcs",
            "Choose merged FCS",
            choices = NULL
          ),
          
          uiOutput("viz_parameters")
        )
      ),
      
      column(
        width = 8,
        box(
          width = 12,
          title = "2D density plot — flowDensity",
          status = "info",
          solidHeader = TRUE,
          checkboxInput(
            "apply_arcsinh",
            "Apply arcsinh transformation",
            value = FALSE
          ),
          
          numericInput(
            "arcsinh_cofactor",
            "Arcsinh cofactor",
            value = 150,
            min = 0.001,
            step = 1
          ),
          
          uiOutput("density_axes"),
          fluidRow(
            column(
              3,
              numericInput(
                "xlim_min",
                "X-axis minimum",
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "xlim_max",
                "X-axis maximum",
                value = 100000
              )
            ),
            column(
              3,
              numericInput(
                "ylim_min",
                "Y-axis minimum",
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "ylim_max",
                "Y-axis maximum",
                value = 100000
              )
            )
          ),
          
          checkboxInput(
            "show_contour",
            "Show density contours",
            value = FALSE
          ),
          
          checkboxInput(
            "density_overlay",
            "Show marginal density curves",
            value = FALSE
          ),
          
          plotOutput(
            "plot_dens",
            width = "600px",
            height = "600px",
          )
        ),
        
        box(
          width = 12,
          title = "Overlay histogram",
          status = "info",
          solidHeader = TRUE,
          
          DTOutput("summaryTable"),
          
          plotOutput(
            "overlay_hist",
            height = "500px",
            
          )
        )
      )
    )
  )
  

)