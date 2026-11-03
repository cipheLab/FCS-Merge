library(shiny)
library(shinyFiles)
library(DT)
library(FlowCIPHE)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)
library(rsconnect)


server <- function(input, output, session){
  uploaded_methods <- reactiveVal(data.frame(
    id = integer(),
    name = character(),
    path = character(),
    stringsAsFactors = FALSE
  ))
  # "local" : sélection de dossiers avec shinyFiles
  # "upload" : fichiers envoyés au serveur Posit
  app_mode <- Sys.getenv("FCS_MERGE_MODE", "upload")
  
  parameter_table_rv <- reactiveVal(NULL)
  merge_summary <- reactiveVal(NULL)
  
  matching_rv <- reactiveVal(NULL)
  selected_parameters_rv <- reactiveVal(NULL)
  parameters_validated <- reactiveVal(FALSE)
  merge_finished <- reactiveVal(FALSE)
  matching_validated <- reactiveVal(FALSE)
  
  raw_files_rv <- reactiveVal(character())
  # ==================================================
  # CROSS-PLATFORM PATHS
  # ==================================================
  
  home_dir <- normalizePath(
    path.expand("~"),
    winslash = "/",
    mustWork = TRUE
  )
  
  app_dir <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
  
  roots <- c(
    Home = home_dir,
    App = app_dir
  )
  
  normalize_dir <- function(path) {
    
    if (
      is.null(path) ||
      length(path) == 0 ||
      is.na(path[1]) ||
      !nzchar(path[1])
    ) {
      return(NA_character_)
    }
    
    normalizePath(
      path[1],
      winslash = "/",
      mustWork = FALSE
    )
  }
  
  # ==================================================
  # RAW FILES
  # ==================================================
  
  if (identical(app_mode, "local")) {
    
    shinyDirChoose(
      input,
      "raw_dir",
      roots = roots
    )
    
    raw_path <- reactive({
      
      req(input$raw_dir)
      
      normalize_dir(
        parseDirPath(roots, input$raw_dir)
      )
      
    })
    
    raw_files <- reactive({
      
      req(raw_path())
      
      list.files(
        raw_path(),
        pattern = "\\.fcs$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      
    })
    
    output$raw_path <- renderText({
      raw_path()
    })
    
  } else {
    
    raw_path <- reactive(NULL)
    
    raw_files <- reactive({
      
      req(input$raw_files)
      
      files <- input$raw_files
      
      files$datapath
      
    })
    
    output$raw_path <- renderText({
      
      req(input$raw_files)
      
      paste(
        nrow(input$raw_files),
        "RAW FCS files uploaded"
      )
      
    })
    
  }

  # ==================================================
  # METHODS STORAGE
  # ==================================================
  
  methods <- reactiveVal(
    data.frame(
      id = integer(),
      suffix = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  )
  get_method_sources <- reactive({
    
    dat <- methods()
    uploaded <- uploaded_methods()
    
    sources <- list()
    
    for (i in seq_len(nrow(dat))) {
      
      id <- dat$id[i]
      suffix <- dat$suffix[i]
      
      if (identical(app_mode, "local")) {
        
        sources[[suffix]] <- dat$path[i]
        
      } else {
        
        files <- uploaded[
          uploaded$id == id,
          ,
          drop = FALSE
        ]
        
        files <- files[
          file.exists(files$path),
          ,
          drop = FALSE
        ]
        
        sources[[suffix]] <- files$path
        
      }
      
    }
    
    sources
    
  })
  
  next_id <- reactiveVal(1)
  
  
  # ==================================================
  # ADD METHOD + DIRECTORY
  # ==================================================
  observeEvent(
    input$add_method,
    {
      
      dat <- methods()
      
      if(nrow(dat) > 0){
        
        for(i in dat$id){
          
          value <- input[[paste0("suffix_", i)]]
          
          if(!is.null(value)){
            dat$suffix[dat$id == i] <- value
          }
          
        }
        
      }
      
   
      id <- next_id()
      next_id(id + 1)
      
      
      dat <- rbind(
        dat,
        data.frame(
          id = id,
          suffix = paste0("process", id),
          path = "",
          stringsAsFactors = FALSE
        )
      )
      
   
      methods(dat)
      
    
      if (identical(app_mode, "local")) {
        
        shinyDirChoose(
          input,
          paste0("method_dir_", id),
          roots = roots
        )
        
        observeEvent(
          input[[paste0("method_dir_", id)]],
          {
            
            folder_input <- input[[paste0("method_dir_", id)]]
            
            if (is.null(folder_input)) {
              return()
            }
            
            path <- normalize_dir(
              parseDirPath(roots, folder_input)
            )
            
            if (is.na(path)) {
              return()
            }
            
            dat <- methods()
            idx <- match(id, dat$id)
            
            if (!is.na(idx)) {
              dat$path[idx] <- path
              methods(dat)
            }
            
          },
          ignoreInit = TRUE
        )
        
        output[[paste0("method_path_", id)]] <- renderText({
          
          dat <- methods()
          idx <- match(id, dat$id)
          
          if (is.na(idx) || !nzchar(dat$path[idx])) {
            return("No folder selected")
          }
          
          dat$path[idx]
          
        })
        
      }
      
      
   

      output[[paste0("method_path_", id)]] <- renderText({
        
        dat <- methods()
        
        idx <- match(id, dat$id)
        
        if(is.na(idx)){
          return("No folder selected")
        }
        
        if(dat$path[idx] == ""){
          return("No folder selected")
        }
        
        dat$path[idx]
        
      })
      
      
    }
  )
  # ==================================================
  # DELETE METHOD
  # ==================================================
  
  observe({
    
    dat <- methods()
    
    
    if(nrow(dat)==0)
      return()
    
    
    for(i in dat$id){
      
      local({
        
        id <- i
        
        observeEvent(
          
          input[[paste0(
            "delete_",
            id
          )]],
          
          {
            
            dat <- methods()
            
            dat <- dat[
              dat$id != id,
              ,
              drop=FALSE
            ]
            
            
            methods(dat)
            
          },
          
          ignoreInit=TRUE
          
        )
        
      })
      
    }
    
  })
  
  observe({
    dat <- methods()
    
    if (nrow(dat) == 0) return()
    
    uploaded <- uploaded_methods()
    
    for (i in seq_len(nrow(dat))) {
      
      id <- dat$id[i]
      input_id <- paste0("method_file_", id)
      
      files <- input[[input_id]]
      
      if (!is.null(files) && nrow(files) > 0) {
        
        for (j in seq_len(nrow(files))) {
          
          idx <- which(
            uploaded$id == id &
              uploaded$name == files$name[j]
          )
          
          new_file <- data.frame(
            id = id,
            name = files$name[j],
            path = files$datapath[j],
            stringsAsFactors = FALSE
          )
          
          if (length(idx) > 0) {
            uploaded[idx, ] <- new_file
          } else {
            uploaded <- rbind(uploaded, new_file)
          }
        }
      }
    }
    
    uploaded_methods(uploaded)
  })

  
  # ==================================================
  # UI METHODS
  # ==================================================
  
  # ==================================================
  # UI METHODS - INSERTION SANS RECREER LES ANCIENS
  # ==================================================
  
  output$methods_container <- renderUI({
    
    dat <- methods()
    
    if (nrow(dat) == 0) {
      return(
        tags$div(
          style = "color:grey; font-style:italic;",
          "No process added yet"
        )
      )
    }
    
    tagList(
      lapply(dat$id, function(id) {
        
        suffix <- dat$suffix[match(id, dat$id)]
        
        fluidRow(
          
          column(
            5,
            
            box(
              width = 12,
              title = paste("Process", id),
              status = "info",
              solidHeader = TRUE,
              
              if (identical(app_mode, "local")) {
                
                tagList(
                  shinyDirButton(
                    paste0("method_dir_", id),
                    "Choose FCS folder",
                    "Select",
                    class = "btn-raw"
                  ),
                  
                  br(),
                  br(),
                  
                  verbatimTextOutput(
                    paste0("method_path_", id)
                  )
                )
                
              } else {
                
                tagList(
                  fileInput(
                    paste0("method_file_", id),
                    "Fichier FCS",
                    multiple = TRUE,
                    accept = ".fcs"
                  ),
                  
                  uiOutput(
                    paste0("method_uploaded_", id)
                  )
                )
                
              }
            )
          ),
          
          column(
            4,
            
            box(
              width = 12,
              title = "Suffix",
              
              textInput(
                paste0("suffix_", id),
                NULL,
                value = suffix
              )
            )
          ),
          
          column(
            2,
            
            br(),
            
            actionButton(
              paste0("delete_", id),
              "",
              icon = icon("trash"),
              class = "btn-danger"
            )
          )
          
        )
      })
    )
  })
  # ==================================================
  # CHECK
  # ==================================================
  
  output$validate_button <- renderUI({
    
    ## ==========================
    ## Verification RAW
    ## ==========================
    
    raw_ok <- length(raw_files()) > 0
    
    ## ==========================
    ## V??rification m??thodes
    ## ==========================
    
    dat <- methods()
    
    if (!raw_ok || nrow(dat) == 0) {
      return(NULL)
    }
    
    all_ok <- TRUE
    
    if (identical(app_mode, "local")) {
      
      all_ok <- all(
        !is.na(dat$path) &
          nzchar(dat$path)
      )
      
    } else {
      
      uploaded <- uploaded_methods()
      
      for (id in dat$id) {
        
        files <- uploaded[
          uploaded$id == id,
          ,
          drop = FALSE
        ]
        
        if (nrow(files) == 0) {
          all_ok <- FALSE
          break
        }
        
      }
      
    }
    
    ## ==========================
    ## Affichage bouton
    ## ==========================
    
    if (isTRUE(all_ok)) {
      
      actionButton(
        "check_files",
        "Validate files",
        icon = icon("check"),
        class = "btn-primary"
      )
      
    } else {
      
      NULL
      
    }
    
  })
 
  match_fcs_files <- function(raw_files, method_sources) {
    
    if (length(raw_files) == 0) {
      stop("No RAW FCS files found.")
    }
    
    raw_files <- raw_files[file.exists(raw_files)]
    
    if (length(raw_files) == 0) {
      stop("No valid RAW FCS files found.")
    }
    
    results <- data.frame(
      raw_file = raw_files,
      stringsAsFactors = FALSE
    )
    
    for (method_name in names(method_sources)) {
      
      source <- method_sources[[method_name]]
      
      # Une source peut être un dossier ou une liste de fichiers
      if (length(source) == 1 && dir.exists(source)) {
        
        method_files <- list.files(
          source,
          pattern = "\\.fcs$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
      } else {
        
        method_files <- source[
          file.exists(source)
        ]
        
        method_files <- method_files[
          grepl("\\.fcs$", method_files, ignore.case = TRUE)
        ]
        
      }
    
      matched <- rep(NA_character_, length(raw_files))
      
      # Aucun fichier méthode disponible
      if (length(method_files) == 0) {
        results[[method_name]] <- matched
        next
      }
      
      method_names <- tools::file_path_sans_ext(
        basename(method_files)
      )
      
      for (i in seq_along(raw_files)) {
        
        raw_name <- tools::file_path_sans_ext(
          basename(raw_files[i])
        )
        
        scores <- stringdist::stringdist(
          raw_name,
          method_names,
          method = "jw"
        )
        
        best <- which.min(scores)
        
        if (
          length(best) > 0 &&
          is.finite(scores[best]) &&
          scores[best] < 0.25
        ) {
          
          matched[i] <- method_files[best]
          
        } else {
          
          raw_fcs <- tryCatch(
            flowCore::read.FCS(
              raw_files[i],
              transformation = FALSE,
              truncate_max_range = FALSE
            ),
            error = function(e) NULL
          )
          
          if (!is.null(raw_fcs)) {
            
            raw_cells <- nrow(flowCore::exprs(raw_fcs))
            
            cell_diff <- vapply(
              method_files,
              function(f) {
                
                fcs <- tryCatch(
                  flowCore::read.FCS(
                    f,
                    transformation = FALSE,
                    truncate_max_range = FALSE
                  ),
                  error = function(e) NULL
                )
                
                if (is.null(fcs)) {
                  return(Inf)
                }
                
                abs(
                  nrow(flowCore::exprs(fcs)) - raw_cells
                )
                
              },
              numeric(1)
            )
            
            if (any(is.finite(cell_diff))) {
              matched[i] <- method_files[which.min(cell_diff)]
            }
            
          }
          
        }
        
      }
      
      results[[method_name]] <- matched
      
    }
    
    results
  }

  output$validate_parameters_button <- renderUI({
    
    
    # Pas encore de fichiers valid??s
    if(!isTRUE(matching_validated()) || is.null(parameter_table_rv())){
      
      return(
        fluidRow(
          
          column(
            12,
            
            box(
              width = 12,
              title = "Parameters selection",
              status = "warning",
              solidHeader = TRUE,
              
              h3(
                icon("exclamation-triangle"),
                " Please upload files first"
              ),
              
              p(
                "Go to the Upload FCS tab, select a RAW folder and add at least one process before choosing parameters."
              )
              
            )
            
          )
          
        )
      )
      
    }
    
    
    # Fichiers OK -> affichage param??tres
    tagList(
      
      p(
        "Select parameters that will be added to RAW FCS files."
      ),
      
      
      DTOutput(
        "parameter_table"
      ),
      
      
      br(),
      
      
      actionButton(
        "use_parameters",
        "Validate selected parameters",
        icon = icon("check"),
        class = "btn-primary"
      )
      
    )
    
  })
  output$merge_button_ui <- renderUI({
    
    req(parameters_validated())
    req(matching_validated())
    
    fluidRow(
      
      column(
        12,
        
        box(
          width = 12,
          title = "",
          status = "success",
          solidHeader = TRUE,
          
          div(
            style="
          text-align:center;
          padding:20px;
          ",
            
            h3(
              icon("circle-check"),
              " Ready to merge"
            ),
            
            p(
              "All selected parameters and files are validated."
            ),
            
            br(),
            
            actionButton(
              "run_merge",
              "Run FCS merge",
              icon = icon("play"),
              class = "btn-success btn-lg"
            )
            
          )
          
        )
        
      )
      
    )
    
  })
  get_fcs_parameters <- function(file) {
    
    if (
      is.null(file) ||
      length(file) != 1 ||
      is.na(file) ||
      !file.exists(file)
    ) {
      return(
        data.frame(
          name = character(),
          desc = character(),
          stringsAsFactors = FALSE
        )
      )
    }
    
    fcs <- flowCore::read.FCS(
      file,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    data <- fcs@parameters@data
    
    data.frame(
      name = data$name,
      desc = data$desc,
      stringsAsFactors = FALSE
    )
  }
  observeEvent(input$check_files, {
    
    tryCatch({
      
      raw_file_list <- raw_files()
      dat <- methods()
      
      req(length(raw_file_list) > 0)
      req(nrow(dat) > 0)
      
      # Récupérer les suffixes saisis
      for (i in dat$id) {
        value <- input[[paste0("suffix_", i)]]
        
        if (!is.null(value)) {
          dat$suffix[dat$id == i] <- value
        }
      }
      
      # Vérifier que les suffixes sont renseignés et uniques
      validate(
        need(
          all(nzchar(trimws(dat$suffix))),
          "Every process must have a suffix."
        ),
        need(
          !anyDuplicated(dat$suffix),
          "Each process must have a unique suffix."
        )
      )
      
      methods(dat)
      
      # Récupérer les fichiers ou dossiers des méthodes
      method_paths <- get_method_sources()
      
      # Mettre les suffixes à jour comme noms de liste
      names(method_paths) <- dat$suffix
      
      # Faire correspondre les fichiers RAW et méthode
      matching <- match_fcs_files(
        raw_file_list,
        method_paths
      )
      
      matching_rv(matching)
      matching_validated(TRUE)
      
      # Choisir un fichier RAW pour identifier les paramètres
      raw_file <- matching$raw_file[1]
      
      param_list <- list()
      param_list$RAW <- get_fcs_parameters(raw_file)
      
      # Récupérer les paramètres des fichiers méthode correspondants
      for (col in names(matching)[-1]) {
        
        method_file <- matching[[col]][
          match(raw_file, matching$raw_file)
        ]
        
        param_list[[col]] <- get_fcs_parameters(method_file)
      }
      
      # Garder les paramètres communs à tous les fichiers
      common_names <- Reduce(
        intersect,
        lapply(param_list, function(x) x$name)
      )
      
      
      raw_parameters <- param_list$RAW
      
      parameter_table <- data.frame(
        name = common_names,
        desc = raw_parameters$desc[
          match(common_names, raw_parameters$name)
        ],
        stringsAsFactors = FALSE
      )
      
      # Ajouter les descriptions des méthodes
      for (n in names(param_list)) {
        
        tmp <- param_list[[n]]
        
        parameter_table[[paste0("desc_", n)]] <-
          tmp$desc[match(common_names, tmp$name)]
      }
      
      parameter_table_rv(parameter_table)
      
      merge_summary(NULL)
      parameters_validated(FALSE)
      
      showNotification(
        paste(
          nrow(matching),
          "RAW files matched;",
          length(common_names),
          "common parameters found."
        ),
        type = "message"
      )
      
    }, error = function(e) {
      
      matching_rv(NULL)
      matching_validated(FALSE)
      parameter_table_rv(NULL)
      
      showNotification(
        paste("File validation failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
      
    })
    
  })
  output$parameter_table <- renderDT({
    
    dat <- parameter_table_rv()
    
    req(dat)
    
    dat$Keep <- paste0(
      '<input type="checkbox" class="param_check" value="',
      dat$name,
      '" ',
      ifelse(
        !is.na(dat$desc) &
          nzchar(dat$desc) &
          !grepl("^\\.SC-[AHW]", dat$name),
        "checked",
        ""
      ),
      '>'
    )
    
    
    # mettre Keep en premi??re colonne
    dat <- dat[, c(
      "Keep",
      setdiff(names(dat), "Keep")
    )]
    
    
    datatable(
      dat,
      escape = FALSE,
      
      selection = "none",
      rownames = FALSE,
      
      callback = DT::JS("
      
    var selected = [];

$('.param_check:checked').each(function(){
    selected.push($(this).val());
});

Shiny.setInputValue(
    'checked_parameters',
    selected,
    {priority:'event'}
);

      


      table.on('draw.dt',function(){

        $('.param_check').each(function(){

          var value = $(this).val();

          if($(this).prop('checked')){

            if(selected.indexOf(value) === -1){
              selected.push(value);
            }

          } else {

            selected = selected.filter(function(x){
              return x !== value;
            });

          }

        });


        $('.param_check').each(function(){

          if(selected.indexOf($(this).val()) !== -1){

            $(this).prop('checked',true);

          }

        });


        Shiny.setInputValue(
  'checked_parameters',
  selected,
  {priority:'event'}
);

      });



      table.on('change','.param_check',function(){

        var value = $(this).val();

        if($(this).prop('checked')){

          if(selected.indexOf(value) === -1){
            selected.push(value);
          }

        } else {

          selected = selected.filter(function(x){
            return x !== value;
          });

        }

        Shiny.setInputValue(
  'checked_parameters',
  selected,
  {priority:'event'}
);

      });

    "),
      
      
      options = list(
        
        paging = FALSE,
        
        # garde le scroll pour les tr??s grandes tables
        scrollX = TRUE,
        
        scrollCollapse = TRUE,
        
        autoWidth = FALSE,
        
        searching = TRUE,
        
        lengthChange = FALSE,
        
        dom = 'ft',
        
        
        columnDefs = c(
          
          list(
            list(
              targets = 0,
              width = "35px",
              className = "dt-center"
            ),
            
            list(
              targets = 1,
              width = "110px"
            ),
            
            list(
              targets = 2,
              width = "120px"
            )
          ),
          
          
          if(ncol(dat) > 3)
            list(
              list(
                targets = 3:(ncol(dat)-1),
                width = "90px"
              )
            )
          else
            list()
          
        )
        
      ),
      
      
      class = "compact stripe hover",
      
      width = "50%"
      
    )
    
    
  }, server = FALSE)
  observeEvent(input$use_parameters, {
    
    params <- input$checked_parameters
    
    if(is.null(params)){
      params <- character(0)
    }
    
    if(length(params)==0){
      
      showNotification(
        "No parameter selected",
        type="error"
      )
      
      parameters_validated(FALSE)
      merge_summary(NULL)
      
      return()
      
    }
    
    
    selected_parameters_rv(unique(params))
    parameters_validated(TRUE)
    
    
    dat <- methods()
    
    raw_file_list <- raw_files()
    
    # ==========================================
    # Labels affichage avec desc entre parenth??ses
    # ==========================================
    
    parameter_table <- parameter_table_rv()
    
    parameter_labels <- params
    
    if(!is.null(parameter_table)){
      
      idx <- match(
        params,
        parameter_table$name
      )
      
      desc <- parameter_table$desc[idx]
      
      parameter_labels <- ifelse(
        !is.na(desc) &
          nzchar(desc),
        paste0(
          params,
          " (",
          desc,
          ")"
        ),
        params
      )
      
    }
    
    
    merge_summary(
      list(
        
        n_methods = nrow(dat),
        
        methods = dat$suffix,
        
        n_files = length(raw_file_list),
        
        n_parameters = length(params),
        
        # vrais noms utilis??s pour le merge
        parameters = unique(params),
        
        # noms enrichis pour l'affichage
        parameter_labels = unique(parameter_labels),
        
        folders = rbind(
          
          data.frame(
            type = "RAW",
            folder = if (identical(app_mode, "local")) {
              raw_path()
            } else {
              "Uploaded RAW files"
            },
            suffix = "",
            stringsAsFactors = FALSE
          ),
          
          data.frame(
            type = "Correction",
            folder = if (identical(app_mode, "local")) {
              dat$path
            } else {
              paste(dat$suffix, "files")
            },
            suffix = dat$suffix,
            stringsAsFactors = FALSE
          )
          
        )
        
      )
    )
    
    
    showNotification(
      paste(
        length(params),
        "parameters selected"
      ),
      type="message"
    )
    
  })
  output$analysis_summary <- renderUI({
    
    s <- merge_summary()
    
    if(!isTRUE(parameters_validated()) || is.null(s)){
      
      return(
        fluidRow(
          column(
            12,
            box(
              width = 12,
              title = "Merge summary",
              status = "warning",
              solidHeader = TRUE,
              
              h4("Parameters not selected"),
              
              p(
                "Go to the Parameters tab, select markers and validate your selection."
              )
            )
          )
        )
      )
      
    }
    
    
    fluidRow(
      
      column(
        4,
        box(
          width = 12,
          title = "Methods",
          status = "primary",
          solidHeader = TRUE,
          
          h2(
            s$n_methods,
            style="text-align:center;"
          ),
          
          p(
            "processing methods",
            style="text-align:center;color:#666;"
          )
        )
      ),
      
      
      column(
        4,
        box(
          width = 12,
          title = "RAW files",
          status = "success",
          solidHeader = TRUE,
          
          h2(
            s$n_files,
            style="text-align:center;"
          ),
          
          p(
            "files detected",
            style="text-align:center;color:#666;"
          )
        )
      ),
      
      
      column(
        4,
        box(
          width = 12,
          title = "Parameters",
          status = "info",
          solidHeader = TRUE,
          
          h2(
            s$n_parameters,
            style="text-align:center;"
          ),
          
          p(
            "markers selected",
            style="text-align:center;color:#666;"
          )
        )
      )
      
    )
    
    
    fluidRow(
      
      column(
        6,
        
        box(
          width = 12,
          title = "Input folders",
          status = "primary",
          solidHeader = TRUE,
          
          tableOutput("folders_table")
          
        )
        
      ),
      
      
      column(
        6,
        
        box(
          width = 12,
          title = paste0(
            "Selected parameters (",
            s$n_parameters,
            ")"
          ),
          status = "primary",
          solidHeader = TRUE,
          
          div(
            style="
          max-height:250px;
          overflow-y:auto;
          ",
            
            tags$ul(
              lapply(
                s$parameter_labels,
                function(x){
                  
                  tags$li(x)
                  
                }
              )
            )
            
          )
          
        )
        
      )
      
    )
    
  })
  output$folders_summary <- renderUI({
    
    s <- merge_summary()
    
    req(s$folders)
    
    folders <- s$folders
    
    tableOutput("folders_table")
    
  })
  
  
  output$folders_table <- renderTable({
    
    s <- merge_summary()
    
    req(s$folders)
    
    data.frame(
      Type = ifelse(
        s$folders$type == "RAW",
        "RAW",
        "Method"
      ),
      
      Suffix = ifelse(
        s$folders$type == "RAW",
        "-",
        s$folders$suffix
      ),
      
      Folder = basename(s$folders$folder),
      Parent_path = ifelse(
        grepl("Uploaded", s$folders$folder),
        "-",
        dirname(s$folders$folder)
      ),
      
      stringsAsFactors = FALSE
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  merge_one_fcs <- function(raw_file,
                            method_files,
                            selected_parameters,
                            output_file){
    
    
    raw_fcs <- read.FCS(
      raw_file,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    
    for(method_name in names(method_files)){
      
      
      method_file <- method_files[[method_name]]
      
      
      if(is.na(method_file) || !file.exists(method_file))
        next
      
      
      method_fcs <- read.FCS(
        method_file,
        transformation = FALSE,
        truncate_max_range = FALSE
      )
      
      
      method_expr <- exprs(method_fcs)
      
      
      common <- intersect(
        selected_parameters,
        colnames(method_expr)
      )
      
      
      if(length(common) == 0)
        next
      
      
      # param??tres du fichier m??thode
      method_params <- method_fcs@parameters@data
      
      

      for(marker in common){
        
        
        newcolumn <- method_expr[, marker]
        
        
        new_name <- paste0(
          marker,
          "_",
          method_name
        )
        
        

        raw_fcs <- FlowCIPHE::enrich.FCS.CIPHE(
          raw_fcs,
          newcolumn,
          new_name
        )

        raw_params <- raw_fcs@parameters@data
        
        
        raw_desc <- raw_params$desc[
          match(marker, raw_params$name)
        ]
        
        
        if(is.na(raw_desc) || !nzchar(raw_desc)){
          
          raw_desc <- method_params$desc[
            match(marker, method_params$name)
          ]
          
        }
        
        
        idx <- match(
          new_name,
          raw_params$name
        )
        
        
        if(!is.na(idx)){
          
          if(!is.na(raw_desc) && nzchar(raw_desc)){
            
            raw_fcs@parameters@data$desc[idx] <- raw_desc
            
          } else {
            
            raw_fcs@parameters@data$desc[idx] <- new_name
            
          }
          
        }
        
      }
      
    }
    # ==========================================
    # R??organisation des colonnes
    # RAW marker puis corrections associ??es
    # ==========================================
    
    current_expr <- exprs(raw_fcs)
    
    new_order <- character()
    
    for(marker in selected_parameters){
      
      # colonne RAW
      if(marker %in% colnames(current_expr)){
        new_order <- c(
          new_order,
          marker
        )
      }
      
      # colonnes corrig??es associ??es
      added_cols <- grep(
        paste0("^", marker, "_"),
        colnames(current_expr),
        value = TRUE
      )
      
      new_order <- c(
        new_order,
        added_cols
      )
    }
    
    
    # garder les autres colonnes non s??lectionn??es
    remaining <- setdiff(
      colnames(current_expr),
      new_order
    )
    
    new_order <- c(
      new_order,
      remaining
    )
    
    

    exprs(raw_fcs) <- current_expr[, new_order, drop=FALSE]
    
    write.FCS(
      raw_fcs,
      filename = output_file
    )
    
  }
  
  generate_overlay_plot <- function(raw_file,
                                    method_files,
                                    marker){
    
    all_data <- list()
    
    
    raw <- read.FCS(
      raw_file,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    
    if(marker %in% colnames(exprs(raw))){
      
      all_data[["RAW"]] <-
        log10(exprs(raw)[,marker]+1)
      
    }
    
    
    for(method in names(method_files)){
      
      f <- method_files[[method]]
      
      if(!is.na(f) && file.exists(f)){
        
        fcs <- read.FCS(
          f,
          transformation = FALSE,
          truncate_max_range = FALSE
        )
        
        
        if(marker %in% colnames(exprs(fcs))){
          
          all_data[[method]] <-
            log10(exprs(fcs)[,marker]+1)
          
        }
        
      }
      
    }
    
    
    if(length(all_data)<2)
      return(NULL)
    
    
    
    # ==============================
    # Density avec offset Y
    # ==============================
    
    dens <- lapply(
      all_data,
      function(x){
        density(
          x,
          na.rm=TRUE,
          n=512
        )
      }
    )
    
    
    ymax <- max(
      sapply(
        dens,
        function(d) max(d$y)
      )
    )
    
    
    gap <- ymax * 0.25   # espace vertical entre plots
    
    
    plots <- data.frame()
    
    
    for(i in seq_along(dens)){
      
      
      d <- dens[[i]]
      
      tmp <- data.frame(
        x = d$x,
        y = d$y + (i-1)*gap,
        group = names(dens)[i]
      )
      
      plots <- rbind(
        plots,
        tmp
      )
      
    }
    
    
    
    ggplot(
      plots,
      aes(
        x=x,
        y=y,
        colour=group,
        fill=group
      )
    )+
      
      geom_area(
        alpha=0.25,
        position="identity"
      )+
      
      geom_line(
        linewidth=1.2
      )+
      
      theme_bw(
        base_size=14
      )+
      
      labs(
        title=paste(
          "Marker:",
          marker
        ),
        x="log10 intensity",
        y="Density + vertical offset",
        colour="Sample",
        fill="Sample"
      )+
      
      theme(
        legend.position="right",
        legend.text=element_text(size=12),
        legend.title=element_text(size=13)
      )
    
  }
  create_merge_report <- function(
    matching,
    methods,
    parameters,
    out_dir
  ){
    
    report_file <- file.path(
      out_dir,
      "FCS_Merge_Report.html"
    )
    
    
    html <- c(
      
      "<html>",
      "<head>",
      "<title>FCS Merge Report</title>",
      
      "<style>
    body{
      font-family:Arial;
      margin:40px;
    }

    h1{
      color:#243447;
    }

    .box{
      background:#eef6ff;
      padding:20px;
      border-radius:10px;
      margin-bottom:20px;
    }

    img{
      width:900px;
      margin-bottom:30px;
    }

    </style>",
      
      "</head>",
      "<body>",
      
      
      "<h1>FCS Merge Report</h1>",
      
      
      "<div class='box'>",
      
      paste0(
        "<h2>Merge summary</h2>",
        "<p><b>Number of RAW files merged:</b> ",
        nrow(matching),
        "</p>",
        
        "<p><b>Correction methods:</b><br>",
        paste(methods, collapse=", "),
        "</p>",
        
        
        "<p><b>Parameters merged:</b><br>",
        paste(parameters, collapse=", "),
        "</p>"
        
      ),
      
      "</div>",
      
      
      "<h2>Overlay density plots</h2>"
      
    )
    
    
    for(i in seq_len(nrow(matching))){
      
      
      raw_file <- matching$raw_file[i]
      
      
      method_files <- list()
      
      
      for (m in names(matching)[-1]) {
        method_files[[m]] <- matching[[m]][i]
      }
      
      for(marker in parameters){
        
        
        p <- generate_overlay_plot(
          raw_file,
          method_files,
          marker
        )
        
        
        if(!is.null(p)){
          
          
          png_file <- tempfile(
            fileext=".png"
          )
          
          
          ggsave(
            png_file,
            p,
            width=10,
            height=7,
            dpi=150
          )
          
          
          img <- base64enc::dataURI(
            file=png_file,
            mime="image/png"
          )
          
          
          html <- c(
            html,
            
            paste0(
              "<h3>",
              basename(raw_file),
              " - ",
              marker,
              "</h3>",
              
              "<img src='",
              img,
              "'>"
            )
          )
          
          
        }
        
      }
      
    }
    
    
    html <- c(
      html,
      "</body>",
      "</html>"
    )
    
    
    writeLines(
      html,
      report_file
    )
    
    
    report_file
    
  }
  observeEvent(input$run_merge,{
    
    
    
    matching <- matching_rv()
    params <- selected_parameters_rv()
    
    
    req(matching)
    req(params)
    
    
    out_dir <- file.path(
      dirname(matching$raw_file[1]),
      "Merged_FCS"
    )
    
    if (!dir.exists(out_dir)) {
      dir.create(
        out_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
    
    if (!dir.exists(out_dir)) {
      stop("Unable to create the output directory.")
    }
    
    
    withProgress(
      
      message="Merging FCS files...",
      value=0,
      
      {
    
        n <- nrow(matching)
        
        
        for(i in seq_len(n)){
          
          
          raw_file <- matching$raw_file[i]
          
          
          method_files <- list()
          
          
          for(method in names(matching)[-1]){
            
            method_files[[method]] <-
              matching[[method]][i]
            
          }
          
          
          output_file <- file.path(
            out_dir,
            paste0(
              tools::file_path_sans_ext(
                basename(raw_file)
              ),
              "_Merged.fcs"
            )
          )
          
          merge_one_fcs(
            raw_file,
            method_files,
            params,
            output_file
          )
          
          
          incProgress(
            1/n
          )
          
        }
        
      }
    )
    files <- list.files(
      out_dir,
      pattern = "\\.fcs$",
      full.names = FALSE
    )
    
    updateSelectInput(
      session,
      "viz_fcs",
      choices = files,
      selected = files[1]
    )
    output$merge_status <- renderText({
      
      paste(
        "Merge completed successfully.",
        "\nOutput directory:",
        out_dir
      )
      
    })
    merge_finished(TRUE)
    
  })
  # ==================================================
  # VISUALIZATION MERGED FCS
  # ==================================================
  
  viz_dir <- reactive({
    
    matching <- matching_rv()
    
    req(matching)
    
    file.path(
      dirname(matching$raw_file[1]),
      "Merged_FCS"
    )
    
  })
  
  
  # Liste des fichiers merged
  observe({
    
    req(dir.exists(viz_dir()))
    
    files <- list.files(
      viz_dir(),
      pattern="\\.fcs$",
      full.names=FALSE
    )
    
    updateSelectInput(
      session,
      "viz_fcs",
      choices = files
    )
    
  })
  
  output$merge_report_ui <- renderUI({
    
    req(merge_finished())
    
    fluidRow(
      
      column(
        
        12,
        
        box(
          
          width = 12,
          
          title = "Merge report",
          status = "info",
          solidHeader = TRUE,
          
        ),
        
        
        downloadButton(
          "download_report",
          "Generate FCS Merge Report",
          icon = icon("download"),
          class = "btn-primary"
        )      )
      
    ) 
    
  })
  

  # ==================================================
  # Param??tres disponibles
  # ==================================================
  
  output$viz_parameters <- renderUI({
    
    req(input$viz_fcs)
    
    f <- file.path(
      viz_dir(),
      input$viz_fcs
    )
    
    fcs <- read.FCS(
      f,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    params <- colnames(exprs(fcs))
    
    p_data <- fcs@parameters@data
    
    labels <- sapply(params, function(x){
      
      idx <- match(x, p_data$name)
      
      desc <- p_data$desc[idx]
      
      if(!is.na(desc) && nzchar(desc)){
        paste0(x, " (", desc, ")")
      } else {
        x
      }
      
    })
    
  names(labels) <- params
    
    choices <- setNames(
      object = params,
      nm = ifelse(
        is.na(p_data$desc[match(params, p_data$name)]) |
          p_data$desc[match(params, p_data$name)] == "",
        params,
        paste0(
          p_data$desc[match(params, p_data$name)],
          " (", params, ")"
        )
      )
    )
  
    selectInput(
      "viz_markers",
      "Columns to overlay",
      choices = choices,
      selected = params[1],
      multiple = TRUE
    )
    
  })
  
  
 
  # ==================================================
  # SELECTION DES DEUX MARQUEURS
  # ==================================================
  
  output$density_axes <- renderUI({
    
    req(input$viz_fcs)
    req(dir.exists(viz_dir()))
    
    f <- file.path(viz_dir(), input$viz_fcs)
    
    validate(
      need(file.exists(f), "Merged FCS file not found.")
    )
    
    fcs <- flowCore::read.FCS(
      f,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    params <- colnames(flowCore::exprs(fcs))
    
    validate(
      need(length(params) >= 2, "At least two channels are required.")
    )
    
    names(params) <- NULL
    
    tagList(
      selectInput(
        "marker_x",
        "Marker X",
        choices = params,
        selected = params[1]
      ),
      
      selectInput(
        "marker_y",
        "Marker Y",
        choices = params,
        selected = params[2]
      )
    )
  })
  
  
  # ==================================================
  # PLOT DENSITY FLOWDENSITY
  # ==================================================
  
  
  # ==================================================
  # PLOT DENSITY AVEC densCols()
  # ==================================================
  
  output$plot_dens <- renderPlot({
    
    req(
      input$viz_fcs,
      input$marker_x,
      input$marker_y
    )
    
    validate(
      need(
        input$marker_x != input$marker_y,
        "Please select two different markers."
      )
    )
    
    f <- file.path(viz_dir(), input$viz_fcs)
    
    validate(
      need(file.exists(f), "Merged FCS file not found.")
    )
    
    fcs <- flowCore::read.FCS(
      f,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    channels <- c(input$marker_x, input$marker_y)
    
    validate(
      need(
        all(channels %in% colnames(flowCore::exprs(fcs))),
        "One or both selected markers are missing."
      )
    )
    
    # --------------------------------
    # Récupération des valeurs
    # --------------------------------
    
    dat <- flowCore::exprs(fcs)[, channels, drop = FALSE]
    
    valid <- apply(
      dat,
      1,
      function(z) all(is.finite(z))
    )
    
    validate(
      need(sum(valid) >= 20, "Not enough valid events to plot.")
    )
    
    x_vals <- dat[valid, 1]
    y_vals <- dat[valid, 2]
    
    # --------------------------------
    # Arcsinh transformation
    # --------------------------------
    
    cofactor <- 150
    
    if (isTRUE(input$apply_arcsinh)) {
      
      cofactor <- input$arcsinh_cofactor
      
      validate(
        need(
          length(cofactor) == 1 &&
            is.finite(cofactor) &&
            cofactor > 0,
          "The arcsinh cofactor must be positive."
        )
      )
      
      x_vals <- asinh(x_vals / cofactor)
      y_vals <- asinh(y_vals / cofactor)
      
    }
    
    # --------------------------------
    # Axis limits
    # --------------------------------
    
    x_min <- input$xlim_min
    x_max <- input$xlim_max
    y_min <- input$ylim_min
    y_max <- input$ylim_max
    
    validate(
      need(
        all(is.finite(c(x_min, x_max, y_min, y_max))),
        "Axis limits must be finite numbers."
      ),
      need(
        x_min < x_max && y_min < y_max,
        "Axis minimum must be smaller than axis maximum."
      )
    )
    
    if (isTRUE(input$apply_arcsinh)) {
      
      plot_xlim <- asinh(c(x_min, x_max) / cofactor)
      plot_ylim <- asinh(c(y_min, y_max) / cofactor)
      
    } else {
      
      plot_xlim <- c(x_min, x_max)
      plot_ylim <- c(y_min, y_max)
      
    }
    
    # --------------------------------
    # Couleurs selon la densité
    # --------------------------------
    if (input$density){
      
      cols <- grDevices::densCols(
        x_vals,
        y_vals,
        colramp = colorRampPalette(
          c(
            "blue",
            "turquoise",
            "green",
            "yellow",
            "orange",
            "red"
          )
        )
      )
      
     
      
    }else{
      cols="black"
    }
    # --------------------------------
    # Scatter plot avec densité
    # --------------------------------
    
    plot(
      x_vals,
      y_vals,
      pch = 16,
      cex = 0.5,
      col = cols,
      
      xlab = input$marker_x,
      ylab = input$marker_y,
      
      main = paste(
        input$marker_x,
        "vs",
        input$marker_y
      ),
      
      xlim = plot_xlim,
      ylim = plot_ylim
    )
    
  }, res = 110)
  
  
  output$overlay_hist <- renderPlot({
    
    req(
      input$viz_fcs,
      input$viz_markers
    )
    
    f <- file.path(
      viz_dir(),
      input$viz_fcs
    )
    
    fcs <- read.FCS(
      f,
      transformation = FALSE,
      truncate_max_range = FALSE
    )
    
    dat <- as.data.frame(exprs(fcs))
    
    markers <- input$viz_markers
    
    # Arcsinh cofactor validation
    if (isTRUE(input$apply_arcsinh)) {
      
      cofactor <- input$arcsinh_cofactor
      
      validate(
        need(
          length(cofactor) == 1 &&
            is.finite(cofactor) &&
            cofactor > 0,
          "The arcsinh cofact or must be a positive number."
        )
      )
    }
    
    # Transform marker intensities
    values <- lapply(
      markers,
      function(m) {
        
        if (!m %in% colnames(dat))
          return(NULL)
        
        x <- dat[[m]]
        
        x <- x[is.finite(x)]
        
        if (length(x) < 10)
          return(NULL)
        
        if (isTRUE(input$apply_arcsinh)) {
          
          # Arcsinh transformation
          asinh(x / cofactor)
          
        } else {
          
          # Original log10 transformation
          log10(x + 1)
          
        }
      }
    )
    keep <- !sapply(values, is.null)
    
    values <- values[keep]
    
    markers <- markers[keep]
    
    req(length(values) > 0)
    
    # Density estimation
    dens <- lapply(
      values,
      function(x) {
        density(
          x,
          n = 512,
          na.rm = TRUE
        )
      }
    )
    
    names(dens) <- markers
    
    xlim <- range(
      unlist(lapply(dens, function(d) d$x)),
      finite = TRUE
    )
    
    ymax <- max(
      sapply(
        dens,
        function(d) max(d$y)
      )
    )
    
    # Vertical offset
    offset <- ymax * 0.15
    
    cols <- rainbow(
      length(dens),
      alpha = 0.35
    )
    
    plot(
      dens[[1]]$x,
      dens[[1]]$y,
      type = "n",
      xlim = xlim,
      ylim = c(
        0,
        ymax + offset * length(dens)
      ),
      main = "Overlay density plot",
      xlab = if (isTRUE(input$apply_arcsinh)) {
        paste0("Arcsinh intensity (cofactor = ", cofactor, ")")
      } else {
        "log10 intensity"
      },
      ylab = "Density"
    )
    
    for (i in seq_along(dens)) {
      
      yshift <- (i - 1) * offset
      
      polygon(
        c(
          dens[[i]]$x,
          rev(dens[[i]]$x)
        ),
        c(
          dens[[i]]$y + yshift,
          rep(
            yshift,
            length(dens[[i]]$y)
          )
        ),
        col = cols[i],
        border = NA
      )
      
      lines(
        dens[[i]]$x,
        dens[[i]]$y + yshift,
        col = cols[i],
        lwd = 2
      )
    }
    
    legend(
      "topright",
      legend = markers,
      fill = cols,
      border = NA,
      cex = 1.2,
      pt.cex = 2,
      text.width = NULL,
      bty = "n",
      inset = 0.02
    )
    
  }, width = 700, height = 700)
  
  
  
 
    output$download_report <- downloadHandler(
      
      filename = function(){
        
        print("DOWNLOAD START")
        
        paste0(
          "FCS_Merge_Report_",
          Sys.Date(),
          ".html"
        )
        
      },
      
      
      content = function(file){
        
        print("CREATING REPORT")
        
        matching <- matching_rv()
        params <- selected_parameters_rv()
        
        print(matching)
        print(params)
        
        req(matching)
        req(params)
        
        
        out_dir <- file.path(
          dirname(matching$raw_file[1]),
          "Merged_FCS"
        )
        
        
        report <- create_merge_report(
          matching = matching,
          methods = names(matching)[-1],
          parameters = params,
          out_dir = out_dir
        )
        
        
        print(report)
        
        
        file.copy(
          report,
          file,
          overwrite=TRUE
        )
        
        print("REPORT SENT")
        
      }
    )
    
    output$download_fcs <- downloadHandler(
      
      filename = function() {
        paste0("FCS_Merge_", Sys.Date(), ".zip")
      },
      
      content = function(file) {
        
        req(merge_finished())
        
        out_dir <- viz_dir()
        
        fcs_files <- list.files(
          out_dir,
          pattern = "\\.fcs$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
        validate(
          need(
            length(fcs_files) > 0,
            "No merged FCS files found."
          )
        )
        
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        
        setwd(out_dir)
        
        utils::zip(
          zipfile = file,
          files = basename(fcs_files)
        )
      },
      
      contentType = "application/zip"
    )
 
}









