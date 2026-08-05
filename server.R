library(shiny)
library(shinyFiles)
library(DT)
library(FlowCIPHE)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)


server <- function(input, output, session){
  parameter_table_rv <- reactiveVal(NULL)
  merge_summary <- reactiveVal(NULL)
  
  matching_rv <- reactiveVal(NULL)
  selected_parameters_rv <- reactiveVal(NULL)
  parameters_validated <- reactiveVal(FALSE)
  
  roots <- c(
    Home="/mnt/md0/R_D/FCS-Merge"
  )

  current_dir <- reactiveVal("/")
  last_selected_dir <- reactiveVal("/")
  # ==================================================
  # RAW
  # ==================================================
  
  shinyDirChoose(
    input,
    "raw_dir",
    roots=roots
  )
  
  
  output$raw_path <- renderText({
    
    req(input$raw_dir)
    
    path <- parseDirPath(
      roots,
      input$raw_dir
    )
    current_dir(dirname(path))
    last_selected_dir(dirname(path))
    
    path
    
  })
  

  
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
      
      
      # Cr??ation de la nouvelle m??thode
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
      
      
      # Cr??ation du s??lecteur de dossier
      shinyDirChoose(
        input,
        paste0("method_dir_", id),
        roots = roots
      )
      
      
      # Sauvegarde du chemin s??lectionn??
      observeEvent(
        input[[paste0("method_dir_", id)]],
        {
          
          folder_input <- input[[paste0("method_dir_", id)]]
          
          if(is.null(folder_input) ||
             length(folder_input) == 0){
            return()
          }
          
          
          path <- parseDirPath(
            roots,
            folder_input
          )
          
          
          if(length(path) == 0 ||
             is.na(path) ||
             !nzchar(path)){
            return()
          }
          
          
          dat <- methods()
          
          idx <- match(id, dat$id)
          
          if(!is.na(idx)){
            
            dat$path[idx] <- path
            
            methods(dat)
            
          }
          
        },
        ignoreInit = TRUE
      )
      
      
      # Sauvegarde du suffixe en temps r??el
      observeEvent(
        input[[paste0("suffix_", id)]],
        {
          
          dat <- methods()
          
          idx <- match(id, dat$id)
          
          if(!is.na(idx)){
            
            dat$suffix[idx] <- input[[paste0("suffix_", id)]]
            
            methods(dat)
            
          }
          
        },
        ignoreInit = TRUE
      )
      
      
      # Affichage du chemin
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
  
  
  
  
  # ==================================================
  # UI METHODS
  # ==================================================
  
  output$methods_ui <- renderUI({
    
    
    dat <- methods()
    
    
    if(nrow(dat)==0){
      
      return(
        tags$div(
          style="
          color:grey;
          font-style:italic;
          ",
          "No process added yet"
        )
      )
      
    }
    
    
    lapply(
      seq_len(nrow(dat)),
      
      function(x){
        
        
        id <- dat$id[x]
        
        suffix <- dat$suffix[x]
      
        
        fluidRow(
          
          column(
            5,
            
            box(
              width=12,
              title=paste(
                "Process",
                id
              ),
              status="info",
              solidHeader=TRUE,
              
              shinyDirButton(
                paste0(
                  "method_dir_",
                  id
                ),
                "Choose FCS folder",
                "Select",
                class = "btn-raw"
              ),
              
              br(),
              br(),
              
              verbatimTextOutput(
                paste0(
                  "method_path_",
                  id
                )
              )
              
            )
            
          ),
          
          
          column(
            4,
            
            box(
              width=12,
              title = tagList(
                "Suffix ",
                tags$span(
                  "?",
                  title = paste0(
                    "The suffix will be added to the parameter name in the merged FCS file.\n\n",
                    "Example: if the original channel is V-V500 and the process suffix is 'cycomb',\n",
                    "the new merged parameter will be named V-V500_cycomb."
                  ),
                  style = "
        display:inline-block;
        width:18px;
        height:18px;
        border-radius:50%;
        background:#3b82f6;
        color:white;
        text-align:center;
        line-height:18px;
        font-weight:bold;
        cursor:help;
        margin-left:5px;
      "
                )
              ),
     
              textInput(
                paste0(
                  "suffix_",
                  id
                ),
                NULL,
                value=suffix
              )
            )
            
          ),
       
          
          column(
            2,
            
            br(),
            
            actionButton(
              paste0(
                "delete_",
                id
              ),
              "",
              icon=icon("trash"),
              class="btn-danger"
            )
            
          )
          
        )
        
      }
      
    )
    
    
  })
  
  # ==================================================
  # CHECK
  # ==================================================
  
  observeEvent(
    input$check_files,
    {
      
      print(methods())
      
      showNotification(
        paste(
          nrow(methods()),
          "methods detected"
        ),
        type="message"
      )
      
    }
  )
  output$validate_button <- renderUI({
    
    ## ==========================
    ## V??rification RAW
    ## ==========================
    
    raw_ok <- FALSE
    
    if (!is.null(input$raw_dir) &&
        length(input$raw_dir) > 0) {
      
      raw_path <- parseDirPath(
        roots,
        input$raw_dir
      )
      
      if (length(raw_path) > 0 &&
          !is.na(raw_path) &&
          nzchar(raw_path)) {
        
        raw_ok <- TRUE
        
      }
      
    }
    
    ## ==========================
    ## V??rification m??thodes
    ## ==========================
    
    dat <- methods()
    
    
    if (!raw_ok || nrow(dat) == 0) {
      return(NULL)
    }
    
    
    all_ok <- TRUE
    
    
    for (i in seq_len(nrow(dat))) {
      
      
      method_path <- dat$path[i]
      
      
      if (is.null(method_path) ||
          length(method_path) == 0 ||
          is.na(method_path) ||
          !nzchar(method_path)) {
        
        all_ok <- FALSE
        break
        
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
 
  
  library(flowCore)
  library(stringdist)
  
  
  match_fcs_files <- function(raw_dir, method_dirs){
    
    raw_files <- list.files(
      raw_dir,
      pattern="\\.fcs$",
      full.names=TRUE,
      ignore.case=TRUE
    )
    
    
    if(length(raw_files)==0){
      stop("No RAW FCS files found")
    }
    
    
    results <- data.frame(
      raw_file = raw_files,
      stringsAsFactors = FALSE
    )
    
    for(method_name in names(method_dirs)){
      
      method_files <- list.files(
        method_dirs[[method_name]],
        pattern="\\.fcs$",
        full.names=TRUE,
        ignore.case=TRUE
      )
      
      
      matched <- character(length(raw_files))
      
      
      for(i in seq_along(raw_files)){
        
        
        raw_name <- tools::file_path_sans_ext(
          basename(raw_files[i])
        )
        
        
        method_names <- tools::file_path_sans_ext(
          basename(method_files)
        )
        
        
        # =========================
        # 1) Matching par nom
        # =========================
        
        scores <- stringdist(
          raw_name,
          method_names,
          method="jw"
        )
        
        
        best <- which.min(scores)
        
        if(length(method_files)==0){
          matched[i] <- NA
          next
        }
        
        # seuil de confiance
        if(length(best)>0 && scores[best] < 0.25){
          
          matched[i] <- method_files[best]
          
        } else {
          
          
          # =========================
          # 2) Matching cellule
          # =========================
          
          
          raw_fcs <- tryCatch(
            read.FCS(
              raw_files[i],
              transformation=FALSE,
              truncate_max_range=FALSE
            ),
            error=function(e) NULL
          )
          
          
          if(!is.null(raw_fcs)){
            
            
            raw_cells <- nrow(exprs(raw_fcs))
            
            
            cell_diff <- sapply(
              method_files,
              function(f){
                
                fcs <- tryCatch(
                  read.FCS(
                    f,
                    transformation=FALSE,
                    truncate_max_range=FALSE
                  ),
                  error=function(e) NULL
                )
                
                
                if(is.null(fcs)){
                  return(Inf)
                }
                
                
                abs(
                  nrow(exprs(fcs))-raw_cells
                )
                
              }
            )
            
            
            if(length(cell_diff)>0 && min(cell_diff) < Inf){
              
              matched[i] <- method_files[which.min(cell_diff)]
              
            } else {
              
              matched[i] <- NA
              
            }
            
            
          }
          
        }
        
      }
      
      
      results[[method_name]] <- matched
      
    }
    
    
    results
    
  }
  
  observeEvent(
    input$check_files,
    {
      
      raw_path <- parseDirPath(
        roots,
        input$raw_dir
      )
      
      
      method_paths <- list()
      
      dat <- methods()
      
      
      for(i in dat$id){
        
        method_paths[[dat$suffix[i]]] <-
          dat$path[i]
        
      }
      
      
      matching <- match_fcs_files(
        raw_path,
        method_paths
      )
      matching_rv(matching)
      
      # ==========================================
      # Affichage uniquement des noms de fichiers
      # ==========================================
      
      matching_display <- matching
      
      
      for(col in names(matching_display)){
        
        matching_display[[col]] <- basename(
          matching_display[[col]]
        )
        
      }
      
      

      
    }
  )
  
  get_fcs_parameters <- function(file){
    
    if(length(file)==0 || is.na(file) || !file.exists(file)){
      return(
        data.frame(
          name=character(),
          desc=character(),
          stringsAsFactors=FALSE
        )
      )
    }
    
    fcs <- read.FCS(
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
  observeEvent(
    input$check_files,
    {
      
      raw_path <- parseDirPath(
        roots,
        input$raw_dir
      )
      
      
      method_paths <- list()
      
      dat <- methods()
      
      
      for(i in dat$id){
        
        method_paths[[dat$suffix[i]]] <-
          dat$path[i]
        
      }
      
      
      matching <- match_fcs_files(
        raw_path,
        method_paths
      )
      
      
      # ===============================
      # Choisir un RAW au hasard
      # ===============================
      
      raw_file <- sample(
        matching$raw_file,
        1
      )
      
      
      # param??tres RAW
      
      param_list <- list()
  
      param_list$RAW <- get_fcs_parameters(
        raw_file
      )
      
      
      # ===============================
      # Param??tres m??thodes
      # ===============================
      
      for(col in names(matching)[-1]){
        
        method_file <- matching[
          matching$raw_file == raw_file,
          col
        ]
        print(method_file)
        param_list[[col]] <- get_fcs_parameters(
          method_file
        )
        
      }
      
      # ===============================
      # garder uniquement les names communs
      # RAW + toutes les m??thodes
      # ===============================
      
      common_names <- Reduce(
        intersect,
        lapply(
          param_list,
          function(x)
            x$name
        )
      )
      
      
      # ===============================
      # construction table
      # avec desc RAW
      # ===============================
      
      raw_parameters <- param_list$RAW
      
      
      parameter_table <- data.frame(
        
        name = common_names,
        
        desc = raw_parameters$desc[
          match(
            common_names,
            raw_parameters$name
          )
        ],
        
        stringsAsFactors = FALSE
        
      )
      
      
      parameter_table_rv(parameter_table)
      merge_summary(NULL)
      
      for(n in names(param_list)){
        
        tmp <- param_list[[n]]
        
        parameter_table[[paste0(
          "desc_",
          n
        )]] <-
          tmp$desc[
            match(
              common_names,
              tmp$name
            )
          ]
        
      }
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
          
          scrollX = TRUE,
          
          scrollCollapse = TRUE,
          
          autoWidth = FALSE,
          
          searching = TRUE,
          
          lengthChange = FALSE,
          
          columnDefs = list(
            
            list(
              targets = 0,
              width = "35px"
            ),
            
            list(
              targets = 1,
              width = "80px"
            ),
            
            list(
              targets = 2,
              width = "80px"
            ),
            
            list(
              targets = 3:(ncol(dat)-1),
              width = "80px"
            )
  
          )
          
        ),
        
        class = "compact stripe hover",
        
        width = "50%"
      )
      
      output$parameter_selector <- renderUI({
        
        selectInput(
          "selected_parameters",
          "Parameters to keep",
          choices = common_names,
          selected = common_names,
          multiple = TRUE
        )
        
      })
      
      
    }
  )
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
    
    if(is.null(params) || length(params)==0){
      
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
    
    raw_path <- parseDirPath(
      roots,
      input$raw_dir
    )
    
    raw_files <- list.files(
      raw_path,
      pattern="\\.fcs$",
      full.names=TRUE,
      ignore.case=TRUE
    )
    
    
    merge_summary(
      list(
        n_methods = nrow(dat),
        methods = dat$suffix,
        n_files = length(raw_files),
        n_parameters = length(params),
        parameters = unique(params),
        
        folders = rbind(
          data.frame(
            type = "RAW",
            folder = basename(raw_path),
            parent = dirname(raw_path),
            suffix = "",
            stringsAsFactors = FALSE
          ),
          data.frame(
            type = "Correction",
            folder = basename(dat$path),
            parent = dirname(dat$path),
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
              width=12,
              title="Merge summary",
              status="warning",
              solidHeader=TRUE,
              
              h3(
                icon("exclamation-triangle"),
                " Please choose markers"
              ),
              
              p(
                "Go to Parameters tab, select markers and click 'Use selected parameters'."
              )
            )
          )
        )
      )
      
    }
    
    
    # R??cup??ration du r??sum?? apr??s validation
    s <- merge_summary()
    
    if (is.null(s)) {
      
      return(
        fluidRow(
          column(
            12,
            box(
              width = 12,
              title = "Merge summary",
              status = "warning",
              solidHeader = TRUE,
              
              h3("No analysis available")
            )
          )
        )
      )
      
    }
    
    
    fluidRow(
      
      column(
        4,
        
        valueBox(
          s$n_methods,
          "Methods suffixes",
          icon = icon("wand-magic-sparkles"),
          color = "blue"
        )
        
      ),
      
      
      column(
        4,
        
        valueBox(
          s$n_files,
          "RAW files",
          icon = icon("file"),
          color = "green"
        )
        
      ),
      
      
      column(
        4,
        
        valueBox(
          s$n_parameters,
          "Parameters added",
          icon = icon("flask"),
          color = "purple"
        )
        
      )
      
    )
    
    
    fluidRow(
      
      column(
        6,
        
        box(
          width=12,
          title="Folders",
          status="primary",
          solidHeader=TRUE,
          
          DTOutput("folders_summary")
        )
        
      ),
      
      
      column(
        6,
        
        box(
          width=12,
          title="Selected parameters",
          status="info",
          solidHeader=TRUE,
          
          div(
            style="
        max-height:200px;
        overflow-y:auto;
        ",
            
            tags$p(
              paste(
                s$parameters,
                collapse=", "
              )
            )
            
          )
          
        )
        
      )
      
    )
    
  })
  output$folders_summary <- renderDT({
    
    s <- merge_summary()
    
    req(s$folders)
    
    datatable(
      s$folders,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = 't',
        columnDefs = list(
          list(
            targets = "_all",
            width = "150px"
          )
        )
      ),
      class = "compact stripe hover"
    )
    
  })
  
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
      
      
      for(m in names(matching)[-1]){
        
        method_files[[m]] <- matching[i,m]
        
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
    
    
    if(!dir.exists(out_dir))
      dir.create(out_dir)
    
    
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
              matching[i,method]
            
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
    
    
  })
  # ==================================================
  # VISUALIZATION MERGED FCS
  # ==================================================
  
  viz_dir <- reactive({
    
    raw_path <- parseDirPath(
      roots,
      input$raw_dir
    )
    
    file.path(
      raw_path,
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
    
    
    # IMPORTANT :
    # valeur envoy??e ?? Shiny = nom colonne r??el
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
    
    
    values <- lapply(
      markers,
      function(m){
        
        if(!m %in% colnames(dat))
          return(NULL)
        
        x <- dat[[m]]
        x <- x[is.finite(x)]
        
        if(length(x) < 10)
          return(NULL)
        
        log10(x + 1)
        
      }
    )
    
    
    keep <- !sapply(values, is.null)
    
    values <- values[keep]
    markers <- markers[keep]
    
    req(length(values) > 0)
    
    
    dens <- lapply(
      values,
      function(x){
        density(
          x,
          n = 512,
          na.rm = TRUE
        )
      }
    )
    
    
    names(dens) <- markers
    
    
    xlim <- range(
      unlist(lapply(dens, function(d)d$x)),
      finite = TRUE
    )
    
    
    ymax <- max(
      sapply(
        dens,
        function(d) max(d$y)
      )
    )
    
    
    # l??ger d??calage uniquement
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
      xlab = "log10 intensity",
      ylab = "Density"
    )
    
    
    for(i in seq_along(dens)){
      
      # petit d??calage vertical
      yshift <- (i-1) * offset
      
      
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
      
      paste0(
        "FCS_Merge_Report_",
        Sys.Date(),
        ".html"
      )
      
    },
    
    
    content = function(file){
      
      matching <- matching_rv()
      params <- selected_parameters_rv()
      
      req(matching)
      req(params)
      
      
      out_dir <- file.path(
        dirname(matching$raw_file[1]),
        "Merged_FCS"
      )
      
      
      # Cr??ation du rapport uniquement ici
      report <- create_merge_report(
        matching = matching,
        methods = names(matching)[-1],
        parameters = params,
        out_dir = out_dir
      )
      
      
      # Envoi du fichier au navigateur
      file.copy(
        report,
        file,
        overwrite = TRUE
      )
      
    }
  )
}











