#' Landing page
#' 
#' \code{.landing_page} creates the landing page of miaDash, where TreeSE
#' objects can be built and iSEE can be launched.
#'
#' @return The UI is defined by the function. A \code{NULL} value is invisibly
#'   returned.
#'
#' @name landing_page
#' @keywords internal

#' @rdname landing_page
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody box
#' @importFrom htmltools HTML br tags div tagList
#' @importFrom shinyjs disable
#' @importFrom shinyWidgets addSpinner
#' @importFrom utils data
.landing_page <- function(FUN, input, output, session) {
    # nocov start
    
    # Galaxy Interactive Tool wiring: "Send to Galaxy" buttons for the export
    # download, and the import of the raw tables staged by the tool wrapper.
    # Both are no-ops outside Galaxy, see R/galaxy_ie.R and R/galaxy_input.R.
    galaxy_ie_app("miadash", "MIADASH_OUTPUT_DIR")
    galaxy_registry <- galaxy_ie_send_server(input, output, session,
        label = "miaDash results")
    
    mia_datasets <- .import_datasets()
    
    output$allPanels <- renderUI({

        dashboardPage(
          
            dashboardHeader(disable = TRUE),
            dashboardSidebar(disable = TRUE),
            dashboardBody(
                
                tags$head(tags$style(HTML(".btn-primary {color: white}")),
                    galaxy_ie_send_ui(ids = c(ls(galaxy_registry),
                        galaxy_ie_isee_export_id()))),
              
                fluidRow(
                    .define_import_panel(mia_datasets),
                    .define_manipulate_panel(),
                    .define_estimate_panel()
                ),
                
                fluidRow(
                    .define_visualise_panel(),
                    .define_output_panel()
                )
            )
        )
    })
    
    ## Disable navbar buttons that are not linked to any observer yet
    disable("iSEE_INTERNAL_organize_panels")  # organize panels
    disable("iSEE_INTERNAL_link_graph")       # link graph
    disable("iSEE_INTERNAL_panel_settings")   # panel settings
    disable("iSEE_INTERNAL_draft_tour")       # tour draft
    
    rObjects <- reactiveValues(tse = NULL, appMode = "analysis")
    pObjects <- new.env()
    
    observe(.print_welcome_message())
    
    .create_import_observers(input, rObjects, pObjects)
    .create_manipulate_observers(input, rObjects, pObjects)
    .create_estimate_observers(input, rObjects, pObjects)
    # Update observers that depend on user input
    .update_observers(input, session, rObjects)
    # Overwrite iSEE general observers with miaDash original ones
    .create_general_observers(input, session, rObjects, pObjects)
    
    .create_launch_observers(FUN, input, session, rObjects)
    
    .render_overview(output, rObjects)
    .render_download(input, output, rObjects, galaxy_registry)

    # Import the raw tables staged by the Galaxy tool wrapper, if any. Runs once
    # on the first flush, and reads only environment variables, so nothing here
    # can loop; isolate() keeps the error path of .update_tse-style helpers from
    # registering a reactive dependency on rObjects$tse.
    observe({
        isolate(.galaxy_staged_import(rObjects, pObjects))
    })

    invisible(NULL)
    # nocov end
}

# Define boxes
.define_import_panel <- function(mia_datasets){
    # nocov start
    import_panel <- box(id = "import.panel", title = "Import", width = 4,
        status = "primary", solidHeader = TRUE, collapsible = TRUE,
        
        tabsetPanel(id = "format",
        
            tabPanel(title = "Dataset", value = "dataset", br(),
                    
                selectInput(inputId = "data", label = "Dataset:",
                    choices = mia_datasets, selected = mia_datasets[1L])),
            
            tabPanel(title = "R Object", value = "rds", br(),
                
                radioButtons(inputId = "rds_format", label = "Format:",
                    choices = .rdsFormats, inline = TRUE),
                
                fileInput(inputId = "file", label = "RDS:",
                    accept = ".rds", placeholder = "object.rds")),
            
            tabPanel(title = "Raw Data", value = "raw", br(),
                    
                fileInput(inputId = "assay", label = "Assays:",
                    accept = ".tsv", multiple = TRUE,
                    placeholder = "assay.tsv"),
                div(style = "margin-top: -20px"),
                        
                fileInput(inputId = "coldata", label = "colData:",
                    accept = ".tsv", placeholder = "coldata.tsv"),
                div(style = "margin-top: -20px"),
                        
                fileInput(inputId = "rowdata", label = "rowData:",
                    accept = ".tsv", placeholder = "rowdata.tsv"),
                div(style = "margin-top: -20px"),
                        
                fileInput(inputId = "row.tree", label = "rowTree:",
                    placeholder = "row.tree", accept = c(".tree", ".tre")),
                div(style = "margin-top: -20px"),
                      
                fileInput(inputId = "col.tree", label = "colTree:",
                    placeholder = "col.tree", accept = c(".tree", ".tre")),
                div(style = "margin-top: -20px"),
                      
                checkboxInput(inputId = "taxa.from.rownames",
                    label = "Derive rowData from assay rownames")),
                
            tabPanel(title = "Foreign", value = "foreign", br(),
                
                radioButtons(inputId = "ftype", label = "Format:",
                    choices = .foreignFormats, inline = TRUE),
                
                fileInput(inputId = "main.file", label = "Main file:",
                    accept = c(".biom", ".tsv", ".shared", ".QZA", ".txt"),
                    placeholder = "biom, tsv, shared, QZA or txt"),
                div(style = "margin-top: -20px"),
                
                fileInput(inputId = "col.data", label = "colData:",
                    accept = c(".tsv", ".design"),
                    placeholder = "tsv or design"),
                div(style = "margin-top: -20px"),
                
                conditionalPanel(
                    condition = paste0("input.ftype == 'Mothur' | ",
                        "input.ftype == 'QIIME2'"),
                    
                    fileInput(inputId = "f.rowdata", label = "rowData:",
                        accept = c(".taxonomy", ".qza"),
                        placeholder = "taxonomy or qza"),
                    div(style = "margin-top: -20px")),
                
                conditionalPanel(
                    condition = paste0("input.ftype == 'biom' | ",
                        "input.ftype == 'MetaPhlAn'"),
                    
                    fileInput(inputId = "tree.file", label = "rowTree:",
                        placeholder = "tree.tree",
                        accept = c(".tree", ".tre", ".qza")),
                    div(style = "margin-top: -20px")),
                
                conditionalPanel(
                    condition = paste0("input.ftype == 'biom' | ",
                        "input.ftype == 'HUMAnN'"),
                    
                    checkboxInput(inputId = "rm.tax.pref",
                        label = "Remove taxa prefixes")),
                
                conditionalPanel(condition = "input.ftype == 'biom'",
                    
                    checkboxInput(inputId = "rank.from.pref",
                        label = "Derive taxa from prefixes")),
                
                conditionalPanel(condition = "input.ftype == 'HUMAnN'",
                    
                    checkboxInput(inputId = "rm.hum.suf",
                        label = "Remove sample suffix")))),
        
        actionButton("import", "Upload", class = "btn-primary"))
    
    return(import_panel)
    # nocov end
}

.define_manipulate_panel <- function(){
    # nocov start
    manipulate_panel <- box(id = "manipulate.panel", title = "Manipulate",
        width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        
        tabsetPanel(id = "manipulate",
            
            tabPanel(title = "Subset", value = "subset", br(),
                
                radioButtons(inputId = "subkeep", label = "Keep:",
                    choices = c("prevalent", "rare"), inline = TRUE),
                
                selectInput(inputId = "subassay", label = "Assay:",
                    choices = NULL),
                
                sliderInput(inputId = "prevalence", value = 0, step = 0.01,
                    label = "Prevalence threshold:",  min = 0, max = 1),
                
                numericInput(inputId = "detection", value = 0, step = 1,
                    label = "Detection threshold:", min = 0)),
                
            tabPanel(title = "Agglomerate", value = "agglomerate", br(),
                
                selectInput(inputId = "taxrank", label = "Taxonomic rank:",
                    choices = NULL)),
                
            tabPanel(title = "Transform", value = "transform", br(),
                
                selectInput(inputId = "assay.type", label = "Assay:",
                    choices = NULL),
                
                selectInput(inputId = "trans_method", label = "Method:",
                    choices = .transformMethods),
                
                conditionalPanel(
                    condition = "input.trans_method == 'philr'",
                    
                    selectInput(inputId = "trans.tree", label = "Tree:",
                        choices = NULL)),
                
                checkboxInput(inputId = "pseudocount", label = "Pseudocount"),
                
                textInput(inputId = "assay.name", label = "Name:"),
                
                radioButtons(inputId = "margin", label = "Margin:",
                    choices = c("features", "samples"), inline = TRUE))),
              
        actionButton("apply", "Apply", class = "btn-primary"))
    
    return(manipulate_panel)
    # nocov end
}

.define_estimate_panel <- function(){
    # nocov start
    estimate_panel <- box(id = "estimate.panel", title = "Estimate", width = 4,
        status = "primary", solidHeader = TRUE, collapsible = TRUE,

        tabsetPanel(id = "estimate",
                                
            header = tagList(br(), selectInput(inputId = "estimate.assay",
                label = "Assay:", choices = NULL)),
                        
            tabPanel(title = "Quality", value = "quality",
                            
                checkboxGroupInput(inputId = "quality.metrics",
                    label = "Metrics:", choices = .qualityMetrics)),
                          
            tabPanel(title = "Alpha", value = "alpha",
                      
                selectInput(inputId = "alpha.index", label = "Metrics:",
                    multiple = TRUE, choices = .alphaMetrics)),

            tabPanel(title = "Beta", value = "beta",
                           
                radioButtons(inputId = "bmethod", label = "Method:",
                    choices = .betaMethods, inline = TRUE),
                      
                conditionalPanel(
                    condition = paste0("input.bmethod == 'MDS' || ",
                        "input.bmethod == 'NMDS' || input.bmethod == 'RDA'"),
                     
                    selectInput(inputId = "beta.index", label = "Metric:",
                        choices = .betaMetrics)),
                    
                conditionalPanel(condition = "input.bmethod == 'RDA'",
                    
                    textInput(inputId = "rda.formula", label = "Formula:",
                        placeholder = "data ~ var1 + var2 * var3")),
                    
                    numericInput(inputId = "ncomponents", value = 5,
                        label = "Number of components:", min = 1, step = 1)),
                    
            tabPanel(title = "Cluster", value = "cluster",
                                 
                radioButtons(inputId = "cmethod", label = "Method:",
                    choices = .clustMethods, inline = TRUE),
                            
                conditionalPanel(
                    condition = paste0("input.cmethod == 'Dmm' | ",
                        "input.cmethod == 'Kmeans'"),
                                
                    numericInput(inputId = "kclusters", value = 3,
                        label = "Number of clusters:", min = 1, step = 1)),
                            
                conditionalPanel(condition = "input.cmethod == 'Dmm'",
                              
                    selectInput(inputId = "dmm.type", label = "Criterion:",
                        choices = .DmmCriteria, selected = .DmmCriteria[1]),
                                
                    numericInput(inputId = "dmm.seed", value = 1L,
                        label = "Seed:", min = 0, step = 1)),
                            
                conditionalPanel(condition = "input.cmethod == 'NNGraph'",
                              
                    numericInput(inputId = "kneighbours", value = 10,
                        label = "Number of neighbours:", min = 1, step = 1),
                                
                    checkboxInput(inputId = "nn.shared",
                        label = "Construct shared graph")),
                            
                    checkboxInput(inputId = "clust.full",
                        label = "Add to metadata"),
                            
                    radioButtons(inputId = "clust.margin", label = "Margin:",
                        inline = TRUE, choices = c("samples", "features"),
                        selected = "features")),
                        
            footer = textInput(inputId = "estimate.name", label = "Name:")),
              
        actionButton("compute", "Compute", class = "btn-primary"))
    
    return(estimate_panel)
    # nocov end
}

.define_visualise_panel <- function(){
    # nocov start
    visualise_panel <- box(id = "visualise.panel", title = "Visualise",
        width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
      
        selectInput(inputId = "panels", label = "Panels:",
            choices = c(.miaDashDefaultPanels, .miaDashOtherPanels),
            multiple = TRUE, selected = c(.miaDashDefaultPanels)),
      
      actionButton("launch", "Launch iSEE",
                   class = "btn-primary"))
    
    return(visualise_panel)
    # nocov end
}

.define_output_panel <- function(){
    # nocov start
    output_panel <- box(id = "output.panel", title = "Output", width = 8,
        status = "primary", solidHeader = TRUE, collapsible = TRUE,
        
        addSpinner(verbatimTextOutput(outputId = "object"), color = "#007bff"))
    
    return(output_panel)
    # nocov end
}