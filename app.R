
library(shiny)

## Load required packages
source('helpers.R')
source('static_content.R')

## UI elements - Data Explorer (DE)
## - Menu
##  1) Response kinetics - single:  ("autodry_kinresp_ui[['single']]")
##  2) Response kinetics - global:  ("autodry_kinresp_ui[['global']]")
##  3) Autophagy competence - single: ("autodry_competence_ui[['single']]")
##  4) Autophagy competence - global: ("autodry_competence_ui[['global']]")
##

get_kinetic_response_params <- function(){

  params <-
    c("Perturbation -N",
      "Perturbation +N",
      "Perturbation overall",
      "Slope (sigmoid) -N",
      "Slope (sigmoid) +N",
      "Slope (tangent) -N",
      "Slope (tangent) +N",
      "Autophagy start",
      "Autophagy max",
      "Autophagy final",
      "T50 -N",
      "T50 +N",
      "T lag -N",
      "T lag +N",
      "T final -N",
      "T final +N",
      "Dynamic range -N",
      "Dynamic range +N")
  return(params)

}


autodry_competence_ui <- list()
autodry_kinresp_ui <- list()
for(e in c('single','global')){
  autodry_competence_ui[[e]] <- list()
  autodry_kinresp_ui[[e]] <- list()
}

autodry_kinresp_ui[['single']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,
    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        condition = "!output.isSafari",
        shiny::selectizeInput(
          "gene_id_kinresp_single",
          "Select gene/ORF mutant",
          options =
            list(maxOptions = 5500),     # Set this to your max or desired number
          choices = NULL,
          multiple = FALSE,
          width = "100%")
      ),
      shiny::conditionalPanel(
        condition = "output.isSafari",
        shiny::tagList(
          shiny::textInput(
            "gene_search_input_kinresp_single",
            "Search for gene/ORF mutant",
            "atg", width = "100%"),
          shiny::uiOutput("gene_mutant_suggestions_kinresp_single")
        )
      ),
      width = "320px"
    ),
    bslib::page_fillable(
      fill = TRUE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          shiny::textOutput("selected_gene_kinetic")),
        bslib::card_body(
          fill = TRUE,
          fillable = TRUE,
          min_height = "700px",
          shiny::htmlOutput("gene_info_kinetic"),
          shiny::uiOutput("kinresp_warning")
        )
      ),
      htmltools::div(
        style = "padding-top: 1rem;",
        shiny::includeHTML("data/section_content/citation_footnote.md")
      )
    )
  )

autodry_kinresp_ui[['global']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,
    sidebar = bslib::sidebar(
      list(
        conditionalPanel(
          condition = "output.isSafari",
          tagList(
            shiny::textInput(
              "search_term_kinresp_global", 
              "Search gene/ORF mutants:"),
            shiny::uiOutput("matched_genes_kinresp_global"),
            shiny::actionButton(
              "add_gene_kinresp_global", 
              "Add to selection"),
            shiny::actionButton(
              "clear_highlighted_kinresp_global", 
              "Clear all"),
            shiny::uiOutput("highlighted_genes_kinresp_global")
          )
        ),
        conditionalPanel(
          condition = "!output.isSafari",
          shiny::selectizeInput(
            inputId = "gene_id_kinresp_global",
            label = "Highlight gene/ORF mutants (max 10)",
            selected = "AAC1 / YMR056C",
            choices = NULL,
            
            multiple = TRUE,
            options = list(
              maxOptions = 5500,     # Set this to your max or desired number
              minItems = 1,
              maxItems = 10))
        ),
        shiny::selectInput(
          "x_var_kin",
          "X-axis variable",
          get_kinetic_response_params(),
          selected = "T50 +N"
        ),
        shiny::selectInput(
          "y_var_kin",
          "Y-axis variable",
          get_kinetic_response_params(),
          selected = "T50 -N"
        ),
        shiny::checkboxInput(
          "use_perturbation_data",
          "Use normalized values",
          value = F),
        shiny::checkboxInput(
          "contour", "Show contour plots", value = F)),
      width = "320px"
    ),
    bslib::page_fillable(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Global autophagy response kinetics"),
        bslib::card_body(
          fill = TRUE,
          fillable = TRUE,
          min_height = "700px",
          shiny::plotOutput("kinresp_global_plot")
        )
      ),
      fill = T
    )
  )


autodry_competence_ui[['single']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,

    sidebar = bslib::sidebar(
      shiny::conditionalPanel(
        condition = "!output.isSafari",
        shiny::selectizeInput(
          "gene_id_acomp_single",
          "Select gene/ORF mutant",
          options =
            list(maxOptions = 5500),
          choices = NULL,
          multiple = FALSE,
          width = "100%")
      ),
      shiny::conditionalPanel(
        condition = "output.isSafari",
        shiny::tagList(
          shiny::textInput(
            "gene_search_input_acomp_single",
            "Search for gene/ORF mutant",
            "", width = "100%"),
          shiny::uiOutput("gene_mutant_suggestions_acomp_single")
        )
      ),
      width = "320px"
    ),
    bslib::page_fillable(
      fill = TRUE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          shiny::textOutput("selected_gene_bf")),
        bslib::card_body(
          shiny::htmlOutput("gene_info_bf"),
          fill = TRUE,
          fillable = TRUE,
          min_height = "700px",
          shiny::uiOutput("acomp_warning")
        )
      ),
      shiny::includeHTML(
        "data/section_content/citation_footnote.md")
    )
  )

autodry_competence_ui[['global']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,
    sidebar = bslib::sidebar(
      list(
        conditionalPanel(
          condition = "output.isSafari",
          tagList(
            shiny::textInput(
              "search_term_acomp_global", 
              "Search gene/ORF mutants:"),
            shiny::uiOutput("matched_genes_acomp_global"),
            shiny::actionButton(
              "add_gene_acomp_global", 
              "Add to selection"),
            shiny::actionButton(
              "clear_highlighted_acomp_global", 
              "Clear selection"),
            shiny::uiOutput("highlighted_genes_acomp_global")
          )
        ),
        conditionalPanel(
          condition = "!output.isSafari",
          shiny::selectizeInput(
            inputId = "gene_id_acomp_global",
            label = "Highlight gene/ORF mutants (max 10)",
            selected = "AAC1 / YMR056C",
            choices = NULL,
            
            multiple = TRUE,
            options = list(
              maxOptions = 5500,     # Set this to your max or desired number
              minItems = 1,
              maxItems = 10))
        ),
        shiny::selectInput(
          "bf_x_var",
          "X-axis variable",
          c("Overall autophagy",
            "Autophagosome formation",
            "Autophagosome clearance"),
          selected = "Autophagosome formation"
        ),
        shiny::selectInput(
          "bf_y_var",
          "Y-axis variable",
          c("Overall autophagy",
            "Autophagosome formation",
            "Autophagosome clearance"),
          selected = "Autophagosome clearance"
        ),
        shiny::checkboxInput(
          "bf_library_adjustment",
          "Perform library correction", value = F),
        shiny::checkboxInput(
          "bf_contour", "Show contour plots", value = F)
      ), width = "320px"
    ),
    bslib::page_fillable(
      fill = TRUE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark", "Global autophagy competence"),
        bslib::card_body(
          fill = TRUE,
          fillable = TRUE,
          min_height = "700px",
          shiny::plotOutput("acomp_global")
        )
      )
    )
  )


gw_autoph_response_data <- load_kinetic_response_data()
gw_autoph_competence_data <- load_autophagy_competence_data()
gw_autoph_type_data <- load_type_data()
main_gene_ids <- load_main_gene_ids()

search_page <-
  bslib::nav_panel(
    "Main",
    fill = TRUE,
    bslib::page_fillable(
      tags$style(HTML(".card {border-radius: 0.9rem;}")),
      tags$style(HTML(".card2 {border-radius: 0.1rem;}")),
      bslib::card(
        class = "card2",
        full_screen = F,
        fillable = F,
        fill = F,
        bslib::card_body(
          htmltools::div(style = "margin-top: 40px;"),
          htmltools::div(
            #class = "d-flex align-items-center justify-content-center",
            class = "d-flex flex-column align-items-center justify-content-between",
            style = "min-height: 60vh; height: 100%; padding: 2rem;",
            htmltools::div(
              class = "text-center",
              style = "max-width: 600px; width: 100%;",
              # 1. Logo or Image
              htmltools::tags$img(
                src = "Autodry_logo.png",
                height = "150px",
                style = "margin-bottom: 30px;"),

              # 2. 2x2 Radio Grid
              shiny::conditionalPanel(
                condition = "output.isSafari",
                htmltools::div(
                  class = "mb-3",
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        "analysis_view_safari", "",
                        choices = c(
                          "Kinetic response" = "kinresp_single"),
                        selected = "kinresp_single",
                        inline = TRUE
                      )
                    ),
                    column(
                      6,
                      radioButtons(
                        "analysis_view_safari", "",
                        choices = c(
                          "Autophagy competence" = "acomp_single"),
                        selected = "kinresp_single",
                        inline = TRUE
                      )
                    )
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "!output.isSafari",
                htmltools::div(
                  class = "mb-3",
                  fluidRow(
                    column(
                      6,
                      radioButtons(
                        "analysis_view", "",
                        choiceNames = c(
                          "Kinetic response (single)",
                          "Kinetic response (global)"),
                        choiceValues = c(
                          "kinresp_single",
                          "kinresp_global"),
                        selected = "kinresp_single",
                        inline = TRUE
                      )
                    ),
                    column(
                      6,
                      radioButtons(
                        "analysis_view", "",
                        choiceNames = c(
                          "Autophagy competence (single)",
                          "Autophagy competence (global)"),
                        choiceValues = c(
                          "acomp_single",
                          "acomp_global"),
                        selected = "kinresp_single",
                        inline = TRUE
                      )
                    )
                  )
                )
              ),
            

              shiny::textInput(
                "gene_search_input",
                "Search for yeast gene/ORF mutant",
                "", width = "100%"),
              shiny::uiOutput("gene_mutant_suggestions"),

              # 4. Submit Button
              shiny::actionButton(
                "submit_btn",
                "Submit",
                class = "btn-primary mt-3"),
              htmltools::div(style = "margin-top: 10px;")
            ),
            #htmltools::div(style = "flex-grow: 1;"),
            htmltools::div(
              shiny::HTML("Citation: Chica et al., bioRxiv (2025). <b>Time-resolved functional genomics using deep learning reveals a global hierarchical control of autophagy</b>. <a name='citation' href='https://www.biorxiv.org/content/10.1101/2024.04.06.588104v2' target='_blank'>doi: 10.1101/2024.04.06.588104</a>."),
            ),
            shiny::conditionalPanel(
              condition = "output.isSafari",
              htmltools::div(
                class = "text-center",
                style = "font-color: red; max-width: 900px; width: 100%;",
                shiny::HTML("<br><br><i style='color:red'><b>WARNING</b>: Data interaction works suboptimally with the Safari web browser - please consider using Chrome/Firefox/Edge/Brave when visiting AutoDRY</i>")
              )
            ),
            htmltools::div(style = "margin-bottom: 20px;")
          )
        ),
        autodry_footer
      )
  ))

close_navbar_dropdown_js <-
  shiny::tags$script("
    Shiny.addCustomMessageHandler(
        'toggleDropdown',
        function toggleDropdown(msg) {
          $('.dropdown-menu').removeClass('show')
        });
      ")

detect_browser_js <-
  shiny::tags$script(shiny::HTML("
    Shiny.addCustomMessageHandler('getBrowserInfo', function(message) {
      var userAgent = navigator.userAgent;
      var browser = 'Unknown';
      if (userAgent.indexOf('Chrome') > -1 && userAgent.indexOf('Edg') === -1) {
        browser = 'Chrome';
      } else if (userAgent.indexOf('Safari') > -1 && userAgent.indexOf('Chrome') === -1) {
        browser = 'Safari';
      } else if (userAgent.indexOf('Firefox') > -1) {
        browser = 'Firefox';
      } else if (userAgent.indexOf('Edg') > -1) {
        browser = 'Edge';
      }
      Shiny.setInputValue('client_browser', browser, {priority: 'event'});
    });
  "))

ui_original <-
  bslib::page_navbar(
    id = "main_nav",
    header = htmltools::tags$head(
      close_navbar_dropdown_js,
      detect_browser_js,
      htmltools::tags$link(rel="shortcut icon", href="favicon-ous.svg")),
    navbar_options = bslib::navbar_options(
      bg = "#593196",
    ),
    #fillable_mobile = TRUE,
    theme = bslib::bs_theme(
      bootswatch = "pulse",
      version = 5,
      "dropdown-menu-bg" = "#fff") |>
      bslib::bs_add_rules(
        list(
          ".navbar {padding-left: 20px; padding-right: 20px;}",
          ".nav.navbar-nav {
              font-size:1.2em;
              background-color: $primary !important;
              color: #ffffff !important;
           }",
          ".navbar-nav .dropdown-menu {
              background-color: #ffffff !important;
              border: none !important; /* Remove default border that may appear dark */
              box-shadow: none !important; /* Prevent shadow that can appear as black */
              padding-top: 0 !important;
              padding-bottom: 0 !important;
              margin-top: 0 !important;
              margin-bottom: 0 !important;
            }")),
    fillable = TRUE,
    fillable_mobile = TRUE,
    window_title = "AutoDRY",
    title = htmltools::span(
      "AutoDRY: Autophagy Dynamics Repository Yeast",
      style="font-size:1.6rem; padding-left:10px;"),
    bslib::nav_spacer(),
    search_page,
    bslib::nav_menu(
      title = "Data Explorer",
      bslib::nav_panel(title = "Response kinetics - single",
                       fill = TRUE,
                       value = "kinresp_single_view",
                       autodry_kinresp_ui[['single']]),
      bslib::nav_panel(title = "Response kinetics - global",
                       fill = TRUE,
                       value = "kinresp_global_view",
                       autodry_kinresp_ui[['global']]),
      bslib::nav_panel(title = "Autophagy competence - single",
                       fill = TRUE,
                       value = "acomp_single_view",
                       autodry_competence_ui[['single']]),
      bslib::nav_panel(title = "Autophagy competence - global",
                       fill = TRUE,
                       value = "acomp_global_view",
                       autodry_competence_ui[['global']])
    ),
    bslib::nav_menu(
      title = "Documentation",
      bslib::nav_panel(title = "About the study", fill = TRUE, about_page),
      bslib::nav_panel(title = "Portal usage", fill = TRUE, docs_page)
    ),

    bslib::nav_panel(title = htmltools::span(
      "DISCLAIMER", style="padding-right:10px;"),
      disclaimer_page)
  )

## Shiny server functions
server <- function(input, output, session) {

  #user_browser <- reactiveVal()
  selected_gene <- shiny::reactiveVal()

  ### BROWSER DETECTION ###

  observe({
    session$sendCustomMessage("getBrowserInfo", list())
  })

  browser_type <- reactiveVal("Unknown")

  observeEvent(input$client_browser, {
    if (grepl("safari", tolower(input$client_browser))
              && !grepl("chrome", tolower(input$client_browser))) {
      browser_type("Safari")
    } else {
      browser_type("Non-Safari")
    }
  })

  output$isSafari <- reactive({
    browser_type() == "Safari"
  })
  outputOptions(output, "isSafari", suspendWhenHidden = FALSE)


  #### MAIN UI LOGIC - ANY BROWSER TYPE ####

  filtered_genes_main <- shiny::reactive({
    req(nchar(input$gene_search_input) >= 2)
    matches <- grep(
      input$gene_search_input,
      main_gene_ids$orf_gene_id,
      ignore.case = TRUE,
      value = TRUE)
    head(matches, 80)  # limit suggestions for performance
  })

  # Render selectInput dynamically once user types enough
  output$gene_mutant_suggestions <- shiny::renderUI({
    if (nchar(input$gene_search_input) < 2) return(NULL)

    choices <- filtered_genes_main()
    if (length(choices) == 0) return("No matches found.")

    cat("Browser: ", input$client_browser, " - ", browser_type(), "\n")

    shiny::selectInput(
      "main_gene_search",
      "Select a match in the list to explore different views of autophagy dynamics:",
      choices = choices,
      multiple = FALSE,
      width = "100%",
      selectize = F)
  })
  
  shiny::observeEvent(input$submit_btn, {
    selected_gene(input$main_gene_search)
    
    # Switch to appropriate nav_panel based on selected analysis view
    target_panel <- NULL
    if (input$client_browser != "Safari"){
      target_panel <- switch(
        input$analysis_view,
        "kinresp_single" = "kinresp_single_view",
        "kinresp_global" = "kinresp_global_view",
        "acomp_single" = "acomp_single_view",
        "acomp_global" = "acomp_global_view")
    }else{
      target_panel <- switch(
        input$analysis_view_safari,
        "kinresp_single" = "kinresp_single_view",
        "acomp_single" = "acomp_single_view")
    }
    
    cat(input$client_browser, "\t",
        "Switching to panel:", target_panel, "\n")
    shiny::updateNavbarPage(
      inputId = "main_nav",
      selected = target_panel)
    
    # Update the corresponding selectizeInput in the selected panel
    if (input$client_browser != "Safari"){
      #cat("Non-Safari\tupdating ", paste0("gene_id_", input$analysis_view), "\n")
      shiny::updateSelectizeInput(
        session,
        paste0("gene_id_", input$analysis_view),
        selected = input$main_gene_search)
    }
    else {
      shiny::updateTextInput(
        session,
        inputId = paste0("gene_search_input_",input$analysis_view_safari),
        value = input$gene_search_input
      )
      
      shiny::updateSelectInput(
        session,
        inputId = paste0("gene_id_", input$analysis_view_safari),
        choices = input$main_gene_search,
        selected = input$main_gene_search
      )
    }
    
  })
  
  shiny::observe({
    if (input$main_nav %in% "kinresp_single_view" ||
        input$main_nav %in% "kinresp_global_view" ||
        input$main_nav %in% "acomp_single_view" ||
        input$main_nav %in% "acomp_global_view") {
      session$sendCustomMessage(
        type = "toggleDropdown",
        message = list(msg = "hide dropdown"))
    }
  })

  #### KINETIC RESPONSE SINGLE VIEW UI LOGIC ####

  filtered_genes_kinresp_single <- shiny::reactive({
    req(nchar(input$gene_search_input_kinresp_single) >= 2)
    matches <- grep(
      input$gene_search_input_kinresp_single,
      main_gene_ids$orf_gene_id,
      ignore.case = TRUE,
      value = TRUE)
    head(matches, 80)  # limit suggestions for performance
  })

  # Render selectInput dynamically once user types enough
  output$gene_mutant_suggestions_kinresp_single <- shiny::renderUI({
    if (nchar(input$gene_search_input_kinresp_single) < 2) return(NULL)

    choices_RK1 <- filtered_genes_kinresp_single()
    if (length(choices_RK1) == 0) return("No matches found.")

    shiny::selectInput(
      "gene_id_kinresp_single",
      "Chosen gene/ORF:",
      choices = choices_RK1,
      multiple = FALSE,
      width = "100%",
      selectize = F)
  })

  #### AUTOPHAGY COMPETENCE SINGLE VIEW UI LOGIC ####

  filtered_genes_acomp_single <- shiny::reactive({
    req(nchar(input$gene_search_input_acomp_single) >= 2)
    matches <- grep(
      input$gene_search_input_acomp_single,
      main_gene_ids$orf_gene_id,
      ignore.case = TRUE,
      value = TRUE)
    head(matches, 80)  # limit suggestions for performance
  })

  # Render selectInput dynamically once user types enough
  output$gene_mutant_suggestions_acomp_single <- shiny::renderUI({
    if (nchar(input$gene_search_input_acomp_single) < 2) return(NULL)

    choices_AC1 <- filtered_genes_acomp_single()
    if (length(choices_AC1) == 0) return("No matches found.")

    #cat("Browser: ", input$client_browser, " - ", browser_type(), "\n")

    shiny::selectInput(
      "gene_id_acomp_single",
      "Chosen gene/ORF:",
      choices = choices_AC1,
      multiple = FALSE,
      width = "100%",
      selectize = F)
  })
  
  #### KINETIC RESPONSE GLOBAL VIEW UI LOGIC - SAFARI ####
  
  selected_genes_kinresp_global <- reactiveVal(character())
  
  filtered_choices_kinresp_global <- reactive({
    req(input$search_term_kinresp_global)
    matches <- grep(
      input$search_term_kinresp_global,
      main_gene_ids$orf_gene_id,
      value = TRUE,
      ignore.case = TRUE)
    head(matches, 80)
  })
  
  output$matched_genes_kinresp_global <- renderUI({
    if (nchar(input$search_term_kinresp_global) < 2) return(NULL)
    shiny::selectInput(
      "match_select_kinresp_global",
      "Matching gene/ORF mutants:",
      choices = filtered_choices_kinresp_global(),
      selectize = FALSE)
  })
  
  observeEvent(input$add_gene_kinresp_global, {
    current <- selected_genes_kinresp_global()
    new_item <- input$match_select_kinresp_global
    if (!is.null(new_item) && !(new_item %in% current)) {
      if(length(c(current,new_item)) <= 10){
        selected_genes_kinresp_global(c(current, new_item))
      }
      else {
        shiny::showNotification(
          "A maximum of 10 genes/ORFs can be highlighted.",
          type = "message")
      }
    }
  })
  
  output$highlighted_genes_kinresp_global <- renderUI({
    choices <- selected_genes_kinresp_global()
    checkboxGroupInput(
      "gene_id_kinresp_global",
      "Highlighted gene/ORFs (uncheck to remove):",
      choices = choices,
      selected = choices)
  })
  
  observeEvent(input$gene_id_kinresp_global, {
    selected_genes_kinresp_global(input$gene_id_kinresp_global)
  })
  
  observeEvent(input$clear_highlighted_kinresp_global, {
    selected_genes_kinresp_global(character())
  })

  
  #### AUTOPHAGY COMPETENCE GLOBAL VIEW UI LOGIC - SAFARI ####
  
  selected_genes_acomp_global <- reactiveVal(character())
  
  filtered_choices_acomp_global <- reactive({
    req(input$search_term_acomp_global)
    matches <- grep(
      input$search_term_acomp_global,
      main_gene_ids$orf_gene_id,
      value = TRUE,
      ignore.case = TRUE)
    head(matches, 80)
  })
  
  output$matched_genes_acomp_global <- renderUI({
    if (nchar(input$search_term_acomp_global) < 2) return(NULL)
    shiny::selectInput(
      "match_select_acomp_global",
      "Matching gene/ORF mutants:",
      choices = filtered_choices_acomp_global(),
      selectize = FALSE)
  })
  
  observeEvent(input$add_gene_acomp_global, {
    current <- selected_genes_acomp_global()
    new_item <- input$match_select_acomp_global
    if (!is.null(new_item) && !(new_item %in% current)) {
      if(length(c(current,new_item)) <= 10){
        selected_genes_acomp_global(c(current, new_item))
      }
      else {
        shiny::showNotification(
          "A maximum of 10 genes/ORFs can be highlighted.",
          type = "message")
      }
    }
  })
  
  output$highlighted_genes_acomp_global <- renderUI({
    choices <- selected_genes_acomp_global()
    checkboxGroupInput(
      "gene_id_acomp_global",
      "Highlighted gene/ORFs (uncheck to remove):",
      choices = choices,
      selected = choices)
  })
  
  observeEvent(input$gene_id_acomp_global, {
    selected_genes_acomp_global(input$gene_id_acomp_global)
  })
  
  observeEvent(input$clear_highlighted_acomp_global, {
    selected_genes_acomp_global(character())
  })
  
  BF_variables <- colnames(
    gw_autoph_competence_data[['bf_overall']])[5:7]

  ac_global_init <- list()

  ## these should be initialized
  ac_global_init[['Positions']] <- c()
  ac_global_init[['mat']] <-
    gw_autoph_competence_data[['bf_overall']] |>
    as.data.frame()

  ac_global_init[['mat']]$Type <- gw_autoph_type_data$Type[match(
    paste(ac_global_init[['mat']]$Gene,
          ac_global_init[['mat']]$ORF),paste(
      gw_autoph_type_data$Gene,
      gw_autoph_type_data$ORF))]

  ac_global_init$mat$Type[which(ac_global_init$mat$Plate_controls == "+")] <-
    "KO"
  ac_global_init$mat$Type[which(ac_global_init$mat$Plate_controls == "+" & is.na(ac_global_init$mat$ORF))] <-
    "WT"

  ac_global_init[['mat_select']] <- data.frame()
  ac_global_init[['mat_select']] <-
    gw_autoph_competence_data[['bf_overall']] |>
    subset(paste(Plate, Position) %in% ac_global_init$Positions) |>
    as.data.frame()


  ac_global_init[['X']] <- BF_variables[1]
  ac_global_init[['Y']] <- BF_variables[1]
  ac_global_init[['lab_x']] <-
    paste0("<br><b>Overall autophagy</b><br><br>",
           "<i>log BF (WT:ATG1)</i>")
  ac_global_init[['lab_y']] <-
    paste0("<br><b>Overall autophagy</b><br><br>",
           "<i>log BF (WT:ATG1)</i>")

  ac <- shiny::reactiveValues(
    'mat' = ac_global_init[['mat']],
    'mat_select' = ac_global_init[['mat_select']],
    'X' = ac_global_init[['X']],
    'Y' = ac_global_init[['Y']],
    'lab_x' = ac_global_init[['lab_x']],
    'lab_y' = ac_global_init[['lab_y']],
    'Positions' = ac_global_init[['Positions']],
  )
  
  ac_global_state <- ac_global_init

  Value <- "Value"
  kinresp_global_init <- list()
  kinresp_global_init[['Value']] <- Value
  kinresp_global_init[['X']] = "T50 +N"
  kinresp_global_init[['Y']] = "T50 -N"
  kinresp_global_init[['Positions']] <- c()
  kinresp_global_init[['mat']] <- as.data.frame(
      gw_autoph_response_data[['ds_parms']] |>
        dplyr::filter(
          .data$Parameter %in%
            c(kinresp_global_init[['X']],
              kinresp_global_init[['Y']])) |>
        dplyr::select(c("Plate","Position","ORF",
                        "Gene","primary_identifier",
                        "Reference_sets",
                        "Parameter",
                        rlang::sym(Value))) |>
        tidyr::pivot_wider(
          names_from = Parameter,
          values_from = Value))
  kinresp_global_init[['mat']]$X <-
    kinresp_global_init[['mat']][, kinresp_global_init[['X']]]
  kinresp_global_init[['mat']]$Y <-
    kinresp_global_init[['mat']][, kinresp_global_init[['Y']]]
  kinresp_global_init[['mat_select']] <- data.frame()

  kinresp_global_state <- kinresp_global_init

  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_kinresp_single",
        choices = gw_autoph_response_data$gene_info_kinetic$orf_gene_id,
        server = TRUE)
    }
  })

  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_kinresp_global",
        selected = "AAC1 / YMR056C",
        choices = gw_autoph_response_data$gene_info_kinetic_multi$orf_gene_id,
        server = TRUE)
    }
  })

  # Dynamically show plot or warning
  output$kinresp_warning <- shiny::renderUI({
    if (is.null(input$gene_id_kinresp_single) || input$gene_id_kinresp_single == "") {
      div(
        style = "color: red; font-weight: bold;",
        "⚠️ Please select a gene/ORF mutant to visualize the autophagy response kinetics."
      )
    } else {
      shiny::plotOutput("kinresp", height = "100%")
    }
  })

  output$kinresp <- shiny::renderPlot({
    req(input$gene_id_kinresp_single)
    plot_kinetic_response(
      response_data = gw_autoph_response_data$per_ko[[input$gene_id_kinresp_single]])
  })

  output$kinresp_global_plot <- shiny::renderPlot({
    req(kinresp_global_state_mat())
    req(kinresp_global_XY_vars())
    
    mat_select <- kinresp_global_state_mat_select()
    mat <- kinresp_global_state_mat()
    X <- kinresp_global_XY_vars()[['X']]
    Y <- kinresp_global_XY_vars()[['Y']]
                                
    plot_kinetic_response_global(
      mat = mat,
      mat_select = mat_select,
      X = X,
      Y = Y,
      show_library_type_contour = input$contour)
  })

  output$acomp_global <- shiny::renderPlot({
    req(acomp_global_state_selected())
    req(acomp_global_state_library_adjustment())
    mat_select <- acomp_global_state_selected()
    ac_global_state <- acomp_global_state_library_adjustment()
    ac_global_state$mat_select <- mat_select
    plot_autophagy_competence_global(
      ac_multi_data = ac_global_state,
      show_library_type_contour = input$bf_contour)
  })

  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_acomp_global",
        choices = gw_autoph_competence_data$gene_info_bf$orf_gene_id,
        selected = "AAC1 / YMR056C",
        server = TRUE
      )
    }
  })

  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_acomp_single",
        choices = gw_autoph_competence_data$gene_info_bf$orf_gene_id,
        server = TRUE)
    }
  })


  # Dynamically show plot or warning
  output$acomp_warning <- shiny::renderUI({
    if (is.null(input$gene_id_acomp_single) || input$gene_id_acomp_single == "") {
      div(
        style = "color: red; font-weight: bold;",
        "⚠️ Please select a gene/ORF mutant to visualize the autophagy competence."
      )
    } else {
      shiny::plotOutput("acomp", height = "100%")
    }
  })

  output$acomp <- shiny::renderPlot({
    req(input$gene_id_acomp_single)
    plot_autophagy_competence(
      competence_data =
        gw_autoph_competence_data$per_ko[[input$gene_id_acomp_single]])
  })

  output$selected_gene_kinetic <- shiny::renderText({
    response_data <-
      gw_autoph_response_data$per_ko[[input$gene_id_kinresp_single]]
    library_type <- ""
    if("slibrary" %in% names(response_data)){
      library_type <- response_data$slibrary
    }
    paste("Autophagy response kinetics -",
          input$gene_id_kinresp_single, library_type)
  })

  output$selected_gene_bf <- shiny::renderText({
    competence_data = gw_autoph_competence_data$per_ko[[input$gene_id_acomp_single]]
    library_type <- ""
    if("Library" %in% names(competence_data)){
      library_type <- competence_data$Library
    }
    paste("Autophagy competence -",
          input$gene_id_acomp_single, library_type)
  })

  output$gene_info_kinetic <- shiny::renderUI({
    ginf <- show_gene_info(
      primary_id = input$gene_id_kinresp_single,
      gene_info = gw_autoph_response_data$gene_info_kinetic
    )
    shiny::HTML(
      "<div><ul><li>Genename: ",ginf[['sgd_link']],"</li>",
      "<li>Description: ",ginf[['description']],"</li>",
      "<li>Human orthologs: ",ginf[['human_orthologs']],"</li>",
      "<li>Autophagy perturbation response profile: ",
      ginf[['response_profile']],"</li>",
      "<li><b>Tip</b>: Expand the plot by clicking on the 'Expand' link",
      "when hovering the mouse in the bottom right corner of the plot </li>",
      "</ul></div>")
  })

  output$gene_info_bf <- shiny::renderUI({
    ginf <- show_gene_info(
      primary_id = input$gene_id_acomp_single,
      gene_info = gw_autoph_competence_data$gene_info_bf
    )
    shiny::HTML(
      "<div><ul><li>Genename: ",ginf[['sgd_link']],"</li>",
      "<li>Description: ",ginf[['description']],"</li>",
      "<li>Human orthologs: ",ginf[['human_orthologs']],"</li>",
      "<li><b>Tip</b>: Expand the plot by clicking on the 'Expand' link",
      "when hovering the mouse in the bottom right corner of the plot </li>",
      "</ul></div>")
  })

  #observe ({
  acomp_global_state_vars <- reactive({
    req(input$bf_x_var)
    req(input$bf_y_var)
    ac <- list()
    ac[['X']] <- ac_global_init[['X']]
    ac[['Y']] <- ac_global_init[['Y']]
    if(input$bf_x_var == "Autophagosome formation"){
      ac[['X']] <- BF_variables[3]
    }
    if(input$bf_y_var == "Autophagosome formation"){
      ac[['Y']] <- BF_variables[3]
    }
    if(input$bf_x_var == "Autophagosome clearance"){
      ac[['X']] <- BF_variables[2]
    }
    if(input$bf_y_var == "Autophagosome clearance"){
      ac[['Y']] <- BF_variables[2]
    }

    if(grepl("VAM6.ATG1", ac[['X']], fixed = T)){
      ac[['lab_x']] <- paste0("<br><b>Autophagosome formation</b><br><br>",
                      "<i>log BF (VAM6:ATG1)</i>")
    }else if(grepl("WT.ATG1", ac[['X']], fixed = T)){
      ac[['lab_x']] <- paste0("<br><b>Overall autophagy</b><br><br>",
                      "<i>log BF (WT:ATG1)</i>")
    }else{
      ac[['lab_x']] <- paste0("<br><b>Autophagosome clearance</b><br><br>",
                      "<i>log BF (WT:VAM6)</i>")
    }
    if(grepl("VAM6.ATG1", ac[['Y']], fixed = T)){
      ac[['lab_y']] <- paste0("<b>Autophagosome formation</b><br><br>",
                      "<i>log BF (VAM6:ATG1)</i><br>")

    }else if(grepl("WT.ATG1", ac[['Y']], fixed = T)){
      ac[['lab_y']] <- paste0("<b>Overall autophagy</b><br><br>",
                      "<i>log BF (WT:ATG1)</i><br>")
    }else{
      ac[['lab_y']] <- paste0("<b>Autophagosome clearance</b><br><br>",
                      "<i>log BF (WT:VAM6)</i><br>")
    }

    return(ac)

  })

  #observe ({
  acomp_global_state_selected <- reactive ({
    #req(input$gene_id_acomp_global) ## NEW
    Positions <- c()
    for(i in input$gene_id_acomp_global){
      BF_response <- gw_autoph_competence_data[['bf_temporal']] |>
        dplyr::filter(primary_identifier == i)
      #If mutant in rec plate, only evaluate rec mutants
      if(any(grepl("Rec",BF_response$Plate))){
        BF_response <- BF_response[which(grepl("Rec",BF_response$Plate)),]
      }
      #Find representative response
      BF_response <- BF_response |>
        dplyr::group_by(TimeR) |>
        dplyr::mutate(d=abs(log_BFt_WT.ATG1-median(log_BFt_WT.ATG1))+
                        abs(log_BFt_WT.VAM6-median(log_BFt_WT.VAM6))+
                        abs(log_BFt_VAM6.ATG1-median(log_BFt_VAM6.ATG1))) |>
        dplyr::group_by(Plate, Position) |>
        dplyr::mutate(
          d = mean(d),
          NCells = mean(NCells))
      BF_response <-
        BF_response[which(BF_response$d==min(BF_response$d)),]
      BF_response <-
        BF_response[which(BF_response$NCells==max(BF_response$NCells)),]
      Positions <-
        c(Positions,paste(BF_response$Plate, BF_response$Position)[1])
    }

    mat_select <- gw_autoph_competence_data[['bf_overall']] |>
      subset(paste(Plate, Position) %in% Positions) |>
      as.data.frame()

    return(mat_select)
  })

  #observe ({
  acomp_global_state_library_adjustment <- reactive({

    #req(input$bf_library_adjustment)
    # Library adjustment for BF data
    req(acomp_global_state_vars())
    ## 1) We standardize the distributions of KO, DAmP and WT populations
    ##    to have equal mean and variance.
    ## 2) We cannot assume that the variance is the same for WT and
    ##    mutant distributions so we ensure that the sd is unchanged.

    ac_vars <- acomp_global_state_vars()
    ac_global_state[['X']] <- ac_vars[['X']]
    ac_global_state[['Y']] <- ac_vars[['Y']]
    ac_global_state[['lab_x']] <- ac_vars[['lab_x']]
    ac_global_state[['lab_y']] <- ac_vars[['lab_y']]

    X <- ac_global_state$X
    Y <- ac_global_state$Y
    #X <- ac[['X']]
    #Y <- ac[['Y']]
    #mat <- ac[['mat']]
    mat <- ac_global_state$mat
    if(input$bf_library_adjustment == T){
      mat_lib <- mat[is.na(mat$Plate_controls),] |>
        dplyr::mutate(
          mean_x = mean(!!rlang::sym(X), na.rm = T),
          mean_y = mean(!!rlang::sym(Y), na.rm = T),
          sd_x = sd(!!rlang::sym(X), na.rm = T),
          sd_y = sd(!!rlang::sym(Y), na.rm = T)) |>
        dplyr::group_by(Type) %>%
        dplyr::summarise(
          mean_type_x = mean(!!rlang::sym(X), na.rm = T),
          mean_type_y = mean(!!rlang::sym(Y), na.rm = T),
          sd_type_x = sd(!!rlang::sym(X), na.rm = T),
          sd_type_y = sd(!!rlang::sym(Y), na.rm = T),
          mean_x = mean(mean_x, na.rm = T),
          mean_y = mean(mean_y, na.rm = T),
          sd_x = mean(sd_x, na.rm = T),
          sd_y = mean(sd_y, na.rm = T))

      mat$Type[which(mat$Plate_controls == "+")] <-
        "KO"
      mat$Type[which(mat$Plate_controls == "+" & is.na(mat$ORF))] <-
        "WT"

      mat_wt <- mat[which(mat$Plate_controls == "+" & is.na(mat$ORF)),] |>
        dplyr::mutate(
          mean_x = mean(!!rlang::sym(X)),
          mean_y = mean(!!rlang::sym(Y)),
          sd_x = sd(!!rlang::sym(X)),
          sd_y = sd(!!rlang::sym(Y))) |>
        dplyr::group_by(Type) |>
        dplyr::summarise(
          mean_type_x = mean(!!rlang::sym(X), na.rm = T),
          mean_type_y = mean(!!rlang::sym(Y), na.rm = T),
          sd_type_x = sd(!!rlang::sym(X), na.rm = T),
          sd_type_y = sd(!!rlang::sym(Y), na.rm = T),
          mean_x = mean(mean_x, na.rm = T),
          mean_y = mean(mean_y, na.rm = T),
          sd_x = mean(sd_x, na.rm = T),
          sd_y = mean(sd_y, na.rm = T))
      #set global target distribution mean and SD for WT
      mat_wt[,6:9] <- mat_lib[1,6:9]
      #keep SD unchanged for the WT, i.e. only change mean
      mat_wt[1,4:5] <- mat_wt[1,8:9]
      mat_lib <- rbind(mat_lib, mat_wt)

      mat[,X] <-  mat[,X] +
        (mat_lib$mean_x-mat_lib$mean_type_x)[match(mat$Type,mat_lib$Type)]
      mat[,Y] <- mat[,Y] +
        (mat_lib$mean_y-mat_lib$mean_type_y)[match(mat$Type,mat_lib$Type)]
      mat[,X] <- mat[,X] *
        ((mat_lib$sd_x/mat_lib$sd_type_x)[match(mat$Type,mat_lib$Type)])
      mat[,Y] <- mat[,Y] *
        ((mat_lib$sd_y/mat_lib$sd_type_y)[match(mat$Type,mat_lib$Type)])

      #ac[['mat']] <- mat
      ac_global_state$mat <- mat
    }

    return(ac_global_state)

  })

  kinresp_global_XY_vars <- reactive({
    req(input$x_var_kin)
    req(input$y_var_kin)
    return(list('X' = input$x_var_kin,'Y' = input$y_var_kin))
  })

  kinresp_global_state_selected <- reactive({
    #req(input$gene_id_kinresp_global)
    Positions <- c()
    for(i in input$gene_id_kinresp_global){
      y_pred <- gw_autoph_response_data[['ds_curvefits']] |>
        dplyr::filter(primary_identifier == i)

      #If mutant in rec plate, only evaluate rec mutants
      if(any(grepl("Rec",y_pred$Plate))){
        y_pred <- y_pred[which(grepl("Rec",y_pred$Plate)),]
      }
      y_pred <- y_pred |>
        dplyr::group_by(Time) |>
        dplyr::mutate(d = abs(P1_30_fit-median(P1_30_fit))) |>
        dplyr::group_by(Plate, Position) |>
        dplyr::mutate(d = mean(d))

      y_pred <- y_pred[which(y_pred$d==min(y_pred$d)),]
      Positions <- c(
        Positions, paste(y_pred$Plate, y_pred$Position)[1])
    }
    return(Positions)
  })

  kinresp_global_normalized_vals <- reactive({
    if(input$use_perturbation_data == T){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  kinresp_global_state_mat_select <- reactive({
    req(kinresp_global_XY_vars())

    Value <- "Value"
    if(kinresp_global_normalized_vals() == T){
      Value <- "Perturbation"
    }
    X <- kinresp_global_XY_vars()[['X']]
    Y <- kinresp_global_XY_vars()[['Y']]
    Positions <-
      kinresp_global_state_selected()
    mat_select <- data.frame()
    if(length(Positions) > 0){

      mat_select <- as.data.frame(
        gw_autoph_response_data[['ds_parms_comb']] |>
          dplyr::filter(
            paste(.data$Plate, .data$Position) %in%
              Positions) |>
          dplyr::filter(
            .data$Parameter %in%
              c(X, Y)) |>
          dplyr::select(
            c("Plate","Position","ORF",
              "Gene","Reference_sets",
              "Plate_controls",
              "Parameter",
              rlang::sym(Value))) |>
          tidyr::pivot_wider(
            names_from = Parameter,
            values_from = Value)
      )

      mat_select$X <-
        mat_select[,X]
      mat_select$Y <-
        mat_select[,Y]
    }
    
    return(mat_select)

  })

  kinresp_global_state_mat <- reactive({
    req(kinresp_global_XY_vars())

    kinresp_data <- list()
    kinresp_data[['Value']] <- kinresp_global_state[['Value']]
    kinresp_data[['X']] <-
      kinresp_global_XY_vars()[['X']]
    kinresp_data[['Y']] <-
      kinresp_global_XY_vars()[['Y']]

    mattype <- "raw"
    if(kinresp_global_normalized_vals() == T){
      kinresp_data[['Value']] <- "Perturbation"
      mattype <- "norm"
    }

    kinresp_data[['mat']] <- as.data.frame(
      gw_autoph_response_data[['ds_parms']] |>
        dplyr::filter(
          .data$Parameter %in%
            c(kinresp_data[['X']],
              kinresp_data[['Y']])) |>
        dplyr::select(c("Plate","Position","ORF",
                        "Gene","primary_identifier",
                        "Reference_sets",
                        "Parameter",
                        rlang::sym(kinresp_data[['Value']]))) |>
        tidyr::pivot_wider(
          names_from = Parameter,
          values_from = kinresp_data[['Value']]))

    kinresp_data[['mat']]$X <-
      kinresp_data[['mat']][,kinresp_data[['X']]]
    kinresp_data[['mat']]$Y <-
      kinresp_data[['mat']][,kinresp_data[['Y']]]

    kinresp_data[['mat']] <- kinresp_data[['mat']] |>
      dplyr::left_join(
        gw_autoph_type_data |>
          dplyr::select(Gene, ORF, Type),
        by = c("Gene", "ORF"),
        relationship = "many-to-many"
      ) |>
      dplyr::distinct()

    return(kinresp_data$mat)

  })
}

# Run the application
shiny::shinyApp(ui = ui_original, server = server)
