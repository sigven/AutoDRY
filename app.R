
library(shiny)

## Load required packages
source('helpers.R')

## Create UI elements

#options(shiny.reactlog = TRUE)

## UI elements - Data Explorer (DE)
## - Menu
##  1) Response kinetics - single:  ("autodry_response_ui[['single']]")
##  2) Response kinetics - global:  ("autodry_response_ui[['global']]")
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
autodry_response_ui <- list()
for(e in c('single','global')){
  autodry_competence_ui[[e]] <- list()
  autodry_response_ui[[e]] <- list()
}

autodry_response_ui[['single']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,
    sidebar = bslib::sidebar(
      shiny::selectizeInput(
        "gene_id_arkin_single", 
        "Select gene/ORF mutant", 
        options =
          list(maxOptions = 5500),     # Set this to your max or desired number
        choices = NULL, 
        width = "100%")
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
          shiny::uiOutput("arkin_warning")
        )
      ),
      htmltools::div(
        style = "padding-top: 1rem;",
        shiny::includeHTML("data/section_content/citation_footnote.md")
      )
    )
  )

autodry_response_ui[['global']] <-
  bslib::layout_sidebar(
    fillable = TRUE,
    fill = TRUE,
    sidebar = bslib::sidebar(
      list(
        shiny::selectizeInput(
          inputId = "gene_id_arkin_global",
          label = "Highlight gene/ORF mutants (max 10)",
          choices = NULL,
          multiple = TRUE,
          selected = "AAC1 / YMR056C",
          options = list(
            maxOptions = 5500,     # Set this to your max or desired number
            minItems = 1,
            maxItems = 10)),
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
          "contour", "Show contour plots", value = F))
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
          shiny::plotOutput("arkin_global")
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
      shiny::selectizeInput(
      "gene_id_acomp_single", 
      "Select gene/ORF mutant", 
      options =
        list(maxOptions = 5500),     # Set this to your max or desired number
      choices = NULL, width = "100%")
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
        shiny::selectizeInput(
          inputId = "gene_id_acomp_global",
          label = "Highlight gene/ORF mutants (max 10)",
          selected = "AAC1 / YMR056C",
          choices = NULL,
          
          multiple = TRUE,
          options = list(
            maxOptions = 5500,     # Set this to your max or desired number
            minItems = 1,
            maxItems = 10)),
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
      )
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



paper_info <- paste0(
  "<h5><b><p style=text-align:justify;'>",
  "<a href='https://www.biorxiv.org/content/10.1101/2024.04.06.588104v2 ",
  "target='_blank'>Time-resolved functional genomics using deep learning ",
  "reveals a global hierarchical control of autophagy (bioRxiv, 2025)</a></p></b></h5>",
  "<h6><p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/authors.md"),
  "</p></h6>",
  "<p style='text-align:justify;'><h6>Correspondence to <i>nathac@uio.no </i>",
  "or <i>j.m.enserink@ibv.uio.no</i></h6></p>")

synopsis <- paste0(
  "<h5><b><p style='color:$primary;text-align:justify;'>",
  htmltools::includeText("data/section_content/synopsis_I.md"),
  "</p></b></h5>",
  "<h6><p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/synopsis_II.md"),
  "</p></h6>")

what_is_autodry <- paste0(
  "<h5><b><p style='color:$primary;text-align:justify;'>",
  htmltools::includeText("data/section_content/what_is_autodry_I.md"),
  "</p></b></h5>",
  "<h6><p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/what_is_autodry_II.md"),
  "</p></h6>")

about_study_text <- paste0(
  "<p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/about_the_study_I.md"),
  "</p>",
  "AutoDRY provides two key outputs that provide different ",
  "perspectives on autophagy regulation:<br>",
  "<p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/about_the_study_II.md"),
  "</p>")

disclaimer_text <- paste0(
  "<p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/disclaimer.md"),
  "</p>")

documentation_text <- paste0(
  "<p style='text-align:justify;'>",
  htmltools::includeText("data/section_content/documentation.md"),
  "</p>")


autodry_footer <-
  bslib::card_footer(
    shiny::markdown(
      paste0(
        "<br>",
        "<div align='center'>",
        "<a href='https://www.uio.no' target='_blank'><img src='uio.png' alt='uio' style='width:20%;height:90%;'></a>",
        "<a href='https://ous-research.no/institute' target='_blank'><img src='ous.png' alt='ous' style='width:20%'></a>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
        "<a href='https://www.med.uio.no/cancell/english/' target='_blank'><img src='cancell.png' style='width:4%;height:48%;'></a>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
        "<a href='https://arctic-autophagy.no/' target='_blank'><img src='arctic.png' style='width:10%;height:45%'></a>",
        "</div>")
    )
  )

about_page <-
  bslib::page_fillable(
    tags$style(HTML(".card {border-radius: 0.9rem;}")),
    tags$style(HTML(".card2 {border-radius: 0.1rem;}")),
    bslib::card(
      class = "card2",
      full_screen = F,
      fillable = F,
      fill = F,
      bslib::card_body(
        #min_height = 300,
        bslib::layout_column_wrap(
          width = NULL,
          fill = F,
          style = bslib::css(
            grid_template_columns = "22fr 1fr 22fr 1fr 22fr"),
          bslib::card(
            bslib::card_header(class = "bg-dark","  Background"),
            shiny::markdown(synopsis)),
          shiny::markdown(""),
          bslib::card(
            bslib::card_header(class = "bg-dark","  What is AutoDRY?"),
            shiny::markdown(what_is_autodry)),
          shiny::markdown(""),
          bslib::card(
            bslib::card_header(class = "bg-dark","  Citation"),
            shiny::markdown(paper_info)
          )
        ),
        shiny::markdown(
          paste0(
            "<br><br><div align='center'>",
            "<img src='MNS_Figure_1A_v2.png' alt='Autophagy_Dynamics_Study_Overview'",
            " width='90%' height='100%' align='center'/></div><br>"
          )
        ),
        bslib::layout_column_wrap(
          width = NULL,
          fill = F,
          bslib::card(
            bslib::card_header(class = "bg-dark","  About the study"),
            shiny::markdown(about_study_text)
          )
        )
      ),
      autodry_footer
    )
  )


docs_page <- 
  bslib::page_fillable(
    tags$style(HTML(".card {border-radius: 0.9rem;}")),
    tags$style(HTML(".card2 {border-radius: 0rem}")),
    bslib::card(
      class = "card2",
      full_screen = F,
      fillable = F,
      fill = F,
      bslib::card_body(
        bslib::layout_column_wrap(
          width = NULL,
          fill = TRUE,
          bslib::card(
            bslib::card_header(
              class = "bg-dark","  Documentation"),
            bslib::card_body(
              shiny::markdown(documentation_text)
            )
          )
        )
      ),
      autodry_footer
    )
  )

disclaimer_page <-
  bslib::page_fillable(
    tags$style(HTML(".card {border-radius: 0.9rem;}")),
    tags$style(HTML(".card2 {border-radius: 0rem}")),
    bslib::card(
      class = "card2",
      full_screen = F,
      fillable = F,
      fill = F,
      bslib::card_body(
        bslib::layout_column_wrap(
          width = NULL,
          fill = TRUE,
          bslib::card(
            bslib::card_header(
              class = "bg-dark","  DISCLAIMER"),
            bslib::card_body(
              shiny::markdown(disclaimer_text)
            )
          )
        )
      ),
      autodry_footer
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
            class = "d-flex align-items-center justify-content-center",
            style = "height: 100%; padding: 2rem;",
            div(
              class = "text-center",
              style = "max-width: 600px; width: 100%;",
              # 1. Logo or Image
              htmltools::tags$img(
                src = "Autodry_logo.png", 
                height = "150px", 
                style = "margin-bottom: 30px;"),
              
              # 2. 2x2 Radio Grid
              div(
                class = "mb-3",
                fluidRow(
                  column(
                    6,
                    radioButtons(
                      "analysis_view", "",
                      choices = c(
                        "Kinetic response (single)" = "arkin_single", 
                        "Kinetic response (global)" = "arkin_global"),
                      selected = "arkin_single",
                      inline = TRUE
                    )
                  ),
                  column(
                    6,
                    radioButtons(
                      "analysis_view", "",
                      choices = c(
                        "Autophagy competence (single)" = "acomp_single", 
                        "Autophagy competence (global)" = "acomp_global"),
                      selected = "arkin_single",
                      inline = TRUE
                    )
                  )
                )
              ),
              
              shiny::textInput(
                "gene_search_input", 
                "Search for yeast gene/ORF mutant", 
                "", width = "100%"),
              shiny::uiOutput("gene_mutant_suggestions"),
              # 3. Selectize Input
              #shiny::selectizeInput(
              #  "main_gene_search", 
              #  "Query a yeast gene/ORF mutant to explore different views of autophagy dynamics", 
              #  choices = NULL, 
              #  options = list(
              #    maxOptions = 5500     # Set this to your max or desired number
              #  ),
              #  width = "100%"),
              
              # 4. Submit Button
              shiny::actionButton(
                "submit_btn", 
                "Submit", 
                class = "btn-primary mt-3"),
              htmltools::div(style = "margin-top: 80px;"),
              htmltools::div(
                 shiny::HTML("Citation: Chica et al., bioRxiv (2025). <b>Time-resolved functional genomics using deep learning reveals a global hierarchical control of autophagy</b>. <a name='citation' href='https://www.biorxiv.org/content/10.1101/2024.04.06.588104v2' target='_blank'>doi: 10.1101/2024.04.06.588104</a>.")),
              htmltools::div(style = "margin-top: 30px;")
            )
          )
        ),
        autodry_footer
      )
  ))

close_dn_js0 <- 
  shiny::tags$script("
        Shiny.addCustomMessageHandler(
          'toggleDropdown',
          function toggleDropdown(msg) {
            $('.dropdown-menu').removeClass('show')
          });
        "
  )

detect_browser_js <- 
  shiny::tags$script(
  shiny::HTML("
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
      close_dn_js0,
      detect_browser_js,
      #shiny::includeHTML("google_analytics.html"),
      htmltools::tags$script(
        shiny::HTML(
        "Shiny.addCustomMessageHandler('getUserAgent', function(message) {
          Shiny.setInputValue('client_browser', navigator.userAgent);
        });")),
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
                       value = "arkin_single_view",
                       autodry_response_ui[['single']]),
      bslib::nav_panel(title = "Response kinetics - global", 
                       fill = TRUE,
                       value = "arkin_global_view",
                       autodry_response_ui[['global']]),
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

  observe({
    session$sendCustomMessage("getBrowserInfo", list())
  })
  
  filtered_genes <- shiny::reactive({
    req(nchar(input$gene_search_input) >= 2)
    matches <- grep(
      input$gene_search_input, 
      main_gene_ids$orf_gene_id, 
      ignore.case = TRUE, 
      value = TRUE)
    head(matches, 50)  # limit suggestions for performance
  })
  
  # Render selectInput dynamically once user types enough
  output$gene_mutant_suggestions <- shiny::renderUI({
    if (nchar(input$gene_search_input) < 2) return(NULL)
    
    choices <- filtered_genes()
    if (length(choices) == 0) return("No matches found.")
    
    shiny::selectInput(
      "main_gene_search", 
      "Select a match in the list to explore different views of autophagy dynamics:", multiple = TRUE,
      choices = choices, width = "100%", selectize = F)
  })
  
  
  # output$conditional_main_select <- shiny::renderUI({
  #   req(input$client_browser)
  #   #cat(input$client_browser,"\n")
  #   if (input$client_browser == "Safari") {
  #     shiny::textInput(
  #       "gene_search_input", 
  #       "Search for yeast gene/ORF mutant to explore different views of autophagy dynamics:", 
  #       "")
  #     shiny::uiOutput(gene_mutant_suggestions)
  #   } else {
  #     shiny::selectizeInput(
  #       "main_gene_search", 
  #       "Query a yeast gene/ORF mutant to explore different views of autophagy dynamics", 
  #       choices = NULL, 
  #       options = list(
  #         maxOptions = 5500     # Set this to your max or desired number
  #       ),
  #       width = "100%")
  #   }
  # })
  
  shiny::observeEvent(input$submit_btn, {
    selected_gene(input$main_gene_search)
    
    # Switch to appropriate nav_panel based on selected analysis view
    target_panel <- switch(
      input$analysis_view,
      "arkin_single" = "arkin_single_view",
      "arkin_global"  = "arkin_global_view",
      "acomp_single" = "acomp_single_view",
      "acomp_global" = "acomp_global_view")
    
    # Change page programmatically using bslib’s update_navs
    shiny::updateNavbarPage(
      inputId = "main_nav", 
      selected = target_panel)
    
    # Update the corresponding selectizeInput in the selected panel
    shiny::updateSelectizeInput(
      session, 
      paste0("gene_id_", input$analysis_view),
      selected = input$main_gene_search)
    
  })
  
  shiny::observe({
    if (input$main_nav %in% "arkin_single_view" || 
        input$main_nav %in% "arkin_global_view" || 
        input$main_nav %in% "acomp_single_view" || 
        input$main_nav %in% "acomp_global_view") {
      session$sendCustomMessage(
        type = "toggleDropdown",
        message = list(msg = "hide dropdown"))
    }
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
  
  ac_global_state <- ac_global_init
  
  Value <- "Value"
  arkin_global_init <- list()
  arkin_global_init[['Value']] <- Value
  arkin_global_init[['X']] = "T50 +N"
  arkin_global_init[['Y']] = "T50 -N"
  arkin_global_init[['Positions']] <- c()
  arkin_global_init[['mat']] <- as.data.frame(
      gw_autoph_response_data[['ds_parms']] |> 
        dplyr::filter(
          .data$Parameter %in% 
            c(arkin_global_init[['X']],
              arkin_global_init[['Y']])) |> 
        dplyr::select(c("Plate","Position","ORF",
                        "Gene","primary_identifier",
                        "Reference_sets",
                        "Parameter",
                        rlang::sym(Value))) |> 
        tidyr::pivot_wider(
          names_from = Parameter, 
          values_from = Value))
  arkin_global_init[['mat']]$X <- 
    arkin_global_init[['mat']][, arkin_global_init[['X']]]
  arkin_global_init[['mat']]$Y <- 
    arkin_global_init[['mat']][, arkin_global_init[['Y']]]
  arkin_global_init[['mat_select']] <- data.frame()
  
  arkin_global_state <- arkin_global_init
  
  # observeEvent(input$client_browser, {
  #   if (input$client_browser != "Safari") {
  #     shiny::updateSelectizeInput(
  #       session, "main_gene_search",
  #       choices = main_gene_ids$orf_gene_id,
  #       server = TRUE)
  #   }
  # })
  
  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_arkin_single",
        choices = gw_autoph_response_data$gene_info_kinetic$orf_gene_id,
        server = TRUE)
    }
  })
    
  shiny::observeEvent(input$client_browser, {
    if (input$client_browser != "Safari") {
      shiny::updateSelectizeInput(
        session, "gene_id_arkin_global",
        selected = "AAC1 / YMR056C",
        choices = gw_autoph_response_data$gene_info_kinetic_multi$orf_gene_id,
        server = TRUE)
    }
  })
      
  # Dynamically show plot or warning
  output$arkin_warning <- shiny::renderUI({
    if (is.null(input$gene_id_arkin_single) || input$gene_id_arkin_single == "") {
      div(
        style = "color: red; font-weight: bold;",
        "⚠️ Please select a gene/ORF mutant to visualize the autophagy response kinetics."
      )
    } else {
      shiny::plotOutput("arkin", height = "100%")
    }
  })
  
  output$arkin <- shiny::renderPlot({
    req(input$gene_id_arkin_single)
    plot_response_kinetics(
      response_data = gw_autoph_response_data$per_ko[[input$gene_id_arkin_single]])
  })

  output$arkin_global <- shiny::renderPlot({
    req(arkin_global_state_mat())
    req(arkin_global_state_mat_select())
    req(arkin_global_XY_vars())
    
    plot_response_kinetics_global(
      mat = arkin_global_state_mat(),
      mat_select = arkin_global_state_mat_select(),
      X = arkin_global_XY_vars()[['X']],
      Y = arkin_global_XY_vars()[['Y']],
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
      gw_autoph_response_data$per_ko[[input$gene_id_arkin_single]]
    library_type <- ""
    if("slibrary" %in% names(response_data)){
      library_type <- response_data$slibrary
    }
    paste("Autophagy response kinetics -",
          input$gene_id_arkin_single, library_type)
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
      primary_id = input$gene_id_arkin_single,
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
  
  acomp_global_state_selected <- reactive ({
    #req(input$gene_id_bf_multiple)
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
  
  acomp_global_state_library_adjustment <- reactive({

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
      
      ac_global_state$mat <- mat
    }

    return(ac_global_state)

  })
  
  arkin_global_XY_vars <- reactive({
    req(input$x_var_kin)
    req(input$y_var_kin)
    return(list('X' = input$x_var_kin,'Y' = input$y_var_kin))
  })
  
  arkin_global_state_selected <- reactive({
    req(input$gene_id_arkin_global)
    Positions <- c()
    for(i in input$gene_id_arkin_global){
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
  
  arkin_global_normalized_vals <- reactive({
    if(input$use_perturbation_data == T){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
  
  arkin_global_state_mat_select <- reactive({
    #req(arkin_global_XY_vars())
    req(arkin_global_state_selected())
    #req(arkin_global_normalized_vals())
    
    Value <- "Value"
    if(arkin_global_normalized_vals() == T){
      Value <- "Perturbation"
    }
    X <- arkin_global_XY_vars()[['X']]
    Y <- arkin_global_XY_vars()[['Y']]
    Positions <- 
      arkin_global_state_selected()
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
  
  arkin_global_state_mat <- reactive({
    req(arkin_global_XY_vars())

    arkin_data <- list()
    arkin_data[['Value']] <- arkin_global_state[['Value']]
    arkin_data[['X']] <-
      arkin_global_XY_vars()[['X']]
    arkin_data[['Y']] <-
      arkin_global_XY_vars()[['Y']]
    
    mattype <- "raw"
    if(arkin_global_normalized_vals() == T){
      arkin_data[['Value']] <- "Perturbation"
      mattype <- "norm"
    }
    
    # arkin_data[['mat']] <- 
    #   gw_arkin_mats[[mattype]][gw_arkin_mats[[mattype]]$key == 
    #                                          paste(arkin_data[['X']],
    #                                                arkin_data[['Y']], sep="_"),] |>
    #   tidyr::pivot_wider(
    #     names_from = Parameter, 
    #     values_from = arkin_data$Value)
    
    arkin_data[['mat']] <- as.data.frame(
      gw_autoph_response_data[['ds_parms']] |>
        dplyr::filter(
          .data$Parameter %in%
            c(arkin_data[['X']],
              arkin_data[['Y']])) |>
        dplyr::select(c("Plate","Position","ORF",
                        "Gene","primary_identifier",
                        "Reference_sets",
                        "Parameter",
                        rlang::sym(arkin_data[['Value']]))) |>
        tidyr::pivot_wider(
          names_from = Parameter,
          values_from = arkin_data[['Value']]))

    arkin_data[['mat']]$X <- 
      arkin_data[['mat']][,arkin_data[['X']]]
    arkin_data[['mat']]$Y <- 
      arkin_data[['mat']][,arkin_data[['Y']]]
    
    arkin_data[['mat']] <- arkin_data[['mat']] |>
      dplyr::left_join(
        gw_autoph_type_data |>
          dplyr::select(Gene, ORF, Type),
        by = c("Gene", "ORF"),
        relationship = "many-to-many"
      ) |>
      dplyr::distinct()
    
    return(arkin_data$mat)
    
    #return(arkin_data)
  })
}

# Run the application
shiny::shinyApp(ui = ui_original, server = server)
