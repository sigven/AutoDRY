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
