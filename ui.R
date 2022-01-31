# ui.R     The user-interface for this Shiny app

shinyUI(
  fluidPage(
    htmltools::includeCSS("styles.css"),
    headerPanel(
      title = htmltools::a("EJ Proximity Tool for Multiple Facilities",href = "ibutton_help.html",target = "_blank"),
      windowTitle = "EJ Proximity Tool for Multiple Facilities"
    ),
    #titlePanel(a("EJSCREEN Batch Processor",href="ibutton_help.html", target="_blank")),

    wellPanel(
      # Universe of interest #
      fluidRow(column(
        12, style = "overflow: hidden;", htmltools::h4("1. Universe of Interest")
      )),

      # Select Industry by NAICS ##########
      wellPanel(
                fluidRow(
                  column(12,style = "overflow: hidden;",
                         column(12,
                                htmltools::h5("Select Industry",htmltools::a(htmltools::img(id = "ibutton",src = "i.png",height = 15,width = 15),href = "ibutton_help.html#help_industry",target = "_blank"))),
                         column(5,
                                selectInput(
                                  "selectIndustry2",
                                  label = htmltools::h6("Select industry of interest"),
                                  choices = s_options,
                                  selected = s_dropdown_naics,
                                  width = 400,
                                  multiple = TRUE
                                )
                         ),
                         column(2,
                                htmltools::br(),htmltools::br(),htmltools::h4("OR", align = "center")
                         ),
                         column(5,
                                textInput(
                                  "selectIndustry1",
                                  label = htmltools::h6(
                                    "Enter NAICS codes of interest - ",
                                    htmltools::a("Look up NAICS", href ="https://www.census.gov/naics"),
                                    htmltools::a(htmltools::img(id = "ibutton",src = "i.png",height = 15,width = 15),href = "ibutton_help.html#help_naicslist",target = "_blank")
                                  ),
                                  value = "",
                                  width = 400,
                                  placeholder = NULL
                                )
                         ),
                         htmltools::br(),
                         textOutput("inputWarning"),
                         htmltools::br(),
                         htmltools::tags$head(htmltools::tags$style("#inputWarning{color: red;font-size: 14px;font-style: italic;}")),

                         column(
                           12,
                           #radioButtons("selectFrom1", label = htmltools::h5("Match your NAICS code selection with:"),
                           #               c("Any EPA data system" = "any","Select data systems" = "some"), selected = NULL, inline = TRUE, width = NULL),
                           htmltools::h5(
                             "2. Match your NAICS code selection with (all included by default):",
                             htmltools::a(
                               htmltools::img(
                                 id = "ibutton",
                                 src = "i.png",
                                 height = 15,
                                 width = 15
                               ),
                               href = "ibutton_help.html#help_match",
                               target = "_blank"
                             )
                           ),
                          ## selectNaicsDS1 ####
                           checkboxGroupInput(
                             "selectNaicsDS1",
                             "",
                             c(
                               "TRIS" = "TRIS",
                               "RCRAINFO" = "RCRAINFO",
                               "AIRS/AFS" = "AIRS/AFS",
                               "E-GGRT" =
                                 "E-GGRT",
                               "NPDES" =
                                 "NPDES",
                               "RCRAINFO" =
                                 "RCRAINFO",
                               "RMP" =
                                 "RMP"
                             ),
                             inline = TRUE
                           ),

                           #radioButtons("selectFrom2", label = htmltools::h5("Include facilities with records in:"),
                           #               c("All EPA data systems" = "any", "Select data systems" = "some"), selected = NULL, inline = TRUE, width = NULL),
                           htmltools::h5(
                             "3. Return facilities with records in (all included by default):",
                             htmltools::a(
                               htmltools::img(
                                 id = "ibutton",
                                 src = "i.png",
                                 height = 15,
                                 width = 15
                               ),
                               href = "ibutton_help.html#help_include",
                               target = "_blank"
                             )
                           ),
                           checkboxGroupInput(
                             "selectNaicsDS2",
                             "",
                             c(
                               "TRIS" = "TRIS",
                               "RCRAINFO" = "RCRAINFO",
                               "AIRS/AFS" = "AIRS/AFS",
                               "E-GGRT" =
                                 "E-GGRT",
                               "NPDES" =
                                 "NPDES",
                               "RCRAINFO" =
                                 "RCRAINFO",
                               "RMP" =
                                 "RMP"
                             ),
                             inline = TRUE
                           )
                         )
                  )
                )),

      htmltools::h4("OR", align = "center"),

      fluidRow(
        column(12, wellPanel(
          ########## ########## ########## upload FRS IDs as file_uploaded_FRS_IDs ##########
          fileInput(
            'file_uploaded_FRS_IDs',
            label = htmltools::h5(
              "Upload list of FRS identifiers",
              htmltools::a(
                htmltools::img(
                  id = "ibutton",
                  src = "i.png",
                  height = 15,
                  width = 15
                ),
                href = "ibutton_help.html#help_frs",
                target = "_blank"
              )
            )
          )
        )),
        htmltools::h4("OR", align = "center"),

        column(12, wellPanel(
          # upload lat lon list as file_uploaded_latlons ##########
          fileInput(
            "file_uploaded_latlons",
            label = htmltools::h5(
              "Upload list of locations with coordinates",
              htmltools::a(
                htmltools::img(
                  id = "ibutton",
                  src = "i.png",
                  height = 15,
                  width = 15
                ),
                href = "ibutton_help.html#help_location",
                target = "_blank"
              )
            )
          )
        ))
      )
    ),

    wellPanel(# Distance (radius) ##########

              fluidRow(column(
                12, htmltools::h4(
                  "2. Distance",
                  htmltools::a(
                    htmltools::img(
                      id = "ibutton",
                      src = "i.png",
                      height = 15,
                      width = 15
                    ),
                    href = "ibutton_help.html#help_distance",
                    target = "_blank"
                  )
                )
              )),
              fluidRow(
                column(
                  6,
                  style = "overflow: hidden;",
                  numericInput(
                    'cutoffRadius',
                    label = htmltools::h5("Define Buffer Distance"),
                    value = 3.0,
                    min = 0.0,
                    max = NA,
                    step = NA,
                    width = NULL
                  )
                ),
                column(
                  6,
                  style = "overflow: hidden;",

                  radioButtons(
                    "expandRadius",
                    label = htmltools::h5(
                      "Expand distance for facilities with no census block centroid within selected buffer distance."
                    ),
                    c("Yes" = "yes",
                      "No" = "no"),
                    selected = "no",
                    inline = TRUE,
                    width = NULL
                  )
                )
              )),

    fluidRow(column(12, wellPanel(
      # Person near 2+ sites double-counted or not ##########
      radioButtons(
        "uniqueOutput",
        label = htmltools::h4(
          "3. Output",
          htmltools::a(
            htmltools::img(
              id = "ibutton",
              src = "i.png",
              height = 15,
              width = 15
            ),
            href = "ibutton_help.html#help_output",
            target = "_blank"
          )
        ),
        c(
          "Individual facility statistics" = "no",
          "Output for aggregating population statistics [*this may change]" = "yes"
        ),
        selected = NULL,
        inline = TRUE,
        width = NULL
      )
    ))),

    htmltools::br(),
    htmltools::br(),
    # Download Results ##########
    downloadButton('downloadData1', 'Download'),

    textOutput("inputWarning2"),
    htmltools::br(),
    htmltools::tags$head(
      htmltools::tags$style(
        "#inputWarning2{color: red; font-size: 14px; font-style: italic; }"
      )
    ),


    mainPanel(# mainPanel ??? probably to view variables while testing ##########
              #verbatimTextOutput("selectInd1"),
              #verbatimTextOutput("selectInd2"),
              #verbatimTextOutput("selectScope1"),
              #verbatimTextOutput("selectScope2"),
              #verbatimTextOutput("file_uploaded_FRS_IDs_df"),
              #verbatimTextOutput("file_uploaded_latlons_df"),)
    )
  )
)
