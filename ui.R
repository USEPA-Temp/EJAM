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
      # 1. LOCATIONS TO ANALYZE (universe of interest) ####
      fluidRow(column(
        12, style = "overflow: hidden;", htmltools::h4("1. Universe of Interest")
      )),
      
      ## A) by Industry / NAICS ##########
      wellPanel(
        fluidRow(
          column(12,style = "overflow: hidden;",
                 ### i. pick list/ search Industry (selectNaics_in_Datasystem1) ####
                 column(12,
                        htmltools::h5(
                          "Select Industry",
                          htmltools::a(htmltools::img(id = "ibutton",src = "i.png",height = 15,width = 15),href = "ibutton_help.html#help_industry",target = "_blank"))
                        ),
                 column(5,
                        selectInput(
                          "selectIndustry2_by_selectInput",
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
                 ### ii. type in NAICS #####
                 column(5,
                        textInput(
                          "selectIndustry1_byNAICS",
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
                 
                 ### & limit to fac w NAICS in this EPA program #####
                 column(
                   12,
                   #radioButtons("selectFrom1", label = htmltools::h5("Match your NAICS code selection with:"),
                   #   c("Any EPA data system" = "any","Select data systems" = "some"), selected = NULL, inline = TRUE, width = NULL),
                   htmltools::h5(
                     "Limit to facilities where selected NAICS is found within these EPA lists: (all included by default):
                             (A facilty may have different NAICS in each list)",
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
                   
                   checkboxGroupInput(
                     "selectNaics_in_Datasystem1",
                     "",
                     c("TRIS" = "TRIS",
                       "RCRAINFO" = "RCRAINFO",
                       "AIRS/AFS" = "AIRS/AFS",
                       "E-GGRT" = "E-GGRT",
                       "NPDES" = "NPDES",
                       "RCRAINFO" = "RCRAINFO",
                       "RMP" = "RMP"
                     ),
                     inline = TRUE
                   ),
                   
                   ### & limit to fac in this EPA program #####
                   #radioButtons("selectFrom2", label = htmltools::h5("Include facilities with records in:"),
                   #  c("All EPA data systems" = "any", "Select data systems" = "some"), selected = NULL, inline = TRUE, width = NULL),
                   htmltools::h5(
                     "Limit to facilities on these EPA lists (all included by default):",
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
                     "selectNaics_and_Datasystem2",
                     "",
                     c("TRIS" = "TRIS",
                       "RCRAINFO" = "RCRAINFO",
                       "AIRS/AFS" = "AIRS/AFS",
                       "E-GGRT" = "E-GGRT",
                       "NPDES" = "NPDES",
                       "RCRAINFO" = "RCRAINFO",
                       "RMP" = "RMP"
                     ),
                     inline = TRUE
                   )
                 )
          )
        )),
      
      htmltools::h4("OR", align = "center"),
      
      fluidRow(
        column(12, wellPanel(
          ## B) by Facility IDs uploaded (as file_uploaded_FRS_IDs) ##########
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
          ## C) by points (lat lon) uploaded (file_uploaded_latlons) ##########
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
    
    wellPanel(# 2. DISTANCE (circular buffer radius)  ##########
              
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
      # 3. OVERALL SUMMARY or SITE-BY-SITE? ####### 
      # dissolve sites so person near 2+ sites double-counted, or not
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
          "Site-by-Site results" = "no",
          "Overall Summary (avoids double-counting any residents near 2+ sites) [*this may change]" = "yes"
        ),
        selected = NULL,
        inline = TRUE,
        width = NULL
      )
    ))),
    
    htmltools::br(),
    htmltools::br(),
    # DOWNLOAD RESULTS  ##########
    downloadButton('downloadData1', 'Download'),
    
    textOutput("inputWarning2"),
    htmltools::br(),
    htmltools::tags$head(
      htmltools::tags$style(
        "#inputWarning2{color: red; font-size: 14px; font-style: italic; }"
      )
    ),
    
    
    mainPanel( # for testing: see selections ##########
               verbatimTextOutput("selectInd2_for_testing"),
               verbatimTextOutput("selectInd1_for_testing"),
              verbatimTextOutput("selectScope1"),
              verbatimTextOutput("selectScope2"),
              verbatimTextOutput("file_uploaded_FRS_IDs_df"),
              verbatimTextOutput("file_uploaded_latlons_df"),
    )
  )
)
