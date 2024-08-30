### This is ui.R

library(shiny)
library(shinydashboard)
library(DT)
library(rhandsontable)
library(markdown)
library(rmarkdown)
library(openxlsx)
library(knitr)


source("standview.R")
source("func_and_tags.R")



shinyUI(
  fluidPage(
    # remove shiny "red" warning messages on GUI
    tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
             ),
    tags$style(HTML(".sidebar-menu li a { font-size: 18px; }")),   # increase font size in sidebar
  dashboardPage(
    dashboardHeader(title = "Stand Density Management Diagram", titleWidth = 300),
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/forests' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='images/BC_FOR_V_RGB_rev.png' width = '186'></a>",
                         "<br>"
                         )
                         ),
                       menuItem("Plot", tabName = "Plot", icon = icon("chart-line")),
                       menuItem("Export", tabName = "Export", icon = icon("file-image")),
                       menuItem("About", tabName = "About", icon = icon("circle-info")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://www.facebook.com/BCProvincialGovernment' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.youtube.com/@BCPublicService' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.instagram.com/governmentofbc' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"
                         )
                         )
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      fluidRow(
        tabItems(
          tabItem("Plot",
                  box(width = 3,
                    title = "Specification",
                    status = "warning",
                    solidHeader = TRUE,
                    selectInput(inputId = "type",
                                label = "Select Diagram",
                                choices = c("Reineke" = 1,
                                            "Gingrich" = 2),
                                selected = 1),
                    selectInput(inputId = "ineq",
                                label = "Select Species and Reference",
                                choices = c("User Defined" = 1,
                                            "Ponderosa pine:	Long and Shaw (2005)" = 2,
                                            "Ponderosa pine:	Jang et al. (2021)" = 3,
                                            "Ponderosa pine:	Edminster (1988)" = 4,
                                            "Ponderosa pine:	Cochran and Barrett (1992)" = 5,
                                            "California mixed-conifer:	Long and Shaw (2012)" = 6,
                                            "Douglas-fir:	Long et al. (1988)" = 7,
                                            "White fir:	Zhang et al. (2007)" = 8,
                                            "Lodgepole pine:	McCarter and Long (1986)" = 9,
                                            "Spruce/Fir (Weiskittel and Woodall 2023)" = 10,
                                            "Red Spruce (Weiskittel and Woodall 2023)" = 11,
                                            "Balsam Fir (Weiskittel and Woodall 2023)" = 12,
                                            "Redwood/Douglas-fir (Ritchie and Berrill 2022)" = 13,
                                            "Douglas-fir/Redwood (Ritchie and Berrill 2022)" = 14,
                                            "Loblolly pine (Williams 1994)" = 15,
                                            "NW mixed-conifer: Kimsey Jr. et al. (2019)" = 16),
                                selected = 3),
                    conditionalPanel(
                      condition = "input.ineq == 16",
                      selectInput(inputId = "IFCsp",
                                  label = "Select Species Model",
                                  choices = c("INW-DF" = 1,
                                              "INW-GF" = 2,
                                              "INW-LP" = 3,
                                              "INW-PP" = 4,
                                              "INW-WL" = 5,
                                              "PNW-DF Mix" = 6,
                                              "PNW-HEMFIR" = 7),
                                  selected = 6)
                      ),
                    selectInput(inputId = "use.metric",
                                label = "Select Unit Type",
                                choices = c("English" = FALSE, "Metric" = TRUE),
                                selected = FALSE),
                    conditionalPanel(
                      condition = "(input.type==1&!(input.ineq == 1|input.ineq == 4|input.ineq == 5|input.ineq == 8 |input.ineq == 16))",
                      selectInput(inputId = "invol",
                                  label = "Overlay the volume iso-lines?",
                                  choices = c("No" = FALSE, "Yes" = TRUE),
                                  selected = TRUE)),
                    conditionalPanel(
                      condition = "(input.ineq==1 | input.ineq==6 | input.ineq==10 | input.ineq==11 | input.ineq==12 | input.ineq==13 | input.ineq==14 | input.ineq == 16)",
                      numericInput("max.sdi", "Maximum SDI", NA),
                      uiOutput("max_sdi_input_txt")
                      ),
                    conditionalPanel(
                      condition = "input.ineq==1|input.ineq==6",
                      sliderInput("mgt.zone", "Management Zone",
                                  min = 0, max = 1,
                                  value = c(0.35, 0.60)
                                  )
                      ),
                    conditionalPanel(
                      condition = "input.ineq==16",
                      numericInput("des.sdi", "Desired %SDImax", NA),
                      numericInput("qmd.target", "Target QMD", NA)
                      ),
                    conditionalPanel(
                      condition = "input.ineq == 1",
                      helpText("Note: All information should be filled to render the diagram."),
                      numericInput("reineke.term", "Reineke's Slope", 1.605),
                      textInput("title", "Title", "User Defined")
                      ),
                    conditionalPanel(
                      condition = "((input.ineq == 1 | input.ineq == 16) & input.type == 1)",
                      selectInput(inputId = "sdi.lines",
                                  label = "Select SDI Lines for Display",
                                  choices =  c("50" = 50, "75" = 75, "100" = 100,
                                               "125" = 125, "150" = 150, "175" = 175,
                                               "200" = 200, "225" = 225, "250" = 250,
                                               "275" = 275, "300" = 300, "325" = 325,
                                               "350" = 350, "375" = 375, "400" = 400,
                                               "425" = 425, "450" = 450, "475" = 475,
                                               "500" = 500, "600" = 600, "700" = 700,
                                               "800" = 800, "900" = 900, "1000" = 1000,
                                               "1100" = 1100, "1200" = 1200, "1300" = 1300,
                                               "1400" = 1400, "1500" = 1500, "1600" = 1600
                                               ),
                                  multiple = TRUE)
                      ),
                    conditionalPanel(
                      condition = "input.ineq == 5",
                      numericInput("bsi", "Site Index", 90)
                      ),
                    hr(),
                    numericInput("plotsize", "Display Image Height (px)", 750),
                    ),
                  
                  box(width = 3, status = "success",
                      title = "Density Management Regime",
                      solidHeader = TRUE,
                      selectInput(inputId = "drord",
                                  label = "Point Entering Order",
                                  choices = c("Forward" = 1, "Backward" = 2),
                                  selected = 1),
                      hr(),
                      helpText("Enter data points for display."),
                      rHandsontableOutput("point_entry"),
                      hr(),
                      h4("Stand Summary Calculation"),
                      helpText("Predict stand attributes. Click the Calculate button to update and report."),
                      actionButton("calc", "Calculate")
                      ),
                  box(width = 6, status = "info",
                      uiOutput("dmdview")
                      ),
                  conditionalPanel(
                    condition = "input.calc",
                    box(width = 6,
                        status = "danger",
                        solidHeader = TRUE,
                        title ="Stand Summary Table",
                        DTOutput("Est_vol")
                        )
                      )
                  ),
          
          tabItem(tabName = "Export",
                  box(width = 4,
                      status = "info",
                      title ="Export Outputs",
                      solidHeader = TRUE,
                      helpText("Note: For download, please make sure that your SDMD plot AND the Stand Attribute Table were generated from the Plot panel."),
                      hr(),
                      h5("Export SDMD plot"),
                      radioButtons("IMGformat",
                                   "Image format",
                                   c("PDF", "PNG", "TIFF"),
                                   inline = TRUE),
                      downloadButton("SDMD_out", "Save SDMD"),
                      hr(),
                      h5("Export Stand Attribute Table"),
                      radioButtons("Tblformat",
                                   "File format",
                                   c("CSV", "XLSX"),
                                   inline = TRUE),
                      downloadButton("stable_out", "Save Table"),
                      hr(),
                      h5("Download Report"),
                      radioButtons("format",
                                   "Document format",
                                   c("PDF", "HTML"),
                                   inline = TRUE),
                      downloadButton("downloadReport","Create Report"),
                      hr()
                      ),
                  box(width = 6,
                      status = "success",
                      title ="Additional Notes for Report",
                      solidHeader = TRUE,
                      textAreaInput("comments",
                                    "Enter some comments for your reference. Please note that using unicode character might cause errors in creating report file.",
                                    "",
                                    width = "800px",
                                    height = "510px")
                      )
                  ),
          
          tabItem(tabName = "About",
                  column(6, offset = 0, style = 'position:relative; left:10%',
                         includeMarkdown("README.md")
                         )
                  )
          ),
        column(width = 12,
               style = "background-color:#003366; border-top:2px solid #fcba19;",
               tags$footer(class = "footer",
                           tags$div(class = "container",
                                    style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                    tags$ul(style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                            tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                            )
                                    )
                           )
               )
        )
    )
  )
  )
)
