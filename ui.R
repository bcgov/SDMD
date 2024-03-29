### This is ui.R

library(shiny)
library(DT)


navbarPage("Stand Density Management Diagram",id = "SDMD",
           theme = "bcgov.css",
           

  # Give the title page
 
  

 tabPanel("Plot", 
    
    
    fluidRow(
      column(3, wellPanel(
        selectInput(inputId="type",label="Select Diagram",
                    choices = c("Reineke" =1, 
                                "Gingrich" =2),
                    selected = 1),
        selectInput(inputId="ineq",label="Select species and reference",
             choices = c("User Defined" =1, 
                         "ponderosa pine:	Long and Shaw (2005)" =2,
                         "ponderosa pine:	Jang et al. (2021)" = 3,
                         "ponderosa pine:	Edminster (1988)" = 4,
                         "ponderosa pine:	Cochran and Barrett (1992)" = 5,
                         "California mixed-conifer:	Long and Shaw (2012)" =6,
                         "Douglas-fir:	Long et al. (1988)"=7,
                         "White fir:	Zhang et al. (2007)"=8,
                         "Lodgepole pine:	McCarter and Long (1986)"=9),
              selected = 3),
    
        selectInput(inputId="use.metric",label="Select type of unit",
              choices = c("English"=FALSE,"Metric"=TRUE),
              selected = FALSE),

      
        conditionalPanel(
          condition = "(input.type==1&!(input.ineq == 1|input.ineq == 4|input.ineq == 5|input.ineq == 8))",
          selectInput(inputId="invol",label="Overlay the volume iso-lines?",
              choices = c("No"=FALSE,"Yes"=TRUE),
              selected = FALSE)),

        conditionalPanel(
          condition = "input.ineq==6",
          numericInput("max.sdi6", "Maximum SDI", NA),
          helpText("Provide maximum SDI for Rendering"),
          sliderInput("mgt.zone6", "Management Zone",
                    min = 0, max = 1,
                    value = c(0.35,0.60))
          ),
               
       conditionalPanel(
          condition = "input.ineq==1",
          helpText("Note: All information should be filled to render the diagram."),
          numericInput("max.sdi1", "Maximum SDI", NA),
          textInput("title1","Title","User Defined"),
          sliderInput("mgt.zone1", "Management Zone",
                      min = 0, max = 1,
                      value = c(0.35,0.60)),
          numericInput("reineke.term", "Reineke's Slope", 1.605)
          ),
      
        conditionalPanel(
          condition = "(input.ineq==1&input.type==1)",
          selectInput(inputId="sdi.lines",label="Select SDI lines",
                      choices =  c("50" =50, "75" = 75, "100" = 100, "125" = 125,
                                   "150" = 150, "175" = 175, "200" = 200, "225" = 225,
                                   "250" = 250, "275" = 275, "300" = 300, "325" = 325, 
                                   "350" = 350, "375" = 375, "400" = 400, "425" = 425, 
                                   "450" = 450, "475" = 475, "500" = 500, "600" = 600,
                                   "700" = 700, "800" = 800, "900" = 900, "1000"= 1000,
                                   "1100" = 1100,"1200" = 1200,"1300" = 1300,"1400" = 1400,
                                   "1500" = 1500, "1600" = 1600
                      ),
                      multiple = TRUE)  
          
        ),
        
      conditionalPanel(
          condition = "input.ineq ==5",
          numericInput("bsi", "Site index", 90))   
      
    

      )),

        
  column(3, wellPanel(
      h3("Density Management Regime"),
      selectInput(inputId="drord",label="Point Entering Order",
                  choices = c("Forward" =1, 
                              "Backward" =2),
                  selected = 1)),
      conditionalPanel(
        condition = "input.type == 1",
        helpText("TPA/TPH: trees per acre/ha, QMD: inch/cm (English/metric)")),
      conditionalPanel(
        condition = "input.type == 2",
        helpText("TPA/TPH: trees per acre/ha, Basal Area: ft2 per acre/m2 per ha (English/metric)")),

      conditionalPanel(
          condition = "input.type == 1",
          splitLayout(
            numericInput("tpa1", "TPA/TPH", NULL),
            numericInput("qmd1", "QMD", NULL)),
          splitLayout(
            numericInput("tpa2", "TPA/TPH", NULL),
            numericInput("qmd2", "QMD", NULL)),
          splitLayout(
            numericInput("tpa3", "TPA/TPH", NULL),
            numericInput("qmd3", "QMD", NULL)),
          splitLayout(
            numericInput("tpa4", "TPA/TPH", NULL),
            numericInput("qmd4", "QMD", NULL)),
          splitLayout(
            numericInput("tpa5", "TPA/TPH", NULL),
            numericInput("qmd5", "QMD", NULL)),
          splitLayout(
            numericInput("tpa6", "TPA/TPH", NULL),
            numericInput("qmd6", "QMD", NULL)),
          splitLayout(
            numericInput("tpa7", "TPA/TPH", NULL),
            numericInput("qmd7", "QMD", NULL)),
          splitLayout(
            numericInput("tpa8", "TPA/TPH", NULL),
            numericInput("qmd8", "QMD", NULL))),
        
         conditionalPanel(
           condition = "input.type == 2",
           splitLayout(
             numericInput("tpa1_g", "TPA/TPH", NULL),
             numericInput("ba1", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa2_g", "TPA/TPH", NULL),
             numericInput("ba2", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa3_g", "TPA/TPH", NULL),
             numericInput("ba3", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa4_g", "TPA/TPH", NULL),
             numericInput("ba4", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa5_g", "TPA/TPH", NULL),
             numericInput("ba5", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa6_g", "TPA/TPH", NULL),
             numericInput("ba6", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa7_g", "TPA/TPH", NULL),
             numericInput("ba7", "Basal Area", NULL)),
           splitLayout(
             numericInput("tpa8_g", "TPA/TPH", NULL),
             numericInput("ba8", "Basal Area", NULL))),  

  
        conditionalPanel(
          condition = "input.ineq == 3",
          helpText("Predict stand attributes. Click the Calculate button to update."),
          actionButton("calc", "Calculate")),
        
        conditionalPanel(
          condition = "!(input.ineq == 3)",
          helpText("Stand attribute prediction is not available."))
    ),
  
  
  column(6, 
         uiOutput("dmdview")
    )


  
  ),
  
  fluidRow(
      column(6, conditionalPanel(
      condition = "(input.ineq == 3 & input.tpa1>0)",
      h4("Stand Summary"),
      DTOutput("Est_vol")
      
  )
  
  ))),
 
 tabPanel("Export",
    fluidRow(
            
        column(6, mainPanel(      
            h4("Export Outputs"),
            helpText("Note: For download, please make sure that your SDMD Plot and/or stand attribute table were created from the Plot panel."),
            hr(),
            h5("Export SDMD plot"),
            radioButtons("IMGformat", "Image format", c("PDF", "PNG","TIFF"),
                 inline = TRUE),
            downloadButton("SDMD_out", "Save SDMD"),
            hr(),
            
            conditionalPanel(
                condition = "!(input.ineq == 1|input.ineq == 4|input.ineq == 5|input.ineq == 8)",
                
            
            h5("Export Stand Attribute Table"),
            radioButtons("Tblformat", "File format", c("CSV", "XLSX"),
                         inline = TRUE),
            downloadButton("stable_out", "Save Table"),
            hr(),
            h5("Download Report"),
            radioButtons("format", "Document format", c("PDF", "HTML", "Word"),
                 inline = TRUE),
            downloadButton("downloadReport","Create Report")
            )
            )
            )
            ) 
 ),
 
 tabPanel("About",
          fluidRow(
            column(6, offset = 0, style='position:relative; left:10%',
                   includeMarkdown("README.md")
              )      
          
          )
 ),
 
 column(width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        
        tags$footer(class="footer",
                    tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                             tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
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

