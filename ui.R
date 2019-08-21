### This is ui.R

library(shiny)

navbarPage("Stand Density Management Diagram",

  # Give the title page
 
  

 tabPanel("Plot", 
    
    
    fluidRow(
      column(3, wellPanel(
        selectInput(inputId="ineq",label="Select species and reference",
             choices = c("User Defined" =1, 
                         "ponderosa pine:	Long and Shaw (2005)" =2,
                         "ponderosa pine:	Ritchie and Zhang (In Press)" = 3,
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
          condition = "!(input.ineq == 1|input.ineq == 4|input.ineq == 5|input.ineq == 8)",
          selectInput(inputId="invol",label="Overlay the volume iso-lines?",
              choices = c("No"=FALSE,"Yes"=TRUE),
              selected = FALSE)),

        conditionalPanel(
          condition = "input.ineq==6",
          numericInput("max.sdi6", "Maximum SDI", NA),
          sliderInput("mgt.zone6", "Management Zone",
                    min = 0, max = 1,
                    value = c(0.35,0.60)),
          helpText("Provide maximum SDI for Rendering")),
               
        conditionalPanel(
          condition = "input.ineq==7",
          hr(),
          numericInput("max.sdi7", "Maximum SDI", NA),
          helpText("Note: Maximum SDI is used for stand attribute calculation only.")),
      
        conditionalPanel(
          condition = "input.ineq==1",
          helpText("Note: All information should be filled to render the diagram."),
          numericInput("max.sdi1", "Maximum SDI", NA),
          textInput("title1","Title","User Defined"),
          sliderInput("mgt.zone1", "Management Zone",
                      min = 0, max = 1,
                      value = c(0.35,0.60)),
          numericInput("reineke.term", "Reineke's Slope", 1.605),
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
            numericInput("qmd8", "QMD", NULL)),
          
  
        conditionalPanel(
          condition = "!(input.ineq == 4|input.ineq == 5|input.ineq == 8)",
          helpText("Predict stand attributes. Click the Calculate button to update"),
          actionButton("calc", "Calculate")),
        
        conditionalPanel(
          condition = "(input.ineq == 4|input.ineq == 5|input.ineq == 8)",
          helpText("Stand attribute prediction is not available"))
    )),
  
  
  column(3, mainPanel(      
        plotOutput("dmdview",width = "500px", height="800px"),

        
        conditionalPanel(
          condition = "!(input.ineq == 4|input.ineq == 5|input.ineq == 8)",
          verbatimTextOutput("Est_vol")
        
        )
    )
  ))),
 
 tabPanel("Export",
    fluidRow(
            
        column(6, mainPanel(      
            h4("Export Outputs"),
            helpText("Note: In order to export, SDMD Plot and/or stand attribute table should be created in advance."),
            hr(),
            h5("Export SDMD plot"),
            radioButtons("IMGformat", "Image format", c("PDF", "PNG","TIFF"),
                 inline = TRUE),
            downloadButton("SDMD_out", "Save SDMD"),
            hr(),
            
            conditionalPanel(
                condition = "!(input.ineq == 4|input.ineq == 5|input.ineq == 8)",
                
            
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
              column(9,
                     includeMarkdown("about.md")
              )      
          
          )
 )
)

