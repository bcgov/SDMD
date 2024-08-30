function(input, output, session) {

observe({
    temp <- input$ineq
    if (temp == 16) { # update the point entry order for eq. 16 since it start with target qmd
      updateSelectInput(session, "drord", selected = 2)} else {
      updateSelectInput(session, "drord", selected = 1)}  
        
    if (temp == 1|temp == 16) {
      if (input$use.metric == FALSE) {
      updateSelectInput(session, "sdi.lines", selected = c(50,  100, 150, 200, 250, 325 ))} else {
      updateSelectInput(session, "sdi.lines", selected = c(125, 250, 375,  500,  625, 800  ))}  
    }
  })  

# data point entry table
points_df <- data.frame(c1 = rep(NA, 10), c2 = rep(NA, 10)) # initiating


output$max_sdi_input_txt<-renderUI({
  helpText(max_sdi_txt(input$ineq, input$use.metric))
})


output$point_entry <- renderRHandsontable({
   points_df$c1 <- as.character(points_df$c1); points_df$c2 <- as.character(points_df$c2)   # for better look for NAs
   if(input$type == 1 & input$use.metric == TRUE) {
     colnames(points_df) <- c("TPH", "QMD (cm)")
     if(input$ineq == 16) {
       points_df[1, 2] <- input$qmd.target
       points_df[1, 1] <- round(input$max.sdi/(input$qmd.target/25.4)^
                                  IFCslope(as.numeric(input$IFCsp))*input$des.sdi / 100, 0)
     }
   }
   if(input$type == 1 & input$use.metric == FALSE){
     colnames(points_df) <- c("TPA", "QMD (inch)")
     if(input$ineq == 16) {
       points_df[1, 2] <- input$qmd.target
       points_df[1, 1] <- round(input$max.sdi/(input$qmd.target/10)^
                                  IFCslope(as.numeric(input$IFCsp))*input$des.sdi / 100, 0)
     }
   }
   if(input$type == 2 & input$use.metric == TRUE){
     colnames(points_df) <- c("TPH", "BA (m2 per ha)")
     if(input$ineq == 16) {
       points_df[1, 1] <- round(input$max.sdi/(input$qmd.target/25.4)^
                                  IFCslope(as.numeric(input$IFCsp))*input$des.sdi / 100, 0)
       points_df[1, 2] <- round((input$qmd.target)^2 * (0.0000785 * as.numeric(points_df[1, 1])), 1)
     }
   }
   if(input$type == 2 & input$use.metric == FALSE){
     colnames(points_df) <- c("TPA", "BA (ft2 per ac)")
     if(input$ineq == 16) {
       points_df[1, 1] <- round(input$max.sdi/(input$qmd.target/10)^
                                  IFCslope(as.numeric(input$IFCsp))*input$des.sdi / 100, 0)
       points_df[1, 2] <- round((input$qmd.target)^2 * (0.005454 * as.numeric(points_df[1, 1])), 1)
     }
   }
                           
   rhandsontable(points_df, stretchH = 'all') %>%
     hot_cols(colWidths = 100) %>%
     hot_rows(rowHeights = 35)
})


# function for drawing SDMD and segmented lines  
p1<-function(){
    coords <- hot_to_r(input$point_entry)       # convert rhandsontable to r object
    coords <- coords[complete.cases(coords), ]  # remove incomplete data entry
    if (input$drord == 2) coords<- coords[seq(dim(coords)[1],1),] # reverse if backward entry

    if (input$type == 1){
        do.call(dmd.view, dmd_view_args(in_ineq = input$ineq, in_max.sdi = input$max.sdi, 
                                        in_title = input$title, in_sdilines = input$sdi.lines, 
                                        in_mgtzone = input$mgt.zone, in_reineketerm = input$reineke.term,
                                        in_IFCsp = input$IFCsp, in_bsi = input$bsi, invol = input$invol, 
                                        in_use.num = input$use.metric))
        } else {
        do.call(gdmd.view, gdmd_view_args(in_ineq = input$ineq, in_max.sdi = input$max.sdi,
                                          in_mgtzone = input$mgt.zone,in_reineketerm = input$reineke.term,
                                          in_IFCsp = input$IFCsp, in_bsi = input$bsi, 
                                          in_use.num = input$use.metric))  
        }
    x <- as.numeric(c(coords[1,1], coords[, 1]))
    y <- as.numeric(c(ifelse(as.logical(input$use.metric), 3, 1), coords[, 2]))
    points(x, y, pch = c(NA, rep(19, length(x)-1)), cex = 1.3)
    s <- seq(length(x) - 1) # determine number of points
    segments(x[s], y[s], x[s+1], y[s+1], lwd=1.3)
    }

DF1 <- eventReactive(input$calc, input$point_entry)

t1 <- function(){
  DF2 <- hot_to_r(DF1())
  DF2[, 1] <- as.numeric(DF2[, 1]); DF2[, 2] <- as.numeric(DF2[, 2])
  DF2 <- DF2[complete.cases(DF2), ]  # remove incomplete data entry
  if (input$drord == 2) DF2 <- DF2[seq(dim(DF2)[1],1),] # reverse if backward entry
  # calculate stand attribute summary
  t_out <- dmd.vol.out(DF2, type = as.numeric(input$type), 
                            ineq = as.numeric(input$ineq),
                            use.metric = as.logical(input$use.metric), 
                            max.sdi = as.numeric(input$max.sdi))
  t_out
    }

output$dmdview <- renderUI({
  output$plot_temp <- renderPlot({
    p1()
       })
  plotOutput("plot_temp", height = input$plotsize, width = ifelse(input$type==1,"85%", "100%"))
  
})

output$Est_vol <- renderDT({
  datatable(t1(),
            container = eval(parse(text = head_maker(ineq = input$ineq, 
                                                     use.metric = input$use.metric))), 
            rownames = FALSE, 
            options = sum_tb_opt,
            caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: left;',
                htmltools::tags$p('Note: ', 
                      htmltools::em(cap_maker(ineq = input$ineq, 
                                              use.metric = input$use.metric))))       
            )
})

output$SDMD_out = downloadHandler(
  filename = function() {
    paste("SDMD", sep = ".", switch(
      input$IMGformat, PDF = "pdf", PNG = "png", TIFF = "tiff")
    )
  },
  content = function(file) {
    switch(
      input$IMGformat,
      PDF=pdf(file, width=8, height=12),
      PNG=png(file, width = 6, height = 8, units = 'in', pointsize=10, res = 300),
      TIFF=tiff(file, width = 6, height = 8, units = 'in', pointsize=10, res = 300))
    p1()
    dev.off()
  }
)

output$stable_out <- downloadHandler(
  filename = function(){
    paste("SDMD_table", switch(input$Tblformat, CSV="csv", XLSX="xlsx"), sep = "." )
  },
  content = function(file) {
    t_2<-t1()
    switch(
      input$Tblformat, 
      CSV=write.csv(t_2, file, row.names = FALSE),
      XLSX=write.xlsx(t_2, file, row.names = FALSE)
    )
    
  }
)

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('SDMD_report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html')
    )
  },
  
  content = function(file) {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document()
    ))
    file.rename(out, file)
  }
)



}
  