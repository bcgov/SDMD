library(shiny)
#library(standview)
library(rmarkdown)
library(openxlsx)
library(DT)
source("standview.R")


function(input, output) {
  

# function for drawing SDMD and segmented lines  
p1<-function(){
    x_cord<-rep(NA,9)
    y_cord<-rep(NA,9)
    pch_index<-rep(NA,9)
    
    x_temp<-c(input$tpa1,input$tpa2,input$tpa3,input$tpa4,input$tpa5,input$tpa6,input$tpa7,input$tpa8)
    y_temp<-c(input$qmd1,input$qmd2,input$qmd3,input$qmd4,input$qmd5,input$qmd6,input$qmd7,input$qmd8)
    x_temp_g<-c(input$tpa1_g,input$tpa2_g,input$tpa3_g,input$tpa4_g,input$tpa5_g,input$tpa6_g,input$tpa7_g,input$tpa8_g)
    y_temp_g<-c(input$ba1,input$ba2,input$ba3,input$ba4,input$ba5,input$ba6,input$ba7,input$ba8)
  
    if (input$drord==1&input$type==1) {
        x_cord<-c(x_temp[1],x_temp)
        y_cord<-c(ifelse(as.logical(input$use.metric),3,1),y_temp)
        pch_index<-c(NA,rep(19,8))
    } else if (input$drord==2&input$type==1&sum(!is.na(x_temp))>0) {
        na.id<-length(x_temp[!is.na(x_temp)])
        x_cord<-c(rep(x_temp[na.id],9-na.id),x_temp[na.id:1])
        y_cord<-c(rep(ifelse(as.logical(input$use.metric),3,1),9-na.id),y_temp[na.id:1])
        pch_index<-c(rep(NA,9-na.id),rep(19,na.id))
    } else  if (input$drord==1&input$type==2) {
        x_cord<-c(x_temp_g[1],x_temp_g)
        y_cord<-c(1,y_temp_g)
        pch_index<-c(NA,rep(19,8))
    } else if (input$drord==2&input$type==2&sum(!is.na(x_temp_g))>0) {
        na.id<-length(x_temp_g[!is.na(x_temp_g)])
        x_cord<-c(rep(x_temp_g[na.id],9-na.id),x_temp_g[na.id:1])
        y_cord<-c(rep(1,9-na.id),y_temp_g[na.id:1])
        pch_index<-c(rep(NA,9-na.id),rep(19,na.id))
    }
    
    
  s <- 1:8
  if (input$type==1){
  dmd.view(ineq         = input$ineq,
                          inul         = TRUE,
                          insdi        = TRUE,
                          inply        = TRUE,
                          insdr        = FALSE,
                          insdl        = TRUE,
                          max.sdi      = ifelse(input$ineq==1,input$max.sdi1,ifelse(input$ineq==6,input$max.sdi6,
                                                                                    ifelse(input$ineq==7,input$max.sdi7,NA))),
                          dmd.title    = ifelse(input$ineq==1,input$title1," "),
                          sdi.lines    = input$sdi.lines,
                          mgt.zone     = if (input$ineq==1) input$mgt.zone1 else if (input$ineq==6) input$mgt.zone6 else NA,
                          reineke.term = ifelse(input$ineq==1,input$reineke.term,1.605),
                          bsi          = input$bsi,
                          mzcol        = "grey",
                          sdicol       = "red",
                          invol        = as.logical(input$invol),
                          vcol         = "blue",
                          use.metric   = as.logical(input$use.metric)
  )
  } else {
    gdmd.view(ineq     = input$ineq,
                        inul         = TRUE,
                        inrd         = TRUE,
                        rdlabel      = TRUE,
                        inply        = TRUE,
                        inqmd        = TRUE,
                        inspace      = TRUE,
                        max.sdi      = ifelse(input$ineq==1,input$max.sdi1,ifelse(input$ineq==6,input$max.sdi6,
                                                                                  ifelse(input$ineq==7,input$max.sdi7,NA))),
                        umz          = NA,
                        lmz          = NA,
                        mgt.zone     = if (input$ineq==1) input$mgt.zone1 else if (input$ineq==6) input$mgt.zone6 else c(0.35,0.60),
                        reineke.term = ifelse(input$ineq==1,input$reineke.term,1.6),
                        bsi          = ifelse(is.na(input$bsi),90,input$bsi),
                        dcol         = "blue",
                        rdcol        = "black",
                        mzcol        = "lightgrey",
                        dmd.title    = " ",
                        use.metric   = as.logical(input$use.metric))  
    
  }

    points(x_cord,y_cord,pch=pch_index,cex=1.3)
    segments(x_cord[s],y_cord[s], x_cord[s+1], y_cord[s+1], lwd=1.3)

}

tpa1<-eventReactive(input$calc,as.numeric(input$tpa1))
qmd1<-eventReactive(input$calc,as.numeric(input$qmd1))
tpa2<-eventReactive(input$calc,as.numeric(input$tpa2))
qmd2<-eventReactive(input$calc,as.numeric(input$qmd2))
tpa3<-eventReactive(input$calc,as.numeric(input$tpa3))
qmd3<-eventReactive(input$calc,as.numeric(input$qmd3))
tpa4<-eventReactive(input$calc,as.numeric(input$tpa4))
qmd4<-eventReactive(input$calc,as.numeric(input$qmd4))
tpa5<-eventReactive(input$calc,as.numeric(input$tpa5))
qmd5<-eventReactive(input$calc,as.numeric(input$qmd5))
tpa6<-eventReactive(input$calc,as.numeric(input$tpa6))
qmd6<-eventReactive(input$calc,as.numeric(input$qmd6))
tpa7<-eventReactive(input$calc,as.numeric(input$tpa7))
qmd7<-eventReactive(input$calc,as.numeric(input$qmd7))
tpa8<-eventReactive(input$calc,as.numeric(input$tpa8))
qmd8<-eventReactive(input$calc,as.numeric(input$qmd8))

tpa1_g<-eventReactive(input$calc,as.numeric(input$tpa1_g))
ba1<-eventReactive(input$calc,as.numeric(input$ba1))
tpa2_g<-eventReactive(input$calc,as.numeric(input$tpa2_g))
ba2<-eventReactive(input$calc,as.numeric(input$ba2))
tpa3_g<-eventReactive(input$calc,as.numeric(input$tpa3_g))
ba3<-eventReactive(input$calc,as.numeric(input$ba3))
tpa4_g<-eventReactive(input$calc,as.numeric(input$tpa4_g))
ba4<-eventReactive(input$calc,as.numeric(input$ba4))
tpa5_g<-eventReactive(input$calc,as.numeric(input$tpa5_g))
ba5<-eventReactive(input$calc,as.numeric(input$ba5))
tpa6_g<-eventReactive(input$calc,as.numeric(input$tpa6_g))
ba6<-eventReactive(input$calc,as.numeric(input$ba6))
tpa7_g<-eventReactive(input$calc,as.numeric(input$tpa7_g))
ba7<-eventReactive(input$calc,as.numeric(input$ba7))
tpa8_g<-eventReactive(input$calc,as.numeric(input$tpa8_g))
ba8<-eventReactive(input$calc,as.numeric(input$ba8))

  

t1<-function(){


  if (input$type==1){    
  tpa_comb<-c(tpa1(),tpa2(),tpa3(),tpa4(),tpa5(),tpa6(),tpa7(),tpa8())
  qmd_comb<-c(qmd1(),qmd2(),qmd3(),qmd4(),qmd5(),qmd6(),qmd7(),qmd8())
  }else{
  tpa_comb<-c(tpa1_g(),tpa2_g(),tpa3_g(),tpa4_g(),tpa5_g(),tpa6_g(),tpa7_g(),tpa8_g())
  ba_comb<-c(ba1(),ba2(),ba3(),ba4(),ba5(),ba6(),ba7(),ba8())
  qmd_comb<-sqrt(tpa_comb/ba_comb/0.005454) 
  
}
  #remove NAs
  tpa_comb<-tpa_comb[!is.na(tpa_comb)]
  qmd_comb<-qmd_comb[!is.na(qmd_comb)]


    t_out<-dmd.volume(ineq=as.numeric(input$ineq),
                      tpa=tpa_comb[1],
                      qmd=qmd_comb[1],
                      max.sdi      = ifelse(input$ineq==1,input$max.sdi1,ifelse(input$ineq==6,input$max.sdi6,
                                                                                ifelse(input$ineq==7,input$max.sdi7,NA))),
                      use.metric   = as.logical(input$use.metric)
    )
    if (length(tpa_comb)>1){
    for (i in 2:length(tpa_comb)){
    t_out<-rbind(t_out, dmd.volume(ineq=as.numeric(input$ineq),
                                     tpa=tpa_comb[i],
                                     qmd=qmd_comb[i],
                                     max.sdi      = ifelse(input$ineq==1,input$max.sdi1,ifelse(input$ineq==6,input$max.sdi6,
                                                                                               ifelse(input$ineq==7,input$max.sdi7,NA))),
                                     use.metric   = as.logical(input$use.metric)))
    }
    }

t_out<-round(t_out,1)
t_out

}

output$dmdview <- renderPlot({
   par(mar=c(5.1,.5,4.1,2.1)) 
    p1()
       })

  
output$Est_vol<-renderDT({
if(input$use.metric==FALSE){
  SDMD.header = htmltools::withTags(table(class = 'display', 
    thead(
      tr(
        th(rowspan = 2, "TPA", style = "text-align: center"),
        th(rowspan = 2, "QMD", style = "text-align: center"),
        th(rowspan = 2, "BA", style = "text-align: center"),
        th(colspan = 2, "Volume (cubic feet per acre)", style = "text-align: center"),
        th(colspan = 2, "Top Height (feet)", style = "text-align: center"),
        th(colspan = 2, "Biomass (Tons per acre)", style = "text-align: center"),
        th(colspan = 2, "Canopy Cover (%)", style = "text-align: center")
      ),
      tr(
        lapply(rep(c("Estimates", "SE"),4), th)
      )  
    )
  )
  )
  
  datatable(t1(),
            container =  SDMD.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-center"))),
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: left;',
              htmltools::tags$p('Note: ', htmltools::em('TPA = trees per acre, QMD = quadratic mean diameter (inch), BA = basal area (square feet per acre), SE = standard error.'))
            )          

            )
}
else {
  SDMD.header = htmltools::withTags(table(
    class = 'display', 
    thead(
      tr(
        th(rowspan = 2, "TPH", style = "text-align: center"),
        th(rowspan = 2, "QMD", style = "text-align: center"),
        th(rowspan = 2, "BA", style = "text-align: center"),
        th(colspan = 2, "Volume (cubic m per ha)", style = "text-align: center"),
        th(colspan = 2, "Top Height (m)", style = "text-align: center"),
        th(colspan = 2, "Biomass (Tons per ha)", style = "text-align: center"),
        th(colspan = 2, "Canopy Cover (%)", style = "text-align: center")
      ),
      tr(
        lapply(rep(c("Estimates", "SE"),4), th)
      )  
    )
  )
  )
  
  datatable(t1(),
            container =  SDMD.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-center"))),
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: left;',
              htmltools::tags$p('Note: ', htmltools::em('TPH = trees per ha, QMD = quadratic mean diameter (cm), BA = basal area (square m per ha), SE = standard error.'))
            )          
  )    
}
}  
)

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
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx')
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
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)




}
  