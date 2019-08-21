library(shiny)
library(standview)
library(rmarkdown)
library(openxlsx)


function(input, output) {
  

# function for drawing SDMD and segmented lines  
p1<-function(){
  x_cord<-c(input$tpa1,input$tpa2,input$tpa3,input$tpa4,input$tpa5,input$tpa6,input$tpa7,input$tpa8)
  y_cord<-c(input$qmd1,input$qmd2,input$qmd3,input$qmd4,input$qmd5,input$qmd6,input$qmd7,input$qmd8)
  s <- 1:7
  
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
  
    points(x_cord,y_cord,pch=19,cex=1.3)
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


  

t1<-function(){


    
  tpa_comb<-c(tpa1(),tpa2(),tpa3(),tpa4(),tpa5(),tpa6(),tpa7(),tpa8())
  qmd_comb<-c(qmd1(),qmd2(),qmd3(),qmd4(),qmd5(),qmd6(),qmd7(),qmd8())

  #remove NAs
  tpa_comb<-tpa_comb[!is.na(tpa_comb)]
  qmd_comb<-qmd_comb[!is.na(qmd_comb)]

  
  if (input$ineq==1&!as.logical(input$use.metric)){
    

    tpa.ac<-NULL
    qmd.in<-NULL
    ba.ft2ac<-NULL
    volume.ft3ac<-NULL
    vol.95CI<-NULL
    height.ft<-NULL
    ht.95CI<-NULL
    biomass.Tonsac<-NULL
    bio.95CI<-NULL
    ccpct<-NULL
    cc.95CI<-NULL
    

    for (i in 1:length(tpa_comb)){
      tpa.ac<-c(tpa.ac, tpa_comb[i])
      qmd.in<-c(qmd.in, qmd_comb[i])
      ba.ft2ac<-c(ba.ft2ac, round(tpa_comb[i]*qmd_comb[i]*qmd_comb[i]*pi/456,2))
      
      volume.ft3ac<-c(volume.ft3ac, round(vol.fun(tpa_comb[i],qmd_comb[i])*1000,2))
      vol.95CI<-c(vol.95CI, paste("[",paste(c(
          round((vol.fun(tpa_comb[i],qmd_comb[i])-VOL.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881))*1000,2),
          round((vol.fun(tpa_comb[i],qmd_comb[i])+VOL.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881))*1000,2)),
          collapse=" "),"]"))
      
      height.ft<-c(height.ft, round(ht.fun(tpa_comb[i],qmd_comb[i]),1))
      ht.95CI<-c(ht.95CI, paste("[",paste(c(
        round(ht.fun(tpa_comb[i],qmd_comb[i])-HT.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 911),2),
        round(ht.fun(tpa_comb[i],qmd_comb[i])+HT.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 911),2)),
        collapse=" "),"]"))
      
      biomass.Tonsac<-c(biomass.Tonsac, round(bio.fun(tpa_comb[i],qmd_comb[i]),2))
      bio.95CI<-c(bio.95CI, paste("[",paste(c(
        round(bio.fun(tpa_comb[i],qmd_comb[i])-BIO.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881),2),
        round(bio.fun(tpa_comb[i],qmd_comb[i])+BIO.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881),2)),
        collapse=" "),"]"))
      
      ccpct<-c(ccpct, round(cc.fun(tpa_comb[i],qmd_comb[i]),1))
      cc.95CI<-c(cc.95CI, paste("[",paste(c(
        round(cc.fun(tpa_comb[i],qmd_comb[i])-CC.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881),2),
        round(cc.fun(tpa_comb[i],qmd_comb[i])+CC.SE_delta(tpa_comb[i],qmd_comb[i])*qt(0.975, 881),2)),
        collapse=" "),"]"))  

    }

    t_out<-as.data.frame( cbind(tpa.ac,qmd.in, ba.ft2ac, volume.ft3ac, vol.95CI,
                                   height.ft, ht.95CI, biomass.Tonsac, bio.95CI, ccpct, cc.95CI))
    colnames(t_out)<-c("tpa.ac","qmd.in", "ba.ft2ac", "volume.ft3ac", "vol.95CI",
                         "height.ft", "ht.95CI", "biomass.Tonsac", "bio.95CI", "ccpct", "cc.95CI")
    
     
    
                    
  } else if (input$ineq==1&as.logical(input$use.metric)){  
    
    # convert to english for calculation and after then revert to metric 
    
    tph_e<-round(tpa_comb/2.471052,0)
    qmd_e<-round(qmd_comb/2.54,1)
    
    
    tph.ha<-NULL
    qmd.cm<-NULL
    ba.m2ha<-NULL
    volume.m3ha<-NULL
    vol.95CI<-NULL
    height.m<-NULL
    ht.95CI<-NULL
    biomass.Mgha<-NULL
    bio.95CI<-NULL
    ccpct<-NULL
    cc.95CI<-NULL
    
    
    for (i in 1:length(tpa_comb)){
      tph.ha<-c(tph.ha, tpa_comb[i]) # print as it is (English)
      qmd.cm<-c(qmd.cm, qmd_comb[i]) # print as it is (English)
      ba.m2ha<-c(ba.m2ha, round(tpa_comb[i]*qmd_comb[i]*qmd_comb[i]*pi/40000,2)) # calculated metric input directly
      
      volume.m3ha<-c(volume.m3ha, round(vol.fun(tph_e[i],qmd_e[i])*1000*0.0283168*2.471052,2))
      vol.95CI<-c(vol.95CI, paste("[",paste(c(
        round((vol.fun(tph_e[i],qmd_e[i])-VOL.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881))*1000*0.0283168*2.471052,2),
        round((vol.fun(tph_e[i],qmd_e[i])+VOL.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881))*1000*0.0283168*2.471052,2)),
        collapse=" "),"]"))
      
      height.m<-c(height.m, round(ht.fun(tph_e[i],qmd_e[i])*0.3048,1))
      ht.95CI<-c(ht.95CI, paste("[",paste(c(
        round((ht.fun(tph_e[i],qmd_e[i])-HT.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 911))*0.3048,2),
        round((ht.fun(tph_e[i],qmd_e[i])+HT.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 911))*0.3048,2)),
        collapse=" "),"]"))
      
      biomass.Mgha<-c(biomass.Mgha, round(bio.fun(tph_e[i],qmd_e[i])*0.907185*2.471052,2))
      bio.95CI<-c(bio.95CI, paste("[",paste(c(
        round((bio.fun(tph_e[i],qmd_e[i])-BIO.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881))*0.907185*2.471052,2),
        round((bio.fun(tph_e[i],qmd_e[i])+BIO.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881))*0.907185*2.471052,2)),
        collapse=" "),"]"))
      
      ccpct<-c(ccpct, round(cc.fun(tph_e[i],qmd_e[i]),1)) 
      cc.95CI<-c(cc.95CI, paste("[",paste(c(
        round(cc.fun(tph_e[i],qmd_e[i])-CC.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881),2),
        round(cc.fun(tph_e[i],qmd_e[i])+CC.SE_delta(tph_e[i],qmd_e[i])*qt(0.975, 881),2)),
        collapse=" "),"]"))  
      
    }
    
    t_out<-as.data.frame( cbind(tph.ha,qmd.cm, ba.m2ha, volume.m3ha, vol.95CI,
                                height.m, ht.95CI, biomass.Mgha, bio.95CI, ccpct, cc.95CI))
    colnames(t_out)<-c("tpa.ha","qmd.cm", "ba.m2ha", "volume.m3ha", "vol.95CI",
                       "height.m", "ht.95CI", "biomass.Mgha", "bio.95CI", "ccpct", "cc.95CI")
    
    
    
  } else {
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
}
#t_out<-round(t_out,1) #round numbers to 1 decimal place -> this caused error, suppressed for now.
t_out
}

output$dmdview <- renderPlot({
   par(mar=c(5.1,.5,4.1,2.1)) 
    p1()
       })

  
output$Est_vol<-renderPrint({
    t1()
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
  