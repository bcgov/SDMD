## this file is for functions and html tags for readability of server.R and ui. R files.

# max.sdi input range guidelines

max_sdi_txt <- function(ineq, use.metric){
  if(use.metric == FALSE){
    return(switch(as.numeric(ineq),
        "Enter maximum SDI within a range of 300-800 TPA",   #1
        "",                                                  #2
        "",                                                  #3
        "",                                                  #4
        "",                                                  #5
        "Enter maximum SDI within a range of 450-600 TPA",   #6
        "",                                                  #7
        "",                                                  #8
        "",                                                  #9
        "Enter maximum SDI within a range of 527-564 TPA",   #10
        "Enter maximum SDI within a range of 547-608 TPA",   #11
        "Enter maximum SDI within a range of 502-669 TPA",   #12
        "Enter maximum SDI within a range of 750-1000 TPA",  #13
        "Enter maximum SDI within a range of 550-700 TPA",   #14
        "",                                                  #15
        "")                                                  #16
    )} else {
    return(switch(as.numeric(ineq),
        "Enter maximum SDI within a range of 750-2000 TPH",  #1
        "",                                                  #2
        "",                                                  #3
        "",                                                  #4
        "",                                                  #5
        "Enter maximum SDI within a range of 1112-1483 TPH", #6
        "",                                                  #7
        "",                                                  #8
        "",                                                  #9
        "Enter maximum SDI within a range of 1302-1394 TPH", #10
        "Enter maximum SDI within a range of 1352-1502 TPH", #11
        "Enter maximum SDI within a range of 1240-1653 TPH", #12
        "Enter maximum SDI within a range of 1875-2500 TPH", #13
        "Enter maximum SDI within a range of 1360-1750 TPH", #14
        "",                                                  #15
        ""                                                   #16
      ))
    }
    }

# function for determine Reineke's slope term of Eq. #16
IFCslope <- function(IFCspp){
  return(switch(IFCspp,
                1.449,     # INW-DF
                1.326,     # INW-GF
                1.321,     # INW-LP
                1.49,      # INW-PP
                1.364,     # INW-WL
                1.517,     # PNW-DF MIX
                1.461))}   # PNW - HEMFIR

# (g)dmd.view function arguments generator
dmd_view_args <- function(in_ineq, in_max.sdi, in_title, in_sdilines, in_mgtzone, in_reineketerm,
                          in_IFCsp, in_bsi, invol, in_use.num) {
  return(list(ineq         = ifelse(in_ineq == 16, 1, in_ineq),
              inul = TRUE, insdi = TRUE, inply = TRUE, insdr = FALSE, insdl = TRUE,
              max.sdi      = in_max.sdi,
              dmd.title    = ifelse(in_ineq == 1, in_title, ifelse(in_ineq == 16, "NW mixed-conifer: Kimsey Jr. et al. (2019)", " ")),
              sdi.lines    = if(in_ineq %in% c(1, 16)) in_sdilines else NA,
              mgt.zone     = in_mgtzone,
              reineke.term = ifelse(in_ineq==1, in_reineketerm, 
                                    ifelse(in_ineq == 16, IFCslope(as.numeric(in_IFCsp)), 1.605)),
              bsi          = in_bsi,
              mzcol        = "grey",
              sdicol       = "red",
              invol        = as.logical(invol),
              vcol         = "blue",
              use.metric   = as.logical(in_use.num)))
    }

gdmd_view_args <- function(in_ineq, in_max.sdi, in_mgtzone, in_reineketerm,
                            in_IFCsp, in_bsi, in_use.num) {
  return(list(ineq  = ifelse(in_ineq == 16, 1, in_ineq),
              inul = TRUE, inrd = TRUE, rdlabel = TRUE, inply = TRUE, inqmd = TRUE, inspace = TRUE,
              max.sdi = in_max.sdi, umz = NA, lmz = NA,
              mgt.zone  = in_mgtzone,
              reineke.term = ifelse(in_ineq==1, in_reineketerm, 
                                  ifelse(in_ineq == 16, IFCslope(as.numeric(in_IFCsp)), 1.605)),
              bsi  = in_bsi,
              dcol = "blue", rdcol = "black", mzcol = "lightgrey", dmd.title    = " ",
              use.metric   = as.logical(in_use.num)))
    }


# dmd.volume converter; metric vs. English:
# The dmd.volume uses tpa and qmd for calculation. 
# Thus, for Gingrich's SDMD, BA input should be converted to qmd.

dmd.vol.conv <- function(df, type, use.metric){
  colnames(df)[1] <- "DEN"
  colnames(df)[2] <- ifelse(type == 1, "QMD", "BA")
  if(type == 1 & use.metric == FALSE) {         # Reineke, English
    df$BA <- df$QMD **2 * 0.005454 * df$DEN
  } else if (type == 1 & use.metric == TRUE) {  # Reineke, English
    df$BA <- df$QMD **2 * 0.00007854 * df$DEN
  } else if (type == 2 & use.metric == FALSE) { # Ginglich, English
    df$QMD <- sqrt(df$BA/df$DEN/0.005454)
  } else {                                      # Ginglish, metric
    df$QMD <- sqrt(df$BA/df$DEN/0.00007854)
  }
  return(df)
}  

dmd.vol.out <- function(df, type, ineq, use.metric, max.sdi = NULL) {
  df <- dmd.vol.conv(df, type, use.metric)
  if (ineq %in% c(1, 4, 5, 8:16)){ # produce DEN, BA, QMD only
    if(use.metric == FALSE){
      tpa.ac   <- df$DEN; qmd.in   <- df$QMD; ba.ft2ac <- df$BA
      stands <- as.data.frame(cbind(tpa.ac, qmd.in, ba.ft2ac) )
    } else {
      tpa.ha  <- df$DEN; qmd.cm  <- df$QMD; ba.m2ha <- df$BA
      stands <- as.data.frame(cbind(tpa.ha, qmd.cm, ba.m2ha) )
    }
  } else {
    stands <- dmd.volume(ineq = ineq,
                        tpa = df$DEN[1],
                        qmd = df$QMD[1],
                        max.sdi      = max.sdi,
                        use.metric   = use.metric)
    if (dim(df)[1] > 1){
      for (i in 2:dim(df)[1]){
        stands <- rbind(stands, dmd.volume(ineq = ineq,
                                       tpa = df$DEN[i],
                                       qmd = df$QMD[i],
                                       max.sdi      = max.sdi,
                                       use.metric   = use.metric))
      }
    }
    
  } 
  # trimming
  if (ineq == 2){stands <- stands[, 1:4]}
  if (ineq %in% c(6, 7, 9)){stands <- stands[, c(1:4, 6)]}
  stands <- round(stands, 1)
  stands
} 


  
head_maker <- function(ineq, use.metric){
# header tags for SDMD stand summary table
  if(ineq == 3 & use.metric == FALSE){
  header <- 'htmltools::withTags(table(class = "display", 
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
  )'
  } else if (ineq == 3 & use.metric == TRUE){
  header <- 'htmltools::withTags(table(
  class = "display", 
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
  )'
  } else if (ineq == 2){
    header <- 'htmltools::withTags(table(class = "display", 
    thead(
      tr(
        th("TPA(H)", style = "text-align: center"),
        th("QMD", style = "text-align: center"),
        th("BA", style = "text-align: center"),
        th("Volume", style = "text-align: center")
      ) 
    )
  )
  )'
  } else if (ineq %in% c(6, 7, 9)){
    header <- 'htmltools::withTags(table(class = "display", 
    thead(
      tr(
        th("TPA(H)", style = "text-align: center"),
        th("QMD", style = "text-align: center"),
        th("BA", style = "text-align: center"),
        th("Volume", style = "text-align: center"),
        th("Top Height", style = "text-align: center")
      ) 
    )
  )
  )'
  } else {
  header <- 'htmltools::withTags(table(class = "display", 
    thead(
      tr(
        th("TPA(H)", style = "text-align: center"),
        th("QMD", style = "text-align: center"),
        th("BA", style = "text-align: center")
      ) 
    )
  )
  )'
  }
  header
}

# options for SDMD stand summary table 
sum_tb_opt = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                  columnDefs=list(list(targets= '_all', class="dt-center")))

# tags for SDMD stand summary table caption
cap_maker <- function (ineq, use.metric) {
  if(use.metric == FALSE){
  base.string <- "TPA = trees per acre, QMD = quadratic mean diameter (inch), BA = basal area (square feet per acre)"
  } else {
  base.string <- "TPH = trees per ha, QMD = quadratic mean diameter (cm), BA = basal area (square m per ha)"  
  }
  if (ineq == 3){
  out.string <- paste(base.string, ", SE = standard error.", sep ="")  
  }
  if (ineq == 2 & use.metric == FALSE){
    out.string <- paste(base.string, ", Volume: cubic feet per acre.", sep ="")  
  } else if (ineq == 2 & use.metric == TRUE){
    out.string <- paste(base.string, ", Volume: cubic metre per hectare.", sep ="")   
  } else if (ineq %in% c(6, 7, 9) & use.metric == FALSE){
    out.string <- paste(base.string, ", Volume: cubic feet per acre. Height: feet", sep ="")
  } else if (ineq %in% c(6, 7, 9) & use.metric == TRUE){
    out.string <- paste(base.string, ", Volume: cubic metre per hectare. Height: metre", sep ="")  
  } else {
    out.string <- paste(base.string, "." , sep ="")    
  }
  out.string
}

cap_maker2 <- function (ineq, use.metric) {
  if (ineq == 3 & use.metric == FALSE){
    out.string <- "Volume: cubic feet per acre, Top Height: feet, Biomass: tons per acre, Canopy cover: %."
  } else if (ineq == 3 & use.metric == TRUE){ 
    out.string <- "Volume: cubic metre per hactare, Top Height: metre, Biomass: tons per hactare, Canopy cover: %."
  } else if (ineq == 2 & use.metric == FALSE){
    out.string <- "Volume: cubic feet per acre."  
  } else if (ineq == 2 & use.metric == TRUE){
    out.string <- "Volume: cubic metre per hectare."   
  } else if (ineq %in% c(6, 7, 9) & use.metric == FALSE){
    out.string <- "Volume: cubic feet per acre. Height: feet"
  } else if (ineq %in% c(6, 7, 9) & use.metric == TRUE){
    out.string <- "Volume: cubic metre per hectare. Height: metre"  
  } else {
    out.string <- ""   
  }
  out.string
}


