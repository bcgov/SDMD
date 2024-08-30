# This file is a compiled one from: DMDview.r, dmdiso.r, dmdvolume.r, gdmd.r
# Date: 2024 Aug. 6
# source code located: https://github.com/mwritchie/standview


# DMDview.r
# Code written by Martin Ritchie, USFS, Pacific Southwest Research Station, Redding Laboratory


dmd.view<-function(ineq         = 3,
                   inul         = TRUE,
                   insdi        = TRUE,
                   inply        = TRUE,
                   insdr        = FALSE,
                   insdl        = FALSE,
                   max.sdi      = NA,
                   dmd.title    = " ",
                   sdi.lines    = NA,
                   mgt.zone     = c(0.35,0.60),
                   reineke.term = 1.605,
                   bsi          = 90,
                   mzcol        = "grey",
                   sdicol       = "red",
                   invol        = FALSE,
                   vcol         = "blue",
                   use.metric   = FALSE){
  
  ineq<-as.integer(ineq)
  max.sdi<-as.numeric(max.sdi)
  sdi.lines<-as.numeric(sdi.lines)
  mgt.zone<-as.numeric(mgt.zone)
  bsi<- as.numeric(bsi)
  reineke.term<-as.numeric(reineke.term)
  #mzcol="grey"
  
  # standard conversions
  ac.to.ha <- 2.471054
  in.to.cm <- 2.54
  feet.to.m <- 0.3048
  ft2.to.m2 <- 0.092903
  max.ineq  <- 15
  
  # Error Checking Section
  
  e.code <- 0 # error code 0 indicates successful process
  # test for acceptable range of values
  if(!(ineq %in% 1:max.ineq)) {
    message("Unacceptable value for argument ineq; Figure not Rendered")
    return()
  }
  # test for acceptable title, must be character string
  if(!is.character(dmd.title)){
    message("Invalid argument dmd.title; must be a character. Figured not rendered by dmd.view")
    return()
  }
  # test for acceptable site index must be between 70 and 110
  if(ineq==5){
    if(!use.metric){
      if(!(bsi>=70 && bsi<=110)){
        message("Invalid argument bsi; Barrett's SI must be between 70 and 110 feet")
        return()
      }
    } else{
      if(!(bsi>=21 && bsi<=34)){
        message("Invalid argument bsi; Barrett's SI must be between 21 and 34 m")
        return()
      }
    }
  }
  
  #if(sum(is.na(sdi.lines))){
  #  message("Invalid sdi.lines array, contains NA or negative values")
  #  return()
  #}
  
  #check values of max.sdi
  if(ineq==1 || ineq==6 || ineq==10 || ineq==11 || ineq==12 || ineq==13 || ineq==14){
    if(is.na(max.sdi)){
      message("Error: User must specify max.sdi if ineq=1, 6, 10, 11, 12, 13, or 14. DMD not rendered")
      return()
    }
    if(!use.metric){
      if(ineq==1 && !((max.sdi>=300) && (max.sdi<=800))){
        message("sdi upper limit for ineq=1 (300-800 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==6 && !((max.sdi>=450) && (max.sdi<=600))){
        message("sdi upper limit for ineq=6 (450-600 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==10 && !((max.sdi>=527) && (max.sdi<=564))){
        message("sdi upper limit for ineq=10 (527-564 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==11 && !((max.sdi>=547) && (max.sdi<=608))){
        message("sdi upper limit for ineq=10 (547-608 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==12 && !((max.sdi>=502) && (max.sdi<=669))){
        message("sdi upper limit for ineq=10 (502-669 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==13 && !((max.sdi>=750) && (max.sdi<=1000))){
        message("sdi upper limit for ineq=11 (750-1000 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==14 && !((max.sdi>=550) && (max.sdi<=700))){
        message("sdi upper limit for ineq=12 (550-700 TPA) invalid: DMD not rendered")
        return()
      }
      
      
    } else{
      if(ineq==1 && !((max.sdi>=750) && (max.sdi<=2000))){
        message("sdi upper limit for ineq=1 (750-2000 THPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==6 && !((max.sdi>=1112) && (max.sdi<=1483))){
        message("sdi upper limit for ineq=6 (1112-1483 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==10 && !((max.sdi>=1302) && (max.sdi<=1394))){
        message("sdi upper limit for ineq=10 (1302-1394 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==11 && !((max.sdi>=1352) && (max.sdi<=1502))){
        message("sdi upper limit for ineq=10 (1352-1502 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==12 && !((max.sdi>=1240) && (max.sdi<=1653))){
        message("sdi upper limit for ineq=10 (1240-1653 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==13 && !((max.sdi>=1875) && (max.sdi<=2500))){
        message("sdi upper limit for ineq=11 (1875-2500 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==14 && !((max.sdi>=1375) && (max.sdi<=1750))){
        message("sdi upper limit for ineq=12 (1360-1750 TPHA) invalid: DMD not rendered")
        return()
      }
      
    }
  }
  
  if(ineq==1){
    sdi.lines<-sort(sdi.lines)
    # check to see if the sdi.lines are less than max.sdi
    if(min(max.sdi-sdi.lines)<=0){
      message("Invalid sdi.lines are not all less than sdi maximum")
      return()
    }
  }
  
  if(ineq>=2){
    if(!anyNA(sdi.lines)){
      sdi.lines<-sort(sdi.lines)
      # check to see if the sdi.lines are less than max.sdi
      if(min(max.sdi-sdi.lines)<=0){
        message("Invalid sdi.lines are not all less than sdi maximum")
        return()
      }
    }
  }
  
  
  # test for acceptable reineke term must be between 1.30 and 2.0
  if(!(reineke.term>=1.30 & reineke.term<=2.00)){
    message("Invalid argument reineke.term; must be between 1.30 and 2.00")
    return()
  }
  
  # reference diameter for sdi: 10 inches, or 25.4 cm
  sdi.index <- ifelse(!use.metric, 10, 25.4 )
  
  #####Set Title #######################################################
  main.t <- dmd.title
  if(dmd.title==" "){
    main.t  <- switch(ineq,
                      "Generic DMD",                                       #1
                      "Ponderosa Pine (Long and Shaw 2005)",               #2
                      "Ponderosa Pine (Jang et al. 2021)",                 #3
                      "Ponderosa Pine (Edminster 1988)",                   #4
                      "Ponderosa Pine (Cochran 1992)",                     #5
                      "Mixed-Conifer (Long and Shaw 2012)",                #6
                      "Coastal Douglas-Fir (Long et al. 1988)",            #7
                      "White Fir (Zhang et al. 2007)",                     #8
                      "Lodgepole Pine (McCarter and Long 1986)",           #9
                      "Spruce/Fir (Weiskittel and Woodall 2023)",         #10
                      "Red Spruce (Weiskittel and Woodall 2023)",         #11
                      "Balsam Fir (Weiskittel and Woodall 2023)",         #12
                      "Redwood/Douglas-fir (Ritchie and Berrill 2022)",   #13
                      "Douglas-fir/Redwood (Ritchie and Berrill 2022)",   #14
                      "loblolly pine (Williams 1994)" )                   #15
  }
  
  # Set the Max SDI as a function of ineq
  if(!use.metric){ # English Units
    max.sdi <- switch(ineq,
                      ifelse(max.sdi<=1000 && max.sdi>=300, max.sdi , 400),  #1
                      450,                                                   #2
                      400,                                                   #3
                      410,                                                   #4
                      365,                                                   #5
                      ifelse(max.sdi<=600 && max.sdi>=450, max.sdi, 550 ),   #6
                      600,                                                   #7
                      800,                                                   #8
                      700,                                                   #9
                      ifelse(max.sdi<=564 && max.sdi>=527, max.sdi, 527 ),   #10
                      ifelse(max.sdi<=608 && max.sdi>=547, max.sdi, 547 ),   #11
                      ifelse(max.sdi<=669 && max.sdi>=502, max.sdi, 502 ),   #12
                      ifelse(max.sdi<=1000 && max.sdi>=700, max.sdi, 900),   #13
                      ifelse(max.sdi<=700 && max.sdi>=500, max.sdi, 700),    #14
                      400 )                                                  #15
  } else{ # metric units
    max.sdi <- switch(ineq,
                      ifelse(max.sdi<=2470 && max.sdi>=741, max.sdi , 988),
                      1110,
                      988,
                      1013,
                      901,
                      ifelse(!(max.sdi<=1482 && max.sdi>=1112), 1359, max.sdi),
                      1482,
                      1977,
                      1730,
                      ifelse(max.sdi<=1394 && max.sdi>=1302, max.sdi, 1302),
                      ifelse(max.sdi<=1502 && max.sdi>=1352, max.sdi, 1352),
                      ifelse(max.sdi<=1653 && max.sdi>=1240, max.sdi, 1240),
                      ifelse(max.sdi<=2470 && max.sdi>=1730, max.sdi, 2223),
                      ifelse(max.sdi<=1730 && max.sdi>=1235, max.sdi, 1730),
                      988)
    
  }
  
  tcex    <- switch(ineq, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50,
                    0.50, 0.50, 0.50, 0.50, 0.50)
  acex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75,
                    0.75, 0.75, 0.75, 0.75, 0.75)
  # size of text for sdi annotation:
  scex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.65,
                    0.65, 0.65, 0.65, 0.65, 0.75)
  sdilw   <- switch(ineq, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,
                    1.00, 1.00, 1.00, 1.00, 1.00)
  
  gridcol <- switch(ineq, "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey",
                    "grey", "grey", "grey", "grey", "grey")
  gridlw  <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                    0.2, 0.2, 0.2, 0.2, 0.2)
  
  # min for x and y axis
  if(!use.metric){
    min.x   <- switch(ineq, 40, 40, 30, 40, 40, 50, 50, 50, 80, 60, 60, 60, 40, 40, 40)
    min.y   <- switch(ineq,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
  } else {
    min.x   <- switch(ineq, 100, 100, 80, 100, 125, 125, 125, 120, 200, 150, 150, 150, 100, 100, 100)
    min.y   <- switch(ineq,   3,   3,  3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3,   3)
  }
  
  # max for x and y axis
  if(!use.metric){
    max.x   <- switch(ineq, 2000, 1000, 1000,  1200,  1000,  1000,  1000,  2000, 2000, 1500,
                      1500, 1500, 2000,  2000,  1200 )
    max.y   <- switch(ineq,   30,   36,   36,    36,    36,    36,    36,    36,   26,   18,
                      18,   18,   36,    36,    24 )
  } else {
    max.x   <- switch(ineq, 5000, 2500, 2500,  3000,  2500,  2500,  2500,  5000, 5000, 4000,
                      4000, 4000,  5000, 5000,  3000 )
    max.y   <- switch(ineq,   76,   92,   92,    92,    92,    92,    96,    92,   66,   46,
                      46,   46,    92,   92,    60 )
  }
  
  # This is the slope term from Reineke Space
  slp     <- switch(ineq,
                    reineke.term,        #1
                    1.6000,              #2
                    1.7721,              #3
                    1.66113,             #4
                    1.7721,              #5
                    1.600,               #6
                    1.605,               #7
                    1.500,               #8
                    1.600,               #9
                    1.605,               #10
                    1.605,               #11
                    1.605,               #12
                    1.605,               #13
                    1.605,               #14
                    1.5053)              #15
  islp    <- 1/slp              # inverse of the slope
  
  # x-array for some plots
  tx <- seq(from=min.x, to=max.x, by=1)
  
  ylim.adj<- switch(ineq,
                    c( 0.90, 1.10),      #1
                    c( 0.88, 1.10),      #2
                    c( 0.88, 1.10),      #3
                    c( 0.88, 1.10),      #4
                    c( 0.88, 1.10),      #5
                    c( 0.88, 1.10),      #6
                    c( 0.88, 1.10),      #7
                    c( 0.88, 1.10),      #8
                    c( 0.88, 1.10),      #9
                    c( 0.88, 1.10),      #10
                    c( 0.88, 1.10),      #11
                    c( 0.88, 1.10),      #12
                    c( 0.88, 1.10),      #13
                    c( 0.88, 1.10),      #14
                    c( 0.88, 1.10))      #15
  
  # sets lower and upper limit of plotable stuff
  xlim.adj<- switch(ineq,
                    c( 0.70, 1.20),     #1
                    c( 0.70, 1.20),     #2
                    c( 0.70, 1.20),     #3
                    c( 0.70, 1.20),     #4
                    c( 0.70, 1.20),     #5
                    c( 0.70, 1.20),     #6
                    c( 0.70, 1.20),     #7
                    c( 0.70, 1.20),     #8
                    c( 0.70, 1.20),     #9
                    c( 0.70, 1.20),     #10
                    c( 0.70, 1.20),     #11
                    c( 0.70, 1.20),     #12
                    c( 0.70, 1.20),     #13
                    c( 0.70, 1.20),     #14
                    c( 0.70, 1.20) )    #15
  
  # tick marks on x axis
  if(!use.metric){  # for English Units
    x.at    <- switch(ineq,
                      c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                        seq(1000, 1900, 100), max.x),
                      c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                        200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                      c(min.x, 40, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                        200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                      c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                        200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1100, max.x),
                      c(min.x, seq(from=50, to=100, by=10), 120, 140, 170,
                        200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                      c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 220, 240, 260, 280, 300, 320, 340, 360, 380, 400,
                        440, 480, 520, 560, 600, 640, 680, 720, 760, 800, 850,
                        900, 950, max.x),
                      c(min.x, 60,  70,  80,  90, 100, 125, 150, 175,
                        200,  225, 250, 275, 300, 325, 350, 375, 400,
                        450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, max.x),
                      c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                        seq(1000, 1900, 100), max.x),
                      c(min.x, 90, 100, 120, 140, 170,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                        seq(1000, 1900, 100), max.x),
                      c(min.x, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000,
                        1100, 1200, 1300, 1400, max.x),
                      c(min.x, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000,
                        1100, 1200, 1300, 1400, max.x),
                      c(min.x, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000,
                        1100, 1200, 1300, 1400, max.x),
                      c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                        seq(1000, 1900, 100), max.x),
                      c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                        200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                        seq(1000, 1900, 100), max.x),
                      c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                        200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1100, max.x))
  }else{  # for metric
    x.at    <- switch(ineq,
                      c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                        4400, 4600, 4800, max.x),
                      c(min.x, 120, 150, 170, 200, 230, 260, 300, 350, 400,
                        500, 600, 700, 850, 1000, 1250, 1500, 1750, 2000, 2250, max.x),
                      c(min.x, 100, 125, 150, 170, 200, 230, 260, 300, 350, 400, 500,
                        600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000,2250, max.x),
                      c(min.x, 120, 150, 170, 200, 230, 260, 300, 350, 400, 500, 600,
                        700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, max.x),
                      c(min.x, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800,
                        900, 1000, 1250, 1500, 1750, 2000, 2250, max.x),
                      c(min.x, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900,
                        2000, 2100, 2200, 2300, 2400, max.x),
                      c(min.x, 150, 175, 200, 225, 250, 300, 350, 400, 450, 500,
                        550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1100, 1200, 1300,
                        1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300,
                        2400, max.x),
                      c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                        4400, 4600, 4800, max.x),
                      c(min.x, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000,
                        1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000,
                        3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, max.x),
                      c(min.x, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, max.x),
                      c(min.x, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, max.x),
                      c(min.x, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, max.x),
                      c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                        4400, 4600, 4800, max.x),
                      c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                        900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                        seq(2200,max.x,200)),
                      c(min.x, 120, 150, 170, 200, 230, 260, 300, 350, 400, 500, 600,
                        700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, max.x) )
  }
  
  # This is the list of annotations for x-axis (levels of tpa)
  if(!use.metric){
    xaxl    <- switch(ineq,
                      x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #1
                      x.at[c(seq(1,length(x.at)-2,2), length(x.at))],                       #2
                      x.at[c(seq(1, 11,2), 13, 16, 19, length(x.at))],                      #3
                      x.at[c(1, seq(3,length(x.at)-4,2), length(x.at))],                    #4
                      x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],                    #5
                      x.at[c(seq(1,8,2), 11, 16, 21, 26, 31, length(x.at))],                #6
                      x.at[c(1, 2,  4, 6, 10, 14, 18, 22, 26, length(x.at))],               #7
                      x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #8
                      x.at[c(1, 3, 5, 7, 9, 11, 13, 15, 18, 23, length(x.at))],             #9
                      x.at[c(1, 3,  5, 7, 10, 12, 14, 16, 17, 19, 21,  length(x.at))],    #10
                      x.at[c(1, 3,  5, 7, 10, 12, 14, 16, 17, 19, 21,  length(x.at))],    #11
                      x.at[c(1, 3,  5, 7, 10, 12, 14, 16, 17, 19, 21,  length(x.at))],    #12
                      x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #13
                      x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #14
                      x.at[c(1, seq(3,length(x.at)-4,2), length(x.at))] )                   #15
  } else{
    xaxl    <- switch(ineq,
                      x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],        #1
                      x.at[c(seq(1,length(x.at)-2,2), length(x.at))],                  #2
                      x.at[c(seq(1, 11, 2), 13, 16, 19, length(x.at))],                #3
                      x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],               #4
                      x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],               #5
                      x.at[c(seq(1, 10, 2), 13, 18, length(x.at))],                    #6
                      x.at[c(1, 2, 4, 6, 7, 9, 13, 17, 21, 26, 31, length(x.at))],     #7
                      x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],        #8
                      x.at[c(1, 3, 5, 7, 9, 11, 16, 21, length(x.at))],                #9
                      x.at[c(1, 2, 4, 6, 8, 10, 12, 17, 22, 27, length(x.at))],        #10
                      x.at[c(1, 2, 4, 6, 8, 10, 12, 17, 22, 27, length(x.at))],        #11
                      x.at[c(1, 2, 4, 6, 8, 10, 12, 17, 22, 27, length(x.at))],        #12
                      x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],        #13
                      x.at[c(1, 3, 5, 7, 9, 11, 14, 18, 23, 28, length(x.at))],        #14
                      x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))] )              #15
  }
  # tick marks on y-axis
  if(!use.metric){
    y.at    <- switch(ineq,
                      seq(from=min.y, to=max.y, by=1),                   #1
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #2
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #3
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #4
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #5
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #6
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #7
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #8
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #9
                      c(seq(from=min.y, to=12, by=0.5), 11:max.y),       #10
                      c(seq(from=min.y, to=12, by=0.5), 11:max.y),       #11
                      c(seq(from=min.y, to=12, by=0.5), 11:max.y),       #12
                      c(seq(from=min.y, to=16.0, by=0.5), 17:max.y),     #13
                      c(seq(from=min.y, to=16.0, by=0.5), 17:max.y),     #14
                      c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:22, max.y) ) #15
  } else{
    y.at    <- switch(ineq,
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #1
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #2
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #3
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #4
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #5
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #6
                      c(min.y:30, seq(from=33, to=72, by=3), 78, 84, 90, 96),    #7
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #8
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                 #9
                      c(min.y:18, seq(from=20, to=max.y, by=2)),                #10
                      c(min.y:18, seq(from=20, to=max.y, by=2)),                #11
                      c(min.y:18, seq(from=20, to=max.y, by=2)),                #12
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                #13
                      c(min.y:30, seq(from=33, to=max.y, by=3)),                #14
                      c(min.y:30, seq(from=33, to=max.y, by=3)) )               #15
  }
  
  # This is the list of annotations for y-axis (levels of qmd)
  if(!use.metric){ #English units
    yaxl   <- switch(ineq,
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #1
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #2
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #3
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #4
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #5
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #6
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #7
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #8
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #9
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #10
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #11
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #12
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #13
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #14
                     c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)) ) #15
  } else{ # metric units
    yaxl   <- switch(ineq,
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #1
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #2
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #3
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #4
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #5
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #6
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #7
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #8
                     c(seq(min.y, 36, 3), seq(42, to=max.y, by=6)),       #9
                     c(seq(min.y, 12, 1), seq(14, to=max.y, by=2)),       #10
                     c(seq(min.y, 12, 1), seq(14, to=max.y, by=2)),       #11
                     c(seq(min.y, 12, 1), seq(14, to=max.y, by=2)),       #12
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #13
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #14
                     c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)) )      #15
  }
  # vertical grid lines at:
  v.grid <- x.at[2:length(x.at)]
  
  # horizontal gridlines at:
  h.grid <- y.at[2:length(y.at)]
  
  #grid.lw <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
  
  # this is the tx[] array offset for starting the plot of max sdi line
  if(!use.metric){ #English units
    sdi.strt<- switch(ineq,
                      1,
                      7,
                      4,
                      4,
                      1,
                      4,
                      4,
                      4,
                      4,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      4)
  } else {
    sdi.strt<- switch(ineq,
                      2.54,
                      7,
                      4,
                      4,
                      1,
                      4,
                      4,
                      4,
                      4,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                      4)
  }
  
  # this is the adjustments on the upper limit annotation
  ul.loc<- switch(ineq,
                  c(2.21, 1.08, 0.94, 1.25),    #1
                  c(2.21, 1.08, 0.94, 1.25),    #2
                  c(2.41, 1.08, 0.94, 1.35),    #3
                  c(2.21, 1.08, 0.94, 1.25),    #4
                  c(2.51, 1.08, 0.94, 1.60),    #5
                  c(2.21, 1.08, 0.94, 1.25),    #6
                  c(2.21, 1.08, 0.94, 1.25),    #7
                  c(2.21, 1.08, 0.94, 1.25),    #8
                  c(2.21, 1.08, 0.94, 1.25),    #9
                  c(2.21, 1.08, 0.94, 1.25),   #10
                  c(2.21, 1.08, 0.94, 1.25),   #11
                  c(2.21, 1.08, 0.94, 1.25),   #12
                  c(2.21, 1.08, 0.94, 1.25),   #13
                  c(2.21, 1.08, 0.94, 1.25),   #14
                  c(2.21, 1.08, 0.94, 1.25) )  #15
  
  if(anyNA(sdi.lines)){
    if(!use.metric){ #English units
      sdi.lines<- switch(ineq,
                         sdi.lines,                           #1
                         c(50,  100, 150, 200, 250, 350 ),    #2
                         c(50,  100, 150, 200, 250, 325 ),    #3
                         c(50,  100, 150, 200, 250, 325 ),    #4
                         c(50,  100, 150, 200, 250, 300 ),    #5
                         c(50,  100, 150, 200, 300, 400 ),    #6
                         c(100, 150, 200, 300, 400, 500 ),    #7
                         c(100, 200, 300, 400, 500, 600 ),    #8
                         c(100, 200, 300, 400, 500, 600 ),    #9
                         c(100, 125, 175, 250, 350, 450 ),   #10
                         c(100, 125, 175, 250, 350, 450 ),   #11
                         c(100, 125, 175, 250, 350, 450 ),   #12
                         c(100, 200, 300, 400, 500, 600 ),   #13
                         c(100, 150, 200, 300, 400, 500 ),   #14
                         c(50,  100, 150, 200, 250, 300 ) )  #15
    } else{  # metric units
      sdi.lines<- switch(ineq,
                         sdi.lines,                             #1
                         c(125, 250,  375,  500,  625, 860  ),  #2
                         c(125, 250,  375,  500,  625, 800  ),  #3
                         c(125, 250,  375,  500,  625, 800  ),  #4
                         c(125, 250,  375,  500,  625, 740  ),  #5
                         c(125, 200,  300,  450,  700, 1000 ),  #6
                         c(250, 370,  500,  740, 1000, 1200 ),  #7
                         c(250, 500,  750, 1000, 1250, 1500 ),  #8
                         c(250, 500,  750, 1000, 1250, 1500 ),  #9
                         c(250, 310,  435,  620, 865,  1115 ),  #10
                         c(250, 310,  435,  620, 865,  1115 ),  #11
                         c(250, 310,  435,  620, 865,  1115 ),  #12
                         c(250, 450,  750, 1000, 1250, 1500 ),  #13
                         c(250, 375,  500,  750, 1000, 1250 ),  #14
                         c(125, 250,  375,  500,  625, 740 ) )  #15
      
    }
  }
  
  
  #check consistency between max sdi and sdi.lines
  if(max(sdi.lines >= max.sdi)){
    message("One or more sdi.lines exceeds max.sdi; image not rendered")
    return()
  }
  
  # upper limit of the mz in x dim
  ux.mz      <- switch(ineq, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x,
                       max.x, max.x, max.x, max.x, max.x)
  
  
  # these guide placement of the iso lines and annotation
  # What the hell are these? I don't remember how this works, mwr 5/22
  # well the last one is the point (x) at which the isoline gets drawn
  # for most situations
  # the first is vestigal, no longer in use
  # the second is measured in sdi english units only,
  # this is a bug because it does not account for metric
  if(!use.metric){ #English units
    isod.adj<- switch(ineq,
                      c(160, 300, 12, 5),   #1
                      c(360, 440, 12, 5),   #2
                      c(210, 400, 12, 5),   #3
                      c(330, 400, 12, 5),   #4
                      c(210, 300, 12, 5),   #5
                      c(210, 300, 12, 5),   #6
                      c(210, 300, 12, 5),   #7
                      c(210, 300, 12, 5),   #8
                      c(210, 400, 12, 5),   #9
                      c(210, 750, 12, 5),   #10
                      c(210, 750, 12, 5),   #11
                      c(210, 750, 12, 5),   #12
                      c(210, 1000, 12, 5),  #13
                      c(210, 750,  12, 5),  #14
                      c(330, 400, 12, 5) )  #15
  } else {
    isod.adj<- switch(ineq,
                      c(160, 300,  12, 5),     #1
                      c(360, 440,  12, 5),     #2
                      c(210, 990,  12, 5),     #3
                      c(330, 400,  12, 5),     #4
                      c(210, 740,  30.5, 5),   #5
                      c(210, 740,  30.5, 5),   #6
                      c(210, 1300, 31, 5),   #7
                      c(210, 740,  30.5, 5),   #8
                      c(210, 990,  30.5, 5),   #9
                      c(210, 1852, 30.5, 5),   #10
                      c(210, 1852, 30.5, 5),   #11
                      c(210, 1852, 30.5, 5),   #12
                      c(210, 2475, 30.5, 5),   #13
                      c(210, 1856, 30.5, 5),   #14
                      c(330,  990, 30.5, 5) )  #15
  }
  # this is an annotation vert adjustment for max sdi
  #                     1    2    3    4     5    6    7    8    9    10
  ulanny<-switch(ineq, 0.0, 0.0, 0.0, 0.0, -4.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0,  0.0)
  
  #adjustment for annotations on right side of graph
  sdra<- switch(ineq,
                ifelse(!use.metric, 1.150, 1.150),   #1
                ifelse(!use.metric, 1.150, 1.150),   #2
                ifelse(!use.metric, 1.150, 1.150),   #3
                ifelse(!use.metric, 1.150, 1.150),   #4
                ifelse(!use.metric, 1.150, 1.150),   #5
                ifelse(!use.metric, 1.150, 1.150),   #6
                ifelse(!use.metric, 1.150, 1.150),   #7
                ifelse(!use.metric, 1.150, 1.150),   #8
                ifelse(!use.metric, 1.150, 1.150),   #9
                ifelse(!use.metric, 1.150, 1.150),   #10
                ifelse(!use.metric, 1.150, 1.150),   #11
                ifelse(!use.metric, 1.150, 1.150),   #12
                ifelse(!use.metric, 1.150, 1.150),   #13
                ifelse(!use.metric, 1.150, 1.150),   #14
                ifelse(!use.metric, 1.150, 1.150) )  #15
  
  # lower sdi limit of the management zone
  lsd     <- switch(ineq,
                    ifelse(!is.na(mgt.zone[1]),          #1
                           round(mgt.zone[1]*max.sdi,0),
                           sdi.lines[3]),
                    ifelse(!use.metric,                  #2
                           150,
                           round(150*2.47, 0) ),
                    10* round((0.30*max.sdi/10), 0),     #3
                    10* round((0.20*max.sdi/10), 0),     #4
                    ifelse(!use.metric,                  #5
                           60,
                           round(60*2.47,0) ),
                    ifelse(!is.na(mgt.zone[1]),          #6
                           round(mgt.zone[1]*max.sdi,0),
                           sdi.lines[3]),
                    10* round((0.30*max.sdi/10), 0),     #7
                    10* round((0.30*max.sdi/10), 0),     #8
                    10* round((0.30*max.sdi/10), 0),     #9
                    ifelse(!is.na(mgt.zone[1]),          #10
                           round(mgt.zone[1]*max.sdi,0),
                           sdi.lines[3]),
                    ifelse(!is.na(mgt.zone[1]),          #11
                           round(mgt.zone[1]*max.sdi,0),
                           sdi.lines[3]),
                    ifelse(!is.na(mgt.zone[1]),          #12
                           round(mgt.zone[1]*max.sdi,0),
                           sdi.lines[3]),
                    10* round((0.35*max.sdi/10), 0),     #13
                    10* round((0.35*max.sdi/10), 0),     #14
                    10* round((0.40*max.sdi/10), 0) )    #15
  
  # upper sdi limit of the management zone
  usd     <- switch(ineq,
                    ifelse(!is.na(mgt.zone[2]),                         #1
                           round(mgt.zone[2]*max.sdi,0),
                           200 ),
                    ifelse(!use.metric,                                 #2
                           250,
                           618 ),
                    10 * round((0.55*max.sdi/10),0),                    #3
                    10 * round((0.60*max.sdi/10),0),                    #4
                    ifelse(!use.metric,                                 #5
                           round(max.sdi*(-0.36+0.01*bsi),0 ),
                           round(max.sdi*(-0.36+0.01*bsi/feet.to.m),0 ) ),
                    ifelse(!is.na(mgt.zone[2]),                         #6
                           round(mgt.zone[2]*max.sdi,0),
                           300 ),
                    10 * round((0.55*max.sdi/10),0),                    #7
                    ifelse(!is.na(mgt.zone[2]),
                           round(mgt.zone[2]*max.sdi,0),
                           440),                                        #8
                    10 * round((0.55*max.sdi/10),0),                    #9
                    ifelse(!is.na(mgt.zone[2]),                         #10
                           round(mgt.zone[2]*max.sdi,0),
                           300 ),
                    ifelse(!is.na(mgt.zone[2]),                         #11
                           round(mgt.zone[2]*max.sdi,0),
                           300 ),
                    ifelse(!is.na(mgt.zone[2]),                         #12
                           round(mgt.zone[2]*max.sdi,0),
                           300 ),
                    10 * round((0.55*max.sdi/10),0),                    #13
                    10 * round((0.55*max.sdi/10),0),                    #14
                    10 * round((0.55*max.sdi/10),0))                    #15
  
  ##########################################
  
  if( lsd>=usd || is.na(lsd) || is.na(usd)){
    message("Invalid management zone limits, figure failed to render")
    return()
  }
  
  if( !(reineke.term <= 2.0 && reineke.term >= 1.3)){
    message("Invalid Reineke slope. DMD failed to render")
    e.code<-3
    return(e.code)
  }
  if(!use.metric){ # for English units
    if( !(max.sdi <= 1000 && max.sdi >300) ){
      message("Invalid Limiting SDI. DMD failed to render")
      return()
    }
  } else {         # for metric units
    if( !(max.sdi <= 2470 && max.sdi >740) ){
      message("Invalid Limiting SDI. DMD failed to render")
      return()
    }
  }
  
  # lower limit of the mgt zone in x dim was at line 403; moved on 5/22 mwr
  #lx.mz.1    <- lsd*(max.y/sdi.index)^(-(slp))
  lx.mz.1 <- switch(ineq,
                    min.x,                                     #1
                    min.x,                                     #2
                    min.x,                                     #3
                    min.x,                                     #4
                    min.x,                                     #5
                    min.x,                                     #6
                    min.x,                                     #7
                    min.x,                                     #8
                    min.x,                                     #9
                    lsd*(max.y/sdi.index)^(-(slp)),            #10
                    lsd*(max.y/sdi.index)^(-(slp)),            #11
                    lsd*(max.y/sdi.index)^(-(slp)),            #12
                    min.x,                                     #13
                    min.x,                                     #14
                    lsd*(max.y/sdi.index)^(-(slp)) )           #15
  
  #lx.mz.2    <- usd*(max.y/sdi.index)^(-(slp))
  lx.mz.2 <- switch(ineq,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    min.x,
                    usd*(max.y/sdi.index)^(-(slp)),
                    usd*(max.y/sdi.index)^(-(slp)),
                    usd*(max.y/sdi.index)^(-(slp)),
                    min.x,
                    min.x,
                    usd*(max.y/sdi.index)^(-(slp)) )
  
  # parameters for volume
  vty <- 1   # one solid, two dashed, three dotted
  vwd <- 1.5 # this is the line width
  vcex<- 0.75
  
  # parameters for dominant height
  hty <- 2  # two is for dashed line
  hwd <- 2  # this is the line width
  hcex<-0.75
  
  # various functions used
  # long and shaw volume function
  #ls2005.vol <- function(ttt, ddd){
  #  vvv <- -152 + 0.017*ttt*(ddd)^2.8
  #  return(vvv)
  #}
  # Ritchie and zhang volume function
  #rz2017.vol <- function(ttt, ddd){
  #  vvv <- (ttt^0.9648)*(exp(-3.8220-1.3538/sqrt(ttt)))*(ddd^2.7863)
  #  return(vvv)
  #}
  
  fhgrid<-function(h.grid,rt,mxsdi,mxx,mnx){
    ngrid<-length(h.grid)
    for(g in 1:ngrid){
      ulgh <- mxsdi*( h.grid[g]/sdi.index )^(-rt)
      if( ulgh > mxx ){
        graphics::segments(mnx,  h.grid[g], mxx, h.grid[g], lwd=0.2, col="grey")
      }
      else{
        graphics::segments(mnx,  h.grid[g], ulgh, h.grid[g], lwd=0.2, col="grey")
      }
    }
  }
  
  fvgrid<-function(v.grid,rt,mxsdi,mxy,mny){
    ng<-length(v.grid)
    for(g in 1:ng){
      ulgv <- ((mxsdi/v.grid[g])^(1/rt))*sdi.index
      if( ulgv > mxy){
        graphics::segments(v.grid[g], mny, v.grid[g],    mxy, lwd=gridlw, col=gridcol)
      }
      else{
        graphics::segments(v.grid[g], mny, v.grid[g],  ulgv, lwd=gridlw, col=gridcol)
      }
    }
  }
  
  
  
  ######################################################
  ######################################################
  ### Walter Meyer maximum line for CFVOL
  wmx<- c(45, 65, 100, 175, 400, 482, 600, 760, 1000, 1350, 1900)
  wmy<- c(30, 25,  20,  15,  10,   9,   8,   7,    6,    5,    4)
  ######################################################
  
  ############################Begin Plot #######################################################
  
  graphics::plot(NA,
                 frame.plot=FALSE,
                 main=main.t,
                 ylab="",
                 xlab="",
                 ylim=c(min.y*ylim.adj[1], max.y*ylim.adj[2]),
                 xlim=c(min.x*xlim.adj[1], max.x*xlim.adj[2]),
                 log="xy",
                 axes=FALSE,
                 asp=8/6)
  
  # draw x axis#
  graphics::axis(side=1,
                 at= x.at,
                 labels=FALSE,
                 pos=min.y,
                 cex=0.3, lwd=1.25)
  
  if(!use.metric){
    graphics::mtext(expression(paste("Trees Acre"^-1)),
                    side=1, line=-0.75, cex=1.0)
  }else{
    graphics::mtext(expression(paste("Trees Hectare"^-1)),
                    side=1, line=-0.75, cex=1.0)
  }
  
  for(jj in 1:(length(xaxl)-1)){
    graphics::text(xaxl[jj],
                   min.y*0.90,
                   paste(xaxl[jj]), cex=acex)
  }
  graphics::text(xaxl[length(xaxl)]*1.03,
                 min.y*0.90,
                 paste(xaxl[length(xaxl)]), cex=acex)
  
  # draw y axis ################################################################
  graphics::axis(side=2,
                 at = y.at,
                 labels= FALSE,
                 pos=min.x,
                 cex=0.25, lwd=1.25)
  
  if(!use.metric){
    graphics::mtext("Diameter (inches)      ",
                    side=2, line=-1.00, cex=1.0)
  }else{
    graphics::mtext("Diameter (cm)      ",
                    side=2, line=-1.00, cex=1.0)
  }
  # yaxis values
  for(jj in 1:length(yaxl)){
    graphics::text(min.x*0.81, yaxl[jj], as.character(yaxl[jj]), cex=acex)
  }
  
  # draw vertical and horizontal grids
  fvgrid(v.grid, slp, max.sdi, max.y, min.y)
  
  fhgrid(h.grid, slp, max.sdi, max.x, min.x)
  
  # now draw in the max sdi line and annotate #############################
  graphics::lines(tx[sdi.strt:length(tx)],
                  ((max.sdi/tx[sdi.strt:length(tx)])^islp)*sdi.index,
                  type="l", col=sdicol, lwd=sdilw)
  
  # maximum sdi annotation elements #######################################
  if(inul){
    yyloc <- max.y + ulanny
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc*ul.loc[2] ,
                   "Stand Density Index",      cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc      ,
                   paste("Upper Limit of", max.sdi), cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc*ul.loc[3] ,
                   paste("Reineke Value of", slp), cex=scex, col=sdicol)
    if(inply){
      graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1],
                     yyloc*ul.loc[3]*0.92,
                     paste("UMZ of ", usd), cex=scex, col="darkgrey")
    }
    
    graphics::segments( (max.sdi*(yyloc/sdi.index)^(-slp)),           yyloc,
                        (max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[4], yyloc,
                        lwd=1, col=sdicol)
  }
  
  # now draw mgt zone box for the management zone if inply = 1 ###########
  if(inply){
    
    mzx <- c( lx.mz.1, lx.mz.2, ux.mz, ux.mz )  #x-coords for mz polygon
    mzy <- c(sdi.index*( lsd / mzx[1] )^(islp),
             sdi.index*( usd / mzx[2] )^(islp),
             sdi.index*( usd / mzx[3] )^(islp),
             sdi.index*( lsd / mzx[4] )^(islp) )  #y-coords for mz polygon
    mzw <- 2          # linewidth for mzbox
    
    graphics::polygon( x=mzx, y=mzy, density=NA,
                       col=grDevices::rgb(grDevices::col2rgb(mzcol)[1]/255,
                                          grDevices::col2rgb(mzcol)[2]/255,
                                          grDevices::col2rgb(mzcol)[3]/255, 0.50))
  }
  
  ###############################################################
  
  # make iso-density lines below the maximum if insdi = TRUE ########
  if(insdi){
    for(jsd in 1:length(sdi.lines)){
      isd  <- ((sdi.lines[jsd]/tx)^(islp))*sdi.index
      
      #find where line goes below the minimum
      if(isd[length(tx)] <= min.y) {irng <- which(isd <= min.y)[1]} # if the sdiline goes below ymin stop here
      else {irng <- length(tx)}    # default is draw the whole line
      #
      
      yyann <- isd[1] # just set this as the default, not really necessary
      
      if(isd[1] <= max.y * 1.08){  # if it starts no more than 1.08 times the max y
        graphics::lines(tx[1:irng], isd[1:irng],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.66 * min.x
        yyann <- isd[1]
        if(insdl){                # for drawing short line on left side of graph
          graphics::segments(0.87*min.x, yyann,
                             min.x, yyann,
                             col=sdicol, lwd=sdilw)
        }
      } else if(sdi.lines[jsd]>=isod.adj[2]){
        graphics::lines(tx[isod.adj[3]:length(tx)],
                        isd[isod.adj[3]:length(tx)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.90 * tx[isod.adj[3]]
        yyann <- 1.00 * isd[isod.adj[3]]
      } else{
        strtln.x <- trunc(sdi.lines[jsd]*(max.y/sdi.index)^(-(slp)) - (min.x)*1.0)
        graphics::lines(tx[strtln.x:length(tx)],
                        isd[strtln.x:length(tx)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.90 * tx[strtln.x]
        yyann <- 1.05 * isd[strtln.x]
      }
      
      if( (insdl) && (yyann <= (max.y*1.15))){
        graphics::text(xxann,
                       yyann,
                       paste(sdi.lines[jsd]),
                       cex=scex, col=sdicol )
      }
    }
  }
  if(insdr){
    # sdi values annotation on the right of graph
    graphics::text(max.x*sdra,
                   1.1*sdi.index*(max.sdi/max.x)^islp,
                   "SDI", cex=scex, col=sdicol)
    graphics::text(max.x*sdra,
                   sdi.index*(max.sdi/max.x)^islp,
                   paste(max.sdi),           cex=scex, col=sdicol)
    for(iii in 1:6){
      graphics::text(max.x*sdra,
                     sdi.index*(sdi.lines[iii]/max.x)^islp,
                     as.character(sdi.lines[iii]), cex=scex, col=sdicol)
    }
  }
  #######################################################
  # volume iso lines:
  if(invol){
    if(!use.metric){
      vol.levels<-switch(ineq,
                         NULL,
                         c(200, 400, 600, 800, 1000, 1500, 2000,
                           3000, 4000, 5000, 6000, 7000, 8000),
                         c(200, 400, 600, 800, 1000, 1500, 2000,
                           3000, 4000, 5000, 6000, 7000, 8000),
                         NULL,
                         NULL,
                         c(200, 400, 600, 800, 1000, 1500, 2000,
                           3000, 4000, 5000, 6000, 7000, 8000),
                         c(200, 400, 600, 800, 1000, 1500, 2000,
                           3000, 4000, 6000, 8000),
                         NULL,
                         c(50, 200, 400, 600, 800, 1000, 1500, 2000,
                           3000, 4000, 5000, 6000, 7000, 8000),
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         c(50, 200, 400, 600, 1000, 1500, 2000,
                           3000, 4000, 5000, 6000, 8000))
      dmd.iso(ineq,
              v.at=vol.levels,
              range.x=c(min.x, max.x),
              max.sdi=max.sdi,
              reineke.term=slp,
              show.vol=TRUE,
              v.ann=TRUE,
              use.metric=FALSE)
    }else{
      vol.levels<-switch(ineq,
                         NULL,
                         c(30, 45, 60, 75, 100, 150,
                           200, 250, 300, 400, 500, 600, 700, 800),
                         c(30, 45, 60, 75, 100, 150,
                           200, 250, 300, 400, 500, 600, 700, 800),
                         NULL,
                         NULL,
                         c(30, 45, 60, 75, 100, 150,
                           200, 250, 300, 400, 500, 600, 700, 800),
                         c(30, 45, 60, 75, 100, 150,
                           200, 300, 400, 600, 800, 1000),
                         NULL,
                         c(30, 45, 60, 75, 100, 150,
                           200, 300, 400, 500, 600, 700, 800),
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         c(30, 45, 60, 75, 150,
                           200, 300, 400, 500, 600, 800))
      dmd.iso(ineq,
              v.at=vol.levels,             #already set to english or metric
              range.x=c(min.x, max.x),     #already set to english or metric
              max.sdi=max.sdi,
              reineke.term=slp,
              show.vol=TRUE,
              v.ann=TRUE,
              use.metric=TRUE)
    }
  }
  return()
}


# dmdiso.r


dmd.iso<-function(ineq         = 3,
                  v.at         = NULL,
                  range.x      = NULL,
                  max.sdi      = NULL,
                  reineke.term = 1.60,
                  vty          = 2,
                  vcex         = 0.75,
                  vcol         = "blue",
                  show.vol     = TRUE,
                  v.ann        = TRUE,
                  use.metric   = FALSE){
  
  if(!(ineq %in% c(2, 3, 6, 7, 9, 15))){
    message("Invalid equation number provided to dmd.iso, iso-lines not rendered")
    return()
  }
  
  if(is.null(range.x)){
    message("Range of x values not specified by range.x for dmd.iso, iso-lines not rendered")
    return()
  }
  
  if(range.x[1]<=0 | range.x[2] <=0){
    message("Invalid range of x values, iso-lines not rendered ")
    return()
  }
  
  if(is.null(v.at) & show.vol){
    message("Volumes not specified by v.at for dmd.iso, iso-lines not rendered")
    return()
  }
  
  if(is.null(max.sdi) & (ineq %in% c(1, 6))){
    message("Limiting sdi not specified by max.sdi for dmd.iso, iso-lines not rendered")
    return()
  }
  # note I have put all ineq values in here even though we are limited to 2, 3, 6, 7, 9 and 15
  if(!use.metric){   # English Units
    max.sdi <- switch(ineq,
                      ifelse(max.sdi<=1000 && max.sdi>=300, max.sdi , 400),
                      450,
                      400,
                      410,
                      365,
                      ifelse(max.sdi<=600 && max.sdi>=450, max.sdi, 550),
                      600,
                      800,
                      700,
                      ifelse((max.sdi<=564 && max.sdi>=527), max.sdi, 527),
                      ifelse((max.sdi<=608 && max.sdi>=547), max.sdi, 547),
                      ifelse((max.sdi<=669 && max.sdi>=502), max.sdi, 502),
                      max.sdi,
                      max.sdi,
                      max.sdi)
  } else{            # metric units
    max.sdi <- switch(ineq,
                      ifelse(max.sdi<=2470 & max.sdi>=741, max.sdi , 988),
                      1112,
                      988,
                      1013,
                      902,
                      ifelse(!(max.sdi<=1482 & max.sdi>=1112), 1359, max.sdi),
                      1482,
                      1976,
                      1729,
                      ifelse((max.sdi<=1394 & max.sdi>=1302), max.sdi, 1302),
                      ifelse((max.sdi<=1502 & max.sdi>=1352), max.sdi, 1352),
                      ifelse((max.sdi<=1653 & max.sdi>=1240), max.sdi, 1240),
                      max.sdi,
                      max.sdi,
                      max.sdi)
  }
  
  # volume functions
  # for ineq=2
  vf2<-function(vvv, ttt){
    ivv <- (( 0.017*ttt )/( vvv  + 152 ))^(-1/2.8)
    return(ivv)
  }
  # for ineq=3 Jang et al. (2020)
  vf3<-function(vvv, ttt){
    #  ivv <- ( vvv*(ttt^(-.9648))*exp(3.8220+1.3538/sqrt(ttt)) )^(1/2.7863)
    ivv <- exp( log( vvv* ttt^(-1.0353))/3.0602  + 4.7412/3.0602 + (2.3196/sqrt(ttt))/3.0602)
    return(ivv)
  }
  
  # Long and shaw (2012)
  # for ineq=6
  vf6<-function(vvv, ttt){
    ivv <- ((vvv/0.007)*ttt^(-1.146))^(1/2.808)
    return(ivv)
  }
  
  # from Drew and Flewelling (1979) equation 4:
  # for ineq=7
  vf7<-function(vvv, ttt){
    mxdv<-exp(12.644)*(ttt)^-1.5
    ivv <- ((68.682*(vvv/ttt)-6.8084)^0.36716)*(1 - (0.32375*((vvv/ttt)/mxdv)^0.44709))
    return(ivv)
  }
  
  # from McCarter and Long (1986)
  # for ineq=9
  vf9<-function(vvv, ttt){
    ivv <- ((54.4*(vvv/ttt) + 5.14)^0.361)*((1 - 0.00759*ttt^0.446))
    return(ivv)
  }
  
  vf15<-function(vvv, ttt){
    ivv <- exp(1.830 + 0.2967*log(vvv/ttt) - 0.0655*log(ttt))
    return(ivv)
  }
  
  # dominant height functions
  #hf3<-function(hhh,ttt){
  #  hp <- c(  276.49514, -124.88647, -0.0335065, 0.0452437,  1.1606388 ) #height parms for ineq=3
  #  m1<-hp[1]+hp[2]/sqrt(ttt)
  #  m2<-hp[3]+hp[4]/sqrt(ttt)
  #  ihh<-(log(1-((hhh-4.5)/m1)^hp[5]))/m2
  #  return(ihh)
  #}
  
  #hf6<-function(hhh,ttt){
  #  hp <- c( -1.143, 0.679, -0.254, 1.062 ) #height parms for ineq=6
  #  m1 <- hp[2]*(ttt^hp[3])
  #  ihh <- (m1*hhh^(1/hp[5]))-hp[1]
  #  return(ihh)
  #}
  
  #hf7<-function(hhh,ttt){
  #  vvv <- exp(12.644-1.5*log(ttt)) #cubic foot per tree
  #  ihh <- (((vvv*hhh^-1.10319)-0.008695)/0.0007764)^(1/2.1987)
  #  return(ihh)
  #}
  
  #hf9<-function(hhh,ttt){
  #  mv1<-0.00396+0.000779
  #  vvv <- exp(12.644-1.5*log(ttt)) #cubic foot per tree
  #  ihh <- (((vvv*hhh^-1.10319)-0.008695)/0.0007764)^(1/2.1987)
  #  return(ihh)
  #}
  
  
  if(show.vol){
    
    v.array<-v.at
    
    range.x<-sort(range.x)
    tx<-range.x[1]:range.x[2]      #this tx is either metric or English
    
    islp<-1/reineke.term
    # array of volumes to be produced in cubic feet per acre
    v.array<-sort(v.array)
    for(k in 1:length(v.array)){
      if(!use.metric){    #for English units
        ivol<- switch(ineq,
                      NULL,
                      vf2(v.array[k], tx),
                      vf3(v.array[k], tx),
                      NULL,
                      NULL,
                      vf6(v.array[k], tx),
                      vf7(v.array[k], tx),
                      NULL,
                      vf9(v.array[k], tx),
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      vf15(v.array[k], tx))
        iaa  <- 10*(max.sdi/tx)^(islp)
        qq   <- sum((iaa-ivol)>0)
      }else{
        txe <- tx * 0.404686 #convert trees to back to English
        v.arraye <- v.array*35.3147*.404686 #Convert volume to English also
        #These Conversions are made to faclilitate enlish unit functions
        ivol<- switch(ineq,  # so ivol is metric units
                      NULL,
                      vf2(v.arraye[k], txe),
                      vf3(v.arraye[k], txe),
                      NULL,
                      NULL,
                      vf6(v.arraye[k], txe),
                      vf7(v.arraye[k], txe),
                      NULL,
                      vf9(v.arraye[k], txe),
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      vf15(v.arraye[k], txe))
        iaa  <- 10*((max.sdi*.404686)/txe)^(islp)  #iaa is also english units
        qq   <- sum((iaa-ivol)>0)  # count how many are less than the max.sdi
      }
      
      # now draw the volume iso line
      if(!use.metric){
        graphics::lines(tx[1:qq], ivol[1:qq], type="l", col=vcol, lwd=0.5, lty=vty )
      }else{
        graphics::lines(tx[1:qq], ivol[1:qq]*2.54, type="l", col=vcol, lwd=0.5, lty=vty )
      }
      
      # now annotate the volume
      if(k==length(v.array)){
        
        if(v.ann){
          if(!use.metric){
            graphics::text(range.x[2]*1.15, ivol[qq]*1.18,  "Volume", cex=vcex, col=vcol)
            graphics::text(range.x[2]*1.15, ivol[qq]*1.09,
                           expression("ft"^3~"acre"^-1), cex=vcex, col=vcol)
          }else{
            graphics::text(range.x[2]*1.15, 2.54*ivol[qq]*1.18,  "Volume", cex=vcex, col=vcol)
            graphics::text(range.x[2]*1.15, 2.54*ivol[qq]*1.09,
                           expression("m"^3~"ha"^-1), cex=vcex, col=vcol)
          }
        }
      }
      if(v.ann){
        if(!use.metric){
          graphics::text(range.x[2]*1.15,
                         ivol[qq], as.character(v.array[k]), cex=vcex, col=vcol)
        }else{
          graphics::text(range.x[2]*1.15,
                         2.54*ivol[qq], as.character(v.array[k]), cex=vcex, col=vcol)
        }
      }
      # now draw a segment linking the line and annotation if needed
      if(min( iaa ) < ivol[qq] ){
        if(!use.metric){
          graphics::segments(tx[qq],   ivol[qq], range.x[2]*0.98,
                             ivol[qq],  col=vcol, lwd=0.5,   lty=1)
        }else{
          graphics::segments(tx[qq],   2.54*ivol[qq],
                             range.x[2]*0.98,
                             2.54*ivol[qq],
                             col=vcol, lwd=0.5,   lty=1)
        }
      }
    }
  }
  
  return()
  
}


# dmdvolume.r

# This file handles calculation of volume, biomass, crown cover, and height
#
# the way I handled use.metric in here is as follows:
# I assume that the input is metric if use.metric=TRUE
# and English if use.metric=FALSE. Then since the functions are
# written in English, if metric input then convert to English,
# calculate volume as normal, then if metric convert back to a volume
# in metric units.       mwr    March 21 2018.
#

#					a0          a          b          c
#rz.beta.bio <-c(0.9936280, -7.2543293,  2.4742879, -0.2006977)

#	              	 b             c             d
#rz.beta.cc <-c(-0.0002236136,  1.2746535623,  0.9308677542)

#						               a0            a             b           c
#rz.cov.vol <-matrix(c( 0.0010595783, -0.007960152,  0.0005359328,  0.01472568,
#                      -0.0079601516,  0.061858773, -0.0049967689, -0.10403963,
#                       0.0005359328, -0.004996769,  0.0007618920,  0.00354421,
#                       0.0147256780, -0.104039627,  0.0035442099,  0.24614517), nrow=4, byrow=TRUE)

#		                 				a0             a             b             c
#rz.cov.bio <-matrix(c(  2.861185e-05, -0.0002138692,  1.237792e-05,  4.393887e-04,
#                       -2.138692e-04,  0.0016656694, -1.246945e-04, -3.066672e-03,
#                        1.237792e-05, -0.0001246945,  2.189819e-05,  5.837887e-05,
#                        4.393887e-04, -0.0030666722,  5.837887e-05,  8.179116e-03), nrow=4, byrow=TRUE)


#			               b            c            d
#rz.cov.cc <-matrix(c( 3.512070e-10, 3.006206e-07, 1.632807e-07,
#                      3.006206e-07, 2.810850e-04, 1.285552e-04,
#                      1.632807e-07, 1.285552e-04, 8.169589e-05), nrow=3, byrow=TRUE)


####################################################################################################
jrzheight<-function(tpa, qmd){ # Jang Ritchie and Zhang height function
  rz.beta.ht <-c(314.29288264,  17.46283433,  -0.02719311,   0.02898155,  1.50607209) # from Woong Song Jang
  hp <- rz.beta.ht      #height parms for R&Z i know this is redundant but it makes fn easier to read
  
  height <- 4.5 + (hp[1]+hp[2]*sqrt(tpa))*(1-exp(qmd*(hp[3]+hp[4]/sqrt(tpa))))^hp[5]
  
  return(height)
}

#ls2005height<-function(tpa, qmd){ # Long and Shaw (2005) height function
#  qtmp<-qmd
#  qtmp[qtmp<2.1]<-NA
#    height<-( (qtmp-2.07) / (202-200*tpa^0.0011) )^(1/0.64)
#  return(height)
#}   # took this function out because it is bogus

ls2012height<-function(tpa, qmd){ # Long and Shaw (2012) height function
  qtmp<-qmd
  qtmp[qtmp<1.2]<-NA
  height<-((-1.143+qtmp)/(0.679*tpa^-0.254))^1.062
  return(height)
}

McC1986height<-function(tpa, qmd){ # McCarter and Long (1986)
  mess1 <- ((1/54.4)*((( qmd/(1-0.00759*tpa^0.446) )^(1/0.361))-5.14))
  mess2 <- ( 0.00396 + 0.000779*qmd^2.27 )
  
  height <- (1/1.27)*(mess1^(1/0.916)/mess2)^(1/1.09)
  return(height)
}

Wl1994height<-function(qmd,mV){ # Williams (1994) in English units
  if(mV<=0){
    message("Invalid volume calculation, negative mean Vol")
    return()
  }
  mess1 <- (log(mV)/0.9749) - (-5.5943/0.9749) - ((1.8919*log(qmd))/0.9749)
  height<- exp(mess1)
  return(height)
}

################################################################################
dmd.volume<-function(ineq  = 2,
                     max.sdi=NULL,
                     tpa=NULL,
                     qmd=NULL,
                     ba=NULL,
                     use.metric=FALSE){
  
  # coefficients for Ritchie and Zhang pp functions (ineq=3)
  # These came from the fits done by Woong Song Jang August 22, 2019
  # They were fit using fixed effects after mixed-effects models were found inferior on independent data
  # height
  beta.ht03 <-c(314.29288264,  17.46283433,  -0.02719311,   0.02898155,  1.50607209)
  
  #					a0           a1            b0            b1             c
  cov.ht03 <- matrix(c(6050.3474793, 198.04600749,  4.807445e-01, -2.726396e-01, -5.6827222828,
                       198.0460075,   17.14930305,  2.220025e-02, -3.010459e-02, -0.1812731844,
                       0.4807445,    0.02220025,  4.312286e-05, -3.739720e-05, -0.0004778776,
                       -0.2726396,   -0.03010459, -3.739720e-05,  6.419454e-05,  0.0003236961,
                       -5.6827223,   -0.18127318, -4.778776e-04,  3.236961e-04,  0.0062340926), nrow=5, byrow=TRUE)
  
  ht.deriv03 <- deriv(mh40 ~ 4.5 + ( a0 + a1*sqrt(tpa)  ) * (1-exp((b0 + b1/sqrt(tpa))*qmd))^c,
                      c("a0","a1", "b0", "b1", "c"), function(a0, a1, b0, b1, c, stpa, qmd){} )
  
  ht.se_d03<-function(tpa, qmd){
    f.new <- ht.deriv03(beta.ht03[1],beta.ht03[2],beta.ht03[3],beta.ht03[4],beta.ht03[5], tpa, qmd)
    g.new <- attr(f.new,"gradient")
    GS=rowSums((g.new%*%cov.ht03)*g.new)
    GS[GS<=0]<-NA # this converts any negative values to NA so the sqrt does not barf
    sqrt(GS)
  }
  
  ht.fun03 <- function(tpa,qmd) {
    4.5 + ( beta.ht03[1] + beta.ht03[2]*sqrt(tpa)  ) * (1-exp((beta.ht03[3] + beta.ht03[4]/sqrt(tpa))*qmd))^beta.ht03[5]
  }
  
  # volume
  #				                     a0			        a			         b 			     c
  beta.vol03 <-        c( 1.035384,    -4.741209,     3.060285,     -2.319617 )
  
  
  cov.vol03 <- matrix(c( 0.0010595783, -0.007960152,  0.0005359328,  0.01472568,
                         -0.0079601516,  0.061858773, -0.0049967689, -0.10403963,
                         0.0005359328, -0.004996769,  0.0007618920,  0.00354421,
                         0.0147256780, -0.104039627,  0.0035442099,  0.24614517), nrow=4, byrow=TRUE)
  
  vol.deriv03 <- deriv(mvol ~ (tpa^a0)* exp( ( a  ) +  b * log(qmd) + c/sqrt(tpa) ),
                       c("a0", "a", "b", "c"), function(a0, a, b, c, stpa, qmd){} )
  
  vol.se_d03<-function(tpa, qmd){
    f.new <- vol.deriv03(beta.vol03[1],beta.vol03[2],beta.vol03[3],beta.vol03[4], tpa, qmd)
    g.new <- attr(f.new,"gradient")
    GS=rowSums((g.new%*%cov.vol03)*g.new)
    GS[GS<=0]<-NA # this converts any negative values to NA so the sqrt does not barf
    sqrt(GS)
  }
  
  vol.fun03 <- function(tpa, qmd){
    (tpa^beta.vol03[1])* exp( ( beta.vol03[2]  ) +  beta.vol03[3] * log(qmd) + beta.vol03[4]/sqrt(tpa) )
  }
  
  # biomass
  #            					          a0          a            b                c
  beta.bio03  <-        c(  0.9936280,   -7.2543293,    2.4742879,    -0.2006977)
  
  
  cov.bio03 <- matrix(c(2.861185e-05, -2.138692e-04,  1.237792e-05,  4.393887e-04,
                        -2.138692e-04,  0.0016656694, -1.246945e-04, -3.066672e-03,
                        1.237792e-05, -0.0001246945,  2.189819e-05,  5.837887e-05,
                        4.393887e-04, -3.066672e-03,  5.837887e-05,  8.179116e-03), nrow=4, byrow=TRUE)
  
  bio.deriv03 <- deriv(bio ~ (tpa^a0)* exp( ( a ) +  b * log(qmd) + c/sqrt(tpa)  ),
                       c("a0", "a", "b", "c"), function(a0, a, b, c, stpa, qmd){} )
  
  bio.se_d03<-function(tpa, qmd){
    f.new <- bio.deriv03(beta.bio03[1],beta.bio03[2],beta.bio03[3],beta.bio03[4], tpa, qmd)
    g.new <- attr(f.new,"gradient")
    GS=rowSums((g.new%*%cov.bio03)*g.new)
    GS[GS<=0]<-NA # this converts any negative values to NA so the sqrt does not barf
    sqrt(GS)
  }
  
  bio.fun03 <- function(tpa, qmd){
    (tpa^beta.bio03[1])* exp( ( beta.bio03[2] ) +  beta.bio03[3] * log(qmd) + beta.bio03[4]/sqrt(tpa) )
  }
  
  # crown cover
  #	              	 b             c             d
  beta.cc03 <-c(-0.0002236136,  1.2746535623,  0.9308677542)
  
  #    			               b            c            d
  cov.cc03 <-matrix(c( 3.512070e-10, 3.006206e-07, 1.632807e-07,
                       3.006206e-07, 2.810850e-04, 1.285552e-04,
                       1.632807e-07, 1.285552e-04, 8.169589e-05), nrow=3, byrow=TRUE)
  
  cc.deriv03 <- deriv(cc ~ 100 *(1 - exp(b*(qmd^c)*(tpa^d))),
                      c("b","c","d"), function(b, c, d, tpa, qmd){} )
  
  cc.se_d03<-function(tpa, qmd){
    f.new <- cc.deriv03(beta.cc03[1],beta.cc03[2],beta.cc03[3], tpa, qmd)
    g.new <- attr(f.new,"gradient")
    GS=rowSums((g.new%*%cov.cc03)*g.new)
    GS[GS<=0]<-NA # this converts any negative values to NA so the sqrt does not barf
    sqrt(GS)
  }
  
  
  cc.fun03 <- function(tpa, qmd){
    100 *(1 - exp(beta.cc03[1]*(qmd^beta.cc03[2])*(tpa^beta.cc03[3])))
  }
  
  # Drew and Flewelling volume
  dfvol<-function(tpa, qmd, max.sdi){ # Drew and Flewelling (1979) volume equation
    dfrd <- tpa*((qmd/10)^1.6)/max.sdi
    vol<- (1/68.682)*(6.8084+(qmd*(1-0.32375*dfrd^0.44709)^-1)^(1/0.36716))
    vol<-vol*tpa
    return(vol)
  }
  df1979height<-function(tpa, qmd, dfvol){ # Drew and Flewelling (1979) height
    height<- ((dfvol/tpa)/(0.008695+0.0007764*qmd^2.1987))^(1/1.10319)
    return(height)
  }
  
  if(!(ineq %in% c(2,3,6,7,9))){
    message(paste( "Error in dmd.volume, invalid input value for ineq:", ineq))
    return(NULL)
  }
  # first check to see if two NULL values are present
  if(is.null(tpa) & is.null(ba)){
    message("Both tpa and ba are NULL, invalid input to dmd.volume")
    return(NULL)
  }
  if(is.null(ba) & is.null(qmd)){
    message("Both ba and qmd are NULL, invalid input to dmd.volume")
    return(NULL)
  }
  if(is.null(tpa) & is.null(qmd)){
    message("Both tpa and qmd are NULL, invalid input to dmd.volume")
    return(NULL)
  }
  
  if(is.null(tpa)){tpa<-NA}
  if(is.null(qmd)){qmd<-NA}
  if(is.null(ba)){ba<-NA}
  
  # second error check the input values, no negative etc.
  if( sum(is.na(tpa))==length(tpa) ){
    if(length(qmd) != length(ba)){
      message("Invalid input to dmd.volume: missing values")
      return(NULL)
    } else if(sum(qmd<0, na.rm=TRUE)>0 | sum(ba<0, na.rm=TRUE)>0 ){
      message("Invalid negative values input to dmd.volume")
      return(NULL)
    } else {
      if(!use.metric){
        tpa<-(ba/(qmd*qmd))*576/pi
      }else{
        tpa<-(ba/(qmd*qmd))*40000/pi
      }
    }
  } else if( sum(is.na(qmd))==length(qmd) ) {
    if(length(tpa) != length(ba) | sum(tpa==0)<1){
      message("Invalid input to dmd.volume: missing values")
      return(NA)
    } else if(sum(tpa<0, na.rm=TRUE)>0 || sum(ba<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      if(!use.metric){
        qmd<-sqrt(ba*576/(tpa*pi))
      }else{
        qmd<-sqrt(ba*40000/(tpa*pi))
      }
    }
    
  } else if( sum(is.na(ba))==length(ba)) {
    if(length(tpa) != length(qmd)){
      message("Invalid input to dmd.volume missing values")
      return(NA)
    } else if (sum(tpa<0, na.rm=TRUE) >0 | sum(qmd<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      if(!use.metric){
        ba<-round(tpa*qmd*qmd*pi/576, 4)
      }else{
        ba<-round(tpa*qmd*qmd*pi/40000, 4)
      }
    }
  }
  # done with error checking, build the data frame
  if(!use.metric){
    tpa.ac   <- tpa
    qmd.in   <- qmd
    ba.ft2ac <- ba
    stands<-as.data.frame( cbind(tpa.ac, qmd.in, ba.ft2ac) )
  } else {
    tpa.ha  <- tpa
    qmd.cm  <- qmd
    ba.m2ha <- ba
    stands<-as.data.frame( cbind(tpa.ha, qmd.cm, ba.m2ha) )
  }
  # set the temporary variables in English:
  if(!use.metric){ #then it is already English
    tpae <- tpa
    qmde <- qmd
    bae  <- ba
    max.sdie<- max.sdi
  }else{           #convert to English
    tpae <- tpa*0.404686
    qmde <- qmd/2.54
    bae  <- ba*4.356
    max.sdie<- max.sdi*0.404686
  }
  # calculate volumes in cubic feet per acre, change to metric if directed by use.metric
  volume <- switch(ineq,
                   NA,
                   -152+0.017*tpae*qmde^2.8,
                   vol.fun03(tpa=tpae, qmd=qmde),
                   NA,
                   NA,
                   0.007*(tpae^1.146)*(qmde^2.808),
                   dfvol(tpae, qmde, max.sdie),
                   NA,
                   (tpae/54.4)*((( qmde/(1-0.00759*tpae^0.446) )^(1/0.361))-5.14),
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   0.022954*(qmde^3.370408)*tpae^1.220762)
  
  volume <- (volume+abs(volume))/2     #get rid of neg values
  vole<-volume
  if(!use.metric){
    stands$volume.ft3ac <- volume
  } else {
    stands$volume.m3ha <- volume/14.2913
  }
  
  # calculate vol s.e. and change to metric if directed by use.metric
  vol.se <- switch(ineq,
                   NA,                           #1. NULL
                   NA,                           #2. L&S (2005)
                   vol.se_d03(tpa=tpae, qmd=qmde), #3. R&Z (2018)
                   NA,                           #4. CE  (1988)
                   NA,                           #5. PC  (1992)
                   NA,                           #6. L&S (2012)
                   NA,                           #7. D&F
                   NA,                           #8. ZWF
                   NA,                           #9. McC (1986)
                   NA,                           #10
                   NA,                           #11
                   NA,                           #12
                   NA,                           #13
                   NA,                           #14
                   NA)                           #15
  
  if(!use.metric){
    stands$vol.se.ft3ac <- vol.se
  } else {
    stands$vol.se.m3ha  <- vol.se/14.2913
  }
  
  # calculate Dominant Height in feet, change to metric if directed by use.metric
  
  height <- switch(ineq,
                   NA,                           #1. NULL
                   NA,                           #2. L&S (2005)
                   ht.fun03(tpa=tpae, qmd=qmde), #3. JR&Z (2020)
                   NA,                           #4. CE  (1988)
                   NA,                           #5. PC  (1992)
                   ls2012height(tpa=tpae,
                                qmd=qmde),       #6. L&S (2012)
                   df1979height(tpa=tpae,
                                qmd=qmde,
                                dfvol=vole),     #7. D&F
                   NA,                           #8. ZWF
                   McC1986height(tpa=tpae,
                                 qmd=qmde),      #9. McC (1986)
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   Wl1994height(qmd=qmde,
                                mV=vole/tpae) )
  
  if(!use.metric){
    stands$height.ft <- height
  } else {
    stands$height.m  <- height*0.3048
  }
  # calculate dom height s.e. and change to metric if directed by use.metric
  height.se <- switch(ineq,
                      NA,                           #1. NULL
                      NA,                           #2. L&S (2005)
                      ht.se_d03(tpa=tpae, qmd=qmde), #3. JR&Z (2020)
                      NA,                           #4. CE  (1988)
                      NA,                           #5. PC  (1992)
                      NA,                           #6. L&S (2012)
                      NA,                           #7. D&F
                      NA,                           #8. ZWF
                      NA,                           #9. McC (1986)
                      NA,
                      NA,
                      NA,
                      NA,
                      NA,
                      NA)
  if(!use.metric){
    stands$height.se.ft <- height.se
  } else {
    stands$height.se.m  <- height.se*0.3048
  }
  
  
  # calculate Biomass in tons per acre, change to metric if directed by use.metric
  biomass <- switch(ineq,
                    NA,
                    NA,
                    bio.fun03(tpa=tpae, qmd=qmde),
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA)
  if(!use.metric){
    stands$biomass.Tonsac <- biomass
  } else {
    stands$biomass.Mgha   <- biomass*2.2417
  }
  
  # calculate biomass s.e. and change to metric if directed by use.metric
  
  biomass.se <- switch(ineq,
                       NA,                           #1. NULL
                       NA,                           #2. L&S (2005)
                       bio.se_d03(tpa=tpae, qmd=qmde), #3. JR&Z (2020)
                       NA,                           #4. CE  (1988)
                       NA,                           #5. PC  (1992)
                       NA,                           #6. L&S (2012)
                       NA,                           #7. D&F
                       NA,                           #8. ZWF
                       NA,                           #9. McC (1986)
                       NA,
                       NA,
                       NA,
                       NA,
                       NA,
                       NA)                           #15 Williams (1994)
  
  if(!use.metric){
    stands$biomass.se.Tonsac <- biomass.se
  } else {
    stands$biomass.se.Mgha   <- biomass.se*2.2417
  }
  
  
  # calculate Crown Cover %
  bae   <- (0.005454154*qmde*qmde*tpae)
  sdie  <- tpae*(qmde/10)^1.605
  ccpct <- switch(ineq,
                  NA,
                  NA,
                  cc.fun03(tpa=tpae, qmd=qmde),
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA)
  stands$ccpct  <- ccpct
  
  cc.se <- switch(ineq,
                  NA,                           #1. NULL
                  NA,                           #2. L&S (2005)
                  cc.se_d03(tpa=tpae, qmd=qmde), #3. JR&Z (2018)
                  NA,                           #4. CE  (1988)
                  NA,                           #5. PC  (1992)
                  NA,                           #6. L&S (2012)
                  NA,                           #7. D&F
                  NA,                           #8. ZWF
                  NA,                            #9. McC (1986)
                  NA,
                  NA,
                  NA,
                  NA,
                  NA,
                  NA)
  
  stands$ccpct.se  <- cc.se
  
  
  return(stands)
  
}


# gdmd.r


gdmd.view<-function(ineq         = 3,
                    inul         = TRUE,
                    inrd         = TRUE,
                    rdlabel      = TRUE,
                    inply        = TRUE,
                    inqmd        = TRUE,
                    inspace      = TRUE,
                    max.sdi      = NA,
                    umz          = NA,
                    lmz          = NA,
                    mgt.zone     = c(0.35,0.60),
                    reineke.term = 1.6,
                    bsi          = 90,
                    dcol         = "blue",
                    rdcol        = "black",
                    mzcol        = "lightgrey",
                    dmd.title    = " ",
                    use.metric   = FALSE){
  
  ineq         <- as.integer(ineq)
  max.sdi      <- as.numeric(max.sdi)
  mgt.zone     <- as.numeric(mgt.zone)
  bsi          <- as.numeric(bsi)
  reineke.term <- as.numeric(reineke.term)
  umz          <- as.numeric(umz)
  
  dcol         <- as.character(dcol)
  rdcol        <- as.character(rdcol)
  dmd.title    <- as.character(dmd.title)
  
  bae.to.bam   <- 0.2295681
  sdi.index    <- ifelse(!use.metric, 10, 25.4 )
  fk           <- ifelse(!use.metric, pi/576, pi/40000)   #Foresters constant
  feet.to.m    <- 0.3048
  max.ineq     <- 15
  #olap.warn    <- 0
  
  if(!(ineq %in% 1:max.ineq)) {
    message("Unacceptable value for argument ineq; Figure not Rendered")
    return()
  }
  # test for acceptable reineke term must be between 1.50 and 2.0
  if(!(reineke.term>=1.30 && reineke.term<=2.00)){
    message("Invalid argument reineke.term; must be between 1.50 and 2.00 diagram not rendered")
    return()
  }
  
  if(ineq==1 || ineq==6 || ineq==10 || ineq==11 || ineq==12){
    if(is.na(max.sdi)){
      message("Error: User must specify max.sdi if ineq=1, 6, 10, 11 or 12. DMD not rendered")
      return()
    }
    if(!use.metric){
      if(ineq==1 && !((max.sdi>=200) && (max.sdi<=1000))){
        message("sdi upper limit  for ineq=1 (200-1000 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==6 && !((max.sdi>=450) && (max.sdi<=600))){
        message("sdi upper limit for ineq=6 (450-600 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==10 && !((max.sdi>=527) && (max.sdi<=564))){
        message("sdi upper limit for ineq=10 (527-564 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==11 && !((max.sdi>=547) && (max.sdi<=608))){
        message("sdi upper limit for ineq=10 (547-608 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==12 && !((max.sdi>=502) && (max.sdi<=669))){
        message("sdi upper limit for ineq=10 (502-669 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==13 && !((max.sdi>=700) && (max.sdi<=1000))){
        message("sdi upper limit for ineq=11 (700-1000 TPA) invalid: DMD not rendered")
        return()
      }
      if(ineq==14 && !((max.sdi>=550) && (max.sdi<=700))){
        message("sdi upper limit for ineq=12 (550-700 TPA) invalid: DMD not rendered")
        return()
      }
      
    } else{
      if(ineq==1 && !((max.sdi>=494) && (max.sdi<=2470))){
        message("sdi upper limit for ineq=1 (494-2470 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==6 && !((max.sdi>=1112) && (max.sdi<=1483))){
        message("sdi upper limit for ineq=6 (1112-1483 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==10 && !((max.sdi>=1302) && (max.sdi<=1394))){
        message("sdi upper limit for ineq=10 (1302-1394 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==11 && !((max.sdi>=1352) && (max.sdi<=1502))){
        message("sdi upper limit for ineq=11 (1352-1502 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==12 && !((max.sdi>=1240) && (max.sdi<=1653))){
        message("sdi upper limit for ineq=12 (1240-1653 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==13 && !((max.sdi>=1730) && (max.sdi<=2470))){
        message("sdi upper limit for ineq=13 (1730-2470 TPHA) invalid: DMD not rendered")
        return()
      }
      if(ineq==14 && !((max.sdi>=1240) && (max.sdi<=1730))){
        message("sdi upper limit for ineq=14 (1240-1730 TPHA) invalid: DMD not rendered")
        return()
      }
      
    }
  }
  
  # test for acceptable site index must be between 70 and 110
  if(!use.metric){
    if(!(bsi>=70 && bsi<=110)){
      message("Invalid argument bsi; Barrett's SI must be between 70 and 110 ft, diagram not rendered")
      return()
    }
  } else {
    if(ineq==5 && !(bsi>=21 && bsi<=34 )){
      message("Invalid argument bsi; Barrett's SI must be between 21 m and 34 m, diagram not rendered")
      return()
    }
  }
  # test for acceptable mgt.zone input
  if(!(length(mgt.zone)==2)){
    message("mgt.zone must be vector of length 2, NA is acceptable")
    return()
  }
  if(!is.na(mgt.zone[1])){
    if(mgt.zone[1]> 0.45 || mgt.zone[1] < 0.15){
      message("unnaceptable lower limit of management zone: mgt.zone[1]")
      return()
    }
  }
  
  if(!is.na(mgt.zone[2])){
    if(mgt.zone[2]> 0.80 || mgt.zone[2] < 0.50){
      message("unnaceptable upper limit of management zone: mgt.zone[1]")
      return()
    }
  }
  
  # test for acceptable umz direct input
  if(is.na(mgt.zone[2])){
    if(is.na (umz)){
      message("Invalid argument umz; must not be NA")
      return()
    }
  }
  # test for acceptable umz direct input
  if(is.na(mgt.zone[1])){
    if(is.na (lmz)){
      message("Invalid argument lmz; must not be NA")
      return()
    }
  }
  # test for acceptable umz direct input
  if(is.na(mgt.zone[2])){
    if(umz > max.sdi){
      message("Invalid argument umz; must be less than max.sdi")
      return()
    }
  }
  # test for acceptable lmz direct input
  if(!is.na(lmz) && !is.na(umz)){
    if(lmz > umz){
      message("Invalid argument lmz; must be less than umz")
      return()
    }
  }
  
  axis4.off    <- ifelse(!use.metric, 1.1, 1.1)
  
  #mzcol        = "lightgrey"
  main.t<-dmd.title
  if(dmd.title==" "){
    main.t <- switch(ineq,
                     dmd.title,                                           #1
                     "Ponderosa Pine Long and Shaw (2005)",               #2
                     "Ponderosa Pine Jang et al. (2021)",                 #3
                     "Ponderosa Pine Edminster (1988)",                   #4
                     "Ponderosa Pine (Cochran 1992)",                     #5
                     "Mixed-Conifer (Long and Shaw 2012)",                #6
                     "Coastal Douglas-Fir (Long et al 1988)",             #7
                     "California White Fir (Zhang et al 2007)",           #8
                     "Lodgepole pine (McCarter and Long 1988)",           #9
                     "Spruce-Fir (Weiskittel and Woodall 2023)",         #10
                     "Red Spruce (Weiskittel and Woodall 2023)",         #11
                     "Balsam Fir (Weiskittel and Woodall 2023)",         #12
                     "Redwood/Douglas-fir (Ritchie and Berrill 2022)",   #13
                     "Douglas-fir/Redwood (Ritchie and Berrill 2022)",   #14
                     "Loblolly pine (Williams 1994)"               )     #15
  }
  #                        1      2     3     4     5     6     7     8      9    10
  #inrd    <- switch(ineq, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE )
  if(!use.metric){        #     1      2     3     4     5     6     7     8     9     10    11    12    13   14   15
    max.x   <- switch(ineq,   600,   600,  600,  600,  600,  600,  600,  800,  800,  1000, 1000, 1000, 1000, 800, 1000 )
    max.y   <- switch(ineq,   450,   350,  300,  350,  250,  450,  500,  700,  550,   450,  450,  450,  800, 600,  450)
  } else{
    max.x   <- switch(ineq,  1500,  1500, 1500, 1500, 1500, 1500, 1500, 2000, 2000,  2500, 2500, 2500, 2500, 2000, 2500 )
    max.y   <- switch(ineq,   150,    80,   70,   80,   60,  110,  120,  170,  130,   110,  110,  100,  200,  140,  110 )
  }
  
  if(!use.metric){
    max.sdi <- switch(ineq,
                      ifelse(max.sdi<=1000 && max.sdi>=300, max.sdi, 400),
                      450,
                      400,
                      410,
                      365,
                      ifelse(max.sdi<=600 && max.sdi>=450, max.sdi, 550 ),
                      600,
                      800,
                      700,
                      ifelse(max.sdi<=564 && max.sdi>=527, max.sdi, 527 ),
                      ifelse(max.sdi<=608 && max.sdi>=547, max.sdi, 547 ),
                      ifelse(max.sdi<=669 && max.sdi>=502, max.sdi, 502 ),
                      ifelse(max.sdi<=1000 && max.sdi>=700, max.sdi, 900),
                      ifelse(max.sdi<=700 && max.sdi>=500, max.sdi, 700),
                      400)
  } else {
    max.sdi <- switch(ineq,
                      max.sdi,
                      1110,
                      988,
                      1013,
                      901,
                      ifelse(!(max.sdi<=1482 && max.sdi>=1112), 1359, max.sdi),
                      1482,
                      1977,
                      1730,
                      ifelse(max.sdi<=1394 && max.sdi>=1302, max.sdi, 1302),
                      ifelse(max.sdi<=1502 && max.sdi>=1352, max.sdi, 1352),
                      ifelse(max.sdi<=1653 && max.sdi>=1240, max.sdi, 1240),
                      ifelse(max.sdi<=2470 && max.sdi>=1730, max.sdi, 2220),
                      ifelse(max.sdi<=1730 && max.sdi>=1240, max.sdi, 1730),
                      988)
  }
  
  slp     <- switch(ineq,
                    reineke.term,
                    1.6000,
                    1.7721,
                    1.66113,
                    1.7721,
                    1.600,
                    1.600,
                    1.500,
                    1.605,
                    1.605,
                    1.605,
                    1.605,
                    1.605,
                    1.605,
                    1.5053)
  
  mgt.zone <-switch(ineq,
                    mgt.zone,
                    c(0.35, 0.55),
                    c(0.25, 0.55),
                    c(0.30, 0.60),
                    c(0.20, 0.60),
                    c(0.35, 0.55),
                    c(0.35, 0.55),
                    c(0.20, 0.55),
                    c(0.20, 0.55),
                    mgt.zone,
                    mgt.zone,
                    mgt.zone,
                    c(0.35, 0.55),
                    c(0.35, 0.55),
                    c(0.40, 0.55))
  
  islp    <- 1/slp             # inverse of reineke term
  if(!use.metric){
    xr      <- c(-10, max.x+50)  # x range
    yr      <- c(  0, max.y+40)    # y range
  } else {
    xr      <- c(-25, max.x+124)  # x range
    yr      <- c(  0, max.y+10)    # y range
  }
  rdl   <- seq(from=0.2, to=1.0, by=0.1)      # relative density levels
  
  #set maximum diameter limit
  if(!use.metric){
    max.d<-switch(ineq,
                  24,
                  24,
                  24,
                  24,
                  24,
                  24,
                  24,
                  24,
                  20,
                  18,
                  18,
                  18,
                  30,
                  30,
                  24)
  } else {
    max.d<-switch(ineq,
                  60,
                  60,
                  60,
                  60,
                  60,
                  60,
                  60,
                  60,
                  50,
                  45,
                  45,
                  45,
                  75,
                  75,
                  60)
  }
  
  # set the diameter array for display
  if(!use.metric){
    if(ineq==15 || ineq==5){
      diso  <- c(3:11, seq(from=12, to=20, by=2), 24) #diameter lines
    }else{
      diso  <- c(3:11, seq(from=12, to=max.d, by=2)) #diameter lines
    }
  } else {
    if(ineq==15 || ineq==5){
      diso  <- c(seq(8,32,2), seq(from=35, to=50, by=5), 60) #diameter lines
    } else{
      diso  <- c(seq(8,32,2), seq(from=35, to=max.d, by=5)) #diameter lines
    }
  }
  
  # limits for the density lines
  frt   <- max(diso)
  tot   <- min(diso)
  eprd  <-array(0.00, dim=c(length(rdl),2))  # array for rd endpoints
  
  # markers for square spacing on the x-axis:
  if(!use.metric){
    space <- switch(ineq,
                    10:18,
                    10:18,
                    10:18,
                    10:18,
                    10:18,
                    10:18,
                    10:18,
                    c(8:16, 18),
                    c(8:16, 18),
                    c(8:14, 16, 18),
                    c(8:14, 16, 18),
                    c(8:14, 16, 18),
                    c(8:14, 16, 18),
                    c(8:14, 16, 18),
                    c(8:14,16))
  } else {
    space <- switch(ineq,
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(3.0,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,5.5,0.5),
                    seq(2.5,4.5,0.5))
  }
  
  # lower sdi limit of the management zone
  if(!use.metric){
    lsd     <- switch(ineq,
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      150,
                      10* round((0.30*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      60,
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      10* round((0.30*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      10* round((0.40*max.sdi/10), 0) )
  } else {
    lsd     <- switch(ineq,
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      34,
                      10* round((0.30*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      14,
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      10* round((0.30*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      10* round((0.20*max.sdi/10), 0),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                      10* round((0.40*max.sdi/10), 0))
  }
  # upper sdi limit of the management zone
  if(!use.metric){
    usd     <- switch(ineq,
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      250,
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.60*max.sdi/10),0),
                      round(max.sdi*(-0.36+0.01*bsi),0),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.55*max.sdi/10),0),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      10 * round((0.55*max.sdi/10),0) )
  } else {
    usd     <- switch(ineq,
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      57,
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.60*max.sdi/10),0),
                      round(max.sdi*(-0.36+0.01*bsi/feet.to.m),0),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.55*max.sdi/10),0),
                      10 * round((0.55*max.sdi/10),0),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                      10 * round((0.55*max.sdi/10),0) )
  }
  
  
  # bump of RD annotation on the y-axis (multiplied by rd percent)
  if(!use.metric){
    y.off <- 6
    y.bump  <- switch(ineq, 0.025, 0.025, 0.025, 0.025, 0.000, 0.0400, 0.069, 0.200, 0.063, 0.063, 0.063, 0.063, 0.120, 0.066, 0.050)
  } else {
    y.off <- 1.3
    y.bump  <- switch(ineq, 0.025, 0.010, 0.002, 0.015, 0.000, 0.0200, 0.044, 0.040, 0.025, 0.020, 0.030, 0.030, 0.030, 0.032, 0.010)
  }
  
  # bump the qmd label
  if(!use.metric){
    q.offy<-switch(ineq, 1.10, 1.09, 1.10, 1.10, 1.10, 1.10, 1.07, 1.07, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08)
  } else {
    q.offy<-switch(ineq, 1.08, 1.07, 1.08, 1.08, 1.08, 1.07, 1.06, 1.08, 1.06, 1.06, 1.06, 1.06, 1.06, 1.06, 1.08)
  }
  # location of x-axis tpa label increment
  x.ann.at<-switch(use.metric+1, 100, 200)
  
  #location of diameter annotations
  if(!use.metric){
    d.l <- switch(ineq,
                  c(11, 5.0, 50,   10.0, 0),
                  c(11, 5.0, 50,   10.0, 5),
                  c(11, 5.0, 50,   10.0, 0),
                  c(11, 5.0, 50,   10.0, 0),
                  c(11, 5.0, 50,    5.0, 0),
                  c(11, 5.0, 50,   10.0, 0),
                  c(11, 5.0, 50,   10.0, 5),
                  c(11, 5.0, 50,   13.5, 0),
                  c(11, 5.0, 50,   10.0, 0),
                  c(11, 5.0, 50,   8.02, 0),
                  c(11, 5.0, 50,   8.02, 0),
                  c(11, 5.0, 50,   8.02, 0),
                  c(11, 5.0, 50,   12.0, 5),
                  c(11, 5.0, 50,   10.0, 5),
                  c(11, 4.0, 50,   7.00, 7) )
  } else{
    d.l <- switch(ineq,
                  c(35, 1.02, 150,  3.02, 0),
                  c(35, 1.02, 150,  2.02, 10),
                  c(35, 1.02, 150,  2.02, 0),
                  c(35, 1.02, 150,  3.02, 0),
                  c(35, 1.02, 150,  2.02, 0),
                  c(35, 1.02, 150,  3.02, 0),
                  c(35, 1.02, 150,  3.02, 0),
                  c(35, 1.02, 150,  3.52, 0),
                  c(35, 1.02, 150,  3.52, 0),
                  c(35, 1.02, 170,  2.02, 0),
                  c(35, 1.02, 150,  2.02, 0),
                  c(35, 1.02, 150,  2.02, 0),
                  c(35, 1.02, 150,  4.02, 10),
                  c(35, 1.02, 150,  1.02, 10),
                  c(35, 1.02, 150,  2.60, 20) )
  }
  ###############################################################################################################
  ###############################################################################################################
  # now draft the plot
  graphics::plot(NA,
                 frame.plot=FALSE,
                 main=main.t,
                 ylab="",
                 xlab="",
                 ylim=yr,
                 xlim=xr,
                 axes=FALSE)
  
  #AXIS Block
  # draw x axis
  if(!use.metric){
    graphics::axis(side=1,
                   at= seq(0, max.x*1.1, by=10),
                   labels=FALSE,
                   pos=0,
                   cex=0.3,
                   tck=+0.01,
                   lwd=1.25)
  } else {
    graphics::axis(side=1,
                   at= seq(0, max.x*1.1, by=25),
                   labels=FALSE,
                   pos=0,
                   cex=0.3,
                   tck=+0.01,
                   lwd=1.25)
  }
  if(!use.metric){
    graphics::mtext( expression("Trees Acre"^-1), side=1, line=0.80, cex=1.2, font=2)
  } else {
    graphics::mtext( expression("Trees Ha"^-1), side=1, line=0.80, cex=1.2, font=2)
  }
  for(i in seq(from=0, to=max.x, by=x.ann.at)){
    graphics::mtext(side=1, at=i, paste(i), cex=1.0, line=-0.8)
  }
  #Enhance tick marks for axis 1
  if(!use.metric){
    for(i in seq(from=0, to=max.x, by=x.ann.at)){
      graphics::segments(i, 0, i, 8, lwd=1.25)
    }
  } else {
    for(i in seq(from=0, to=max.x, by=x.ann.at)){
      graphics::segments(i, 0, i, 3, lwd=1.25)
    }
  }
  # add square spacing to x axis
  if(inspace){
    #  atspc<-ifelse(!use.metric, 43560/(space*space),10000/(space*space))
    if(!use.metric){
      atspc <- 43560/(space*space)
    } else {
      atspc <- 10000/(space*space)
    }
    
    for(i in 1:length(space)){
      if(!use.metric){
        graphics::segments(atspc, -.013*max.y, atspc, +.011*max.y, lwd=1.0, col="red")
      } else{
        graphics::segments(atspc, -.013*max.y, atspc, +.011*max.y, lwd=1.0, col="red")
      }
      
      graphics::mtext(side=1, at=atspc[i], paste(space[i]), cex=0.8, col="red", line=-0.1)
    }
    #  if(!use.metric) {graphics::mtext(side=1, at=50, paste("Square"), cex=0.8, col="red", line=-0.75)}
    if(!use.metric){
      graphics::mtext(side=1, at=50, paste("Spacing (ft):"), cex=0.8, col="red", line=-0.1)
    } else {
      graphics::mtext(side=1, at=70, paste("Spacing (m):"), cex=0.8, col="red", line=-0.1)
    }
    # think we need fix here so labels show up; see line 542
  }
  
  ## draw y axis 2
  if(!use.metric){
    graphics::axis(side=2,
                   at = seq(0, max.y, by=10),
                   labels= FALSE,
                   pos=0,
                   cex=0.3,
                   tck=+0.012,
                   lwd=1.25)
  } else {
    graphics::axis(side=2,
                   at = seq(0, max.y, by=2),
                   labels= FALSE,
                   pos=0,
                   cex=0.3,
                   tck=+0.012,
                   lwd=1.25)
  }
  
  if(!use.metric){
    graphics::mtext(expression(paste("Basal Area (ft"^2," ac"^-1,")")),
                    side=2, line=+0.2, cex=1.2, font=2)
  } else {
    graphics::mtext(expression(paste("Basal Area (m"^2," ha"^-1,")")),
                    side=2, line=+0.2, cex=1.2, font=2)
  }
  
  if(!use.metric){
    axe<-seq(from=0, to=max.y, by=50)
  } else {
    axe<-seq(from=0, to=max.y, by=10)
  }
  
  # not sure what this does any more mwr 4/2024
  for(i in 1:length(axe)){
    graphics::text(x=-0.029*max.x, y=axe[i], paste(axe[i]), cex=1.0)
    if(i > 0){
      graphics::segments(0, axe[i], 0.020*max.x, axe[i], lwd=1.25)
    }
  }
  
  ## draw y-axis 4
  if(!use.metric){
    graphics::axis(side=4,
                   at = seq(0, max.y, by=10),
                   labels=FALSE,
                   pos=max.x*axis4.off,
                   cex=0.3,
                   tck=+0.012,
                   lwd=1.25)
  } else {
    graphics::axis(side=4,
                   at = seq(0, max.y, by=2),
                   labels=FALSE,
                   pos=max.x*axis4.off,
                   cex=0.3,
                   tck=+0.012,
                   lwd=1.25)
  }
  
  #Enhance tick marks for axis 4
  if(!use.metric){
    for(i in seq(from=0, to=max.y, by=50)){
      graphics::segments((max.x*axis4.off)*.98,  i, max.x*axis4.off,   i,  lwd=1.25)
    }
  } else {
    for(i in seq(from=0, to=max.y, by=10)){
      graphics::segments((max.x*axis4.off)*.98,  i, max.x*axis4.off,   i,  lwd=1.25)
    }
  }
  #make the management zone
  if(inply){
    for(mm in 1:length(mgt.zone)){
      #set sdi level for upper and lower threshold
      if(ineq==5 & mm==2){
        mzl <- ifelse(!use.metric,
                      max.sdi*(-0.36+0.01*bsi),
                      max.sdi*(-0.36+0.01*bsi/feet.to.m))
      } else {
        mzl <- max.sdi*mgt.zone[mm]
      }
      fff <- (mzl) * (frt/sdi.index)^(-slp) # set starting point
      ttt <- (mzl) * (tot/sdi.index)^(-slp)
      if(ttt <= max.x){
        tpa.ar<- seq(from=fff, to=ttt, by=10)
        tpa.ar<- c(tpa.ar,ttt)
        ba.ar <- fk*tpa.ar*(sdi.index*(mzl/tpa.ar)^islp)^2
      }else{
        tpa.ar<- seq(from=fff, to=max.x, by=5)
        tpa.ar<- c(tpa.ar,max.x)
        ba.ar <- fk*tpa.ar*(sdi.index*(mzl/tpa.ar)^islp)^2
      }
      if(mm==1){
        mzxl<-tpa.ar
        mzyl<-ba.ar
      }
      if(mm==2){
        mzxu<-rev(tpa.ar)
        mzyu<-rev(ba.ar)
      }
    }
    
    mzxa<-append(mzxl, mzxu)
    mzya<-append(mzyl, mzyu)
    graphics::polygon( x=mzxa, y=mzya, density=NA, border=NA,
                       col=mzcol)
    
    graphics::lines(x=mzxu, mzyu, lwd=1.5) # top of mz line
    olap.warn<-min(mzyu) # this sets a variable for annotate check
    # now fix that goofy little triangle at the end of the mz
    
    if(max(mzxl) < max.x){
      graphics::polygon(x= c(max(mzxl),                    max.x, max.x),
                        y= c(min(mzyl),     max.x*fk*min(diso)^2, min(mzyu)),
                        density=NA, border=NA,
                        col=mzcol)
    }
    # grDevices::rgb(0.1, 0.1, 0.1, 0.25)
  }
  # Draw relative density lines
  if(inrd){
    for(j in 1:length(rdl)){
      ird   <- rev(rdl)[j]
      fff   <- (max.sdi*ird) * (frt/sdi.index)^(-slp) # set starting point
      ttt   <- (max.sdi*ird) * (tot/sdi.index)^(-slp)
      if(ttt <= max.x){
        tpa.ar<- seq(from=fff, to=ttt, by=10)
        tpa.ar<- c(tpa.ar,ttt)
        ba.ar <- fk*tpa.ar*(sdi.index*(max.sdi*ird/tpa.ar)^islp)^2
      }else{
        tpa.ar<- seq(from=fff, to=max.x, by=5)
        tpa.ar<- c(tpa.ar,max.x)
        ba.ar <- fk*tpa.ar*(sdi.index*(max.sdi*ird/tpa.ar)^islp)^2
      }
      graphics::lines(x=tpa.ar,  y=ba.ar, lwd=0.8, col=rdcol)
      eprd[j,1] <- rev(tpa.ar)[1] # save x-endpoint of each line
      eprd[j,2] <- rev(ba.ar)[1] # save y-endpoint of each line
    }
  }
  
  #next do the diameter lines.
  ar.n<-array(0.00,dim=c(length(diso),2))  # x array vars for diameter iso lines
  ar.b<-array(0.00,dim=c(length(diso),2))  # y array vars for diameter iso lines
  # this next block of code uses a layering of lines to get the effect of different
  # line widths and line types on the diameter lines.
  for(i in 1:length(diso)){
    if(!use.metric){
      ar.n[i,] <- c( min(max.sdi*0.2*(diso[i]/sdi.index)^(-slp), 100),
                     max.sdi*(diso[i]/sdi.index)^(-slp) )
      if( ar.n[i,2] > max.x ) {
        ar.n[i,2] <- max.x
      }
    } else {
      ar.n[i,] <- c( min(max.sdi*0.2*(diso[i]/sdi.index)^(-slp), 300),
                     max.sdi*(diso[i]/sdi.index)^(-slp) )
      if( ar.n[i,2] > max.x ) {
        ar.n[i,2] <- max.x
      }
    }
    ar.b[i,] <- ar.n[i,] * diso[i]*diso[i]*fk
    
    # draw heavy line if beginning or end
    if(i==1 | i==length(diso)){
      graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=2, col=dcol, lty=1) #heavy line
    }
    # then draw dotted line
    graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=1, col=dcol, lty=3) # dotted line
    # then draw solid line
    t.n<-ar.n[i,]
    t.b<-ar.b[i,]
    if(ar.n[i,2]>=max.x){
      t.n[2]<-ar.n[i,2]*0.92
      t.b[2]<-ar.b[i,2]*0.92
    }
    graphics::lines(x=t.n, y=t.b, lwd=1, col=dcol, lty=1)
    
    # insert annotations for diameter lines
    if(ar.n[i,2]>= max.x){       # annotations on side
      graphics::text(ar.n[i,2]+d.l[1],
                     ar.b[i,2]+d.l[2],
                     paste(diso[i]), col=dcol, cex=0.82)
    }else{
      if(ar.n[i,2]<= max.x-d.l[3]){ # annotations along the top
        graphics::text(ar.n[i,2]+d.l[5],
                       ar.b[i,2]+d.l[4],
                       paste(diso[i]), col=dcol, cex=0.82)
      }
    }
  }
  
  # Caption for diameters
  if(inqmd){
    if(!use.metric){
      graphics::text(ar.n[length(diso),2], ar.b[length(diso),2]*q.offy,
                     "QMD (inches)", col=dcol)
    } else {
      graphics::text(ar.n[length(diso),2], ar.b[length(diso),2]*q.offy,
                     "QMD (cm)", col=dcol)
    }
    graphics::segments(max.x, (min(diso)^2)*fk*max.x,
                       max.x, max.x*fk*(sdi.index*(max.sdi/max.x)^islp)^2,
                       lwd=1.5)
  }
  
  if(inul){
    graphics::text(max.x*.90, max.y*1.05, paste("Reineke Value= ", round(slp,3) ), col=rdcol)
    graphics::text(max.x*.90, max.y*1.00, paste("Max. SDI=", max.sdi), col=rdcol)
    graphics::text(max.x*.90, max.y*0.95, paste("UMZ=", round(mzl,0)), col=rdcol)
  }
  #Annotate RD lines
  if(rdlabel){
    for(j in 1:length(rdl)){
      rdpct<-100*rev(rdl)[j]
      yspot<-eprd[j,2]+y.off+y.bump*rdpct
      if(!(yspot < olap.warn*1.1 & yspot > olap.warn*0.90)){
        graphics::text(eprd[j,1]*0.96,
                       eprd[j,2]+y.off+y.bump*rdpct,
                       paste0(rdpct,"%"), cex=0.80, col=rdcol)
      }
    }
    if(ineq==15){
      graphics::text(max.x*0.96,
                     1.21*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                     "Relative", cex=0.80, col=rdcol)
      
      graphics::text(max.x*0.96,
                     1.12*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                     "Density",  cex=0.80, col=rdcol)
      
    }else{
      graphics::text(max.x*0.96,
                     1.16*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                     "Relative", cex=0.80, col=rdcol)
      
      graphics::text(max.x*0.96,
                     1.09*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                     "Density",  cex=0.80, col=rdcol)
    }
  }
  
}
