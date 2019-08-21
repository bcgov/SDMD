## This global.r contains mainly the procedure to calculate standard errors for 
## means of predicted Ht20, volume, biomass, and Canopy cover.
## Note that all units are based on English unit


beta.ht <-c(314.29288264,  17.46283433,  -0.02719311,   0.02898155,  1.50607209)

#				a0			a			b			c
beta.vol <-c( 1.035384, -4.741209,  3.060285, -2.319617 )

#					a0          a          b          c
beta.bio <-c(0.9936280, -7.2543293,  2.4742879, -0.2006977)

#	         	 b             c             d
beta.cc <-c(-0.0002236136,  1.2746535623,  0.9308677542)



#					a0           a1            b0            b1             c
cov.ht<-matrix(c(6050.3474793, 198.04600749,  4.807445e-01, -2.726396e-01, -5.6827222828,
				  198.0460075,  17.14930305,  2.220025e-02, -3.010459e-02, -0.1812731844,
				    0.4807445,   0.02220025,  4.312286e-05, -3.739720e-05, -0.0004778776,
				   -0.2726396,  -0.03010459, -3.739720e-05,  6.419454e-05,  0.0003236961,
				   -5.6827223,  -0.18127318, -4.778776e-04,  3.236961e-04,  0.0062340926), nrow=5, byrow=TRUE)


#						a0            a             b           c
cov.vol <-matrix(c( 0.0010595783, -0.007960152,  0.0005359328,  0.01472568,
				   -0.0079601516,  0.061858773, -0.0049967689, -0.10403963,
				    0.0005359328, -0.004996769,  0.0007618920,  0.00354421,
				    0.0147256780, -0.104039627,  0.0035442099,  0.24614517), nrow=4, byrow=TRUE)
					
#						a0             a             b             c
cov.bio <-matrix(c(  2.861185e-05, -0.0002138692,  1.237792e-05,  4.393887e-04,
                     -2.138692e-04,  0.0016656694, -1.246945e-04, -3.066672e-03,
                     1.237792e-05, -0.0001246945,  2.189819e-05,  5.837887e-05,
                     4.393887e-04, -0.0030666722,  5.837887e-05,  8.179116e-03), nrow=4, byrow=TRUE)					
	


					
#			               b            c            d
cov.cc <-matrix(c( 3.512070e-10, 3.006206e-07, 1.632807e-07,
                   3.006206e-07, 2.810850e-04, 1.285552e-04,
                   1.632807e-07, 1.285552e-04, 8.169589e-05), nrow=3, byrow=TRUE)				




ht.deriv <- deriv(mh40 ~ 4.5 + ( a0 + a1*sqrt(stpa)  ) * (1-exp((b0 + b1/sqrt(stpa))*qmd))**c,
					c("a0","a1", "b0", "b1", "c"), function(a0, a1, b0, b1, c, stpa, qmd){} )
					
vol.deriv <- deriv(mvol ~ 0.001 *  (stpa**a0)* exp( ( a  ) +  b * log(qmd) + c/sqrt(stpa) ), 
					c("a0", "a", "b", "c"), function(a0, a, b, c, stpa, qmd){} )
					
bio.deriv <- deriv(bio ~ (stpa**a0)* exp( ( a ) +  b * log(qmd) + c/sqrt(stpa)  ), 
          c("a0", "a", "b", "c"), function(a0, a, b, c, stpa, qmd){} ) 

cc.deriv <- deriv(CC ~ 100 *(1 - exp(b*qmd**c*stpa**d)), 
					c("b","c","d"), function(b, c, d, stpa, qmd){} ) 


HT.SE_delta<-function(tpa_new, qmd_new){
f.new <- ht.deriv (beta.ht[1],beta.ht[2],beta.ht[3],beta.ht[4],beta.ht[5], tpa_new, qmd_new)
g.new <- attr(f.new,"gradient")
GS=rowSums((g.new%*%cov.ht)*g.new)
sqrt(GS)
}

VOL.SE_delta<-function(tpa_new, qmd_new){
f.new <- vol.deriv (beta.vol[1],beta.vol[2],beta.vol[3],beta.vol[4], tpa_new, qmd_new)
g.new <- attr(f.new,"gradient")
GS=rowSums((g.new%*%cov.vol)*g.new)
sqrt(GS)
}

BIO.SE_delta<-function(tpa_new, qmd_new){
f.new <- bio.deriv (beta.bio[1],beta.bio[2],beta.bio[3], beta.bio[4], tpa_new, qmd_new)
g.new <- attr(f.new,"gradient")
GS=rowSums((g.new%*%cov.bio)*g.new)
sqrt(GS)
}

CC.SE_delta<-function(tpa_new, qmd_new){
  f.new <- cc.deriv (beta.cc[1],beta.cc[2],beta.cc[3], tpa_new, qmd_new)
  g.new <- attr(f.new,"gradient")
  GS=rowSums((g.new%*%cov.cc)*g.new)
  sqrt(GS)
}



ht.fun <- function(tpa,qmd) {
  4.5 + ( 314.29288264 + 17.46283433*sqrt(tpa)  ) * (1-exp((-0.02719311 + 0.02898155/sqrt(tpa))*qmd))**1.50607209
}		

vol.fun <- function(tpa, qmd) {
  0.001 *  (tpa**1.035384)* exp( ( -4.741209  ) +  3.060285 * log(qmd) + -2.319617/sqrt(tpa) )
}

bio.fun <- function(tpa, qmd){
  (tpa**0.9936280)* exp( ( -7.2543293 ) +  2.4742879 * log(qmd) + -0.2006977/sqrt(tpa) )
}

cc.fun <- function(tpa, qmd){
  100 *(1 - exp(-0.0002236136*qmd**1.2746535623*tpa**0.9308677542))
} 



