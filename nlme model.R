library(gstat)
dat.spat<-read.csv("/Users/mrcartlidge/Documents/Classes/Student/Dissertation_Hedonic/spatial_variogram_dataset.csv")
head(dat.spat)
attach(dat.spat)

coordinates(dat.spat)=~lat+long_

vargram<-variogram(log(sale_amoun)~1, dat.spat)
plot1<-plot(vargram)

vargram2<-variogram(log(sale_amoun)~pc1+pc2+pc3+pc4+pc5+pc6+pc7+factor(sale_yr_cat)+prairie_x, dat.spat)
plot2<-plot(vargram2)

#vgm needs : partial sill, model, range , nugget#
fit.vargram2<-fit.variogram(vargram2, vgm(0.02,"Exp",0.01,0.04))
plot(fit.vargram2)

#Mixed models approach to modeling selling price#
library(nlme)
dummy<-rep(1,nrow(dat.spat))

spat.mod<-lme(log(sale_amoun)~pc1+pc2+pc3+pc4+pc5+pc6+pc7+factor(sale_yr_cat)+prairie_x,
random=~1|dummy, correlation=corExp(c(0.01,0.04), form=~long_+lat, nugget=TRUE))

spat.mod2<-lme(log(sale_amoun)~pc1+pc2+pc3+pc4+pc5+pc6+pc7+factor(sale_yr_cat)+prairie_x,
random=~1|dummy, correlation=corSpher(1, form=~long_+lat))

spat.mod3<-lme(log(sale_amoun)~pc1+pc2+pc3+pc4+pc5+pc6+pc7+factor(sale_yr_cat)+prairie_x,
random=~1|dummy, correlation=corGaus(1, form=~long_+lat))