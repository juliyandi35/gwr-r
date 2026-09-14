library(spgwr)

data(columbus)
attach(columbus)

colex0 <- lm(CRIME ~ (INC + HOVAL))
summary(colex0)

resid<-residuals(colex0)
par(mfrow=c(2,2))
qqnorm(resid); qqline(resid, col="red"); 
plot(resid~fitted(colex0),xlab = "Predicted Values",ylab = "Residuals")
abline(h=0, col="red")
hist(resid) #histogram utk residual
plot(1:nrow(columbus), resid, pch=20,type="b")
abline(h=0, col="red")

shapiro.test(resid) # Normality test

lmtest::bptest(colex0) # Heteroskedastisity test

library(spdep)
coords<-columbus[c("X","Y")]
jarak<-as.matrix(1/dist(coords))
lm.morantest(colex0,listw=mat2listw(jarak), alternative="two.sided") # Moran Index

## Spatially Dissagregated Model
colex <- lm(CRIME ~ (INC + HOVAL)*(X + Y))
summary(colex)

colex$coefficients

bihoval <- b[3] + b[8] * X + b[9] * Y
bihoval

summary(bihoval)

library(rgdal)
col.shp<-readOGR(dsn=“your directory", layer=“your shp filename")
col.shp@data$bi<-bihoval

spplot(col.shp, zcol="bi")

## Basic GWR
# Menentukan bandwidth optimal
library(GWmodel)
# convert to sp
columbus.sp = as(columbus, "Spatial")
# determine the kernel bandwidth
bw <- bw.gwr(CRIME ~ INC + HOVAL,
             approach = "AIC",
             adaptive = T,
             data=columbus.sp)

# Modelling
m.gwr <- gwr.basic(CRIME ~ INC + HOVAL,
                   adaptive = T,
                   data=columbus.sp,
                   bw = bw)

# Evaluation
summary(m.gwr)



