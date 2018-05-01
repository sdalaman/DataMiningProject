library(MASS)
library(ISLR)
library(corrplot)
library("ggplot2")
library("plyr") 
library("car")
library(Hmisc)
library(pastecs)

rdata <- read.csv('work-file.csv', header = T)
attach(rdata)
nrow(rdata)
ncol(rdata)
names(rdata)
str(rdata)
summary(rdata$Number.of.rooms.per.person)
head(rdata)

describe(rdata)
stat.desc(rdata)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2),pch=19, col="black",type="p")

plot(Seq, Hopeful, main="Hopefull vs. Province",ylab= "Level",xlab="Province Seq")
abline(h=c(mean(Hopeful)), lwd=1.5, lty=2, col="red")
text(Province, Hopeful,Province,cex=0.6, pos=4, col="darkblue")

plot(Seq, Life.Expectancy.Total, main="Life Expectancy vs. Province", ylab= "Level",xlab="Provincev Seq")
abline(h=c(mean(Life.Expectancy.Total)), lwd=1.5, lty=2, col="red")
text(Province, Life.Expectancy.Total,Province,cex=0.6, pos=4, col="darkblue")

plot(Seq, Life.Satisfaction.Index, main="Life Satisfaction Index vs. Province",ylab= "Level",xlab="Province Seq")
abline(h=c(mean(Life.Satisfaction.Index)), lwd=1.5, lty=2, col="red")
text(Province, Life.Satisfaction.Index,Province,cex=0.6, pos=4, col="darkblue")

plot(Seq, Level.of.happiness, main="Happiness vs. Province", ylab= "Level",xlab="Province Seq")
abline(h=c(mean(Level.of.happiness)), lwd=1.5, lty=2, col="red")
text(Province, Level.of.happiness,Province,cex=0.6, pos=4, col="darkblue")
par(opar)


qplot(y = Life.Expectancy.Total, data = rdata, colour = 'blue')
bins <- cut(Life.Expectancy.Total, 3, include.lowest = TRUE, 
            labels = c('Low','Medium','High'))

scatterplotMatrix(rdata[3:5])

sapply(rdata[3:20],mean)

sapply(rdata[3:59],sd)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

standardisedconcentrations <- as.data.frame(scale(rdata[3:60]))
standardisedconcentrations
standardisedconcentrations$Life.Expectancy.Total

normalize <- function (value) 
{
  normalize = (value - min(value)) / (max(value) - min(value));
}

Life.Expectancy.Total.normalized <- normalize(Life.Expectancy.Total)

barplot(Life.Expectancy.Total)
title(main = "Life Expectancy", font.main = 4)

opar <- par(no.readonly=TRUE)
par(mfrow=c(3,1),pch=19, col="black",type="p")
hist(Life.Expectancy.Total,main = "Life Expectancy(Total)", font.main = 4)
hist(Life.Expectancy.Male,main = "Life Expectancy(Male)", font.main = 4)
hist(Life.Expectancy.Female,main = "Life Expectancy(Female)", font.main = 4)
par(opar)

opar <- par(no.readonly=TRUE)
par(mfrow=c(3,1),pch=19, col="black",type="p")
hist(Level.of.happiness,main = "Level of happiness", font.main = 4)
hist(Hopeful,main = "Hopeful", font.main = 4)
hist(Life.Satisfaction.Index,main = "Life Satisfaction Index", font.main = 4)
par(opar)


opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2),col="black",cex.lab = 1.5, cex.main = 1.4)
#par(las=2)
minx <- min(rdata$Murder.rate..per.million.people.)
maxx <- max(rdata$Murder.rate..per.million.people.)
odata <- rdata[order(rdata[,"Murder.rate..per.million.people."],decreasing=TRUE),]
hdata <- odata[1:5,c("Province","Murder.rate..per.million.people.")]
barplot(hdata[,2],names.arg=hdata[,1],horiz=FALSE,col="darkgreen",ylim=c(minx,maxx))
title(main = "Highest Murder Rates", font.main = 4)
ldata <- odata[(nrow(odata)-5):nrow(odata),
               c("Province","Murder.rate..per.million.people.")]
barplot(ldata[,2],names.arg=ldata[,1],horiz=FALSE,col="darkgreen",ylim=c(minx,maxx))
title(main = "Lowest Murder Rates", font.main = 4)
par(opar)



opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2),col="black",cex.lab = 1.5, cex.main = 1.4)
#par(las=2)
odata <- rdata[order(rdata[,"Unemployment.rate"],decreasing=TRUE),]
miny <- min(odata$Unemployment.rate)
maxy <- max(odata$Unemployment.rate)
hdata <- odata[1:5,c("Province","Unemployment.rate")]
barplot(hdata[,2],names.arg=hdata[,1],horiz=FALSE,col="darkgreen")
title(main = "Highest Unemployment rate", font.main = 4)
ldata <- odata[(nrow(odata)-5):nrow(odata),
               c("Province","Unemployment.rate")]
barplot(ldata[,2],names.arg=ldata[,1],horiz=FALSE,col="darkgreen")
title(main = "Lowest Unemployment rate", font.main = 4)
par(opar)


library(gclus)
dta <- mtcars[c(1,3,5,6)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.o <- order.single(dta.r)
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal

cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

nda <- rdata[c(3:43)] # get data 
nda.r <- abs(cor(nda)) # get correlations
nda.o <- order.single(nda.r)
nda.col <- dmat.color(nda.r) # get colors

cpairs(nda, nda.o[1:5], panel.colors=nda.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

scatterplotMatrix(rdata[c(nda.o[1:5])])

library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(x, y, xbins=50) 
plot(bin, main="Hexagonal Binning")

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg, main="3D Scatterplot")
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

library(Rcmdr)
attach(mtcars)
scatter3d(wt, disp, mpg)


library(aplpack)
attach(mtcars)
bagplot(wt,mpg, xlab="Car Weight", ylab="Miles Per Gallon",
        main="Bagplot Example")


library(lattice) 
attach(mtcars)
splom(rdata[c(3,4,5,6)], 
      main="Data")

super.sym <- trellis.par.get("superpose.symbol")
attach(mtcars)
library(lattice)
splom(mtcars[c(1,3,5,6)], groups=cyl, data=mtcars,
      panel=panel.superpose, 
      key=list(title="Three Cylinder Options",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))


# Scatterplot Matrices from the car Package
library(car)
scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars,
                   main="Three Cylinder Options")



# First Correlogram Example
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

library(corrgram) 
col.corrgram <- function(ncol){   
  colorRampPalette(c("green", "blue",
                     "yellow", "red"))(ncol)} 
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
upper.panel=panel.pie, text.panel=panel.txt, 
cor.method="pearson",
main="Correlogram of Car Mileage Data (PC2/PC1 Order)")

library(corrgram) 
col.corrgram <- function(ncol){   
  colorRampPalette(c("darkgoldenrod4", "burlywood1",
                     "darkkhaki", "darkgreen"))(ncol)} 
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Correlogram of Car Mileage Data (PC2/PC1 Order)")


