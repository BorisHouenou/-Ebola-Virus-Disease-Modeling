
#98% of Susceptible people will remain in the Susceptible state,2% infected, 0% recovered 
#50% of those who are Infected will move to the Recovered Category and 50% remain infected between successive timesteps. 
#95% of recovered stays recovered and 5% relapsed???
#g<-function(a){
#exp(-a)
# 1/(1+a)
#}
a<-seq(1,1000, by=.5)
gdata<-rep(0, length(a))
for(i in 1: length(a)){
  gdata[i]<-1/(1+a[i])
}
gdata
plot(a, gdata)
####SIR for Sierra Leone
library(markovchain)
mcSIRSL <- new("markovchain", states=c("S","I","R"),
                #transitionMatrix=matrix(data=c(0.98,0.02,0,0.1,0.4,0.5, 0,0.8,0.2),
               transitionMatrix=matrix(data=c(0.98,0.02,0,0,0.5,0.5,0, 0.05,0.95),
                                       
                                      byrow=TRUE, nrow=3), name="SIR")
initialState <- c(7000000,1,0)
show(mcSIRSL)

plot(mcSIRSL,package="diagram")

timesteps <- 61
sir.dfSL <- data.frame( "timestep" = numeric(),
                         "S" = numeric(), "I" = numeric(),"R" = numeric(),
                         stringsAsFactors=FALSE)
for (i in 0:timesteps) {
  newrow <- as.list(c(i,round(as.numeric(initialState * mcSIRSL ^ i),0)))
  sir.dfSL[nrow(sir.dfSL) + 1, ] <- newrow
}

head(sir.dfSL)

####SIRD for Guinea
mcSIRgn <- new("markovchain", states=c("S","I","R"),
               #transitionMatrix=matrix(data=c(0.98,0.02,0,0.1,0.4,0.5, 0,0.8,0.2),
               transitionMatrix=matrix(data=c(0.98,0.02,0,0,0.5,0.5,0, 0.05,0.95),  
                                     byrow=TRUE, nrow=3), name="SIR")
initialState <- c(12000000,1,0)
show(mcSIRgn)

plot(mcSIRgn,package="diagram")

timesteps <- 61
sir.dfgn <- data.frame( "timestep" = numeric(),
                        "S" = numeric(), "I" = numeric(),"R" = numeric(),
                        stringsAsFactors=FALSE)
for (i in 0:timesteps) {
  newrow <- as.list(c(i,round(as.numeric(initialState * mcSIRgn ^ i),0)))
  sir.dfgn[nrow(sir.dfgn) + 1, ] <- newrow
}

head(sir.dfgn)
####SIR for Liberia
mcSIRLB <- new("markovchain", states=c("S","I","R"),
               #transitionMatrix=matrix(data=c(0.98,0.02,0,0.1,0.4,0.5, 0,0.8,0.2),
               transitionMatrix=matrix(data=c(0.98,0.02,0,0,0.5,0.5,0, 0.05,0.95), 
                                      byrow=TRUE, nrow=3), name="SIR")
initialState <- c(4700000,1,0)
show(mcSIRLB)

plot(mcSIRLB,package="diagram")

timesteps <- 61
sir.dfLB <- data.frame( "timestep" = numeric(),
                        "S" = numeric(), "I" = numeric(),"R" = numeric(),
                        stringsAsFactors=FALSE)
for (i in 0:timesteps) {
  newrow <- as.list(c(i,round(as.numeric(initialState * mcSIRLB ^ i),0)))
  sir.dfLB[nrow(sir.dfLB) + 1, ] <- newrow
}

head(sir.dfLB)


sir.dfSL$country<-"Sierra_Leone"
sir.dfgn$country<-"Guinea"
sir.dfLB$country<-"Liberia"
sir.df<-rbind(sir.dfSL, sir.dfgn, sir.dfLB)
plot(sir.df$timestep,sir.df$S)
points(sir.df$timestep,sir.df$I, col="red")
points(sir.df$timestep,sir.df$R, col="blue")
#points(sird.df$timestep,sird.df$D, col="green")
library(ggplot2)
library(gridExtra)
library(grid)
p <-ggplot(sir.df, aes(timesteps, S, fill = country))
#geom_line(aes(colour=country), size=3)+
  geom_point()

absorbingStates(mcSIR)
transientStates(mcSIR)
steadyStates(mcSIR)
ab.state <- absorbingStates(mcSIR)
#occurs.at <- min(which(sid.df[,ab.state]==max(sid.df[,ab.state])))
#(sid.df[row,]$timestep)+1

#sid.df[occurs.at,]$timestep+1.state]==max(sid.df[,ab.state])))

#sid.df[occurs.at,]$timestep+1
simulated<-sir.df
#Loading data
setwd("/Users/borishouenou/Downloads")
df1=read.csv("casesEVD.csv")
df2=read.csv("Coverage.csv")
df3=read.csv("dataEbola.csv")
df4=read.csv("ETC.csv")
str(df1)
str(df2)
str(df3)
str(df4)
library(sqldf)# loading SQL package
#Manipulating & wrangling data: join and date transformation
dfa<-sqldf("select * from df1 join df2 on df1.Date=df2.Date")
dfb<-sqldf("select * from df3 join df4 on df3.Date=df4.Date")
#df<-sqldf("select * from dfa join dfb on dfa.Date=dfb.Date")
str(dfa)
Date1<-as.Date(dfa$Date, "%m/%d/%y")
df<-cbind(dfa, Date1)
str(df)
dfs<-sqldf("select Date1, Total_Cases, Total_Deaths, Value from df")
dfsSL<-sqldf("select Date1, Total_Cases_SierraLeone, Total_Deaths_SierraLeone, Value from df")
dfsSL$ctry<-"Sierra_Leone"
colnames(dfsSL)<-c("date","cases", "deaths", "coverage", "country")
dfsG<-sqldf("select Date1, Total_Cases_Guinea, Total_Deaths_Guinea, Value from df")
dfsG$ctry<-"Guinea"
colnames(dfsG)<-c("date","cases", "deaths", "coverage", "country")
dfsL<-sqldf("select Date1, Total_Cases_Liberia, Total_Deaths_Liberia, Value from df")
dfsL$ctry<-"Liberia"
colnames(dfsL)<-c("date","cases", "deaths", "coverage", "country")
dpanel<-rbind (dfsSL, dfsG, dfsL)
dpanel$recovered<-dpanel$cases-dpanel$deaths
data<-cbind(dpanel, simulated)
data1<-data[which(data$country=="Sierra_Leone"),]
data2<-data[which(data$country=="Guinea"),]
data3<-data[which(data$country=="Liberia"),]

library(latticeExtra)
data1<-data[which(data$country=="Sierra_Leone"),]
data2<-data[which(data$country=="Guinea"),]
data3<-data[which(data$country=="Liberia"),]

######INFECTED#####
#####Cases Sierra Leone#####
# --> construct separate plots for each series
Observed_Cases_SierraLeone <- xyplot(cases ~ date, data1, type = "l" , lwd=2,
                                      xlab="Time",
                                     ylab="Cases")
Simulated_Cases_SierraLeone<- xyplot(I ~ date, data1, type = "l", lwd=2, xlab="Time",
                                     ylab="Cases")

# --> Make the plot with second y axis:
doubleYScale(Observed_Cases_SierraLeone, Simulated_Cases_SierraLeone, add.ylab2 = TRUE)

## 3=== Same graph with a key legend
ISierra<-update(doubleYScale(Observed_Cases_SierraLeone, Simulated_Cases_SierraLeone, text = c("Observed_Cases", "Simulated_Cases") , add.ylab2 = TRUE, add.axis = FALSE),
                par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

#####Cases Guinea#####
# --> construct separate plots for each series
Observed_Cases_Guinea <- xyplot(cases ~ date, data2, type = "l" , lwd=2, xlab="Time",
                                ylab="Cases")
Simulated_Cases_Guinea<- xyplot(I ~ date, data2, type = "l", lwd=2, xlab="Time",
                                ylab="Cases")

# --> Make the plot with second y axis:
doubleYScale(Observed_Cases_Guinea, Simulated_Cases_Guinea, add.ylab2 = TRUE)

## 3=== Same graph with a key legend
IGuinea<-update(doubleYScale(Observed_Cases_Guinea, Simulated_Cases_Guinea, text = c("Observed_Cases_Guinea", "Simulated_Cases_Guinea") , add.ylab2 = TRUE,add.axis = FALSE),
                par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

#####Cases Liberia#####
# --> construct separate plots for each series
Observed_Cases_Liberia <- xyplot(cases ~ date, data3, type = "l" , lwd=2, xlab="Time",
                                 ylab="Cases")
Simulated_Cases_Liberia<- xyplot(I ~ date, data3, type = "l", lwd=2, xlab="Time",
                                 ylab="Cases")

# --> Make the plot with second y axis:
doubleYScale(Observed_Cases_Liberia, Simulated_Cases_Liberia, add.ylab2 = TRUE)

## 3=== Same graph with a key legend
ILiberia<-update(doubleYScale(Observed_Cases_Liberia, Simulated_Cases_Liberia, text = c("Observed_Cases_Liberia", "Simulated_Cases_Liberia") , add.ylab2 = TRUE, add.axis = FALSE),
                 par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

graph1<-c(Sierra_Leone=ISierra, Guinea=IGuinea, Liberia=ILiberia)
# ######DEATHS#####
# #####deaths Sierra Leone#####
# # --> construct separate plots for each series
# Observed_deaths_SierraLeone <- xyplot(deaths ~ date, data1, type = "l" , lwd=2)
# Simulated_deaths_SierraLeone<- xyplot(D ~ date, data1, type = "l", lwd=2)
# 
# # --> Make the plot with second y axis:
# doubleYScale(Observed_deaths_SierraLeone, Simulated_deaths_SierraLeone, add.ylab2 = TRUE)
# 
# ## 3=== Same graph with a key legend
# DSierra<-update(doubleYScale(Observed_deaths_SierraLeone, Simulated_deaths_SierraLeone, text = c("Observed_deaths", "Simulated_deaths") , add.ylab2 = TRUE, add.axis = FALSE),
#                 par.settings = simpleTheme(col = c('black','black'), lty = 1:2))
# #####deaths Guinea#####
# # --> construct separate plots for each series
# Observed_deaths_Guinea <- xyplot(deaths ~ date, data2, type = "l" , lwd=2)
# Simulated_deaths_Guinea<- xyplot(D ~ date, data2, type = "l", lwd=2)
# 
# # --> Make the plot with second y axis:
# doubleYScale(Observed_deaths_Guinea, Simulated_deaths_Guinea, add.ylab2 = TRUE)
# 
# ## 3=== Same graph with a key legend
# DGuinea<-update(doubleYScale(Observed_deaths_Guinea, Simulated_deaths_Guinea, text = c("Observed_deaths_Guinea", "Simulated_deaths_Guinea") , add.ylab2 = TRUE),
#                 par.settings = simpleTheme(col = c('black','black'), lty = 1:2))
# #####deaths Liberia#####
# # --> construct separate plots for each series
# Observed_deaths_Liberia <- xyplot(deaths ~ date, data3, type = "l" , lwd=2)
# Simulated_deaths_Liberia<- xyplot(D ~ date, data3, type = "l", lwd=2)
# 
# # --> Make the plot with second y axis:
# doubleYScale(Observed_deaths_Liberia, Simulated_deaths_Liberia, add.ylab2 = TRUE)
# 
# ## 3=== Same graph with a key legend
# DLiberia<-update(doubleYScale(Observed_deaths_Liberia, Simulated_deaths_Liberia, text = c("Observed_deaths_Liberia", "Simulated_deaths_Liberia") , add.ylab2 = TRUE),
#                  par.settings = simpleTheme(col = c('black','black'), lty = 1:2))
# 
# graph2<-c(Sierra_Leone=DSierra, Guinea=DGuinea, Liberia=DLiberia)
######RECOVERED#####
#####recovered Sierra Leone#####
# --> construct separate plots for each series
Observed_recovered_SierraLeone <- xyplot(recovered ~ date, data1, type = "l" , lwd=2,xlab="Time",
                                         ylab="Recovered")
Simulated_recovered_SierraLeone<- xyplot(R ~ date, data1, type = "l", lwd=2, xlab="Time",
                                         ylab="Recovered")

# --> Make the plot with second y axis:
doubleYScale(Observed_recovered_SierraLeone, Simulated_recovered_SierraLeone, add.ylab2 = FALSE)

## 3=== Same graph with a key legend
RSierra<-update(doubleYScale(Observed_recovered_SierraLeone, Simulated_recovered_SierraLeone, text = c("Observed_recovered", "Simulated_recovered") , add.ylab2 = TRUE),
                par.settings = simpleTheme(col = c('black','black'), lty = 1:2))
#####recovered Guinea#####
# --> construct separate plots for each series
Observed_recovered_Guinea <- xyplot(recovered ~ date, data2, type = "l" , lwd=2, xlab="Time",
                                    ylab="Recovered")
Simulated_recovered_Guinea<- xyplot(R ~ date, data2, type = "l", lwd=2, xlab="Time",
                                    ylab="Recovered")

# --> Make the plot with second y axis:
doubleYScale(Observed_recovered_Guinea, Simulated_recovered_Guinea, add.ylab2 = TRUE)

## 3=== Same graph with a key legend
RGuinea<-update(doubleYScale(Observed_recovered_Guinea, Simulated_recovered_Guinea, text = c("Observed_recovered_Guinea", "Simulated_recovered_Guinea") , add.ylab2 = TRUE),
                par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

#####recovered Liberia#####
# --> construct separate plots for each series
Observed_recovered_Liberia <- xyplot(recovered ~ date, data3, type = "l" , lwd=2, xlab="Time",
                                     ylab="Recovered")
Simulated_recovered_Liberia<- xyplot(R ~ date, data3, type = "l", lwd=2, xlab="Time",
                                     ylab="Recovered")

# --> Make the plot with second y axis:
doubleYScale(Observed_recovered_Liberia, Simulated_recovered_Liberia, add.ylab2 = FALSE)

## 3=== Same graph with a key legend
#asTheEconomist(update(doubleYScale(Observed_recovered_Liberia, Simulated_recovered_Liberia, text = c("Observed_recovered_Liberia", "Simulated_recovered_Liberia") , add.ylab2 = FALSE),
#par.settings = simpleTheme(col = c('black','black'), lty = 1:2)))

RLiberia<-update(doubleYScale(Observed_recovered_Liberia, Simulated_recovered_Liberia, text = c("Observed_recovered_Liberia", "Simulated_recovered_Liberia") , add.ylab2 = FALSE),
                 par.settings = simpleTheme(col = c('black','black'), lty = 1:2))
graph3<-c(Sierra_Leone=RSierra, Guinea=RGuinea, Liberia=RLiberia)


#####coverage and observed recovered Liberia#####
# --> construct separate plots for each series
Observed_recovered_Liberia <- xyplot(recovered ~ date, data3, type = "l" , lwd=2, xlab="Time",
                                     ylab="Observed Recovered")
Coverage_Liberia<- xyplot(coverage ~ date, data3, type = "l", lwd=2, xlab="Time",
                          ylab="Coverage Rate")

# --> Make the plot with second y axis:
doubleYScale(Observed_recovered_Liberia, Coverage_Liberia, add.ylab2 = FALSE)

## 3=== Same graph with a key legend
#asTheEconomist(update(doubleYScale(Observed_recovered_Liberia, Coverage_Liberia, text = c("Observed_recovered_Liberia", "Coverage_Liberia") , add.ylab2 = FALSE),
#par.settings = simpleTheme(col = c('black','black'), lty = 1:2)))

coverage<-update(doubleYScale(Observed_recovered_Liberia, Coverage_Liberia, text = c("Observed_recovered_Liberia", "Coverage_Liberia") , add.ylab2 = TRUE),
                 par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

#####coverage and Simulated recovered Liberia#####
# --> construct separate plots for each series
Simulated_recovered_Liberia <- xyplot(R ~ date, data3, type = "l" , lwd=2,xlab="Time",
                                      ylab="Simulated Recovered")
Coverage_Liberia<- xyplot(coverage ~ date, data3, type = "l", lwd=2,xlab="Time",
                          ylab="Coverage Rate")

# --> Make the plot with second y axis:
doubleYScale(Simulated_recovered_Liberia, Coverage_Liberia, add.ylab2 = TRUE)
coverage2<-update(doubleYScale(Simulated_recovered_Liberia, Coverage_Liberia, text = c("Simulated_recovered_Liberia", "Coverage_Liberia"), add.ylab2 = TRUE), par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

## 3=== Same graph with a key legend
asTheEconomist(update(doubleYScale(Simulated_recovered_Liberia, Coverage_Liberia, text = c("Simulated_recovered_Liberia", "Coverage_Liberia") , add.ylab2 = FALSE),
                      par.settings = simpleTheme(col = c('black','black'), lty = 1:2)))

Simulated<-update(doubleYScale(Simulated_recovered_Liberia, Coverage_Liberia, text = c("Simulated_recovered_Liberia", "Coverage_Liberia") , add.ylab2 = FALSE),
                  par.settings = simpleTheme(col = c('black','black'), lty = 1:2))

graph4<-c(Observed_Recoverage=coverage, Simulated_Recoverage=Simulated)

c(graph1, graph3, graph4)

graph1
graph3
graph4
library(ggplot2)

p1 <- ggplot(data, aes(date, cases)) + 
  geom_line(aes(linetype=country), size=1) + 
  facet_wrap(~country, ncol=1)
print(p1)

p2 <- ggplot(data, aes(date, I)) + 
  geom_line(aes(linetype=country), size=1) + 
  facet_wrap(~country, ncol=1)
print(p2)

p3 <- ggplot(data, aes(date, coverage)) + 
  geom_line(aes(linetype=country), size=1) + 
  facet_wrap(~country, ncol=1)
print(p3)



library(reshape2)
df <- melt(data[, c("date", "cases", "I")], id="date")
ggplot(data) + geom_line(aes(x=date, y=I ,color=country)) + labs(title="Cases")# plot multiple time series by melting

ggplot(data) + geom_line(aes(x=date, y=cases, color=country)) + geom_line(aes(x=date, y=I, col=country)) + scale_color_discrete(name="Legend") + labs(title="Economics") #


g<- ggplot(data, aes(x = date, y = cases)) +
  geom_line() +  #plot flow
  geom_line(aes(y = I), colour = "black", size =1.5)   # plot rain2 
g + facet_grid(scales = "fixed",country ~ .)


facet_wrap(~country)
