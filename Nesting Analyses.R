library(dplyr)
library(tidyverse)
library(ggplot2)

#import data

data<-read.csv("WORKING DATA.csv")
data$Year<-as.factor(data$Year)
data$Relocated<-as.factor(data$Relocated)
data$Uncovered.<-as.factor(data$Uncovered.)
data$Predated<-as.factor(data$Predated)
data$Washed.Over<-as.factor(data$Washed.Over)
data$Complete.Wash.out<-as.factor(data$Complete.Wash.out)
data$Mod...Disturbances<-as.factor(data$Mod...Disturbances)
data$C_GC<-as.factor(data$C_GC)
data$C<-as.factor(data$C)
data$Inventoried<-as.factor(data$Inventoried)
data$Clutch.Est<-as.numeric(as.character(data$Clutch.Est))
summary(data)

#set up vector of beach names and years
bch_name<-c("CSB", "CSTGI", "DI", "FM", "GS", "OB", "PCB", "SJ", "STG", "STGISP", "STJSP", "STVNWR")
data_yr<-seq(2012,2018,1)

#set up dataframes for each beach

CSB<-subset(data,Beach.Name %in% c("CSB"))
CSTGI<-subset(data, Beach.Name %in% c("CSTGI"))
DI<-subset(data, Beach.Name %in% c("DI"))
FM<-subset(data, Beach.Name %in% c("FM"))
GS<-subset(data, Beach.Name %in% c("GS"))
OB<-subset(data, Beach.Name %in% c("OB"))
PCB<-subset(data, Beach.Name %in% c("PCB"))
SJ<-subset(data, Beach.Name %in% c("SJ"))
STG<-subset(data, Beach.Name %in% c("STG"))
STGISP<-subset(data, Beach.Name %in% c("STGISP"))
STJSP<-subset(data, Beach.Name %in% c("STJSP"))
STVNWR<-subset(data, Beach.Name %in% c("STVNWR"))

#set up empty vectors
data2<-NULL
CSB2<-NULL
CSTGI2<-NULL
DI2<-NULL
FM2<-NULL
GS2<-NULL
OB2<-NULL
PCB2<-NULL
SJ2<-NULL
STG2<-NULL
STGISP2<-NULL
STJSP2<-NULL
STVNWR2<-NULL

#set up empty matrices
data_Summary<-matrix(data = NA, nrow = 7, ncol = 16)
CSB_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
CSTGI_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
DI_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
FM_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
GS_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
OB_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
PCB_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
SJ_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
STG_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
STGISP_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
STJSP_Summary <- matrix(data = NA, nrow = 7, ncol = 16)
STVNWR_Summary <- matrix(data = NA, nrow = 7, ncol = 16)

#beach-specific stats
sumstats<-function(x) #creates function to store following summary statistic functions
{
  N<-length(x) #number
  Mean<-round(mean(x, na.rm=T),2) #mean
  StDev<-round(sd(x),2) #standard deviation
  Summary<-c(N, Mean, StDev) #aggregate calculations
  names(Summary)<-c("N", "Mean", "StDev") #name calculations
  return(Summary)} #output values of calculations

##Total Annual Stats
for(i in data_yr){
  data_YR<-data[data$Year==i,]
  N_Nests<-length(data_YR$State)
  N_Invent<-length(data_YR$Inventoried[data_YR$Inventoried=="1"])
  N_Rel<-length(data_YR$Relocated[data_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(data_YR$Uncovered.[data_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(data_YR$Predated[data_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(data_YR$Washed.Over[data_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(data_YR$Complete.Wash.out[data_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(data_YR$C_GC[data_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(data_YR$C[data_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  data2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  data_Summary[i-2011,] <- data2
}
    
data_Summary<-as.data.frame(data_Summary)
rownames(data_Summary)<-data_yr
colnames(data_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
data_Summary
  
#create data frame of summary statistics for data
data_Annual<-as.data.frame(apply(data_Summary, 2, sumstats))

##total stats
  Tot_Nests<-length(data$State)
  Tot_Invent<-length(data$Inventoried[data$Inventoried=="1"])
  NTot_Rel<-length(data$Relocated[data$Relocated=="1"])
  PTot_Rel<- round((NTot_Rel/Tot_Nests)*100, 1)
  NTot_Prot<-length(data$Uncovered.[data$Uncovered.=="0"])
  PTot_Prot<-round((NTot_Prot/Tot_Nests)*100,1)
  NTot_Pred<-length(data$Predated[data$Predated=="1"])
  PTot_Pred<-round((NTot_Pred/Tot_Nests)*100,1)
  NTot_WOver<-length(data$Washed.Over[data$Washed.Over=="1"])
  PTot_WOver<-round((NTot_WOver/Tot_Nests)*100,1)
  NTot_WOut<-length(data$Complete.Wash.out[data$Complete.Wash.out=="1"])
  PTot_WOut<-round((NTot_WOut/Tot_Nests)*100,1)
  NTot_GCC<-length(data$C_GC[data$C_GC=="1"])
  PTot_GCC<-round((NTot_GCC/Tot_Nests)*100,1)
  NTot_C<-length(data$C[data$C=="1"])
  PTot_C<-round((NTot_C/Tot_Nests)*100,1)
  data_Tot<-c(Tot_Nests, Tot_Invent, NTot_Rel, PTot_Rel, NTot_Prot, PTot_Prot, NTot_Pred, PTot_Pred, NTot_WOver, PTot_WOver, NTot_WOut, PTot_WOut, NTot_GCC, PTot_GCC, NTot_C, PTot_C)
  data_Tot<-as.data.frame(data_Tot)
  rownames(data_Tot)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
data_Tot

##1: CSB Annual Stats

for(i in data_yr){
  CSB_YR<-CSB[CSB$Year==i,]
  N_Nests<-length(CSB_YR$State)
  N_Invent<-length(CSB_YR$Inventoried[CSB_YR$Inventoried=="1"])
  N_Rel<-length(CSB_YR$Relocated[CSB_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(CSB_YR$Uncovered.[CSB_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(CSB_YR$Predated[CSB_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(CSB_YR$Washed.Over[CSB_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(CSB_YR$Complete.Wash.out[CSB_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(CSB_YR$C_GC[CSB_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(CSB_YR$C[CSB_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  CSB2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  CSB_Summary[i-2011,] <- CSB2
}

CSB_Summary<-as.data.frame(CSB_Summary)
rownames(CSB_Summary)<-data_yr
colnames(CSB_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
CSB_Summary

#create data frame of summary statistics for CSB
CSB_Annual<-as.data.frame(apply(CSB_Summary, 2, sumstats))


##2: CSTGI Annual Stats

for(i in data_yr){
  CSTGI_YR<-CSTGI[CSTGI$Year==i,]
  N_Nests<-length(CSTGI_YR$State)
  N_Invent<-length(CSTGI_YR$Inventoried[CSTGI_YR$Inventoried=="1"])
  N_Rel<-length(CSTGI_YR$Relocated[CSTGI_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(CSTGI_YR$Uncovered.[CSTGI_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(CSTGI_YR$Predated[CSTGI_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(CSTGI_YR$Washed.Over[CSTGI_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(CSTGI_YR$Complete.Wash.out[CSTGI_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(CSTGI_YR$C_GC[CSTGI_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(CSTGI_YR$C[CSTGI_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  CSTGI2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  CSTGI_Summary[i-2011,] <- CSTGI2
}

CSTGI_Summary<-as.data.frame(CSTGI_Summary)
rownames(CSTGI_Summary)<-data_yr
colnames(CSTGI_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
CSTGI_Summary

#create data frame of summary statistics for CSTGI
CSTGI_Annual<-as.data.frame(apply(CSTGI_Summary, 2, sumstats))


##3: DI Annual Stats

for(i in data_yr){
  DI_YR<-DI[DI$Year==i,]
  N_Nests<-length(DI_YR$State)
  N_Invent<-length(DI_YR$Inventoried[DI_YR$Inventoried=="1"])
  N_Rel<-length(DI_YR$Relocated[DI_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(DI_YR$Uncovered.[DI_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(DI_YR$Predated[DI_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(DI_YR$Washed.Over[DI_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(DI_YR$Complete.Wash.out[DI_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(DI_YR$C_GC[DI_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(DI_YR$C[DI_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  DI2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  DI_Summary[i-2011,] <- DI2
}

DI_Summary<-as.data.frame(DI_Summary)
rownames(DI_Summary)<-data_yr
colnames(DI_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
DI_Summary

#create data frame of summary statistics for DI
DI_Annual<-as.data.frame(apply(DI_Summary, 2, sumstats))


##4: FM Annual Stats

for(i in data_yr){
  FM_YR<-FM[FM$Year==i,]
  N_Nests<-length(FM_YR$State)
  N_Invent<-length(FM_YR$Inventoried[FM_YR$Inventoried=="1"])
  N_Rel<-length(FM_YR$Relocated[FM_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(FM_YR$Uncovered.[FM_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(FM_YR$Predated[FM_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(FM_YR$Washed.Over[FM_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(FM_YR$Complete.Wash.out[FM_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(FM_YR$C_GC[FM_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(FM_YR$C[FM_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  FM2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  FM_Summary[i-2011,] <- FM2
}

FM_Summary<-as.data.frame(FM_Summary)
rownames(FM_Summary)<-data_yr
colnames(FM_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
FM_Summary

#create data frame of summary statistics for FM
FM_Annual<-as.data.frame(apply(FM_Summary, 2, sumstats))


##5: GS Annual Stats

for(i in data_yr){
  GS_YR<-GS[GS$Year==i,]
  N_Nests<-length(GS_YR$State)
  N_Invent<-length(GS_YR$Inventoried[GS_YR$Inventoried=="1"])
  N_Rel<-length(GS_YR$Relocated[GS_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(GS_YR$Uncovered.[GS_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(GS_YR$Predated[GS_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(GS_YR$Washed.Over[GS_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(GS_YR$Complete.Wash.out[GS_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(GS_YR$C_GC[GS_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(GS_YR$C[GS_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  GS2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  GS_Summary[i-2011,] <- GS2
}

GS_Summary<-as.data.frame(GS_Summary)
rownames(GS_Summary)<-data_yr
colnames(GS_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
GS_Summary

#create data frame of summary statistics for GS
GS_Annual<-as.data.frame(apply(GS_Summary, 2, sumstats))


##6:OB Annual Stats

for(i in data_yr){
  OB_YR<-OB[OB$Year==i,]
  N_Nests<-length(OB_YR$State)
  N_Invent<-length(OB_YR$Inventoried[OB_YR$Inventoried=="1"])
  N_Rel<-length(OB_YR$Relocated[OB_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(OB_YR$Uncovered.[OB_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(OB_YR$Predated[OB_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(OB_YR$Washed.Over[OB_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(OB_YR$Complete.Wash.out[OB_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(OB_YR$C_GC[OB_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(OB_YR$C[OB_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  OB2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  OB_Summary[i-2011,] <- OB2
}

OB_Summary<-as.data.frame(OB_Summary)
rownames(OB_Summary)<-data_yr
colnames(OB_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
OB_Summary

#create data frame of summary statistics for OB
OB_Annual<-as.data.frame(apply(OB_Summary, 2, sumstats))


##7: PCB Annual Stats

for(i in data_yr){
  PCB_YR<-PCB[PCB$Year==i,]
  N_Nests<-length(PCB_YR$State)
  N_Invent<-length(PCB_YR$Inventoried[PCB_YR$Inventoried=="1"])
  N_Rel<-length(PCB_YR$Relocated[PCB_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(PCB_YR$Uncovered.[PCB_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(PCB_YR$Predated[PCB_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(PCB_YR$Washed.Over[PCB_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(PCB_YR$Complete.Wash.out[PCB_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(PCB_YR$C_GC[PCB_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(PCB_YR$C[PCB_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  PCB2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  PCB_Summary[i-2011,] <- PCB2
}

PCB_Summary<-as.data.frame(PCB_Summary)
rownames(PCB_Summary)<-data_yr
colnames(PCB_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
PCB_Summary

#create data frame of summary statistics for PCB
PCB_Annual<-as.data.frame(apply(PCB_Summary, 2, sumstats))


##8: SJ Annual Stats

for(i in data_yr){
  SJ_YR<-SJ[SJ$Year==i,]
  N_Nests<-length(SJ_YR$State)
  N_Invent<-length(SJ_YR$Inventoried[SJ_YR$Inventoried=="1"])
  N_Rel<-length(SJ_YR$Relocated[SJ_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(SJ_YR$Uncovered.[SJ_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(SJ_YR$Predated[SJ_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(SJ_YR$Washed.Over[SJ_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(SJ_YR$Complete.Wash.out[SJ_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(SJ_YR$C_GC[SJ_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(SJ_YR$C[SJ_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  SJ2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  SJ_Summary[i-2011,] <- SJ2
}

SJ_Summary<-as.data.frame(SJ_Summary)
rownames(SJ_Summary)<-data_yr
colnames(SJ_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
SJ_Summary

#create data frame of summary statistics for SJ
SJ_Annual<-as.data.frame(apply(SJ_Summary, 2, sumstats))


##9: STG Annual Stats

for(i in data_yr){
  STG_YR<-STG[STG$Year==i,]
  N_Nests<-length(STG_YR$State)
  N_Invent<-length(STG_YR$Inventoried[STG_YR$Inventoried=="1"])
  N_Rel<-length(STG_YR$Relocated[STG_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(STG_YR$Uncovered.[STG_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(STG_YR$Predated[STG_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(STG_YR$Washed.Over[STG_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(STG_YR$Complete.Wash.out[STG_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(STG_YR$C_GC[STG_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(STG_YR$C[STG_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  STG2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  STG_Summary[i-2011,] <- STG2
}

STG_Summary<-as.data.frame(STG_Summary)
rownames(STG_Summary)<-data_yr
colnames(STG_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
STG_Summary

#create data frame of summary statistics for STG
STG_Annual<-as.data.frame(apply(STG_Summary, 2, sumstats))


##10: STGISP Annual Stats

for(i in data_yr){
  STGISP_YR<-STGISP[STGISP$Year==i,]
  N_Nests<-length(STGISP_YR$State)
  N_Invent<-length(STGISP_YR$Inventoried[STGISP_YR$Inventoried=="1"])
  N_Rel<-length(STGISP_YR$Relocated[STGISP_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(STGISP_YR$Uncovered.[STGISP_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(STGISP_YR$Predated[STGISP_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(STGISP_YR$Washed.Over[STGISP_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(STGISP_YR$Complete.Wash.out[STGISP_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(STGISP_YR$C_GC[STGISP_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(STGISP_YR$C[STGISP_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  STGISP2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  STGISP_Summary[i-2011,] <- STGISP2
}

STGISP_Summary<-as.data.frame(STGISP_Summary)
rownames(STGISP_Summary)<-data_yr
colnames(STGISP_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
STGISP_Summary

#create data frame of summary statistics for STGISP
STGISP_Annual<-as.data.frame(apply(STGISP_Summary, 2, sumstats))


##11: STJSP Annual Stats

for(i in data_yr){
  STJSP_YR<-STJSP[STJSP$Year==i,]
  N_Nests<-length(STJSP_YR$State)
  N_Invent<-length(STJSP_YR$Inventoried[STJSP_YR$Inventoried=="1"])
  N_Rel<-length(STJSP_YR$Relocated[STJSP_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(STJSP_YR$Uncovered.[STJSP_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(STJSP_YR$Predated[STJSP_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(STJSP_YR$Washed.Over[STJSP_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(STJSP_YR$Complete.Wash.out[STJSP_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(STJSP_YR$C_GC[STJSP_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(STJSP_YR$C[STJSP_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  STJSP2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  STJSP_Summary[i-2011,] <- STJSP2
}

STJSP_Summary<-as.data.frame(STJSP_Summary)
rownames(STJSP_Summary)<-data_yr
colnames(STJSP_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
STJSP_Summary

#create data frame of summary statistics for STJSP
STJSP_Annual<-as.data.frame(apply(STJSP_Summary, 2, sumstats))

##12: STVNWR Annual Stats

for(i in data_yr){
  STVNWR_YR<-STVNWR[STVNWR$Year==i,]
  N_Nests<-length(STVNWR_YR$State)
  N_Invent<-length(STVNWR_YR$Inventoried[STVNWR_YR$Inventoried=="1"])
  N_Rel<-length(STVNWR_YR$Relocated[STVNWR_YR$Relocated=="1"])
  P_Rel<- round((N_Rel/N_Nests)*100, 1)
  N_Prot<-length(STVNWR_YR$Uncovered.[STVNWR_YR$Uncovered.=="0"])
  P_Prot<-round((N_Prot/N_Nests)*100,1)
  N_Pred<-length(STVNWR_YR$Predated[STVNWR_YR$Predated=="1"])
  P_Pred<-round((N_Pred/N_Nests)*100,1)
  N_WOver<-length(STVNWR_YR$Washed.Over[STVNWR_YR$Washed.Over=="1"])
  P_WOver<-round((N_WOver/N_Nests)*100,1)
  N_WOut<-length(STVNWR_YR$Complete.Wash.out[STVNWR_YR$Complete.Wash.out=="1"])
  P_WOut<-round((N_WOut/N_Nests)*100,1)
  N_GCC<-length(STVNWR_YR$C_GC[STVNWR_YR$C_GC=="1"])
  P_GCC<-round((N_GCC/N_Nests)*100,1)
  N_C<-length(STVNWR_YR$C[STVNWR_YR$C=="1"])
  P_C<-round((N_C/N_Nests)*100,1)
  STVNWR2<-c(N_Nests, N_Invent, N_Rel, P_Rel, N_Prot, P_Prot, N_Pred, P_Pred, N_WOver, P_WOver, N_WOut, P_WOut, N_GCC, P_GCC, N_C, P_C)
  STVNWR_Summary[i-2011,] <- STVNWR2
}

STVNWR_Summary<-as.data.frame(STVNWR_Summary)
rownames(STVNWR_Summary)<-data_yr
colnames(STVNWR_Summary)<-c("N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")
STVNWR_Summary

#create data frame of summary statistics for STVNWR
STVNWR_Annual<-as.data.frame(apply(STVNWR_Summary, 2, sumstats))

##check summary data frames
data_Summary
CSB_Summary
CSTGI_Summary
FM_Summary
DI_Summary
FM_Summary
GS_Summary
OB_Summary
PCB_Summary
SJ_Summary
STG_Summary
STGISP_Summary
STJSP_Summary
STVNWR_Summary

##check annual data frames
data_Annual
CSB_Annual
CSTGI_Annual
FM_Annual
DI_Annual
FM_Annual
GS_Annual
OB_Annual
PCB_Annual
SJ_Annual
STG_Annual
STGISP_Annual
STJSP_Annual
STVNWR_Annual

##create data frame of all annual stats
annual_stats<-as.data.frame(rbind(CSB_Annual, CSTGI_Annual,DI_Annual, FM_Annual, GS_Annual, OB_Annual, PCB_Annual, SJ_Annual,STG_Annual,STGISP_Annual,STJSP_Annual, STVNWR_Annual, data_Annual))
annual_stats$Sites<-c("CSB","CSB","CSB", "CSTGI","CSTGI","CSTGI", "DI","DI","DI", "FM","FM","FM", "GS","GS","GS", "OB", "OB", "OB", "PCB","PCB","PCB", "SJ","SJ","SJ", "STG","STG","STG", "STGISP","STGISP","STGISP","STJSP","STJSP","STJSP","STVNWR","STVNWR","STVNWR", "Total","Total", "Total")
annual_stats<-annual_stats[,c("Sites", "N_Nests", "N_Invent", "N_Rel", "P_Rel", "N_Prot", "P_Prot", "N_Pred", "P_Pred", "N_WOver", "P_WOver", "N_WOut", "P_WOut", "N_GCC", "P_GCC", "N_C", "P_C")]
annual_stats
write.csv(annual_stats, "Annual_Stats.csv")

#######calculate site specific means for ECS, ES, and P###############

##format data for means
CSB[CSB==999]<-NA
CSTGI[CSTGI==999]<-NA
DI[DI==999]<-NA
FM[FM==999]<-NA
GS[GS==999]<-NA
OB[OB==999]<-NA
PCB[PCB==999]<-NA
SJ[SJ==999]<-NA
STG[STG==999]<-NA
STGISP[STGISP==999]<-NA
STJSP[STJSP==999]<-NA
STVNWR[STVNWR==999]<-NA
data_na<-rbind(CSB, CSTGI, DI, FM, GS, OB, PCB, SJ, STG, STGISP,STJSP, STVNWR)

##set up dataframes for undisturbed nests from each beach

CSB_UN<-subset(CSB,Disturbed. %in% c("0"))
CSTGI_UN<-subset(CSTGI, Disturbed. %in% c("0"))
DI_UN<-subset(DI, Disturbed. %in% c("0"))
FM_UN<-subset(FM, Disturbed. %in% c("0"))
GS_UN<-subset(GS, Disturbed. %in% c("0"))
OB_UN<-subset(OB, Disturbed. %in% c("0"))
PCB_UN<-subset(PCB, Disturbed. %in% c("0"))
SJ_UN<-subset(SJ, Disturbed. %in% c("0"))
STG_UN<-subset(STG, Disturbed. %in% c("0"))
STGISP_UN<-subset(STGISP, Disturbed. %in% c("0"))
STJSP_UN<-subset(STJSP, Disturbed. %in% c("0"))
STVNWR_UN<-subset(STVNWR, Disturbed. %in% c("0"))
data_un<-rbind(CSB_UN,CSTGI_UN,DI_UN, FM_UN, GS_UN, OB_UN, PCB_UN, SJ_UN, STG_UN, STGISP_UN, STJSP_UN, STVNWR_UN)

#set up empty vectors
data2u<-NULL
CSB2u<-NULL
CSTGI2u<-NULL
DI2u<-NULL
FM2u<-NULL
GS2u<-NULL
OB2u<-NULL
PCB2u<-NULL
SJ2u<-NULL
STG2u<-NULL
STGISP2u<-NULL
STJSP2u<-NULL
STVNWR2u<-NULL

#set up empty matrices
data_u<-matrix(data = NA, nrow = 7, ncol = 10)
CSB_u<- matrix(data = NA, nrow = 7, ncol = 10)
CSTGI_u<- matrix(data = NA, nrow = 7, ncol = 10)
DI_u <- matrix(data = NA, nrow = 7, ncol = 10)
FM_u <- matrix(data = NA, nrow = 7, ncol = 10)
GS_u <- matrix(data = NA, nrow = 7, ncol = 10)
OB_u <- matrix(data = NA, nrow = 7, ncol = 10)
PCB_u <- matrix(data = NA, nrow = 7, ncol = 10)
SJ_u <- matrix(data = NA, nrow = 7, ncol = 10)
STG_u <- matrix(data = NA, nrow = 7, ncol = 10)
STGISP_u <- matrix(data = NA, nrow = 7, ncol = 10)
STJSP_u <- matrix(data = NA, nrow = 7, ncol = 10)
STVNWR_u <- matrix(data = NA, nrow = 7, ncol = 10)

##Overall calculations##

for(i in data_yr){
  N<-data_Summary$N_Nests[c(i-2011)]
  data_YR<-data_na[data_na$Year==i,]
  dataUN_YR<-data_un[data_un$Year==i,]
  ECS<-round(mean(data_YR$Clutch.Est, na.rm=T),0)
  ECSsd<-round(sd(data_YR$Clutch.Est, na.rm =T),0)
  ECS_UN<-round(mean(dataUN_YR$Clutch.Est, na.rm=T),0)
  ECS_UNsd<-round(sd(dataUN_YR$Clutch.Est, na.rm=T),0)
  ES<-round(mean(data_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(data_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(dataUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(dataUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES*N,0)
  P_UN<-round(ECS_UN*ES_UN*N,0)
  data2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  data_u[i-2011,] <- data2u
}

data_u<-as.data.frame(data_u)
rownames(data_u)<-data_yr
colnames(data_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
data_u

#create data frame of summary statistics for all data
data_u_Ann<-as.data.frame(apply(data_u, 2, sumstats))
data_u_Ann


##1: CSB calculations

for(i in data_yr){
  N<-CSB_Summary$N_Nests[c(i-2011)]
  CSB_YR<-CSB[CSB$Year==i,]
  CSBUN_YR<-CSB_UN[CSB_UN$Year==i,]
  ECS<-round(mean(CSB_YR$Clutch.Est, na.rm=T),0)
  ECSsd<-round(sd(CSB_YR$Clutch.Est, na.rm =T),0)
  ECS_UN<-round(mean(CSBUN_YR$Clutch.Est, na.rm=T),0)
  ECS_UNsd<-round(sd(CSBUN_YR$Clutch.Est, na.rm=T),0)
  ES<-round(mean(CSB_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(CSB_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(CSBUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(CSBUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES*N,0)
  P_UN<-round(ECS_UN*ES_UN*N,0)
  CSB2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  CSB_u[i-2011,] <- CSB2u
}

CSB_u<-as.data.frame(CSB_u)
rownames(CSB_u)<-data_yr
colnames(CSB_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
CSB_u

#create data frame of summary statistics for STVNWR
CSB_u_Ann<-as.data.frame(apply(CSB_u, 2, sumstats))
CSB_u_Ann


##2 CSTGI calculations

for(i in data_yr){
  CSTGI_YR<-CSTGI[CSTGI$Year==i,]
  CSTGIUN_YR<-CSTGI_UN[CSTGI_UN$Year==i,]
  ECS<-round(mean(CSTGI_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(CSTGI_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(CSTGIUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(CSTGIUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(CSTGI_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(CSTGI_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(CSTGIUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(CSTGIUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  CSTGI2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  CSTGI_u[i-2011,] <- CSTGI2u
}

CSTGI_u<-as.data.frame(CSTGI_u)
rownames(CSTGI_u)<-data_yr
colnames(CSTGI_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
CSTGI_u

#create data frame of summary statistics for STVNWR
CSTGI_u_Ann<-as.data.frame(apply(CSTGI_u, 2, sumstats))
CSTGI_u_Ann

##3 DI calculations

for(i in data_yr){
  DI_YR<-DI[DI$Year==i,]
  DIUN_YR<-DI_UN[DI_UN$Year==i,]
  ECS<-round(mean(DI_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(DI_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(DIUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(DIUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(DI_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(DI_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(DIUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(DIUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  DI2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  DI_u[i-2011,] <- DI2u
}

DI_u<-as.data.frame(DI_u)
rownames(DI_u)<-data_yr
colnames(DI_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
DI_u

#create data frame of summary statistics for STVNWR
DI_u_Ann<-as.data.frame(apply(DI_u, 2, sumstats))
DI_u_Ann

##4 FM calculations

for(i in data_yr){
  FM_YR<-FM[FM$Year==i,]
  FMUN_YR<-FM_UN[FM_UN$Year==i,]
  ECS<-round(mean(FM_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(FM_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(FMUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(FMUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(FM_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(FM_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(FMUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(FMUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  FM2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  FM_u[i-2011,] <- FM2u
}

FM_u<-as.data.frame(FM_u)
rownames(FM_u)<-data_yr
colnames(FM_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
FM_u

#create data frame of summary statistics for STVNWR
FM_u_Ann<-as.data.frame(apply(FM_u, 2, sumstats))
FM_u_Ann

##5 GS calculations

for(i in data_yr){
  GS_YR<-GS[GS$Year==i,]
  GSUN_YR<-GS_UN[GS_UN$Year==i,]
  ECS<-round(mean(GS_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(GS_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(GSUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(GSUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(GS_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(GS_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(GSUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(GSUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  GS2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  GS_u[i-2011,] <- GS2u
}

GS_u<-as.data.frame(GS_u)
rownames(GS_u)<-data_yr
colnames(GS_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
GS_u

#create data frame of summary statistics for STVNWR
GS_u_Ann<-as.data.frame(apply(GS_u, 2, sumstats))
GS_u_Ann

##6 OB calculations

for(i in data_yr){
  OB_YR<-OB[OB$Year==i,]
  OBUN_YR<-OB_UN[OB_UN$Year==i,]
  ECS<-round(mean(OB_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(OB_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(OBUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(OBUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(OB_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(OB_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(OBUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(OBUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  OB2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  OB_u[i-2011,] <- OB2u
}

OB_u<-as.data.frame(OB_u)
rownames(OB_u)<-data_yr
colnames(OB_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
OB_u

#create data frame of summary statistics for STVNWR
OB_u_Ann<-as.data.frame(apply(OB_u, 2, sumstats))
OB_u_Ann

##7 PCB calculations

for(i in data_yr){
  PCB_YR<-PCB[PCB$Year==i,]
  PCBUN_YR<-PCB_UN[PCB_UN$Year==i,]
  ECS<-round(mean(PCB_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(PCB_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(PCBUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(PCBUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(PCB_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(PCB_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(PCBUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(PCBUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  PCB2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  PCB_u[i-2011,] <- PCB2u
}

PCB_u<-as.data.frame(PCB_u)
rownames(PCB_u)<-data_yr
colnames(PCB_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
PCB_u

#create data frame of summary statistics for STVNWR
PCB_u_Ann<-as.data.frame(apply(PCB_u, 2, sumstats))
PCB_u_Ann

##8 SJ calculations

for(i in data_yr){
  SJ_YR<-SJ[SJ$Year==i,]
  SJUN_YR<-SJ_UN[SJ_UN$Year==i,]
  ECS<-round(mean(SJ_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(SJ_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(SJUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(SJUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(SJ_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(SJ_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(SJUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(SJUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  SJ2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  SJ_u[i-2011,] <- SJ2u
}

SJ_u<-as.data.frame(SJ_u)
rownames(SJ_u)<-data_yr
colnames(SJ_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
SJ_u

#create data frame of summary statistics for STVNWR
SJ_u_Ann<-as.data.frame(apply(SJ_u, 2, sumstats))
SJ_u_Ann

##9 STG calculations

for(i in data_yr){
  STG_YR<-STG[STG$Year==i,]
  STGUN_YR<-STG_UN[STG_UN$Year==i,]
  ECS<-round(mean(STG_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(STG_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(STGUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(STGUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(STG_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(STG_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(STGUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(STGUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  STG2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  STG_u[i-2011,] <- STG2u
}

STG_u<-as.data.frame(STG_u)
rownames(STG_u)<-data_yr
colnames(STG_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
STG_u

#create data frame of summary statistics for STVNWR
STG_u_Ann<-as.data.frame(apply(STG_u, 2, sumstats))
STG_u_Ann

##10 STGISP calculations

for(i in data_yr){
  STGISP_YR<-STGISP[STGISP$Year==i,]
  STGISPUN_YR<-STGISP_UN[STGISP_UN$Year==i,]
  ECS<-round(mean(STGISP_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(STGISP_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(STGISPUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(STGISPUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(STGISP_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(STGISP_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(STGISPUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(STGISPUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  STGISP2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  STGISP_u[i-2011,] <- STGISP2u
}

STGISP_u<-as.data.frame(STGISP_u)
rownames(STGISP_u)<-data_yr
colnames(STGISP_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
STGISP_u

#create data frame of summary statistics for STVNWR
STGISP_u_Ann<-as.data.frame(apply(STGISP_u, 2, sumstats))
STGISP_u_Ann

##11 STJSP calculations

for(i in data_yr){
  STJSP_YR<-STJSP[STJSP$Year==i,]
  STJSPUN_YR<-STJSP_UN[STJSP_UN$Year==i,]
  ECS<-round(mean(STJSP_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(STJSP_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(STJSPUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(STJSPUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(STJSP_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(STJSP_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(STJSPUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(STJSPUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  STJSP2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  STJSP_u[i-2011,] <- STJSP2u
}

STJSP_u<-as.data.frame(STJSP_u)
rownames(STJSP_u)<-data_yr
colnames(STJSP_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
STJSP_u

#create data frame of summary statistics for STVNWR
STJSP_u_Ann<-as.data.frame(apply(STJSP_u, 2, sumstats))
STJSP_u_Ann

##12 STVNWR calculations

for(i in data_yr){
  STVNWR_YR<-STVNWR[STVNWR$Year==i,]
  STVNWRUN_YR<-STVNWR_UN[STVNWR_UN$Year==i,]
  ECS<-round(mean(STVNWR_YR$Clutch.Est, na.rm=T),2)
  ECSsd<-round(sd(STVNWR_YR$Clutch.Est, na.rm =T),2)
  ECS_UN<-round(mean(STVNWRUN_YR$Clutch.Est, na.rm=T),2)
  ECS_UNsd<-round(sd(STVNWRUN_YR$Clutch.Est, na.rm=T),2)
  ES<-round(mean(STVNWR_YR$ES.Overall, na.rm=T),2)
  ESsd<-round(sd(STVNWR_YR$ES.Overall, na.rm=T),2)
  ES_UN<-round(mean(STVNWRUN_YR$ES.Overall, na.rm=T),2)
  ES_UNsd<-round(sd(STVNWRUN_YR$ES.Overall, na.rm=T),2)
  P<-round(ECS*ES,0)
  P_UN<-round(ECS_UN*ES_UN,0)
  STVNWR2u<-c(ECS, ECSsd, ECS_UN, ECS_UNsd, ES, ESsd, ES_UN, ES_UNsd, P, P_UN)
  STVNWR_u[i-2011,] <- STVNWR2u
}

STVNWR_u<-as.data.frame(STVNWR_u)
rownames(STVNWR_u)<-data_yr
colnames(STVNWR_u)<-c("ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")
STVNWR_u

#create data frame of summary statistics for STVNWR
STVNWR_u_Ann<-as.data.frame(apply(STVNWR_u, 2, sumstats))
STVNWR_u_Ann

##check summary data frames
#data_Summary
CSB_u
CSTGI_u
FM_u
DI_u
FM_u
GS_u
OB_u
PCB_u
SJ_u
STG_u
STGISP_u
STJSP_u
STVNWR_u

##check annual data frames
#data_Annual
CSB_u_Ann
CSTGI_u_Ann
FM_u_Ann
DI_u_Ann
FM_u_Ann
GS_u_Ann
OB_u_Ann
PCB_u_Ann
SJ_u_Ann
STG_u_Ann
STGISP_u_Ann
STJSP_u_Ann
STVNWR_u_Ann

##create data frame of all annual stats
annual_means<-as.data.frame(rbind(CSB_u_Ann, CSTGI_u_Ann,DI_u_Ann, FM_u_Ann, GS_u_Ann, OB_u_Ann, PCB_u_Ann, SJ_u_Ann,STG_u_Ann,STGISP_u_Ann,STJSP_u_Ann, STVNWR_u_Ann))
annual_means$Sites<-c("CSB","CSB","CSB", "CSTGI","CSTGI","CSTGI", "DI","DI","DI", "FM","FM","FM", "GS","GS","GS", "OB", "OB", "OB", "PCB","PCB","PCB", "SJ","SJ","SJ", "STG","STG","STG", "STGISP","STGISP","STGISP","STJSP","STJSP","STJSP","STVNWR","STVNWR","STVNWR")
annual_means<-annual_means[,c("Sites","ECS", "ECSsd", "ECS_UN", "ECS_UNsd", "ES", "ESsd", "ES_UN", "ES_UNsd", "P", "P_UN")]
annual_means
write.csv(annual_means, "Annual_Means.csv")

#######means########

#set up empty vectors
data_means<-NULL

#set up empty matrices
data_means2<-matrix(data = NA, nrow = 12, ncol = 8, dimnames=list(bch_name))

## Beach by Beach calculations
for(i in bch_name){
  BCH_DAT<-data_na[data_na$Beach.Name==i,]
  BCH_UN<-data_un[data_un$Beach.Name==i,]
  N<-length(BCH_DAT$State)
  M_ECS<-round(mean(BCH_DAT$Clutch.Est, na.rm=T),2)
  M_ECSsd<-round(sd(BCH_DAT$Clutch.Est, na.rm =T),2)
  M_ECS_UN<-round(mean(BCH_UN$Clutch.Est, na.rm=T),2)
  M_ECS_UNsd<-round(sd(BCH_UN$Clutch.Est, na.rm=T),2)
  M_ES<-round(mean(BCH_DAT$ES.Overall, na.rm=T),3)
  M_ESsd<-round(sd(BCH_DAT$ES.Overall, na.rm=T),3)
  M_ES_UN<-round(mean(BCH_UN$ES.Overall, na.rm=T),3)
  M_ES_UNsd<-round(sd(BCH_UN$ES.Overall, na.rm=T),3)
  data_means<-c(M_ECS_UN, M_ECS_UNsd, M_ECS, M_ECSsd,M_ES_UN, M_ES_UNsd, M_ES, M_ESsd)
  data_means2[i,] <- data_means 
}

data_tot_means<-as.data.frame(data_means2)
colnames(data_tot_means)<-c("ECS_UN", "ECS_UNsd","ECS", "ECSsd","ES_UN","ES_UNsd", "ES", "ESsd")
data_tot_means
write.csv(data_tot_means, "Tot_Means.csv")

##add P and P_UN and P_INC to Tot_Means##

#set up empty vectors
CSB2u<-NULL
CSTGI2u<-NULL
DI2u<-NULL
FM2u<-NULL
GS2u<-NULL
OB2u<-NULL
PCB2u<-NULL
SJ2u<-NULL
STG2u<-NULL
STGISP2u<-NULL
STJSP2u<-NULL
STVNWR2u<-NULL

#set up empty matrices
CSB_u2<- matrix(data = NA, nrow = 7, ncol = 4)
CSTGI_u2<- matrix(data = NA, nrow = 7, ncol = 4)
DI_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
FM_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
GS_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
OB_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
PCB_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
SJ_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
STG_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
STGISP_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
STJSP_u2 <- matrix(data = NA, nrow = 7, ncol = 4)
STVNWR_u2 <- matrix(data = NA, nrow = 7, ncol = 4)

##1: CSB calculations

for(i in data_yr){
  N<-CSB_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["CSB", "ECS"]
  M_ES<-data_tot_means["CSB", "ES"]
  M_ECS_UN<-data_tot_means["CSB", "ECS_UN"]
  M_ES_UN<-data_tot_means["CSB", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  CSB2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  CSB_u2[i-2011,] <- CSB2u
}

CSB_Ps<-as.data.frame(CSB_u2)
rownames(CSB_Ps)<-data_yr
colnames(CSB_Ps)<-c("P", "P_UN", "DP", "P_INC")
CSB_Ps

#create data frame of summary statistics for STVNWR
CSB_Psu<-as.data.frame(apply(CSB_Ps, 2, sumstats))
CSB_Psu

##2: CSTGI calculations

for(i in data_yr){
  N<-CSTGI_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["CSTGI", "ECS"]
  M_ES<-data_tot_means["CSTGI", "ES"]
  M_ECS_UN<-data_tot_means["CSTGI", "ECS_UN"]
  M_ES_UN<-data_tot_means["CSTGI", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  CSTGI2u<-c(M_P, M_P_UN,M_DP, M_P_INC)
  CSTGI_u2[i-2011,] <- CSTGI2u
}

CSTGI_Ps<-as.data.frame(CSTGI_u2)
rownames(CSTGI_Ps)<-data_yr
colnames(CSTGI_Ps)<-c("P", "P_UN", "DP", "P_INC")
CSTGI_Ps

#create data frame of summary statistics for STVNWR
CSTGI_Psu<-as.data.frame(apply(CSTGI_Ps, 2, sumstats))
CSTGI_Psu

##3: DI calculations

for(i in data_yr){
  N<-DI_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["DI", "ECS"]
  M_ES<-data_tot_means["DI", "ES"]
  M_ECS_UN<-data_tot_means["DI", "ECS_UN"]
  M_ES_UN<-data_tot_means["DI", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  DI2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  DI_u2[i-2011,] <- DI2u
}

DI_Ps<-as.data.frame(DI_u2)
rownames(DI_Ps)<-data_yr
colnames(DI_Ps)<-c("P", "P_UN", "DP", "P_INC")
DI_Ps

#create data frame of summary statistics for STVNWR
DI_Psu<-as.data.frame(apply(DI_Ps, 2, sumstats))
DI_Psu

##4: FM calculations

for(i in data_yr){
  N<-FM_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["FM", "ECS"]
  M_ES<-data_tot_means["FM", "ES"]
  M_ECS_UN<-data_tot_means["FM", "ECS_UN"]
  M_ES_UN<-data_tot_means["FM", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  FM2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  FM_u2[i-2011,] <- FM2u
}

FM_Ps<-as.data.frame(FM_u2)
rownames(FM_Ps)<-data_yr
colnames(FM_Ps)<-c("P", "P_UN", "DP", "P_INC")
FM_Ps

#create data frame of summary statistics for STVNWR
FM_Psu<-as.data.frame(apply(FM_Ps, 2, sumstats))
FM_Psu

##5: GS calculations

for(i in data_yr){
  N<-GS_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["GS", "ECS"]
  M_ES<-data_tot_means["GS", "ES"]
  M_ECS_UN<-data_tot_means["GS", "ECS_UN"]
  M_ES_UN<-data_tot_means["GS", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  GS2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  GS_u2[i-2011,] <- GS2u
}

GS_Ps<-as.data.frame(GS_u2)
rownames(GS_Ps)<-data_yr
colnames(GS_Ps)<-c("P", "P_UN", "DP", "P_INC")
GS_Ps

#create data frame of summary statistics for STVNWR
GS_Psu<-as.data.frame(apply(GS_Ps, 2, sumstats))
GS_Psu

##6: OB calculations

for(i in data_yr){
  N<-OB_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["OB", "ECS"]
  M_ES<-data_tot_means["OB", "ES"]
  M_ECS_UN<-data_tot_means["OB", "ECS_UN"]
  M_ES_UN<-data_tot_means["OB", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  OB2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  OB_u2[i-2011,] <- OB2u
}

OB_Ps<-as.data.frame(OB_u2)
rownames(OB_Ps)<-data_yr
colnames(OB_Ps)<-c("P", "P_UN", "DP", "P_INC")
OB_Ps

#create data frame of summary statistics for STVNWR
OB_Psu<-as.data.frame(apply(OB_Ps, 2, sumstats))
OB_Psu

##7: PCB calculations

for(i in data_yr){
  N<-PCB_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["PCB", "ECS"]
  M_ES<-data_tot_means["PCB", "ES"]
  M_ECS_UN<-data_tot_means["PCB", "ECS_UN"]
  M_ES_UN<-data_tot_means["PCB", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  PCB2u<-c(M_P, M_P_UN, M_DP,M_P_INC)
  PCB_u2[i-2011,] <- PCB2u
}

PCB_Ps<-as.data.frame(PCB_u2)
rownames(PCB_Ps)<-data_yr
colnames(PCB_Ps)<-c("P", "P_UN", "DP", "P_INC")
PCB_Ps

#create data frame of summary statistics for STVNWR
PCB_Psu<-as.data.frame(apply(PCB_Ps, 2, sumstats))
PCB_Psu

##8: SJ calculations

for(i in data_yr){
  N<-SJ_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["SJ", "ECS"]
  M_ES<-data_tot_means["SJ", "ES"]
  M_ECS_UN<-data_tot_means["SJ", "ECS_UN"]
  M_ES_UN<-data_tot_means["SJ", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  SJ2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  SJ_u2[i-2011,] <- SJ2u
}

SJ_Ps<-as.data.frame(SJ_u2)
rownames(SJ_Ps)<-data_yr
colnames(SJ_Ps)<-c("P", "P_UN", "DP", "P_INC")
SJ_Ps

#create data frame of summary statistics for STVNWR
SJ_Psu<-as.data.frame(apply(SJ_Ps, 2, sumstats))
SJ_Psu

##9: STG calculations

for(i in data_yr){
  N<-STG_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["STG", "ECS"]
  M_ES<-data_tot_means["STG", "ES"]
  M_ECS_UN<-data_tot_means["STG", "ECS_UN"]
  M_ES_UN<-data_tot_means["STG", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  STG2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  STG_u2[i-2011,] <- STG2u
}

STG_Ps<-as.data.frame(STG_u2)
rownames(STG_Ps)<-data_yr
colnames(STG_Ps)<-c("P", "P_UN", "DP", "P_INC")
STG_Ps

#create data frame of summary statistics for STVNWR
STG_Psu<-as.data.frame(apply(STG_Ps, 2, sumstats))
STG_Psu

##10: STGISP calculations

for(i in data_yr){
  N<-STGISP_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["STGISP", "ECS"]
  M_ES<-data_tot_means["STGISP", "ES"]
  M_ECS_UN<-data_tot_means["STGISP", "ECS_UN"]
  M_ES_UN<-data_tot_means["STGISP", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  STGISP2u<-c(M_P, M_P_UN,M_DP, M_P_INC)
  STGISP_u2[i-2011,] <- STGISP2u
}

STGISP_Ps<-as.data.frame(STGISP_u2)
rownames(STGISP_Ps)<-data_yr
colnames(STGISP_Ps)<-c("P", "P_UN","DP", "P_INC")
STGISP_Ps

#create data frame of summary statistics for STVNWR
STGISP_Psu<-as.data.frame(apply(STGISP_Ps, 2, sumstats))
STGISP_Psu

##11: STJSP calculations

for(i in data_yr){
  N<-STJSP_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["STJSP", "ECS"]
  M_ES<-data_tot_means["STJSP", "ES"]
  M_ECS_UN<-data_tot_means["STJSP", "ECS_UN"]
  M_ES_UN<-data_tot_means["STJSP", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  STJSP2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  STJSP_u2[i-2011,] <- STJSP2u
}

STJSP_Ps<-as.data.frame(STJSP_u2)
rownames(STJSP_Ps)<-data_yr
colnames(STJSP_Ps)<-c("P", "P_UN", "DP", "P_INC")
STJSP_Ps

#create data frame of summary statistics for STVNWR
STJSP_Psu<-as.data.frame(apply(STJSP_Ps, 2, sumstats))
STJSP_Psu

##12: STVNWR calculations

for(i in data_yr){
  N<-STVNWR_Summary$N_Nests[c(i-2011)]
  M_ECS<-data_tot_means["STVNWR", "ECS"]
  M_ES<-data_tot_means["STVNWR", "ES"]
  M_ECS_UN<-data_tot_means["STVNWR", "ECS_UN"]
  M_ES_UN<-data_tot_means["STVNWR", "ES_UN"]
  M_P<-round(M_ECS*M_ES*N,0)
  M_P_UN<-round(M_ECS_UN*M_ES_UN*N,0)
  M_DP<-M_P_UN-M_P
  M_P_INC<-round((M_P_UN/M_P)*100,1)
  STVNWR2u<-c(M_P, M_P_UN, M_DP, M_P_INC)
  STVNWR_u2[i-2011,] <- STVNWR2u
}

STVNWR_Ps<-as.data.frame(STVNWR_u2)
rownames(STVNWR_Ps)<-data_yr
colnames(STVNWR_Ps)<-c("P", "P_UN", "DP", "P_INC")
STVNWR_Ps

#create data frame of summary statistics for STVNWR
STVNWR_Psu<-as.data.frame(apply(STVNWR_Ps, 2, sumstats))
STVNWR_Psu

#agreggate all data
Tot_Ps<-as.data.frame(rbind(CSB_Psu, CSTGI_Psu, DI_Psu,FM_Psu, GS_Psu, OB_Psu,PCB_Psu,SJ_Psu, STG_Psu,STGISP_Psu, STJSP_Psu, STVNWR_Psu))
Tot_Ps$SITE<-c("CSB","CSB","CSB", "CSTGI","CSTGI","CSTGI", "DI","DI","DI","FM","FM","FM","GS","GS","GS", "OB", "OB","OB","PCB","PCB","PCB","SJ","SJ","SJ","STG","STG","STG","STGISP","STGISP","STGISP","STJSP","STJSP","STJSP","STVNWR","STVNWR","STVNWR")
Tot_Ps<-Tot_Ps[,c("SITE", "P_UN", "P", "DP", "P_INC")]
write.csv(Tot_Ps, "Tot_Ps.csv")

###calculate N, ES, P from nests that were predated, WOV, WOU at each site
##Npred*ESpred*ECSun [ECSun better estimate of actual clutch size]

##subset data for each category

#new data years matrix
data_year<-seq(2012,2018,1)

##predated
CSB_pred<-subset(CSB,Predated %in% c("1"))
  CSB_pred<-subset(CSB_pred, Mod...Disturbances %in% c("1"))
CSTGI_pred<-subset(CSTGI, Predated %in% c("1"))
  CSTGI_pred<-subset(CSTGI_pred, Mod...Disturbances %in% c("1"))
DI_pred<-subset(DI, Predated %in% c("1"))
  DI_pred<-subset(DI_pred, Mod...Disturbances %in% c("1"))
FM_pred<-subset(FM, Predated %in% c("1"))
  FM_pred<-subset(FM_pred, Mod...Disturbances %in% c("1"))
GS_pred<-subset(GS, Predated %in% c("1"))
  GS_pred<-subset(GS_pred, Mod...Disturbances %in% c("1"))
OB_pred<-subset(OB, Predated %in% c("1"))
  OB_pred<-subset(OB_pred, Mod...Disturbances %in% c("1"))
PCB_pred<-subset(PCB, Predated %in% c("1"))
  PCB_pred<-subset(PCB_pred, Mod...Disturbances %in% c("1"))
SJ_pred<-subset(SJ, Predated %in% c("1"))
  SJ_pred<-subset(SJ_pred, Mod...Disturbances %in% c("1"))
STG_pred<-subset(STG, Predated %in% c("1"))
  STG_pred<-subset(STG_pred, Mod...Disturbances %in% c("1"))
STGISP_pred<-subset(STGISP, Predated %in% c("1"))
  STGISP_pred<-subset(STGISP_pred, Mod...Disturbances %in% c("1"))
STJSP_pred<-subset(STJSP, Predated %in% c("1"))
  STJSP_pred<-subset(STJSP_pred, Mod...Disturbances %in% c("1"))
STVNWR_pred<-subset(STVNWR, Predated %in% c("1"))
  STVNWR_pred<-subset(STVNWR_pred, Mod...Disturbances %in% c("1"))

##coyote
CSB_coy<-subset(CSB,C %in% c("1"))
  CSB_coy<-subset(CSB_coy, Mod...Disturbances %in% c("1"))
CSTGI_coy<-subset(CSTGI, C %in% c("1"))
  CSTGI_coy<-subset(CSTGI_coy, Mod...Disturbances %in% c("1"))
DI_coy<-subset(DI, C %in% c("1"))
  DI_coy<-subset(DI_coy, Mod...Disturbances %in% c("1"))
FM_coy<-subset(FM, C %in% c("1"))
  FM_coy<-subset(FM_coy, Mod...Disturbances %in% c("1"))
GS_coy<-subset(GS, C %in% c("1"))
  GS_coy<-subset(GS_coy, Mod...Disturbances %in% c("1"))
OB_coy<-subset(OB, C %in% c("1"))
  OB_coy<-subset(OB_coy, Mod...Disturbances %in% c("1"))
PCB_coy<-subset(PCB, C %in% c("1"))
  PCB_coy<-subset(PCB_coy, Mod...Disturbances %in% c("1"))
SJ_coy<-subset(SJ, C %in% c("1"))
  SJ_coy<-subset(SJ_coy, Mod...Disturbances %in% c("1"))
STG_coy<-subset(STG, C %in% c("1"))
  STG_coy<-subset(STG_coy, Mod...Disturbances %in% c("1"))
STGISP_coy<-subset(STGISP, C %in% c("1"))
  STGISP_coy<-subset(STGISP_coy, Mod...Disturbances %in% c("1"))
STJSP_coy<-subset(STJSP, C %in% c("1"))
  STJSP_coy<-subset(STJSP_coy, Mod...Disturbances %in% c("1"))
STVNWR_coy<-subset(STVNWR, C %in% c("1"))
  STVNWR_coy<-subset(STVNWR_coy, Mod...Disturbances %in% c("1"))

##ghostcrab and coyote
CSB_gcc<-subset(CSB,C_GC %in% c("1"))
  CSB_gcc<-subset(CSB_gcc, Mod...Disturbances %in% c("1"))
CSTGI_gcc<-subset(CSTGI, C_GC %in% c("1"))
  CSTGI_gcc<-subset(CSTGI_gcc, Mod...Disturbances %in% c("1"))
DI_gcc<-subset(DI, C_GC %in% c("1"))
  DI_gcc<-subset(DI_gcc, Mod...Disturbances %in% c("1"))
FM_gcc<-subset(FM, C_GC %in% c("1"))
  FM_gcc<-subset(FM_gcc, Mod...Disturbances %in% c("1"))
GS_gcc<-subset(GS, C_GC %in% c("1"))
  GS_gcc<-subset(GS_gcc, Mod...Disturbances %in% c("1"))
OB_gcc<-subset(OB, C_GC %in% c("1"))
  OB_gcc<-subset(OB_gcc, Mod...Disturbances %in% c("1"))
PCB_gcc<-subset(PCB, C_GC %in% c("1"))
  PCB_gcc<-subset(PCB_gcc, Mod...Disturbances %in% c("1"))
SJ_gcc<-subset(SJ, C_GC %in% c("1"))
  SJ_gcc<-subset(SJ_gcc, Mod...Disturbances %in% c("1"))
STG_gcc<-subset(STG, C_GC %in% c("1"))
  STG_gcc<-subset(STG_gcc, Mod...Disturbances %in% c("1"))
STGISP_gcc<-subset(STGISP, C_GC %in% c("1"))
  STGISP_gcc<-subset(STGISP_gcc, Mod...Disturbances %in% c("1"))
STJSP_gcc<-subset(STJSP, C_GC %in% c("1"))
  STJSP_gcc<-subset(STJSP_gcc, Mod...Disturbances %in% c("1"))
STVNWR_gcc<-subset(STVNWR, C_GC %in% c("1"))
  STVNWR_gcc<-subset(STVNWR_gcc, Mod...Disturbances %in% c("1"))
Data_gcc<-subset(data, C_GC %in% c("1"))
  Data_gcc<-subset(Data_gcc, Mod...Disturbances %in% c("1"))
  Data_gcc[Data_gcc==999]<-NA  

##wash over
CSB_wove<-subset(CSB,Washed.Over %in% c("1"))
  CSB_wove<-subset(CSB_wove, Mod...Disturbances %in% c("1"))
CSTGI_wove<-subset(CSTGI, Washed.Over %in% c("1"))
  CSTGI_wove<-subset(CSTGI_wove, Mod...Disturbances %in% c("1"))
DI_wove<-subset(DI, Washed.Over %in% c("1"))
  DI_wove<-subset(DI_wove, Mod...Disturbances %in% c("1"))
FM_wove<-subset(FM, Washed.Over %in% c("1"))
  FM_wove<-subset(FM_wove, Mod...Disturbances %in% c("1"))
GS_wove<-subset(GS, Washed.Over %in% c("1"))
  GS_wove<-subset(GS_wove, Mod...Disturbances %in% c("1"))
OB_wove<-subset(OB, Washed.Over %in% c("1"))
  OB_wove<-subset(OB_wove, Mod...Disturbances %in% c("1"))
PCB_wove<-subset(PCB, Washed.Over %in% c("1"))
  PCB_wove<-subset(PCB_wove, Mod...Disturbances %in% c("1"))
SJ_wove<-subset(SJ, Washed.Over %in% c("1"))
  SJ_wove<-subset(SJ_wove, Mod...Disturbances %in% c("1"))
STG_wove<-subset(STG, Washed.Over %in% c("1"))
  STG_wove<-subset(STG_wove, Mod...Disturbances %in% c("1"))
STGISP_wove<-subset(STGISP, Washed.Over %in% c("1"))
  STGISP_wove<-subset(STGISP_wove, Mod...Disturbances %in% c("1"))
STJSP_wove<-subset(STJSP, Washed.Over %in% c("1"))
  STJSP_wove<-subset(STJSP_wove, Mod...Disturbances %in% c("1"))
STVNWR_wove<-subset(STVNWR, Washed.Over %in% c("1"))
  STVNWR_wove<-subset(STVNWR_wove, Mod...Disturbances %in% c("1"))
Data_Wove<-subset(data, Washed.Over %in% c("1"))
  Data_Wove<-subset(Data_Wove, Mod...Disturbances %in% c("1"))
  Data_Wove[Data_Wove==999]<-NA

##wash out
CSB_wou<-subset(CSB,Complete.Wash.out %in% c("1"))
CSTGI_wou<-subset(CSTGI, Complete.Wash.out %in% c("1"))
DI_wou<-subset(DI, Complete.Wash.out %in% c("1"))
FM_wou<-subset(FM, Complete.Wash.out %in% c("1"))
GS_wou<-subset(GS, Complete.Wash.out %in% c("1"))
OB_wou<-subset(OB, Complete.Wash.out %in% c("1"))
PCB_wou<-subset(PCB, Complete.Wash.out %in% c("1"))
SJ_wou<-subset(SJ, Complete.Wash.out %in% c("1"))
STG_wou<-subset(STG, Complete.Wash.out %in% c("1"))
STGISP_wou<-subset(STGISP, Complete.Wash.out %in% c("1"))
STJSP_wou<-subset(STJSP, Complete.Wash.out %in% c("1"))
STVNWR_wou<-subset(STVNWR, Complete.Wash.out %in% c("1"))

##set up dummy matrices and vectors

#set up empty vectors
CSB_DPs<-NULL
CSTGI_DPs<-NULL
DI_DPs<-NULL
FM_DPs<-NULL
GS_DPs<-NULL
OB_DPs<-NULL
PCB_DPs<-NULL
SJ_DPs<-NULL
STG_DPs<-NULL
STGISP_DPs<-NULL
STJSP_DPs<-NULL
STVNWR_DPs<-NULL

#set up empty matrices
CSB_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
CSTGI_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
DI_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
FM_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
GS_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
OB_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
PCB_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
SJ_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
STG_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
STGISP_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
STJSP_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)
STVNWR_DPs_tot<- matrix(data = NA, nrow = 7, ncol = 10)

#set up dummy matrix for final percent calcs
Tot_Pers_DPs<-matrix(data = NA, nrow = 12, ncol = 5)
rownames(Tot_Pers_DPs)<-bch_name
colnames(Tot_Pers_DPs)<-c("% Inc Pred", "% Inc Wove", "% Inc Wout", "% Inc Coy", "% Inc GCC")

##1: CSB
for(i in data_year){
  CSB_pred_yr<-CSB_pred[CSB_pred$Year==i,]
  CSB_wove_yr<-CSB_wove[CSB_wove$Year==i,]
  CSB_wou_yr<-CSB_wou[CSB_wou$Year==i,]
  CSB_coy_yr<-CSB_coy[CSB_coy$Year==i,]
  CSB_gcc_yr<-CSB_gcc[CSB_gcc$Year==i,]
  
  DP_ECS<-data_tot_means["CSB", "ECS_UN"]
  DP_ES_un<-data_tot_means["CSB","ES_UN"]
  DP_P<-CSB_Ps[i-2011, "P"]
  
  N_Pred<-CSB_Summary[i-2011,"N_Pred"]
  N_Wove<-CSB_Summary[i-2011,"N_WOver"]
  N_Wou<-CSB_Summary[i-2011, "N_WOut"]
  N_coy<-CSB_Summary[i-2011, "N_C"] 
  N_gcc<-CSB_Summary[i-2011, "N_GCC"] 
  
  ES_Pred<-round(mean(CSB_pred_yr$ES.Overall, na.rm=T),2)
  ES_Wove<-round(mean(CSB_wove_yr$ES.Overall, na.rm=T),2)
  Es_Wou<-0
  ES_coy<-round(mean(CSB_coy_yr$ES.Overall, na.rm=T),2) 
  ES_gcc<-round(mean(CSB_gcc_yr$ES.Overall, na.rm=T),2) 
  
  P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
  P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
  P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
  P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
  P_Wove_un
  P_Wou<-0
  P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
  P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
  P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
  P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
  P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
  
  D_Pred<-P_Pred_un-P_Pred
  Per_Pred<-round(100*(D_Pred/DP_P),2)
  D_Wove<-P_Wove_un-P_Wove
  Per_Wove<-round(100*(D_Wove/DP_P),2)
  D_Wou<-P_Wou_un-P_Wou
  Per_Wou<-round(100*(D_Wou/DP_P),2)
  D_coy<-P_coy_un-P_coy 
  Per_coy<-round(100*(D_coy/DP_P),2) 
  D_gcc<-P_gcc_un-P_gcc 
  Per_gcc<-round(100*(D_gcc/DP_P),2) 
  CSB_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc) 
  CSB_DPs_tot[i-2011,]<-CSB_DPs
}

#store as data frame
  CSB_DPs_tot<-as.data.frame(CSB_DPs_tot)
  rownames(CSB_DPs_tot)<-data_yr
  colnames(CSB_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC") 
  CSB_DPs_tot
  
#create data frame of summary statistics for CSB
  CSB_DPs_sum<-as.data.frame(apply(CSB_DPs_tot, 2, sumstats))
  CSB_DPs_sum
  
#calculate Percents based on annual means
  CSB_TotPer_Pred<-CSB_DPs_sum[2,1]/Tot_Ps[2,3]
  CSB_TotPer_Wove<-CSB_DPs_sum[2,3]/Tot_Ps[2,3]
  CSB_TotPer_Wout<-CSB_DPs_sum[2,5]/Tot_Ps[2,3]
  CSB_TotPer_Coy<-CSB_DPs_sum[2,7]/Tot_Ps[2,3] 
  CSB_TotPer_GCC<-CSB_DPs_sum[2,9]/Tot_Ps[2,3]
  CSB_TotPers<-c(CSB_TotPer_Pred,CSB_TotPer_Wove,CSB_TotPer_Wout,CSB_TotPer_Coy, CSB_TotPer_GCC)
  Tot_Pers_DPs["CSB",]<-CSB_TotPers
  
##2: CSTGI
  for(i in data_year){
    CSTGI_pred_yr<-CSTGI_pred[CSTGI_pred$Year==i,]
    CSTGI_wove_yr<-CSTGI_wove[CSTGI_wove$Year==i,]
    CSTGI_wou_yr<-CSTGI_wou[CSTGI_wou$Year==i,]
    CSTGI_coy_yr<-CSTGI_coy[CSTGI_coy$Year==i,] 
    CSTGI_gcc_yr<-CSTGI_gcc[CSTGI_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["CSTGI", "ECS_UN"]
    DP_ES_un<-data_tot_means["CSTGI","ES_UN"]
    DP_P<-CSTGI_Ps[i-2011, "P"]
    
    N_Pred<-CSTGI_Summary[i-2011,"N_Pred"]
    N_Wove<-CSTGI_Summary[i-2011,"N_WOver"]
    N_Wou<-CSTGI_Summary[i-2011, "N_WOut"]
    N_coy<-CSTGI_Summary[i-2011, "N_C"] 
    N_gcc<-CSTGI_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(CSTGI_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(CSTGI_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(CSTGI_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(CSTGI_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    CSTGI_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    CSTGI_DPs_tot[i-2011,]<-CSTGI_DPs
  }
  
#store as data frame
  CSTGI_DPs_tot<-as.data.frame(CSTGI_DPs_tot)
  rownames(CSTGI_DPs_tot)<-data_yr
  colnames(CSTGI_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  CSTGI_DPs_tot
  
#create data frame of summary statistics for CSTGI
  CSTGI_DPs_sum<-as.data.frame(apply(CSTGI_DPs_tot, 2, sumstats))
  CSTGI_DPs_sum

  #calculate Percents based on annual means
  CSTGI_TotPer_Pred<-CSTGI_DPs_sum[2,1]/Tot_Ps[5,3]
  CSTGI_TotPer_Wove<-CSTGI_DPs_sum[2,3]/Tot_Ps[5,3]
  CSTGI_TotPer_Wout<-CSTGI_DPs_sum[2,5]/Tot_Ps[5,3]
  CSTGI_TotPer_Coy<-CSTGI_DPs_sum[2,7]/Tot_Ps[5,3] 
  CSTGI_TotPer_GCC<-CSTGI_DPs_sum[2,9]/Tot_Ps[5,3]
  CSTGI_TotPers<-c(CSTGI_TotPer_Pred,CSTGI_TotPer_Wove,CSTGI_TotPer_Wout,CSTGI_TotPer_Coy, CSTGI_TotPer_GCC)
  Tot_Pers_DPs["CSTGI",]<-CSTGI_TotPers
  
##3: DI
  for(i in data_year){
    DI_pred_yr<-DI_pred[DI_pred$Year==i,]
    DI_wove_yr<-DI_wove[DI_wove$Year==i,]
    DI_wou_yr<-DI_wou[DI_wou$Year==i,]
    DI_coy_yr<-DI_coy[DI_coy$Year==i,] 
    DI_gcc_yr<-DI_gcc[DI_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["DI", "ECS_UN"]
    DP_ES_un<-data_tot_means["DI","ES_UN"]
    DP_P<-DI_Ps[i-2011, "P"]
    
    N_Pred<-DI_Summary[i-2011,"N_Pred"]
    N_Wove<-DI_Summary[i-2011,"N_WOver"]
    N_Wou<-DI_Summary[i-2011, "N_WOut"]
    N_coy<-DI_Summary[i-2011, "N_C"] 
    N_gcc<-DI_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(DI_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(DI_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(DI_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(DI_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    DI_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    DI_DPs_tot[i-2011,]<-DI_DPs
  }
  
#store as data frame
  DI_DPs_tot<-as.data.frame(DI_DPs_tot)
  rownames(DI_DPs_tot)<-data_yr
  colnames(DI_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  DI_DPs_tot
  
#create data frame of summary statistics for DI
  DI_DPs_sum<-as.data.frame(apply(DI_DPs_tot, 2, sumstats))
  DI_DPs_sum
  
  #calculate Percents based on annual means
  DI_TotPer_Pred<-DI_DPs_sum[2,1]/Tot_Ps[8,3]
  DI_TotPer_Wove<-DI_DPs_sum[2,3]/Tot_Ps[8,3]
  DI_TotPer_Wout<-DI_DPs_sum[2,5]/Tot_Ps[8,3]
  DI_TotPer_Coy<-DI_DPs_sum[2,7]/Tot_Ps[8,3] 
  DI_TotPer_GCC<-DI_DPs_sum[2,9]/Tot_Ps[8,3]
  DI_TotPers<-c(DI_TotPer_Pred,DI_TotPer_Wove,DI_TotPer_Wout,DI_TotPer_Coy, DI_TotPer_GCC)
  Tot_Pers_DPs["DI",]<-DI_TotPers
  
##4: FM
  for(i in data_year){
    FM_pred_yr<-FM_pred[FM_pred$Year==i,]
    FM_wove_yr<-FM_wove[FM_wove$Year==i,]
    FM_wou_yr<-FM_wou[FM_wou$Year==i,]
    FM_coy_yr<-FM_coy[FM_coy$Year==i,] 
    FM_gcc_yr<-FM_gcc[FM_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["FM", "ECS_UN"]
    DP_ES_un<-data_tot_means["FM","ES_UN"]
    DP_P<-FM_Ps[i-2011, "P"]
    
    N_Pred<-FM_Summary[i-2011,"N_Pred"]
    N_Wove<-FM_Summary[i-2011,"N_WOver"]
    N_Wou<-FM_Summary[i-2011, "N_WOut"]
    N_coy<-FM_Summary[i-2011, "N_C"] 
    N_gcc<-FM_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(FM_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(FM_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(FM_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(FM_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    FM_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    FM_DPs_tot[i-2011,]<-FM_DPs
  }
  
#store as data frame
  FM_DPs_tot<-as.data.frame(FM_DPs_tot)
  rownames(FM_DPs_tot)<-data_yr
  colnames(FM_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  FM_DPs_tot
  
#create data frame of summary statistics for FM
  FM_DPs_sum<-as.data.frame(apply(FM_DPs_tot, 2, sumstats))
  FM_DPs_sum

  #calculate Percents based on annual means
  FM_TotPer_Pred<-FM_DPs_sum[2,1]/Tot_Ps[11,3]
  FM_TotPer_Wove<-FM_DPs_sum[2,3]/Tot_Ps[11,3]
  FM_TotPer_Wout<-FM_DPs_sum[2,5]/Tot_Ps[11,3]
  FM_TotPer_Coy<-FM_DPs_sum[2,7]/Tot_Ps[11,3] 
  FM_TotPer_GCC<-FM_DPs_sum[2,9]/Tot_Ps[11,3]
  FM_TotPers<-c(FM_TotPer_Pred,FM_TotPer_Wove,FM_TotPer_Wout,FM_TotPer_Coy, FM_TotPer_GCC)
  Tot_Pers_DPs["FM",]<-FM_TotPers  
  
##5: GS
  for(i in data_year){
    GS_pred_yr<-GS_pred[GS_pred$Year==i,]
    GS_wove_yr<-GS_wove[GS_wove$Year==i,]
    GS_wou_yr<-GS_wou[GS_wou$Year==i,]
    GS_coy_yr<-GS_coy[GS_coy$Year==i,] 
    GS_gcc_yr<-GS_gcc[GS_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["GS", "ECS_UN"]
    DP_ES_un<-data_tot_means["GS","ES_UN"]
    DP_P<-GS_Ps[i-2011, "P"]
    
    N_Pred<-GS_Summary[i-2011,"N_Pred"]
    N_Wove<-GS_Summary[i-2011,"N_WOver"]
    N_Wou<-GS_Summary[i-2011, "N_WOut"]
    N_coy<-GS_Summary[i-2011, "N_C"] 
    N_gcc<-GS_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(GS_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(GS_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(GS_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(GS_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    GS_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    GS_DPs_tot[i-2011,]<-GS_DPs
  }
  
#store as data frame
  GS_DPs_tot<-as.data.frame(GS_DPs_tot)
  rownames(GS_DPs_tot)<-data_yr
  colnames(GS_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  GS_DPs_tot
  
#create data frame of summary statistics for GS
  GS_DPs_sum<-as.data.frame(apply(GS_DPs_tot, 2, sumstats))
  GS_DPs_sum

  #calculate Percents based on annual means
  GS_TotPer_Pred<-GS_DPs_sum[2,1]/Tot_Ps[14,3]
  GS_TotPer_Wove<-GS_DPs_sum[2,3]/Tot_Ps[14,3]
  GS_TotPer_Wout<-GS_DPs_sum[2,5]/Tot_Ps[14,3]
  GS_TotPer_Coy<-GS_DPs_sum[2,7]/Tot_Ps[14,3] 
  GS_TotPer_GCC<-GS_DPs_sum[2,9]/Tot_Ps[14,3]
  GS_TotPers<-c(GS_TotPer_Pred,GS_TotPer_Wove,GS_TotPer_Wout,GS_TotPer_Coy, GS_TotPer_GCC)
  Tot_Pers_DPs["GS",]<-GS_TotPers  
  
##6: OB
  for(i in data_year){
    OB_pred_yr<-OB_pred[OB_pred$Year==i,]
    OB_wove_yr<-OB_wove[OB_wove$Year==i,]
    OB_wou_yr<-OB_wou[OB_wou$Year==i,]
    OB_coy_yr<-OB_coy[OB_coy$Year==i,] 
    OB_gcc_yr<-OB_gcc[OB_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["OB", "ECS_UN"]
    DP_ES_un<-data_tot_means["OB","ES_UN"]
    DP_P<-OB_Ps[i-2011, "P"]
    
    N_Pred<-OB_Summary[i-2011,"N_Pred"]
    N_Wove<-OB_Summary[i-2011,"N_WOver"]
    N_Wou<-OB_Summary[i-2011, "N_WOut"]
    N_coy<-OB_Summary[i-2011, "N_C"] 
    N_gcc<-OB_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(OB_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(OB_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(OB_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(OB_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    OB_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    OB_DPs_tot[i-2011,]<-OB_DPs
  }
  
#store as data frame
  OB_DPs_tot<-as.data.frame(OB_DPs_tot)
  rownames(OB_DPs_tot)<-data_yr
  colnames(OB_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  OB_DPs_tot
  
#create data frame of summary statistics for OB
  OB_DPs_sum<-as.data.frame(apply(OB_DPs_tot, 2, sumstats))
  OB_DPs_sum

  #calculate Percents based on annual means
  OB_TotPer_Pred<-OB_DPs_sum[2,1]/Tot_Ps[17,3]
  OB_TotPer_Wove<-OB_DPs_sum[2,3]/Tot_Ps[17,3]
  OB_TotPer_Wout<-OB_DPs_sum[2,5]/Tot_Ps[17,3]
  OB_TotPer_Coy<-OB_DPs_sum[2,7]/Tot_Ps[17,3] 
  OB_TotPer_GCC<-OB_DPs_sum[2,9]/Tot_Ps[17,3]
  OB_TotPers<-c(OB_TotPer_Pred,OB_TotPer_Wove,OB_TotPer_Wout,OB_TotPer_Coy, OB_TotPer_GCC)
  Tot_Pers_DPs["OB",]<-OB_TotPers  
  
##7: PCB
  for(i in data_year){
    PCB_pred_yr<-PCB_pred[PCB_pred$Year==i,]
    PCB_wove_yr<-PCB_wove[PCB_wove$Year==i,]
    PCB_wou_yr<-PCB_wou[PCB_wou$Year==i,]
    PCB_coy_yr<-PCB_coy[PCB_coy$Year==i,] 
    PCB_gcc_yr<-PCB_gcc[PCB_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["PCB", "ECS_UN"]
    DP_ES_un<-data_tot_means["PCB","ES_UN"]
    DP_P<-PCB_Ps[i-2011, "P"]
    
    N_Pred<-PCB_Summary[i-2011,"N_Pred"]
    N_Wove<-PCB_Summary[i-2011,"N_WOver"]
    N_Wou<-PCB_Summary[i-2011, "N_WOut"]
    N_coy<-PCB_Summary[i-2011, "N_C"] 
    N_gcc<-PCB_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(PCB_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(PCB_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(PCB_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(PCB_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    PCB_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    PCB_DPs_tot[i-2011,]<-PCB_DPs
  }
  
#store as data frame
  PCB_DPs_tot<-as.data.frame(PCB_DPs_tot)
  rownames(PCB_DPs_tot)<-data_yr
  colnames(PCB_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  PCB_DPs_tot
  
#create data frame of summary statistics for PCB
  PCB_DPs_sum<-as.data.frame(apply(PCB_DPs_tot, 2, sumstats))
  PCB_DPs_sum

  #calculate Percents based on annual means
  PCB_TotPer_Pred<-PCB_DPs_sum[2,1]/Tot_Ps[20,3]
  PCB_TotPer_Wove<-PCB_DPs_sum[2,3]/Tot_Ps[20,3]
  PCB_TotPer_Wout<-PCB_DPs_sum[2,5]/Tot_Ps[20,3]
  PCB_TotPer_Coy<-PCB_DPs_sum[2,7]/Tot_Ps[20,3] 
  PCB_TotPer_GCC<-PCB_DPs_sum[2,9]/Tot_Ps[20,3]
  PCB_TotPers<-c(PCB_TotPer_Pred,PCB_TotPer_Wove,PCB_TotPer_Wout,PCB_TotPer_Coy, PCB_TotPer_GCC)
  Tot_Pers_DPs["PCB",]<-PCB_TotPers  
    
##8: SJ
  for(i in data_year){
    SJ_pred_yr<-SJ_pred[SJ_pred$Year==i,]
    SJ_wove_yr<-SJ_wove[SJ_wove$Year==i,]
    SJ_wou_yr<-SJ_wou[SJ_wou$Year==i,]
    SJ_coy_yr<-SJ_coy[SJ_coy$Year==i,] 
    SJ_gcc_yr<-SJ_gcc[SJ_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["SJ", "ECS_UN"]
    DP_ES_un<-data_tot_means["SJ","ES_UN"]
    DP_P<-SJ_Ps[i-2011, "P"]
    
    N_Pred<-SJ_Summary[i-2011,"N_Pred"]
    N_Wove<-SJ_Summary[i-2011,"N_WOver"]
    N_Wou<-SJ_Summary[i-2011, "N_WOut"]
    N_coy<-SJ_Summary[i-2011, "N_C"] 
    N_gcc<-SJ_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(SJ_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(SJ_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(SJ_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(SJ_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    SJ_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    SJ_DPs_tot[i-2011,]<-SJ_DPs
  }
  
#store as data frame
  SJ_DPs_tot<-as.data.frame(SJ_DPs_tot)
  rownames(SJ_DPs_tot)<-data_yr
  colnames(SJ_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  SJ_DPs_tot
  
#create data frame of summary statistics for SJ
  SJ_DPs_sum<-as.data.frame(apply(SJ_DPs_tot, 2, sumstats))
  SJ_DPs_sum
  
  #calculate Percents based on annual means
  SJ_TotPer_Pred<-SJ_DPs_sum[2,1]/Tot_Ps[23,3]
  SJ_TotPer_Wove<-SJ_DPs_sum[2,3]/Tot_Ps[23,3]
  SJ_TotPer_Wout<-SJ_DPs_sum[2,5]/Tot_Ps[23,3]
  SJ_TotPer_Coy<-SJ_DPs_sum[2,7]/Tot_Ps[23,3] 
  SJ_TotPer_GCC<-SJ_DPs_sum[2,9]/Tot_Ps[23,3]
  SJ_TotPers<-c(SJ_TotPer_Pred,SJ_TotPer_Wove,SJ_TotPer_Wout,SJ_TotPer_Coy, SJ_TotPer_GCC)
  Tot_Pers_DPs["SJ",]<-SJ_TotPers
  
##9: STG
  for(i in data_year){
    STG_pred_yr<-STG_pred[STG_pred$Year==i,]
    STG_wove_yr<-STG_wove[STG_wove$Year==i,]
    STG_wou_yr<-STG_wou[STG_wou$Year==i,]
    STG_coy_yr<-STG_coy[STG_coy$Year==i,] 
    STG_gcc_yr<-STG_gcc[STG_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["STG", "ECS_UN"]
    DP_ES_un<-data_tot_means["STG","ES_UN"]
    DP_P<-STG_Ps[i-2011, "P"]
    
    N_Pred<-STG_Summary[i-2011,"N_Pred"]
    N_Wove<-STG_Summary[i-2011,"N_WOver"]
    N_Wou<-STG_Summary[i-2011, "N_WOut"]
    N_coy<-STG_Summary[i-2011, "N_C"] 
    N_gcc<-STG_Summary[i-2011, "N_GCC"]
    
    ES_Pred<-round(mean(STG_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(STG_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(STG_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(STG_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    STG_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    STG_DPs_tot[i-2011,]<-STG_DPs
  }
  
#store as data frame
  STG_DPs_tot<-as.data.frame(STG_DPs_tot)
  rownames(STG_DPs_tot)<-data_yr
  colnames(STG_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  STG_DPs_tot
  
#create data frame of summary statistics for STG
  STG_DPs_sum<-as.data.frame(apply(STG_DPs_tot, 2, sumstats))
  STG_DPs_sum
  
  #calculate Percents based on annual means
  STG_TotPer_Pred<-STG_DPs_sum[2,1]/Tot_Ps[26,3]
  STG_TotPer_Wove<-STG_DPs_sum[2,3]/Tot_Ps[26,3]
  STG_TotPer_Wout<-STG_DPs_sum[2,5]/Tot_Ps[26,3]
  STG_TotPer_Coy<-STG_DPs_sum[2,7]/Tot_Ps[26,3] 
  STG_TotPer_GCC<-STG_DPs_sum[2,9]/Tot_Ps[26,3]
  STG_TotPers<-c(STG_TotPer_Pred,STG_TotPer_Wove,STG_TotPer_Wout,STG_TotPer_Coy, STG_TotPer_GCC)
  Tot_Pers_DPs["STG",]<-STG_TotPers  
  
##10: STGISP
  for(i in data_year){
    STGISP_pred_yr<-STGISP_pred[STGISP_pred$Year==i,]
    STGISP_wove_yr<-STGISP_wove[STGISP_wove$Year==i,]
    STGISP_wou_yr<-STGISP_wou[STGISP_wou$Year==i,]
    STGISP_coy_yr<-STGISP_coy[STGISP_coy$Year==i,] 
    STGISP_gcc_yr<-STGISP_gcc[STGISP_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["STGISP", "ECS_UN"]
    DP_ES_un<-data_tot_means["STGISP","ES_UN"]
    DP_P<-STGISP_Ps[i-2011, "P"]
    
    N_Pred<-STGISP_Summary[i-2011,"N_Pred"]
    N_Wove<-STGISP_Summary[i-2011,"N_WOver"]
    N_Wou<-STGISP_Summary[i-2011, "N_WOut"]
    N_coy<-STGISP_Summary[i-2011, "N_C"] 
    N_gcc<-STGISP_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(STGISP_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(STGISP_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(STGISP_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(STGISP_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    STGISP_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    STGISP_DPs_tot[i-2011,]<-STGISP_DPs
  }
  
#store as data frame
  STGISP_DPs_tot<-as.data.frame(STGISP_DPs_tot)
  rownames(STGISP_DPs_tot)<-data_yr
  colnames(STGISP_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  STGISP_DPs_tot
  
#create data frame of summary statistics for STGISP
  STGISP_DPs_sum<-as.data.frame(apply(STGISP_DPs_tot, 2, sumstats))
  STGISP_DPs_sum

  #calculate Percents based on annual means
  STGISP_TotPer_Pred<-STGISP_DPs_sum[2,1]/Tot_Ps[29,3]
  STGISP_TotPer_Wove<-STGISP_DPs_sum[2,3]/Tot_Ps[29,3]
  STGISP_TotPer_Wout<-STGISP_DPs_sum[2,5]/Tot_Ps[29,3]
  STGISP_TotPer_Coy<-STGISP_DPs_sum[2,7]/Tot_Ps[29,3] 
  STGISP_TotPer_GCC<-STGISP_DPs_sum[2,9]/Tot_Ps[29,3]
  STGISP_TotPers<-c(STGISP_TotPer_Pred,STGISP_TotPer_Wove,STGISP_TotPer_Wout,STGISP_TotPer_Coy, STGISP_TotPer_GCC)
  Tot_Pers_DPs["STGISP",]<-STGISP_TotPers  
  
##11: STJSP
  for(i in data_year){
    STJSP_pred_yr<-STJSP_pred[STJSP_pred$Year==i,]
    STJSP_wove_yr<-STJSP_wove[STJSP_wove$Year==i,]
    STJSP_wou_yr<-STJSP_wou[STJSP_wou$Year==i,]
    STJSP_coy_yr<-STJSP_coy[STJSP_coy$Year==i,] 
    STJSP_gcc_yr<-STJSP_gcc[STJSP_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["STJSP", "ECS_UN"]
    DP_ES_un<-data_tot_means["STJSP","ES_UN"]
    DP_P<-STJSP_Ps[i-2011, "P"]
    
    N_Pred<-STJSP_Summary[i-2011,"N_Pred"]
    N_Wove<-STJSP_Summary[i-2011,"N_WOver"]
    N_Wou<-STJSP_Summary[i-2011, "N_WOut"]
    N_coy<-STJSP_Summary[i-2011, "N_C"] 
    N_gcc<-STJSP_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(STJSP_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(STJSP_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(STJSP_coy_yr$ES.Overall, na.rm=T),2) 
    ES_gcc<-round(mean(STJSP_gcc_yr$ES.Overall, na.rm=T),2) 
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0) 
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    STJSP_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    STJSP_DPs_tot[i-2011,]<-STJSP_DPs
  }
  
#store as data frame
  STJSP_DPs_tot<-as.data.frame(STJSP_DPs_tot)
  rownames(STJSP_DPs_tot)<-data_yr
  colnames(STJSP_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  STJSP_DPs_tot
  
#create data frame of summary statistics for STJSP
  STJSP_DPs_sum<-as.data.frame(apply(STJSP_DPs_tot, 2, sumstats))
  STJSP_DPs_sum

#calculate Percents based on annual means
  STJSP_TotPer_Pred<-STJSP_DPs_sum[2,1]/Tot_Ps[32,3]
  STJSP_TotPer_Wove<-STJSP_DPs_sum[2,3]/Tot_Ps[32,3]
  STJSP_TotPer_Wout<-STJSP_DPs_sum[2,5]/Tot_Ps[32,3]
  STJSP_TotPer_Coy<-STJSP_DPs_sum[2,7]/Tot_Ps[32,3] 
  STJSP_TotPer_GCC<-STJSP_DPs_sum[2,9]/Tot_Ps[32,3]
  STJSP_TotPers<-c(STJSP_TotPer_Pred,STJSP_TotPer_Wove,STJSP_TotPer_Wout,STJSP_TotPer_Coy, STJSP_TotPer_GCC)
  Tot_Pers_DPs["STJSP",]<-STJSP_TotPers  
  
##12: STVNWR
  for(i in data_year){
    STVNWR_pred_yr<-STVNWR_pred[STVNWR_pred$Year==i,]
    STVNWR_wove_yr<-STVNWR_wove[STVNWR_wove$Year==i,]
    STVNWR_wou_yr<-STVNWR_wou[STVNWR_wou$Year==i,]
    STVNWR_coy_yr<-STVNWR_coy[STVNWR_coy$Year==i,] 
    STVNWR_gcc_yr<-STVNWR_gcc[STVNWR_gcc$Year==i,] 
    
    DP_ECS<-data_tot_means["STVNWR", "ECS_UN"]
    DP_ES_un<-data_tot_means["STVNWR","ES_UN"]
    DP_P<-STVNWR_Ps[i-2011, "P"]
    
    N_Pred<-STVNWR_Summary[i-2011,"N_Pred"]
    N_Wove<-STVNWR_Summary[i-2011,"N_WOver"]
    N_Wou<-STVNWR_Summary[i-2011, "N_WOut"]
    N_coy<-STVNWR_Summary[i-2011, "N_C"]
    N_gcc<-STVNWR_Summary[i-2011, "N_GCC"] 
    
    ES_Pred<-round(mean(STVNWR_pred_yr$ES.Overall, na.rm=T),2)
    ES_Wove<-round(mean(STVNWR_wove_yr$ES.Overall, na.rm=T),2)
    Es_Wou<-0
    ES_coy<-round(mean(STVNWR_coy_yr$ES.Overall, na.rm=T),2)
    ES_gcc<-round(mean(STVNWR_gcc_yr$ES.Overall, na.rm=T),2)
    
    P_Pred<-round(DP_ECS*N_Pred*ES_Pred,0)
    P_Pred_un<-round(DP_ECS*N_Pred*DP_ES_un,0)
    P_Wove<-round(DP_ECS*N_Wove*ES_Wove,0)
    P_Wove_un<-round(DP_ECS*N_Wove*DP_ES_un,0)
    P_Wove_un
    P_Wou<-0
    P_Wou_un<-round(DP_ECS*N_Wou*DP_ES_un,0)
    P_coy<-round(DP_ECS*N_coy*ES_coy,0) 
    P_coy_un<-round(DP_ECS*N_coy*DP_ES_un,0) 
    P_gcc<-round(DP_ECS*N_gcc*ES_gcc,0)
    P_gcc_un<-round(DP_ECS*N_gcc*DP_ES_un,0) 
    
    D_Pred<-P_Pred_un-P_Pred
    Per_Pred<-round(100*(D_Pred/DP_P),2)
    D_Wove<-P_Wove_un-P_Wove
    Per_Wove<-round(100*(D_Wove/DP_P),2)
    D_Wou<-P_Wou_un-P_Wou
    Per_Wou<-round(100*(D_Wou/DP_P),2)
    D_coy<-P_coy_un-P_coy 
    Per_coy<-round(100*(D_coy/DP_P),2) 
    D_gcc<-P_gcc_un-P_gcc 
    Per_gcc<-round(100*(D_gcc/DP_P),2) 
    STVNWR_DPs<-c(D_Pred, Per_Pred, D_Wove, Per_Wove, D_Wou, Per_Wou, D_coy, Per_coy, D_gcc, Per_gcc)
    STVNWR_DPs_tot[i-2011,]<-STVNWR_DPs
  }
  
  #store as data frame
  STVNWR_DPs_tot<-as.data.frame(STVNWR_DPs_tot)
  rownames(STVNWR_DPs_tot)<-data_yr
  colnames(STVNWR_DPs_tot)<-c("D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")
  STVNWR_DPs_tot
  
  #create data frame of summary statistics for STVNWR
  STVNWR_DPs_sum<-as.data.frame(apply(STVNWR_DPs_tot, 2, sumstats))
  STVNWR_DPs_sum
  
  #calculate Percents based on annual means
  STVNWR_TotPer_Pred<-STVNWR_DPs_sum[2,1]/Tot_Ps[35,3]
  STVNWR_TotPer_Wove<-STVNWR_DPs_sum[2,3]/Tot_Ps[35,3]
  STVNWR_TotPer_Wout<-STVNWR_DPs_sum[2,5]/Tot_Ps[35,3]
  STVNWR_TotPer_Coy<-STVNWR_DPs_sum[2,7]/Tot_Ps[35,3]
  STVNWR_TotPer_GCC<-STVNWR_DPs_sum[2,9]/Tot_Ps[35,3]
  STVNWR_TotPers<-c(STVNWR_TotPer_Pred,STVNWR_TotPer_Wove,STVNWR_TotPer_Wout,STVNWR_TotPer_Coy, STVNWR_TotPer_GCC)
  Tot_Pers_DPs["STVNWR",]<-STVNWR_TotPers

##aggregate all site data and write CSV

Tot_DPs<-as.data.frame(rbind(CSB_DPs_sum, CSTGI_DPs_sum, DI_DPs_sum,FM_DPs_sum, GS_DPs_sum, OB_DPs_sum,PCB_DPs_sum,SJ_DPs_sum, STG_DPs_sum,STGISP_DPs_sum, STJSP_DPs_sum, STVNWR_DPs_sum))
Tot_DPs$SITE<-c("CSB","CSB","CSB", "CSTGI","CSTGI","CSTGI", "DI","DI","DI","FM","FM","FM","GS","GS","GS", "OB", "OB","OB","PCB","PCB","PCB","SJ","SJ","SJ","STG","STG","STG","STGISP","STGISP","STGISP","STJSP","STJSP","STJSP","STVNWR","STVNWR","STVNWR")
Tot_DPs<-Tot_DPs[,c("SITE", "D_Pred","%_Pred","D_Wove", "%_Wove", "D_Wou", "%_Wou", "D_Coy", "%_Coy", "D_GCC", "%_GCC")]
#Tot DPs is the average of 7 years at each site
write.csv(Tot_DPs, "Tot_DPs.csv")
#Tot Pers DPs is 
write.csv(Tot_Pers_DPs, "Tot_Pers_DPs.csv")

##Wash Over ES

CSB_ES_Wove<-round(mean(CSB_wove$ES.Overall, na.rm=T),3)
CSB_ES_WOVE_SD<-round(sd(CSB_wove$ES.Overall, na.rm=T),3)
CSTGI_ES_Wove<-round(mean(CSTGI_wove$ES.Overall, na.rm=T),3)
CSTGI_ES_WOVE_SD<-round(sd(CSTGI_wove$ES.Overall, na.rm=T),3)
DI_ES_Wove<-round(mean(DI_wove$ES.Overall, na.rm=T),3)
DI_ES_WOVE_SD<-round(sd(DI_wove$ES.Overall, na.rm=T),3)
FM_ES_Wove<-round(mean(FM_wove$ES.Overall, na.rm=T),3)
FM_ES_WOVE_SD<-round(sd(FM_wove$ES.Overall, na.rm=T),3)
GS_ES_Wove<-round(mean(GS_wove$ES.Overall, na.rm=T),3)
GS_ES_WOVE_SD<-round(sd(GS_wove$ES.Overall, na.rm=T),3)
OB_ES_Wove<-round(mean(OB_wove$ES.Overall, na.rm=T),3)
OB_ES_WOVE_SD<-round(sd(OB_wove$ES.Overall, na.rm=T),3)
PCB_ES_Wove<-round(mean(PCB_wove$ES.Overall, na.rm=T),3)
PCB_ES_WOVE_SD<-round(sd(PCB_wove$ES.Overall, na.rm=T),3)
SJ_ES_Wove<-round(mean(SJ_wove$ES.Overall, na.rm=T),3)
SJ_ES_WOVE_SD<-round(sd(SJ_wove$ES.Overall, na.rm=T),3)
STG_ES_Wove<-round(mean(STG_wove$ES.Overall, na.rm=T),3)
STG_ES_WOVE_SD<-round(sd(STG_wove$ES.Overall, na.rm=T),3)
STGISP_ES_Wove<-round(mean(STGISP_wove$ES.Overall, na.rm=T),3)
STGISP_ES_WOVE_SD<-round(sd(STGISP_wove$ES.Overall, na.rm=T),3)
STJSP_ES_Wove<-round(mean(STJSP_wove$ES.Overall, na.rm=T),3)
STJSP_ES_WOVE_SD<-round(sd(STJSP_wove$ES.Overall, na.rm=T),3)
STVNWR_ES_Wove<-round(mean(STVNWR_wove$ES.Overall, na.rm=T),3)
STVNWR_ES_WOVE_SD<-round(sd(STVNWR_wove$ES.Overall, na.rm=T),3)
Data_ES_Wove<-round(mean(Data_Wove$ES.Overall, na.rm=T),3)
Data_ES_WOVE_SD<-round(sd(Data_Wove$ES.Overall, na.rm=T),3)
ES_Vals<-c(CSB_ES_Wove, CSB_ES_WOVE_SD, CSTGI_ES_Wove, CSTGI_ES_WOVE_SD, DI_ES_Wove, DI_ES_WOVE_SD, FM_ES_Wove, FM_ES_WOVE_SD, GS_ES_Wove, GS_ES_WOVE_SD, OB_ES_Wove, OB_ES_WOVE_SD, PCB_ES_Wove, PCB_ES_WOVE_SD, SJ_ES_Wove, SJ_ES_WOVE_SD, STG_ES_Wove, STG_ES_WOVE_SD, STGISP_ES_Wove, STGISP_ES_WOVE_SD, STJSP_ES_Wove, STJSP_ES_WOVE_SD, STVNWR_ES_Wove, STVNWR_ES_WOVE_SD, Data_ES_Wove, Data_ES_WOVE_SD)
ES_Vals

##predation
CSB_ES_gcc<-round(mean(CSB_gcc$ES.Overall, na.rm=T),3)
CSB_ES_gcc_SD<-round(sd(CSB_gcc$ES.Overall, na.rm=T),3)
CSTGI_ES_gcc<-round(mean(CSTGI_gcc$ES.Overall, na.rm=T),3)
CSTGI_ES_gcc_SD<-round(sd(CSTGI_gcc$ES.Overall, na.rm=T),3)
DI_ES_gcc<-round(mean(DI_gcc$ES.Overall, na.rm=T),3)
DI_ES_gcc_SD<-round(sd(DI_gcc$ES.Overall, na.rm=T),3)
FM_ES_gcc<-round(mean(FM_gcc$ES.Overall, na.rm=T),3)
FM_ES_gcc_SD<-round(sd(FM_gcc$ES.Overall, na.rm=T),3)
GS_ES_gcc<-round(mean(GS_gcc$ES.Overall, na.rm=T),3)
GS_ES_gcc_SD<-round(sd(GS_gcc$ES.Overall, na.rm=T),3)
OB_ES_gcc<-round(mean(OB_gcc$ES.Overall, na.rm=T),3)
OB_ES_gcc_SD<-round(sd(OB_gcc$ES.Overall, na.rm=T),3)
PCB_ES_gcc<-round(mean(PCB_gcc$ES.Overall, na.rm=T),3)
PCB_ES_gcc_SD<-round(sd(PCB_gcc$ES.Overall, na.rm=T),3)
SJ_ES_gcc<-round(mean(SJ_gcc$ES.Overall, na.rm=T),3)
SJ_ES_gcc_SD<-round(sd(SJ_gcc$ES.Overall, na.rm=T),3)
STG_ES_gcc<-round(mean(STG_gcc$ES.Overall, na.rm=T),3)
STG_ES_gcc_SD<-round(sd(STG_gcc$ES.Overall, na.rm=T),3)
STGISP_ES_gcc<-round(mean(STGISP_gcc$ES.Overall, na.rm=T),3)
STGISP_ES_gcc_SD<-round(sd(STGISP_gcc$ES.Overall, na.rm=T),3)
STJSP_ES_gcc<-round(mean(STJSP_gcc$ES.Overall, na.rm=T),3)
STJSP_ES_gcc_SD<-round(sd(STJSP_gcc$ES.Overall, na.rm=T),3)
STVNWR_ES_gcc<-round(mean(STVNWR_gcc$ES.Overall, na.rm=T),3)
STVNWR_ES_gcc_SD<-round(sd(STVNWR_gcc$ES.Overall, na.rm=T),3)
Data_ES_gcc<-round(mean(Data_gcc$ES.Overall, na.rm=T),3)
Data_ES_gcc_SD<-round(sd(Data_gcc$ES.Overall, na.rm=T),3)
ES_Vals_gcc<-c(CSB_ES_gcc, CSB_ES_gcc_SD, CSTGI_ES_gcc, CSTGI_ES_gcc_SD, DI_ES_gcc, DI_ES_gcc_SD, FM_ES_gcc, FM_ES_gcc_SD, GS_ES_gcc, GS_ES_gcc_SD, OB_ES_gcc, OB_ES_gcc_SD, PCB_ES_gcc, PCB_ES_gcc_SD, SJ_ES_gcc, SJ_ES_gcc_SD, STG_ES_gcc, STG_ES_gcc_SD, STGISP_ES_gcc, STGISP_ES_gcc_SD, STJSP_ES_gcc, STJSP_ES_gcc_SD, STVNWR_ES_gcc, STVNWR_ES_gcc_SD, Data_ES_gcc, Data_ES_gcc_SD)
ES_Vals_gcc

##Plot for Diss Def
pngm<-read.csv("PGOM_6_11_23.csv")
pngm$Year<-as.factor(pngm$Year)
summary(pngm)

pngm%>%
  dplyr::filter(Group=="Brost et al. 2015")%>%
  ggplot(aes(x=Year, y=Hatchling.Production, color=Group, fill=Group))+
  geom_bar(stat="identity", position="dodge")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=14,angle=45, hjust =1),
        axis.text.y = element_text(size=14),
        legend.title=element_blank(), 
        legend.text=element_text(size=12))+
  scale_x_discrete(limit=c("2002", "2003", "2004", "2005","2006", "2007", "2008", "2009", "2010", "2011", "2012"))+
  #scale_y_continuous(limits=c(0,110000),
                     #breaks=c(0, 30000,60000,90000),
                     #labels=c("0", "30000", "60000", "90000"))+
  geom_hline(aes(yintercept=33625), linetype="dashed", linewidth=2, color=c("#F8766D"))
  
pngm%>%ggplot(aes(x=Year, y=Hatchling.Production, color=Group, fill=Group))+
  geom_bar(stat="identity", position="dodge")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=14,angle=45, hjust =1),
        axis.text.y = element_text(size=14),
        legend.title=element_blank(), 
        legend.text=element_text(size=12))+
  scale_y_continuous(limits=c(0,110000),
                    breaks=c(0, 30000,60000,90000),
                    labels=c("0", "30000", "60000", "90000"))+
  geom_hline(aes(yintercept=33625), linetype="dashed", linewidth=2, color=c("#F8766D"))+
  geom_hline(aes(yintercept=78306), linetype="dashed", linewidth=2, color=c("#00BFC4"))

