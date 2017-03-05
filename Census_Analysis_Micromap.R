# 1. Calling the required library

library(micromapST)

# 2. A data frame preparation function   

## Run

microFull2Ab <- function(stateDF,stateId=NULL,
                         ref=stateNamesFips){
      if(is.null(stateId)) nam <- row.names(stateDF) else
            nam <- stateDF[,stateId]
      nam <- ifelse(nam=="District of Columbia","D.C.",nam)
      check <-  match(nam,row.names(ref)) 
      bad <- is.na(check)
      good <- !bad
      nbad <- sum(bad)
      if(nbad>0){
            warning(paste(nbad,"Unmatch Names Removed",nam[bad])) 
            stateDF <- stateDF[!bad,]
            nam <- nam[!bad]
            check <- check[!bad]
            good <- good[!bad]
      }
      ngood <- sum(good)
      if(ngood < 51)warning(paste("Only",ngood,"State Ids"))
      row.names(stateDF) <- ref[check,2]
      return(stateDF)
}

## End

# 3. Creating dataset

census <- read.csv(file="US_Census_Statistics.csv",
                 header=T, as.is=TRUE)


head(census)
summary(census)

# Prepare a suitable data input data.frame

census1 <- microFull2Ab(census,"State")
head(census1)


## End

# 4. Creating a layout for the report

## Run

stateDf <- census1
nam<- colnames(stateDf)
names(nam) =1:length(nam)
nam

panelDesc <- data.frame(
      type=c("mapcum","id","arrow","bar", "dot"),		
      lab1=c("","","Population in","% Change in GDP from", "% Change in Crime Rate from"),       
      lab2=c("","","2000 and 2010","2000 to 2010", "2000 to 2010"),  
      col1=c(NA,NA,2,10,15), 		
      col2=c(NA,NA,3,NA,NA)		
)

# 5.1 Creating a micromap to see a pattern in Population change to GDP change from 2000-2010

png(file="Population-Change-To-GDP-Change.png",width=7.5,height=10,units="in",res=300)
micromapST(census1,panelDesc,sortVar=3,ascend=FALSE,
           title=c("Change in population to GDP and Crime Index Change",
                   "from 2000 to 2010")) 
dev.off()

# 5.2 Scatter Plots for comparing 2000-2010
head(census1)

panelDesc2 <- data.frame(                 
      type=c("mapmedian","id","scatdot","scatdot"),  
      lab1=c("","","Population Change to ","Population Change to"),   
      lab2=c("","","GDP Change from 2000-2010","GDP Change 2010-2014"),
      lab3=c("","","% Change in Population", "% Change in Population"),
      lab4=c("","","% Change GDP","% Change GDP"),	
      col1=c(NA,NA,5,6),                 
      col2=c(NA,NA,10,11)		
)
ExTitle <- c( "Scatter Plots Comparing 2000 to 2014",
              "Population rise/fall factors")
png(file="Scatter Plots for comparing 2000-2010.png",width=7.5,height=10,units="in",res=300)
micromapST(census1,panelDesc2,sortVar=5,ascend=FALSE,title=ExTitle)  
dev.off()

pdf(file="Scatter Plots for comparing 2000-2010.pdf",width=7.5,height=10)
micromapST(census1,panelDesc2,sortVar=5,ascend=FALSE,title=ExTitle)  
dev.off()

# 5.3 Scatter Plots for comparing 2000-2010
head(census1)

panelDesc3 <- data.frame(                 
      type=c("maptail","id","bar","dot", "dot"),  
      lab1=c("","","Net Migration","Average Weekly","CO2 Emissions"),   
      lab2=c("","","","Wages",""),
      col1=c(NA,NA,31,18,22)
)
ExTitle <- c( "Net Migration influencing factors")
png(file="Scatter Plots for Net Migration.png",width=7.5,height=10,units="in",res=300)
micromapST(census1,panelDesc3,sortVar=31,ascend=FALSE,title=ExTitle)  
dev.off()

panelDesc4 <- data.frame(                 
      type=c("maptail","id","bar","dot", "dot", "bar"),  
      lab1=c("","","Net Migration","Average Weekly","CO2 Emissions","Mean Income"),   
      lab2=c("","","","Wages","",""),
      col1=c(NA,NA,31,18,22,41)
)
ExTitle <- c( "Net Migration influencing factors")
png(file="Scatter Plots for Net Migration2.png",width=7.5,height=10,units="in",res=300)
micromapST(census1,panelDesc4,sortVar=31,ascend=FALSE,title=ExTitle)  
dev.off()

pdf(file="Scatter Plots for comparing 2000-2010.pdf",width=7.5,height=10)
micromapST(census1,panelDesc2,sortVar=5,ascend=FALSE,title=ExTitle)  
dev.off()
