library(readxl)
GCIPrawdata <- read_excel("~/USI/TESI/GCIPrawdata.xlsx")
#Source: Global Consumption and Income Project. All incomes expressed in 2005 USD PPP.
#https://jackblun.github.io/Globalinc/


# # Start by creating the matrix hosting various inequality measures
Inequality_Measures <- matrix(data = NA, nrow = nrow(GCIPrawdata), ncol = 6)

colnames(Inequality_Measures) <- c("Country", "Year", "50/10 Ratio", "90/50 Ratio", "90/10 Ratio", "GINI Index")

Inequality_Measures[,c("Country", "Year")] <- as.matrix(GCIPrawdata[,c("Country", "Year")]) 


# # Calculating the 50/10 Ratio, 90/50 Ratio and 90/10 Ratio, and adding them to the Inequality Measures matrix in the corresponding column
Inequality_Measures[,"50/10 Ratio"] <- as.matrix(GCIPrawdata[,"Decile 5 Income"]/GCIPrawdata["Decile 1 Income"])

Inequality_Measures[,"90/50 Ratio"] <- as.matrix(GCIPrawdata[,"Decile 9 Income"]/GCIPrawdata["Decile 5 Income"])

Inequality_Measures[,"90/10 Ratio"] <- as.matrix(GCIPrawdata[,"Decile 9 Income"]/GCIPrawdata["Decile 1 Income"])


# # The computation of the GINI Index requires multiple steps

# Step 1: Creation of an auxiliary matrix to exploit for computations
Decile_Income <- as.matrix(GCIPrawdata[,c(3:12)])

# Step 2: Computation of the relative cumulative income per decile of the distribution
Cumulative_Income <- matrix(data = NA, nrow = nrow(Decile_Income), ncol = ncol(Decile_Income))

for (k in 1:nrow(Cumulative_Income)) {
  
  #Given the structure of the decile matrix, this cycle is iterated on every row
  for (i in 1:ncol(Cumulative_Income)) {
   
     #Per each decile, we calculate the relative cumulative income as the sum of the cumulation divided per the total
    Cumulative_Income[k,i] <- sum(Decile_Income[k,1:i]) / sum(Decile_Income[k,1:ncol(Decile_Income)]) 
    
  }
}

# Step 3: The cumulative income matrix can be complited uniting country names and years, and finally named
Cumulative_Income_Complete <- cbind(as.matrix(GCIPrawdata[,c("Country", "Year")]),Cumulative_Income)
colnames(Cumulative_Income_Complete) <- c("Country", "Year","Cumulative Decile 1 Income", "Cumulative Decile 2 Income",
                                          "Cumulative Decile 3 Income","Cumulative Decile 4 Income","Cumulative Decile 5 Income",
                                          "Cumulative Decile 6 Income","Cumulative Decile 7 Income","Cumulative Decile 8 Income",
                                          "Cumulative Decile 9 Income","Cumulative Decile 10 Income")

# Step 4: Computation of the GINI Index
for (j in 1:nrow(Cumulative_Income)) {
 
  #Given the structure of the decile matrix, this cycle is iterated on every row
   
  #Creation of an auxiliary vector. It will contain the total area underlying the Lorenz Curve
   vector <- array(data = NA, dim = 10)
  
    #First decile is given
   vector[1] <- as.numeric(Cumulative_Income[j,1]) * 0.1 / 2
  
    for(m in 2:10) {
    
        ##Per each decile, we calculate the underlying area as if an right-angled trapezium
      vector[m] <- (as.numeric(Cumulative_Income[j,m])+as.numeric(Cumulative_Income[j,m-1])) * 0.1 / 2
   
       }
  
   #Finally, the GINI Index is computed finding the area between the Line of Perfect Equality and the Lorenz Curve
  Inequality_Measures[j,"GINI Index"] <-  ( 0.5 - sum(vector) ) / 0.5 

}

# # Next, modelling a country inequality analysis leveraging on computed inequality measures 
Country_Inequality_Analysis <- function(Country){
  
  #Different vectors will contain the relevant metrics for the given country
  Year <- Inequality_Measures[Inequality_Measures[,1]==Country,"Year"]
  Bottom_Ratio <- Inequality_Measures[Inequality_Measures[,1]==Country,"50/10 Ratio"]
  Top_Ratio <- Inequality_Measures[Inequality_Measures[,1]==Country,"90/50 Ratio"]
  Top_Bottom_Ratio <- Inequality_Measures[Inequality_Measures[,1]==Country,"90/10 Ratio"]
  GINI_Index <- Inequality_Measures[Inequality_Measures[,1]==Country,"GINI Index"]
  
  #Visual representation of the relevant metrics and their evolution over time
  plot(Year, Bottom_Ratio, type="o", pch=1, col="red", lty=1,  main = paste("Inequality Ratios Variation for", Country,Year[1],"-",Year[length(Year)]),
      ylab="Ratio Value", xlab=NULL, ylim = range(as.numeric(c(Bottom_Ratio,Top_Ratio,Top_Bottom_Ratio))))
  lines(Year, Top_Ratio,type="o", pch=0, lty=1, col="green")
  lines(Year, Top_Bottom_Ratio, type="o", pch=2, lty=1, col="blue")
  legend("bottom", legend=c("90/10 Ratio","90/50 Ratio","50/10 Ratio"),
         pch=c(2,0,1), lty=1, xpd=TRUE, inset  = c(0, -0.275), horiz = TRUE, col=c("blue","green","red"))
  
  #Second plot exclusive for the GINI Index
  plot(Year, GINI_Index, type="o", pch=1, lty=1,  main = paste("Gini Index Variation for", Country, Year[1],"-",Year[length(Year)]),
       ylab="GINI Index Value", xlab=NULL, ylim = range(as.numeric(GINI_Index)), col="blue")
  
  #Preparation of values for the trend analysis
  if(Top_Bottom_Ratio[length(Top_Bottom_Ratio)] > Top_Bottom_Ratio[1]){
    Top_Bottom_Ratio_Direction <- "Increased"} 
  else {Top_Bottom_Ratio_Direction <- "Decreased"}
  
  if(Top_Ratio[length(Top_Ratio)] > Top_Ratio[1]){
    Top_Ratio_Direction <- "Increased"} 
  else {Top_Ratio_Direction <- "Decreased"}
  
  if(Bottom_Ratio[length(Bottom_Ratio)] > Bottom_Ratio[1]){
    Bottom_Ratio_Direction <- "Increased"} 
  else {Bottom_Ratio_Direction <- "Decreased"}
  
  if(GINI_Index[length(GINI_Index)] > GINI_Index[1]){
    GINI_Index_Direction <- "Increased"} 
  else {GINI_Index_Direction <- "Decreased"}
  
  #Finalization and conclusion of the analysis with a concluding statement 
  Text<-paste("In", Country, ", the 90/10 Inequality Ratio", Top_Bottom_Ratio_Direction, "from ", round(as.numeric(Top_Bottom_Ratio[1]),digits = 2), " to ", round(as.numeric(Top_Bottom_Ratio[length(Top_Bottom_Ratio)]),digits = 2),
              ", the 90/50 Inequality Ratio", Top_Ratio_Direction,  "from ", round(as.numeric(Top_Ratio[1]),digits = 2), " to ", round(as.numeric(Top_Ratio[length(Top_Ratio)]),digits = 2),
              " and the 50/10 Inequality Ratio", Top_Ratio_Direction, "from ", round(as.numeric(Bottom_Ratio[1]),digits = 2), " to ", round(as.numeric(Bottom_Ratio[length(Bottom_Ratio)]),digits = 2),
              ". Finally, the Gini Index", GINI_Index_Direction, "from ", round(as.numeric(GINI_Index[1]),digits = 2), " to ", round(as.numeric(GINI_Index[length(GINI_Index)]),digits = 2))
  
  return(Text)

}

# # The function allows to grasp inequality trends per a given country
Country_Inequality_Analysis("Italy")


# # Afterwards, modelling a country comparative inequality analysis leveraging on computed inequality measures
Countries_Comparison <- function(countries, ratio, average = FALSE) {
  
  #The Year vector will contain the relevant period for the given countries
  Year <- Inequality_Measures[Inequality_Measures[,1]==countries[1],"Year"]
  
  #Creation of an auxiliary matrix to exploit for computations and visualization
  Countries_matrix <- matrix(data = NA, nrow = length(countries), ncol = length(Year))
  colnames(Countries_matrix) <- c(Year[1:length(Year)])
  rownames(Countries_matrix) <- c(countries[1:length(countries)])
  
  for (f in 1:length(countries)) {
    #A loops transcribes the requested values from the Inequality Measures matrix to our auxiliary Countries matrix
      Countries_matrix[f,(1:length(Year))] <- Inequality_Measures[Inequality_Measures[,1]==countries[f],ratio]
  }
  
  #The computation of the mean can be helpful to follow the average trend of a specific selection of country
  if(average == TRUE){
    mean_vec <- array(data = NA, dim = ncol(Countries_matrix))
  
  for (h in 1:ncol(Countries_matrix)) {
    #The mean is calculated as the arithmetic mean of the countries' observations per each year
    mean_vec[h]<-mean(as.numeric(Countries_matrix[,h]))
  }
  }
  
  #Colors are determined beforehand for coherency throughout the report - palette can always be changed
  cols <- rainbow(length(countries))
  
  #Visual representation comparing evolution of the relevant values over time
  matplot(as.numeric(as.character(Year)),t(Countries_matrix), type="l", pch=1, lty=1,  main = paste(ratio, "Variation for Selected Countries", Year[1],"-",Year[length(Year)]),
       ylab=ratio, xlab=NULL, ylim = range(as.numeric(Countries_matrix)), xaxt = "n", col= cols, lwd = 2)
      axis(side=1, Year)
      
    legend("bottom", legend=c(countries),lty=1, xpd=TRUE, inset  = c(0, -0.275), horiz = TRUE, col=c(cols))
  
    if(average == TRUE){
      lines(Year, mean_vec,type="o", pch=1, lty=1, lwd=1, col="black")
      legend("topleft", legend = "Average", pch = 1)
    }
    
    #Preparation of values for the trend analysis
    Direction <- array(data = NA, dim = length(countries))  
    for(p in 1:length(countries)){
      if (Countries_matrix[p,ncol(Countries_matrix)] > Countries_matrix[p,1]) {
        Direction[p] <- paste("In", countries[p], "the", ratio, "Increased from", round(as.numeric(Countries_matrix[p,1]),digits=2), " to ", round(as.numeric(Countries_matrix[p,ncol(Countries_matrix)]), digits=2))
      } else {Direction[p] <- paste("In", countries[p], "the", ratio, "Decreased from", round(as.numeric(Countries_matrix[p,1]), digits=2), " to ", round(as.numeric(Countries_matrix[p,ncol(Countries_matrix)]),digits=2))}
    }
    
    #Preparation of values for the maximum and minimum analysis
    Max <- paste("A maximum of",round(as.numeric(max(Countries_matrix)), digits=2) ,
                 "was reached by", countries[which(Countries_matrix == max(Countries_matrix), arr.ind=TRUE)[1,1]], 
                 "In", Year[which(Countries_matrix == max(Countries_matrix), arr.ind=TRUE)[1,2]] )
    
    Min <- paste("A minimum of",round(as.numeric(min(Countries_matrix)), digits=2) ,
                 "was reached by", countries[which(Countries_matrix == min(Countries_matrix), arr.ind=TRUE)[1]], 
                 "In", Year[which(Countries_matrix == min(Countries_matrix), arr.ind=TRUE)[2]] )
    
    #Finalization and conclusion of the analysis with a concluding statement
    return(c(Direction, Min, Max))
  
}

# # The function allows to compare inequality trends per a given country
Countries_Comparison(countries = c("Italy", "Spain", "France"),ratio = "90/10 Ratio", average = FALSE)
Countries_Comparison(countries = c("Argentina", "Brazil", "Venezuela", "Ecuador"),ratio = "90/10 Ratio", average=TRUE)
