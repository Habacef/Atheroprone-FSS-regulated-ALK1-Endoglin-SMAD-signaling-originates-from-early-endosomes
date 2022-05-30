# R-Script to postprocess data from ImageJ, these operations could all be done in ImageJ macros. 
# For publication the data was plotted differently, still the code to calculate the number of caveolin-1 rings, its Endoglin positivity and the SMAD inside was done via the following R-script.

# Adjust the following Path to your Data
path="PATH-TO-YOUR-DATA";
library(MASS)
posEngNum=0;
p=0;
lstCav <- list()
lstEngprozent <- list()
files <- list.files(path=path, pattern="", full.names=TRUE, recursive=FALSE)

# Import the data from the ImageJ output and calculate the number and percentage of endoglin positive Caveolin-1 structures.
for (fileName in files)  {
  files1 <- list.files(path=fileName, pattern=".txt", full.names=TRUE, recursive=FALSE)
  RingpCellList<-vector();
  AverageImageInt <-vector();
  CellList <-vector();
  cells=0;
  PercentageOfEngPositives<-vector();
  i=0;
  for (fileName1 in files1) {
    
    ResultTable <- read.table(fileName1, dec=".",fill=TRUE)
    if (nrow(ResultTable)<4 && nrow(ResultTable)>2) {
      NumberOfCells<-ResultTable[c(1),c(1)]
      NumberOfCavs<-ResultTable[c(2),c(1)]
      RingpCell=NumberOfCavs/NumberOfCells;
      CellList <-c(CellList, NumberOfCells);
      cells=cells+NumberOfCells;
      RingpCellList <- c(RingpCellList, RingpCell);
      AverageImageInt <- c(AverageImageInt, ResultTable[c(3),c(1)])
    }
  }
  for (fileName1 in files1) {
    print(fileName1)
    print(paste(fileName, "/ENG.txt", sep = ""))
    if (fileName1==paste(fileName,"/ENG.txt", sep = "")) {
      print("IT IS");
      ResultTable <- read.table(fileName1, dec=".",fill=TRUE)
      i=i+1;
      # get all regions of interest that are Endoglin positive.
      for (Row in 1:nrow(ResultTable)) {
        if (ResultTable[c(Row),c(7)] > (AverageImageInt[c(i)]*1.15)) {
          
       #   posEngNum=posEngNum+1;
       #   for analyzing the number of Endoglin-positive vesicles, delete the following for loop and uncomment the previous line.
       #   The following calculates if the Endoglin-pos vesicles are also positive for SMAD1   
         for (fileName2 in files1) {
            ResultTable2 <- read.table(fileName2, dec=".",fill=TRUE)
            if (fileName2 != paste(fileName,"/ENG.txt", sep = "") && (nrow(ResultTable2)>4)) {
                if (ResultTable2[c(Row),c(8)] > 0) {
                  posEngNum=posEngNum+1;

                }
              
            
        } 
          
        
          }}   
          b = (posEngNum / CellList[c(i)]);+
          # comment out the following line to get the number and not the percentage
          b = (b / RingpCellList[c(i)])*100;
       
      }    
     
      } }
      PercentageOfEngPositives<- c(PercentageOfEngPositives, b);
      posEngNum=0;
      b=0;
  
  p=p+1;
  lstCav[[p]] <- RingpCellList
  lstEngprozent[[p]] <-PercentageOfEngPositives;
 }

# Write the calculated data to textfiles.
capture.output(unlist(lstCav), file = "Cav-number-per-cell.txt")
capture.output(unlist(lstEngprozent), file = "Eng-and-SMAD-positives-per-cell.txt")
