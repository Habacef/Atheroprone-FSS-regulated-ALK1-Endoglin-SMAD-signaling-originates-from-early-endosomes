# R-Script to postprocess data from ImageJ, these operations could all be done in ImageJ macros,
# For publication the data was plotted differently, still the code to calculate the number of caveolin-1 rings was done via the following R-script.

# Adjust the following Path to your Data
path="PATH/TO/YOUR/IMAGEJ/ANALYSIS/DATA";
library(MASS)
posEngNum=0;
NumberOfEEandCavs=0;
p=0;
lstCav <- list()
lstEEs <- list()
lstDoublePos <- list()
lstEngprozent <- list()
files <- list.files(path=path, pattern="", full.names=TRUE, recursive=FALSE)

# Import the data from the ImageJ output and calculate the number and percentage of endoglin positive Caveolin-1 structures.
for (fileName in files)  {
  files1 <- list.files(path=fileName, pattern=".txt", full.names=TRUE, recursive=FALSE)
  NumberOfEEandCavs=0;
  RingpCellList<-vector();
  CavpCell<-vector();
  CavpCellList<-vector();
  EEpCell<-vector();
  EEpCellList<-vector();
  AverageImageInt <-vector();
  CellList <-vector();
  cells=0;
  PercentageOfEngPositives<-vector();
  i=0;
  for (fileName1 in files1) {
    ResultTable <- read.table(fileName1, dec=".",fill=TRUE)
    if (nrow(ResultTable) == 5) {
      NumberOfCells<-ResultTable[c(1),c(1)]
      CellList2 <- append(CellList, NumberOfCells)
      CellList <-c(CellList, NumberOfCells);
      cells=cells+NumberOfCells;
      NumberOfCavs<-ResultTable[c(2),c(1)]
      CavpCell=NumberOfCavs/NumberOfCells;
      CavpCellList <- c(CavpCellList, CavpCell);
      NumberOfEEs<-ResultTable[c(3),c(1)]
      EEpCell=NumberOfEEs/NumberOfCells;
      EEpCellList <- c(EEpCellList, EEpCell);
      AverageImageInt <- c(AverageImageInt, ResultTable[c(5),c(1)])
    }
    if (nrow(ResultTable) < 5) {
      NumberOfCells<-ResultTable[c(1),c(1)]
      for (VesiclesCheck in files1) {
        Vesiclestable <- read.table(VesiclesCheck, dec=".",fill=TRUE)
        if (VesiclesCheck == paste(fileName,"/VESICLES.txt", sep = ""))  { 
          for (Row in 1:nrow(Vesiclestable)) {
          if (Vesiclestable[c(Row),c(7)] > 0) {
            print(Vesiclestable[c(Row),c(7)])
            NumberOfEEandCavs=NumberOfEEandCavs+1;
            RingpCell=NumberOfEEandCavs/NumberOfCells;
          }
        } 
      } }
      CellList2 <- append(CellList, NumberOfCells)
      CellList <-c(CellList, NumberOfCells);
      cells=cells+NumberOfCells;
      RingpCellList <- c(RingpCellList, RingpCell);
      NumberOfEEandCavs=0;
    }
    
  }
  
  for (fileName1 in files1) {
    ResultTable <- read.table(fileName1, dec=".",fill=TRUE)

    if (fileName1 == paste(fileName,"/ENG.txt", sep = ""))  {
      i=i+1;

      for (Row in 1:nrow(ResultTable)) {
        if (ResultTable[c(Row),c(7)] > (AverageImageInt[c(i)]*1.15)) {
          for (VesiclesCheck in files1) {
            Vesiclestable <- read.table(VesiclesCheck, dec=".",fill=TRUE)
            if (VesiclesCheck == paste(fileName,"/VESICLES.txt", sep = ""))  { 
            
                if (Vesiclestable[c(Row),c(7)] > 0) {
                  
                  posEngNum=posEngNum+1;
                }
            }}
         
        }
        
        
      }
      b = (posEngNum / NumberOfCells);
      # comment out the following line to calculate the number and not the percentages of caveolin-positive vesicles.
      b = (b / RingpCellList[c(i)])*100;
      PercentageOfEngPositives<- c(PercentageOfEngPositives, b);
      posEngNum=0;
      b=0;
      
    }
    
  }
  p=p+1;
  lstCav[[p]] <- CavpCellList
  lstEEs[[p]] <- EEpCellList
  lstDoublePos[[p]] <- RingpCellList
  lstEngprozent[[p]] <-PercentageOfEngPositives;
  
}

# Write the calculated data to textfiles.
capture.output(unlist(lstCav), file = "Cav-number-per-cell.txt")
capture.output(unlist(lstEEs), file = "EE-number-per-cell.txt")
capture.output(unlist(lstDoublePos), file = "Cav-EE-doublepositive-number-per-cell.txt")
capture.output(unlist(lstEngprozent), file = "Eng-positives-number-of-Cav-EEA-doublepositive-per-cell.txt")
