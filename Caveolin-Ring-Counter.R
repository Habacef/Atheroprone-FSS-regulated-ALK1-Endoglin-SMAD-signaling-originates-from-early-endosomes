# R-Script to postprocess data from ImageJ, these operations could all be done in ImageJ macros,
# however, the script was originally used to also plot the data using the libary plotly.
# For publication the data was plotted differently, still the code to calculate the number of caveolin-1 rings was done via the following R-script.

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
  files1 <- list.files(path=fileName1, pattern="", full.names=TRUE, recursive=FALSE)
  RingpCellList<-vector();
  AverageImageInt <-vector();
  CellList <-vector();
  cells=0;
  PercentageOfEngPositives<-vector();
  i=0;
  for (fileName1 in files1) {
      ResultTable <- read.table(fileName2, dec=".",fill=TRUE)
      if (nrow(ResultTable)<4) {
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
      ResultTable <- read.table(fileName2, dec=".",fill=TRUE)
      if (nrow(ResultTable)<4)  {}
      else {
          i=i+1;
          print(i)
          for (Row in 1:nrow(ResultTable)) {
              if (ResultTable[c(Row),c(7)] > (AverageImageInt[c(i)]*1.15)) {
                  posEngNum=posEngNum+1;
              }
              b = (posEngNum / CellList[c(i)]);
              b = (b / RingpCellList[c(i)])*100;
              
          }
          PercentageOfEngPositives<- c(PercentageOfEngPositives, b);
          posEngNum=0;
          b=0;
          
      }
      
  }
  p=p+1;
  lstCav[[p]] <- RingpCellList
  lstEngprozent[[p]] <-PercentageOfEngPositives;
  
#  fig1 <- fig1 %>% add_trace(
#    y = RingpCellList,
#    type = "box",
#    boxpoints = 'all',
#    pointpos = 0,
#    jitter = 5,
#   name=paste(substr(fileName1,nchar(fileName1)-24, nchar(fileName1)), " Cav-Number per cell"))
    
#  fig2 <- fig2 %>% add_trace(
#    y = PercentageOfEngPositives,
#    type = "box",
#    boxpoints = 'all',
#    pointpos = 0,
#    jitter = 5,
#    name=paste(substr(fileName1,nchar(fileName1)-24, nchar(fileName1)), " per cell - % of Eng-pos. per Cav-rings"))
}

# Write the calculated data to textfiles.
lapply(lstCav, write, "Cav-number-per-cell.txt", append=TRUE, ncolumns=1000)
lapply(lstEngprozent, write, "Eng-positives-percent-of-Cav-number-per-cell.txt", append=TRUE, ncolumns=1000)
