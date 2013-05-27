# A little script to show some of the useful arguments for read.table()
setwd('/Users/ian/BEACON_COURSE_2011/BEACON R tutorial/')
avidaData <- read.table('LZ_avida_average.dat')
str(avidaData)
summary(avidaData)

updates <- avidaData$V1
fitness <- avidaData$V4
abundance <- avidaData$V9

par(mfrow=c(2,1))
plot(fitness~updates)
plot(abundance~updates)


# However we probably we want to include more meta-data with this. For instance column names and types.
# Also for very large datasets, it is extremely useful to specify the data type of each column as well as the maximum number of rows for the file.
# This will really speed getting the data into R.

avidaData_better <- read.table('LZ_avida_average.dat',
                                header = F, # Is there a header (column names)
                                na.strings = "NA", 
                                sep = "", # field seperator
                                skip = 19, # skip the first 19 lines
                                nrows= 1020,  # Max number of rows.
                                col.names=c("update","merit","gestationT","fitness","RepRate", "size","CopiedSize", 
                                  "ExecutedSize","abundance","PropBirth","PropBreed","GenotypeDepth", "generation","neutral", 
                                  "lineage","TrueRepRate"),# names for each column
                                colClasses=c(rep("numeric",16))
                                 )
                                 

# To access some of pre-written functions                                 
source("/Users/ian/BEACON_COURSE_2011/BEACON R tutorial/useful_R_function_ID_Jan20_2011.R")

ls()
