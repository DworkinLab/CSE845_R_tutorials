# A little script to show some of the useful arguments for read.table()

# I will explain this more later, but this will read in a dataset (from the course website in this case)
setwd('/Users/ian/BEACON_COURSE_2011/BEACON R tutorial/')


# Don't worry about the details for the moment
avida_data <- read.table('LZ_avida_average.dat',
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

# What is the class of the new object?

# How many rows? columns?

# what are the data types within each column?

# Let's say we are interested in only looking at subsets of the data for generations above 50, 
# how would you make a new data set to do so?
                                
                                
# How about if we only wanted the 1st, 3rd and 5th column of the data?                                