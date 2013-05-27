parasite.data <- read.csv('/Users/ian/BEACON_COURSE_2011/March17_2011/sexAsex.csv', h=T)

parasite.data$Sex <- factor(parasite.data$Sex)
parasite.data$Parasites <- factor(parasite.data$Parasites)

#copy & paste helper line
parasiteSubsetNoSex <- subset(parasite.data, Sex==0 & Parasites==1)
parasiteSubsetSex <- subset(parasite.data, Sex==1 & Parasites==1)
noParasites <- subset(parasite.data, Parasites==0)
noParasitesSex <- subset(parasite.data, Parasites==0 & Sex==1)
noParasitesNoSex <- subset(parasite.data, Parasites==0 & Sex==0)
parasiteSubset <- subset(parasite.data, Parasites==1 & ParasiteLost != "True")
sexSubset <- subset(parasite.data, Sex == 1)
noSexSubset <- subset(parasite.data, Sex == 0)


plot(FinalGenerations ~ Sex + Parasites, data= parasite.data)
plot(FinalFitness ~ Sex + Parasites, data= parasite.data)

plot(ParasiteLost ~ HostMutationRate, data=parasite.data)
plot(ParasiteLost ~ Sex, data=parasite.data)



#Interesting
plot(FinalDiversityIndex ~ factor(HostMutationRate), data=parasite.data, main="Sex & NoSex", notch=T)

plot(FinalDiversityIndex ~ factor(HostMutationRate), data=parasiteSubsetNoSex, main="NoSex", notch=T)

plot(FinalDiversityIndex ~ factor(HostMutationRate), data=parasiteSubsetSex, main="Sex", notch=T)

plot(FinalDiversityIndex ~ factor(HostMutationRate), data= noParasitesNoSex, main="NoSex NoParasties", notch=T)

plot(FinalDiversityIndex ~ factor(HostMutationRate), data= noParasitesSex, main="Sex NoParasties", notch=T)

#I think this is the reason why mutationrate at first glance seems to be
#homogenizing
#plot(FinalGenerations ~ factor(HostMutationRate), data=noParasitesSex)
#it is not simply that higher mutation rates + sex means higher fitness peak, and thus less
#diversity
#plot(FinalFitness ~ factor(HostMutationRate), data= noParasitesSex, main="Sex NoParasties", notch=T)
#but at least the asexual populations are robust to the effect
#plot(FinalGenerations ~ factor(HostMutationRate), data=noParasitesNoSex)


plot(FinalDiversityIndex ~ Sex, data=noParasites, main="No Parasites")
plot(FinalDiversityIndex ~ Sex, data= parasiteSubset, main="Parasites")

plot(FinalDiversityIndex ~ Parasites, data=noSexSubset, main="No Sex")
plot(FinalDiversityIndex ~ Parasites, data=sexSubset, main="Sex")


plot(AveHostNumTasks ~ Parasites, data=parasite.data)


#hm
plot(AveHostNumTasks ~ Sex, data=noParasites, main="No Parasites")
plot(AveHostNumTasks ~ Sex, data=parasiteSubset, main="Parasites")

plot(FinalFitness ~ Sex, data=parasiteSubset, main="Parasites", notch=T)
plot(FinalFitness ~ Sex, data=noParasites, main="No Parasites", notch=T)





