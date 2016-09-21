rm(list=ls())
setwd("/Users/six/Dropbox/Università/PhD/Papers/2014/URSW 3/Test/")

### Functions ###

# It calculates the SUS of a specific user
calculateSUS = function (list) {
	sum = 0
	for (i in 1:10) {
		value = list[[i]]
		if (i %% 2 == 0) {
			sum = sum + 5 - value
		} else {
			sum = sum + value - 1
		}
	}
	return(sum * 2.5)
}

# It calculates the SUS of each user (one per row)
calculateUsersSUS = function (list) {
	result = data.frame()
	len = length(list[,1])
	for (i in 1:len) {
		result[paste("User",i,sep=""),1] = calculateSUS(list[i,])
	}
	
	names(result) = "SUS values"
	return(result)
}

# It calculates the learnability of a specific USER
calculateLearnability = function (list) {
	return((10 - list[[4]] - list[[10]]) * 12.5)
}

# It calculates the learnability dimension from SUS value of each user
calculateUsersLearnability = function (list) {
	result = data.frame()
	len = length(list[,1])
	for (i in 1:len) {
		result[paste("User",i,sep=""),1] = calculateLearnability(list[i,])
	}
	
	names(result) = "Learnability"
	return(result)
}

# It calculates the usability of a specific USER
calculateUsability = function (list) {
	userSUS = calculateSUS(list) / 2.5
	userLearn = calculateLearnability(list) / 12.5
	return((userSUS - userLearn) * 3.125)
}

# It calculates the usability dimension from SUS value of each user
calculateUsersUsability = function (list) {
	result = data.frame()
	len = length(list[,1])
	for (i in 1:len) {
		result[paste("User",i,sep=""),1] = calculateUsability(list[i,])
	}
	
	names(result) = "Usability"
	return(result)
}

# It calculates the basic statistics of SUS
summarise = function (data,text) {
	return(cat(
		"\n## ", text, ": basic statistics ##\n",
		"\tn. of " , text, " values: ", length(data[,1]), "\n",
		"\t", text, " mean: ", sapply(data,mean), "\n",
		"\tmedian: ", sapply(data,median), "\n",
		"\tmax: ", sapply(data,max), "\n",
		"\tmin: ", sapply(data,min), "\n",
		"\ts. deviation: ", sapply(data,sd), "\n",
		"\tvariance: ", sapply(data,var), "\n",
		"---------------------------------------\n",
		sep = ""
	));
}

# It calculates the ANOVA of the stack
anova = function (inputstack,text) {
	
	cat("\n\n# Homogeneity of variance for", text,"\n")
	# Check if the variance is homogeneous
	print(bartlett.test(values ~ ind, data=inputstack))

	cat("# ANOVA calculation of", text,"\n")
	m = aov(values ~ ind, data=inputstack)
	print(summary(m))

	cat("\n# Tukey of", text,"\n")
	print(TukeyHSD(m))
}

# It calculates the percentage values in a 1 to 5 range of values
percentage15 = function (dframe, text) {
	cat("\n\n## Percentage of", text)
	for (i in 1:length(dframe)) {
		cat("\n\n# Column",i,"percentage values")
		current_length = length(dframe[,i])
		for (j in 1:5) {
			count = length(which(dframe[,i] == j))
			cat("\nValue",j,"has percentage:",as.integer(round(count/current_length,2)*100),"% -",count,"out of",current_length)
		}
	}
	cat("\n\n")
}

### Operations ###

# Carica i dati dei questionari SUS dai CSV e vincola le variabili a R
sus = read.csv("sus.csv")
attach(sus)

# SUS
print("SUS")
usersSUS = calculateUsersSUS(sus)
usersLearn = calculateUsersLearnability(sus)
usersUsability = calculateUsersUsability(sus)
summarise(usersSUS, "SUS")
summarise(usersUsability, "Usability")
summarise(usersLearn, "Learnability")

background = read.csv("prequestions.csv")
attach(background)

# Sub-values from questionaries
experience = as.data.frame(t(t(sapply(as.data.frame(t(background[,1:7])),sum))))

print("Pearson with full SUS")
print(cor.test(experience$V1,usersSUS$"SUS values"))

print("Pearson with Learnability")
print(cor.test(experience$V1,usersLearn$"Learnability"))

print("Pearson with Usability")
print(cor.test(experience$V1,usersUsability$"Usability"))

# Plot the diagrams of the aforementioned comparisons
par(mfrow=c(2,3))
plot(experience$V1,usersSUS$"SUS values",xlab="Experience score",ylab="SUS score",pch=16,col="blue",cex=2)
abline(lm(usersSUS$"SUS values"~experience$V1), col="red", cex=5, lty=2)

plot(experience$V1,usersLearn$"Learnability",xlab="Experience score",ylab="Learnability score",pch=16,col="blue",cex=2)
abline(lm(usersLearn$"Learnability"~experience$V1), col="red", cex=5, lty=2)

plot(experience$V1,usersUsability$"Usability",xlab="Experience score",ylab="Usability score",pch=16,col="blue",cex=2)
abline(lm(usersUsability$"Usability"~experience$V1), col="red", cex=5, lty=2)
