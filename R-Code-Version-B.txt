#data(diabetes)
###################1##############
plot(diabetes$Insulin, diabetes$Glucose, xlab = "Insulin Concentration", ylab = "Glucose",  main = "Relation between Glucose and Insulin", col = "green")


###################2##############
hist(diabetes$Glucose, xlim = c(50, 150), col = "lightblue", xlab = "Glucose Value", main = "Glucose value in blood", breaks = 20 )


###################3##############
#plot(diabetes$BloodPressure,  main = "Blood Pressure Measurements")
boxplot(diabetes$BloodPressure, col = rainbow(6), main = "Blood Pressure Measurements")


###################4##############
max_insulin <- aggregate(diabetes[,5],list(diabetes$Age), max)
max_insulin
barplot(max_insulin$Insulin, names.arg= max_insulin$Group.1, main= "maximum Insulin dose grouped per age of patient")

###################5##############
barplot(table(diabetes$Pregnancies), main = "Pregnancies table")

###################6##############
max_pie_insulin <- max(diabetes$Insulin, na.rm = TRUE)
min_pie_insulin <- min(diabetes$Insulin, na.rm = TRUE)
mean_pie_insulin <- mean(diabetes$Insulin)
median_pie_insulin <- median(diabetes$Insulin, na.rm = TRUE)
slices <- c(mean_pie_insulin, median_pie_insulin, min_pie_insulin, max_pie_insulin)
lbls <- c("mean", "median", "min", "max")
lbls <- paste(lbls,"= [")
lbls <- paste(lbls, slices)
lbls <- paste(lbls,"]",sep="")
pie(slices, labels = lbls, col= rainbow (length(lbls)), main="Pie Chart of Countries")

###################7##############
par(mfcol = c(1,2))
plot(density(diabetes$DiabetesPedigreeFunction),main ="Diabetes Pedigree Function Distribution")
plot(density(diabetes$Age),main ="Age Distribution")

###################8##############
dotchart(diabetes$DiabetesPedigreeFunction, groups=diabetes$Outcome, labels = diabetes$Outcome, cex =0.7, main = "Diabetes Pedigree Function per Diabetes outcome", xlab= "Diabetes Pedigree Function per Diabetes outcome")

#dotchart(diabetes$DiabetesPedigreeFunction,diabetes$Outcome,cex = 0.7, main="Diabetes Pedigree Function per Diabetes outcome",xlab="Diabetes Pedigree Function per Diabetes outcome")

###################9 part 1##############

#plot(diabetes[-c(1,4,6,9)], col = chart_col)
chart_col2 <- rainbow(2)
pairs(diabetes[-c(1,4,6,9)], main = "pairwise relationships between 5 attributes", pch = 21)

###################9 part 2##############
chart_col <- rainbow(2)
pairs(diabetes[-c(1,4,6,9)], main = "pairwise relationships with outcome", pch = 21, col = chart_col)
par(xpd = TRUE)
legend(0.2, 0.2, horiz = TRUE, as.vector(unique(diabetes$Outcome)), fill = chart_col, bty = "n")

###################10##############
par(mfcol = c(3,3))
#--------p1-----------
plot(diabetes$Insulin, diabetes$Glucose, xlab = "Insulin Concentration", ylab = "Glucose",  main = "Relation between Glucose and Insulin", col = "green")
#--------p2-----------
hist(diabetes$Glucose, xlim = c(50, 150), col = "lightblue", xlab = "Glucose Value", main = "Glucose value in blood", breaks = 20 )
#--------p3-----------
boxplot(diabetes$BloodPressure, col = rainbow(6), main = "Blood Pressure Measurements")
#--------p4-----------
max_insulin <- aggregate(diabetes[,5],list(diabetes$Age), max)
max_insulin
barplot(max_insulin$Insulin, names.arg= max_insulin$Group.1, main= "maximum Insulin dose grouped per age of patient")
#--------p5-----------
barplot(table(diabetes$Pregnancies), main = "Pregnancies table")
#--------p6-----------
max_pie_insulin <- max(diabetes$Insulin, na.rm = TRUE)
min_pie_insulin <- min(diabetes$Insulin, na.rm = TRUE)
mean_pie_insulin <- mean(diabetes$Insulin)
median_pie_insulin <- median(diabetes$Insulin, na.rm = TRUE)
slices <- c(mean_pie_insulin, median_pie_insulin, min_pie_insulin, max_pie_insulin)
lbls <- c("mean", "median", "min", "max")
lbls <- paste(lbls,"= [")
lbls <- paste(lbls, slices)
lbls <- paste(lbls,"]",sep="")
pie(slices, labels = lbls, col= rainbow (length(lbls)), main="Pie Chart of Countries")
#--------p7-----------
plot(density(diabetes$DiabetesPedigreeFunction),main ="Diabetes Pedigree Function Distribution")
plot(density(diabetes$Age),main ="Age Distribution")
#--------p8-----------
dotchart(diabetes$DiabetesPedigreeFunction, groups=diabetes$Outcome, labels = diabetes$Outcome, cex =0.7, main = "Diabetes Pedigree Function per Diabetes outcome", xlab= "Diabetes Pedigree Function per Diabetes outcome")

