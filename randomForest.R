***Using randomForest Package***
Sep 2, 2009
Description: To execute randomForest() to build prediction model, and using k-fold cross-validation to validated.
Functions: runRF(), doCV()

# Load randomForest package.
library(randomForest)

# Load *.csv data.
fin <- read.csv("~/Desktop/Demo_XD.csv", header = TRUE, sep = ",", dec =".")

# runRF(): Usage runRF() - To do training & testing, output classification result, type I & type II error, prediction rate.
runRF = function()
{
	ind <- sample(2, nrow(fin), replace = TRUE, prob = c(0.8, 0.2))
	#ind <- sample(c(1:2472), replace =FALSE )

	fin.rf <- randomForest(type ~ ., data = fin[ind==1,], ntree=1000,mtry=7,importance = TRUE, proximity = TRUE)
	fin.pred <- predict(fin.rf, fin[ind==2, ])
	classError <- table(observed = fin[ind==2, "type"], predicted = fin.pred)

	#class.rate <- (classError[1, 1] + classError[2, 2]+classError[3,3]+classError[4,4]))/length(fin.pred)

	# Sum of "A" samples
	fSamples = sum(classError[1, ])

	# Sum of "B" samples
	mSamples = sum(classError[2, ])

  # Sum of "C" samples
  nsamples=sum(classError[3,])

   #Sum of "D" samples
   xsamples=sum(classError[4,])

  # Classify "A" to "B" error samples
 	errorF = classError[1, 2]

                #Classify "A" to "C" error samples
                errorF2=classError[1,3]

                #Classify "A to "D" error samples
                errorF3=classError[1,4]

                # Classify "B" to "A" error samples
                errorM= classError[2, 1]

               # Classify "B" to "C" error samples
               errorM2=classError[2,3]

               #Classify"C"to"D"error samples
               errorM3=classError[2,4]

              # Classify "C" to "A" error samples
               errorN=classError[3,1]

             # Classify "C" to "B" error samples
             errorN2=classError[3,2]

             #Classify"C"to"D" error samples
             errorN3=classError[3,4]

             #Classify"D"to"A" error samples
             errorX=classError[4,1]

             #Classify"D"to"B" error samples
             errorX2=classError[4,2]

             #Classify"D"to"C" error samples
             errorX3=classError[4,3]



	# Accuracy
	accuracy <- (classError[1, 1] + classError[2, 2]+classError[3,3]+classError[4,4])/length(fin.pred)

	# Output results
	list(F = fSamples, M = mSamples,N=nsamples,X=xsamples, eF1 = errorF,eF2=errorF2,eF3=errorF3, eM1= errorM, eM2=errorM2, eM3=errorM3, eN1=errorN, eN2=errorN2, eN3=errorN3, eX1=errorX, eX2=errorX2, eX3=errorX3, Accuracy = accuracy)
}

# doCV(): Usage doCV(x) - The x express the implementation of times
doCV = function(x)
{
	x = x
	cvResult = data.frame(matrix(0, x, 17))
	for(i in 1:x)
	{
		# print(runRF())
		RF = runRF()
		cvResult[i,] = c(RF$F, RF$M,RF$N,RF$X, RF$eF1,RF$eF2,RF$eF3, RF$eM1,RF$eM2,RF$eM3, RF$eN1,RF$eN2,RF$eN3,RF$eX1,RF$eX2,RF$eX3,RF$Accuracy)
		rownames(cvResult[i, ]) = c(i)
	}
	colnames(cvResult) = c("A", "B", "C","D","A¡÷B", "A¡÷C","A¡÷D","B¡÷A","B¡÷C","B¡÷D","C¡÷A","C¡÷B","C¡÷D","D¡÷A","D¡÷B","D¡÷C" ,"Accuracy")

	# Save the result to *.csv.
	write.csv(cvResult, file = "result.csv")
	# Print to screen.
	cvResult
}

==============================================================================================================================
#ÅÜ¼Æ¿z¿ï ¥Hbootstrap(bagging)¬°°òÂ¦
library(varSelRF)#¥ý¥hRªº©xºô§ì«Ê¥]

fin<- read.csv("sample5-2.csv", header = TRUE, sep = ",", dec =".")
x<-fin[,1:22]#1¨ìN­ÓÄæ¦ì¿é¤J
y<-fin[,23]  #¥Ø¼ÐÅÜ¼Æ¦b²ÄX­ÓÄæ¦ì
cl<-factor(y)
fin.vs1 <- varSelRF(x, cl, ntree =200, ntreeIterat = 100,vars.drop.frac = 0.2,c.sd=1)
fin1.vs1#©I¥s¤W­±ªºµ²ªG¡A±o¥XÅãµÛÅÜ¼Æ
fin.vsb <- varSelRFBoot(x, cl,bootnumber = 10,usingCluster = FALSE,srf = fin.vs1)#±o¥X¬D¿ï«á¤ÀÃþ·Ç½T²v


# ÅÜ¼Æ­«­n¹Ï 2¡A¥HRF¬°°òÂ¦
fin <- read.csv("sample5-2.csv", header = TRUE, sep = ",", dec =".")
x<-fin[,1:22]
y<-fin[,23]
cl<-factor(y)
fin.rf <- randomForest(type ~ ., data=fin, ntree=3000, keep.forest=FALSE,importance=TRUE)
fin.rvi <- randomVarImpsRF(x, cl, fin.rf,numrandom = 22, usingCluster = FALSE)
varSelImpSpecRF(fin.rf, randomImps =fin.rvi)
randomVarImpsRFplot(fin.rvi, fin.rf)# show ¹Ï


#°²¦p¡AÁÙ¦³°ÝÃD ½Ð°Ýhelp(varSelRF)