##### Keshava R code for 
## Predicting Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car

library(MASS)
library(car)

CarPrice <- read.csv("CarPrice_Assignment.csv")

#### check for duplicate CAR ID
sum(duplicated(CarPrice$car_ID))

#Examine the structure of the dataset
str(CarPrice)

library(tidyr)
CarPrice <- separate(CarPrice, CarName, into=c("Company","ModelName"),sep = " ",extra = "merge", fill = "right")
# Remove Model Name and retain only comany name.
CarPrice = CarPrice[-4]


##  Step-1   #########################
####################################################
# Variable formatting #
# Check the types of various variables
# Check for NA values
# Changing required number  to numeric
########################################################

#DUMMY VARIABLE CREATION.

###structure of variable "fueltype".
str(CarPrice$fueltype)
summary(factor(CarPrice$fueltype))
levels(CarPrice$fueltype) <- c(0,1)
# Store the numeric values in the same variable
CarPrice$fueltype<- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

###structure of variable "aspiration".
str(CarPrice$aspiration)
summary(factor(CarPrice$aspiration))
levels(CarPrice$aspiration) <- c(1,0)
# Store the numeric values in the same variable
CarPrice$aspiration<- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

###structure of variable "doornumber".
str(CarPrice$doornumber)
summary(factor(CarPrice$doornumber))
levels(CarPrice$doornumber) <- c(0,1)
# Store the numeric values in the same variable
CarPrice$doornumber<- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]



###structure of variable "Company".
summary(factor(CarPrice$Company))
#Converting "Company" into dummies . 
dummy_1 <- data.frame(model.matrix(~Company, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "Company" column
CarPrice <- cbind(CarPrice[,-3], dummy_1)
#View(CarPrice)



###structure of variable "carbody".
summary(factor(CarPrice$carbody))
#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix(~carbody, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
CarPrice <- cbind(CarPrice[,-6], dummy_1)
#View(CarPrice)

###structure of variable "drivewheel".
summary(factor(CarPrice$drivewheel))
#Converting "drivewheel" into dummies . 
dummy_1 <- data.frame(model.matrix(~drivewheel, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "drivewheel". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
CarPrice <- cbind(CarPrice[,-6], dummy_1)
#View(CarPrice)


###structure of variable "enginelocation".
str(CarPrice$enginelocation)
summary(factor(CarPrice$enginelocation))
levels(CarPrice$enginelocation) <- c(0,1)
# Store the numeric values in the same variable
CarPrice$enginelocation<- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]


###structure of variable "enginetype".
summary(factor(CarPrice$enginetype))
#Converting "enginetype" into dummies . 
dummy_1 <- data.frame(model.matrix(~enginetype, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "enginetype". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
CarPrice <- cbind(CarPrice[,-12], dummy_1)
#View(CarPrice)


###structure of variable "cylindernumber".
summary(factor(CarPrice$cylindernumber))
#Converting "cylindernumber" into dummies . 
dummy_1 <- data.frame(model.matrix(~cylindernumber, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
CarPrice <- cbind(CarPrice[,-12], dummy_1)
#View(CarPrice)





###structure of variable "fuelsystem".
summary(factor(CarPrice$fuelsystem))
#Converting "fuelsystem" into dummies . 
dummy_1 <- data.frame(model.matrix(~fuelsystem, data = CarPrice))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
CarPrice <- cbind(CarPrice[,-13], dummy_1)
#View(CarPrice)




##  Step-2   # Data cleaning # #########################
####################################################
# # Data cleaning #
# # Outlier verification
# # Outlier treatment of Acceleration variable:

########################################################

boxplot.stats(CarPrice$fueltype) # No Outliner 

boxplot.stats(CarPrice$wheelbase) # Out Liner

# Outlier treatment of wheelbase variable:
# Higher values to be capped at Upper hinge + 1.5X IQR
# Lower values to be capped at Lower hinge - 1.5X IQR


LHinge<-quantile(CarPrice$wheelbase,prob=0.25)
UHinge<-quantile(CarPrice$wheelbase,prob=0.75)

IQR<-UHinge-LHinge

CarPrice$wheelbase<-ifelse(CarPrice$wheelbase<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(CarPrice$wheelbase>UHinge+1.5*IQR,UHinge+1.5*IQR,CarPrice$wheelbase))


boxplot.stats(CarPrice$carlength) # No Outliner
boxplot.stats(CarPrice$carwidth)
boxplot.stats(CarPrice$curbweight) # No Outliner

boxplot.stats(CarPrice$carheight)# No Outliner

boxplot.stats(CarPrice$enginesize)# Out Liner
################## Outlier treatment of enginesize variable #################
# Higher values to be capped at Upper hinge + 1.5X IQR
# Lower values to be capped at Lower hinge - 1.5X IQR

LHinge<-quantile(CarPrice$enginesize,prob=0.25)
UHinge<-quantile(CarPrice$enginesize,prob=0.75)
IQR<-UHinge-LHinge
CarPrice$enginesize<-ifelse(CarPrice$enginesize<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(CarPrice$enginesize>UHinge+1.5*IQR,UHinge+1.5*IQR,CarPrice$enginesize))

boxplot.stats(CarPrice$horsepower)# Out Liner
LHinge<-quantile(CarPrice$horsepower,prob=0.25)
UHinge<-quantile(CarPrice$horsepower,prob=0.75)
IQR<-UHinge-LHinge
CarPrice$horsepower<-ifelse(CarPrice$horsepower<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(CarPrice$horsepower>UHinge+1.5*IQR,UHinge+1.5*IQR,CarPrice$horsepower))

boxplot.stats(CarPrice$peakrpm)# No Out Liner

boxplot.stats(CarPrice$citympg) #  Out Liner
LHinge<-quantile(CarPrice$citympg,prob=0.25)
UHinge<-quantile(CarPrice$citympg,prob=0.75)
IQR<-UHinge-LHinge
CarPrice$citympg<-ifelse(CarPrice$citympg<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(CarPrice$citympg>UHinge+1.5*IQR,UHinge+1.5*IQR,CarPrice$citympg))


boxplot.stats(CarPrice$highwaympg)#  Out Liner
LHinge<-quantile(CarPrice$highwaympg,prob=0.25)
UHinge<-quantile(CarPrice$highwaympg,prob=0.75)
IQR<-UHinge-LHinge
CarPrice$highwaympg<-ifelse(CarPrice$highwaympg<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(CarPrice$highwaympg>UHinge+1.5*IQR,UHinge+1.5*IQR,CarPrice$highwaympg))

#Remove Card-id

CarPrice = CarPrice[-1]

##  Step-3   # # Creating training and testing data sets ##########################
####################################################
# Divide data in 70:30 ratio

# Modelling process
#--------------------
    # GOAL :::: ???  Constraints:
    # The model should not contain more than 5 variables.
    # According to the business needs, set the VIF to 2. 
    # The model should be highly predictive in nature i.e it should show 80% (R squared) of accuracy.

########################################################

set.seed(101)
indices= sample(1:nrow(CarPrice), 0.7*nrow(CarPrice))

carp.train = CarPrice[indices,]
carp.test  = CarPrice[-indices,]

# Modelling process
#--------------------

carp.model1<-lm(price ~ .,data=carp.train)
summary(carp.model1)

step <- stepAIC(carp.model1, direction="both")
step

# Resulting formula
carp.model2 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + highwaympg + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth + Companyporcshce + 
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + Companyvw + carbodyhatchback + 
    carbodysedan + carbodywagon + enginetypedohcv + enginetypeohcv + 
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model2) #R-squared :0.9656
vif(carp.model2)


# Remove highwaympg (p-value  = 0.118559 + VIF = 9.874055). 

carp.model3 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth + Companyporcshce + 
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + Companyvw + carbodyhatchback + 
    carbodysedan + carbodywagon + enginetypedohcv + enginetypeohcv + 
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model3) #R-squared :0.9647
vif(carp.model3)

# Remove Companyporcshce (p-value  = 0.107809 ).

carp.model4 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + Companyvw + carbodyhatchback + 
    carbodysedan + carbodywagon + enginetypedohcv + enginetypeohcv + 
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model4) #R-squared :0.9637
vif(carp.model4)

# Remove enginetypeohcv (p-value  = 0.097308 + VIF = 4.608238).
carp.model5 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + Companyvw + carbodyhatchback + 
    carbodysedan + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model5) #R-squared :0.9627
vif(carp.model5)

# Remove Companyvw (p-value  = 0.121669 )
carp.model6 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    carbodysedan + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model6) #R-squared :0.9618
vif(carp.model6)

# Remove carbodysedan (p-value  = 0.28293 + VIF = 10.097377)

carp.model7 = lm(formula = price ~ fueltype + enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model7) #R-squared :0.961
vif(carp.model7)

# Remove fueltype (p-value  = 0.213929 + VIF = 157.462272)

carp.model8 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + compressionratio + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model8) #R-squared :0.9604
vif(carp.model8)

# Remove compressionratio (p-value  = 0.837888 + VIF = 2.911835)

carp.model9 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi, 
    data = carp.train)

summary(carp.model9) #R-squared :0.9604
vif(carp.model9)

# Remove fuelsystemspdi (p-value  = 0.186415 + VIF = 2.065047)

carp.model10 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix + fuelsystemmfi + fuelsystemmpfi , 
    data = carp.train)

summary(carp.model10) #R-squared :0.9597
vif(carp.model10)

# Remove fuelsystemmfi (p-value  = 0.119223 )

carp.model11 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + carbodywagon + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model11) #R-squared :0.9588
vif(carp.model11)

# Remove carbodywagon (p-value  = 0.534002 )

carp.model12 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + 
    horsepower + peakrpm + Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model12) #R-squared : 0.9586
vif(carp.model12)

# Remove peakrpm (p-value  = 0.054453 )

carp.model13 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength + carwidth + carheight + curbweight + 
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model13) #R-squared : 0.9586
vif(carp.model13)

# Remove carwidth (p-value  = 0.023632 ,V-Value = 11.554771 )

carp.model14 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength +  carheight + curbweight + 
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta + Companyvokswagen + 
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model14) #R-squared : 0.9551
vif(carp.model14)


# Remove Companyvokswagen (p-value  = 0.061905 )

carp.model15 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength +  carheight + curbweight + 
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model15) #R-squared : 0.9537
vif(carp.model15)

# Remove curbweight (VIF  = 12.123307 )

carp.model16 = lm(formula = price ~ enginelocation + wheelbase + 
    carlength +  carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model16) #R-squared : 0.9351
vif(carp.model16)


# Remove carlength (p-value   = 0.272830 )

carp.model17 = lm(formula = price ~ enginelocation + wheelbase + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model17) #R-squared : 0.9351
vif(carp.model17)

# Remove wheelbase (VIF   =  5.424661 )

carp.model18 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen + Companyvolvo + carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model18) #R-squared : 0.9344
vif(carp.model18)

### Now We can start removing the CarNames also (Nissa,Tayota like that )

# Remove Companyvolvo (p-value = 0.926319  )

carp.model19 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan + Companypeugeot + Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model19) #R-squared : 0.9147
vif(carp.model19)

# Remove Companypeugeot (p-value = 0.352896  )

carp.model20 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    cylindernumbersix +  fuelsystemmpfi , 
    data = carp.train)

summary(carp.model20) #R-squared : 0.914
vif(carp.model20)


# Remove cylindernumbersix (VIF = 7.252423  )

carp.model21 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfive + cylindernumberfour + 
    fuelsystemmpfi , 
    data = carp.train)

summary(carp.model21) #R-squared : 0.8741 
# Note ********** 
# Removing the cylindernumbersix drastically reduces the Rvalue from 0.914 to 0.8741 ??? 
# 
 
vif(carp.model21)

# Remove cylindernumberfive (VIF = 7.252423  )

carp.model22 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetypedohcv +  
    enginetyperotor + cylindernumberfour + 
    fuelsystemmpfi , 
    data = carp.train)

summary(carp.model22) #R-squared : 0.8731
vif(carp.model22)



# Remove enginetypedohcv (p-value  = 0.620059  )

carp.model23 = lm(formula = price ~ enginelocation + 
    carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour + 
    fuelsystemmpfi , 
    data = carp.train)

summary(carp.model23) #R-squared : 0.8728
vif(carp.model23)

# Remove enginelocation (p-value  = 0.200676  )

carp.model24 = lm(formula = price ~ carheight +  
    horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour + 
    fuelsystemmpfi , 
    data = carp.train)

summary(carp.model24) #R-squared : 0.8711
vif(carp.model24)

# Remove carheight (p-value  = 0.084322  )

carp.model25 = lm(formula = price ~ horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour + 
    fuelsystemmpfi , 
    data = carp.train)

summary(carp.model25) #R-squared : 0.8678
vif(carp.model25)


# Remove fuelsystemmpfi (VIF   = 3.244873  )

carp.model26 = lm(formula = price ~ horsepower +  Companybmw + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model26) #R-squared : 0.8496
vif(carp.model26)

# Remove Companybmw (p-value    = 0.159602  )

carp.model27 = lm(formula = price ~ horsepower + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymazda + Companymercury + Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model27) #R-squared : 0.8444
vif(carp.model27)


# Remove Companymazda (p-value    = 0.126344  )

carp.model28 = lm(formula = price ~ horsepower + Companychevrolet + 
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model28) #R-squared : 0.8414
vif(carp.model28)


# Remove Companychevrolet (p-value    = 0.107117  )

carp.model29 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda + Companyisuzu + Companymaxda + 
    Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model29) #R-squared : 0.8381
vif(carp.model29)

# Remove Companymaxda (p-value    = 0.120735  )

carp.model30 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda + Companyisuzu +  
    Companymitsubishi + Companynissan + 
    CompanyNissan +  Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model30) #R-squared : 0.8349
vif(carp.model30)

# Remove CompanyNissan (p-value    = 0.083463  )

carp.model31 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda + Companyisuzu +  
    Companymitsubishi + Companynissan + 
    Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model31) #R-squared : 0.831
vif(carp.model31)

# Remove Companyisuzu (p-value    = 0.087247  )

carp.model32 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda +  
    Companymitsubishi + Companynissan + 
    Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + enginetyperotor + cylindernumberfour , data = carp.train)

summary(carp.model32) #R-squared : 0.8271
vif(carp.model32)

# Remove enginetyperotor (p-value    = 0.026285  )

carp.model33 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda +  
    Companymitsubishi + Companynissan + 
    Companyplymouth +  
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model33) #R-squared : 0.8203
vif(carp.model33)



# Remove Companyplymouth (p-value    = 0.028891  )

carp.model34 = lm(formula = price ~ horsepower +  
    Companydodge + Companyhonda +  
    Companymitsubishi + Companynissan + 
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model34) #R-squared : 0.8135
vif(carp.model34)


# Remove Companyhonda (p-value    = 0.028891  )

carp.model35 = lm(formula = price ~ horsepower +  
    Companydodge + Companymitsubishi + Companynissan + 
    Companysubaru + Companytoyota + Companytoyouta +  
    Companyvolkswagen +  carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model35) #R-squared : 0.8087
vif(carp.model35)

# Remove Companyvolkswagen (p-value    = 0.057672  )

carp.model36 = lm(formula = price ~ horsepower +  
    Companydodge + Companymitsubishi + Companynissan + 
    Companysubaru + Companytoyota + Companytoyouta +  
    carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model36) #R-squared : 0.8034
vif(carp.model36)

# Remove Companytoyota (p-value    = 0.051648  )

carp.model37 = lm(formula = price ~ horsepower +  
    Companydodge + Companymitsubishi + Companynissan + 
    Companysubaru + Companytoyouta +  
    carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model37) #R-squared : 0.7977
vif(carp.model37)

# Remove Companydodge (p-value    = 0.080237  )

carp.model38 = lm(formula = price ~ horsepower +  
    Companymitsubishi + Companynissan + 
    Companysubaru + Companytoyouta +  
    carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model38) #R-squared : 0.793
vif(carp.model38)

# Remove Companysubaru (p-value    = 0.068436  )

carp.model39 = lm(formula = price ~ horsepower +  
    Companymitsubishi + Companynissan + 
    Companytoyouta +  
    carbodyhatchback + 
    + cylindernumberfour , data = carp.train)

summary(carp.model39) #R-squared : 0.7879
vif(carp.model39)


###### predicting the results in test dataset

Predict_1 <- predict(carp.model39,carp.test)
carp.test$test_price <- Predict_1

##### Now, we need to test the r-square between actual and predicted Prise 
r <- cor(carp.test$price,carp.test$test_price)
rsquared <- cor(carp.test$price,carp.test$test_price)^2
rsquared



### Finnay  Priting summaryy ####

summary(carp.model39) 
vif(carp.model39)


## **********Below variable are important Variable for  *********** 

# Item-1 
## ****** Below variable are imporatnt driving factor of the car parameter *****

#1. horsepower
#2. carbodyhatchback
#3. cylindernumberfour

# Item-2
##***** Infact Geely Auto can go to market with Branded name below Branded Cars ***

#1. Companymitsubishi
#2. Companynissan
#3. Companytoyouta

####

# r-square between actual and predicted sales.
rsquared 

# Adjusted R value = 0.7785 (taken from summary(carp.model39) )

