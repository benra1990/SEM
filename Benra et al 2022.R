##--------------
##
##Script name: Mismatches in the ecosystem services-wellbeing nexus in chilean Patagonia
##
##Purpose of script: To calculate structural equation models for three ecosystem services categories in two model groups representing ES supply-wellbeing connections, where model group 1 has ES supply as explanatory variable of wellbeing (income) and model group 2 considers the opposite. 
##
##Author: Felipe Benra
##
##Copyright (c) Felipe Benra, 2022
##
##--------------
##
##Notes:Input data is made available: 
##
##Scrips is provided to develop the practical process.
##
##Description of the input data required to replicate these analyses can be found at Benra et al. 2023. doi TBC
##
##


##Load up packages:

library(lavaan)
library(semPlot)
library(semTable)
library(matrixcalc)
library(corrplot)
library(openxlsx)
library(data.table)
library(bestNormalize)
library(corrplot)
library(corrr)
library(Hmisc)
library(dplyr)
library(xtable)


#Import database
db_short<-read.xlsx("H:/SIG/Procesos SIG/BD_inequity/base de datos/db_short.xlsx")# change this depending on where you want it on your computers!
str(db_short)#check structure
data.table(colnames(db_short))#get order of columns in the dataframe


#Generating subset of variables to be used and new working dataframe named "db"

ha<-db_short[c("Pers_nat","pers_jurid","P_INDIG" ,"prof_ed_univ")]##human agency
tot_sup<- db_short[c("tot_water_sup","tot_water_regulation","tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]##variables of total supply (all supply within the limits of each municipality)
income<-db_short[c("weighted_mean_income")]##mean income & gini income (data chile)
area<-db_short[c("area_promedio_predios")]##mean area  of all the properties within a municipality


#arrange db and assign names to columns
db<-data.frame(ha,income,area, tot_sup)#
data.table(colnames(db))#check order of columns
colnames(db)[c(1:6)]<-c("indiv","jurid", "indig_censoP","educa","inc", "area")
data.table(colnames(db))

#looking at NA in each column
sapply(db, function(x) sum(is.na(x)))

#Check the data and transform when needed (scale and normalize)

unlist(lapply(db,is.numeric))##tells you which columns are numeric

for (i in 1:length(db))  { ##transform all variables to numeric
  db[,i] <- as.numeric (db[,i])
}

#Histograms for all variables
#windows()
par(mfrow= c (5,7),mar=c(1,2,2,0.5))     
for (i in 1:13) {
  hist(db[,c(1:13)][,i],main=names(db [,c(1:13)])[i],xlab=names(db[,c(1:13)])[i])
}
dev.off()

#check correlations between all variables of model group 1 and 2

db_cor<-db[c("indiv","jurid","indig_censoP","educa","inc","area","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]

colnames(db_cor)<-c("individual person tenure","legal person tenure","indigenous population","university education","income","property area","water supply","water regulation","carbon sequestration","carbon storage","sediment retention","native timber","recreation")#change to shorter names for better display in correlation matrix

cor_datos<-round(cor(db[c("indiv","jurid","indig_censoP","educa","inc","area","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")], use="pairwise.complete.obs", method="spearman"), 2)

str(cor_datos)

#write.csv(cor_datos, "H:/SIG/Procesos SIG/Spatial distribution/Tables/correlaciones1.csv")##save correlation matrix


##alternative calculation of correlations with HMSIC package

cor_matrix<-rcorr(as.matrix(db_cor))#corrr package produces r & P values


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
matriz_plana<-flattenCorrMatrix(cor_matrix$r, cor_matrix$P)##visualizing matriz plana

#plot correlations matrix
corrplot(cor_matrix$r,  type="lower", method = "square", order="original", pch.cex = 0.8, pch.col="black",
         p.mat = cor_matrix$P, tl.col="black", sig.level = c(0.001, 0.01, 0.05), insig = "label_sig", diag=FALSE)#here you use above calculated r y P values



#check which normalization technique is the best for each variable with bestNorlamize package. Note here that there are some variables that look kind of normal so they would not need a transformation. But here I calculated the best theoretical normalization method for all of them to be applied case by case.

#total supply
bestNormalize(db$tot_water_sup)
bestNormalize(db$tot_water_regulation)
bestNormalize(db$tot_supl_cseq)
bestNormalize(db$tot_supl_cstor)
bestNormalize(db$tot_supl_erosion)
bestNormalize(db$tot_supl_timber)
bestNormalize(db$tot_supl_recreation)
#human agency
bestNormalize(db$indig_censoP)
bestNormalize(db$educa)
bestNormalize(db$indiv)
bestNormalize(db$jurid)
#Land area
bestNormalize(db$area)
#income
bestNormalize(db$inc)

# Transform ecosystem service variables to try to meet normality.This is done because the "Maximum Likelihood estimator" (ML) requires multivariate normality for calculations. It is the most common estimator in SEM analyses but there are others as well. Maria also agreed that a normalization is needed on this case.
db%>%dplyr::mutate(tot_water_sup=predict(bestNormalize::orderNorm(tot_water_sup)),
                   tot_water_regulation=predict(bestNormalize::orderNorm(tot_water_regulation)),
                   tot_supl_cseq=predict(bestNormalize::orderNorm(tot_supl_cseq)),           
                   tot_supl_cstor=predict(bestNormalize::orderNorm(tot_supl_cstor)),
                   tot_supl_erosion=predict(bestNormalize::boxcox(tot_supl_erosion)),
                   tot_supl_timber=predict(bestNormalize::boxcox(tot_supl_timber)),
                   tot_supl_recreation=predict(bestNormalize::boxcox(tot_supl_recreation)),
                   #human agency
                   indig_censoP=predict (bestNormalize::orderNorm(indig_censoP)),   
                   educa=predict (bestNormalize::arcsinh_x(educa)), 
                   indiv=predict(bestNormalize::orderNorm(indiv)),
                   jurid=predict(bestNormalize::yeojohnson(jurid)),
                   #Land endowment 
                   area=predict(bestNormalize::orderNorm(area)),
                   #income
                   inc=predict (bestNormalize::boxcox(inc))
                   
)->dbn#new data base with normalized variables is "dbn"

data.table(colnames(dbn))
dbn<-lapply(dbn[,c(1:13)], scales::rescale)#rescaling data 0 to 1
dbn<-as.data.frame(dbn)# transforming to dataframe again

#new histogram

par(mfrow= c (5,7),mar=c(1,2,2,0.5))     
for (i in 1:13) {
  hist(dbn[,c(1:13)][,i],main=names(dbn [,c(1:13)])[i],xlab=names(dbn [,c(1:13)])[i])
}

#to check variable order again
data.table(colnames(dbn))

####model group 1: Regression model with "income" as the outcome variable. As we're using ES categories (provisioning, regulating and cultural) we will name the models depending on the ES category. prov= provisioning services; reg=regulating services; cult:cultural services####

#Step 1: Model specification

model1_total_prov<-'#Structural model
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+indig_censoP+indiv+jurid
        
        
          #Regression
         inc~tot_prov+ha+area
         tot_prov~ha+area
         area~ha
        

         #Covariance structure(of latent variables)
#Covariance structure(of latent variables)
         tot_supl_timber~~0*tot_supl_timber#avoid variance of rurality to become negative. It is possible that the model fits the data well but that the true value of the residual variance is very close to zero. If this happens then the estimate can become negative because of sampling fluctuations. If you think that this is the case then you force lavaan to maintain non-negative variance estimates by specifying a non-linear constraint on this residual variance
'

#Step 2: Model estimation
model1_total_prov_fit<-sem(model1_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)

#Step 3: Evaluate the model
summary(model1_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_prov<-fitMeasures(model1_total_prov_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))#typical goodness-of-fit indicators
std_prov1<-standardizedSolution(model1_total_prov_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model1_total_prov_fit)#checking residuals

#Model-implied covariance matrix
fitted(model1_total_prov_fit)
parameterEstimates(model1_total_prov_fit, ci=FALSE, rsquare=TRUE, standardized=TRUE)#gives the currently used parameters of the model fit

#Step 4: Visualize the path model
semPlot::semPaths(model1_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_prov_fit, sort.=TRUE,minimum.value = 10)


##model1_total_reg##
model1_total_reg<-'#Structural model
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_reg=~tot_water_regulation+tot_supl_cseq+tot_supl_cstor+tot_supl_erosion
          ha=~+educa+indig_censoP+indiv+jurid

         #Regressions
         inc~ha+tot_reg+area
         tot_reg~ha+area
         area~ha

         #Covariance structure(of latent variables)
          tot_supl_cstor~~0*tot_supl_cstor#avoid variance of rurality to become negative.
         
  '

#Step 2: Model estimation
model1_total_reg_fit<-sem(model1_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#

#Step 3: Evaluate the model
summary(model1_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_reg<-fitMeasures(model1_total_reg_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))#typical goodness-of-fit indicators
std_reg1<-standardizedSolution(model1_total_reg_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model1_total_reg_fit)

#Model-implied covariance matrix
fitted(model1_total_reg_fit)
parameterEstimates(model1_total_reg_fit)#gives the currently used parameters of the model fit

#Step 4: Visualize the path model
semPlot::semPaths(model1_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_reg_fit, sort.=TRUE,minimum.value = 10)



##model1_total_cult##
model1_total_cult<-'structural model
        
         #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable. Only human agency(ha) is defined here as latent variable
         #!this model does not run with just one Latent Variable, which would be in this case human agency, we have only one ES, hence we cant build a secont latent variable. Instead we just use all variables as measurement variables. But maybe rachel you have a better solution? I actually havent found anything saying that it is not possible to use just one latent variable, but as the model does not run I assuming thats the case. The results are as you might see weird (fit indices are perfect)
         #ha=~+educa+indig_censoP+indiv+jurid
         

         #Regressions
         inc~educa+indig_censoP+indiv+jurid+tot_supl_recreation+area
          tot_supl_recreation~educa+indig_censoP+indiv+jurid+area
          area~educa+indig_censoP+indiv+jurid
        
'
#Step 2: Model estimation
model1_total_cult_fit<-sem(model1_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)

#Step 3: Evaluate the model
summary(model1_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_cult<-fitMeasures(model1_total_cult_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))#typical goodness-of-fit indicators
std_cult1<-standardizedSolution(model1_total_cult_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model1_total_cult_fit)

#Model-implied covariance matrix
fitted(model1_total_cult_fit)
parameterEstimates(model1_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_cult_fit, sort.=TRUE,minimum.value = 10)


####results model group 1####
####IMPORTANT: change to your respective folders for saving

#goodness-of-fit of models
model1_total<-cbind(uno_a_total_prov,uno_a_total_reg,uno_a_total_cult)
colnames(model1_total)<-c("provisioning","regulating","cultural")
model1_total<-format(round(model1_total,3),nsmall = 3)
model1_total<-as.data.frame(model1_total)
#write.xlsx(model1_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total.xlsx", row.names=TRUE, overwrite=TRUE)


#explanatory power of models
std_total1<-rbind(std_prov1,std_reg1,std_cult1)
#write.xlsx(std_total1, "H:/SIG/Procesos SIG/Spatial distribution/Tables/std_total1.xlsx", row.names=TRUE, overwrite=TRUE)



#####Model group 2: Regression model with "ES supply inequality" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural service.####

#Step 1: Model specification

model2_total_prov<-'#Structural model
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+indig_censoP+indiv+jurid

         #Regression
         tot_prov~inc+ha+area
         inc~ha+area
         area~ha

         #Covariance structure(of latent variables)
         tot_supl_timber~~0*tot_supl_timber#avoid variance of rurality to become negative. It is possible that the model fits the data well but that the true value of the residual variance is very close to zero. If this happens then the estimate can become negative because of sampling fluctuations. Ifyou think that this is the case then you force lavaan to maintain non-negative variance estimates by specifying a non-linear constraint on this residual variance
     
'

#Step 2: Model estimation
model2_total_prov_fit<-sem(model2_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#

#Step 3: Evaluate the model
summary(model2_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_prov<-fitMeasures(model2_total_prov_fit, c("cfi","chisq","rmsea","srmr", "pvalue","ifi","tli", "nfi"))
std_prov2<-standardizedSolution(model2_total_prov_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model2_total_prov_fit)

#Model-implied covariance matrix
fitted(model2_total_prov_fit)
parameterEstimates(model2_total_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2_total_prov_fit, sort.=TRUE,minimum.value = 10)


##model2_total_reg##
model2_total_reg<-'#Structural model using raw indicators - total ES supply (provisioning ES)
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_reg=~tot_water_regulation+tot_supl_cseq+tot_supl_cstor+tot_supl_erosion
          ha=~+educa+indig_censoP+indiv+jurid

         #Regressions
         tot_reg~inc+ha+area
         inc~ha+area
         area~ha

         #Covariance structure(of latent variables)
           tot_supl_cstor~~0*tot_supl_cstor#avoid variance of rurality to become negative
         
        '

#Step 2: Model estimation
model2_total_reg_fit<-sem(model2_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)

#Step 3: Evaluate the model
summary(model2_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_reg<-fitMeasures(model2_total_reg_fit, c("cfi","chisq","rmsea","srmr", "pvalue","ifi","tli", "nfi"))
std_reg2<-standardizedSolution(model2_total_reg_fit)#standardized values for latent variables, regressions and covariances
inspect(model2_total_prov_fit,what="std")$lambda#gives factor loadings

#set variance of Cstor to 0

#Residuals
resid(model2_total_reg_fit)

#Model-implied covariance matrix
fitted(model2_total_reg_fit)
parameterEstimates(model2_total_reg_fit)#gives the currently used parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2_total_reg_fit, sort.=TRUE,minimum.value = 10)


##model2_total_cult## CHECK COMMENT OF THE OTHER CULTURAL MODEL; IN THIS CASE THE SAME APPLIES NO LATENT VARIABLES9
model2_total_cult<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable. Only human agency(ha) is defined here as latent variable
         #ha=~+educa+indig_censoP+indiv+jurid

         #Regressions
         tot_supl_recreation~inc+educa+indig_censoP+indiv+jurid+area
         inc~educa+indig_censoP+indiv+jurid+area
         area~educa+indig_censoP+indiv+jurid

         #Covariance structure(of latent variables)   
      
'

#Step 2: Model estimation
model2_total_cult_fit<-sem(model2_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_cult<-fitMeasures(model2_total_cult_fit, c("cfi","chisq","rmsea","srmr", "pvalue","ifi","tli", "nfi"))
std_cult2<-standardizedSolution(model2_total_cult_fit,type = c("std.all"))#standardized values for latent variables, regressions and covariances

inspect(model2_total_prov_fit,what="std")$lambda#gives factor loadings


#Residuals
resid(model2_total_cult_fit)

#Model-implied covariance matrix
fitted(model2_total_cult_fit)
parameterEstimates(model2_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2_total_cult_fit, sort.=TRUE,minimum.value = 10)


####Results model 2####
#goodness-of-fit
model2_total<-cbind(dos_a_total_prov,dos_a_total_reg,dos_a_total_cult)
colnames(model2_total)<c("provisioning","regulating", "cultural")
model2_total<-format(round(model2_total,3),nsmall = 3)
model2_total<-as.data.frame(model2_total)
#write.xlsx(model2_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)

#explanatory power of models
std_total2<-rbind(std_prov2,std_reg2,std_cult2 )
#write.xlsx(std_total2, "H:/SIG/Procesos SIG/Spatial distribution/Tables/std_total2.xlsx", row.names=TRUE, overwrite=TRUE)


#END#










































#former trials
##model 1
##
##
##
##
##These models correspond to previous tries, which I don't need anymore, but I am keeping just in case

