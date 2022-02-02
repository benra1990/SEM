##Structural equation model (SEM) paper 3 with lavaan##
#Maria and Rachel->Please check the script and see if it makes sense, I have developed the models following the discussion we had last week.
#Let me know if you have trouble installing github/Rstudio connection.
#Remember to always pull before commiting and pushing! In this way you first incorporate changes that others have done and then you incorporate your changes into the repository.
#Select a root folder in your computer that you're sure you wont delete by accident, if you delete it being connected to the cloud repository everyone looses everything! I will anyhow make regular copies of our progress.


##Libraries##

#install.packages(c("lavaan","semPlot","corrplot", "semTable" ))
library(lavaan)
library(semPlot)
library(semTable)
library(matrixcalc)
library(corrplot)
library(openxlsx)
library(data.table)
#install.packages("bestNormalize")
library(bestNormalize)
library(corrplot)
#install.packages("corrr")
library(corrr)
library(Hmisc)
library(dplyr)
library(xtable)


##Import database
db_short<-read.xlsx("H:/SIG/Procesos SIG/BD_inequity/base de datos/db_short.xlsx")# this will change depending on where you have it on your computers!
str(db_short)#check structure
data.table(colnames(db_short))#get order of columns in the dataframe


#hypothesis:property area and  and human agency drive the relationship between ES yield and income. We will test two models, number 1 were income is the outcome variable and one were ES supply inequality is the outcome variable. This is the so called "bidirectionality" because this linkages represent one way - the more common way in the ES cascade from ES supply to wellbeing - and the opposite direction from wellbeing to ES supply.

#Generating subset of variables to be used and new working dataframe named"db"

ha<-db_short[c("Pers_nat","pers_jurid","P_INDIG" ,"prof_ed_univ")]##human agency
tot_sup<- db_short[c("tot_water_sup","tot_water_regulation","tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]##variables of total supply (all supply within the limits of each municipality)
income<-db_short[c("weighted_mean_income")]##mean income & gini income (data chile)
area<-db_short[c("area_promedio_predios")]##mean area  of all the properties within a municipality

db<-data.frame(ha,income,area, tot_sup)#
data.table(colnames(db))#check order of columns
colnames(db)[c(1:6)]<-c("indiv","jurid", "indig_censoP","educa","inc", "area")
data.table(colnames(db))

sapply(db, function(x) sum(is.na(x)))#looking at NA in each column

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

#check correlations between all measurement variables (exogenous variables)

db_cor<-db[c("indiv","jurid","indig_censoP","educa","inc","area","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]#extract only variables to be visualized in correlations"productivity_water_regulation","productivity_cseq","productivity_cstor","productivity_erosion","productivity_timber","productivity_recreation

colnames(db_cor)<-c("individual person tenure","legal person tenure","indigenous population","university education","income","property area","water supply","water regulation","carbon sequestration","carbon storage","sediment retention","native timber","recreation")#change to shorter names for better display in correlation matrix"yield_w_reg","yield_cseq","yield_cs","yield_ero","yield_timber","yield_recre"

cor_datos<-round(cor(db[c("indiv","jurid","indig_censoP","educa","inc","area","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")], use="pairwise.complete.obs", method="spearman"), 2)
str(cor_datos)


##con HMSIC##
cor_matrix<-rcorr(as.matrix(db_cor))#con corrr produce valores r y P


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
matriz_plana<-flattenCorrMatrix(cor_matrix$r, cor_matrix$P)##visualizar matriz plana


corrplot(cor_matrix$r,  type="lower", method = "square", order="original", pch.cex = 0.8, pch.col="black",
         p.mat = cor_matrix$P, tl.col="black", sig.level = c(0.001, 0.01, 0.05), insig = "label_sig", diag=FALSE)#con corrplop, necesita valores r y P de corrr


write.csv(cor_datos, "H:/SIG/Procesos SIG/Spatial distribution/Tables/correlaciones1.csv")##


#check which normalization technique is the best for each variable. Note here that there are some variables that look kind of normal so they would not need a transformation. But here I calculated the best theoretical normalization method for all of them to be applied case by case.

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
  
#Land endowment
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

####Model 1: Regression model with "income" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models depending on the ES category. prov= provisioning services; reg=regulating services; cult:cultural services####


####model 1 (total)####

#Step 1: Model specification

model1_total_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+indig_censoP+indiv+jurid
        
        
          #Regression
         inc~tot_prov+ha+area
         tot_prov~ha+area
         area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
#Covariance structure(of latent variables)
         tot_supl_timber~~0*tot_supl_timber#avoid variance of rurality to become negative. It is possible that the model fits the data well but that the true value of the residual variance is very close to zero. If this happens then the estimate can become negative because of sampling fluctuations. Ifyou think that this is the case then you force lavaan to maintain non-negative variance estimates by specifying a non-linear constraint on this residual variance
         
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)     
'

#Step 2: Model estimation
model1_total_prov_fit<-sem(model1_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_prov<-fitMeasures(model1_total_prov_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))
std_prov1<-standardizedSolution(model1_total_prov_fit)#standardized values for latent variables, regressions and covariances
#varianace of cstor negative, fix it to 0

#Residuals
resid(model1_total_prov_fit)

#Model-implied covariance matrix
fitted(model1_total_prov_fit)
parameterEstimates(model1_total_prov_fit, ci=FALSE, rsquare=TRUE, standardized=TRUE)#gives the currently used parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_prov_fit, sort.=TRUE,minimum.value = 10)

##model1_total_reg##
model1_total_reg<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
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
model1_total_reg_fit<-sem(model1_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_reg<-fitMeasures(model1_total_reg_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))
std_reg1<-standardizedSolution(model1_total_reg_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model1_total_reg_fit)

#Model-implied covariance matrix
fitted(model1_total_reg_fit)
parameterEstimates(model1_total_reg_fit)#gives the currently used parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_reg_fit, sort.=TRUE,minimum.value = 10)
#recommended modification indiv~~jurid

##model1_total_cult##THIS MODEL REQUIRES SPECIAL ATTENTION!
model1_total_cult<-'#Structural model using raw indicators - ES yield (cultural ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable. Only human agency(ha) is defined here as latent variable
         #!this model does not run with just one Latent Variable, which would be in this case human agency, we have only one ES, hence we cant build a secont latent variable. Instead we just use all variables as measurement variables. But maybe rachel you have a better solution? I actually havent found anything saying that it is not possible to use just one latent variable, but as the model does not run I assuming thats the case. The results are as you might see weird (fit indices are perfect)
         #ha=~+educa+indig_censoP+indiv+jurid
         

         #Regressions
         inc~educa+indig_censoP+indiv+jurid+tot_supl_recreation+area
          tot_supl_recreation~educa+indig_censoP+indiv+jurid+area
          area~educa+indig_censoP+indiv+jurid

         #Covariance structure(of latent variables) 
         #indiv~~jurid
      
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        
'
#Step 2: Model estimation
model1_total_cult_fit<-sem(model1_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_cult<-fitMeasures(model1_total_cult_fit, c("cfi","tli", "pnfi","chisq","df","rmsea","srmr", "pvalue"))
std_cult1<-standardizedSolution(model1_total_reg_fit)#standardized values for latent variables, regressions and covariances

#Residuals
resid(model1_total_cult_fit)

#Model-implied covariance matrix
fitted(model1_total_cult_fit)
parameterEstimates(model1_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1_total_cult_fit, sort.=TRUE,minimum.value = 10)



####results model 1####

#goodness-of-fit
model1_total<-cbind(uno_a_total_prov,uno_a_total_reg,uno_a_total_cult)
colnames(model1_total)<-c("provisioning","regulating","cultural")
model1_total<-format(round(model1_total,3),nsmall = 3)
model1_total<-as.data.frame(model1_total)
write.xlsx(model1_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data
#another way of extracting
fit_indicators<-compareLavaan(list_model1_yield,file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/fit_indicators", fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                                                                                                                    "cfi", "tli", "srmr", "aic", "bic"),chidif = FALSE, type="html")

#explanatory power of models# this will be a longer table and probably will go to the appendix but some results are key in my opinion to compara "both directions", as as you will see with model 2 the goodness-of.fit indicators are equal but the estimates, standard errors and so on are quote different among variables.
#with standardizedsolution function
std_total1<-rbind(std_prov1,std_reg1,std_cult1)
write.xlsx(std_total1, "H:/SIG/Procesos SIG/Spatial distribution/Tables/std_total1.xlsx", row.names=TRUE, overwrite=TRUE)

#with semTable
list_model1_total<-list(model1_total_prov_fit,model1_total_reg_fit)#,model1_total_cult_fit)#original model fits

sem_tabla<-semTable(list_model1_total, file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/sem_tabla_model1", paramSets="all",type="html")#change location to see table



#####Model 2: Regression model with "ES supply inequality" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural service.####





####model 2 (total)####

#Step 1: Model specification

model2_total_prov<-'#Structural model using raw indicators - total ES supply (provisioning ES)
        
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
model2_total_prov_fit<-sem(model2_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

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
model2_total_reg_fit<-sem(model2_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_reg<-fitMeasures(model2_total_reg_fit, c("cfi","chisq","rmsea","srmr", "pvalue","ifi","tli", "nfi"))
std_reg2<-standardizedSolution(model2_total_reg_fit)#standardized values for latent variables, regressions and covariances

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
std_cult2<-standardizedSolution(model2_total_cult_fit)#standardized values for latent variables, regressions and covariances

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
write.xlsx(model2_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)

#another way of extracting
fit_indicators<-compareLavaan(list_model1_yield,file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/fit_indicators", fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                                                                                                                    "cfi", "tli", "srmr", "aic", "bic"),chidif = FALSE, type="html")


#explanatory power of models# 
#this will be a longer table and probably will go to the appendix but some results are key in my opinion to compara "both directions", as as you will see with model 2 the goodness-of.fit indicators are equal but the estimates, standard errors and so on are quote different among variables.
#with standardizedsolution function
std_total2<-rbind(std_prov2,std_reg2,std_cult2 )
write.xlsx(std_total2, "H:/SIG/Procesos SIG/Spatial distribution/Tables/std_total2.xlsx", row.names=TRUE, overwrite=TRUE)

#with semTable
list_model2_total<-list(model2_total_prov_fit,model2_total_reg_fit)#,model2_total_cult_fit)#original model fits

sem_tabla<-semTable(list_model2_total, file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/sem_tabla_model2", paramSets="all",type="html")#change location to see table

#END#










































#former trials
##model 1
##
##
##
##
##These models correspond to previous tries, which I don't need anymore, but I am keeping just in case
####Model 1a (yield)####
#Step 1: Model specification

model1a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions

        gini_income~ha+sup_prov+area
        sup_prov~ha+area
#area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
      
'
#Step 2: Model estimation

model1a_yield_prov_fit<-sem(model1a_yield_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_yield_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_yield_prov<-fitMeasures(model1a_yield_prov_fit, c("cfi","rmsea","srmr", "pvalue","ifi","tli", "nfi"))
inspect(model1a_yield_prov_fit,"r2")

#Residuals
resid(model1a_yield_prov_fit)

#Model-implied covariance matrix
fitted(model1a_yield_prov_fit)
parameterEstimates(model1a_yield_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_yield_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices (it's good for exploring but we need to assess if it makes sense to use the suggestions)
modindices(model1a_yield_prov_fit, sort.=TRUE,minimum.value = 10)

##model1a_yield_reg##

model1a_yield_reg<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured

          sup_reg=~productivity_water_regulation+productivity_cseq+productivity_cstor+productivity_erosion
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         inc~sup_reg+ha+atk_area
         sup_reg~ha+atk_area
         

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          #rur~~varrur*rur#avoid variance of rurality to become negative
          #varrur>0
          #gini_income~~ha+area
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        '

#Step 2: Model estimation
model1a_yield_reg_fit<-sem(model1a_yield_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_yield_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)

#summary(model1a_yield_fit, fit.measures=TRUE)
uno_a_yield_reg<-fitMeasures(model1a_yield_reg_fit, c("cfi","rmsea","srmr", "pvalue","ifi","tli", "nfi"))

#Residuals
resid(model1a_yield_reg_fit)

#Model-implied covariance matrix
fitted(model1a_yield_reg_fit)
parameterEstimates(model1a_yield_reg_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_yield_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1a_yield_reg_fit, sort.=TRUE,minimum.value = 10)

##model1a_yield_cult##
model1a_yield_cult<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
       #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable.          Only human agency(ha) is defined here as latent variable
         ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         inc~productivity_recreation+ha+area
         productivity_recreation~ha+area
         
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative. In some cases due to calculation probles the variances of some variables become slightly negative, which is bad, so you fix it to 0
          #varrur>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
         
         '


#Step 2: Model estimation
model1a_yield_cult_fit<-sem(model1a_yield_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_yield_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_yield_cult<-fitMeasures(model1a_yield_cult_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1a_yield_cult_fit)

#Model-implied covariance matrix
fitted(model1a_yield_cult_fit)
parameterEstimates(model1a_yield_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_yield_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1a_yield_cult_fit, sort.=TRUE,minimum.value = 10)

####model 1b (yield)####

##model1b_yield_prov##
#Step 1: Model specification
model1b_yield_prov<-'#Structural model using inequality indicators
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
           gini_prov=~gini_water_sup_prod+gini_timber_prod
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         inc~gini_prov+ha+area
         gini_prov~ha
         gini_area~ha
    
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model) 
'

#Step 2: Model estimation
model1b_yield_prov_fit<-sem(model1b_yield_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model

summary(model1b_yield_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_yield_prov<-fitMeasures(model1b_yield_prov_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1b_yield_prov_fit)

#Model-implied covariance matrix
fitted(model1b_yield_prov_fit)
parameterEstimates(model1b_yield_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_yield_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_yield_prov_fit, sort.=TRUE,minimum.value = 10)

##model1b_yield_reg##
#Step 1: Model specification
model1b_yield_reg<-'#Structural model using inequality indicators
        
         #Measurement models/defining latent variables,variables that cannot be directly measured       
         gini_reg=~gini_water_reg_prod+gini_cseq_prod+gini_cstor_prod+gini_erosion_prod
         ha=~+educa+rur+indig_censoP
        
         #Regressions
         gini_gini_incomeome~gini_reg+ha+gini_area
         gini_reg~ha+gini_area
         gini_area~ha
         
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         #~~
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)     
           
'

#Step 2: Model estimation
model1b_yield_reg_fit<-sem(model1b_yield_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model
summary(model1b_yield_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_yield_reg<-fitMeasures(model1b_yield_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1b_yield_reg_fit)

#Model-implied covariance matrix
fitted(model1b_yield_reg_fit)
parameterEstimates(model1b_yield_reg_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_yield_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_yield_reg_fit, sort.=TRUE,minimum.value = 10)

##model1b_yield_cult##
#Step 1: Model specification
model1b_yield_cult<-'#Structural model using inequality indicators
        
         #Measurement models/defining latent variables,variables that cannot be directly measured      
         ha=~+educa+rur+indig_censoP

         #Regressions
         gini_gini_incomeome~gini_recreation_prod+ha+gini_area
         gini_recreation_prod~ha+gini_area
         gini_area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
'

#Step 2: Model estimation
model1b_yield_cult_fit<-sem(model1b_yield_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model
summary(model1b_yield_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_yield_cult<-fitMeasures(model1b_yield_cult_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1b_yield_cult_fit)

#Model-implied covariance matrix
fitted(model1b_yield_cult_fit)
parameterEstimates(model1b_yield_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_yield_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_yield_cult_fit, sort.=TRUE,minimum.value = 10)

####model 1b (total)####
model1b_total_prov<-'#Structural model using inequality indicators  
         #Measurement models/defining latent variables,variables that cannot be directly measured       
           gini_tot=~gini_water_sup_tot+gini_timber_tot
           ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         gini_income~gini_tot+ha+area
         gini_tot~ha+area
    
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
'

#Step 2: Model estimation
model1b_total_prov_fit<-sem(model1b_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model
summary(model1b_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_total_prov<-fitMeasures(model1b_total_prov_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1b_total_prov_fit)

#Model-implied covariance matrix
fitted(model1b_total_prov_fit)
parameterEstimates(model1b_total_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_total_prov_fit, sort.=TRUE,minimum.value = 10)

##model1b_total_reg##
#Step 1: Model specification
model1b_total_reg<-'#Structural model using inequality indicators       
         #Measurement models/defining latent variables,variables that cannot be directly measured
         gini_reg=~gini_water_reg_tot+gini_cseq_tot+gini_cstor_tot+gini_erosion_tot
         ha=~+educa+rur+indig_censoP
        
         #Regressions
         gini_income~gini_reg+ha+gini_area
         gini_reg~ha+gini_area
         gini_area~ha
         
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         #~~
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model    
'

#Step 2: Model estimation
model1b_total_reg_fit<-sem(model1b_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model
summary(model1b_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_total_reg<-fitMeasures(model1b_total_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1b_total_reg_fit)

#Model-implied covariance matrix
fitted(model1b_total_reg_fit)
parameterEstimates(model1b_total_reg_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_total_reg_fit, sort.=TRUE,minimum.value = 10)

##model1b_total_cult##

#Step 1: Model specification
model1b_total_cult<-'#Structural model using inequality indicators        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         ha=~+educa+rur+indig_censoP
         
         #Regressions
         gini_income~gini_recreation_tot+ha+gini_area
         gini_recreation_tot~ha+gini_area
         gini_area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
'

#Step 2: Model estimation
model1b_total_cult_fit<-sem(model1b_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model
summary(model1b_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_b_total_cult<-fitMeasures(model1b_total_cult_fit, c("cfi","rmsea","srmr", "pvalue"))
inspect(model1b_total_cult_fit, "rsquare")

#Residuals
resid(model1b_total_cult_fit)

#Model-implied covariance matrix
fitted(model1b_total_cult_fit)
parameterEstimates(model1b_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1b_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1b_total_cult_fit, sort.=TRUE,minimum.value = 10)


# model 2
# ####Model 2a (yield)####
#Step 1: Model specification

model2a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+indig_censoP+age+indiv+jurid
          
         #Regressions

         sup_prov~inc+ha+area
         inc~ha+area
         
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
      
'
#Step 2: Model estimation

model2a_yield_prov_fit<-sem(model2a_yield_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_yield_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_yield_prov<-fitMeasures(model2a_yield_prov_fit, c("cfi","rmsea","srmr", "pvalue"))
inspect(model2a_yield_prov_fit,"r2")
inspect(model2a_yield_prov_fit,"vcov")

#Residuals
resid(model2a_yield_prov_fit)

#Model-implied covariance matrix
fitted(model2a_yield_prov_fit)
parameterEstimates(model2a_yield_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_yield_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices (it's good for exploring but we need to assess if it makes sense to use the suggestions)
modindices(model2a_yield_prov_fit, sort.=TRUE,minimum.value = 10)


##model2a_yield_reg##

model2a_yield_reg<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
           #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_reg=~productivity_water_sup+productivity_timber
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions

         sup_reg~inc+ha+area
         inc~ha+area
         

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative
         # varrur>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        '

#Step 2: Model estimation
model2a_yield_reg_fit<-sem(model2a_yield_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_yield_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)

#summary(model2a_yield_fit, fit.measures=TRUE)
dos_a_yield_reg<-fitMeasures(model2a_yield_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model2a_yield_reg_fit)

#Model-implied covariance matrix
fitted(model2a_yield_reg_fit)
parameterEstimates(model2a_yield_reg_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_yield_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a_yield_reg_fit, sort.=TRUE,minimum.value = 10)


##model2a_yield_cult##
model2a_yield_cult<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
       #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable.          Only human agency(ha) is defined here as latent variable
         ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         productivity_recreation~inc+ha+area
         inc~ha+area
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative. In some cases due to calculation probles the variances of some variables become slightly negative, which is bad, so you fix it to 0
         #varrur>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
         
         '


#Step 2: Model estimation
model2a_yield_cult_fit<-sem(model2a_yield_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_yield_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_yield_cult<-fitMeasures(model2a_yield_cult_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model2a_yield_cult_fit)

#Model-implied covariance matrix
fitted(model2a_yield_cult_fit)
parameterEstimates(model2a_yield_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_yield_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a_yield_cult_fit, sort.=TRUE,minimum.value = 10)









#bestNormalize old
productivity_water_sup=predict(bestNormalize::sqrt_x(productivity_water_sup+1)),
productivity_water_regulation=predict(bestNormalize::orderNorm         (productivity_water_regulation)),
productivity_cseq=predict(bestNormalize::orderNorm (productivity_cseq)),      productivity_cstor=predict(bestNormalize::sqrt_x(productivity_cstor+1)),
productivity_erosion=predict(bestNormalize::orderNorm(productivity_erosion)),
productivity_timber=predict(bestNormalize::orderNorm(productivity_timber)),
productivity_recreation=predict(bestNormalize::orderNorm(productivity_recreation)),

#yield
Atkinson_water_sup_prod=predict(bestNormalize::log_x(Atkinson_water_sup_prod)),
Atkinson_water_reg_prod=predict(bestNormalize::center_scale(Atkinson_water_reg_prod)),
Atkinson_cstor_prod=predict(bestNormalize::log_x(Atkinson_cstor_prod+1)),
gini_cseq_prod=predict(bestNormalize::orderNorm(gini_cseq_prod)),
Atkinson_erosion_prod=predict(bestNormalize::yeojohnson(Atkinson_erosion_prod)),
Atkinson_recreation_prod=predict(bestNormalize::orderNorm(Atkinson_recreation_prod)),
Atkinson_timber_prod=predict(bestNormalize::orderNorm(Atkinson_timber_prod)),
#total
Atkinson_water_sup_tot=predict(bestNormalize::sqrt_x(Atkinson_water_sup_tot)),
Atkinson_water_reg_tot=predict(bestNormalize::orderNorm(Atkinson_water_reg_tot)),
Atkinson_cstor_tot=predict(bestNormalize::center_scale(Atkinson_cstor_tot)),
gini_cseq_tot=predict(bestNormalize::yeojohnson(gini_cseq_tot)),
Atkinson_erosion_tot=predict(bestNormalize::orderNorm(Atkinson_erosion_tot)),
Atkinson_recreation_tot=predict(bestNormalize::log_x(Atkinson_recreation_tot+1)),
Atkinson_timber_tot=predict(bestNormalize::arcsinh_x(Atkinson_timber_tot)),    
