##Structural equation model (SEM) paper 3 with lavaan##
#Maria and Rachel->Please check the script and see if it makes sence, I have developed the models following the discussion we had last week.
#Let me know if you have trouble installing github/Rstudio connection.
#Remember to always pull before commiting and pushing! In this way you first incorporate changes that others have done and then you incorporate your changes into the repository.
#Select a root folder in your computer that you're sure you wont delete by accident, if you delete it being connected to the cloud repository everyone looses everything! I will anyhow make regular copies of our progress.


##Libraries##

#install.packages(c("lavaan","semPlot","corrplot","matrixcalc" ))
library(lavaan)
library(semPlot)
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


#hypothesis: land size inequality and human agency mediate the relationship between ES yield and income. We will test two models, number 1 were income is the outcome variable 
#and one were ES supply inequality is the outcome variable.

#Generating subset of variables to be used and new working dataframe named"db"

ha<-db_short[c("Prod_indiv","pers_jurid","indig_area","P_INDIG" ,"prof_ed_univ","age" )]##human agency
tot_sup<- db_short[c("tot_water_sup","tot_water_regulation","tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]##variables of total supply (all supply within the limits of each municipality)
product<-db_short[c("productivity_water_sup","productivity_water_regulation","productivity_cseq","productivity_cstor","productivity_erosion","productivity_timber", "productivity_recreation")]##productivity or yield variables (mean yield per ha)
ginis<-db_short[c("gini_water_sup_prod", "gini_water_sup_tot","gini_water_reg_prod","gini_water_reg_tot","gini_cseq_prod","gini_cseq_tot","gini_cstor_prod","gini_cstor_tot","gini_erosion_prod","gini_erosion_tot","gini_timber_prod","gini_timber_tot","gini_recreation_prod", "gini_recreation_tot")]##Gini coefficients of ES supply variables
income<-db_short[c("weighted_mean_income","weighted_gini_income")]##mean income & gini income (data chile)
area<-db_short[c("area_promedio_predios", "gini_land")]##mean area  of all the properties within a municipality
data.table(colnames(db_short))#check order of columns

db<-data.frame(ha,income,area, product,tot_sup,ginis)#
colnames(db)[c(1:10)]<-c("indiv","jurid","indig_censoA", "indig_censoP","educa","age","inc","gini_income", "area", "gini_area")
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
for (i in 1:37) {
  hist(db[,c(1:37)][,i],main=names(db [,c(1:37)])[i],xlab=names(db[,c(1:37)])[i])
}
#dev.off()
#check which normalization technique is the best for each variable. Note here that there are some variables that look kind of normal so they would not need a transformation. But here I calculated the best theoretical normalization method for all of them to be applied case by case.
#yield/productivity
bestNormalize(db$productivity_water_sup)
bestNormalize(db$productivity_water_regulation)
bestNormalize(db$productivity_cseq)
bestNormalize(db$productivity_cstor)
bestNormalize(db$productivity_erosion)
bestNormalize(db$productivity_timber)
bestNormalize(db$productivity_recreation)

#total supply
bestNormalize(db$tot_water_sup)
bestNormalize(db$tot_water_regulation)
bestNormalize(db$tot_supl_cseq)
bestNormalize(db$tot_supl_cstor)
bestNormalize(db$tot_supl_erosion)
bestNormalize(db$tot_supl_timber)
bestNormalize(db$tot_supl_recreation)

#human agency
#bestNormalize(db$pov_persons)
#bestNormalize(db$pov_percentage)
#bestNormalize(db$rur)
bestNormalize(db$indig_censoP)
bestNormalize(db$educa)
#bestNormalize(db$tot_pop)
#bestNormalize(db$dist_cities)
bestNormalize(db$indiv)
bestNormalize(db$jurid)
bestNormalize(db$age)
bestNormalize(db$area)
                
                
#income
bestNormalize(db$inc)
#bestNormalize(db$gini_income)

       
# Transform ecosystem service variables to try to meet normality.
db%>%dplyr::mutate(productivity_water_sup=predict(bestNormalize::sqrt_x(productivity_water_sup+1)),
               productivity_water_regulation=predict(bestNormalize::orderNorm         (productivity_water_regulation)),
                  productivity_cseq=predict(bestNormalize::orderNorm (productivity_cseq)),      productivity_cstor=predict(bestNormalize::sqrt_x(productivity_cstor+1)),
                        productivity_erosion=predict(bestNormalize::orderNorm(productivity_erosion)),
                        productivity_timber=predict(bestNormalize::orderNorm(productivity_timber)),
                        productivity_recreation=predict(bestNormalize::orderNorm(productivity_recreation)),
                       tot_water_sup=predict(arcsinh_x(productivity_water_sup)),
                        tot_water_regulation=predict(log_x(tot_water_regulation+1)),
                       tot_supl_cseq=predict(arcsinh_x(tot_supl_cseq)),           
                       tot_supl_cstor=predict(yeojohnson(tot_supl_cstor)),
                       tot_supl_erosion=predict(arcsinh_x(tot_supl_erosion)),
                       tot_supl_timber=predict(yeojohnson(tot_supl_timber)),
                        tot_supl_recreation=predict(bestNormalize::boxcox(tot_supl_recreation)),
                        #human agency
                         area=predict(bestNormalize::boxcox(area)),
                         indig_censoP=predict (bestNormalize::orderNorm(indig_censoP)),   
                          educa=predict (bestNormalize::orderNorm(educa)), 
                           indiv=predict(bestNormalize::orderNorm(indiv)),
                            jurid=predict(bestNormalize::yeojohnson(jurid)),
                             age=predict(bestNormalize::orderNorm(age)),
                          #income
                          inc=predict (bestNormalize::boxcox(inc))
               
                        
)->dbn#new data base with normalized variables is "dbn"

dbn<-lapply(dbn[,c(1:38)], scales::rescale)#rescaling data 0 to 1# This is done with "db" database to check for the recommendation of Rachel of looking at how results look like without normalizing the data. If you want to use normalized data change for dbn. Results show no changes in the results when using one or another database.
dbn<-as.data.frame(dbn)# transforming to dataframe again

#new histogram

par(mfrow= c (5,7),mar=c(1,2,2,0.5))     
for (i in 1:38) {
  hist(dbn[,c(1:38)][,i],main=names(dbn [,c(1:38)])[i],xlab=names(dbn [,c(1:38)])[i])
}
 
#check correlations between all measurement variables (exogenous variables)

db_cor<-db[c("indiv","jurid","indig_censoP","educa","age","inc","area", "productivity_water_regulation","productivity_cseq","productivity_cstor","productivity_erosion","productivity_timber","productivity_recreation","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")]#extract only variables to be visualized in correlations

colnames(db_cor)<-c("indiv","leg","indig","educa","age","inc","area","yield_w_reg","yield_cseq","yield_cs","yield_ero","yield_timber","yield_recre","tot_w_sup","tot_w_reg","tot_cseq","tot_cstor","tot_erosion","tot_timber","tot_recre")#change to shorter names for better display in correlation matrix

cor_datos<-round(cor(db[c("indiv","jurid","indig_censoP","educa","age","inc","area", "productivity_water_regulation","productivity_cseq","productivity_cstor","productivity_erosion","productivity_timber","productivity_recreation","tot_water_sup","tot_water_regulation", "tot_supl_cseq","tot_supl_cstor","tot_supl_erosion","tot_supl_timber","tot_supl_recreation")], use="pairwise.complete.obs", method="spearman"), 2)
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
         p.mat = cor_matrix$P, tl.col="black", sig.level = c(0.001, 0.01, 0.05), insig = "label_sig")#con corrplop, necesita valores r y P de corrr


write.csv(cor_datos, "H:/SIG/Procesos SIG/Spatial distribution/Tables/correlaciones1.csv")##

#cor_matrix<-rcorr(as.matrix(dbn[c(1:16)]))


#to check variable order again
data.table(colnames(dbn))

####Model 1: Regression model with "income" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural services####
  

####Model 1a (yield)####
#Step 1: Model specification
 
model1a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+indig_censoP+age+indiv+jurid+area

         #Regressions

        inc~ha+sup_prov
        sup_prov~ha
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
uno_a_yield_prov<-fitMeasures(model1a_yield_prov_fit, c("cfi","rmsea","srmr", "pvalue"))
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
          ha=~+educa+indig_censoP+age+indiv+jurid+area

         #Regressions
         inc~sup_reg+ha
         sup_reg~ha
         

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
uno_a_yield_reg<-fitMeasures(model1a_yield_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

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
         ha=~+educa+indig_censoP+age+indiv+jurid+area

         #Regressions
         inc~productivity_recreation+ha
         productivity_recreation~ha
         
        
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

####model 1a (total)####

#Step 1: Model specification

model1a_total_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+indig_censoP+age+indiv_produc+soc_prod+area

         #Regression
         gini_income~tot_prov+ha
         tot_prov~ha
         
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative
         # varrur>0
         # tot_prov~~vartot_prov*tot_prov
         # tot_prov>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)     
'

#Step 2: Model estimation
model1a_total_prov_fit<-sem(model1a_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_prov<-fitMeasures(model1a_total_prov_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1a_total_prov_fit)

#Model-implied covariance matrix
fitted(model1a_total_prov_fit)
parameterEstimates(model1a_total_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1a_total_prov_fit, sort.=TRUE,minimum.value = 10)

##model1a_total_reg##
model1a_total_reg<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_reg=~tot_water_regulation+tot_supl_cseq+tot_supl_cstor+tot_supl_erosion
          ha=~+educa+indig_censoP+age+indiv_produc+soc_prod+area

         #Regressions
         gini_income~tot_reg+ha
         tot_reg~ha

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        '

#Step 2: Model estimation
model1a_total_reg_fit<-sem(model1a_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_reg<-fitMeasures(model1a_total_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1a_total_reg_fit)

#Model-implied covariance matrix
fitted(model1a_total_reg_fit)
parameterEstimates(model1a_total_reg_fit)#gives the currently used parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1a_total_reg_fit, sort.=TRUE,minimum.value = 10)

##model1a_total_cult##
model1a_total_cult<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable. Only human agency(ha) is defined here as latent variable
        ha=~+educa+indig_censoP+age+indiv_produc+soc_prod+area

         #Regressions
         gini_income~tot_supl_recreation+ha
         tot_supl_recreation~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)   
      
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        
'
#Step 2: Model estimation
model1a_total_cult_fit<-sem(model1a_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model1a_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
uno_a_total_cult<-fitMeasures(model1a_total_cult_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model1a_total_cult_fit)

#Model-implied covariance matrix
fitted(model1a_total_cult_fit)
parameterEstimates(model1a_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model1a_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model1a_total_cult_fit, sort.=TRUE,minimum.value = 10)

####model 1b (yield)####

##model1b_yield_prov##
#Step 1: Model specification
model1b_yield_prov<-'#Structural model using inequality indicators
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
           gini_prov=~gini_water_sup_prod+gini_timber_prod
          ha=~+educa+indig_censoP+age+indiv_produc+soc_prod+area

         #Regressions
         gini_income~gini_prov+ha
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
           ha=~+educa+indig_censoP+age+indiv_produc+soc_prod+area

         #Regressions
         gini_income~gini_tot+ha
         gini_tot~ha
    
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


####results model 1####

model1_yield<-cbind(uno_a_yield_prov,uno_b_yield_prov,uno_a_yield_reg,uno_b_yield_reg,uno_a_yield_cult,uno_b_yield_cult)
colnames(model1_yield)<-c("raw_prov", "gini_prov","raw_reg","gini_reg", "raw_cult", "gini_cult")
model1_yield<-format(round(model1_yield,2),nsmall = 2)
#model1_yield<-as.data.frame(model1_yield)
model1_yield_gini_outcome<-as.data.frame(model1_yield)#I added "_not_normalized" to check how results look like without the normalization process as Rachel recommended. This line need to run if we're usinng the non-normalized data
#write.xlsx(model1_yield, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_yield.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data
write.xlsx(model1_yield_gini_outcome, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_yield_gini_outcome.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data




model1_total<-cbind(uno_a_total_prov,uno_b_total_prov,uno_a_total_reg,uno_b_total_reg,uno_a_total_cult,uno_b_total_cult)
colnames(model1_total)<-c("raw_prov", "gini_prov","raw_reg","gini_reg", "raw_cult", "gini_cult")
model1_total<-format(round(model1_total,2),nsmall = 2)
#model1_total<-as.data.frame(model1_total)
model1_total_gini_outcome<-as.data.frame(model1_total)###I added "_not_normalized" to check how results look like without the normalization process as Rachel recommended. This line need to run if we're usinng the non-normalized data
#write.xlsx(model1_total_not_normalized, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders
write.xlsx(model1_total_gini_outcome, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total_gini_outcomed.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data










#####Model 2: Regression model with "ES supply inequality" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural service.####




####Model 2a (yield)####
#Step 1: Model specification

model2a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+rur+indig

         #Regressions

         sup_prov~inc+ha+area
         inc~ha
         area~ha+inc
        
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
          ha=~+educa+rur+indig

         #Regressions

         sup_reg~inc+ha+area
         inc~ha
         area~ha+inc

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          rur~~varrur*rur#avoid variance of rurality to become negative
          varrur>0
          #gini_incomeome~~ha+area
         
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
         ha=~+educa+rur+indig

         #Regressions
         productivity_recreation~inc+ha+area
         area~ha+inc
         inc~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          rur~~varrur*rur#avoid variance of rurality to become negative. In some cases due to calculation probles the variances of some variables become slightly negative, which is bad, so you fix it to 0
          varrur>0
         
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


####model 2a (total)####

#Step 1: Model specification

model2a_total_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+rur+indig

         #Regression
         tot_prov~inc+ha+area
         inc~ha
         area~ha+inc
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative
         # varrur>0
         # tot_prov~~vartot_prov*tot_prov
         # tot_prov>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)     
'

#Step 2: Model estimation
model2a_total_prov_fit<-sem(model2a_total_prov, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_total_prov_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_prov<-fitMeasures(model2a_total_prov_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model2a_total_prov_fit)

#Model-implied covariance matrix
fitted(model2a_total_prov_fit)
parameterEstimates(model2a_total_prov_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_total_prov_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a_total_prov_fit, sort.=TRUE,minimum.value = 10)


##model2a_total_reg##
model2a_total_reg<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_reg=~tot_water_regulation+tot_supl_cseq+tot_supl_cstor+tot_supl_erosion
          ha=~+educa+rur+indig

         #Regressions
         tot_reg~inc+ha+area
         inc~ha
         area~ha+inc

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        '

#Step 2: Model estimation
model2a_total_reg_fit<-sem(model2a_total_reg, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_total_reg_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_reg<-fitMeasures(model2a_total_reg_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model2a_total_reg_fit)

#Model-implied covariance matrix
fitted(model2a_total_reg_fit)
parameterEstimates(model2a_total_reg_fit)#gives the currently used parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_total_reg_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a_total_reg_fit, sort.=TRUE,minimum.value = 10)



##model2a_total_cult##
model2a_total_cult<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured. there is no latent variable for the recreation ES as it is only one variable. Only human agency(ha) is defined here as latent variable
         ha=~+educa+rur+indig

         #Regressions
         tot_supl_recreation~inc+tot_supl_recreation+ha+area
         inc~ha
         area~ha+inc
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)   
         
      
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be gini_incomeluded in the model)
        
'
#Step 2: Model estimation
model2a_total_cult_fit<-sem(model2a_total_cult, data=dbn, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE, orthogonal=TRUE

#Step 3: Evaluate the model
summary(model2a_total_cult_fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
dos_a_total_cult<-fitMeasures(model2a_total_cult_fit, c("cfi","rmsea","srmr", "pvalue"))

#Residuals
resid(model2a_total_cult_fit)

#Model-implied covariance matrix
fitted(model2a_total_cult_fit)
parameterEstimates(model2a_total_cult_fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model
semPlot::semPaths(model2a_total_cult_fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a_total_cult_fit, sort.=TRUE,minimum.value = 10)


####Results model 2####

model2_yield<-cbind(dos_a_yield_prov,dos_a_yield_reg,dos_a_yield_cult)
colnames(model2_yield)<-c("provisioning","regulating", "cultural")
model2_yield<-format(round(model2_yield,2),nsmall = 2)
#model2_yield<-as.data.frame(model2_yield)
model2_yield<-as.data.frame(model2_yield)#I added "_not_normalized" to check how results look like without the normalization process as Rachel recommended. This line need to run if we're usinng the non-normalized data
#write.xlsx(model2_yield, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_yield.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data
write.xlsx(model2_yield, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_yield.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data



model2_total<-cbind(dos_a_total_prov,dos_a_total_reg,dos_a_total_cult)
colnames(model2_total)<-c("raw_prov","raw_reg", "raw_cult")
model2_total<-format(round(model2_total,2),nsmall = 2)
#model2_total<-as.data.frame(model2_total)
model2_total<-as.data.frame(model2_total)###I added "_not_normalized" to check how results look like without the normalization process as Rachel recommended. This line need to run if we're usinng the non-normalized data
#write.xlsx(model2_total_not_normalized, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders
write.xlsx(model2_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data






##Model comparison: ANOVA

anova(model1, model2)


































