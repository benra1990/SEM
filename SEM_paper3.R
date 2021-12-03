##Structural equation model (SEM) paper 3 with lavaan##
#Maria and Rachel->Please check the script and see if it makes sence, I have developed the models following the discussion we had last week.
#Let me know if you have trouble installing github/Rstudio connection.
#Remember to always pull before commiting and pushing! In this way you first incorporate changes that others have done and then you incorporate your changes into the repository.
#Select a root folder in your computer that you're sure you wont delete by accident, if you delete it being connected to the cloud repository everyone looses everything! I will anyhow make regular copies of our progress.


##Libraries##

install.packages(c("lavaan","semPlot","corrplot","bestNormalize" ))
library(lavaan)
library(semPlot)
library(corrplot)
library(openxlsx)
library(data.table)
library(bestNormalize)
library(corrplot)
library(corrr)
library(Hmisc)

#hola#

##Import database
db_short<-read.xlsx("H:/SIG/Procesos SIG/BD_inequity/base de datos/database_short.xlsx")# this will change depending on where you have it on your computers!
str(db_short)#check structure
data.table(colnames(db_short))#get order of columns in the dataframe


#hypothesis: land size inequality and human agency mediate the relationship between ES yield and income. We will test two models, number 1 were income is the outcome variable 
#and one were ES supply inequality is the outcome variable.

#Generating subset of variables to be used and new working dataframe named"db"

ha<-db_short[c(40:43,44:46,54)]##human agency
tot_sup<- dplyr::select(db_short, starts_with("tot"))##variables of total supply (all supply within the limits of each municipality)
product<-dplyr::select(db_short, starts_with("productivity"))##productivity or yield variables (mean yield per ha)
ginis<-dplyr::select(db_short, starts_with("gini"))##Gini coefficients of ES supply variables
income<-db_short[c(55,57)]##mean income & gini income
area<-db_short[,52]##mean area  of all the properties within a municipality
data.table(colnames(db_short))#check order of columns

db<-data.frame(ha,income,area, product,tot_sup,ginis)#leaving out tot.sup for the moment
db<-db[-c(16,19,25,28:29,40:41,48)]#eliminating ginis for total supply and nntp variables
colnames(db)[c(1:6,8:9,41)]<-c("pov_persons", "pov_percentage","rur", "indig", "forest_area","educa","dist_cities","inc", "gini_income")
data.table(colnames(db))

sapply(db, function(x) sum(is.na(x)))#looking at NA in each column

#Check the data and transform when needed (scale and normalize)

unlist(lapply(db,is.numeric))##tells you which columns are numeric

for (i in 1:length(db))  { ##transform all variables to numeric
  db[,i] <- as.numeric (db[,i])
}

#Histograms for all variables
windows()
par(mfrow= c (5,8),mar=c(1,2,2,0.5))     
for (i in 1:39) {
  hist(db[,c(1:40)][,i],main=names(db [,c(1:40)])[i],xlab=names(db[,c(1:40)])[i])
}
dev.off()
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
bestNormalize(db$pov_persons)
bestNormalize(db$pov_percentage)
bestNormalize(db$rur)
bestNormalize(db$indig)
#bestNormalize(db$forest_area)
bestNormalize(db$educa)
bestNormalize(db$tot_pop)
bestNormalize(db$dist_cities)
                 
#income
bestNormalize(db$income)
bestNormalize(db$gini_income)

#land size
bestNormalize(db$area)
       
# Transform ecosystem service variables to try to meet normality
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
                        area=predict(bestNormalize::boxcox(area)),
                   pov_persons=predict(bestNormalize::orderNorm(pov_persons)),
                      pov_percentage=predict(bestNormalize::arcsinh_x(pov_percentage)),   
                          rur=predict(bestNormalize::yeojohnson(rur)),   
                               indig=predict (bestNormalize::orderNorm(indig)),   
                                  educa=predict (bestNormalize::orderNorm(educa)), 
                                    tot_pop=predict (bestNormalize::boxcox(tot_pop)), 
                                       dist_cities=predict (bestNormalize::orderNorm(dist_cities)),
               inc=predict (bestNormalize::boxcox(inc)),
               gini_inc=predict (orderNorm(gini_inc))
                        
)->dbn#new data base with normalized variables is "dbn"

dbn<-lapply(db[,c(1:41)], scales::rescale)#rescaling data 0 to 1
dbn<-as.data.frame(dbn)# transforming to dataframe again

#new histogram

par(mfrow= c (5,8),mar=c(1,2,2,0.5))     
for (i in 1:40) {
  hist(dbn[,c(1:40)][,i],main=names(dbn [,c(1:40)])[i],xlab=names(dbn [,c(1:40)])[i])
}
 
#check correlations between measurable variables of human agency
cor_datos<-cor(db[,2:8], use="pairwise.complete.obs", method="spearman")
cor_matrix<-rcorr(as.matrix(db[,2:8]))

#to check variable order again
data.table(colnames(dbn))

####Model 1: Regression model with "income" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural services####
  

####Model 1a (yield)####
#Step 1: Model specification
 
model1a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+rur+indig

         #Regressions

         inc~sup_prov+ha+area
         sup_prov~ha+area
         area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
      
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
          ha=~+educa+rur+indig

         #Regressions
         inc~sup_reg+ha+area
         sup_reg~ha+area
         area~ha

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          rur~~varrur*rur#avoid variance of rurality to become negative
          varrur>0
          #income~~ha+area
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
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
         ha=~+educa+rur+indig

         #Regressions
         inc~productivity_recreation+ha+area
         productivity_recreation~ha+area
         area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          rur~~varrur*rur#avoid variance of rurality to become negative. In some cases due to calculation probles the variances of some variables become slightly negative, which is bad, so you fix it to 0
          varrur>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)'
        

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
          ha=~+educa+rur+indig

         #Regression
         inc~tot_prov+ha+area
         tot_prov~ha+area
         area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         # rur~~varrur*rur#avoid variance of rurality to become negative
         # varrur>0
         # tot_prov~~vartot_prov*tot_prov
         # tot_prov>0
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)     
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
          ha=~+educa+rur+indig

         #Regressions
         inc~tot_reg+ha+area
         tot_reg~ha+area
         area~ha

        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          
          #~~
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
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
         ha=~+educa+rur+indig

         #Regressions
         inc~tot_supl_recreation+ha+area
         tot_supl_recreation~ha+area
         area~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)   
      
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
        
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
           ha=~+educa+rur+indig

         #Regressions
         gini_income~gini_prov+ha+gini_land
         gini_prov~ha+gini_land
         gini_land~ha
    
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model) 
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
         ha=~+educa+rur+indig
        
         #Regressions
         gini_income~gini_reg+ha+gini_land
         gini_reg~ha+gini_land
         gini_land~ha
         
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         #~~
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)       
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
         ha=~+educa+rur+indig

         #Regressions
         gini_income~gini_recreation_prod+ha+gini_land
         gini_recreation_prod~ha+gini_land
         gini_land~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
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
           ha=~+educa+rur+indig

         #Regressions
         gini_income~gini_tot+ha+gini_land
         gini_tot~ha+gini_land
         gini_land~ha
    
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
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
         ha=~+educa+rur+indig
        
         #Regressions
         gini_income~gini_reg+ha+gini_land
         gini_reg~ha+gini_land
         gini_land~ha
         
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         #~~
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model    
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
         ha=~+educa+rur+indig
         
         #Regressions
         gini_income~gini_recreation_tot+ha+gini_land
         gini_recreation_tot~ha+gini_land
         gini_land~ha
        
        #New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
         
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
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
model1_yield<-as.data.frame(model1_yield)
write.xlsx(model1_yield, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_yield.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders!

model1_total<-cbind(uno_a_total_prov,uno_b_total_prov,uno_a_total_reg,uno_b_total_reg,uno_a_total_cult,uno_b_total_cult)
colnames(model1_total)<-c("raw_prov", "gini_prov","raw_reg","gini_reg", "raw_cult", "gini_cult")
model1_total<-format(round(model1_total,2),nsmall = 2)
model1_total<-as.data.frame(model1_total)
write.xlsx(model1_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders



##I haven't developed model 2 yet!


#####Model 2: Regression model with "ES supply inequality" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural service.####

model2a<-'#Structural model provisioning services(the service category is changes in each run)
        
         #Measurement models/defining latent variables,variables that cannot be dierectly measured
         
         #sup_prov=~productivity_water_sup+productivity_timber
          sup_prov=~tot_water_sup+tot_supl_timber
           
         ha=~+educa+rur+indig+dist_cities+tot_pop
         
         #g_sup=~gini_water_sup_prod+gini_timber_prod
         g_sup=~gini_water_sup_tot+gini_timber_tot
         
         income=~inc
         
  
         #Regressions
         g_sup~sup_prov
         sup_prov~ha
         sup_prov~area
         area~ha
         area~income
         g_sup~income
     
        
        # New parameter (possible new indirect parameter if there are some)
           #g_sup:=ha*area#indirect effect

         #Covariance structure(of latent variables)
          #g_sup~~sup_prov+area+ha
          #ha~~ha
          #sup_prov~~sup_prov
          
          
         #Residual covariance (this is for measurement variables for which we think covariance or variance should be included in the model)
        
          #dist_cities~~rur
          #tot_pop~~rur
          #tot_pop~~indig
          #educa~~indig
          #educa~~rur
          #pov_percentage~~rur
          #indig~~indig
          #rur~rur
         # pov_percentage~~pov_percentage
         # tot_pop~~tot_pop
          #educa~~educa
         # dist_cities~~dist_cities
          #productivity_water_sup~~productivity_timber
        
'


#Step 2: Model estimation

model2a.fit<-sem(model2a, data=db, meanstructure=FALSE, estimator="ML", check.gradient=FALSE)#auto.cov.y=TRUE

#Step 3: Evaluate the model

summary(model2a.fit, rsquare=TRUE, fit.measures=TRUE,standardized=TRUE)
summary(model2a.fit, fit.measures=TRUE)

# Residuals
resid(model2a.fit)

#Model-implied covariance matrix
fitted(model2a.fit)

parameterEstimates(model2a.fit)#gives the currently use parameters of the model fit

#Step 4: VIsualize the path model

semPlot::semPaths(model1a.fit, rotation=2, layout="tree2",intercepts=FALSE, what="std",whatLabels="std",posCol="black", edge.width=0.5, style="Lisrel", fade=FALSE, edge.label.position=0.55, curve = 1.75)

#Modification of indices
modindices(model2a.fit, sort.=TRUE,minimum.value = 10)







##Model comparison: ANOVA

anova(model1, model2)








