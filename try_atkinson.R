####Model 1: Regression model with "income" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural services####


####Model 1a (yield)####
#Step 1: Model specification

model1a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~Atkinson_water_sup_prod+Atkinson_timber_prod
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions

        inc~ha+sup_prov+atk_area
        sup_prov~ha+atk_area
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
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         gini_income~sup_reg+ha+atk_area
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
         ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         inc~productivity_recreation+ha+Atk
         productivity_recreation~ha+Atk
         
        
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
          ha=~+educa+indig_censoP+age+indiv+jurid
        
        
          #Regression
         inc~tot_prov+ha+Atk
         tot_prov~ha+Atk
         
        
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
          ha=~+educa+indig_censoP+age+indiv+jurid 

         #Regressions
         inc~tot_reg+ha+Atk
         tot_reg~ha+Atk

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
        ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         inc~tot_supl_recreation+ha+Atk
          tot_supl_recreation~ha+Atk
          
          
          
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


####results model 1####

model1_yield<-cbind(uno_a_yield_prov,uno_a_yield_reg,uno_a_yield_cult)
colnames(model1_yield)<-c("raw_prov","raw_reg", "raw_cult")
model1_yield<-format(round(model1_yield,2),nsmall = 2)
model1_yield<-as.data.frame(model1_yield)
write.xlsx(model1_yield, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_yield.xlsx", row.names=TRUE,overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data


model1_total<-cbind(uno_a_total_prov,uno_a_total_reg,uno_a_total_cult)
colnames(model1_total)<-c("raw_prov","raw_reg","raw_cult")
model1_total<-format(round(model1_total,2),nsmall = 2)
model1_total<-as.data.frame(model1_total)
write.xlsx(model1_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model1_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data

#with semTable
list_model1_yield<-list(model1a_yield_prov_fit,model1a_yield_reg_fit,model1a_yield_cult_fit)#original model fits

sem_tabla<-semTable(list_model1_yield, file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/sem_tabla", paramSets="all",type="html")
fit_indicators<-compareLavaan(list_model1_yield,file= "H:/SIG/Procesos SIG/Spatial distribution/Tables/fit_indicators", fitmeas = c("chisq", "df", "pvalue", "rmsea",
                                                                                                                                    "cfi", "tli", "srmr", "aic", "bic"),chidif = FALSE, type="html")


#####Model 2: Regression model with "ES supply inequality" as the outcome variable. As we're using services categories (provisioning, regulating and cultural) we will name the models after the use of yield (a) or total ES values (b) and depending on the Es category. prov= provisioning services; reg=regulating services; cult:cultural service.####




####Model 2a (yield)####
#Step 1: Model specification

model2a_yield_prov<-'#Structural model using raw indicators - ES yield (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
         
          sup_prov=~productivity_water_sup+productivity_timber
          ha=~+educa+indig_censoP+age+indiv+jurid
          
         #Regressions

         sup_prov~inc+ha+Atk
         inc~ha+Atk
         
        
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

         sup_reg~inc+ha+Atk
         inc~ha+Atk
         

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
         productivity_recreation~inc+ha+Atk
         inc~ha+Atk
      
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


####model 2a (total)####

#Step 1: Model specification

model2a_total_prov<-'#Structural model using raw indicators - total ES supply (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_prov=~tot_water_sup+tot_supl_timber
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regression
         tot_prov~inc+ha+Atk
         inc~ha+Atk
        
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
model2a_total_reg<-'#Structural model using raw indicators - total ES supply (provisioning ES)
        
         #Measurement models/defining latent variables,variables that cannot be directly measured
          tot_reg=~tot_water_regulation+tot_supl_cseq+tot_supl_cstor+tot_supl_erosion
          ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         tot_reg~inc+ha+Atk
         inc~ha+Atk

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
         ha=~+educa+indig_censoP+age+indiv+jurid

         #Regressions
         tot_supl_recreation~inc+ha+Atk
         inc~ha+Atk
         
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
colnames(model2_total)<c("provisioning","regulating", "cultural")
model2_total<-format(round(model2_total,2),nsmall = 2)
#model2_total<-as.data.frame(model2_total)
model2_total<-as.data.frame(model2_total)###I added "_not_normalized" to check how results look like without the normalization process as Rachel recommended. This line need to run if we're usinng the non-normalized data
#write.xlsx(model2_total_not_normalized, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders
write.xlsx(model2_total, "H:/SIG/Procesos SIG/Spatial distribution/Tables/model2_total.xlsx", row.names=TRUE, overwrite=TRUE)#change this to your respective folders. This line is when using non-normalized data







































