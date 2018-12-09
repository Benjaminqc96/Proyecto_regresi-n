require(datasets)
require(nortest)
require(car)
require(lmtest)
data("LifeCycleSavings")
########################################model for the regresion###################################
mod1<-lm(pop75~1,data = LifeCycleSavings)
modc<-lm(pop75~.,data = LifeCycleSavings)
########################################generate the step#########################################
#modelo base           basico     completo        hacia en frente, del basico al completo
step(mod1,scope = list(lower=mod1,upper=modc),direction = "forward")
#################################generate the models one by one###################################
mod2<-lm(LifeCycleSavings$pop75~LifeCycleSavings$sr)
mod3<-lm(LifeCycleSavings$pop75~LifeCycleSavings$sr+LifeCycleSavings$pop15)
mod4<-lm(LifeCycleSavings$pop75~LifeCycleSavings$sr+LifeCycleSavings$pop15+
           LifeCycleSavings$dpi)
##########################check up the significance of the models#################################
summary(mod1) #el intercepto tiene significancia de ***
summary(mod2) #el intercepto tiene significancia de ** y la beta de *, coeficiente de determinacion
# de 10.02%
summary(mod3)#intercepto con significancia de ***, beta de sr con significancia . y beta pop15 con 
#significancia de ***, coeficiente de determinacion = 83.73%
summary(mod4)#intercepto con significancia ***, beta sr sin significancia, beta pop15 significancia
#*** y beta dpi con significancia *, coeficiente =85.49%
summary(modc)#intercepto con significancia ***, beta pop15 con significancia ***, beta dpi sig=*, 
#las demas no son significativas.
#####################################experimentation##############################################
modexp<-lm(LifeCycleSavings$pop75~LifeCycleSavings$pop15+LifeCycleSavings$dpi)
summary(modexp)
#descripcion del modelo
#intercepto sig=***, beta pop15 sig=***, beta dpi sig=**, modelo significativo al 99%
#p value de la f<.01, por lo tanto tiene significancia el modelo.
###################################shapiro test####################################################
shapiro.test(modexp$residuals)$p.value # 0.3153299>.01 por lo tanto cumple normalidad
###################################breusch Pagan test##############################################
bptest(modexp)$p.value #0.1532843 bp=3.7509 tiene homocedasticidad
####################################      V I F      ##############################################
vif(modexp) #2.335469                      2.335469 no hay inflacion de varianzas
################################### durbin watson test#############################################
dwtest(modexp)$p.value #0.7353866 DW=2.1961 #los errores son independientes
###################################parte del proyecto##############################################
###prueba de modelo de regresion completa para mod2,mod3 y mod4
##############prueba de normalidad para residuos con kolmogorov smirnov modificada#################
#mod3
sf.test(mod3$residuals) #los datos se distribuyen normal p-value = 0.733
#mod4
sf.test(mod4$residuals) #los datos se distribuyen normal p-value = 0.1347
#modc
sf.test(modc$residuals) #los datos se distribuyen normal p-value = 0.2051
######################prueba de breush pagan para homocedasticidad#################################
#mod3
bptest(mod3)#p-value = 0.1962 tiene homocedasticidad
#mod4
bptest(mod4)#p-value = 0.2714 tiene homocestacidad
#modc
bptest(modc)#p-value = 0.3719 tiene homocedasticidad
####################################prueba de multicolinealidad####################################
#mod3
vif(mod3)#tiene multicolinealidad 1.261853
#mod4
vif(mod4)#tiene multicolinealidad
#mod5
vif(modc)#tiene multicolinealidad
#############################prueba de independencia de caracteres################################
#mod3
dwtest(mod3)#DW = 2.0579, p-value = 0.5665 son independientes
#mod4
dwtest(mod4)#DW = 2.1254, p-value = 0.6497 son independientes
#modc
dwtest(modc)#DW = 2.0588, p-value = 0.562 son independientes
######################prueba para identificar si tienen puntos atipicos###########################
#mod3
d2<-cooks.distance(mod3)
plot(d2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(d2, na.rm=T), col="red")  # add cutoff line
text(x=1:length(d2)+1, y=d2, labels=ifelse(d2>4*mean(d2, na.rm=T),names(d2),""), 
     col="red")  # add labels
#mod4
d3<-cooks.distance(mod4)
plot(d3, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(d3, na.rm=T), col="red")  # add cutoff line
text(x=1:length(d3)+1, y=d3, labels=ifelse(d3>4*mean(d3, na.rm=T),names(d3),""), 
     col="red")  # add labels
#modc
d4<-cooks.distance(modc)
plot(d4, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(d4, na.rm=T), col="red")  # add cutoff line
text(x=1:length(d4)+1, y=d4, labels=ifelse(d4>4*mean(d4, na.rm=T),names(d4),""), 
     col="red")  # add labels
#########################################extraccion de outliers####################################
#mod3
outlierTest(mod3) #21-Ireland
#mod4
outlierTest(mod4) #21-Ireland
#modc
outlierTest(modc) #21-Ireland
base2<-LifeCycleSavings[-21,]
###################################generacion de los nuevos modelos################################
moda<-lm(base2$pop75~base2$sr+base2$pop15)
modb<-lm(base2$pop75~base2$sr+base2$pop15+base2$dpi)
modcc<-lm(pop75~.,data = base2)
####################################resumen de los modelos nuevos##################################
summary(moda)
summary(modb)
summary(modcc)
