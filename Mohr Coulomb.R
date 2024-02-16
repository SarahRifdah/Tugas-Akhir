#Setup initial data MOHR COULOMB
strain<-seq(0, 9, by=0.3)
length(strain)
E<-1
c<-0.288
sigma<-2.1
phi<-0.39 #dalam radians, dalam degree = 22.4

#Stress at Failure
peak<-c+sigma*tan(phi)

#Area Linear
stress<-E*strain
length(stress)

#Area Plastis
stress<-ifelse(stress<peak, stress, peak)
print(stress)

#Plotting grafik
plot(strain,stress, type="o", main="Kurva Mohr Coulomb", xlab="Strain (%)", ylab="Stress (kg/cm2)")


#Setup Initial data HARDENING SOIL
library(pracma)
Rf<- 0.7
m<- 0.5
pref<- 1
E50ref<- 0.19
stressd<-seq(0, 2, by=0.052)
strainHs<-seq(0, 9, by=0.3)

#Nilai E50
E50<-E50ref*((sigma+c*cot(phi))/(sigma+c*cot(phi)))^m

#Nilai qf dan qa
qf<-((6*sin(phi))/(3-sin(phi)))*(sigma+c*cot(phi))
qa<-qf/Rf


#Nilai Stress
stressHS<-ifelse(stressd<qf, stressd, qf)
print(stressHS)

#Nilai Strain
strainHS<-ifelse(stressd<qf, qa/(2*E50)*stressHS/(qa-stressd), strainHS)
print(strainHS)

#Plot Grafik 
plot(strainHS, stressHS, type="o", main="Kurva Hardening Soil", xlab="Strain (%)", ylab="Stress(kg/cm2)")

