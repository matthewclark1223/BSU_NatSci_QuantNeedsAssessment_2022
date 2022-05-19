library(tidyverse)
df<-read_csv("./SurveyData/SurveyResults.csv")

#View(df)

df<-df[-c(2,3,4,5),]
vars<-df[1,]
df<-df[-1,]
df<-df%>%mutate(Progress=as.integer(Progress))%>%filter(Progress>=80)%>%select(!1:17)    
       

#Repeats<-(count(df,IPAddress)%>%filter(n>1))$IPAddress
#Repeats<-filter(df,IPAddress %in% Repeats)   #We could remove these, but i looked through them and none of them have duplicate responses

names(df)[1:19]<-c("Position","PosNA","StartYear","Programs",
                   "ProgramsNA","CareerGoal","CareerGoalNA",
                   "QuantImptCareer","MethodsForJob","CompDataMgmt","CompRegress",
                   "CompMachineLrn","CompDataSim","CompProcessMod","CompABM","CompMath","CompSEM",
                   "ImptQuantEstim","ImptQuantEstimNA")

names(df)[20:61]<-c("ImptRnk_ExpDesgn","ImptRnk_Freq","ImptRnk_Bayes","ImptRnk_ModPerform",
                  "ImptRnk_CommResult","ImptRnk_CmplxSys","ImptRnk_CausalInf","ImptRnk_MachineLrn","ImptRnk_NA1",
                  "ImptRnk_NA2","SoftwareUsed","SoftwareUsedNA","TrainingR","TrainingPython",
                  "TrainingSTAN","TrainingBUGS","TrainingNimble","TrainingJAVA","TrainingJulia",
                  "TrainingNetLogo","TrainingArcGIS","TrainingQGIS","TrainingGEE","PrefGainSkill",
                  "BootcampSoftware","BootcampSoftwareNA","CompProblems","CommunResrcsUsed","NOTRUG","NOTRUGNA",
                  "NOTStatsHR","NOTStatsHRNA","NOTSlackChnl","NOTSlackChnlNA","NOTTSARS","NOTTSARSNA",
                  "NOT1_1","NOT1_1NA","NOTRead","NOTReadNA","LngAnswrGood","LngAnswrBad")



#faculty<-df%>%filter(Position=="Faculty member")
#student<-df%>%filter(!Position=="Faculty member")

degrees<-c("MS Raptor Biology","PhD Ecology, Evolution, & Behavior","Masters of Public Administration",
           "PhD Public Administration","PhD Geoscience","MS Hydrologic Science",
           "MA Anthropology","MAA Anthropology","MA Biology","MS Biology",
           "MS Geoscience","PhD Geophysics")

##DEAL WITH THE 'OTHER' BEFORE THIS

for(d in degrees){
  colName<-gsub(" ","",d) #Remove spaces
  colName<-gsub(",","",colName) #remove commas
  df$NewCol<-grepl(d,df$Programs) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}

df$DegreeOTHER<-grepl("Other",df$Programs) #add column for other. We have to do this outside the loop bc of the open parentheses

df<-df[,-c(2,4,5)] #Remove POSNA column since no one selected it, degree column, and degree NA column




CareerGoals<-c("Private non-profit organization","Academia - primarily research","Government",
               "Academia -  primarily teaching","Independent consulting","Non-university teaching",
               "Private for-profit industry")

for(C in CareerGoals){
  colName<-gsub(" ","",C) #Remove spaces
  colName<-gsub("-","",colName) #remove commas
  colName<-paste0("CAREER_",colName)
  df$NewCol<-grepl(C,df$CareerGoal) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}
df$CAREER_OTHER<-grepl("Other",df$CareerGoal) #add column for other. We have to do this outside the loop bc of the open parentheses
#There's no others worth mentioning

df<-df[,-c(3,4)] #Remove career and career NA cols


#Now job methods

JobMethods<-c("Data management","Multivariate analyses (e.g. principle components, cluster analysis, factor analysis)","Regression analyses",
               "Machine learning","Data simulation","Structural equation modeling","Process models (e.g. hydrologic modeling, fire spread)",
               "Individual or agent-based models/computer simulation","Closed-form mathematical models")

for(j in JobMethods){
  colName<-gsub("/","",j) #Remove slash
  colName<-gsub("\\s*\\([^\\)]+\\)","",colName) #remove everything in a parentheses
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("JobMethod_",colName) #Give some info
  df$NewCol<-grepl(j,df$MethodsForJob) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-4] #Remove original column

#Let's get rid of all parentheses


df$CompDataMgmt<-gsub("\\s*\\([^\\)]+\\)","",df$CompDataMgmt)
df$CompRegress<-gsub("\\s*\\([^\\)]+\\)","",df$CompRegress)
df$CompABM<-gsub("\\s*\\([^\\)]+\\)","",df$CompABM)
df$CompMachineLrn<-gsub("\\s*\\([^\\)]+\\)","",df$CompMachineLrn)
df$CompDataSim<-gsub("\\s*\\([^\\)]+\\)","",df$CompDataSim)
df$CompProcessMod<-gsub("\\s*\\([^\\)]+\\)","",df$CompProcessMod)
df$CompMath<-gsub("\\s*\\([^\\)]+\\)","",df$CompMath)
df$CompSEM<-gsub("\\s*\\([^\\)]+\\)","",df$CompSEM)
#We have a mistake in the survey here where we didn't ask about competence in multivariate analyses (factor/pca etc) 

df$ImptQuantEstim<-gsub("\\s*\\([^\\)]+\\)","",df$ImptQuantEstim) #get rid of things in parentheses for important quantitative estimates





ImpEstimates<-c("Abundance or occurrence of species or events","Efficacy of interventions/treatments","Latent categorical or clustering variables",
              "Spatial autocorrelation in the predictors or response","Vital rates of a population","Epidemiological quantities",
              "Multivariate indices of community composition","Closed-form mathematical models","Other")

for(i in ImpEstimates){
  colName<-gsub("/","",i) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("ImpEstimates_",colName) #Give some info
  df$NewCol<-grepl(i,df$ImptQuantEstim) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-(12:13)] #Remove original column


#Software
df$SoftwareUsed<-gsub("\\s*\\([^\\)]+\\)","",df$SoftwareUsed) 
Softwares<-c("R","Python","ArcGIS","QGIS","STAN","Google Earth Engine","Other","NetLogo","JAVA","Julia","BUGS/JAGS","NIMBLE")

for(s in Softwares){
  colName<-gsub("/","",s) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("SOFTWARE_",colName) #Give some info
  df$NewCol<-grepl(s,df$SoftwareUsed) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-(22:23)] #Remove original column



#PrefGainSkill
df$PrefGainSkill<-gsub("\\s*\\([^\\)]+\\)","",df$PrefGainSkill) 
PrefGain<-c("Integration of coding instruction into class assignments",
             "Coding-focused course offerings","Weeklong ‘bootcamps’ provided by faculty",
             "Self-motivated learning with curated resources","Student-led workshops")

for(p in PrefGain){
  colName<-gsub("/","",p) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-gsub("‘bootcamps’","bootcamps",colName) #remove quote
  colName<-paste0("PrefGainSkill_",colName) #Give some info
  df$NewCol<-grepl(p,df$PrefGainSkill) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-(33)] #Remove original column



##bootcampsoftwares

df$BootcampSoftware<-gsub("\\s*\\([^\\)]+\\)","",df$BootcampSoftware) 
Bootcamp<-c("R","Python","ArcGIS","QGIS","STAN","Google Earth Engine","Other","NetLogo","JAVA","Julia","BUGS/JAGS","NIMBLE")

for(b in Bootcamp){
  colName<-gsub("/","",b) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("BOOTCAMP_",colName) #Give some info
  df$NewCol<-grepl(b,df$BootcampSoftware) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-(33:34)] #Remove original column


#CompProblems

df$CompProblems<-gsub("\\s*\\([^\\)]+\\)","",df$CompProblems) 
Problems<-c("Working with spatial data and creating maps",
            "Managing large or complex data","Fitting complex regression analyses",
            "Taking advantage of high-performance computing resources",
            "Effective data visualization and/or presentation of results")

for(p in Problems){
  colName<-gsub("/","",p) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("PROBLEMS",colName) #Give some info
  df$NewCol<-grepl(p,df$CompProblems) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


df<-df[,-33] #Remove original column


#CommunResc

df$CommunResrcsUsed<-gsub("\\s*\\([^\\)]+\\)","",df$CommunResrcsUsed) 
Resources<-c("R Users' Group","EcoStatsBSU Slack channel",
            "Stats Hour","1:1 R help sessions provided by the Biology department",
            "Time Series & Remote Sensing group",
            "EcoStats Summer reading group")

for(r in Resources){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("RESOURCES_",colName) #Give some info
  df$NewCol<-grepl(r,df$CommunResrcsUsed) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}

df<-df[,-33] #Remove original column


