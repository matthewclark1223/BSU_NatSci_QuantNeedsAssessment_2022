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


#devtools::install_github("jbgb13/peRReo") #Install ragatone pallette

programs<-names(df)[47:59]



stupal<-c("PhDEcologyEvolution&Behavior"="#006d2c",
       "MSBiology"="#41ae76",
       "MSRaptorBiology"="#00441b",
       "PhDGeoscience"="#bf812d",
       "MSHydrologicScience"="#3690c0",
       "MAAnthropology"="#6a51a3",
       "DegreeOTHER"="#bdbdbd",
       "PhDGeophysics"="#8c510a",
       "MSGeoscience"="#dfc27d"
)

mytheme<-  theme(axis.ticks = element_blank(),
                 axis.text.y = element_blank(),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size=25,color="black",vjust = 100),
                 axis.text.x = element_text(size=14,color="black"),
                 panel.grid = element_blank(),
                 panel.border = element_blank())

degreeplotStu<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(programs)%>%
  gather(key="Program",value="val")%>%
  filter(val==TRUE)%>%group_by(Program)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(Question=rep("program",nrow(.)))%>%mutate(Program=fct_reorder(Program,perc,mean))%>%


ggplot(., aes(x=Question,y=perc,fill=Program)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=(stupal),labels=c("PhD Ecology,\nEvolution, &Behavior","MS Biology","MS Raptor\nBiology","PhD Geoscience",
                                          "MS Hydrologic\nScience","MA Anthropology","Other","PhD Geophysics","MS Geoscience"))+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("Proportion of responses")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")


facpal<-c("PhDEcologyEvolution&Behavior"="#006d2c",
       "MSBiology"="#41ae76",
       "MSRaptorBiology"="#00441b",
       "MAAnthropology"="#6a51a3",
       "DegreeOTHER"="#bdbdbd",
       "PhDGeoscience"="#bf812d",
       "MSHydrologicScience"="#3690c0",
       "MSGeoscience"="#dfc27d",
       "MAAAnthropology"="#9e9ac8",
       "PhDPublicAdministration"="#cb181d",
       "PhDGeophysics"="#8c510a",
       "MastersofPublicAdministration"="#ef6548",
       "MABiology"="#99d8c9"
)

degreeplotfac<-df%>%filter(Position%in%c("Faculty member"))%>%select(programs)%>%
  gather(key="Program",value="val")%>%
  filter(val==TRUE)%>%group_by(Program)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(Question=rep("program",nrow(.)))%>%mutate(Program=fct_reorder(Program,perc,mean))%>%
  
  
  ggplot(., aes(x=Question,y=perc,fill=Program)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=(facpal),labels=c("PhD Ecology,\nEvolution, &Behavior","MS Biology","MS Raptor\nBiology","MA Anthropology",
                                             "Other","PhD Geoscience","MS Hydrologic\nScience","MS Geoscience","MAA Anthropology","PhD Public\nAdministration","PhD Geophysics",
                                             "Masters of Public\nAdministration","MA Biology"))+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("Proportion of responses")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")



x<-df%>%select(Position)%>%
  group_by(Position)%>%count()
FacVsStud<-plotly::plot_ly(x, marker = list(colors = c("#8c510a","#5ab4ac","#01665e") ), sort = FALSE) %>% 
  plotly::add_pie(labels=x$Position,values=x$n,hole=0.5)



##studetn career goals
GOALS<-names(df)[60:67]
library(peRReo)
pal<-latin_palette('badbunny1',length(GOALS),type = "discrete" )

StuGoals<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(GOALS)%>%
  gather(key="GOAL",value="val")%>%
  filter(val==TRUE)%>%group_by(GOAL)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("position",nrow(.)))%>%mutate(GOAL=fct_reorder(GOAL,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=GOAL)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Student career goals",
                    labels = c("Non-university\nteaching","Independent consulting","Academia primarily\nteaching",
                               "For-profit industry","Academia primarily\nresearch","Non-profit","Government"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")


##
GOALS<-names(df)[60:67]
library(peRReo)
pal<-latin_palette('badbunny1',length(GOALS),type = "discrete" )

StuGoals<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(GOALS)%>%
  gather(key="GOAL",value="val")%>%
  filter(val==TRUE)%>%group_by(GOAL)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(Question=rep("CareerGoal",nrow(.)))%>%mutate(GOAL=fct_reorder(GOAL,perc,mean))%>%
  
  
  ggplot(., aes(x=Question,y=perc,fill=GOAL)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Student career goals",
                    labels = c("Non-university\nteaching","Independent consulting","Academia primarily\nteaching",
                               "For-profit industry","Academia primarily\nresearch","Non-profit","Government"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")




#quant importance
library(ggalluvial)
pal<-latin_palette('badbunny1',length(GOALS),type = "discrete" )




QUANTIMPforCAREER2<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(c(GOALS,QuantImptCareer))%>%
  gather(key="GOAL",value="val",1:8)%>%
  filter(val==TRUE)%>%group_by(GOAL,QuantImptCareer)%>%count()%>%ungroup()%>%
  mutate(QuantImptCareer=factor( QuantImptCareer,c("Extremely important","Very important","Moderately important")))%>%
  mutate(GOAL=recode(GOAL,CAREER_Privatenonprofitorganization="Non-proft",
         CAREER_Academiaprimarilyresearch="Academia\nprimarily research",
         CAREER_Government="Government",
         CAREER_Academiaprimarilyteaching="Academia\nprimarily teaching",
         CAREER_Independentconsulting="Independent consulting",
         CAREER_Nonuniversityteaching="Non-university teaching",
         CAREER_Privateforprofitindustry= "For-profit industry"))%>%
  mutate(GOAL=factor( GOAL,c("Government","Non-proft","Academia\nprimarily research",
                             "For-profit industry","Academia\nprimarily teaching","Independent consulting","Non-university teaching")))%>%
  mutate(GOAL=fct_rev(GOAL))%>%
  ggplot(.,aes(y=n,axis1=GOAL,axis2=QuantImptCareer))+
  geom_alluvium(aes(fill = GOAL), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Career goal", "Importance of quant\nskills to achieve that\ncareer"), expand = c(.05, .05)) +
  scale_fill_manual(values = pal,labels = c("Academia primarily\nresearch","Academia primarily\nteaching",
                                            "Government","Independent consulting","Non-university\nteaching",
                                            "For-profit industry","Non-profit"))+theme_void()+
  theme(legend.position="none")+theme(axis.text.x=element_text(size=14,color="black"))




##Same data as above but modified per Matt W's suggestions
QUANTIMPforCAREER<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(c(GOALS,QuantImptCareer))%>%
  gather(key="GOAL",value="val",1:8)%>%
  filter(val==TRUE)%>%group_by(GOAL,QuantImptCareer)%>%count()%>%ungroup()%>%
  mutate(QuantImptCareer=factor( QuantImptCareer,c("Extremely important","Very important","Moderately important")))%>%
  mutate(GOAL=recode(GOAL,CAREER_Privatenonprofitorganization="Non-proft",
                     CAREER_Academiaprimarilyresearch="Academia\nprimarily research",
                     CAREER_Government="Government",
                     CAREER_Academiaprimarilyteaching="Academia\nprimarily teaching",
                     CAREER_Independentconsulting="Independent consulting",
                     CAREER_Nonuniversityteaching="Non-university teaching",
                     CAREER_Privateforprofitindustry= "For-profit industry"))%>%
  mutate(GOAL=factor( GOAL,c("Government","Non-proft","Academia\nprimarily research",
                             "For-profit industry","Academia\nprimarily teaching","Independent consulting","Non-university teaching")))%>%
  mutate(GOAL=fct_rev(GOAL))%>%group_by(GOAL)%>%mutate(Perc=n/sum(n))%>%
  ggplot(.,aes(x=QuantImptCareer,y=Perc))+geom_bar(stat="identity",fill="#2171b5",color="darkgrey",size=0.75)+
  facet_wrap(~GOAL)+theme_bw()+ylab("Percent of students with\nthis career goal")+
  xlab("How important they believe quant skills\nwill be for achieving that career")+
  scale_y_continuous(breaks=c(0,.25,.50,.75),labels=scales::percent)+
  theme(axis.title = element_text(size=15,color="black"),
        axis.text.x = element_text(size=7,color="black"),
        axis.text.y = element_text(size=7,color="black"))



##

##Job methods figure
SKILLS<-names(df)[68:76]
COMPS<-names(df)[4:11]

pal<-latin_palette('badgyal',5,type = "discrete" )

mytheme2<-  theme(axis.title = element_text(size=20,color="black"),
                 axis.text.x = element_text(size=8,color="black"),
                 axis.text.y = element_text(size=8,color="black"))

SKILL4Career<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%select(c(SKILLS,COMPS))%>%#group_by(CompProcessMod )%>%
  pivot_longer(names_to ="SKILL",values_to ="val",cols = 1:9)%>%filter(val==TRUE)%>%filter(SKILL=="JobMethod_Datamanagement"
)%>%select(-(9:10))%>%gather(key="SKILL",value="val",1:8)%>%
  group_by(SKILL,val)%>%count()%>%ungroup()%>%filter(!is.na(val))%>%group_by(SKILL)%>%mutate(perc = n/sum(n))%>%
  mutate(SKILL=fct_reorder(SKILL,perc,mean))%>%mutate(val=factor(val,c("No experience","Some entry level experience",
                                                                       "Moderate experience but more training required",
                                                                       "Competent at a professional level","Expert")))%>%
  mutate(SKILL=recode(SKILL,CompABM="ABM",CompDataMgmt="Data\nmanagement",
                      CompDataSim="Data\nsimulation",CompMachineLrn="Machine\nlearning",
                      CompMath="Close-form\nmath",CompProcessMod="Process\nmodeling",
                      CompRegress="Linear\nregression",CompSEM="Structural equation\nmodeling"))%>%
  
  
  ggplot(., aes(x=SKILL,y=n,fill=val)) + geom_bar(stat="identity",position='stack',width = 0.6)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_stack(0.5) ,show.legend = FALSE,color="white",fontface="bold")+
  theme_classic()+ylab("Number of students who indicated this\nas an important skill for getting a job")+xlab("\nSkill perceived as required for career")+
  scale_fill_manual(values=pal,name="Skill level")+
  scale_radius(range = c(2.5,5))+mytheme2#+ggtitle("Percent of responses")



##Coding language skills for career
#


CODES<-names(df)[c(86:91,93:97)]
COMPS<-names(df)[22:32]

pal<-latin_palette('badgyal',4,type = "discrete" )


CODE4Career<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%mutate(ID=1:nrow(.))%>%
  select(c(CODES,COMPS,ID))%>%#group_by(CompProcessMod )%>%
  pivot_longer(names_to ="CODE",values_to ="val",cols = c(1:11))%>%filter(val==TRUE)%>%select(-(13:14))%>% distinct()%>%gather(key="CODE",value="val",1:8)%>%
  group_by(CODE,val)%>%count()%>%ungroup()%>%filter(!is.na(val))%>%group_by(CODE)%>%mutate(perc = n/sum(n))%>%
  mutate(CODE=fct_reorder(CODE,perc,mean))%>%mutate(val=factor(val,c("No experience","Some entry level experience",
                                                                     "Moderate experience but more training required",
                                                                     "Competent at a professional level","Expert")))%>%
  mutate(CODE=recode(CODE, TrainingBUGS="BUGS",TrainingJulia   ="JULIA",
                     TrainingNetLogo ="NetLogo",TrainingPython  ="Python",
                     TrainingR    ="R",TrainingSTAN    ="STAN"))%>%
  
  
  ggplot(., aes(x=CODE,y=n,fill=val)) + geom_bar(stat="identity",position='stack',width = 0.6)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_stack(0.5) ,show.legend = FALSE,color="white",fontface="bold")+
  theme_classic()+ylab("Number of students who indicated this\nas an important software for getting a job")+xlab("\nSoftware perceived as required for career")+
  scale_fill_manual(values=pal,name="Current training")+
  scale_radius(range = c(2.5,5))+mytheme2#+ggtitle("Percent of responses")

##Faculty specific page####################

##Quant skill importance

pal<-latin_palette('badbunny1',length(GOALS),type = "discrete" )




QUANTIMPforCAREER_FAC2<-df%>%filter(Position%in%c("Faculty member"))%>%select(c(GOALS,QuantImptCareer))%>%
  gather(key="GOAL",value="val",1:8)%>%
  filter(val==TRUE)%>%group_by(GOAL,QuantImptCareer)%>%count()%>%ungroup()%>%
  mutate(QuantImptCareer=factor( QuantImptCareer,c("Extremely important","Very important","Moderately important")))%>%
  mutate(GOAL=recode(GOAL,CAREER_Privatenonprofitorganization="Non-proft",
                     CAREER_Academiaprimarilyresearch="Academia\nprimarily research",
                     CAREER_Government="Government",
                     CAREER_Academiaprimarilyteaching="Academia\nprimarily teaching",
                     CAREER_Independentconsulting="Independent consulting",
                     CAREER_Nonuniversityteaching="Non-university teaching",
                     CAREER_Privateforprofitindustry= "For-profit industry"))%>%
  mutate(GOAL=factor( GOAL,c("Government","Non-proft","Academia\nprimarily research",
                             "For-profit industry","Academia\nprimarily teaching","Independent consulting","Non-university teaching")))%>%
  mutate(GOAL=fct_rev(GOAL))%>%na.omit()%>%
  ggplot(.,aes(y=n,axis1=GOAL,axis2=QuantImptCareer))+
  geom_alluvium(aes(fill = GOAL), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Career goal", "Importance of quant\nskills to achieve that\ncareer"), expand = c(.05, .05)) +
  scale_fill_manual(values = pal,labels = c("Academia primarily\nresearch","Academia primarily\nteaching",
                                            "Government","Independent consulting","Non-university\nteaching",
                                            "For-profit industry","Non-profit"))+theme_void()+
  theme(legend.position="none")+theme(axis.text.x=element_text(size=14,color="black"))



##alternative option to the alluvial diagram above
QUANTIMPforCAREER_FAC<-df%>%filter(Position%in%c("Faculty member"))%>%select(c(GOALS,QuantImptCareer))%>%
  gather(key="GOAL",value="val",1:8)%>%
  filter(val==TRUE)%>%group_by(GOAL,QuantImptCareer)%>%count()%>%ungroup()%>%
  mutate(QuantImptCareer=factor( QuantImptCareer,c("Extremely important","Very important","Moderately important")))%>%
  mutate(GOAL=recode(GOAL,CAREER_Privatenonprofitorganization="Non-proft",
                     CAREER_Academiaprimarilyresearch="Academia\nprimarily research",
                     CAREER_Government="Government",
                     CAREER_Academiaprimarilyteaching="Academia\nprimarily teaching",
                     CAREER_Independentconsulting="Independent consulting",
                     CAREER_Nonuniversityteaching="Non-university teaching",
                     CAREER_Privateforprofitindustry= "For-profit industry"))%>%
  mutate(GOAL=factor( GOAL,c("Government","Non-proft","Academia\nprimarily research",
                             "For-profit industry","Academia\nprimarily teaching","Independent consulting","Non-university teaching")))%>%
  mutate(GOAL=fct_rev(GOAL))%>%group_by(GOAL)%>%mutate(Perc=n/sum(n))%>%na.omit()%>%
  ggplot(.,aes(x=QuantImptCareer,y=Perc))+geom_bar(stat="identity",fill="#2171b5",color="darkgrey",size=0.75)+
  facet_wrap(~GOAL)+theme_bw()+ylab("Percent of faculty who's studets\nhave this career goal")+
  xlab("How important they believe quant skills\nwill be for achieving that career")+
  scale_y_continuous(breaks=c(0,.25,.50,.75),labels=scales::percent)+
  theme(axis.title = element_text(size=15,color="black"),
        axis.text.x = element_text(size=7,color="black"),
        axis.text.y = element_text(size=7,color="black"))





#Skills needed vs current level
SKILLS<-names(df)[68:76]
COMPS<-names(df)[4:11]

pal<-latin_palette('badgyal',5,type = "discrete" )

mytheme2<-  theme(axis.title = element_text(size=15,color="black"),
                  axis.text.x = element_text(size=8,color="black"),
                  axis.text.y = element_text(size=8,color="black"))

SKILL4Career_FAC<-df%>%filter(Position%in%c("Faculty member"))%>%select(c(SKILLS,COMPS))%>%#group_by(CompProcessMod )%>%
  pivot_longer(names_to ="SKILL",values_to ="val",cols = 1:9)%>%filter(val==TRUE)%>%
  filter(SKILL=="JobMethod_Datamanagement")%>%select(-(9:10))%>%gather(key="SKILL",value="val",1:8)%>%
  group_by(SKILL,val)%>%count()%>%ungroup()%>%filter(!is.na(val))%>%group_by(SKILL)%>%mutate(perc = n/sum(n))%>%
  mutate(SKILL=fct_reorder(SKILL,perc,mean))%>%mutate(val=factor(val,c("No experience","Some entry level experience",
                                                                       "Moderate experience but more training required",
                                                                       "Competent at a professional level","Expert")))%>%
  mutate(SKILL=recode(SKILL,CompABM="ABM",CompDataMgmt="Data\nmanagement",
                      CompDataSim="Data\nsimulation",CompMachineLrn="Machine\nlearning",
                      CompMath="Close-form\nmath",CompProcessMod="Process\nmodeling",
                      CompRegress="Linear\nregression",CompSEM="Structural equation\nmodeling"))%>%
  
  
  ggplot(., aes(x=SKILL,y=n,fill=val)) + geom_bar(stat="identity",position='stack',width = 0.6)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_stack(0.5) ,show.legend = FALSE,color="white",fontface="bold")+
  theme_classic()+ylab("Number of faculty who indicated this\nas an important skill for getting a job")+xlab("\nSkill perceived as required for career")+
  scale_fill_manual(values=pal,name="Skill level")+
  scale_radius(range = c(2.5,5))+mytheme2#+ggtitle("Percent of responses")




#programming skill vs current level

CODES<-names(df)[c(86:91,93:97)]
COMPS<-names(df)[22:32]

pal<-latin_palette('badgyal',4,type = "discrete" )


CODE4Career_FAC<-df%>%filter(Position%in%c("Faculty member"))%>%
  mutate(ID=1:nrow(.))%>%
  select(c(CODES,COMPS,ID))%>%#group_by(CompProcessMod )%>%
  pivot_longer(names_to ="CODE",values_to ="val",cols = c(1:11))%>%filter(val==TRUE)%>%select(-(13:14))%>% distinct()%>%gather(key="CODE",value="val",1:8)%>%
  group_by(CODE,val)%>%count()%>%ungroup()%>%filter(!is.na(val))%>%group_by(CODE)%>%mutate(perc = n/sum(n))%>%
  mutate(CODE=fct_reorder(CODE,perc,mean))%>%mutate(val=factor(val,c("No experience","Some entry level experience",
                                                                     "Moderate experience but more training required",
                                                                     "Competent at a professional level","Expert")))%>%
  mutate(CODE=recode(CODE, TrainingBUGS="BUGS",TrainingJulia   ="JULIA",
                     TrainingNetLogo ="NetLogo",TrainingPython  ="Python",
                     TrainingR    ="R",TrainingSTAN    ="STAN"))%>%
  
  
  ggplot(., aes(x=CODE,y=n,fill=val)) + geom_bar(stat="identity",position='stack',width = 0.6)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_stack(0.5) ,show.legend = FALSE,color="white",fontface="bold")+
  theme_classic()+ylab("Number of faculty who indicated this\nas an important software for getting a job")+xlab("\nSoftware perceived as required for career")+
  scale_fill_manual(values=pal,name="Current training")+
  scale_radius(range = c(2.5,5))+mytheme2#+ggtitle("Percent of responses")



#####concepts and quantities

#Most important quantities to be able to estimate

QUANTS<-names(df)[77:84]

pal<-latin_palette('karolg',7,type = "discrete" )

IMP_QUANTS<-df%>%select(QUANTS)%>%
  gather(key="QUANT",value="val")%>%
  filter(val==TRUE)%>%group_by(QUANT)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Imp_quants",nrow(.)))%>%mutate(QUANT=fct_reorder(QUANT,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=QUANT)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Most important quantities\nfor students to estimate",
                    labels = c("Epidemiological","Pop vital rates","Latent clustering",
                               "Community composition","intervention efficacy","Autocorrelationt","Abundance/occurance"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")


#Computational Problems

PROBS<-names(df)[115:119]

pal<-latin_palette('nicky',5,type = "discrete" )

COMP_PROBZ<-df%>%select(PROBS)%>%
  gather(key="PROB",value="val")%>%
  filter(val==TRUE)%>%group_by(PROB)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Comp_probs",nrow(.)))%>%mutate(PROB=fct_reorder(PROB,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=PROB)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Common issues you\nrun into",
                    labels = c("Using HPC effectively","Spatial data/maps","Data visualization",
                               "Fitting complex \nregression analyses","Managing large\ndata"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")


##Perf to gain skills

#Computational Problems

GAINS<-names(df)[98:102]

pal<-latin_palette('shakira',5,type = "discrete" )

PREF_GAIN<-df%>%select(GAINS)%>%
  gather(key="GAIN",value="val")%>%
  filter(val==TRUE)%>%group_by(GAIN)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Perf_gain",nrow(.)))%>%mutate(GAIN=fct_reorder(GAIN,perc,mean))%>%
  
  
  ggplot(., aes(x=question,y=perc,fill=GAIN)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="How do you prefer\nto gain new comp/quant skills",
                    labels = c("Student led workshops","Curated self-guided\nresources","Weeklong bootcamps",
                               "Coding in class\nassignments","Coding specific courses"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(3,15))+mytheme#+ggtitle("Percent of responses")

##Languages for a bootcamp

LANGS<-names(df)[103:114]

pal<-latin_palette('ozuna')

BOOT_LANGS<-df%>%select(LANGS)%>%
  gather(key="LANG",value="val")%>%
  filter(val==TRUE)%>%group_by(LANG)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Boot_langs",nrow(.)))%>%mutate(LANG=fct_reorder(LANG,perc,mean))%>%
  mutate(LANG=recode(LANG, BOOTCAMP_BUGSJAGS="BUGS/JAGS",BOOTCAMP_Julia   ="JULIA",
                     BOOTCAMP_NetLogo ="NetLogo",BOOTCAMP_Python  ="Python",
                     BOOTCAMP_R ="R",BOOTCAMP_STAN  ="STAN",
                     BOOTCAMP_GoogleEarthEngine="Google Earth\nEngine",BOOTCAMP_Other="Other",
                     BOOTCAMP_QGIS="QGIS",BOOTCAMP_ArcGIS="ArcGIS",BOOTCAMP_JAVA="JAVA"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=LANG)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_viridis_d(option="plasma", name ="What software would\nyou attend a weeklong\nbootcamp to learn",
                    labels = c("Other","NetLogo","BUGS/JAGS",
                               "JAVA","Julia","STAN","QGIS","ArcGIS","Google Earth\nEngine","Python","R"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(2,15))+mytheme#+ggtitle("Percent of responses")





###Community


##Resources that people use 

SOURCES<-names(df)[120:125]

pal<-latin_palette('ozuna',6,type="discrete")

RESOURCES_USED<-df%>%select(SOURCES)%>%
  gather(key="SOURCE",value="val")%>%
  filter(val==TRUE)%>%group_by(SOURCE)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("community_resources",nrow(.)))%>%mutate(SOURCE=fct_reorder(SOURCE,perc,mean))%>%
  
  ggplot(., aes(x=question,y=perc,fill=SOURCE)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Which community-style resource\ndo you regularly use",
                    labels = c("Summer reading group","TSARS","1:1 R help\nsessions",
                               "Stats hour","R Users' Group","EcoStats Slack\nchannel"),
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")


###Why not various resources

#Need to make some columns for them first

#1_1 frst
df$NOT1_1<-gsub("\\s*\\([^\\)]+\\)","",df$NOT1_1) 
Not_1_1<-c("I do not know what this resource is",
           "I forget",
           "It does not directly relate to the types of computational/statistical problems I typically encounter",
           "It is at a difficult time for me to attend",
           "Some other reason",
           "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_1_1){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_1_1_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOT1_1) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}

#ecostats slack
df$NOTSlackChnl<-gsub("\\s*\\([^\\)]+\\)","",df$NOTSlackChnl) 
Not_slack<-c("I do not know what this resource is",
             "I forget",
             "It does not directly relate to the types of computational/statistical problems I typically encounter",
             "It is at a difficult time for me to attend",
             "Some other reason",
             "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_slack){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_slack_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOTSlackChnl) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


#Not RUG
df$NOTRUG<-gsub("\\s*\\([^\\)]+\\)","",df$NOTRUG) 
Not_rug<-c("I do not know what this resource is",
           "I forget",
           "It does not directly relate to the types of computational/statistical problems I typically encounter",
           "It is at a difficult time for me to attend",
           "Some other reason",
           "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_rug){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_rug_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOTRUG) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


# not stats hour
df$NOTStatsHR<-gsub("\\s*\\([^\\)]+\\)","",df$NOTStatsHR) 
Not_statsHR<-c("I do not know what this resource is",
               "I forget",
               "It does not directly relate to the types of computational/statistical problems I typically encounter",
               "It is at a difficult time for me to attend",
               "Some other reason",
               "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_statsHR){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_statsHR_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOTStatsHR) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}

#Not tsars
df$NOTTSARS<-gsub("\\s*\\([^\\)]+\\)","",df$NOTTSARS) 
Not_tsars<-c("I do not know what this resource is",
             "I forget",
             "It does not directly relate to the types of computational/statistical problems I typically encounter",
             "It is at a difficult time for me to attend",
             "Some other reason",
             "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_tsars){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_tsars_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOTTSARS) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}


#Not summer

df$NOTRead<-gsub("\\s*\\([^\\)]+\\)","",df$NOTRead) 
Not_read<-c("I do not know what this resource is",
             "I forget",
             "It does not directly relate to the types of computational/statistical problems I typically encounter",
             "It is at a difficult time for me to attend",
             "Some other reason",
             "I personally do not find this type of resource helpful for my learning","I am not comfortable attending this group")

for(r in Not_read){
  colName<-gsub("/","",r) #Remove slash
  colName<-gsub(" ","",colName) #Remove spaces
  colName<-gsub("-","",colName) #remove dashes
  colName<-paste0("REASON_read_",colName) #Give some info
  df$NewCol<-grepl(r,df$NOTRead) #Which responses had the full degree name in it
  names(df)[ncol(df)]<-colName #make a new column with the clean degree name. Filler ==d with the T/F from above
}



###Nowplot them
REASONS_1_1<-names(df)[126:132]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_1_1<-df%>%select(REASONS_1_1)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_1_1_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                REASON_1_1_Iforget="I forget",
                REASON_1_1_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                REASON_1_1_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                REASON_1_1_Itisatadifficulttimeformetoattend="Difficult time",
                REASON_1_1_Someotherreason ="Other"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you use\nthe 1:1 R help\nsessions",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")


##RUG
REASONS_rug<-names(df)[140:146]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_rug<-df%>%select(REASONS_rug)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_rug_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                       REASON_rug_Iforget="I forget",
                       REASON_rug_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                       REASON_rug_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                       REASON_rug_Itisatadifficulttimeformetoattend="Difficult time",
                       REASON_rug_Someotherreason ="Other",
                       REASON_rug_Iamnotcomfortableattendingthisgroup = "Not comfortable"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you use\nthe R Users' Group",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")


##TSARS
REASONS_tsars<-names(df)[154:160]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_tsars<-df%>%select(REASONS_tsars)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_tsars_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                       REASON_tsars_Iforget="I forget",
                       REASON_tsars_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                       REASON_tsars_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                       REASON_tsars_Itisatadifficulttimeformetoattend="Difficult time",
                       REASON_tsars_Someotherreason ="Other",
                       REASON_tsars_Iamnotcomfortableattendingthisgroup = "Not comfortable"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you attend\nthe TSARS group",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")

#reason read
REASONS_read<-names(df)[161:167]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_read<-df%>%select(REASONS_read)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_read_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                       REASON_read_Iforget="I forget",
                       REASON_read_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                       REASON_read_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                       REASON_read_Itisatadifficulttimeformetoattend="Difficult time",
                       REASON_read_Someotherreason ="Other",
                       REASON_read_Iamnotcomfortableattendingthisgroup = "Not comfortable"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you attend\nthe summer reading group",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")



#stats hour
REASONS_sthr<-names(df)[147:153]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_sthr<-df%>%select(REASONS_sthr)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_statsHR_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                       REASON_statsHR_Iforget="I forget",
                       REASON_statsHR_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                       REASON_statsHR_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                       REASON_statsHR_Itisatadifficulttimeformetoattend="Difficult time",
                       REASON_statsHR_Someotherreason ="Other",
                       REASON_statsHR_Iamnotcomfortableattendingthisgroup = "Not comfortable"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you attend\nthe stats hour",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")


#slack channel
REASONS_slack<-names(df)[133:139]

pal<-latin_palette('calle13',7,type="discrete")

REASONplt_slack<-df%>%select(REASONS_slack)%>%
  gather(key="REASON",value="val")%>%
  filter(val==TRUE)%>%group_by(REASON)%>%count()%>%ungroup()%>%
  mutate(perc = n/sum(n))%>%mutate(question=rep("Reason_not_used",nrow(.)))%>%mutate(REASON=fct_reorder(REASON,perc,mean))%>%
  mutate(REASON=recode(REASON,REASON_slack_Idonotknowwhatthisresourceis="I don't know\nwhat this is",
                       REASON_slack_Iforget="I forget",
                       REASON_slack_Ipersonallydonotfindthistypeofresourcehelpfulformylearning="Not helpful",
                       REASON_slack_ItdoesnotdirectlyrelatetothetypesofcomputationalstatisticalproblemsItypicallyencounter="Unrelated",
                       REASON_slack_Itisatadifficulttimeformetoattend="Difficult time",
                       REASON_slack_Someotherreason ="Other",
                       REASON_slack_Iamnotcomfortableattendingthisgroup = "Not comfortable"))%>%
  
  ggplot(., aes(x=question,y=perc,fill=REASON)) + geom_bar(stat="identity",position='stack',width = 0.3)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Why don't you use\nthe EcoStats slack",
                    guide = guide_legend(reverse = TRUE) )+
  coord_flip()+theme_classic()+guides(y="none",x="none")+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(4,15))+mytheme#+ggtitle("Percent of responses")






#Most important concepts
pal<-latin_palette('rauw',9,type="discrete")

z<-df[,12:19]

CONCEPTSPLT<-gather(z,key="Concept",value="Rank")%>%mutate(Concept=recode(Concept,
                                                             ImptRnk_ExpDesgn="Experimental Design",
                                                             ImptRnk_Freq="Foundations of\nfrequentist stats",
                                                             ImptRnk_Bayes="Foundations of Bayesian stats",
                                                             ImptRnk_ModPerform="Evaluating model\nperformance",
                                                             ImptRnk_CommResult="Communicating scientific\nresults",
                                                             ImptRnk_CmplxSys="Foundations of\ncomplex systems",
                                                             ImptRnk_CausalInf="Causal inference",
                                                             ImptRnk_MachineLrn="Foundations of\nmachine learning"))%>%
  mutate(Rank=factor(Rank,c("1","2","3","4","5","6","7","8")))%>%na.omit()%>%
  group_by(Concept,Rank)%>%count()%>%ungroup()%>%
  group_by(Concept)%>%
  mutate(perc = n/sum(n))%>%
  
  ggplot(.,aes(x=Rank,y=perc,fill=Concept)) + geom_bar(stat="identity",position='stack',width = 0.8)+
  geom_text(aes(label=paste0(round(perc*100),"%"),size=perc),position=position_fill(0.5),show.legend = FALSE,color="white",fontface="bold")+
  scale_fill_manual(values=pal, name ="Most important concepts\nfor students to learn",
                    guide = guide_legend(reverse = TRUE) )+
 theme_classic()+scale_y_continuous(labels=scales::percent)+ylab("")+
  scale_radius(range = c(2,8))+mytheme2#+ggtitle("Percent of responses")


#cohort plot
COHORT<-df%>%filter(Position%in%c("Masters student","PhD student"))%>%
  ggplot(.,aes(x=as.factor(StartYear)))+geom_bar(fill="#2171b5",color="darkgrey",size=1.5)+theme_classic()+mytheme2+ylab("Number of responses")+xlab("Year started")+
  scale_y_continuous(breaks=c(0,4,8,12))


