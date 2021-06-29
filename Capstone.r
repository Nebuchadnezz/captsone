librarian::shelf(foreign, RColorBrewer, svglite, scales, ggpubr,
                 here, dplyr, ggplot2, plyr, stringr, tidyr,
                 lavaan, performance, kableExtra, semPlot)
data <- read.spss("/users/coreyneff/desktop/project2data/didarp.sav",
                  to.data.frame = TRUE,
                  use.value.labels = FALSE)

####### Some fixes for errors ####

input <- data
input$Q4_RC[input$Q4_RC > 100] <- 100
input$Q9_RC <- input$Q9_RC %>% dplyr::na_if(-99)
input$Q53 <- input$Q53 %>% dplyr::na_if(99)

####### Models ####
inclusion <- input %>% filter(Q16 == 1, Q55 == 1, Q68 == 1)


median(input$Q20_RC)

modeldata <- inclusion %>% 
      dplyr::select(Q17_RC, Q18_RC, Q19_RC,
                    Q20_RC, Q21_RC, Q22_RC,
                    Q88, Q89, Q92_RC, Q94, Q95, YOB)  


modeldata$Q94 <- revalue(as.factor(modeldata$Q94), c("1" = "White",
                                                     "2" = "African American",
                                                     "3" = "Asian",
                                                     "4" = "American Indian/Alaska Native",
                                                     "5" = "Native Hawaiin/Pacific Islander",
                                                     "6" = "More than one",
                                                     "7" = "Other"))
modeldata$Q94 <- relevel(modeldata$Q94, ref='White')
modeldata$Q95 <- revalue(as.factor(modeldata$Q95), c("1" = "Some High School",
                                                     "2" = "GED",
                                                     "3" = "High School",
                                                     "4" = "Some College",
                                                     "5" = "Vocational School",
                                                     "6" = "Associate's Degree",
                                                     "7" = "Bachelor's Degree",
                                                     "8" = "Master's Degree",
                                                     "9" = "Professional Degree",
                                                     "10" = "Doctoral Degree"))
modeldata$Q95 <- relevel(modeldata$Q95, ref="Bachelor's Degree")
modeldata$Q89 <- revalue(as.factor(modeldata$Q89), c("1" = "Male",
                                                     "2" = "Female"))
modeldata$Q89 <- relevel(modeldata$Q89, ref="Male")
modeldata$Q88 <- revalue(as.factor(modeldata$Q88), c("1" = "Married",
                                                     "2" = "Divorced",
                                                     "3" = "Widowed",
                                                     "4" = "Single",
                                                     "5" = "Partnered"))
modeldata$Q88 <- relevel(modeldata$Q88, ref="Married")


librarian::shelf(car, MASS)
modeldata$Q17_cut <- cut(modeldata$Q17_RC, breaks = 4)
Q17_model <- polr(Q17_cut ~ Q88 + Q89 + Q94 + Q95, data = modeldata)
summary(Q17_model)

parameters <- 'self_efficacy =~ Q23_RC+Q24_RC+Q25_RC+Q26_RC
            would_communicate =~ Q42+Q43+Q44
            impact =~ Q49+Q50+Q51
            positive_attitude =~ Q57+Q58+Q59+Q60+Q61+Q62+Q63+Q64+Q65+Q66+Q67
            health_literacy =~ Q82+Q81+Q83+Q84
            percentage =~ Q17_RC+Q18_RC+Q19_RC
            percentage ~ health_literacy + positive_attitude + impact + would_communicate + self_efficacy'

cfa_fit <- lavaan::cfa(factors, data = inclusion, std.lv=TRUE)
summary(cfa_fit, fit.measures=T,standardized=T)

sem_model <- lavaan::sem(parameters, data = inclusion, se="bootstrap")
summary(sem_model, fit.measures = T, rsquare = T)

semPaths(sem_model, 'std', layout = 'tree')


####### Health literacy and information #####
input %>% ggplot(aes(x=Q82)) +
      geom_bar(aes(as.factor(Q82), fill=Q82)) +
      scale_x_discrete(labels = c("1" = "Not at all", "2" = "A little bit",
                                  "3" = "Somewhat", "4" = "Quite a bit",
                                  "5" = "Extremely")) +
      xlab("Confidence") +
      ylab("Count")+
      ggtitle(str_wrap("How confident are you filling out medical forms by yourself?", 60)) + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2()
      ggsave(filename = "Q82.svg")

input %>% ggplot(aes(x=Q81)) +
      geom_bar(aes(as.factor(Q81), fill=Q81)) +
      scale_x_discrete(labels = c("1" = "Always", "2" = "Often",
                                  "3" = "Sometimes", "4" = "Occasionally",
                                  "5" = "Never")) +
      xlab("Frequency") +
      ylab("Count")+
      ggtitle(str_wrap("How often do you have someone help you read healthcare materials?", 60)) + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2()
      ggsave(filename = "Q81.svg")


input %>% ggplot(aes(x=Q83)) +
      geom_bar(aes(as.factor(Q83), fill=Q83)) +
      scale_x_discrete(labels = c("1" = "Always", "2" = "Often",
                                  "3" = "Sometimes", "4" = "Occasionally",
                                  "5" = "Never")) +
      xlab("Frequency") +
      ylab("Count")+
      ggtitle(str_wrap("How often do you have problems learning about your medical condition because of difficulty understanding written information?", 60)) + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2() 
      ggsave(filename = "Q83.svg")

input %>% ggplot() +
      geom_bar(aes(as.factor(Q84), fill=Q84)) +
      scale_x_discrete(labels = c("1" = "Always", "2" = "Often",
                                  "3" = "Sometimes", "4" = "Occasionally",
                                  "5" = "Never")) +
      xlab("Frequency") +
      ylab("Count")+
      ggtitle(str_wrap("How often do you have a problem understanding what is told to you about your medical condition?", 60)) + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2()
      ggsave(filename = "Q84.svg")


input %>% filter(Q85_RC < 998) %>%
      ggplot(aes(x=as.factor(Q85_RC))) +
      geom_bar(fill = "darkred") +
      geom_text(stat='count', aes(label=..count..), vjust=-1) +
      ylab("Count") +
      xlab("Incidence") +
      ggtitle(str_wrap("How many times in the past year have you used an illegal drug or used a prescription medication for non-medical reasons?", 60)) + 
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient2()
      ggsave(filename = "Q85.svg")



####### Q4-9 ####
input %>% tidyr::pivot_longer(cols = c(Q4_RC, Q7_RC), 
                       names_to = "Abuse", values_to = "Abuse_Percent") %>%
      ggplot() +
      geom_density(aes(as.numeric(Abuse_Percent), fill = Abuse), alpha=.5) +
      scale_fill_discrete(labels = c("Physician", "Pharmacist")) + 
      ggtitle(str_wrap("On a typical visit to your pharmacist about what percentage of the time do you intend to speak with him/her about how likely it is that the prescription medications they are dispensing could be abused?", 60)) +
      xlab("Percentage") +
      labs(fill = "Provider")
      ggsave(filename = "Q4_7.svg")

input %>% tidyr::pivot_longer(cols = c(Q5_RC, Q8_RC), 
                       names_to = "SecureStorage", values_to = "Storage_Percent") %>%
      ggplot() +
      geom_density(aes(as.numeric(Storage_Percent), fill = SecureStorage), alpha=.5) +
      scale_fill_discrete(labels = c("Physician", "Pharmacist")) +
      ggtitle(str_wrap("On a typical visit, about what percentage of the time do you intend to speak with him/her about secure storage of prescription drugs with abuse potential?"), 60) +
      xlab("Percentage") +
      labs(fill = "Provider")
      ggsave(filename = "Q5_8.svg")


input %>% tidyr::pivot_longer(cols = c(Q6_RC, Q9_RC), 
                       names_to = "AbuseHistory", values_to = "History_Percent") %>%
      ggplot() +
      geom_density(aes(as.numeric(History_Percent), fill = AbuseHistory), alpha=.5) +
      scale_fill_discrete(labels = c("Physician", "Pharmacist")) +
      ggtitle(str_wrap("On a typical visit about what percentage of the time do you intend to speak with him/her about a personal or family history of prescription or other drug abuse?"), 60) +
      xlab("Percentage") +
      labs(fill = "Provider")
      ggsave(filename = "Q6_9.svg")


####### Q10-15 ###########
input %>% tidyr::pivot_longer(cols = c(Q10, Q13),
                       names_to = "necessary",
                       values_to = "necessary_scale") %>% 
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(necessary_scale, fill = necessary), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      ggtitle("How necessary is it for you to talk to each HCP about likelihood of abuse?") +
      scale_x_discrete(labels = c("1" = "Not at all necessary", "2" = "Somewhat necessary",
                                  "3" = "Necessary", "4" = "Mostly necessary",
                                  "5" = "Totally necessary")) +
      theme(axis.text.x = element_text(angle = 90))
      ggsave(filename = "Q10_13.svg")


input %>% tidyr::pivot_longer(cols = c(Q11, Q14),
                       names_to = "necessary",
                       values_to = "necessary_scale") %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(necessary_scale, fill = necessary), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      ggtitle("How necessary is it for you to talk to each HCP about secure drug storage?") +
      scale_x_discrete(labels = c("1" = "Not at all necessary", "2" = "Somewhat necessary",
                                  "3" = "Necessary", "4" = "Mostly necessary",
                                  "5" = "Totally necessary")) +
      theme(axis.text.x = element_text(angle = 90))
      ggsave(filename = "Q11_14.svg")


input %>% tidyr::pivot_longer(cols = c(Q12, Q15),
                       names_to = "necessary",
                       values_to = "necessary_scale") %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(necessary_scale, fill = necessary), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      ggtitle("How necessary is it for you to talk to each HCP about family history of abuse") +

      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_discrete(labels = c("1" = "Not at all necessary", "2" = "Somewhat necessary",
                                  "3" = "Necessary", "4" = "Mostly necessary",
                                  "5" = "Totally necessary"))
      ggsave(filename = "Q12_15.svg")


####### Q16 #######

Q16_hist <- input %>%  
      dplyr::select(Q16) %>% 
      mutate_all(as.factor) %>%
      ggplot() +
      geom_bar(aes(Q16, fill = Q16)) + 
      scale_x_discrete(labels = c("1" = "Yes", "2" = "No")) +
      xlab("Response") +
      ggtitle("Have you ever received a prescription for a drug that can be abused?") +
      theme(legend.position = "none") +
      theme_bw()
      ggsave(Q16_hist, filename = "Q16.svg")


####### Q17-22 #######

input %>% tidyr::pivot_longer(cols = c(Q17_RC, Q20_RC),
                                   names_to = "Q20",
                                   values_to = "Q20_scale") %>%
      dplyr::select(Q20, Q20_scale) %>%
      ggplot() +
      geom_density(aes(Q20_scale, fill = Q20), alpha = 0.65) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Percentage") + 
      ylab("Density") +
      ggtitle(str_wrap("Approximately what percentage of the time have you discussed with your __ about the fact that the prescription medication could possibly be abused?", 130)) +
      theme(axis.text.x = element_text(angle = 0)) +
      theme_minimal()
      ggsave(filename = "Q17_20.svg")

input %>% tidyr::pivot_longer(cols = c(Q18_RC, Q21_RC),
                                   names_to = "Q21",
                                   values_to = "Q21_scale") %>%
      dplyr::select(Q21, Q21_scale) %>%
      ggplot() +
      geom_density(aes(Q21_scale, fill = Q21), alpha = 0.65) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Percentage") + 
      ylab("Density") +
      ggtitle(str_wrap("Approximately what percentage of the time have you discussed with your __ about secure storage of prescription drugs with abuse potential?", 130)) +
      theme(axis.text.x = element_text(angle = 0)) +
      theme_minimal() +
      ggsave(filename = "Q18_21.svg")

input %>% tidyr::pivot_longer(cols = c(Q19_RC, Q22_RC),
                                   names_to = "Q22",
                                   values_to = "Q22_scale") %>%
      dplyr::select(Q22, Q22_scale) %>%
      ggplot() +
      geom_density(aes(Q22_scale, fill = Q22), alpha = 0.65) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Percentage") + 
      ylab("Density") +
      ggtitle(str_wrap("Approximately what precentage of the time have you discussed with your __ about a personal or family history of prescription or other drug abuse?",130)) +
      theme(axis.text.x = element_text(angle = 0)) +
      theme_minimal()
      ggsave(filename = "Q19_22.svg")

####### Q45-48 #####

input %>% tidyr::pivot_longer(cols = c(Q45, Q47),
                       names_to = "treatment",
                       values_to = "treatment_scale") %>%
      dplyr::select(treatment_scale, treatment) %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(treatment_scale, fill = treatment), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      theme_minimal() +
      ggtitle(str_wrap("If you told your HCP about a history of prescription or other drug abuse, do you believe he/she would not listen to your health concerns?", 60)) +
      scale_x_discrete(labels = str_wrap(c("1" = "Very untrue of what I believe", "2" = "Untrue of what I believe",
                                           "3" = "Neutral", "4" = "True of what I believe",
                                           "5" = "Very True of what I believe"), 15))+
      theme(axis.text.x = element_text(angle = 90)) %>%
      ggsave()

input %>% pivot_longer(cols = c(Q46, Q48),
                       names_to = "treatment",
                       values_to = "treatment_scale") %>%
      select(treatment_scale, treatment) %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(treatment_scale, fill = treatment), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      theme_minimal() +
      ggtitle(str_wrap("If you told your physician about a history of prescription or other drug abuse, do you believe he/she would give you poor care?", 60)) +
      scale_x_discrete(labels = str_wrap(c("1" = "Very untrue of what I believe", "2" = "Untrue of what I believe",
                                           "3" = "Neutral", "4" = "True of what I believe",
                                           "5" = "Very True of what I believe"), 15))+
      theme(axis.text.x = element_text(angle = 90))

####### Q49-54 ######

input %>% pivot_longer(cols = c(Q49, Q52),
                       names_to = "communication",
                       values_to = "communication_scale") %>%
      select(communication_scale, communication) %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(communication_scale, fill = communication), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      theme_minimal() +
      ggtitle(str_wrap("Improving communication between HCPs and patients would deter prescription drug abuse", 60)) +
      scale_x_discrete(labels = str_wrap(c("1" = "Strongly disagree", "2" = "Disagree",
                                           "3" = "Neutral", "4" = "Agree",
                                           "5" = "Strongly Agree"), 15)) +
      theme(axis.text.x = element_text(angle = 90))

input %>% pivot_longer(cols = c(Q50, Q53),
                       names_to = "communication",
                       values_to = "communication_scale") %>%
      select(communication_scale, communication) %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(communication_scale, fill = communication), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      theme_minimal() +
      ggtitle(str_wrap(" Improving communication between HCPS and patients would deter prescription drug sharing.", 60)) +
      scale_x_discrete(labels = str_wrap(c("1" = "Strongly disagree", "2" = "Disagree",
                                           "3" = "Neutral", "4" = "Agree",
                                           "5" = "Strongly Agree"), 15)) +
      theme(axis.text.x = element_text(angle = 90))

input %>% pivot_longer(cols = c(Q51, Q54),
                       names_to = "communication",
                       values_to = "communication_scale") %>%
      select(communication_scale, communication) %>%
      mutate_if(is.numeric, as.factor) %>%
      ggplot() +
      geom_bar(aes(communication_scale, fill = communication), position=position_dodge()) +
      labs(fill = "Provider") +
      scale_fill_discrete(labels = c("Physician",
                                     "Pharmacist")) +
      xlab("Scale") + 
      theme_minimal() +
      ggtitle(str_wrap("Improving communication between HCPs and patients would increase secure storage of prescription drugs with abuse potential.", 60)) +
      scale_x_discrete(labels = str_wrap(c("1" = "Strongly disagree", "2" = "Disagree",
                                           "3" = "Neutral", "4" = "Agree",
                                           "5" = "Strongly Agree"), 15)) +
      theme(axis.text.x = element_text(angle = 90))


####### Q17 Plots ####
Q17_1 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q17_2 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q17_3 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q17_4 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q17_5 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q17_6 <- modeldata %>% 
      ggplot(aes(y=Q17_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange1 <- ggpubr::ggarrange(Q17_1,Q17_5,Q17_6,Q17_4,Q17_2,Q17_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange1,top = text_grob(str_wrap(
      "Q17. What precentage of the time have you discussed with your physician the fact that the prescription medication could possibly be abused?",120), 
      color = "black", face = "bold", size = 14))
      ggsave(filename = "Q17_demographics.svg")

####### Q18 Plots ####
Q18_1 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q18_2 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q18_3 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q18_4 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q18_5 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q18_6 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange2 <- ggpubr::ggarrange(Q18_1,Q18_5,Q18_6,Q18_4,Q18_2,Q18_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange2,top = text_grob(str_wrap(
      "Q18. What precentage of the time have you discussed with your physician secure storage of prescription drugs with abuse potential?",120), 
      color = "black", face = "bold", size = 14))
      ggsave(filename = "Q18_demographics.svg")

####### Q19 Plots ####
Q19_1 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q19_2 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q19_3 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q19_4 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q19_5 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q19_6 <- modeldata %>% 
      ggplot(aes(y=Q19_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange3 <- ggpubr::ggarrange(Q19_1, Q19_5, Q19_6, Q19_4, Q19_2 ,Q19_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange3,top = text_grob(str_wrap(
      "Q19. What precentage of the time have you discussed with your physician a personal or family history of prescription or other drug abuse? ",120), 
      color = "black", face = "bold", size = 14))
      ggsave(filename = "Q19_demographics.svg")

####### Q20 Plots ####
Q20_1 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q20_2 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q20_3 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q20_4 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q20_5 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q20_6 <- modeldata %>% 
      ggplot(aes(y=Q20_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange4 <- ggpubr::ggarrange(Q20_1, Q20_5, Q20_6, Q20_4, Q20_2 ,Q20_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange4,top = text_grob(str_wrap(
      "Q20. What precentage of the time have you discussed with your pharmacist the fact that the prescription medication could possibly be abused?",120), 
      color = "black", face = "bold", size = 14)) 
      ggsave(filename = "Q20_demographics.svg")
      
####### Q21 Plots ####
Q21_1 <- modeldata %>% 
      ggplot(aes(y=Q18_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q21_2 <- modeldata %>% 
      ggplot(aes(y=Q21_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q21_3 <- modeldata %>% 
      ggplot(aes(y=Q21_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q21_4 <- modeldata %>% 
      ggplot(aes(y=Q21_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q21_5 <- modeldata %>% 
      ggplot(aes(y=Q21_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q21_6 <- modeldata %>% 
      ggplot(aes(y=Q21_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange5 <- ggpubr::ggarrange(Q21_1,Q21_5,Q21_6,Q21_4,Q21_2,Q21_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange5,top = text_grob(str_wrap(
      "Q21. What precentage of the time have you discussed with your pharmacist secure storage of prescription drugs with abuse potential?",120), 
      color = "black", face = "bold", size = 14))
      ggsave(filename = "Q21_demographics.svg")
####### Q22 PLots ####
Q22_1 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_boxplot(aes(x=Q89), fill = "skyblue") +
      theme(axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Gender")

Q22_2 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_boxplot(aes(x=Q88), fill = "tomato") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Relationship")

Q22_3 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_boxplot(aes(x=Q94), fill = "lightgreen") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Race")

Q22_4 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_boxplot(aes(x=Q95), fill = "orange") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.y=element_blank()) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      xlab("Education")

Q22_5 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_point(aes(x=2021-YOB)) +
      geom_smooth(aes(x=2021-YOB), method = "lm") +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) + 
      xlab("Age")

Q22_6 <- modeldata %>% 
      ggplot(aes(y=Q22_RC)) +
      geom_point(aes(x=Q92_RC)) +
      geom_smooth(aes(x=Q92_RC), method = "lm") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::percent_format(scale=1)) +
      theme(axis.title.y = element_blank()) +
      xlab("Income")

arrange6 <- ggpubr::ggarrange(Q22_1, Q22_5, Q22_6, Q22_4, Q22_2 ,Q22_3, ncol = 3, nrow=2)
ggpubr::annotate_figure(arrange6,top = text_grob(str_wrap(
      "Q22. What precentage of the time have you discussed with your pharmacist a personal or family history of prescription or other drug abuse? ",120), 
      color = "black", face = "bold", size = 14))       
      ggsave(filename = "Q22_demographics.svg")

####### ML Models ####
librarian::shelf(caret, rpart)

inclusion$Q17_cat <- cut(inclusion$Q17_RC, 4)
MLdata <- inclusion %>% filter(!is.na(Q17_cat)) %>% select(-c(UniqueID, Zip, YOB, YOB2, 
                                                              Q17_RC, Q18_RC, Q19_RC,
                                                              Q20_RC, Q21_RC, Q22_RC,
                                                              Q92_RC, Q92b,
                                                              Q4_RC, Q5_RC, Q6_RC,
                                                              Q7_RC, Q8_RC, Q9_RC))
set.seed(123)
### Tree ###
Q17_tree <- rpart(Q17_cat ~., MLdata, method = "class")
summary(Q17_tree)
plot(Q17_tree)
text(Q17_tree, use.n=TRUE, all=TRUE, cex=.8)

tree_predictions <- MLdata %>% 
      select(-c(Q17_cat)) %>%
      predict(Q17_tree,., type="vector") %>%
      as.factor()
tree_predictions <- factor(tree_predictions, levels = c(1:4))

real <- MLdata$Q17_cat %>%
      revalue(., c("(-0.1,25]" = "1",
                   "(25,50]" = "2",
                   "(50,75]" = "3",
                   "(75,100]" = "4")) %>%
      as.factor()
confusionmatrix_tree <- confusionMatrix(tree_predictions, real)

### naive Bayes ###
Q17_bayes <- e1071::naiveBayes(Q17_cat ~. ,data = MLdata)
bayes_predictions <- MLdata %>% 
      select(-c(Q17_cat)) %>%
      predict(Q17_bayess,., type="vector") %>%
      as.factor() %>%
      revalue(., c("(-0.1,25]" = "1",
                   "(25,50]" = "2",
                   "(50,75]" = "3",
                   "(75,100]" = "4"))
confusionmatrix_bayes <- confusionMatrix(bayes_predictions, real)
confusionmatrix_bayes












