library(ggplot2)
library(RColorBrewer)
library(ggpubr)

## Post-hoc test of which explanatory levels vary.
source("https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R")
myChi<-chisq.test(first_subset$Q21, first_subset$Race)
Observed_table<-myChi$observed

myChi<-chisq.test(first_subset$Q19, first_subset$Race)
Observed_table<-myChi$observed

ggplot(data=first_subset)+
  geom_bar(aes(x=Q8, fill= `Game Night Interest`), position="fill", width=0.25)+
  ylab("Proportion of Students in Each Department")+
  ggtitle("Proportion of Students in each Department based on Gender")+
  scale_fill_brewer(palette="Set2")

first_subset$Q19[first_subset$Q19 == "Other:"] <- NA

ggplot(data = subset(first_subset, !is.na(Q19)))+
  geom_bar(aes(x = Race), fill = "blue")+
  theme(axis.text.x = element_text(angle = 45))

#--------------------------------------------
#Race vs Transportation Barriers

p1 <-ggplot(data= subset(df, !is.na(Q19) & !is.na(Q10_1)))+
  geom_bar(aes(x = Q10_1, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

p2 <- ggplot(data= subset(df, !is.na(Q19) & !is.na(Q10_2)))+
  geom_bar(aes(x = Q10_2, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")


p3 <- ggplot(data= subset(df, !is.na(Q19) & !is.na(Q10_3)))+
  geom_bar(aes(x = Q10_3, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

p4 <- ggplot(data= subset(df, !is.na(Q19) & !is.na(Q10_4)))+
  geom_bar(aes(x = Q10_4, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+ 
  xlab(" ")+
  scale_fill_brewer(palette = "Set2")

p5 <- ggplot(data= subset(df, !is.na(Q19)& !is.na(Q10_5)))+
  geom_bar(aes(x = Q10_5, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

p6 <- ggplot(data= subset(df, !is.na(Q19) & !is.na(Q10_6)))+
  geom_bar(aes(x = Q10_6, fill = Q19), position = "fill")+
  ylab("Transportation Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

ggarrange(p1,p2, p3, p4, p5, p6, widths = c(1,1,1,1,1,1))
#------------------------------------------------------------------------------------------
#Race vs Cost Barriers
c1 <- ggplot(data= subset(df, !is.na(Q21) & !is.na(Q10_1)))+
  geom_bar(aes(x = Q10_1, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

c2 <- ggplot(data= subset(df, !is.na(Q21) & !is.na(Q10_2)))+
  geom_bar(aes(x = Q10_2, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

c3 <- ggplot(data= subset(df, !is.na(Q21) & !is.na(Q10_3)))+
  geom_bar(aes(x = Q10_3, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette = "Set2")

c4 <- ggplot(data= subset(df, !is.na(Q21) & !is.na(Q10_4)))+
  geom_bar(aes(x = Q10_4, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette = "Set2")

c5 <- ggplot(data= subset(df, !is.na(Q21)& !is.na(Q10_5)))+
  geom_bar(aes(x = Q10_5, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

c6 <- ggplot(data= subset(df, !is.na(Q21) & !is.na(Q10_6)))+
  geom_bar(aes(x = Q10_6, fill = Q21), position = "fill")+
  ylab("Cost Proportion")+
  xlab(" ")+
  scale_fill_brewer(palette="Set2")

ggarrange(c1, c2, c3, c4, c5, c6)

#--------------------------------------------------------------------
#Income vs Barriers
first_subset$Q8 <- factor(first_subset$Q8,
                          ordered = TRUE,
                          levels = c("Don't Know", "Less than 26,000", 
                                     "26,000-34,999", "35,000-44,999", "45,000-64,999", "65,000-103,999", "104,000-115,999", "More than 115,999"))
                          labels = c("Don't Know", "Less than 26,000", 
                                     "26,000-34,999", "35,000-44,999", "45,000-64,999", "65,000-103,999", "104,000-115,999", "More than 115,999")
                          )
ggplot(data= subset(first_subset, !is.na(Q21) & !is.na(Q8)))+
  geom_bar(aes(x = Q8, fill = Q21), position = "fill")+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Cost Barrier by Income")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(data= subset(first_subset, !is.na(Q19) & !is.na(Q8)))+
  geom_bar(aes(x = Q8, fill = Q19), position = "fill")+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Transportation Barrier by Income")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))

#-----------------------------------------------
ggplot(data= subset(first_subset, !is.na(Q19) & !is.na(White)))+
  geom_bar(aes(x = Best_Way_to_hear, fill = White))+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Transportation Barrier by Race")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))

  ggplot(data= subset(df, !is.na(Q15_2) & !is.na(Q8)))+
  geom_bar(aes(x = Q15_2, fill = Q8), position = "fill")+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Transportation Barrier by Race")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(data= subset(df, !is.na(Q15_3) & !is.na(Q8)))+
  geom_bar(aes(x = Q15_3, fill = Q8), position = "fill")+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Transportation Barrier by Race")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(data= subset(df, !is.na(Q15_4) & !is.na(Q8)))+
  geom_bar(aes(x = Q15_4, fill = Q8), position = "fill")+
  ylab("Proportion of People with Cost Barrier")+
  xlab("Cost Barriers")+
  ggtitle("Proportion of People with a Transportation Barrier by Race")+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle = 45))
