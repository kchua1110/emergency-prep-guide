#ACPM Emergency Preparedness Guide 
#November 10 -- modified March 26, 2024 (training NCEAS)
#Kristine Chua

#files that were saved during November 10 = have the 3 people who are under 18 years old
#files that have been updated during November 17 = excluded those 3 people from the analysis

install.packages("kableExtra")
install.packages("devtools")
install.packages("sjPlot")
install.packages("likert") # remotes::install_github("jbryer/likert")
install.packages("janitor")
install.packages("flextable")
devtools::install_github("haozhu233/kableExtra")
library(tidyverse)
library(tidyr)
library(kableExtra)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(parameters)
library(dplyr)
library(likert)
library(ggplot2)
library(janitor)
library(flextable)
library(scales)
library(plyr)


# import pilot data
qualtrics_data <- read.csv("data/qualtrics_data.csv")
paper_data <- read.csv("data/paper_data.csv")

#merge data
full_N <- qualtrics_data %>% count() 

merged_df <- qualtrics_data %>% full_join(paper_data)  

#replace all blank cells with NA and remove all the people under 18 (N=3)
merged_df <- merged_df %>% mutate_all(na_if,"") %>%
  filter(is.na(over18)|over18!="No")

#checking unique values for all columns in ascending order
sort(unique( merged_df$Q15 ))

#combining response answers based on previous unique function
merged_df <- merged_df %>%
  mutate(Q15 = dplyr::recode(Q15, `Other - Chicana Latina American` = "Other", `Black or African American; White; Multiracial` = "Multiracial"))%>%
  mutate(Q16 = dplyr::recode(Q16, "Hispanic" = "Hispanic or Latino", "Non-Hispanic" = "Not Hispanic or Latino"))%>%
  mutate(Q17 = dplyr::recode(Q17, "Associate's level" = "Associate's degree level", "Associate\xd5s degree level" = "Associate's degree level",
                      "Bachelor's level" = "Bachelor's degree level", "Bachelor\xd5s degree level" = "Bachelor's degree level",
                      "Doctorage degree level" = "Doctorate degree level", "Doctorage degree level " = "Doctorate degree level", "Doctorate level (PhD, MD, PsyD, DO, etc.)" = "Doctorate degree level",
                      "High school level" = "High school degree level",
                      "Master's degree level " = "Master's degree level", "Master's level (MPH, MA, MS, etc.)" = "Master's degree level", "Master\xd5s degree level" = "Master's degree level",
                      "Other: Please describe" = "Other"))%>%
  mutate(Q18 = dplyr::recode(Q18, "Some College" = "Some college"))%>%
  mutate(Q4 = dplyr::recode(Q4, "Somehwhat agree" = "Somewhat agree"))%>%
  mutate(Q9 = dplyr::recode(Q9, "Strongly Disagree" = "Strongly disagree"))%>%
  mutate(Q11 = dplyr::recode(Q11, "Agree" = "Somewhat agree", "Disagree" = "Somewhat disagree"))%>%
  mutate(Q12 = dplyr::recode(Q12, "Agree" = "Somewhat agree", "Disagree" = "Somewhat disagree"))


#coverting data into factors
merged_df <- merged_df %>%
  mutate_at(c(2:23), as.factor)

#save cleaned and merged data
write.csv(merged_df, "merged_paper_qualtrics_022823.csv", row.names = F)


#checking levels using this example code
levels(merged_df$Q4)


#creating likert plot
guide_feedback <- merged_df[, 5:14 ]


#reordering the Likert responses in the likert plot
guide_feedback <- guide_feedback %>%
  dplyr::mutate(across(1:10, ~ factor(.x, levels = c("Strongly disagree", "Somewhat disagree", 
                                             "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))))

#renaming the columns
colnames(guide_feedback) <- c("This resource is helpful to me and my family.", 
                              "I plan to use this guide to help prepare for emergencies.",
                              "This resource is helpful to other families and individuals with disabilities.",
                              "I plan to use this guide to create an emergency preparedness kit.",
                              "I plan to use this guide to plan out evacuation plans for myself/my family.",
                              "I plan to use this guide to create an emergency care plan for myself/my family.",
                              "I plan to print out the contact information for the resources provided in this guide.",
                              "I plan to use this guide to plan out outbreak plans for myself/my family.",
                              "I believe it is important to adequately prepare for emergencies.",
                              "I believe individuals with disabilities and their families need to prepare for specific situations that other families might not need to prepare for.")


#reorders the group, puts the legend on the right and adds the graph labels
p1 <- plot(likert(guide_feedback), 
           group.order = c("This resource is helpful to me and my family.", 
                           "I plan to use this guide to help prepare for emergencies.",
                           "This resource is helpful to other families and individuals with disabilities.",
                           "I plan to use this guide to create an emergency preparedness kit.",
                           "I plan to use this guide to plan out evacuation plans for myself/my family.",
                           "I plan to use this guide to create an emergency care plan for myself/my family.",
                           "I plan to print out the contact information for the resources provided in this guide.",
                           "I plan to use this guide to plan out outbreak plans for myself/my family.",
                           "I believe it is important to adequately prepare for emergencies.",
                           "I believe individuals with disabilities and their families need to prepare for specific situations that other families might not need to prepare for."),
           legend.position = "right") +
  guides(fill = guide_legend(title = "Responses", reverse = FALSE)) +
  ggtitle("Emergency Preparedness Guide Survey Feedback") +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  ylab("Percentages") +   
  xlab("Questions") 


print(p1)

#save image by via ggsave

ggsave("likertplot.png", width = 12, height = 6, units = "in", dpi = 500)


#close image
dev.off()

#########################
#making table with the frequency of multiple categorical variables
#transpose data so that the columns are now rows and the rows are columns
#get the summary of the frequencies
#create a percentage
#remove the duplicate terms from the first column
df <- merged_df[, 5:14 ]

df1 <- df %>% 
  pivot_longer(
    cols = c("Q3",  "Q4",  "Q5",  "Q6",  "Q7",  "Q8",  "Q9",  "Q10", "Q11", "Q12"), 
    names_to = "Variable",
    values_to = "Level"
  )%>%
  group_by(Variable, Level) %>% 
  mutate(Variable = factor(Variable, levels = c("Q3",  "Q4",  "Q5",  "Q6",  "Q7",  "Q8",  "Q9",  "Q10", "Q11", "Q12"))) %>%
  arrange(Variable)%>%
  mutate(Level = factor(Level, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Missing"))) %>%
  arrange(Level)%>%
  dplyr::summarise(Frequency = n()) %>% 
  mutate(Total = sum(Frequency/10))%>%
  mutate(Percent = Frequency/sum(Frequency/10)) %>%
  mutate(Variable = ifelse(duplicated(Variable), "", Variable)) %>% 
  ungroup()


#rename  columns
df1<- df1 %>%
  mutate(Variable = dplyr::recode(Variable, 
                                  "1" = "This resource is helpful to me and my family.", 
                                  "2" = "I plan to use this guide to help prepare for emergencies.",
                                  "3" = "This resource is helpful to other families and individuals with disabilities.",
                                  "4" = "I plan to use this guide to create an emergency preparedness kit.",
                                  "5" = "I plan to use this guide to plan out evacuation plans for myself/my family.",
                                  "6" = "I plan to use this guide to create an emergency care plan for myself/my family.",
                                  "7" = "I plan to print out the contact information for the resources provided in this guide.",
                                  "8" = "I plan to use this guide to plan out outbreak plans for myself/my family.",
                                  "9" = "I believe it is important to adequately prepare for emergencies.",
                                  "10" = "I believe individuals with disabilities and their families need to prepare for specific situations that other families might not need to prepare for."
                                  ))



#converting percent variable to a percentage to the nearest 10th
df1$Percent <- percent(df1$Percent, accuracy = 0.01)

#adds extra column so that each group of questions are labeled
#repeat the same name and then remove the duplicates
df1 <- df1 %>%
  data.frame(Category = c(rep("Emergency Preparedness Guide as a Tool", 12), rep("Emergency Preparedness Guide Usage", 31), rep("Importance to Prepare for Emergencies", 11) )) %>%
  mutate(Category = ifelse(duplicated(Category), "", Category)) %>% 
  ungroup()

#reorder the columns
df1<- df1[c("Category", "Variable", "Level", "Frequency", "Total", "Percent")]

#renamed NA > missing 
df1$Level <- fct_explicit_na(df1$Level, "Missing")

#save cleaned and merged data
write.csv(df1, "guide_feedback022823.csv", row.names = F)



#reopen data file
#df1 <- read.csv("guide_feedback_renamed111022.csv")
 
#formatting table
df1 %>%
  kbl(caption = "<span style='font-size:xx-large'>Emergency Preparedness Guide Survey Feedback</span>") %>%
  kable_classic(full_width = F, position = "center", font_size = 20) %>%
  column_spec(1, bold =  T, border_right = T, width = "15em")%>% #add border to the right and bold the column text
  column_spec(2, width = "20em")%>%
  column_spec(3, width = "8em")%>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "5em")%>%
  column_spec(6, width = "5em")%>%
  row_spec(12, extra_css = "border-bottom: 1px solid")%>% #adding line
  row_spec(43, extra_css = "border-bottom: 1px solid")

#save table as HTML
#took a screenshot and saved image


#########################
#making table with the demographics
#get subset of data
merged_df <- read_csv("merged_paper_qualtrics_022823.csv")

demo <-merged_df[,c(3:4, 16:19, 21:23)]

glimpse(demo)

#changing NAs factors to "Missing" using the fct_explicit_na function
#demo$over18 <- fct_explicit_na(demo$over18, "Missing") 

#using the if else argument to rename NA into "Missing"
#if the over =18 variable is NA, then replace with "missing," if not, then keep value in over18 variable
demo_clean <- demo %>% 
  mutate(over18 = if_else(is.na(over18), "Missing", over18),
         lend_patient = if_else(is.na(lend_patient), "Missing", lend_patient)) 

#-----------------------------------code above works for renaming NA values as "missing"
### next chunk of code could be to do all of the factor reordering
### code below is what was done originally 2024-03-26




  
#reorder factors 
demo_clean$over18 <- factor(demo_clean$over18, levels = c("Yes", "No", "Missing"))

#demo$lend_patient <- fct_explicit_na(demo$lend_patient, "Missing") #check the NAs here -- demo[demo == “NA”] <- NA; is.factor(demo$lend_patient[1])
demo$lend_patient <- factor(demo$lend_patient, levels = c("Yes", "No", "Missing"))

demo$Q14 <- factor(demo$Q14, levels = c("Male", "Female", "Nonbinary", "Different identity", "Prefer not to answer"))

########### BLACK AND AFRICAN AMERINCAN IS GETTING REPLACED BY MISSING -- NEED TO FIX 12.5.22
#demo$Q15 <- fct_explicit_na(demo$Q15, "Missing")
#demo$Q15 <- factor(demo$Q15, levels = c("American Indian or Alaska Native", "Asian", "Black or African American ", "Multiracial", 
#                                        "Native Hawaiian or Pacific Islander", "White", "Other", "Prefer not to answer", "Missing"))

demo$Q16 <- fct_explicit_na(demo$Q16, "Missing")
demo$Q16 <- factor(demo$Q16, levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Prefer not to answer", "Missing"))

demo$Q17 <- fct_explicit_na(demo$Q17, "Missing")
demo$Q17 <- factor(demo$Q17, levels = c("High school degree level", "Associate's degree level", "Bachelor's degree level", 
                                        "Master's degree level", "Doctorate degree level", "Other", "Prefer not to answer", "Missing"))

demo$Q19 <- factor(demo$Q19, levels = c("Yes", "No", "Prefer not to answer"))
demo$Q20 <- factor(demo$Q20, levels = c("Yes", "No", "Prefer not to answer"))
demo$Q21 <- factor(demo$Q21, levels = c("Yes", "No", "Prefer not to answer"))


#replace column names
#colnames(demo) <- c("Over 18", "UC-LEND patient", "Gender identity", "Race", "Ethnicity", "Education", "Self-identified neurodivergent/disability", "Family Historyclose friend identify as neurodivergent/disability", "Caregiver of neurodivergent/disability individuals")

#check that column names are replaced
#names(demo)

#get frequencies of demographics (all variables)
apply(demo, 2, table)


demo1 <- demo %>% 
  pivot_longer(
    cols = c("over18", "lend_patient", "Q14", "Q15", "Q16", "Q17", "Q19", "Q20", "Q21"),
    names_to = "Variable",
    values_to = "Level",
  ) %>% 
  group_by(Variable, Level) %>% 
  dplyr::summarise(Frequency = n()) %>% 
  mutate(Total = sum(Frequency/9))%>%
  mutate(Percent = Frequency/(sum(Frequency)/9)) #%>% 
  #mutate(Variable = factor(Variable, levels = c("over18", "lend_patient", "Q14", "Q15", "Q16", "Q17", "Q19", "Q20", "Q21"))) %>%
  #arrange(Variable)%>%


#rename  columns
demo1 <- demo1 %>%
  mutate(Variable = dplyr::recode(Variable, "over18" = "Over 18", "lend_patient" = "UC-LEND patient", 
                                 "Q14" = "Gender identity", "Q15" = "Race", "Q16" = "Ethnicity", 
                                 "Q17" = "Education", "Q19" = "Self-identified neurodivergent/disability", 
                                 "Q20" = "Family Historyclose friend identify as neurodivergent/disability", 
                                 "Q21" = "Caregiver of neurodivergent/disability individuals"))



#converting percent variable to a percentage to the nearest 10th
demo1$Percent <- percent(demo1$Percent, accuracy = 0.01)

#manually resorts rows rather than doing in excel, saving and re-uploading 
demo2 <- demo1[c(1:10, 
                 12:18,
                 11,
                 21:22, 20, 19,
                 25:29, 24, 23,
                 30:38), ]

#removing duplicate names from column "Variable"
#did this here so it that the row names didn't get mixed up when it reoders
demo2 <- demo2 %>%
  mutate(Variable = ifelse(duplicated(Variable), "", Variable)) %>% 
  ungroup()

#save cleaned and merged data
write.csv(demo1, "demographics_022823.csv", row.names = F)


#reopen data file
#demo1 <- read.csv("demographics__resorted111022.csv")


#formatting table
demo2 %>%
  kbl(caption = "<span style='font-size:xx-large'>Emergency Preparedness Guide Survey Feedback Participant Demographics</span>") %>%
  kable_classic(full_width = F, position = "center", font_size = 20) %>%
  column_spec(1, bold =  T, border_right = T, width = "15em")%>% #add border to the right and bold the column text
  column_spec(2, width = "15em")%>%
  column_spec(3, width = "8em")%>%
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "5em")%>%
  row_spec(3, extra_css = "border-bottom: 1px solid")%>% #adding line
  row_spec(5, extra_css = "border-bottom: 1px solid") %>%
  row_spec(10, extra_css = "border-bottom: 1px solid") %>%
  row_spec(18, extra_css = "border-bottom: 1px solid") %>%
  row_spec(22, extra_css = "border-bottom: 1px solid") %>%
  row_spec(29, extra_css = "border-bottom: 1px solid") %>%
  row_spec(32, extra_css = "border-bottom: 1px solid") %>%
  row_spec(35, extra_css = "border-bottom: 1px solid")


#save table as HTML
#took a screenshot and saved image


#####
#creating a table for the number of BITLY clicks
#read data 
bitly <- read.csv(file = "bitly.csv")

#rename columns
colnames(bitly) <- c("Survey", "Number of Clicks", "Total Clicks", "Percent")

#recode a value
bitly <- bitly %>%
  mutate(Survey = dplyr::recode(Survey, "Patient Experience survey" = "Patient Experience Feedback Survey"))
  
#covert to percentage
bitly$Percent <- percent(bitly$Percent, accuracy = 0.01)

#formatting table
kbl(bitly, align = "lccc", #align columns
    caption = "<span style='font-size:x-large'>Number of Bitly Clicks Per Each Survey</span>") %>%
  kable_classic(full_width = F, position = "center", font_size = 20) %>%
  column_spec(1, bold =  T, border_right = T, width = "20em")%>% #add border to the right and bold the column text
  column_spec(2, width = "10em")%>%
  column_spec(3, width = "9em")%>%
  column_spec(4, width = "5em")
  

