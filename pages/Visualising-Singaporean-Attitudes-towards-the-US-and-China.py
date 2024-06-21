import streamlit as st
st.set_page_config(layout="wide",initial_sidebar_state="collapsed")

R_viz_code = """# load packages

library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(foreign)

# import survey data set

df<- foreign::read.spss("Pew Research Center Global Attitudes Spring 2022 Dataset.sav")
class(df)

#convert df to dataframe
df<- as.data.frame(df)

class(df)
# Explore data set
str(df)
head(df)

# As expected, it is a large data set with many columns. Due to the scope of the
# TMA, only data from Singapore is required. Columns that we want either contain
# Singapore in their column headers or contain information about the
# characteristics of the survey respondents. By consulting the documentation on
# the survey questions, the main columns that are needed : 
# fav_us,fav_china,growinflu_us,growinflu_china, as well as demographic
# information like age, race and gender


df%>% filter(country == "Singapore") %>% 
  select(id,"fav_us","fav_china","age","sex",contains("singapore")) %>% view()

# After confirming these are the columns needed, save it into a new dataframe

df%>% filter(country == "Singapore") %>% 
  select(id,"fav_us","fav_china","age","sex",contains("singapore")) -> df2

df2 %>% pivot_longer(cols = c(2,3,6,7), names_to = "Question", values_to = "Response") %>% view()

# Firstly, pivot the table so we can plot the general sentiment of Singaporeans towards US and China

df2 %>% pivot_longer(cols = c(2,3,6,7), names_to = "Question", values_to = "Response") %>% 
  filter(Question %in% c("fav_us","fav_china")) -> df_fav

# view counts of each category
df_fav %>% group_by(Response,Question)  %>% summarise(Data.Count = n()) %>%
view()

# checking the data types
str(df_fav)

# All the categorical variables are factors which are appropriate, but age is also a factor
# Convert age to numeric
df_fav %>% filter(Question %in% c("fav_china","fav_us")) %>% 
  mutate(age = as.character(age)) %>% 
  mutate(age = as.numeric(age))-> df_fav

# here , we can begin to plot the responses of Singaporeans

# plot_data dataframe was created to prevent always subsetting the dataset

plot_data <- subset(df_fav,!Response %in% c("Refused (DO NOT READ)","Don’t know (DO NOT READ)"))


# Fig 1 :  opinion towards US and China

p_opinion <- ggplot(plot_data, aes(x = Response, fill = Question)) +
  geom_bar(position = "dodge",mapping = aes(y = after_stat(prop), group = Question)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=c("#de2910","#3C3B6E"), breaks = c("fav_china","fav_us"),
                    labels = c("fav_china" = "China", "fav_us" = "US")) +
  labs(x = "Opinion", y = "Percentage of Singaporeans",
       title = "Opinion Towards The US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       fill=NULL)+
  theme(title = element_text(size=14), 
        axis.text.x = element_text(size = 12,margin = margin(b = 10)), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10)),
        legend.position ="top")

p_opinion

ggsave("Fig1_Opinion Towards The US and China.png",plot = p_opinion)      

# Looking into the demographics of the people who have a favourable view towards China (demo1 to demo4)

# Vector created to label all the facet plots
facet_names <- c(fav_china = "China",fav_us = "US")


# Fig 2 : Age distribution of respondents
p_demo2 <- ggplot(plot_data, aes(x= age,fill=Question)) +
  facet_wrap(~Response) + 
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values=c("#de2910","#3C3B6E"), breaks = c("fav_china","fav_us"),
                    labels = c("fav_china" = "China", "fav_us" = "US")) +
  labs(x = "Age", y = "Density",
       title = "Age Distribution of Respondents",
       subtitle = "Views Towards : ",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",fill=NULL) + 
  theme(title = element_text(size=14), 
        axis.text.x = element_text(size = 12,margin = margin(b = 5)), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10)),
        legend.position ="top")

p_demo2


ggsave("Age Distribution of Respondents.png",plot = p_demo2)    

# Fig 3a : Gender distribution of respondents
p_demo3 <- ggplot(plot_data, aes(x= Response,fill=sex)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = sex)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) +
  labs(x = "", y = "Percentage of respondents",
       title = "Gender distribution of opinions towards the US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey") +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
                                                                                                        
p_demo3
ggsave("Gender Distribution of Respondents.png",plot = p_demo3)  


# Fig 3b : Age,Gender distribution of respondents (unused in report)
p_demo1 <- ggplot(plot_data, aes(x= Response, y = age,fill=sex)) +
  geom_boxplot() + facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) +
  labs(x = "", y = "Age of respondents",
       title = "Age, Gender of Respondents and their opinion on US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey") +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
p_demo1
ggsave("Age, Gender of Respondents and their opinion on US and China.png",plot = p_demo1)    

# Fig 4 : Ethnicity of respondents

p_demo4 <- ggplot(plot_data, aes(x= Response,fill=d_identity_singapore)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = d_identity_singapore)) +
  facet_wrap(~Question,ncol=1,labeller = as_labeller(facet_names)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete (labels=c ('Chinese', 'Malay','Indian','Others')) +
  labs(x = "", y = "Percentage of respondents",
       title = "Ethnicity of respondents",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       subtitle = "Attitudes towards US and China",
       fill = "Ethnicity" ) +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 
p_demo4
ggsave("Ethnicity of respondents.png",plot = p_demo4)  

lookup_df <- data.frame(
  key = 1:10,
  d_educ_singapore = unique(plot_data$d_educ_singapore))

lookup_df %>%
  mutate(edu_level = case_when(key %in% c(2,7) ~ "Primary and below",
                               key %in% c(5,6) ~ "Secondary school",
                               key %in% c(1,8) ~ "Post Secondary",
                               key == 3 ~ "Bachelor's Degree",
                               key == 4 ~ "Master's Degree",
                               key == 10 ~ "Doctorate",
                               key == 9 ~ ""
        )) -> lookup_df

plot_data %>% left_join(y = lookup_df,by = "d_educ_singapore") ->plot_data 

# Fig 5 : Education level of respondents
p_edu_level <- ggplot(subset(plot_data,!d_educ_singapore == "Refused (DO NOT READ)"),aes(x= Response,fill = edu_level)) + 
  geom_bar(position="dodge",mapping = aes(y = after_stat(prop),group = edu_level)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Question,ncol =1, labeller = as_labeller(facet_names)) + theme(legend.position = "right") +
  labs(x = "", y = "Percentage of respondents",
       title = "Education level of respondents",
       subtitle = "Attitudes towards US and China",
       caption = "Source: Pew Research Center Global Attitudes Spring 2022 survey",
       fill = "Education Level" ) +
  theme(title = element_text(size=14), 
        axis.text.y = element_text(size = 12,margin = margin(l = 10)),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18, margin = margin(b = 10))) 

p_edu_level

ggsave("Education level of respondents.png",plot = p_edu_level) 

unique(plot_data$d_income_singapore)"""

st.subheader("R code for Visualising Singaporean Attitudes towards the US and China")
st.code(R_viz_code,"r")