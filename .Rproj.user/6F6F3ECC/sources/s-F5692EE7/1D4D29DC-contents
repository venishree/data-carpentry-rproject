library("tidyverse")
str(surveys)
select(surveys,species_id,weight)
filter(surveys, year==1995)



  #Pipes: %>%
  
  exercise <- surveys %>%
    filter(year==1995)%>% 
  select(species_id,weight)
  
 #only select the rows with species weight 
  #less than 5, then select only the species id, sex 
  # and weight
  
  exercise <- surveys %>% 
    filter(weight < 5)%>%
  select(species_id, sex, weight)
  
  #create new column
  #mutate
  
  data with_kg <- surveys %>% 
  mutate(weight_kg weight/1000)
  
  
  surveys %>% 
  mutate(weight_kg=weight/1000,weight_kg2=weight_kg*2) %>% head()
  
  
  surveys %>% 
    filter(!is.na(weight)) %>%  
    mutate(weight_kg=weight/1000,weight_kg2=weight_kg*2) %>% head()
  
  surveys %>% 
    group_by(sex) %>% 
              summarise(mean_weight=mean(weight, na.rm=TRUE))
  
  surveys %>% 
  group_by(sex,species_id) %>% 
  
           
           surveys %>% 
    filter(!sex=="") %>% 
                 group_by(sex) %>% 
             summarise(mean_weight=mean(weight, na.rm=TRUE))
  
  surveys %>% 
    filter(!sex=="M" | sex=="F") %>% 
    group_by(sex) %>% 
    summarise(mean_weight=mean(weight, na.rm=TRUE))  
  
  surveys %>% 
    filter(!is.na(weight)) %>% 
    group_by(sex, species_id) %>% 
    summarise(mean_weight=mean(weight)) 
  
  summarise_two <- surveys %>% 
    filter(!sex=="") %>% 
    filter(!is.na(weight)) %>% 
    group_by(sex, species_id) %>% 
    summarise(mean_weight=mean(weight),min_weight=min(weight),max_weight=max(weight)) 
  #use the previous commands and add a new column that gives the
  #minimum weight (and maximum weight)
  #counting with two variables
  surveys %>% 
    filter(!is.na(weight)) %>% 
  summarise(mean_weight=mean(weight),min_weight=min(weight),max_weight=max(weight))
  
  surveys %>% 
    filter(!sex=="") %>% 
    count(sex)
  #Reshaping the data
  
  surveys %>% 
    filter(!sex=="") %>% 
    count(sex,species) %>% 
    arrange(species,desc(n))
  