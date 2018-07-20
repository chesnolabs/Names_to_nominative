names_to_nom <- function(x, preserve_order = F){
  
  male_akk_to_nom <- function(x){
    x <- x %>% 
      str_replace("(?<=[оеє])ва ", "в ") %>%
      str_replace("(?<=[іи])на ", "н ") %>%
      str_replace("а$", "") %>%
      str_replace("ого ", "ий ") %>% 
      str_replace_all("а |a ", " ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])ця ", "ець ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])ня ", "ень ") %>%
      str_replace_all("йе", "є") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])я ", "й ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])я ", "ь ") %>%
      str_replace_all("(?<!еск)у ", "а ") %>%
      str_replace_all("ю ", "я ") %>%
      str_replace("(?<![аоіиеуїєюяАОІИЕУЇЄЮЯ])к ", "ко ") %>% 
      str_replace_all("(?<=[иа])л ", "ло ") %>%
      str_replace("вало ", "вал ") %>%
      str_replace("фало ", "фал ") %>%
      str_replace("’ь ", "ей ") %>%
      str_replace("Павл ", "Павло ") %>% 
      str_replace("Михайл ", "Михайло ") %>%
      str_replace("тр ", "тро ") %>%
      str_replace("Ігорь", "Ігор")
    return(x)
  }
  
  male_dat_to_nom <- function(x){
    x <- x %>% 
      str_replace("(?<=[оеє])ву ", "в ") %>%
      str_replace("(?<=[іи])ну ", "н ") %>%
      str_replace("у$", "") %>%
      str_replace("ому ", "ий ") %>% 
      str_replace_all("у |y ", " ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])цю ", "ець ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])ню ", "ень ") %>%
      str_replace_all("йе", "є") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])ю ", "й ") %>%
      str_replace_all("(?<=[^аоіиеуїєюя])ю ", "ь ") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])сі ", "ха ") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])ці ", "ка ") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])зі ", "га ") %>%
      str_replace_all("(?<!іан)і ", "а ") %>%
      str_replace_all("і ", "я ") %>%
      str_replace_all("ї ", "я ") %>%
      str_replace("(?<=[^аоіиеуїєюяАОІИЕУЇЄЮЯ])к ", "ко ") %>% 
      str_replace_all("(?<=[иа])л ", "ло ") %>%
      str_replace("вало ", "вал ") %>%
      str_replace("фало ", "фал ") %>%
      str_replace("’ь ", "ей ") %>%
      str_replace("Павл ", "Павло ") %>% 
      str_replace("Михайл ", "Михайло ") %>%
      str_replace("тр ", "тро ") %>% 
      str_replace("Ігорь", "Ігор")
    return(x)
  }
  
  female_akk_to_nom <- function(x){
    x <- x %>% 
      str_replace_all("(?<!еск)у ", "а ") %>% 
      str_replace_all("(?<!еск)у$", "а") %>% 
      str_replace_all("ю ", "я ")
    return(x)
  }
  
  female_dat_to_nom <- function(x){
    x <- x %>% 
      str_replace_all("і ", "а ") %>% 
      str_replace_all("ій ", "а ") %>%
      str_replace_all("і$", "а") %>% 
      str_replace_all("ї ", "я ") %>% 
      str_replace_all("(?<=[аоіиеуяїєюя])сі ", "ха ") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])ці ", "ка ") %>%
      str_replace_all("(?<=[аоіиеуяїєюя])зі ", "га ")
    return(x)
  }
  
  other_to_nom <- function(x){
    x <- x %>% 
      str_replace_all("а ", " ") %>% 
      str_replace_all("а$", "") 
    return(x)
  }
  
  if(preserve_order == F){
    text_male_akk <- str_subset(x, "[иі]ча$|(?<=[оеє])ва ")
    text_male_dat <- str_subset(x, "[иі]чу$|(?<=[оеє])ву ")
    text_female_akk <- str_subset(x, "вну$")
    text_female_dat <- str_subset(x, "вні$")
    text_other <- x[!(x%in%c(text_male_akk, text_male_dat,
                             text_female_akk, text_female_dat))]
    
    text_male_akk_red <- male_akk_to_nom(text_male_akk)
    text_male_dat_red <- male_dat_to_nom(text_male_dat)
    text_female_akk_red <- female_akk_to_nom(text_female_akk)
    text_female_dat_red <- female_dat_to_nom(text_female_dat)
    text_other_red <- other_to_nom(text_other)
    
    all_text_red <- c(text_male_akk_red, text_male_dat_red,
                      text_female_akk_red, text_female_dat_red,
                      text_other_red) %>% sort()
    return(all_text_red)
    
  } else {
    
    names_df <- data_frame(text_names = x,
                           gender_case = ifelse(str_detect(x, "[иі]ча$|(?<=[оеє])ва "), "male_akk",
                                                ifelse(str_detect(x, "[иі]чу$"), "male_dat",
                                                       ifelse(str_detect(x,"вну$"), "female_akk",
                                                              ifelse(str_detect(x, "вні$"), "female_dat", "other")))))
    names_df$text_names[names_df$gender_case == "male_akk"&!is.na(names_df$gender_case)] <- male_akk_to_nom(names_df$text_names[names_df$gender_case == "male_akk"&!is.na(names_df$gender_case)]) 
    names_df$text_names[names_df$gender_case == "male_dat"&!is.na(names_df$gender_case)] <- male_dat_to_nom(names_df$text_names[names_df$gender_case == "male_dat"&!is.na(names_df$gender_case)])
    names_df$text_names[names_df$gender_case == "female_akk"&!is.na(names_df$gender_case)] <- female_akk_to_nom(names_df$text_names[names_df$gender_case == "female_akk"&!is.na(names_df$gender_case)])
    names_df$text_names[names_df$gender_case == "female_dat"&!is.na(names_df$gender_case)] <- female_dat_to_nom(names_df$text_names[names_df$gender_case == "female_dat"&!is.na(names_df$gender_case)])
    names_df$text_names[names_df$gender_case == "other"&!is.na(names_df$gender_case)] <- other_to_nom(names_df$text_names[names_df$gender_case == "other"&!is.na(names_df$gender_case)])
    
    return(names_df$text_names)
  }
}
