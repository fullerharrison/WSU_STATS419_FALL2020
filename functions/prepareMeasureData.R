prepareMeasureData <- function(measure)
{
  measure.X = measure;
  
  require(dplyr); require(stringr); require(measurements);require(tidyverse)
  
  measure.X[, -1:-2] = mutate_if(measure.X[, -1:-2], is.character, tolower)
  
  measure.X$side[measure.X$side %in% c("n/a") ] = NA
  
  # https://stackoverflow.com/questions/51858966/remove-double-quote-symbol-from-string
  measure.X[,-1:-26] =  data.frame(lapply(measure.X[,-1:-26], FUN = function(x) gsub('["\"]', "", x)))
  
  measure.X$units[measure.X$units %in% c("inches", "Inch", "\"in\"", "inch")] = "in";
  measure.X$units = factor(tolower(measure.X$units));
  
  for (i in 1:nrow(measure))
  {
    if (measure.X$units[i] == "in")
    {
      measure.X[i, 4:26] = conv_unit( measure.X[i, 4:26], from = "inch", to = "cm")
    }
  }
  
  measure.X$units[measure.X$units %in% "in"] = "cm";
  
  measure.X$eye[measure.X$eye %in% c("equal") ] = "both";
  measure.X$eye[measure.X$eye %in% c("brown") ] = "left"; 
  
  measure.X$eye_color[measure.X$eye_color %in% c("left")] = "brown";
  
  measure.X$swinging[measure.X$swinging %in% c("let", "leftt")] = "left";
  measure.X$swinging[measure.X$swinging %in% c("rigth")] = "right";
  
  measure.X$gender[measure.X$gender %in% c("f", "F", "Female")] = "female";
  measure.X$gender[measure.X$gender %in% c("m", "M", "Male")] = "male";
  
  factors = c("data_collector", "person_id","side", "swinging", "writing", "eye", "eye_color", "ethnicity", "gender")
  measure.X[, factors] = lapply(X = measure.X[, factors], FUN = as.factor);
  
  numbers = c("age","quality", "minutes");
  measure.X[, numbers] = lapply(X = measure.X[, numbers], FUN = as.numeric);
  
  measure.X = measure.X %>% distinct(., person_id, .keep_all = T)
  #measure.X = measure.X[unique(measure.X$person_id), ]
  
  measure.X = measure.X[!(measure.X$height.NA == 36 | measure.X$height.NA >= 250), ]
  summary(measure.X)
  
  #measure.X[measure.X$height.NA %in% c(180:300), ] 
  #measure[measure$person_id %in% "1c2408654ef5a2fe1fc9620818A6683B", ] 
  
  return(measure.X)
}