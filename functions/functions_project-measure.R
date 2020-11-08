

clean.nba.data <- function(data)
{
  data = nba[, -c(5,9,11)];

  # https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric
  data[, 5:6] = lapply(data[, 5:6],  function(x) as.numeric(as.character(x)))
  
  # convert data
  data.handL = data %>%
  mutate(hand.length.cm = (Hand.length..in.)*2.54);
  
  data.handW = data %>%
  mutate(hand.width.cm = (Hand.width..in.)*2.54);
  
  # https://stackoverflow.com/questions/54693260/height-conversion-in-r/54693575
  # https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns/24168383#24168383
  data.height = data %>%
  separate(Height.w.o.shoes, c('feet', 'inch'), "'", convert = T) %>%
  mutate(hight.cm = (12*feet + inch)*2.54);
  
  data.reach = data %>%
  separate(Standing.reach, c('feet', 'inch'), "'", convert = T) %>%
  mutate(reach.cm = (12*feet + inch)*2.54);
  
  data.wingspan = data %>%
  separate(Wingspan, c('feet', 'inch'), "'", convert = T) %>%
  mutate(wingspan.cm = (12*feet + inch)*2.54);
  
# Organize columns
  final.df = cbind(data[, c(1:4)], data.handL$hand.length.cm, data.handW$hand.width.cm, data.height$hight.cm, data.reach$reach.cm, data.wingspan$wingspan.cm )

# find distinct and remove NA
  final.df = final.df %>% distinct(., Player, .keep_all = T);
  final.df = stats::na.omit(final.df);
  
# Add status
  final.df$status = "pro";
  
# Write column names
  colnames(final.df) <- c("collected.from", "season", "player", "position", "hand.length", "hand.width", "height", "reach", "wingspan", "status");
  
# Organize data
  final.df <- final.df[,c(10,3:9)]
  return(final.df)
}


subset.getOne.column <- function(data.X, getOne)
{
  n.rows = dim(data.X)[1];


for(one in getOne)
  {
  data.X[one] = NA;
  }
  
for(i in 1:n.rows)
  {  
  measure.row = data.X[i,];
  for(one in getOne)
    {
    nidx = getIndexOfDataFrameColumns(data.X, one);
    
    myleft = paste0(one,".left");
      lidx = getIndexOfDataFrameColumns(measure.row, myleft);
    myright = paste0(one,".right");
      ridx = getIndexOfDataFrameColumns(measure.row, myleft);

      
      row.m = mean(
            c(as.numeric(unlist(measure.row[lidx])),
            as.numeric(unlist(measure.row[ridx]))),
            na.rm=TRUE);
      
    data.X[i,nidx] =  row.m;
    }
}
  return(data.X[,getOne])
}

prepare.measure.data <- function(measure, getOne)
{
  measure.X = measure;
  
# convert to characters to lower case
  measure.X[, -1:-2] = mutate_if(measure.X[, -1:-2], is.character, tolower)
  
# replace characrers that do not make sense
  measure.X$side[measure.X$side %in% c("n/a") ] = NA
  
# Convert values that do not make sense
  # https://stackoverflow.com/questions/51858966/remove-double-quote-symbol-from-string
  measure.X[,-1:-26] =  data.frame(lapply(measure.X[,-1:-26], FUN = function(x) gsub('["\"]', "", x)))
  
  measure.X$units[measure.X$units %in% c("inches", "Inch", "\"in\"", "inch")] = "in";
  measure.X$units = factor(tolower(measure.X$units));
  
#convert units to cm 
  for (i in 1:nrow(measure))
  {
    if (measure.X$units[i] == "in")
    {
      measure.X[i, 4:26] = conv_unit( measure.X[i, 4:26], from = "inch", to = "cm")
    }
  }
  
  measure.X$units[measure.X$units %in% "in"] = "cm";
  
# clean up covariates
  measure.X$eye[measure.X$eye %in% c("equal") ] = "both";
  measure.X$eye[measure.X$eye %in% c("brown") ] = "left"; 
  
  measure.X$eye_color[measure.X$eye_color %in% c("left")] = "brown";
  
  measure.X$swinging[measure.X$swinging %in% c("let", "leftt")] = "left";
  measure.X$swinging[measure.X$swinging %in% c("rigth")] = "right";
  
  measure.X$gender[measure.X$gender %in% c("f", "F", "Female")] = "female";
  measure.X$gender[measure.X$gender %in% c("m", "M", "Male")] = "male";
  
  factors = c("data_collector", "person_id","side", "swinging", "writing", "eye", "eye_color", "ethnicity", "gender")
  measure.X[, factors] = lapply(X = measure.X[, factors], FUN = as.factor);
  
# Clean up numerical values
  numbers = c("age","quality", "minutes");
  measure.X[, numbers] = lapply(X = measure.X[, numbers], FUN = as.numeric);
  
# Get unique values
  measure.X = measure.X %>% distinct(., person_id, .keep_all = T)
  #measure.X = measure.X[unique(measure.X$person_id), ]
  
# Clean up some biologically improbable values
  measure.X = measure.X[!(measure.X$height.NA == 36 | measure.X$height.NA >= 250), ]

  # Finish
  return(measure.X)
  
}

prepare.measure.for.nba.comparison <- function(measure.X, zmin, zmax)
{
    measure.X$position = "none"
    measure.X$status = "avg"
  
  # columns of interest
  measure.items <- c(4, 7:10, 15:17,32, 38:39);
  
  # NBA players must be older than 19
  measure.Xt <- measure.X[!measure.X$age %in% c(1:19), measure.items];
  measure.Xt <- measure.Xt[!is.na(measure.Xt$height.NA), ];
  
  # Get one measurement 
  measure.getOne <- subset.getOne.column(measure.X, getOne = getOne);
  
  # make dataframe similar to nba
  measure.X <- cbind( measure.X[c(2,38:39)], measure.getOne[1:2],measure.X[4], measure.getOne[3], measure.X[17]);
  
  colnames(measure.X) <- c( "player", "position", "status","hand.length", "hand.width", "height", "reach", "wingspan");
  measure.X = measure.X[, c(3, 1:2, 4:8)]
  
  # Remove abnormal outliers via Z score
  measure.X = measure.X[measure.X$height < (measure.X$reach), ];
  outliers = lapply(measure.X[4:8], humanVerseWSU::findOutliersUsingZscores, zmin = zmin, zmax = zmax);
  
  measure.X = measure.X[!measure.X$hand.length %in% outliers$hand.length$df$value, ];
  measure.X = measure.X[!measure.X$hand.width %in% outliers$hand.width$df$value, ];
  measure.X = measure.X[!measure.X$height %in% outliers$height$df$value, ];
  measure.X = measure.X[!measure.X$reach %in% outliers$reach$df$value, ];
  measure.X = measure.X[!measure.X$wingspan %in% outliers$wingspan$df$value, ];
  measure.X = na.omit(measure.X)
  return(measure.X)
}

cluster.measure.nba.features <- function(measure.df, nba.df, method){
  measure.Xs = scale(measure.df[, -1:-3]);
colnames(measure.Xs) = c("avg.hand.length", "avg.hand.width", "avg.height", "avg.reach", "avg.wingspan");
measure.Xts = t(measure.Xs);
rownames(measure.Xts) = colnames(measure.Xs);

nba.Xs = scale(nba.df[-c(181:193),-1:-3]);
colnames(nba.Xs) = c("pro.hand.length", "pro.hand.width", "pro.height", "pro.reach", "pro.wingspan");


measure.nba = cbind(measure.Xs, nba.Xs)

measure.nba.Xts = t(measure.nba);
rownames(measure.nba.Xts) = colnames(measure.nba);


X.hclust = hclust(dist(measure.nba.Xts), method = method);

return(X.hclust)
}

scale.measure.nba.2.factor <- function(measure.df, nba.df, factor){

# compare body measurements to factor
# 
measure.factor.Xs <- na.omit(measure.df) %>%
  mutate(across(c(hand.length:wingspan), ~  round(./measure.df$factor, digits = 2)));

# Compare body measurements to factor
# 
nba.factor.Xs <- na.omit(nba.df) %>%
  mutate(across(c(hand.length:wingspan), ~  round(./nba.df$factor, digits = 2)));

# merge data
# 
factor.prop.bind.Xs <- rbind(nba.factor.Xs, measure.factor.Xs);


factor.prop.merge.Xs <- factor.prop.bind.Xs %>% select(-factor.prop.bind.Xs$factor) %>%
  melt(.,c(1:3));

return(factor.prop.merge.Xs)
}

