internalRanking <- function(actor.movies, by.year){
  
  actor.movies = standardizeDollarsInDataFrame(actor.movies, by.year, "millions", "year", "millions2000");
  
  # adjust for inflation ...
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "millions2000", "DESC");  ## NA's go to end ...
  actor.movies$rank.money = 1:nrow(actor.movies);
  
  
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "minutes", "DESC");
  actor.movies$rank.minutes = 1:nrow(actor.movies);
  
  
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "ratings", "DESC");
  actor.movies$rank.ratings = 1:nrow(actor.movies);
  
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "metacritic", "DESC");
  actor.movies$rank.metacritic = 1:nrow(actor.movies);
  
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "votes", "DESC");
  actor.movies$rank.votes = 1:nrow(actor.movies);
  
  actor.movies = sortDataFrameByNumericColumns(actor.movies, "year", "ASC");
  actor.movies$rank.year = 1:nrow(actor.movies);
  
  
  return(actor.movies);
  
}

#######################################

predictGenderFromBio = function(bio, gender )
{
  bio = gsub("(","",bio,fixed=TRUE);
  bio = gsub(")","",bio,fixed=TRUE);
  bio = gsub('"',"",bio,fixed=TRUE);
  bio = gsub(","," ",bio,fixed=TRUE);
  
  words = getRawWords(bio); # from nlp, converts to lower
  
  mycounts = list("male" = 0, "female" = 0);
  tc = 0;
  gc = 2;
  for(i in 1:gc)
  {
    key = names(gender)[i]; 
    for(j in 1:length(gender[[key]]))
    {
      mc = sum(words == gender[[key]][j]);  
      mycounts[[key]] = mc + mycounts[[key]];
      tc = mc + tc;
    }
  }
  # mycounts;
  # we could return a ratio ... but a decision for this example would likely be best ...
  if(tc == 0) { return (NA); } # no data ...
  result = "male"; # default in hollywood?
  if(mycounts$female > mycounts$male) { result = "female"; }
  result;
}

#######################################

mergeMovieDetails <- function(actor.movies, imdb.data, by){
  df = merge(actor.movies, imdb.data$movies.df$info, by=by);
  return(df);
}

#######################################

subsetIMDB.data = function(imdbData, least.year, highest.year){
  df = subsetDataFrame(imdbData$movies$popular50, "year", ">=", least.year);
  df = subsetDataFrame(df, "year", "<", highest.year);
  return(df);
}

#######################################

RankingMovies <- function(subsetIMDB.data){
  
  
  df.cast = merge(df, imdb.data$movies.df$cast, by="ttid");
  
  network = df.cast;
  n.ttid = length(unique(network$ttid));
  n.nmid = length(unique(network$nmid));
  
  my.ttids = sort( unique(network$ttid) );
  my.nmids = sort( unique(network$nmid) );
  
  AM = matrix(0, nrow=n.nmid, ncol=n.ttid);
  rownames(AM) = my.nmids;
  colnames(AM) = my.ttids;
  
  nrow = nrow(network);
  
  for(i in 1:nrow)
  {
    if(i %% 2500 == 1) { print(i); flush.console();}
    row = network[i,];
    ttid = row$ttid;
    nmid = row$nmid;
    
    r = which(my.nmids == nmid);
    c = which(my.ttids == ttid);
    AM[r,c] = 1;
  }
  
  
  MM = t(AM) %*% (AM);
  
  # Augment the matrix
  
  rownames(MM) = colnames(MM) = my.ttids;
  n.ttids = length(my.ttids);
  
  # run this augment only once ...
  MM=rbind(MM,1);
  MM=cbind(MM,1);
  MM[(n.ttids+1),(n.ttids+1)] = 0;
  diag(MM) = 0;
  
  round(MM[((n.ttids+1-5):(n.ttids+1)),((n.ttids+1-5):(n.ttids+1))],2);
  
  # Normalize row
  MMn = MM / rowSums(MM);
  round(MMn[((n.ttids+1-5):(n.ttids+1)),((n.ttids+1-5):(n.ttids+1))],2);
  
  # Power computation
  MMnp = matrixPower(MMn, 8);
  
  return(MMnp)
}

#######################################

matrixPower = function(M, times=1)
{
  for(i in 1:times)
  {
    M = M %*% M;
  }
  M;
}

#######################################

castGenderData <- function(actor.movies, gender.list, all.movies.actors.characters, all.actors.info)
  {
  # subset actors working with actor
  actor.movies.1 <- merge(actor.movies[, c(1,5)], all.movies.actors.characters, by = "ttid");
  
  # All actor data under certain criteria - all.actors.info for top 50 movie by year
  cast.movie.details <- merge(actor.movies.1, all.actors.info, by = "nmid");
  
  cast.movie.details[, "gender.bio"] <- sapply(cast.movie.details[, c("bio")], FUN = predictGenderFromBio, gender = gender.list);
  
  cast.movie.details[, "gender.role"] <- sapply(cast.movie.details[, c("roles")], FUN = predictGenderFromBio, gender = gender.list);
  # 16 that do not get classified for will 
  #
  # Ratio of male and females from gender prediction for all movies in top 50 in a given year
  
  # convert gender to numeric
  for (i in 1:nrow(cast.movie.details)[1]){
    if ((cast.movie.details[i, "gender.role"] == "male" | is.na(cast.movie.details[i, "gender.role"])) || (cast.movie.details[i, "gender.bio"] == "male" | is.na(cast.movie.details[i, "gender.bio"]))){cast.movie.details[i, "male"] = 1}
    else{cast.movie.details[i, "male"] = 0}
    if ((cast.movie.details[i, "gender.role"] == "female" | (is.na(cast.movie.details[i, "gender.role"])) | cast.movie.details[i, "gender.bio"] == "female" | is.na(cast.movie.details[i, "gender.bio"]))){cast.movie.details[i, "female"] = 1}
    else{cast.movie.details[i, "female"] = 0}
  }
  
  # aggregate number of cast by movie
  movie.by.gender <- aggregate( cbind(male, female)~ttid,data = cast.movie.details, FUN = sum)
  
  # proportion by gender
  movie.by.gender[, "male.prop"] <- round(movie.by.gender[, "male"] / (movie.by.gender[, "male"]+ movie.by.gender[, "female"]), 2)
  
  movie.by.gender[, "female.prop"] <- round(movie.by.gender[, "female"] / (movie.by.gender[, "male"]+ movie.by.gender[, "female"]), 2)
  
  
  # The movies where a cast member has played in a top 50 movie in any given year
actor.movies.2 <- merge(actor.movies, movie.by.gender, by = "ttid")

return(list(actor.movies = actor.movies.2, cast.data = cast.movie.details, all.cast.members = actor.movies.1))
  
  }

#######################################

castAgeData <- function(actor.cast, actor.nmid, analysis.date)
{
  # age of cast from time of analysis
  # https://masterr.org/r/accurate-calculation-of-years-between-dates/
  
  for (i in 1:nrow(actor.cast$cast.data)[1])
  {
    actor.cast$cast.data[i, "age"] = round(lubridate::time_length(difftime(analysis.date, lubridate::as_date(actor.cast$cast.data[i, "born.when"])), "years"))
  }
  
  
  # scale to actors age
  actor.age <- unique(actor.cast$cast.data[actor.cast$cast.data$nmid %in% actor.nmid, "age" ]);
  
  actor.cast$cast.data[, "age.s"] <- round( actor.cast$cast.data[, "age"]/actor.age,  2);
  return(actor.cast)
}
#######################################

castOriginData <- function(actor.cast)
{
  for (i in 1:nrow(actor.cast$cast.data)[1]) {
  # https://stackoverflow.com/questions/42943533/r-get-last-element-from-str-split
    actor.cast$cast.data[i,"country"] = gsub('\\[.*?\\]', "", actor.cast$cast.data[i,"born.where"], perl = T);
    actor.cast$cast.data[i,"country"] = gsub("^.*,", "", actor.cast$cast.data[i,"country"]);
    actor.cast$cast.data[i,"country"] = gsub(" ", "", actor.cast$cast.data[i,"country"]);

    # actor.cast$cast.data[i,"country"] = gsub("[", "", actor.cast$cast.data[i,"country"], fixed = T);
  }
  actor.cast$cast.data[,"values"] = 1;
  
  actor.cast$cast.data %>%
    select(ttid, country, values) %>%
    distinct(ttid, country, .keep_all = T) %>%
    group_by(ttid) %>%
    summarise(countries.n = sum(values)) -> cast.by.country;
  
  actor.cast$cast.data %>%
    pivot_wider(names_from = country, values_from = values) %>%
    group_by(ttid) %>%
    summarise_if(is.numeric, sum, na.rm = T) %>%
    select(-c(actor.rank:age.s)) -> cast.by.countries;
  
  actor.cast$actor.movies <- merge(actor.cast$actor.movies, cast.by.country, by="ttid");
  actor.cast$actor.movies <- merge(actor.cast$actor.movies, cast.by.countries, by="ttid");
  return(actor.cast)
}
#######################################

castHeadlinerData <- function(actor.cast, actors.headliners)
{
  # Total unique actors as headliners... do they repeat in several movies
  # Yes- within movies
  # 209 headlining actors , actors without bio data ~1000... function of starring in a top 50 film,
  # actors with bio data, and appear in at least 15 movies
  headliner.nmid <- data.frame("nmid" = actors.headliners);
  actor.cast$cast.headliner <- merge(actor.cast$cast.data, headliner.nmid, by = "nmid");
  actor.cast$cast.headliner[, "headliners"] <- 1 ;
  
  # aggregate number of headliner by movie
  movie.by.headliner <- aggregate ( headliners~ttid, data = actor.cast$cast.headliner, FUN = sum);
  
  actor.cast$movie.headliner <- merge(actor.cast$actor.movies, movie.by.headliner, by = "ttid");
  
  return(actor.cast)
}
#######################################

genreDiveristyData <- function(actor.cast)
{
  # number of collective genres by movie
  actor.cast$actor.movies %>%
    separate_rows(genre, convert = T) %>%
    mutate(genre.n = 1) %>%
    group_by(ttid) %>%
    summarise(genre.n = sum(genre.n)) -> genre.by.movie;
  # number of genres indepentally by movie
  actor.cast$actor.movies %>%
    select(ttid, genre) %>%
    separate_rows(genre, convert = T)%>%
    mutate(values = 1) %>%
    pivot_wider(names_from = genre, values_from = values, values_fill = 0) -> genres.by.movie;
    
  
  actor.cast$actor.movies = merge(actor.cast$actor.movies, genre.by.movie, by="ttid");
  actor.cast$actor.movies = merge(actor.cast$actor.movies, genres.by.movie, by="ttid");
  return(actor.cast)
}

#######################################
#######################################
#######################################
#######################################



