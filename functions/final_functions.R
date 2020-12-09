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



