doSampleVariance <- function(x, method){
  
  if (method == "naive"){
    count <- Sum <- Sum2 <- 0
    x <- as.vector(unlist(x));
    for (i in x)
    {
      count = count + 1;
      Sum = Sum + i;
      Sum2 = Sum2 + (i* i);
    }
    if(count < 2) { return(NULL);} #
    
    variance = (Sum2 - (Sum * Sum) / count) / (count - 1)
    sd = sqrt(variance);
    
    list(Sum, Sum2, variance, sd)
  }
  else # two-pass
  {
    count <- Sum <- Sum2 <- 0;
    x <- as.vector(unlist(x));
    for (i in x)
    {
      count = count + 1;
      Sum = Sum + i;
    }
    if(count <  2) { return(NULL);} #
    mean = (Sum / count);
    for (j in x) # second pass
    {
      Sum2 = Sum2 + ((j - mean) * (j - mean));
    }
    variance = (Sum2 / (count - 1));
    sd = sqrt(variance);
    
    list(Sum, Sum2, variance, sd) 
  }
}

doMode = function(x)
{
  # code adapted from : ^[https://www.r-bloggers.com/computing-the-mode-in-r/]
  result = c();
  table_of_values = table(x)
  table_of_max_values = max(table_of_values)
  if (all(table_of_values == table_of_max_values))
  {
    result = NA
  } 
  else if(is.numeric(x))
  {
    result = as.numeric(names(table_of_values)[table_of_values == table_of_max_values])
  }
  else
  {
    result = names(table_of_values)[table_of_values == table_of_max_values]
  }
  result;
}

doSummary = function(x, method)
{
  x <- as.vector(unlist(x));
  result <- data.frame( 			matrix(ncol = 1,nrow = 11, 
                                  dimnames = list( c("length",
                                                     "NAs",
                                                     "mean",
                                                     "median",
                                                     "mode",
                                                     "sum",
                                                     "sumSq",
                                                     "variance.naive",
                                                     "variance.two-pass",
                                                     "sd_custom",
                                                     "sd" ),
                                                   "") ));
  result["length",] = length(x);
  result["NAs", ] = sum(is.na(x));
  result["mean", ] = mean(x);
  result["median", ] = median(x);
  result["mode", ] = doMode(x);
  result["sum",] = doSampleVariance(x, method = "naive")[[1]];
  result["sumSq", ] = doSampleVariance(x, method = "naive")[[2]];
  result["variance.naive", ] = doSampleVariance(x, method = "naive")[[3]];
  result["variance.two-pass", ] = doSampleVariance(x, method = "two-pass")[[3]];
  result["sd_custom", ] = doSampleVariance(x, method = "naive")[[4]];
  result["sd", ] = sd(x);
  round(result, digits = 3);
}