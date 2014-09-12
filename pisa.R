##
##
## pisa.R
## Stephen Harrington
## 2014-09-11 
## Extract and analysis of PISA International Test Data
## 'http://pisa2012.acer.edu.au/downloads.php'
##
## 1. Extract code book and use semi-manual process to format
## 2. Create two dataframes, codebook and response.  Merge into questions dataframe
## 3. Perform some rudimentary consistence checks on codebook
##    Determined that questions ending in 'R' are suspect and eliminate from analysis
## 4. Extract raw data from PISA website
## 5. Subset dataframe only on math questions answered in 2012 PISA test
## 6. Consistency and completeness between codebook responses/frequencies and raw data
## 7. Analysis of selected countries for raw scores
## 8. Correlation between finishing test and rankings
## 
##
## To do:  compute normalized scores using Rasch modeling
##  http://en.wikipedia.org/wiki/Rasch_model
##  R package - eRm http://cran.r-project.org/web/packages/eRm/vignettes/eRm.pdf
##
##


setwd('C:/MOOC/edX/1368.1x')


##Codebook for scored cognitive item response data file
url <- 'http://pisa2012.acer.edu.au/downloads/M_cogn_codebook.pdf'
download.file( url, destfile = 'M_cogn_codebook.pdf',method = 'auto', mode = 'w')


##           codebook process
##
## 1. used nitro pdf reader to convert pdf to .txt 
## 2. gawked to get machine readable codebook
##     gawk '{if(NF>2)if($0!~"Number|Created|Organization|Codebook|Category|Columns|(VAR)|Range of Values")print}' codebook0.txt >codebook.txt
## 3. some hand fixing ~15 minutes...
## 4. futzed about using excel
## 5. created two tab text files: codebook and scores merged into questions



## The codebook details the 264 data items in the PISA cognitive dataset; 
## 10 descriptive items (country, schoolid, studentid, etc.)
## 254 test questions for each of the 5 triennial PIS test years 2000-2012
## set year = 0 for the identifying columns
codebook <- read.table( file = 'codebook.txt', quote = "", na.strings = "",
                        sep = "\t", comment.char = "", header = TRUE )

str( codebook )
#'data.frame':  264 obs. of  10 variables:
#$ variable: Factor w/ 264 levels "BOOKID","CNT",..: 2 262 261 4 3 259 260 1 5 6 ...
#$ name    : Factor w/ 264 levels "Book ID","Country",..: 2 262 260 134 133 186 261 1 52 51 ...
#$ category: Factor w/ 4 levels "ID","MATH","READ",..: 1 1 1 1 1 1 1 1 2 2 ...
#$ year    : int  0 0 0 0 0 0 0 0 2012 2012 ...
#$ question: Factor w/ 264 levels "A View with a Room Q1",..: 65 225 223 160 157 198 224 28 16 10 ...
#$ type    : Factor w/ 2 levels "Char","Num": 1 1 1 1 1 1 1 1 1 1 ...
#$ size    : num  3 7 7 1 6 7 5 2 1 1 ...
#$ field   : int  1 2 3 4 5 6 7 8 9 10 ...
#$ start   : int  1 4 11 18 19 25 32 37 39 40 ...
#$ end     : int  3 10 17 18 24 31 36 38 39 40 ...


##
## scores details each test question:
## response values (0-9,r)
## results: 'noCredit','fullCredit','notReached','missing','invalid','N/A'
## frequency of responses
## percent of each response
##
scores  <- read.table( file = 'scores.txt', quote = "", na.strings = "",
                       sep = "\t", comment.char = "", header = TRUE )

str( scores )
#'data.frame':  1613 obs. of  5 variables:
#$ variable : Factor w/ 254 levels "PM00FQ01","PM00GQ01",..: 1 1 1 1 1 2 2 2 2 2 ...
#$ value    : Factor w/ 83 levels "0","00","01",..: 1 7 52 63 83 1 7 52 63 83 ...
#$ result   : Factor w/ 7 levels "Full credit",..: 9 5 8 7 10 9 5 8 7 10 ...
#$ frequency: int  29848 44388 376932 31972 2350 118644 10364 336579 15771 4132 ...
#$ percent  : num  6.15 9.14 77.64 6.59 0.48 ...


##Merge codebook and scores into questions by variable (question ID...) for later use.
questions <- merge( scores, codebook, by = 'variable' )
names( questions )


##
##
## Before extracting raw data, perform consistency checks on codebook/scores
##
##


##      Analyze scores
##
## do all scores add to 100?
## The sum of percent for 40 out of 254 test questions differ from 100
## example: PS527Q01R  75.81
## all test questions that differ from 100 end in "R"
foo  <- tapply( scores$percent, scores$variable, sum )
foo  <- foo[ abs( foo - 100 ) > 0.02 ]
scoresDoNotAddUp  <-  data.frame( variable = names( foo ), sums = as.vector( foo ) )
dim( scoresDoNotAddUp )[ 1 ]



## what are the frequency of each test question?
foo <- tapply( scores$frequency, scores$variable, sum )

## 215 of 254 have a frequency of 485,490
sum( foo == 485490 )

## the remaining 40 are the same as those who's percents do not sum to 100
## historgram of 
rScores <- foo[ foo != 485490 ]
any( is.na( match( names( rScores ), scoresDoNotAddUp$variable ) ) )
hist( rScores, xlim = c( min( foo ), max( foo ) ),
      breaks = 100, xlab = 'testers', 
      main = 'Distribution of Questions\nwith sum(percent) < 100' )


##              Conclusion
## test questions ending in "R" are suspect
## will eliminate from analysis
## investigated frequency of N/As (~70% of all)
## not all questions asked in all years; see below
## each of five years focus on math, reading or science

##remove 'N/A's
xScores <- subset(scores,result!='N/A' & !(variable %in% names(rScores)))
foo<-tapply(xScores$frequency,xScores$variable,sum)
foo<- foo[!is.na(foo)]  ##get rid of factors removed on subset
hist(foo,xlim=c(min(foo),max(foo)),breaks=100,xlab='testers',
     main='Distribution of Test Takers\nBy Question')

##by year???
xScores <- subset(questions,result!='N/A' & year==2012)
foo<-tapply(xScores$frequency,xScores$variable,sum)
foo<- foo[!is.na(foo)]  ##get rid of factors removed on subset
hist(foo,xlim=c(min(foo),max(foo)),breaks=100,xlab='testers',
     main='Distribution of Test Takers\nBy Question')



##
##  Raw data extract
##
##  1. download zip file
##  2. store in temp file
##  3. read zip archive for filename
##  4. unzip to workig directory and delete temp file
##  5. raw text file large - 189M.

url  <-  "http://pisa2012.acer.edu.au/downloads/INT_COG12_DEC03.zip"
temp        <-  tempfile()  ##create a temporary file for the .zip file
download.file( url = url, destfile = temp )
file        <-  unzip( temp, list = TRUE )$Name  ##without unzipping, find the names in the zip archive
dataDate    <-  unzip( temp, list = TRUE )$Date  
dataDate
unzip(  temp )  ## now unzip
unlink( temp )  ## delete zip file in memory



## read fixed-width data set
## this is really slow - 26 minutes!
print(Sys.time())
dat <- read.fwf(
  file   =  file,
  widths =  codebook$size,
  header =  FALSE,
  sep    =  '\t',
  skip   =  0,
  n      = -1, ##  use n=1000 for test run,  n=1000
  nrows  =  485490,  ## this speeds up R file read and improves memory management
  na.strings = "",
  comment.char = "",
  stringsAsFactors = FALSE,
  colClasses = rep( 'character', 264 )
)
print(Sys.time())

## scan and format is much faster, but unimplemented
## fp    <-  file( file, open = 'r' )
## lines <-  readLines( fp )
## close( fp )


##give every field a name from codebook
names( dat ) = codebook$variable


##  2012 - math only 74 questions
##  2000 - all  26 questions 
##  2003 - math/science 38 questions
##  2006 - science only 60 questions
##  2009 - reading only 48 questions
tapply( codebook$variable, codebook$year, length )


## subset for 2012 maths PISA test with valid questions only
ids  <-  as.character( subset( codebook$variable, codebook$year == 0 ) )
Qs   <-  as.character( unique( questions$variable[ questions$year == 2012 ] ) )

##eliminate Questions with an "R" at the end
##only consider test given in 2012 (maths)
Qs     <-  Qs[ substring( Qs, 9, 9 ) != 'R' ]
maths  <-  dat[ , c( ids, Qs ) ]


##
##       Raw data consistency check
##
##  1. Look for question responses not in codebook
##  2. Check the frequency counts summarized in codebook 
##       against the raw data for completeness
## Poorly written R code.

##invalid responses, all 'R' type questions?
##true first time thru, now eliminated in above step.  
## Left code for future use to study other PISA test years

invalid.responses <-  data.frame( question = NULL, response = NULL )
valid.responses   <-  data.frame( question = NULL, response = NULL, frequency = NULL )
for ( question in Qs ) {
  responses <- sort( as.character( unlist( unique( maths[ question ] ) ) ) )
  possible  <- sort( as.character( questions$value[ questions$variable == question ] ) )
  if ( any( !( responses %in% possible ) ) ) {
    j <- match( responses, possible )
    j <- which( is.na( j ) )
    for ( i in j ) {
      invalid.responses <- rbind( invalid.responses,
                                  data.frame( question = question,
                                              response = responses[ i ] ) )
    }
  } else {
    res <- table( maths[ question ] )
    for (i in 1:length( responses ) )
      valid.responses <- rbind( valid.responses,
                                data.frame( question  =  question,
                                            response  =  responses[ i ],
                                            frequency =  res[ responses[ i ] ] )  )
  }
}

q2r <- match( paste( valid.responses$question, valid.responses$response ),
              paste( questions$variable, questions$value ) )

valid.responses$result         <-  questions$result[ q2r ]
valid.responses$reported.freq  <-  questions$frequency[ q2r ]

##All frquencies match between codebook summary and raw data file for 2012 maths test
any( valid.responses$reported.freq != valid.responses$frequency )




##
## Write out maths dataframe for later analysis
## country, school, student and number of each response by student
##

## convert value to result string to normalize result responses to one
## of seven valid results
for ( question in Qs ) {
  foo <-  maths[ , question ]
  j   <-  match( foo, questions$value[ questions$variable == question ] )
  maths[ , question ] <- as.character( questions$result[ questions$variable == question ][ j ] )
}

df <- data.frame(country      = maths$CNT,
                 school       = maths$SCHOOLID,
                 student      = maths$STIDSTD,
                 noCredit     = rowSums( maths[, Qs ] =='No credit'   ),
                 fullCredit   = rowSums( maths[, Qs ] =='Full credit' ),
                 NAs          = rowSums( maths[, Qs ] =='N/A'         ),
                 missing      = rowSums( maths[, Qs ] =='Missing'     ),
                 notReached   = rowSums( maths[, Qs ] =='Not reached' ),
                 invalid      = rowSums( maths[, Qs ] =='Invalid'     ),
                 partialCredit= rowSums( maths[, Qs ] =='Partial credit'))



write.table( df, file = 'maths2012.txt', quote = FALSE, row.names = FALSE, sep = '\t' )



##
##
##           Some analysis
##
## 1. Histograms for the three most likely results; noCredit, fullCredit and notReached
## 2. correlation study of notReached and PISA rankings
##
png('Raw Data Analysis.png',width=600,height=800)

par( mfrow = c( 2, 2 ), oma = c( 0, 0, 4, 0 ) )

## this could be done more generically with a color palette and lookup table
## to add a country, just extend
myColors           <-  c( 'red', 'blue', 'green', 'purple', 'orange' )
names( myColors )  <-  c( 'KOR', 'USA', 'QUC', 'QCN', 'MEX' )
country.names      <-  c( 'Korea', 'USA', 'Mass', 'Shanghai', 'Mexico' )
names( country.names )  <-  names( myColors )

for ( result in c( 'noCredit', 'fullCredit', 'notReached' ) ) {
  for ( country in names( myColors ) ) {
    x    <- df[[ result ]][ df$country == country ]
    if ( sum( x != 0 ) > 1 ) {
      if ( country == names ( myColors ) [ 1 ] ) {
        plot( density( x[ x !=0 ] ), 
              col = myColors[ country ], 
              main  =  result, 
              ylim  =  c( 0, 0.15 ), lwd = 2,
              xlab  =  'Questions', ylab  =  '')
      } else {
        lines( density ( x[ x != 0 ] ), col=myColors [ country ], lwd = 2 )
      }
    } else {
      print(paste('Not 2 values?',country))
    }
  }
  legend( x = 15, y = 0.15,
          legend = as.vector( country.names ),
          text.col = myColors[ names( country.names ) ],
          bty = 'n', cex = 1.0, y.intersp = 0.75 )
}

##Pie chart for relative participation
x    <- tapply( df$student, df$country, length ) 
pie( ( x[ names( myColors ) ] ),
      col    =  as.vector( myColors ),
      labels =  as.vector( country.names ),
      main   =  'Relative # Participants')

mtext( paste( 'PISA Math 2012', 'Selected Countries', sep = '\n' ), 
            outer = TRUE, cex = 1.5, lwd = 2 )

dev.off()



## correlation study
## does notReached % correlate with PISA ranks?

foo  <- subset( df, df$notReached >= 10 )
x    <- tapply( foo$notReached, foo$country, sum ) / tapply( df$student, df$country, length )



##source http://en.wikipedia.org/wiki/PISA_2012
## added in US States, MA, FL, CN and guessed at ranks
## could not find Cyprus in Codebook, so removed
ranks <- read.table( file = 'pisa_math2012.txt', quote = "", na.strings = "",
                     sep = "\t", comment.char = "", header = TRUE )

ranks$notReached <- as.numeric( x[ match( ranks$code, names( x ) ) ] )


png('Scores Correlated with Completing Exam.png',width=600,height=800)

par( mfrow = c( 1, 1 ) )
plot( ranks$score, ranks$notReached,
      main = 'PISA 2012 Maths\nScatter Plot by Country',
      xlab = 'Reported Score', ylab = 'Pct of Questions Not Reached')
text( ranks$score, ranks$notReached, labels = ranks$code, cex = 0.7, pos = 3 )
fit  <- lm( ranks$notReached ~ ranks$score )
abline( fit, lwd = 2, col = 'red' )

r2     <- round( summary( fit )$r.squared, 2 )
correl <- round( cor( ranks$score, ranks$notReached, use = 'pairwise' ), 2 )

text( 550, 0.8, paste('R^2=',r2))
text( 550, 0.7, paste('Correl=',correl))


dev.off()

