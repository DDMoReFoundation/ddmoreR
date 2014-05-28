\name{getDataObjects}
\alias{getDataObjects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Retrieves MCL data object(s).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Retrieves a data object (item of type "dataobj") from a Model Object Group "mogObject" in R, an MDL local file with extension .mdl or from the DDMoRe Library URL and returns a named list with one item for each data object present in the file. It allows users to edit the information in a data object and create a new data object for use in a modelling workflow. The data object is parsed in order to facilitate editing individual blocks and elements.
}
\usage{
getDataObjects(file="2008ThamJCCR.mdl")
getDataObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
getDataObjects(mogObject=myMOG)
getDataObjects(file="2008ThamJCCR.mdl",name="tumour_size_dat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{file}{
%%     ~~Describe \code{x} here~~
File path to the .mdl file containing the data object.

Alternatively, file can be a complete URL. (For the supported URL schemes, see the 'URLs' section of the help for \code{\link{url}}.)
}
\item{mogObject}{
%%     ~~Describe \code{x} here~~
If a Model Object Group (MOG) is already defined via TEL, then getDataObjects will extract the data object from the MOG.
}
\item{name}{
%%     ~~Describe \code{x} here~~
Specifies the data object item, by name, to be retrieved by getDataObjects. If multiple data objects exist in the .mdl file then using the name argument helps users target a specific data object.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
As defined by MCL, the data object will contain a DATA_INPUT_VARIABLES block, and a SOURCE block pointing to the filepath for the file containing the data or having an INLINE sub-block containing the data. Optionally, the data object may also have a DATA_DERIVED_VARIABLES block, or a DESIGN block.

getDataObjects creates a named list (using the MCL data object names) with one item for each data object present in the .mdl file or MOG. Within the list, each data object is represented as a vector of character strings - the "raw" MCL code - and also a parsed list where data object block attributes are interpreted and available for editing by the user (analogous to RNMImport importNM functionality).

NOTE: getDataObjects only retrieves the MCL code, it does not read the data file. A "read" method may be applied to the resulting object which interprets the FILE and HEADER information and reads the specified data file into a data frame. See "read" method description below.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a named list of the data object(s), one item per data object within the .mdl file or MOG, containing the raw MCL code as vectors of character strings, and parsed HEADER, FILE blocks.
}
\references{
%% ~put references to the literature/eb site here ~
}
\author{
%%  ~~who you are~~
Mike K Smith
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read}}
}
\examples{
ThamDataObject<-getDataObjects("2008ThamJCCR.mdl") 
tumourSizeDataObject<-getDataObjects("2008ThamJCCR.mdl",
  				name=" tumour_size_dat")

ThamDataObject[[1]] ## first data object within ThamDataObject

## Using data object name from MCL code
tumourSizeDataObject<-ThamDataObject$tumour_size_dat

tumourSizeData<-read(tumourSizeDataObject) 
## Returns data.frame with names from HEADER 
## and any derived variables calculated based on DERIVED_VARIABLES

tumourSizeData$lnDV<-log(tumourSizeData$DV)

## Using awk to change data Object text lines prior to parsing.
ThamDataObject2<- getDataObjects(awk("myawkfile.awk", ThamDataObject$rawMCL))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
