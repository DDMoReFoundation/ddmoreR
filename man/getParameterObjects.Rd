\name{getParameterObjects}
\alias{getParameterObjects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Retrieves MCL Parameter object(s).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Retrieves a parameter object (item of type "parobj") from a Model Object Group "mogObject" in R, an MDL local file with extension .mdl or from the DDMoRe Library URL and returns a named list with one item for each parameter object present in the file. It allows users to edit the information in a parameter object and create a new parameter object for use in a modelling workflow. The parameter object is parsed in order to facilitate editing individual blocks and elements.
}
\usage{
getParameterObjects(file="2008ThamJCCR.mdl")
getParameterObjects(file="http://ddmore.eu/model-repository/model/download/127.17?filename=2008ThamJCCR.mdl")
getParameterObjects(mogObject=myMOG)
getParameterObjects(file="2008ThamJCCR.mdl",name="tumour_size_par")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{file}{
%%     ~~Describe \code{x} here~~
File path to the .mdl file containing the Data object.

Alternatively, file can be a complete URL. (For the supported URL schemes, see the 'URLs' section of the help for \code{\link{url}}.)
}
\item{mogObject}{
%%     ~~Describe \code{x} here~~
If a Model Object Group (MOG) is already defined via TEL, then getParameterObjects will extract the parameter object from the MOG.
}
\item{name}{
%%     ~~Describe \code{x} here~~
Specifies the parameter object item, by name, to be retrieved by getParameterObjects. If multiple parameter objects exist in the .mdl file then using the name argument helps users target a specific parameter object.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
As defined by MCL, the parameter object will contain a STRUCTURAL block, and may contain a VARIABILITY block.

getParameterObjects creates a named list (using the MCL parameter object names) with one item for each parameter object present in the .mdl file or MOG. Within the list, each parameter object is represented as a vector of character strings - the "raw" MCL code - and also a parsed list where parameter object block attributes are interpreted and available for editing by the user (analogous to RNMImport importNM functionality).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a named list of the parameter object(s), one item per parameter object within the .mdl file or MOG, containing the raw MCL code as vectors of character strings, and parsed STRUCTURAL and VARIABILITY blocks.
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
ThamParameterObject<-getParameterObjects("2008ThamJCCR.mdl") 
tumourSizeParameterObject<-getParameterObjects("2008ThamJCCR.mdl",
  				name=" tumour_size_par")

ThamParameterObject[[1]] ## first parameter object within ThamParameterObject

## Using Data object name from MCL code
tumourSizeParameterObject<-ThamParameterObject$tumour_size_par
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
