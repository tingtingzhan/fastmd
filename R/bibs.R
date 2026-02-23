

#' @title \link[utils]{bibentry} of Statistical Methods 
#' 
#' @description
#' \link[utils]{bibentry} of statistical methods in plain R packages, especially **stats**.
#' 
#' @param key,... additional parameters of function \link[utils]{bibentry}
#' 
#' @keywords internal
#' @name bib
#' @export
.fisher22 <- \(key = 'Fisher22', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = c('Ronald', 'Aylmer'), family = 'Fisher'),
    journal = 'Journal of the Royal Statistical Society',
    number = '1',
    pages = '87--94',
    title = 'On the Interpretation of $\\chi^2$ from Contingency Tables, and the Calculation of $p$',
    volume = '85',
    year = '1922',
    doi = '10.2307/2340521'
  )
}


#' @rdname bib
#' @export
.pearson1900 <- \(key = 'Pearson1900', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = 'Karl', family = 'Pearson'),
    title = 'X. On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling',
    journal = 'The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science',
    volume = '50',
    number = '302',
    pages = '157--175',
    year = '1900',
    publisher = 'Taylor & Francis',
    doi = '10.1080/14786440009463897'
  )
}

#' @rdname bib
#' @export
.holm79 <- \(key = 'Holm79', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    url = 'http://www.jstor.org/stable/4615733',
    author = person(given = 'Sture', family = 'Holm'),
    journal = 'Scandinavian Journal of Statistics',
    number = '2',
    pages = '65--70',
    title = 'A Simple Sequentially Rejective Multiple Test Procedure',
    volume = '6',
    year = '1979'
  )  
}


#' @rdname bib
#' @export
.hochberg88 <- \(key = 'Hochberg88', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = person(given = 'Yosef', family = 'Hochberg'),
    title = 'A sharper {B}onferroni procedure for multiple tests of significance',
    journal = 'Biometrika',
    volume = '75',
    number = '4',
    pages = '800-802',
    year = '1988',
    month = '12',
    doi = '10.1093/biomet/75.4.800'
  )
}


#' @rdname bib
#' @export
.hommel88 <- \(key = 'Hommel88', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = person(given = 'Gerhard', family = 'Hommel'),
    title = 'A stagewise rejective multiple test procedure based on a modified Bonferroni test',
    journal = 'Biometrika',
    volume = '75',
    number = '2',
    pages = '383-386',
    year = '1988',
    month = '06',
    doi = '10.1093/biomet/75.2.383'
  )
}


#' @rdname bib
#' @export
.benjamini_hochberg95 <- \(key = 'BenjaminiHochberg95', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = c(
      person(given = 'Yoav', family = 'Benjamini'), 
      person(given = 'Yosef', family = 'Hochberg')
    ),
    title = 'Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple Testing',
    journal = 'Journal of the Royal Statistical Society: Series B (Methodological)',
    volume = '57',
    number = '1',
    pages = '289--300',
    year = '1995',
    month = '12',
    doi = '10.1111/j.2517-6161.1995.tb02031.x'
  )
}

#' @rdname bib
#' @export
.benjamini_yekutieli01 <-\(key = 'BenjaminiYekutieli01', ...) {
  bibentry(
    bibtype = 'Article', key = key, ..., 
    author = c(
      person(given = 'Yoav', family = 'Benjamini'), 
      person(given = 'Daniel', family = 'Yekutieli')
    ),
    title = 'The control of the false discovery rate in multiple testing under dependency',
    volume = '29',
    journal = 'The Annals of Statistics',
    number = '4',
    pages = '1165 -- 1188',
    year = '2001',
    doi = '10.1214/aos/1013699998'
  )
}

#' @rdname bib
#' @export
.lawley_maxwell71 <- \(key = 'LawleyMaxwell71', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    title = 'Factor Analysis as a Statistical Method',
    journal = 'Journal of the Royal Statistical Society. Series D (The Statistician)',
    author = c(
      person(given = c('D.', 'N.'), family = 'Lawley'), 
      person(given = c('A.', 'E.'), family = 'Maxwell')
    ), 
    year = '1962',
    doi = '10.2307/2986915',
    pages = '209--229',
    volume = '12', number = '3',
    publisher = 'Royal Statistical Society, Wiley'
    #SN  - 00390526, 14679884
  )
}


#' @rdname bib
#' @export
.tukey49 <- \(key = 'Tukey49', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = person(given = c('John', 'W.'), family = 'Tukey'),
    journal = 'Biometrics',
    number = '2',
    pages = '99--114',
    title = 'Comparing Individual Means in the Analysis of Variance',
    volume = '5',
    year = '1949',
    doi = '10.2307/3001913'
  )
}