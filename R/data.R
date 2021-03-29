#' Tehran housing data
#'
#' A dataset containing Tehran housing data. The data set has 372 observations.
#' There are 26 explanatory variables at baseline, including 7 project physical and financial
#' features (V2-V8) and 19 economic variables and indices (V11-V29).
#' The outcome (V9) is the sales price of a real estate single-family
#' residential apartment.
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Residential+Building+Data+Set}
#' @format
#' \describe{
#'  \item{V9}{Actual sales price}
#'  \item{V2}{Total floor area of the building}
#'  \item{V3}{Lot area}
#'  \item{V4}{Total Preliminary estimated construction cost based on the prices at the beginning of the project}
#'  \item{V5}{Preliminary estimated construction cost based on the prices at the beginning of the project}
#'  \item{V6}{Equivalent preliminary estimated construction cost based on the prices at the beginning of the project in a selected base year}
#'  \item{V7}{Duration of construction}
#'  \item{V8}{Price of the unit at the beginning of the project per square meter}
#'  \item{V11}{The number of building permits issued}
#'  \item{V12}{Building services index for preselected base year}
#'  \item{V13}{Wholesale price index of building materials for the base year}
#'  \item{V14}{Total floor areas of building permits issued by the city/municipality}
#'  \item{V15}{Cumulative liquidity}
#'  \item{V16}{Private sector investment in new buildings}
#'  \item{V17}{Land price index for the base year}
#'  \item{V18}{The number of loans extended by banks in a time resolution}
#'  \item{V19}{The amount of loans extended by banks in a time resolution}
#'  \item{V20}{The interest rate for loan in a time resolution}
#'  \item{V21}{The average construction cost by private sector at the completion of construction}
#'  \item{V22}{The average cost of buildings by private sector at the beginning of construction}
#'  \item{V23}{Official exchange rate with respect to dollars}
#'  \item{V24}{Nonofficial (street market) exchange rate with respect to dollars}
#'  \item{V25}{Consumer price index (CPI) in the base year}
#'  \item{V26}{CPI of housing, water, fuel & power in the base year}
#'  \item{V27}{Stock market index}
#'  \item{V28}{Population of the city}
#'  \item{V29}{Gold price per ounce}
#' }

"t.housing"


#' Spine data
#'
#' Lower back pain can be caused by a variety of problems with any parts of the complex,
#' interconnected network of spinal muscles, nerves, bones, discs or tendons
#' in the lumbar spine. This dataset contains 12 biomechanical attributes from
#' 310 patients, of whom 100 are normal and 210 are abnormal (Disk Hernia or
#' Spondylolisthesis). The goal is to differentiate the normal patients from the
#' abnormal using those 12 variables.
#'
#' @source \url{http://archive.ics.uci.edu/ml/datasets/vertebral+column}
#' @format
#' \describe{
#'  \item{pelvic_incidence}{pelvic incidence}
#'  \item{pelvic_tilt}{pelvic tilt}
#'  \item{lumbar_lordosis_angle}{lumbar lordosis angle}
#'  \item{sacral_slope}{sacral slope}
#'  \item{pelvic_radius}{pelvic radius}
#'  \item{degree_spondylolisthesis}{degree of spondylolisthesis}
#'  \item{pelvic_slope}{pelvic slope}
#'  \item{direct_tilt}{direct tilt}
#'  \item{thoracic_slope}{thoracic slope }
#'  \item{cervical_tilt}{cervical tilt}
#'  \item{sacrum_angle}{sacrum angle}
#'  \item{scoliosis_slope}{scoliosis slope}
#'  \item{outcome}{1 is abnormal (Disk Hernia or Spondylolisthesis) and 0 is normal}
#' }

"spine"
