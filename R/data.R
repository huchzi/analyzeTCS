#' D15 vectors
#'
#' The vectors in color space of the 15 caps that make up the Panel D15 color vision test.
#'
#' @format A data frame with 16 rows and 4 variables:
#' \describe{
#'   \item{u}{}
#'   \item{v}{}
#'   \item{su}{}
#'   \item{sv}{}
#'   ...
#' }
"D15_vectors"

#' Norm values [dB]
#'
#' The norm values for photoreceptor-specific temporal contrast sensitivity [dB] for a 40-year old subject. These were re-calculated from the data published in TVST: Huchzermeyer et. al. Photoreceptor-Specific Loss of Perifoveal Temporal Contrast Sensitivity in Retinitis Pigmentosa. Transl Vis Sci Technol 2020:9(6):27.
#'
#' @format A data table with 36 rows and 7 variables:
#' \describe{
#'   \item{type}{Photoreceptor type}
#'   \item{frequency}{Temporal frequency [Hz]}
#'   \item{n}{Number of observations}
#'   \item{mean_dB}{Normal sensitivity [dB] for a 40-year-old subject}
#'   \item{sd_dB}{Standard deviation of sensitivities [dB]}
#'   \item{confint95_left}{Lower limit of the 95% confidence interval [dB]}
#'   \item{confint95_right}{Upper limit of the 95% confidence interval [dB]}
#' }
"norm_values"
