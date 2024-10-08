% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MEAAnalysisClass.R
\name{MEAnalysis}
\alias{MEAnalysis}
\title{MEAnalysis Class}
\description{
This class processes Microelectrode Array (MEA) data, including
sample assignments, treatment averages, well averages, and electrode averages.
It also provides methods for performing statistical analysis and creating plots.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{raw_df}}{A data frame containing the raw MEA data.}

\item{\code{sample_assignments}}{A data frame containing the sample assignments.}

\item{\code{treatment_averages}}{A data frame containing the treatment averages.}

\item{\code{well_averages}}{A data frame containing the well averages.}

\item{\code{treatment_list}}{A list of unique treatment names.}

\item{\code{electrode_averages}}{A data frame containing the electrode averages.}

\item{\code{metrics}}{A character vector of metrics to be analyzed.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MEAnalysis-new}{\code{MEAnalysis$new()}}
\item \href{#method-MEAnalysis-find_first_occurrence}{\code{MEAnalysis$find_first_occurrence()}}
\item \href{#method-MEAnalysis-subset_by_range}{\code{MEAnalysis$subset_by_range()}}
\item \href{#method-MEAnalysis-perform_t_tests}{\code{MEAnalysis$perform_t_tests()}}
\item \href{#method-MEAnalysis-create_combined_t_test_plot}{\code{MEAnalysis$create_combined_t_test_plot()}}
\item \href{#method-MEAnalysis-treatment_averages_t_test_plot}{\code{MEAnalysis$treatment_averages_t_test_plot()}}
\item \href{#method-MEAnalysis-get_sample_assignments}{\code{MEAnalysis$get_sample_assignments()}}
\item \href{#method-MEAnalysis-get_treatment_averages}{\code{MEAnalysis$get_treatment_averages()}}
\item \href{#method-MEAnalysis-get_well_averages}{\code{MEAnalysis$get_well_averages()}}
\item \href{#method-MEAnalysis-get_treatment_list}{\code{MEAnalysis$get_treatment_list()}}
\item \href{#method-MEAnalysis-get_electrode_averages}{\code{MEAnalysis$get_electrode_averages()}}
\item \href{#method-MEAnalysis-remove_well}{\code{MEAnalysis$remove_well()}}
\item \href{#method-MEAnalysis-clone}{\code{MEAnalysis$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-new"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-new}{}}}
\subsection{Method \code{new()}}{
Initialize an MEAnalysis Object

This method initializes the MEAnalysis class with a specified file path.
It reads and processes the MEA data, including sample assignments, treatment averages,
well averages, and electrode averages.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$new(path_to_file)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path_to_file}}{The path to the file to be processed.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An object of class MEAnalysis.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-find_first_occurrence"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-find_first_occurrence}{}}}
\subsection{Method \code{find_first_occurrence()}}{
Find First Occurrence

This method finds the first occurrence of a specific substring in the data frame.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$find_first_occurrence(df, substring_to_find)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame to search in.}

\item{\code{substring_to_find}}{The substring to search for.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The row number of the first occurrence or NA if not found.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-subset_by_range"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-subset_by_range}{}}}
\subsection{Method \code{subset_by_range()}}{
Subset by Range

This method subsets a data frame based on the specified row range.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$subset_by_range(df, start_row, end_row)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame to subset.}

\item{\code{start_row}}{The starting row number.}

\item{\code{end_row}}{The ending row number.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A subset of the data frame.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-perform_t_tests"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-perform_t_tests}{}}}
\subsection{Method \code{perform_t_tests()}}{
Perform T-Tests

This method performs Welch's t-tests between the control group and other treatments.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$perform_t_tests(df, control_group)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the treatment averages.}

\item{\code{control_group}}{The control group name.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with the t-test results.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-create_combined_t_test_plot"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-create_combined_t_test_plot}{}}}
\subsection{Method \code{create_combined_t_test_plot()}}{
Create Combined T-Test Plot

This method creates a combined t-test plot for specified groups and metrics.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$create_combined_t_test_plot(df, control, groups_to_plot, title)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the treatment averages.}

\item{\code{control}}{The control group name.}

\item{\code{groups_to_plot}}{A vector of group names to include in the plot.}

\item{\code{title}}{The title of the plot (optional).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A ggplot object representing the t-test results.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-treatment_averages_t_test_plot"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-treatment_averages_t_test_plot}{}}}
\subsection{Method \code{treatment_averages_t_test_plot()}}{
Treatment Averages T-Test Plot

This method generates a t-test plot for treatment averages.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$treatment_averages_t_test_plot(
  control,
  groups_to_plot,
  title = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{control}}{The control group name.}

\item{\code{groups_to_plot}}{A vector of group names to include in the plot.}

\item{\code{title}}{The title of the plot (optional).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A ggplot object representing the t-test results for treatment averages.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-get_sample_assignments"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-get_sample_assignments}{}}}
\subsection{Method \code{get_sample_assignments()}}{
Get Sample Assignments

This method extracts the sample assignments from the MEA data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$get_sample_assignments(df)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the raw MEA data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with sample assignments.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-get_treatment_averages"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-get_treatment_averages}{}}}
\subsection{Method \code{get_treatment_averages()}}{
Get Treatment Averages

This method extracts the treatment averages from the MEA data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$get_treatment_averages(df)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the raw MEA data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with treatment averages.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-get_well_averages"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-get_well_averages}{}}}
\subsection{Method \code{get_well_averages()}}{
Get Well Averages

This method extracts the well averages from the MEA data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$get_well_averages(df)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the raw MEA data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with well averages.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-get_treatment_list"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-get_treatment_list}{}}}
\subsection{Method \code{get_treatment_list()}}{
Get Treatment List

This method returns a list of unique treatment names from the sample assignments.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$get_treatment_list(df)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the raw MEA data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector of unique treatment names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-get_electrode_averages"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-get_electrode_averages}{}}}
\subsection{Method \code{get_electrode_averages()}}{
Get Electrode Averages

This method extracts the electrode averages from the MEA data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$get_electrode_averages(df)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{df}}{The data frame containing the raw MEA data.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with electrode averages.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-remove_well"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-remove_well}{}}}
\subsection{Method \code{remove_well()}}{
Remove a well

This method reomves a well's data from a given treatment.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$remove_well(well, treatment)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{well}}{The well to be removed.}

\item{\code{treatment}}{The treatment to remove the well from.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MEAnalysis-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MEAnalysis-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MEAnalysis$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
