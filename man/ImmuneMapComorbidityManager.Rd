% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manager_ImmuneMapComorbidity.R
\name{ImmuneMapComorbidityManager}
\alias{ImmuneMapComorbidityManager}
\title{R6 Class to manage Comorbidity Analysis for Immune Maps data}
\description{
Subclass of ImmuneMapFeatureAnalysisManager R6 Class to enable Immune Map specific analysis.
}
\section{Super classes}{
\code{\link[TrisomExploreR:FeatureAnalysisManager]{TrisomExploreR::FeatureAnalysisManager}} -> \code{\link[TrisomExploreR:ImmuneMapFeatureAnalysisManager]{TrisomExploreR::ImmuneMapFeatureAnalysisManager}} -> \code{ImmuneMapComorbidityManager}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{analysisMetadata}}{-}

\item{\code{analysisChoices}}{-}

\item{\code{Analysis}}{-}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ImmuneMapComorbidityManager-new}{\code{ImmuneMapComorbidityManager$new()}}
\item \href{#method-ImmuneMapComorbidityManager-getBaseData}{\code{ImmuneMapComorbidityManager$getBaseData()}}
\item \href{#method-ImmuneMapComorbidityManager-clone}{\code{ImmuneMapComorbidityManager$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="addGSEAInputClass"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-addGSEAInputClass'><code>TrisomExploreR::FeatureAnalysisManager$addGSEAInputClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="addInputSpecialClass"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-addInputSpecialClass'><code>TrisomExploreR::FeatureAnalysisManager$addInputSpecialClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="annotate_volcano_point"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-annotate_volcano_point'><code>TrisomExploreR::FeatureAnalysisManager$annotate_volcano_point()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getAnalyteData"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getAnalyteData'><code>TrisomExploreR::FeatureAnalysisManager$getAnalyteData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getAnalytePlot"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getAnalytePlot'><code>TrisomExploreR::FeatureAnalysisManager$getAnalytePlot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getConditionTree"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getConditionTree'><code>TrisomExploreR::FeatureAnalysisManager$getConditionTree()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getCovariateChoices"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getCovariateChoices'><code>TrisomExploreR::FeatureAnalysisManager$getCovariateChoices()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getDisabledInputClass"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getDisabledInputClass'><code>TrisomExploreR::FeatureAnalysisManager$getDisabledInputClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getFormattedAnalyteSummaryData"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getFormattedAnalyteSummaryData'><code>TrisomExploreR::FeatureAnalysisManager$getFormattedAnalyteSummaryData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getFormattedVolcanoSummaryData"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getFormattedVolcanoSummaryData'><code>TrisomExploreR::FeatureAnalysisManager$getFormattedVolcanoSummaryData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getGSEAData"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getGSEAData'><code>TrisomExploreR::FeatureAnalysisManager$getGSEAData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getGSEAEnrichmentPlot"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getGSEAEnrichmentPlot'><code>TrisomExploreR::FeatureAnalysisManager$getGSEAEnrichmentPlot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getGSEAPathwayData"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getGSEAPathwayData'><code>TrisomExploreR::FeatureAnalysisManager$getGSEAPathwayData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getGSEAPlot"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getGSEAPlot'><code>TrisomExploreR::FeatureAnalysisManager$getGSEAPlot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getGetDataButtonClass"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getGetDataButtonClass'><code>TrisomExploreR::FeatureAnalysisManager$getGetDataButtonClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getHiddenInputClass"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getHiddenInputClass'><code>TrisomExploreR::FeatureAnalysisManager$getHiddenInputClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getSelectedConditionList"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getSelectedConditionList'><code>TrisomExploreR::FeatureAnalysisManager$getSelectedConditionList()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getStudies"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getStudies'><code>TrisomExploreR::FeatureAnalysisManager$getStudies()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getStudyLabel"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getStudyLabel'><code>TrisomExploreR::FeatureAnalysisManager$getStudyLabel()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getVolcanoMultiSelectText"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getVolcanoMultiSelectText'><code>TrisomExploreR::FeatureAnalysisManager$getVolcanoMultiSelectText()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="getVolcanoPlot"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-getVolcanoPlot'><code>TrisomExploreR::FeatureAnalysisManager$getVolcanoPlot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="get_analyte_plot_method"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-get_analyte_plot_method'><code>TrisomExploreR::FeatureAnalysisManager$get_analyte_plot_method()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="get_analyte_plot_title"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-get_analyte_plot_title'><code>TrisomExploreR::FeatureAnalysisManager$get_analyte_plot_title()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="get_analyte_plot_x_axis_label"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-get_analyte_plot_x_axis_label'><code>TrisomExploreR::FeatureAnalysisManager$get_analyte_plot_x_axis_label()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="updateAnalyteAttributes"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-updateAnalyteAttributes'><code>TrisomExploreR::FeatureAnalysisManager$updateAnalyteAttributes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="FeatureAnalysisManager" data-id="validate_volcano_plot"><a href='../../TrisomExploreR/html/FeatureAnalysisManager.html#method-FeatureAnalysisManager-validate_volcano_plot'><code>TrisomExploreR::FeatureAnalysisManager$validate_volcano_plot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="ImmuneMapFeatureAnalysisManager" data-id="getAnalysisChoices"><a href='../../TrisomExploreR/html/ImmuneMapFeatureAnalysisManager.html#method-ImmuneMapFeatureAnalysisManager-getAnalysisChoices'><code>TrisomExploreR::ImmuneMapFeatureAnalysisManager$getAnalysisChoices()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="ImmuneMapFeatureAnalysisManager" data-id="getAnalysisInputChoices"><a href='../../TrisomExploreR/html/ImmuneMapFeatureAnalysisManager.html#method-ImmuneMapFeatureAnalysisManager-getAnalysisInputChoices'><code>TrisomExploreR::ImmuneMapFeatureAnalysisManager$getAnalysisInputChoices()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="ImmuneMapFeatureAnalysisManager" data-id="getCellTypes"><a href='../../TrisomExploreR/html/ImmuneMapFeatureAnalysisManager.html#method-ImmuneMapFeatureAnalysisManager-getCellTypes'><code>TrisomExploreR::ImmuneMapFeatureAnalysisManager$getCellTypes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="ImmuneMapFeatureAnalysisManager" data-id="getKaryotypeChoices"><a href='../../TrisomExploreR/html/ImmuneMapFeatureAnalysisManager.html#method-ImmuneMapFeatureAnalysisManager-getKaryotypeChoices'><code>TrisomExploreR::ImmuneMapFeatureAnalysisManager$getKaryotypeChoices()</code></a></span></li>
<li><span class="pkg-link" data-pkg="TrisomExploreR" data-topic="ImmuneMapFeatureAnalysisManager" data-id="getVolcanoSummaryData"><a href='../../TrisomExploreR/html/ImmuneMapFeatureAnalysisManager.html#method-ImmuneMapFeatureAnalysisManager-getVolcanoSummaryData'><code>TrisomExploreR::ImmuneMapFeatureAnalysisManager$getVolcanoSummaryData()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImmuneMapComorbidityManager-new"></a>}}
\if{latex}{\out{\hypertarget{method-ImmuneMapComorbidityManager-new}{}}}
\subsection{Method \code{new()}}{
Create a new instance of a FeatureAnalysisManager
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImmuneMapComorbidityManager$new(
  applicationName,
  id,
  namespace_config,
  remoteDB,
  localDB
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{applicationName}}{string - name of application}

\item{\code{id}}{string - namespace for this instance}

\item{\code{namespace_config}}{list - configurations for this namespace}

\item{\code{remoteDB}}{R6 class - query manager for remote database queries}

\item{\code{localDB}}{R6 class - query manager for local database queries}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImmuneMapComorbidityManager-getBaseData"></a>}}
\if{latex}{\out{\hypertarget{method-ImmuneMapComorbidityManager-getBaseData}{}}}
\subsection{Method \code{getBaseData()}}{
Get / set sample level data with filers applied
Overrides super method with data model specific to Immune Maps data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImmuneMapComorbidityManager$getBaseData()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImmuneMapComorbidityManager-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ImmuneMapComorbidityManager-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImmuneMapComorbidityManager$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
