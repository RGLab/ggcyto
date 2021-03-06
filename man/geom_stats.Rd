% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_stats.R
\name{geom_stats}
\alias{geom_stats}
\title{Add a popluation statistics layer to a ggcyto plot.}
\usage{
geom_stats(
  gate = NULL,
  ...,
  value = NULL,
  type = "percent",
  negated = FALSE,
  adjust = 0.5,
  location = "gate",
  label.padding = unit(0.05, "lines"),
  label.size = 0,
  digits = 3
)
}
\arguments{
\item{gate}{a 'filterList` or character (represent as a population node in GatingSet)
if not supplied, ggcyto then tries to parse the gate from the first geom_gate layer.}

\item{...}{other arguments passed to geom_label layer}

\item{value}{the pre-calculated stats value. when supplied, the stats computing is skipped.}

\item{type}{a vector of strings to specify the stats types. can be any or multiple values of "percent", "count", "gate_name", or "MFI" (MFI is currently not supported yet).}

\item{negated}{whether the gate needs to be negated}

\item{adjust}{see details for \code{\link{stat_position}}}

\item{location}{see details for \code{\link{stat_position}}}

\item{label.padding, label.size}{arguments passed to geom_label layer}

\item{digits}{control the stats format}
}
\value{
a geom_popStats layer
}
\description{
This is a virtual layer and will be instanatiated as geom_label layer within ggycto.+ operator.
}
\details{
So it is dedicated for ggcyto context and thus cannot be added to ggplot object directly.
}
\examples{
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
p
# add gate and stats layer
p + geom_gate("CD4") + geom_stats()

# display gate name
p + geom_gate(c("CD4", "CD8")) + geom_stats(type = "gate_name")
# display gate name and percent
p + geom_gate(c("CD4", "CD8")) + geom_stats(type = c("gate_name", "percent"))
}
