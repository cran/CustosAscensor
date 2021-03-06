% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShapleyApt.R
\name{ShapleyApt}
\alias{ShapleyApt}
\title{Reparto entre os apartamentos en base ao valor de Shapley}
\usage{
ShapleyApt(andar, cbaixo, cand, nand, napt)
}
\arguments{
\item{andar}{O andar no que se atopa o apartamento}

\item{cbaixo}{O custo correspondente aos traballos feitos no baixo}

\item{cand}{O custo correspondente aos traballos de cada andar adicional}

\item{nand}{O número de andares que ten o edificio}

\item{napt}{O número de apartamentos que ten o andar no que se atopa}
}
\value{
A cantidade que lle corresponde pagar ao apartamento en cuestión
}
\description{
Calcula a cantidade a pagar do custo do ascensor por un apartamento en base ao valor de Shapley. Emprega como unidade de reparto os apartamentos e como unións a priori os andares.
}
\examples{
ShapleyApt(3,60,20,4,3)
ShapleyApt(1,60,20,4,2)
}
