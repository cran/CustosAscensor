% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShapleyAndar.R
\name{ShapleyAndar}
\alias{ShapleyAndar}
\title{Reparto segundo o valor de Shapley entre os andares}
\usage{
ShapleyAndar(cbaixo, cand, nand)
}
\arguments{
\item{cbaixo}{O custo correspondente aos traballos feitos no baixo}

\item{cand}{O custo correspondente aos traballos de cada andar adicional}

\item{nand}{O número de andares que ten o edificio}
}
\value{
Os distintos andares e a cantidade que lle corresponde pagar a cada un deles
}
\description{
Reparte o custo do ascensor entre os andares en base ao valor de Shapley
}
\examples{
ShapleyAndar(60,20,4)
ShapleyAndar(70,15,5)
}
