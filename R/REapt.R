#' Reparto equitativo entre os apartamentos
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do Reparto Equitativo. Emprega como unidade de reparto os apartamentos.
#' @param custo O custo total da instalación do ascensor
#' @param napt O número de apartamentos que ten o edificio
#' @return A cantidade que lle corresponde pagar a cada apartamento
#' @export
#' @examples
#' REapt(140,7)
#' REapt(120,4)
REapt<-function(custo,napt){
  resultado=custo/napt
  return(resultado)
}
