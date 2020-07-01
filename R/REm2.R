#' Reparto equitativo entre os metros cadrados
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do Reparto Equitativo. Emprega como unidade de reparto os metros cadrados.
#' @param custo O custo total da instalación do ascensor
#' @param metros O número de metros cadrados que ten o edificio
#' @param m2apt O número de metros cadrados que ten o apartamento
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REm2(140,590,40)
#' REm2(140,590,60)
REm2<-function(custo,metros,m2apt){
  resultado=m2apt*(custo/metros)
  return(resultado)
}
