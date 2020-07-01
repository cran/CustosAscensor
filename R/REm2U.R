#' Reparto equitativo entre os metros cadrados con unións a priori
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do Reparto Equitativo. Emprega como unidade de reparto os metros cadrados e como unión a priori os andares.
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @param m2and O número de metros cadrados que ten o andar no que se atopa
#' @param m2apt O número de metros cadrados que ten apartamento
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REm2U(140,4,140,40)
#' REm2U(140,4,150,60)
REm2U<-function(custo,nand,m2and,m2apt){
  resultado=m2apt*(custo/(nand*m2and))
  return(resultado)
}
