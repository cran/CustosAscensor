#' Reparto equitativo entre os apartamentos con unións a priori
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do Reparto Equitativo. Emprega como unidade de reparto os apartamentos e como unión a priori os andares.
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @param aptand O número de apartamentos que ten o andar no que se atopa
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REaptU(140,4,2)
#' REaptU(140,4,3)
REaptU<-function(custo,nand,aptand){
  resultado=custo/(nand*aptand)
  return(resultado)
}
