#' Reparto equitativo entre os andares
#'
#' Reparte o custo do ascensor entre os andares en base á regra do Reparto Equitativo
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @return Os distintos andares e a cantidade que lle corresponde pagar a cada un deles
#' @export
#' @examples
#' REandar(140,4)
#' REandar(120,5)
REandar<-function(custo,nand){
  resultado = custo/nand
  return(data.frame(andar=1:nand,custo=resultado))
}
