#' Regra do ascensor entre os andares
#'
#' Reparte o custo do ascensor entre os andares en base á regra do ascensor
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @return Os distintos andares e a cantidade que lle corresponde pagar a cada un deles
#' @export
#' @examples
#' AscensorAndar(140,4)
#' AscensorAndar(120,5)
AscensorAndar<-function(custo,nand){
  suma = nand+(nand*(nand-1))/4
  for (i in 1:nand) {
    resultado = (1+(i-1)*0.5)/suma
  }
  for (i in 1:nand) {
    resultado[i] = (1+(i-1)*0.5)/suma
  }
  return(data.frame(andar=1:nand,custo=resultado*custo))
}
