#' Reparto segundo o valor de Shapley entre os andares
#'
#' Reparte o custo do ascensor entre os andares en base ao valor de Shapley
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @return Os distintos andares e a cantidade que lle corresponde pagar a cada un deles
#' @export
#' @examples
#' ShapleyAndar(60,20,4)
#' ShapleyAndar(70,15,5)
ShapleyAndar<-function(cbaixo,cand,nand){
  for (i in 1:nand) {
    x <- seq(1, i, 1)
    resultado = cbaixo/nand + cand*(sum(1/(nand-(x-1))))
  }
  for (i in 1:nand) {
    x <- seq(1, i, 1)
    resultado[i] = cbaixo/nand + cand*(sum(1/(nand-(x-1))))
  }
  return(data.frame(andar=1:nand,custo=resultado))
}
