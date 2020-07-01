#' Reparto entre os metros cadrados en base ao valor de Shapley
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base ao valor de Shapley. Emprega como unidade de reparto os metros cadrados e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @param m2and O número de metros cadrados que ten o andar no que se atopa
#' @param m2apt O número de metros cadrados que ten apartamento
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' Shapleym2(3,60,20,4,140,40)
#' Shapleym2(1,60,20,4,150,60)
Shapleym2<-function(andar,cbaixo,cand,nand,m2and,m2apt){
  if(andar<=nand)
    if(nand<9){
      x <- seq(1, andar, 1)
      resultado = m2apt*(cbaixo/(nand*m2and) + cand*(sum(1/(m2and*(nand-(x-1))))))
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
