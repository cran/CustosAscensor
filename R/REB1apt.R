#' Reparto entre os apartamentos en base á regra REB1
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra REB1. Emprega como unidade de reparto os apartamentos e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @param napt O número de apartamentos que ten o andar no que se atopa
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REB1apt(3,60,20,4,3)
#' REB1apt(1,60,20,4,2)
REB1apt<-function(andar,cbaixo,cand,nand,napt){
  if(andar<=nand)
    if(nand<9){
      custo = cbaixo+cand*nand
      for (i in 1:nand) {
        x=(cbaixo+cand*i)
      }
      for (i in 1:nand) {
        x[i]=(cbaixo+cand*i)
      }
      sum=sum(x)
      resultado=((cbaixo+cand*andar)/napt)+((custo-sum)/nand)/napt
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
