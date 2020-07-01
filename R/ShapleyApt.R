#' Reparto entre os apartamentos en base ao valor de Shapley
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base ao valor de Shapley. Emprega como unidade de reparto os apartamentos e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @param napt O número de apartamentos que ten o andar no que se atopa
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' ShapleyApt(3,60,20,4,3)
#' ShapleyApt(1,60,20,4,2)
ShapleyApt<-function(andar,cbaixo,cand,nand,napt){
  if(andar<=nand)
    if(nand<9){
      x <- seq(1, andar, 1)
      resultado = cbaixo/(nand*napt) + cand*(sum(1/(napt*(nand-(x-1)))))
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
