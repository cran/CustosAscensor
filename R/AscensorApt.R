#' Reparto entre os apartamentos en base á regra do ascensor
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do ascensor. Emprega como unidade de reparto os apartamentos e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @param napt O número de apartamentos que ten o andar no que se atopa
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' AscensorApt(3,140,4,3)
#' AscensorApt(1,140,4,2)
AscensorApt<-function(andar,custo,nand,napt){
  if(andar<=nand)
    if(nand<9){
      suma = nand+(nand*(nand-1))/4
      resultado = custo*((1+(andar-1)*0.5)/(suma*napt))
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
