#' Reparto entre os metros cadrados en base á regra do ascensor
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do ascensor. Emprega como unidade de reparto os metros cadrados e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param custo O custo total da instalación do ascensor
#' @param nand O número de andares que ten o edificio
#' @param m2and O número de metros cadrados que ten o andar no que se atopa
#' @param m2apt O número de metros cadrados que ten apartamento
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' Ascensorm2(3,140,4,140,40)
#' Ascensorm2(1,140,4,150,60)
Ascensorm2<-function(andar,custo,nand,m2and,m2apt){
  if(andar<=nand)
    if(nand<9){
      suma = nand+(nand*(nand-1))/4
      resultado = m2apt*(custo*((1+(andar-1)*0.5)/(suma*m2and)))
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
