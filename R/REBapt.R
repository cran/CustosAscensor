#' Reparto equitativo do beneficio entre os apartamentos
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra do Reparto Equitativo do Beneficio. Emprega como unidade de reparto os apartamentos.
#' @param andar O andar no que se atopa o apartamento
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @param napt1 O número de apartamentos que ten o andar 1
#' @param napt2 O número de apartamentos que ten o andar 2
#' @param napt3 O número de apartamentos que ten o andar 3
#' @param napt4 O número de apartamentos que ten o andar 4
#' @param napt5 O número de apartamentos que ten o andar 5
#' @param napt6 O número de apartamentos que ten o andar 6
#' @param napt7 O número de apartamentos que ten o andar 7
#' @param napt8 O número de apartamentos que ten o andar 8
#' @param napt9 O número de apartamentos que ten o andar 9
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REBapt(3,60,20,4,2,1,3,1)
#' REBapt(1,60,20,4,2,1,3,1)
REBapt<-function(andar,cbaixo,cand,nand,napt1=0,napt2=0,napt3=0,napt4=0,napt5=0,napt6=0,napt7=0,napt8=0,napt9=0){
  if(andar<=nand)
    if(nand<9){
      custo = cbaixo+cand*nand
      napt = napt1+napt2+napt3+napt4+napt5+napt6+napt7+napt8+napt9
      sumci = ((cbaixo+cand)*napt1)+((cbaixo+2*cand)*napt2)+((cbaixo+3*cand)*napt3)+((cbaixo+4*cand)*napt4)+((cbaixo+5*cand)*napt5)+((cbaixo+6*cand)*napt6)+((cbaixo+7*cand)*napt7)+((cbaixo+8*cand)*napt8)+((cbaixo+9*cand)*napt9)
      resultado=(cbaixo+cand*andar)+(custo-sumci)/napt
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
