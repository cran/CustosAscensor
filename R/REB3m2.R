#' Reparto entre os metros cadrados en base á regra REB3
#'
#' Calcula a cantidade a pagar do custo do ascensor por un apartamento en base á regra REB3. Emprega como unidade de reparto os metros cadrados e como unións a priori os andares.
#' @param andar O andar no que se atopa o apartamento
#' @param m2and O número de metros cadrados do andar no que se atopa
#' @param cbaixo O custo correspondente aos traballos feitos no baixo
#' @param cand O custo correspondente aos traballos de cada andar adicional
#' @param nand O número de andares que ten o edificio
#' @param m2apt O número de metros cadrados que ten o apartamento
#' @param nm21 O número de metros cadrados que ten o andar 1
#' @param nm22 O número de metros cadrados que ten o andar 2
#' @param nm23 O número de metros cadrados que ten o andar 3
#' @param nm24 O número de metros cadrados que ten o andar 4
#' @param nm25 O número de metros cadrados que ten o andar 5
#' @param nm26 O número de metros cadrados que ten o andar 6
#' @param nm27 O número de metros cadrados que ten o andar 7
#' @param nm28 O número de metros cadrados que ten o andar 8
#' @param nm29 O número de metros cadrados que ten o andar 9
#' @return A cantidade que lle corresponde pagar ao apartamento en cuestión
#' @export
#' @examples
#' REB3m2(3,140,60,20,4,40,150,150,140,150)
#' REB3m2(1,150,60,20,4,60,150,150,140,150)
REB3m2<-function(andar,m2and,cbaixo,cand,nand,m2apt,nm21=0,nm22=0,nm23=0,nm24=0,nm25=0,nm26=0,nm27=0,nm28=0,nm29=0){
  if(andar<=nand)
    if(nand<9){
      custo = cbaixo+cand*nand
      custop = cbaixo+cand*andar
      nm2 = nm21+nm22+nm23+nm24+nm25+nm26+nm27+nm28+nm29
      sumci = ((cbaixo+cand)*nm21)+((cbaixo+2*cand)*nm22)+((cbaixo+3*cand)*nm23)+((cbaixo+4*cand)*nm24)+((cbaixo+5*cand)*nm25)+((cbaixo+6*cand)*nm26)+((cbaixo+7*cand)*nm27)+((cbaixo+8*cand)*nm28)+((cbaixo+9*cand)*nm29)
      resultado=m2apt*(custop+((custo-sumci)/(m2and*nand)))
      return(resultado)
    }
  else{
    print("nand non debe superar 9")
  }
  else{
    print("O nivel do andar non pode ser maior que nand")
  }
}
