module class_grau1
  implicit none
  external REACAO1, REACAO2, REACAO3
  integer APOIOQT1, APOIOQT2, APOIOQT3 ! Qtd. de apoios de 1,2,3 genero
  integer REACAO1, REACAO2, REACAO3
  integer G ! Grau de Hiperestaticidade
  integer REACA ! Numero de componentes de reacoes de apoio
  integer NBAR ! Numero de barras
  integer NNOS ! Numero de nos (Rótulas)
  integer NANEIS ! Numero de aneis na estrutura
  integer NEQEQ ! Numero de equações de equilibrio
  integer NARTI ! Numero de eq vindas de art. int.
  integer BARART ! Quantidade de barras na articulação
  integer CONTADOR ! Contador de articulações
  integer tipot
  character(len=4) :: X
end module class_grau1

program grau1
  use class_grau1
  implicit none
1 contador = 0
  narti = 0
  Print *, "Defina o tipo da estrutura"
  Print *, "1 - Viga"
  Print *, "2 - Portico Plano"
  Print *, "3 - Portico Espacial"
  Print *, "4 - Trelica Plana"
  Print *, "5 - Trelica Espacial"
  Print *, "6 - Grelha"
  read *, tipot  
  if (tipot .GT. 6) then
      print *, "Numero invalido. Entre com um dos numeros do menu."
      read *
      call system('cls')
      goto 1
  endif
  if (tipot .LT. 1) then
      print *, "Numero invalido. Entre com um dos numeros do menu."
      read *
      call system('cls')
      goto 1
  endif
    
  print *, "Quantos apoios do primeiro genero ha nessa estrutura?"
  read *, APOIOQT1
  print *, "Quantos apoios do segundo genero ha nessa estrutura?"
  read *, APOIOQT2
  print *, "Quantos apoios do terceiro genero ha nessa estrutura?"
  read *, APOIOQT3
  
  select case (tipot)
    case (1)
      REACA = reacao1(APOIOQT1, APOIOQT2, APOIOQT3)
      CALL TIPO
      CALL VIGA
      Print *, "O grau de indeterminacao estatica dessa viga 'e: ", G
    case (2)
      REACA = reacao1(APOIOQT1,APOIOQT2,APOIOQT3)
      CALL TIPO
      CALL pplano
      Print *, "O grau de indeterminacao estatica desse portico plano 'e: ", G
    case (3)
      REACA = reacao3(APOIOQT1,APOIOQT2,APOIOQT3)
      CALL TIPO
      CALL pesp
      Print *, "O grau de indeterminacao estatica desse portico espacial 'e: ", G
    case (4)
      REACA = reacao1(APOIOQT1,APOIOQT2,APOIOQT3)
      !CALL TIPO
      CALL tplano
      Print *, "O grau de indeterminacao estatica dessa trelica plana 'e: ", G
    case (5)
      REACA = reacao3(APOIOQT1,APOIOQT2,APOIOQT3)
      !CALL TIPO
      CALL tesp
      Print *, "O grau de indeterminacao estatica dessa trelica espacial 'e: ", G
    case (6)
      REACA = reacao2(APOIOQT1,APOIOQT2,APOIOQT3)
      CALL TIPO
      CALL grelha
      Print *, "O grau de indeterminacao estatica dessa grelha 'e: ", G
    end select
    
    print *, "Deseja continuar?"
    read *, X
    select case(X)
      case('sim', 'SIM')
        call system('cls')
        goto 1
    end select
  stop
    end
    
  subroutine TIPO
  use class_grau1
    Print *, "Ha articulacoes nessa estrutura?"
    read *, X
3   select case(x)
      CASE ('sim', 'SIM')
        contador = contador + 1
        Print *, "Quantas barras ha tocando essa articulacao"
        Read *, BARART
        BARART = BARART - 1
        NARTI = NARTI + BARART
        Print *,  "Ha outra articulacao?"
        Read *, X
        GOTO 3
    END SELECT
  RETURN
  END

  function reacao1 (c1, c2, c3)
    implicit none
    integer c1, c2, c3, reacao1
    reacao1 = c1 + c2*2 + c3 * 3
  return
  end

  function reacao2 (c1, c2, c3)
    implicit none
    integer c1, c2, c3, reacao2
    reacao2 = c1 + c2 + c3 * 3
    return
  end

  function reacao3 (c1, c2, c3)
    implicit none
    integer c1, c2, c3, reacao3
    reacao3 = c1 + c2*3 + c3 * 6
    return
  end

  subroutine viga
    use class_grau1
    Print *, "Entre com o numero de barras na estrutura"
    Read *, NBAR
    G = (REACA - NBAR * 3) - (NARTI)
  return
  end

  subroutine pplano
    use class_grau1
    Print *, "Entre com o numero de aneis na estrutura"
    Read *, NANEIS
    G = (REACA + NANEIS * 3) - (3 + NARTI)
    return
    end

    subroutine pesp
    use class_grau1
    Print *, "Entre com o numero de aneis na estrutura"
    Read *, NANEIS
    G = (REACA + NANEIS * 6) - (6 + NARTI)
    return
    end

    subroutine tplano
    use class_grau1
    Print *, "Entre com o numero de barras na estrutura"
    Read *, NBAR
    Print *, "Entre com o numero de nos na estrutura"
    Read *, NNOS
    G = (REACA + NBAR) - (NNOS * 2)
    return
    end

    subroutine tesp
    use class_grau1
    Print *, "Entre com o numero de barras na estrutura"
    Read *, NBAR
    Print *, "Entre com o numero de nos na estrutura"
    Read *, NNOS
    G = (REACA + NBAR * 1) - (NNOS * 3)
    return
    end

    subroutine grelha
    use class_grau1
    Print *, "Entre com o numero de aneis na estrutura"
    Read *, NANEIS
    G = (REACA + NANEIS * 3) - (3 + NARTI)
    return
    end
