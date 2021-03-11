      ******************************************************************
      * Author: Diogo Marques && Arianna Cicero
      * Date: 18/01/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Trabalho2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *Parte de clientes
           select optional clientes
              assign to "clientes12.dat"
              organization is indexed
              access mode is dynamic
              record key is reg-clientes-id
              alternate key is reg-clientes-nome with duplicates
              file status is fs-clientes.
           select ord-clientes
              assign to "SORTclientes.dat"
              sort status is ss-clientes.
           select impressao
              assign to "clientes.txt"
              organization is line sequential
              access mode is sequential
              file status is impressao.
      *parte produtos
           select optional produtos
              assign to "produtos12.dat"
              organization is indexed
              access mode is dynamic
              record key is reg-produtos-id
              alternate key is reg-produtos-nome with duplicates
              file status is fs-produtos.
           select ord-produtos
              assign to "SORTprodutos.dat"
              sort status is ss-produtos.
      *parte faturas
           select optional faturas
              assign to "faturas12.dat"
              organization is indexed
              access mode is dynamic
              record key is reg-faturas-id
              alternate key is reg-faturas-id-cliente with duplicates
              file status is fs-faturas.
           select ord-faturas
              assign to "SORTfaturas.dat"
              sort status is ss-faturas.
      *parte do index/ids automaticos
           select OPTIONAL arquivo-index_ids
           assign to "index_ids.dat"
           organization is SEQUENTIAL.
      *     file status is fs-ids.
       DATA DIVISION.
       FILE SECTION.
      *parte de clientes
       FD  clientes.
       01  reg-clientes.
           10 reg-clientes-id pic 99.
           10 reg-clientes-nome pic x(50).
           10 reg-clientes-data-nasc.
              20 reg-clientes-data-ano pic 9(004).
              20 reg-clientes-data-mes pic 9(002).
              20 reg-clientes-data-dia pic 9(002).
           10 reg-clientes-morada pic x(50).
           10 reg-clientes-telefone pic 9(12).
           10 reg-clientes-nif pic 9(9).
       SD  ord-clientes.
       01  sort-reg-clientes.
         10 sort-clientes-id pic 99.
         10 sort-clientes-nome pic x(50).
         10 sort-clientes-data-nasc.
            20 clientes-data-ano pic 9(004).
            20 clientes-data-mes pic 9(002).
            20 clientes-data-dia pic 9(002).
         10 sort-clientes-morada pic x(50).
         10 sort-clientes-telefone pic 9(12).
         10 sort-clientes-nif pic 9(9).
      *parte produtos
       FD  produtos.
       01  reg-produtos.
           10 reg-produtos-id pic 99.
           10 reg-produtos-nome pic x(50).
           10 reg-produtos-tipo pic x(50).
              88 reg-compota value "c" "C".
              88 reg-marmelada value "m" "M".
              88 reg-licor value "l" "L".
           10 reg-produtos-stock pic 9(3).
       SD  ord-produtos.
       01  sort-reg-produtos.
         10 sort-produtos-id pic 99.
         10 sort-produtos-nome pic x(50).
         10 sort-produtos-tipo pic x(50).
            88 sort-compota value "c" "C".
            88 sort-marmelada value "m" "M".
            88 sort-licor value "l" "L".
         10 sort-produtos-stock pic 9(3).
      *parte faturas
       FD  faturas.
       01  reg-faturas.
           10 reg-faturas-id pic 99.
           10 reg-fatura-data.
               20 reg-faturas-ano pic 9(004).
               20 reg-faturas-mes pic 9(002).
               20 reg-faturas-dia pic 9(002).
           10 reg-faturas-id-cliente pic 99.
       SD  ord-faturas.
       01  sort-reg-faturas.
         10 sort-faturas-id pic 99.
         10 sort-fatura-data.
               20 sort-faturas-ano pic 9(002) value zeros.
               20 sort-faturas-mes pic 9(002) value zeros.
               20 sort-faturas-dia pic 9(004) value zeros.
         10 sort-faturas-id-cliente pic 99.
      *parte do index/id automatico
       01  arquivo pic x.
       FD  arquivo-index_ids.
       01  registo-index_ids.
           05 registo-index_ids_clientes pic 999.
           05 registo-index_ids_produtos pic 999.
           05 registo-index_ids_faturas pic 999.
      * ficheiro de imprsã
       FD  impressao linage is 7 lines with footing at 5 lines
              at top 2 lines at bottom 1.
       01 reg-impressao.
           10 impressoa-filler pic x(180).

       WORKING-STORAGE SECTION.
      *Variavel para performs
       77  x pic 999 value zero.
      *Variaveis para guardar o index/id automatico
       01  index_ids.
           02 index_ids_clientes pic 999 value 001.
           02 index_ids_produtos pic 999 value 001.
           02 index_ids_faturas pic 999 value 001.
      *variaveis para procura de dados
       77  registo_encontrado pic x(001) value "n".
       77  codigo_encontrado pic 9(004).
      *Variavel para ele dizer se quer introduzir mais dados ou voltar para o menu principal
       01  opcao_continuar pic x value space.
           88 sim value "s" "S".
           88 nao value "n" "N".
      *Variavel para ele informar que operação quer fazer no programa
       01  OPCAO PIC 9(001) VALUE 1.
      *variaveis para os file status
      *clientes
       01  fs-clientes.
           10 fs-clientes-1 pic x(001).
           10 fs-clientes-2 pic x(001).
       01  ss-clientes.
           10 ss-clientes-1 pic x(001).
           10 ss-clientes-2 pic x(001).
      *produtos
       01  fs-produtos.
           10 fs-produtos-1 pic x(001).
           10 fs-produtos-2 pic x(001).
       01  ss-produtos.
           10 ss-produtos-1 pic x(001).
           10 ss-produtos-2 pic x(001).
      *faturas
       01  fs-faturas.
           10 fs-faturas-1 pic x(001).
           10 fs-faturas-2 pic x(001).
       01  ss-faturas.
            10 ss-faturas-1 pic x(001).
            10 ss-faturas-2 pic x(001).
      *sistema dos ids automaticos
       01  fs-ids.
           10 fs-ids-1 pic x(001).
           10 fs-ids-2 pic x(001).
      *dados para o sistema de emprimir
       77 pagina pic 9(006) value zeros.
       01  fs-impressao.
            10 fs-impressao-1 pic x(001).
            10 fs-s-2 pic x(001).

       01  Linha-cabecalho.
           10 filler pic x(031) value spaces.
           10 filler pic x(017) value "Lista de clientes".

       01  Linha-cabecalho-labels.
           10 filler pic x(006) value "ID".
           10 filler pic x(001).
           10 filler pic x(050) value "Nome".
           10 filler pic x(001).
           10 filler pic x(10) value "Nasc".
           10 filler pic x(001).
           10 filler pic x(012) value "Telefone".
           10 filler pic x(001).
           10 filler pic x(050) value "Morada".
           10 filler pic x(001).
           10 filler pic x(009) value "Nif".

       01  Linha-cabecalho-linha.
           10 filler pic x(006) value all "=".
           10 filler pic x(001).
           10 filler pic x(050) value all "=".
           10 filler pic x(001).
           10 filler pic x(10) value all "=".
           10 filler pic x(001).
           10 filler pic x(012) value all "=".
           10 filler pic x(001).
           10 filler pic x(050) value all "=".
           10 filler pic x(001).
           10 filler pic x(009) value all "=".

       01  Linha-detalhe.
           10 detalhe-id pic z(006).
           10 filler pic x(001).
           10 detalhe-nome pic x(050).
           10 filler pic x(001).
           10 detalhe-nasc.
              20 detalhe-dia pic 9(002).
              20 filler pic x(001) value "/".
              20 detalhe-mes pic 9(002).
              20 filler pic x(001) value "/".
              20 detalhe-ano pic x(004).
           10 filler pic x(001).
           10 detalhe-telefone pic 9(012).
           10 filler pic x(001).
           10 detalhe-morada pic x(050).
           10 filler pic x(001).
           10 detalhe-nif pic x(009).

       01  Linha-rodape.
           10 filler pic x(006) value spaces.
           10 filler pic x(008) value "Pagina: ".
           10 rodape-pagina pic z(006) value zeros.

       77  wrk-opcao pic x(001) value spaces.
       77  tecla pic x value space.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform load_ids.
           perform menu until opcao = 0.
           STOP RUN.
      *sistema de carregar os ids.
       load_ids.
            display "Carregando os ids".
            open input arquivo-index_ids.
            read arquivo-index_ids
            move registo-index_ids_clientes to index_ids_clientes
            move registo-index_ids_produtos to index_ids_produtos
            move registo-index_ids_faturas to index_ids_faturas
            close arquivo-index_ids.
            display "Ids carregados com sucesso".
            display index_ids_clientes.
            display index_ids_produtos.
            display index_ids_faturas.
      *salvar os ids no ficheiro
       save_ids.
       open output arquivo-index_ids.
           move index_ids_clientes to registo-index_ids_clientes
           move index_ids_produtos to registo-index_ids_produtos
           move index_ids_faturas to registo-index_ids_faturas
           write registo-index_ids.
       close arquivo-index_ids.
      *menus de opções
       menu.
           display "-------------------------------------------" at 0139.
           display "MENU DE OPCOES" at 0253.
           display "-------------------------------------------" at 0339.
           display "1. Inserir" at 0440.
           display "2. Listar" at 0540.
           display "3. Alterar" at 0640.
           display "4. Eliminar"at 0740.
           display "5. Ordenar" at 0840.
           display "6. Imprimir" at 0940.
           display "0. Sair" at 1040.
           display "-------------------------------------------" at 1139.
           display "Escolha a opcao que quer: " at 1240.
           accept opcao at 1266.
           display "-------------------------------------------" at 1339.
      *     display menusection.
      *     accept menusection.
           evaluate true
               when OPCAO = 1
                   display " " at 0101 with erase eos background-color 0
                   perform menu_inserir
               when OPCAO = 2
                   display " " at 0101 with erase eos background-color 0
                   perform menu_listar
               when OPCAO = 3
                   display " " at 0101 with erase eos background-color 0
                   perform alterar_clientes
               when OPCAO = 4
                   display " " at 0101 with erase eos background-color 0
                   perform eliminar_clientes
               when opcao = 5
                   display " " at 0101 with erase eos background-color 0
                   perform ordenar
               when opcao = 6
                   display " " at 0101 with erase eos background-color 0
                   perform imprimir_clientes
               when OPCAO = 0
                   DISPLAY "TERMINADO..." at 6045
               when OTHER
                   DISPLAY "OPCAO INVALIDA!" at 1445
                   DISPLAY "Volte a introduzir a opcao" at 1545
                   display " " at 0101 with erase eos background-color 0
           end-evaluate.

       menu_inserir.
           display "-------------------------------------------" at 0139.
           display "1. Clientes" at 0239.
           display "2. Produtos" at 0339.
           display "3. Faturas" at 0439.
           display "-------------------------------------------" at 0539.
           display "Escolha a opcao: " at 0639.
           accept opcao at 0656.
      *     display inserirsection.
      *     accept inserirasection.
           evaluate true
               when OPCAO = 1
                   display " " at 0101 with erase eos background-color 0
                   perform inserir_clientes
               when OPCAO = 2
                   display " " at 0101 with erase eos background-color 0
                   perform inserir_produtos
               when OPCAO = 3
                   display " " at 0101 with erase eos background-color 0
                   perform inserir_faturas
               when OTHER
                   display "-------------------------------------------" at 0739
                   DISPLAY "OPCAO INVALIDA!" at 0839
                   DISPLAY "Volte a introduzir a opcao" at 0939
                   display " " at 0101 with erase eos background-color 0
           end-evaluate.

       menu_listar.
           display "-------------------------------------------" at 0139.
           display "1. Clientes" at 0239.
           display "2. Produtos" at 0339.
           display "3. Faturas" at 0439.
           display "-------------------------------------------" at 0539.
           display "Escolha a opcao: " at 0639.
           accept opcao at 0656.
      *     display listarsection.
      *     accept listarsection.
           evaluate true
               when OPCAO = 1
                   display " " at 0101 with erase eos background-color 0
                   perform listar_clientes
               when OPCAO = 2
                   display " " at 0101 with erase eos background-color 0
                   perform listar_produtos
               when OPCAO = 3
                   display " " at 0101 with erase eos background-color 0
                   perform listar_faturas
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
                   DISPLAY "Volte a introduzir a opcao"
                   display " " at 0101 with erase eos background-color 0
           end-evaluate.


      *inserir
       inserir_clientes.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes.
           display " " at 0101 with erase eos background-color 0
           display "-------------------------------------------" at 0139.
           display "INFORMACOES DO CLIENTE" at 0248.
           display "-------------------------------------------" at 0339.
           move index_ids_clientes to reg-clientes-id.
           display "ID: " at 0439 index_ids_clientes at 0443.
           display "Nome: " at 0539.
           accept reg-clientes-nome at 0545.
           display "Morada: " at 0639.
           accept reg-clientes-morada at 0647.
           display "Telemovel: " at 0739.
           accept reg-clientes-telefone at 0751.
           perform until reg-clientes-telefone is numeric
               display "ERRO - o telefone so pode ser numerico. "at 0839
                "Volte a introduzir o telefone: " at 0939
               accept reg-clientes-telefone at 0970
           end-perform.
           display "NIF: " at 1039.
           accept reg-clientes-nif at 1044.
           perform until reg-clientes-nif is numeric
               display "ERRO - o NIF nao pode conter caracteres, "
     -        "so numeros" at 1139
               display "Volte a introduzir o NIF: " at 1239
               accept reg-clientes-nif at 1265
           end-perform.
           display "Dia de nascimento: " at 1339.
           accept reg-clientes-data-dia at 1358.
           perform until (reg-clientes-data-dia > 0 and
                          reg-clientes-data-dia < 32)
               display "ERRO - o dia tem que estar entre 1-31" at 1439
               display "Volte a introduzir o dia: " at 1539
               accept reg-clientes-data-dia at 1565
           end-perform.
           display "Mes de nascimento: " at 1639.
           accept reg-clientes-data-mes at 1658.
           perform until (reg-clientes-data-mes > 0 and
                          reg-clientes-data-mes < 13)
               display "ERRO - o mes tem que estar entre 1-12" at 1739
               display "Volte a introduzir o mes: " at 1839
               accept reg-clientes-data-mes at 1865
           end-perform.
           display "Ano de nascimento: " at 1939.
           accept reg-clientes-data-ano at 1958.
           perform until (reg-clientes-data-ano > 1921 AND
                          reg-clientes-data-ano < 2021)
            display "ERRO - o ano tem que estar entre 1921-2021" at 2039
               display "Volte a introduzir o ano: " at 2139
               accept reg-clientes-data-ano at 2165
           end-perform.
           display "-------------------------------------------" at 2239.
           write reg-clientes
              invalid key
              display "Codigo:  " reg-clientes-id at 2339
     -        " foi registado" at 2350.
           close clientes.
           compute index_ids_clientes = index_ids_clientes + 1.
           perform save_ids.
           display "Quer introduzir mais algum cliente?: " at 2439.
           accept opcao_continuar at 2475.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada" at 2539
              display "Volta introduzir a opcao: " at 2639
              accept opcao_continuar at 2665
           end-perform.
           if sim then
              perform inserir_clientes
           end-if.
           display "-------------------------------------------"at 2739.
           display " " at 0101 with erase eos background-color 0.

       inserir_produtos.
           open i-o produtos.
           move space to reg-produtos.
           move zeros to reg-produtos.
           display "-------------------------------------------"at 0139.
           display "INFORMACOES DO PRODUTO" at 0248.
           display "-------------------------------------------"at 0339.
           move index_ids_produtos to reg-produtos-id.
           display "ID: "  at 0439 index_ids_produtos at 0443.
           display "Nome: " at 0539.
           accept reg-produtos-nome at 0545.
           display "Tipo do produto: " at 0639.
           accept reg-produtos-tipo at 0656.
           perform until (reg-produtos-tipo = "l" or
                          reg-produtos-tipo = "L" or
                          reg-produtos-tipo = "m" or
                          reg-produtos-tipo = "M" or
                          reg-produtos-tipo = "c" or
                          reg-produtos-tipo = "C")
                display "ERRO - Tipo de produto invalido." at 0739
                display "Volta a introduzir o tipo de produto(l/m/c):"at
                0839
                accept reg-produtos-tipo at 0968
           end-perform.
           display "Stock: " at 1039.
           accept reg-produtos-stock at 1046.
           perform until reg-produtos-stock is numeric
               display "ERRO - o número de stock tem que ser numerico"at
               1139
               display "volta a introduzir a opcao: " at 1239
               accept reg-produtos-stock at 1258
           end-perform.
           write reg-produtos
              invalid key
              display "Codigo: " at 1339 reg-produtos-id at 1347
              " foi registado" at 1349.
           close produtos.
           compute index_ids_produtos = index_ids_produtos + 1.
           perform save_ids.
           display "-------------------------------------------"at 1439.
           display "Quer introduzir mais algum produto?" at 1539.
           accept opcao_continuar at 1575 .
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada" at 1639
              display "Volta introduzir a opcao: " at 1739
              accept opcao_continuar at 1770
           end-perform.
           if sim then
              perform inserir_produtos
           end-if.
           display "-------------------------------------------"at 1839.
           display " " at 0101 with erase eos background-color 0.

       inserir_faturas.
           open i-o faturas.
           move space to reg-faturas.
           move zeros to reg-faturas.
           display "-------------------------------------------"at 0139.
           display "INFORMACOES DA FATURA" at 0249.
           display "-------------------------------------------"at 0339.
           move index_ids_faturas to reg-faturas-id.
           display "ID: " at 0439 index_ids_faturas at 0439.
           accept reg-fatura-data from date yyyymmdd.
           display "Data da fatura: " at 0539 reg-faturas-dia  at 0555
               "/" at 0556 reg-faturas-mes at 0557 "/" at 0559
               reg-faturas-ano 0560.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes-id.
           display "ID do cliente: " at 0639.
           accept reg-faturas-id-cliente at 0653.
           move reg-faturas-id-cliente to reg-clientes-id.
           perform until (registo_encontrado equal to "S")
              move "S" to registo_encontrado
              read clientes record
                invalid key
                move "N" to registo_encontrado
              display "Nao encontrei nenhum cliente com esse id. " at
              0739 "Volta a introduzir um id de cliente: "at 0770
              accept reg-faturas-id-cliente at 0772
              move reg-faturas-id-cliente to reg-clientes-id
           end-perform.
           close clientes.
           write reg-faturas
              invalid key
              display "Codigo: " at 0839 reg-faturas-id at 0847
              " foi registada" at 0849.
           close faturas.
           compute index_ids_faturas = index_ids_faturas + 1.
           perform save_ids.
           display "-------------------------------------------"at 0939.
           display "Quer introduzir mais alguma fatura?" at 1039.
           accept opcao_continuar at 1075.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao: "
              accept opcao_continuar
           end-perform.
           if sim then
              perform inserir_faturas
           end-if.
           display "-------------------------------------------"at 1039.
           display " " at 0101 with erase eos background-color 0.

      *
       listar_clientes.
           open input clientes.
           if fs-clientes equal zeros
            display "-------------------------------------------"at 0139
              display "INFORMACOES DOS CLIENTES" at 0249
            display "-------------------------------------------"at 0339
              read clientes next
              perform until fs-clientes equal "10"
                display "ID: " at 0439 reg-clientes-id at 0442
                display "Nome: " at 0539 reg-clientes-nome at 0545
                display "Data de nascimento: " at 0639
                reg-clientes-data-dia at 0659 "/" at 0661
                reg-clientes-data-mes at 0662 "/" at 0664
                reg-clientes-data-ano at 0665
                display "Morada: " at 0739 reg-clientes-morada at 0747
                display "Telemovel: " at 0839 reg-clientes-telefone at
                0850
                display "NIF: " at 0939 reg-clientes-nif at 0944
                read clientes next
              end-perform
           display "-------------------------------------------"at 1039
               display "Escreva S/s para sair ao menu principal:"
                   at 1139
               accept opcao_continuar at 1181
           else
              display "-----------------------------------------"at 0139
              display "Não tens nenhum cliente registado" at 0239
              display "-----------------------------------------"at 0339
              display "Escreva S/s para sair ao menu principal:"
                   at 0439
               accept opcao_continuar at 0481
           end-if.
           display " " at 0101 with erase eos background-color 0.
           close clientes.

       listar_produtos.
           display " " at 0101 with erase eos background-color 0.
           open input produtos.
           if fs-produtos equal zeros
            display "-------------------------------------------"at 0139
              display "INFORMACOES DOS PRODUTOS" at 0248
            display "-------------------------------------------"at 0139
              read produtos next
              perform until fs-produtos equal "10"
                 display "ID: " at 0439 reg-produtos-id at 0443
                 display "Nome: " at 0539 reg-produtos-nome at 0545
                 display "Tipo do produto: " at 0639
                   reg-produtos-tipo at 0650
                 display "Stock: " at 0739 reg-produtos-stock at 0745
                 read produtos next
              end-perform
              display "-----------------------------------------"at 0839
              display "Escreva S/s para sair ao menu principal:"
                   at 1039
               accept opcao_continuar at 1081
           else
              display "-----------------------------------------"at 0139
              display "Não tens nenhum cliente registado" at 0239
              display "-----------------------------------------"at 0339
              display "Escreva S/s para sair ao menu principal:"
                   at 0439
               accept opcao_continuar at 0481
           end-if.
           close produtos.
           display " " at 0101 with erase eos background-color 0.

       listar_faturas.
           open input faturas.
           if fs-faturas equal zeros
            display "-------------------------------------------"at 0139
               display "INFORMACOES DAS FATURAS" at 0248
            display "-------------------------------------------"at 0339
               read faturas next
               perform until fs-faturas equal "10"
                  display "ID: " at 0439 reg-faturas-id at 0443
                  display "Data da fatura: " at 0539
                  reg-faturas-dia at 0555 "/" at 0556
                  reg-faturas-mes at 0557 "/" at 0559
                  reg-faturas-ano at 0560
                  display "Id do cliente: " at 0639
                  reg-faturas-id-cliente at 0654
                  read faturas next
               end-perform
              display "-----------------------------------------"at 0339
               display "Escreva S/s para sair ao menu principal:"
                   at 0839
               accept opcao_continuar at 0881
           else
              display "-----------------------------------------"at 0339
              display "Nao tens nenhuma fatura registada"
              display "-----------------------------------------"at 0339
              display "Escreva S/s para sair ao menu principal:"
                   at 0439
               accept opcao_continuar at 0481
           end-if.
           close faturas.
           display " " at 0101 with erase eos background-color 0.
      *alterar
       alterar_clientes.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes.
            display " " at 0101 with erase eos background-color 0
            display "-------------------------------------------"at 0139
           display "Qual e o ID do cliente do qual quer alterar os" at
               0239 " dados?: " at 0285.
           accept x at 0294.
           move x to reg-clientes-id.
           move "S" to registo_encontrado.
           read clientes record
               invalid key
               move "N" to registo_encontrado.
           if registo_encontrado = "N"
            display "-------------------------------------------"at 0339
               display "Registo nao foi encontrado" at 0448
            display "-------------------------------------------"at 0539
           else
            display "-------------------------------------------"at 0339
               display "Registro encontrado" at 0439
            display "-------------------------------------------"at 0539
               perform alterar_registo.
           close clientes.
           display " " at 0101 with erase eos background-color 0.

       alterar_registo.
           display " " at 0101 with erase eos background-color 0.
           display "-------------------------------------------"at 0139
           display "O que e que voce quer alterar?"  at 0239.
           display "1. Nom e" at 0339.
           display "2. Morada " at 0439.
           display "3. Telefone " at 0539.
           display "4. NIF" at 0639.
           display "5. Dia de nascimiento" at 0739.
           display "6. Mes de nascimiento" at 0839.
           display "7. Ano de nascimiento" at 0939.
           display "Opcao: " at 1039.
           accept OPCAO at 1046.
           evaluate true
               when OPCAO = 1
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO NOME------" at 0139
                   display "Nome: " at 0239
                   accept reg-clientes-nome at 0245
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
               when OPCAO = 2
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVA MORADA------" at 0139
                   display "Morada: " at 0239
                   accept reg-clientes-morada at 0247
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
               when OPCAO = 3
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO TELEFONE------" at 0139
                   display "Telefone: " at 0239
                   accept reg-clientes-telefone at 0248
                   perform until reg-clientes-telefone is numeric
                       display "ERRO - o telefone so pode ser numerico."
                       at 0339
                       " Volte a introduzir o telefone: " at 0375
                       accept reg-clientes-telefone at 0395
                   end-perform
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
               when OPCAO = 4
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO NIF------" at 0139
                   display "NIF: " at 0239
                   accept reg-clientes-nif at 0243
                   perform until reg-clientes-nif is numeric
                     display "ERRO - o NIF nao pode conter caracteres, "
     -                 at 0339 "so numeros." at 0375
                       display "Volte a introduzir o NIF: " at 0439
                       accept reg-clientes-nif at 0465
                   end-perform
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
               when OPCAO = 5
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO DIA DE NASCIMENTO------" at 0139
                   display "Dia de nascimento: " at 0239
                   accept reg-clientes-data-dia at 0258
                   perform until (reg-clientes-data-dia > 0 and
                                  reg-clientes-data-dia < 32)
                       display "ERRO - o dia tem que estar entre 1-31"
                       at 0339
                       display "Volte a introduzir o dia: " at 0439
                       accept reg-clientes-data-dia at 0458
                   end-perform
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
                when OPCAO = 6
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO MES DE NASCIMENTO------" at 0139
                   display "Mes de nascimento: " at 0239
                   accept reg-clientes-data-mes at 0358
                   perform until (reg-clientes-data-mes > 0 and
                                  reg-clientes-data-mes < 13)
                       display "ERRO - o mes tem que estar entre 1-12"
                       at 0439
                       display "Volte a introduzir o mes: " at 0539
                       accept reg-clientes-data-mes at 0558
                   end-perform
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
                when OPCAO = 7
                   display " " at 0101 with erase eos background-color 0
                   display "------NOVO ANO DE NASCIMENTO------" at 0139
                   display "Ano de nascimento:" at 0239
                   accept reg-clientes-data-ano at 0258
                   perform until (reg-clientes-data-ano > 1921 AND
                                  reg-clientes-data-ano < 2021)
                    display "ERRO - o ano tem que estar entre 1921-2021"
                    at 0339
                       display "Volte a introduzir o ano: " at 0439
                       accept reg-clientes-data-ano at 0465
                   end-perform
                   perform reescrever-registo
                   display " " at 0101 with erase eos background-color 0
               when other
            display "-------------------------------------------"at 1139
                   display "OPCAO INVALIDA!" at 1239
                   display "Volte a introduzir a opcao." at 1339
                   display "Escreva S/s para sair ao menu principal:"
                   at 1439
                   accept opcao_continuar at 1481
                   display " " at 0101 with erase eos background-color 0
                  continue
           end-evaluate.

       reescrever-registo.
           REWRITE reg-clientes
              INVALID KEY
              DISPLAY "ERRO AO REESCREVER O REGISTO!".

      *eliminar
       eliminar_clientes.
           display "-------------------------------------------"at 0139.
           open i-o clientes.
           move "S" to registo_encontrado.
           move space to reg-clientes.
           move zeros to reg-clientes-id.
           display " " at 0101 with erase eos background-color 0.

           display "Qual e o ID do cliente do que quer apagar? " at
               0239.
           accept x at 0275.
           move x to reg-clientes-id.
           read clientes record
              invalid key
                 move "N" to registo_encontrado.
           if registo_encontrado = "N"
               display "-------------------------------------------" at
                0339
               display "Não encontrei nenhum cliente com esse id" at
                0439
           else
              display " " at 0101 with erase eos background-color 0.
              perform encontrar-registo-cliente.
              display " " at 0101 with erase eos background-color 0
           close clientes.

       encontrar-registo-cliente.
           display " " at 0101 with erase eos background-color 0.
           display "-------------------------------------------"at 0139.
           display "Dados a eliminar " at 0248.
           display "-------------------------------------------"at 0339.
           display "ID: " at 0439 reg-clientes-id at 0443.
           display "Nome: " at 0539 reg-clientes-nome at 0545.
           display "Data de nascimento: " at 0639
               reg-clientes-data-dia at 0658 "/" at 0660
               reg-clientes-data-mes at 0663 "/" at 0664
               reg-clientes-data-ano at 0665.
           display "Morada: " at 0739 reg-clientes-morada at 0747.
           display "Telefone: " at 0839 reg-clientes-telefone at 0849.
           display "Email: " at 0939 reg-clientes-telefone at 0946.
           display "Desaja apagar este cliente?: " at 1039.
           accept opcao_continuar at 1058.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada" at 1139
              display "Volta introduzir a opcao: " at 1239
              accept opcao_continuar at 1255
           end-perform.

           if sim then
              delete clientes record
                 invalid key
                 display "Erro ao excluir o cliente" at 1339.
           display " " at 0101 with erase eos background-color 0.


       ordenar.
           sort ord-clientes ascending sort-clientes-nome
           input procedure sortin-clientes
           output procedure sortout-clientes.

       sortin-clientes.
           open input clientes.
           read clientes next.
           perform until fs-clientes equal "10"
               move reg-clientes-id to sort-clientes-id
               move reg-clientes-nome to sort-clientes-nome
               move reg-clientes-data-nasc to sort-clientes-data-nasc
               move reg-clientes-morada to sort-clientes-morada
               move reg-clientes-telefone to sort-clientes-telefone
               move reg-clientes-nif to sort-clientes-nif
               release sort-reg-clientes
               read clientes next
           end-perform.
           close clientes.

       sortout-clientes.
           perform pega-reg-sort.
           perform lista-clientes until ss-clientes equal "10".

       lista-clientes.
           display " " at 0101 with erase eos background-color 0.
           display "-------------------------------------------"at 0139.
           display "INFORMACAO DOS CLIENTE" 0248
           display "-------------------------------------------"at 0339.
           display "ID: " at 0439 sort-clientes-id at 0443.
           display "Nome: " at 0539 sort-clientes-nome at 0545.
           display "Data de nascimento: " at 0639
               clientes-data-dia at 0659 "/" at 0661
               clientes-data-mes at 0672 "/" at 0674
               clientes-data-ano at 0675.
           display "Morada: " at 0739 sort-clientes-morada at 0747.
           display "Telemovel: " at 0839 sort-clientes-telefone at 0849.
           display "NIF: " at 0939 sort-clientes-nif at 1046
           display "-------------------------------------------"at 1139.
           perform pega-reg-sort.
           display " " at 0101 with erase eos background-color 0.

       pega-reg-sort.
           RETURN ord-clientes
               at end move "10" to ss-clientes.

       imprimir_clientes.
           display " " at 0101 with erase eos background-color 0.
           display "-------------------------------------------"at 0139.
           display "Criando o ficheiro" at 0248.
           display "-------------------------------------------"at 0339.
           open input clientes.
           open output impressao.
           read clientes next.
           if fs-clientes equal zeros
              perform imprime-cabecalho
              perform until fs-clientes equal "10"
                move reg-clientes-id to detalhe-id
                move reg-clientes-nome to detalhe-nome
                move reg-clientes-data-dia to detalhe-dia
                move reg-clientes-data-mes to detalhe-mes
                move reg-clientes-data-ano to detalhe-ano
                move reg-clientes-telefone to detalhe-telefone
                move reg-clientes-morada to detalhe-morada
                move reg-clientes-nif to detalhe-nif
                perform imprime-linha
                read clientes next
              end-perform
           end-if.
           close clientes.
           close impressao.
           display "Clientes imprimidos, por favor verifique o ficheiro"
            at 0439 " clientes.txt" at 0480.
           display "Escreva S/s para sair ao menu principal:" at 0539.
           accept opcao_continuar at 0581.
           display " " at 0101 with erase eos background-color 0.

       imprime-cabecalho.
           write reg-impressao from Linha-detalhe
           before advancing 1 line
           at end-of-page
              perform imprime-rodape
              perform imprime-cabecalho.

       imprime-linha.
           write reg-impressao from Linha-detalhe
           before advancing 1 line
           at end-of-page
              perform imprime-rodape
              perform imprime-cabecalho.

       imprime-rodape.
           add 1 to pagina
           move pagina to rodape-pagina
           write reg-impressao from spaces
              before advancing 1 line
           write reg-impressao from Linha-rodape
              before advancing page.


       END PROGRAM Trabalho2.
