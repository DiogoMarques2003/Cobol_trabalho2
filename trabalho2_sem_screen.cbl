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
           display "-------------------------------------------".
           display "MENU DE OPCOES".
           display "-------------------------------------------".
           display "1. Inserir".
           display "2. Listar".
           display "3. Alterar".
           display "4. Eliminar".
           display "5. Ordenar".
           display "6. Imprimir".
           display "0. Sair".
           display "-------------------------------------------".
           display "Escolha a opcao que quer: ".
           accept opcao.
           display "-------------------------------------------".
           evaluate true
               when OPCAO = 1
                   perform menu_inserir
               when OPCAO = 2
                   perform menu_listar
               when OPCAO = 3
                   perform alterar_clientes
               when OPCAO = 4
                   perform eliminar_clientes
               when opcao = 5
                   perform ordenar
               when opcao = 6
                   perform imprimir_clientes
               when OPCAO = 0
                   DISPLAY "TERMINADO..."
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
                   DISPLAY "Volte a introduzir a opcao"
           end-evaluate.

       menu_inserir.
           display "-------------------------------------------".
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "-------------------------------------------".
           display "Escolha a opcao: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform inserir_clientes
               when OPCAO = 2
                   perform inserir_produtos
               when OPCAO = 3
                   perform inserir_faturas
               when OTHER
                   display "-------------------------------------------"
                   DISPLAY "OPCAO INVALIDA!"
                   DISPLAY "Volte a introduzir a opcao"
           end-evaluate.

       menu_listar.
           display "-------------------------------------------".
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "-------------------------------------------".
           display "Escolha a opcao: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform listar_clientes
               when OPCAO = 2
                   perform listar_produtos
               when OPCAO = 3
                   perform listar_faturas
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
                   DISPLAY "Volte a introduzir a opcao"
           end-evaluate.

      *inserir
       inserir_clientes.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes.
           display "-------------------------------------------".
           display "INFORMACOES DO CLIENTE".
           display "-------------------------------------------".
           move index_ids_clientes to reg-clientes-id.
           display "ID: " index_ids_clientes.
           display "Nome: ".
           accept reg-clientes-nome.
           display "Morada: ".
           accept reg-clientes-morada.
           display "Telemovel: ".
           accept reg-clientes-telefone.
           perform until reg-clientes-telefone is numeric
               display "ERRO - o telefone so pode ser numerico. "
                "Volte a introduzir o telefone: "
               accept reg-clientes-telefone
           end-perform.
           display "NIF: ".
           accept reg-clientes-nif.
           perform until reg-clientes-nif is numeric
               display "ERRO - o NIF nao pode conter caracteres, "
     -        "so numeros"
               display "Volte a introduzir o NIF: "
               accept reg-clientes-nif
           end-perform.
           display "Dia de nascimento: ".
           accept reg-clientes-data-dia.
           perform until (reg-clientes-data-dia > 0 and
                          reg-clientes-data-dia < 32)
               display "ERRO - o dia tem que estar entre 1-31"
               display "Volte a introduzir o dia: "
               accept reg-clientes-data-dia
           end-perform.
           display "Mes de nascimento: ".
           accept reg-clientes-data-mes.
           perform until (reg-clientes-data-mes > 0 and
                          reg-clientes-data-mes < 13)
               display "ERRO - o mes tem que estar entre 1-12"
               display "Volte a introduzir o mes: "
               accept reg-clientes-data-mes
           end-perform.
           display "Ano de nascimento: ".
           accept reg-clientes-data-ano.
           perform until (reg-clientes-data-ano > 1921 AND
                          reg-clientes-data-ano < 2021)
            display "ERRO - o ano tem que estar entre 1921-2021"
               display "Volte a introduzir o ano: "
               accept reg-clientes-data-ano
           end-perform.
           display "-------------------------------------------".
           write reg-clientes
              invalid key
              display "Codigo:  " reg-clientes-id
     -        " foi registado".
           close clientes.
           compute index_ids_clientes = index_ids_clientes + 1.
           perform save_ids.
           display "Quer introduzir mais algum cliente?: ".
           accept opcao_continuar.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao: "
              accept opcao_continuar
           end-perform.
           if sim then
              perform inserir_clientes
           end-if.
           display "-------------------------------------------".
       inserir_produtos.
           open i-o produtos.
           move space to reg-produtos.
           move zeros to reg-produtos.
           display "-------------------------------------------".
           display "INFORMACOES DO PRODUTO".
           display "-------------------------------------------".
           move index_ids_produtos to reg-produtos-id.
           display "ID: " index_ids_produtos.
           display "Nome: ".
           accept reg-produtos-nome.
           display "Tipo do produto: ".
           accept reg-produtos-tipo.
           perform until (reg-produtos-tipo = "l" or
                          reg-produtos-tipo = "L" or
                          reg-produtos-tipo = "m" or
                          reg-produtos-tipo = "M" or
                          reg-produtos-tipo = "c" or
                          reg-produtos-tipo = "C")
                display "ERRO - Tipo de produto invalido."
                display "Volta a introduzir o tipo de produto(l/m/c):"
                accept reg-produtos-tipo
           end-perform.
           display "Stock: ".
           accept reg-produtos-stock.
           perform until reg-produtos-stock is numeric
               display "ERRO - o número de stock tem que ser numerico"
               display "volta a introduzir a opcao: "
               accept reg-produtos-stock
           end-perform.
           write reg-produtos
              invalid key
              display "Codigo: " reg-produtos-id " foi registado".
           close produtos.
           compute index_ids_produtos = index_ids_produtos + 1.
           perform save_ids.
           display "-------------------------------------------".
           display "Quer introduzir mais algum produto?".
           accept opcao_continuar.
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao: "
              accept opcao_continuar
           end-perform.
           if sim then
              perform inserir_produtos
           end-if.
           display "-------------------------------------------".

       inserir_faturas.
           open i-o faturas.
           move space to reg-faturas.
           move zeros to reg-faturas.
           display "-------------------------------------------".
           display "INFORMACOES DA FATURA".
           display "-------------------------------------------".
           move index_ids_faturas to reg-faturas-id.
           display "ID: " index_ids_faturas.
           accept reg-fatura-data from date yyyymmdd.
           display "Data da fatura: " reg-faturas-dia
               "/" reg-faturas-mes  "/"
               reg-faturas-ano 0560.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes-id.
           display "ID do cliente: ".
           accept reg-faturas-id-cliente.
           move reg-faturas-id-cliente to reg-clientes-id.
           perform until (registo_encontrado equal to "S")
              move "S" to registo_encontrado
              read clientes record
                invalid key
                move "N" to registo_encontrado
              display "Nao encontrei nenhum cliente com esse id. "
               "Volta a introduzir um id de cliente: "
              accept reg-faturas-id-cliente
              move reg-faturas-id-cliente to reg-clientes-id
           end-perform.
           close clientes.
           write reg-faturas
              invalid key
              display "Codigo: " reg-faturas-id
              " foi registada".
           close faturas.
           compute index_ids_faturas = index_ids_faturas + 1.
           perform save_ids.
           display "-------------------------------------------".
           display "Quer introduzir mais alguma fatura?" .
           accept opcao_continuar.
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
           display "-------------------------------------------".

      *
       listar_clientes.
           open input clientes.
           if fs-clientes equal zeros
            display "-------------------------------------------"
              display "INFORMACOES DOS CLIENTES"
            display "-------------------------------------------"
              read clientes next
              perform until fs-clientes equal "10"
                display "ID: " reg-clientes-id
                display "Nome: " reg-clientes-nome
                display "Data de nascimento: "
                reg-clientes-data-dia "/"
                reg-clientes-data-mes "/"
                reg-clientes-data-ano
                display "Morada: " reg-clientes-morada
                display "Telemovel: " reg-clientes-telefone
                display "NIF: " reg-clientes-nif
                read clientes next
              end-perform
           display "-------------------------------------------"
           else
              display "-----------------------------------------"
              display "Não tens nenhum cliente registado"
              display "-----------------------------------------"
           end-if.
           close clientes.

       listar_produtos.
           open input produtos.
           if fs-produtos equal zeros
            display "-------------------------------------------"
              display "INFORMACOES DOS PRODUTOS"
            display "-------------------------------------------"
              read produtos next
              perform until fs-produtos equal "10"
                 display "ID: " reg-produtos-id
                 display "Nome: " reg-produtos-nome
                 display "Tipo do produto: " reg-produtos-tipo
                 display "Stock: " reg-produtos-stock
                 read produtos next
              end-perform
               accept opcao_continuar
           else
              display "-----------------------------------------"
              display "Não tens nenhum cliente registado"
              display "-----------------------------------------"
           end-if.
           close produtos.

       listar_faturas.
           open input faturas.
           if fs-faturas equal zeros
            display "-------------------------------------------"
               display "INFORMACOES DAS FATURAS"
            display "-------------------------------------------"
               read faturas next
               perform until fs-faturas equal "10"
                  display "ID: " reg-faturas-id
                  display "Data da fatura: "
                  reg-faturas-dia "/"
                  reg-faturas-mes "/"
                  reg-faturas-ano
                  display "Id do cliente: "
                  reg-faturas-id-cliente
                  read faturas next
               end-perform
              display "-----------------------------------------"
           else
              display "-----------------------------------------"
              display "Nao tens nenhuma fatura registada"
              display "-----------------------------------------"
           end-if.
           close faturas.
      *alterar
       alterar_clientes.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes.
            display "-------------------------------------------".
           display "Qual e o ID do cliente do qual quer alterar os".
           accept x.
           move x to reg-clientes-id.
           move "S" to registo_encontrado.
           read clientes record
               invalid key
               move "N" to registo_encontrado.
           if registo_encontrado = "N"
            display "-------------------------------------------"
               display "Registo nao foi encontrado"
            display "-------------------------------------------"
           else
            display "-------------------------------------------"
               display "Registro encontrado"
            display "-------------------------------------------"
               perform alterar_registo.
           close clientes.

       alterar_registo.
           display "-------------------------------------------".
           display "O que e que voce quer alterar?".
           display "1. Nome".
           display "2. Morada ".
           display "3. Telefone ".
           display "4. NIF".
           display "5. Dia de nascimiento".
           display "6. Mes de nascimiento".
           display "7. Ano de nascimiento".
           display "Opcao: ".
           accept OPCAO.
           evaluate true
               when OPCAO = 1
                   display "------NOVO NOME------"
                   display "Nome: "
                   accept reg-clientes-nome
                   perform reescrever-registo
               when OPCAO = 2
                   display "------NOVA MORADA------"
                   display "Morada: "
                   accept reg-clientes-morada
                   perform reescrever-registo
               when OPCAO = 3
                   display "------NOVO TELEFONE------"
                   display "Telefone: "
                   accept reg-clientes-telefone
                   perform until reg-clientes-telefone is numeric
                       display "ERRO - o telefone so pode ser numerico."
                       " Volte a introduzir o telefone: "
                       accept reg-clientes-telefone
                   end-perform
                   perform reescrever-registo
               when OPCAO = 4
                   display "------NOVO NIF------"
                   display "NIF: "
                   accept reg-clientes-nif
                   perform until reg-clientes-nif is numeric
                     display "ERRO - o NIF nao pode conter caracteres, "
     -                 "so numeros."
                       display "Volte a introduzir o NIF: "
                       accept reg-clientes-nif
                   end-perform
                   perform reescrever-registo
               when OPCAO = 5
                   display "------NOVO DIA DE NASCIMENTO------"
                   display "Dia de nascimento: "
                   accept reg-clientes-data-dia
                   perform until (reg-clientes-data-dia > 0 and
                                  reg-clientes-data-dia < 32)
                       display "ERRO - o dia tem que estar entre 1-31"
                       display "Volte a introduzir o dia: "
                       accept reg-clientes-data-dia
                   end-perform
                   perform reescrever-registo
                when OPCAO = 6
                   display "------NOVO MES DE NASCIMENTO------"
                   display "Mes de nascimento: "
                   accept reg-clientes-data-mes
                   perform until (reg-clientes-data-mes > 0 and
                                  reg-clientes-data-mes < 13)
                       display "ERRO - o mes tem que estar entre 1-12"
                       display "Volte a introduzir o mes: "
                       accept reg-clientes-data-mes
                   end-perform
                   perform reescrever-registo
                when OPCAO = 70
                   display "------NOVO ANO DE NASCIMENTO------"
                   display "Ano de nascimento:"
                   accept reg-clientes-data-ano
                   perform until (reg-clientes-data-ano > 1921 AND
                                  reg-clientes-data-ano < 2021)
                    display "ERRO - o ano tem que estar entre 1921-2021"
                       display "Volte a introduzir o ano: "
                       accept reg-clientes-data-ano
                   end-perform
                   perform reescrever-registo
               when other
            display "-------------------------------------------"
                   display "OPCAO INVALIDA!"
                   display "Volte a introduzir a opcao."
                  continue
           end-evaluate.

       reescrever-registo.
           REWRITE reg-clientes
              INVALID KEY
              DISPLAY "ERRO AO REESCREVER O REGISTO!".

      *eliminar
       eliminar_clientes.
           display "-------------------------------------------"
           open i-o clientes.
           move "S" to registo_encontrado.
           move space to reg-clientes.
           move zeros to reg-clientes-id.
           display "Qual e o ID do cliente do que quer apagar? "
           accept x.
           move x to reg-clientes-id.
           read clientes record
              invalid key
                 move "N" to registo_encontrado.
           if registo_encontrado = "N"
               display "-------------------------------------------"
               display "Não encontrei nenhum cliente com esse id"
           else
              perform encontrar-registo-cliente.
           close clientes.

       encontrar-registo-cliente.
           display "-------------------------------------------".
           display "Dados a eliminar ".
           display "-------------------------------------------".
           display "ID: " reg-clientes-id.
           display "Nome: " reg-clientes-nome.
           display "Data de nascimento: "
               reg-clientes-data-dia "/"
               reg-clientes-data-mes "/"
               reg-clientes-data-ano .
           display "Morada: " reg-clientes-morada.
           display "Telefone: " reg-clientes-telefone.
           display "Email: " reg-clientes-telefone.
           display "Desaja apagar este cliente?: ".
           accept opcao_continuar
           perform until (opcao_continuar = "S" or
                          opcao_continuar = "s" or
                          opcao_continuar = "N" or
                          opcao_continuar = "n" )
              display "ERRO - opcao errada"
              display "Volta introduzir a opcao: "
              accept opcao_continuar
           end-perform.

           if sim then
              delete clientes record
                 invalid key
                 display "Erro ao excluir o cliente".


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

           display "-------------------------------------------"
           display "INFORMACAO DOS CLIENTE".
           display "-------------------------------------------".
           display "ID: " sort-clientes-id.
           display "Nome: " sort-clientes-nome.
           display "Data de nascimento: " clientes-data-dia"/"
               clientes-data-mes "/" clientes-data-ano
           display "Morada: " sort-clientes-morada.
           display "Telemovel: " sort-clientes-telefone.
           display "NIF: " sort-clientes-nif.
           display "-------------------------------------------".
           perform pega-reg-sort.


       pega-reg-sort.
           RETURN ord-clientes
               at end move "10" to ss-clientes.

       imprimir_clientes.
           display "-------------------------------------------"
           display "Criando o ficheiro"
           display "-------------------------------------------"
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
           " clientes.txt".

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
