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
              alternate key is reg-clientes-morada with duplicates
              file status is fs-clientes.
           select ord-clientes
              assign to "SORTclientes.dat"
              sort status is ss-clientes.
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
              file status is fs-produtos.
           select ord-faturas
              assign to "SORTfaturas.dat"
              sort status is ss-faturas.
      *parte produtos faturas
           select optional produtos_faturas
              assign to "produtos_faturas12.dat"
              organization is indexed
              access mode is dynamic
              record key is reg-produtos_faturas_id
              alternate key is reg-produtos_faturas_id_fat with 
              duplicates
              file status is fs-pro_faturas.
           select ord-prod_faturas
              assign to "SORTprodutos_faturas.dat"
              sort status is ss-prod_faturas.
      *parte do index/ids automaticos
           select OPTIONAL arquivo-index_ids
           assign to "index_ids.dat"
           organization is SEQUENTIAL.
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
           10 reg-clientes-telefone pic x(12).
           10 reg-clientes-nif pic 9(9).
       SD  ord-clientes.
       01  sort-reg-clientes.
         10 sort-clientes-id pic 99.
         10 sort-clientes-nome pic x(50).
         10 clientes-data-nasc.
            20 clientes-data-ano pic 9(004).
            20 clientes-data-mes pic 9(002).
            20 clientes-data-dia pic 9(002).
         10 sort-clientes-morada pic x(50).
         10 sort-clientes-telefone pic x(12).
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
           10 reg-faturas-dia pic 99.
           10 reg-faturas-mes pic 99.
           10 reg-faturas-ano pic 9999.
           10 reg-faturas-id-cliente pic 99.
           10 reg-faturas-n-produtos pic 9.
           10 reg-faturas-descricao pic x(50).
       SD  ord-faturas.
       01  sort-reg-faturas.
         10 sort-faturas-id pic 99.
         10 sort-faturas-dia pic 99.
         10 sort-faturas-mes pic 99.
         10 sort-faturas-ano pic 9999.
         10 sort-faturas-id-cliente pic 99.
         10 sort-faturas-n-produtos pic 9.
         10 sort-faturas-descricao pic x(50).
      *parte produtos faturas
       FD  produtos_faturas.
       01  reg-produtos_faturas.
           10 reg-produtos_faturas_id_fat pic 99.
           10 reg-produtos_faturas_id pic 99.
           10 reg-produtos_faturas_nome pic x(50).
           10 reg-produtos_faturas_quant pic 99.
       SD  ord-prod_faturas.
       01  sort-reg-prod-faturas.
         10 sort-prod_faturas_id_fat pic 99.
         10 sort-prod_faturas_id pic 99.
         10 sort-prod_faturas_nome pic x(50).
         10 sort-prod_faturas_quant pic 99.
      *parte do index/id automatico
       01  arquivo pic x.
       FD  arquivo-index_ids.
       01  registo-index_ids.
           05 registo-index_ids_clientes pic 999.
           05 registo-index_ids_produtos pic 999.
           05 registo-index_ids_faturas pic 999.           

       WORKING-STORAGE SECTION.
      *Variaveis para guardar o index/id automatico
       01  index_ids.
           02 index_ids_clientes pic 999 value zeros.
           02 index_ids_produtos pic 999 value zeros.
           02 index_ids_faturas pic 999 value zeros.
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
      *produtos faturas
       01  fs-pro_faturas.
           10 fs-pro_faturas-1 pic x(001).
           10 fs-pro_faturas-2 pic x(001).
       01  ss-prod_faturas.
           10 ss-prod_faturas-1 pic x(001).
           10 ss-prod_faturas-2 pic x(001).
      *ids automaticos
       01  fs-ids.
           10 fs-ids-1 pic x(001).
           10 fs-ids-2 pic x(001).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform load_ids.
           perform menu until opcao = 0.
           STOP RUN.
      *sistema de carregar os ids.
       load_ids.
       display "Carregando os ids".
       open input arquivo-index_ids.
       read arquivo-index_ids.
       move registo-index_ids_clientes to index_ids_clientes.
       move registo-index_ids_produtos to index_ids_produtos.
       move registo-index_ids_faturas to index_ids_faturas.
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
           display "******MENU DE OPCOES******".
           display "1. Inseir".
           display "2. Listar".
           display "3. Alterar".
           display "4. Eliminar".  
           display "5. Ordenar".
           display "0. Sair".
           display "**************************".
           display "Escolha a opcao que quer: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform menu_inserir
               when OPCAO = 2
                   perform menu_listar
               when OPCAO = 3
                   perform menu_alterar
               when OPCAO = 4
                   perform menu_eliminar
               when opcao = 5
                   perform menu_ordenar
               when OPCAO = 0
                   DISPLAY "TERMINADO..."
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
           end-evaluate.
           
       menu_inserir.
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "***************".
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
                   DISPLAY "OPCAO INVALIDA!"
           end-evaluate.
           
       menu_listar.
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "***************".
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
           end-evaluate.  
           
       menu_alterar.
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "***************".
           display "Escolha a opcao: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform alterar_clientes
               when OPCAO = 2
                   perform alterar_produtos
               when OPCAO = 3
                   perform alterar_faturas
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
           end-evaluate.      
             
       menu_eliminar.
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "***************".
           display "Escolha a opcao: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform eliminar_clientes
               when OPCAO = 2
                   perform eliminar_produtos
               when OPCAO = 3
                   perform eliminar_faturas
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
           end-evaluate.

       menu_ordenar.
           display "1. Clientes".
           display "2. Produtos".
           display "3. Faturas".
           display "***************".
           display "Escolha a opcao: ".
           accept opcao.
           evaluate true
               when OPCAO = 1
                   perform ordenar_clientes
               when OPCAO = 2
                   perform ordenar_produtos
               when OPCAO = 3
                   perform ordenar_faturas
               when OTHER
                   DISPLAY "OPCAO INVALIDA!"
           end-evaluate.
      *inserir         
       inserir_clientes.
           open i-o clientes.
           move space to reg-clientes.
           move zeros to reg-clientes.
           display "******INFORMACOES DO CLIENTE******".
           move index_ids_clientes to reg-clientes-id.
           display "Nome: ".
           accept reg-clientes-nome.
           display "Morada: ".
           accept reg-clientes-morada.
           display "Telemovel: ".
           accept reg-clientes-telefone.
           display "NIF: ".
           accept reg-clientes-nif.
           display "Dia de nascimento: ".
           accept reg-clientes-data-dia.
           display "Mes de nascimento: ".
           accept reg-clientes-data-mes.
           display "Ano de nascinento:".
           accept reg-clientes-data-ano.
           display "**********************************".
           write reg-clientes
              invalid key
              display "Codigo: " reg-clientes-id " foi registado".
           close clientes.
           compute index_ids_clientes = index_ids_clientes + 1.
           perform save_ids.
           display "Quer introduzir mais algum cliente?".
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
              perform introduzir-c
           end-if.
           display "-----------------".

       inserir_produtos.
           display "******INFORMACOES DO PRODUTO******".
           display "Nome: ".
           display "Tipo do produto: ".
           perform until (reg-produtos-tipo = "l" or
                          reg-produtos-tipo = "L" or
                          reg-produtos-tipo = "m" or
                          reg-produtos-tipo = "M" or
                          reg-produtos-tipo = "c" or
                          reg-produtos-tipo = "C")
                display "ERRO - Tipo de produto invalido."
                display "Volta a introduzir o tipo de produto(l/m/c): "
                accept reg-produtos-tipo    
           end-perform.
           display "Stock: ".
           display "**********************************".
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
              perform introduzir-c
           end-if.
           display "-----------------".

       inserir_faturas.
           display "******INFORMACOES DA FATURA******".
           display "ID: ".
           display "Dia da fatura: ".
           display "Mes da fatura: ".
           display "Ano da fatura: ".
           display "Nome do cliente: ".
           display "Morada: ".
           display "Numero de produtos da fatura". 
           display "Produtos da fatura: ".
           display "Nome: " 
           display "Quantidade: ".
           
       listar_clientes.
           display "******INFORMACOES DOS CLIENTES******".
           display "ID: ".
           display "Nome: ".
           display "Morada: ".
           display "Telemovel: ".
           display "NIF: ".
           display "Dia de nascimento: ".
           display "Mes de nascimento: ".
           display "Ano de nascinento: ".
           display "**********************************".  
           
       listar_produtos.
           display "******INFORMACOES DOS PRODUTOS******".
           display "ID: "
           display "Nome: ".
           display "Tipo do produto: ".
           display "Stock: ".
           display "**********************************".

       listar_faturas.
           display "******INFORMACOES DAS FATURAS******".
           display "ID: ".
           display "Data da fatura: " "/" "/".
           display "Nome do cliente: ".
           display "Morada: ".
           display "Numero de produtos da fatura". 
           display "Produtos da fatura: ".
           display "   Nome: " ", Quantidade: ".
           display "**********************************".

       alterar_clientes.
           display "Qual e o ID do cliente do qual quer alterar os"
               " dados?: ".
      *     perform until ()
           display "1. Nome".
           display "2. Morada: ".
           display "3. Telemovel: ".
           display "4. NIF: ".
           display "5. Dia de nascimento: ".
           display "6. Mes de nascimento: ".
           display "7. Ano de nascinento:".
           display "**********************************".
           display "Escreva a opcao: ".
           accept opcao.

       gravar_alterar.    
           

       alterar_produtos.


       alterar_faturas.


       END PROGRAM Trabalho2.
