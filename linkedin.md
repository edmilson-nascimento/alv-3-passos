# Passos Principais e Fluxo de Desenvolvimento para Exibição e Utilização do SAP ALV Report

## Escopo da solução proposta

Neste projeto, será criado um relatório utilizando tecnologias SAP, como o ALV Report (neste caso, em forma de método, mas também pode ser implementado com funções como `REUSE_ALV_GRID_DISPLAY` e outras). Os ALV Reports da SAP são uma das soluções mais interessantes para relatórios que já trabalhei. Acho fascinante como funcionam os layouts, subtotais e links (o termo técnico seria *hotspot*) para transações, o que eleva bastante o nível de entendimento das informações exibidas.

Sobre os dados para exibição, serão usadas tabelas EPM (`snwd*`), que podem ser geradas pela transação `SEPM_DG` (conforme post de explicação). Esse grupo de tabelas é ideal para a criação de exemplos e testes gerais em ABAP.

## SRP no SAP?

Cada um faz seu trabalho que tudo vai dar certo no final.

O conceito de SRP (*Single Responsibility Principle*) é amplamente discutido e possui diversas aplicações. Não entrarei em detalhes sobre isso nem explorarei sua aplicação na programação ABAP. Aqui, destaco a seguinte mensagem: **Princípio da Responsabilidade Única**. Isso significa que uma subrotina (método, função, form, define, module, etc.) deve realizar apenas a tarefa para a qual foi programada. Ressalto que, dependendo do tempo disponível, escopo e outras condições (assunto para outro post), é possível aplicar esses conceitos de forma interessante em programas ABAP.

Métodos são fragmentos de código que têm responsabilidades únicas.

## Dados x Informações

Nunca me esqueço de uma aula na faculdade sobre Dados e Informações, onde aprendi que informações são dados organizados de forma a facilitar a tomada de decisões (essa é a minha interpretação e entendimento, que pode ser complementada ou corrigida por outros). Um conceito simples, mas que faz uma grande diferença ao desenvolver uma solução. Sem organização, os dados são apenas um conjunto de valores em colunas. Prover uma boa interpretação é algo que tem grande peso e significado, especialmente em relatórios, como no caso da solução apresentada neste post.

## Três principais passos para esse ALV

1. Buscar Dados  
2. Processar Dados  
3. Exibir Informações  

### Buscar Dados

Nesta rotina, será realizada uma busca simples que, para este exemplo, trará dados de duas tabelas diferentes: `snwd_bpa` (EPM: Business Partners) e `snwd_ad` (EPM: Address Table). Faremos uma busca na tabela de Parceiros e também na tabela de Endereço, ao final exibindo o endereço dos respectivos.

```abap
method buscar_dados .
  refresh:
    bpa_tab, ad_tab .
  if ( lines( bp_id ) eq 0 ).
  else.
    select *
      into table bpa_tab
      from snwd_bpa
      where bp_id in bp_id .
    if ( sy-subrc eq 0 ).
      select *
        into table ad_tab
        from snwd_ad
        for all entries in bpa_tab
        where node_key eq bpa_tab-address_guid .
      if ( sy-subrc eq 0 ).
      endif .
    endif .
  endif .
endmethod .
```

### Processar Dados

Depois de termos os dados, vamos organizá-los de forma que nossa saída de relatório tenha as informações correspondentes de cada parceiro. Nesta parte, usaremos o comando `READ TABLE`, que é bem particular da linguagem SAP ABAP.

O comando `READ TABLE` permite acessar um único registro, usando termos de igualdade e retornando um registro de uma matriz (no caso do ABAP, uma Work Area de uma Internal Table).

```abap
method processar_dados .
  data:
    out_line type class_report=>ty_out .
  refresh out_tab .
  if ( lines( bpa_tab ) gt 0 ) and
     ( lines( ad_tab ) gt 0 ).
    loop at bpa_tab into data(bpa_line) .
      out_line-bp_id          = bpa_line-bp_id .
      out_line-company_name   = bpa_line-company_name .
      out_line-currency_code  = bpa_line-currency_code .
      out_line-web_address    = bpa_line-web_address .
      out_line-email_address  = bpa_line-email_address .
      read table ad_tab into data(ad_line)
        with key node_key = bpa_line-address_guid .
      if ( sy-subrc eq 0 ).
        out_line-country     = ad_line-country .
        out_line-city        = ad_line-city .
        out_line-postal_code = ad_line-postal_code .
        out_line-street      = ad_line-street .
        append out_line to out_tab .
        clear out_line .
      endif .
    endloop .
  endif .
endmethod .
```

### Exibir Informações

Eu gosto de utilizar o método `FACTORY` da classe `CL_SALV_TABLE` porque ele oferece melhor acesso às funcionalidades ALV via métodos, tornando o código menos verboso.

```abap
method exibir_informacoes .
  data:
    salv_table type ref to cl_salv_table,
    columns    type ref to cl_salv_columns_table,
    display    type ref to cl_salv_display_settings.
  if ( lines( out_tab ) eq 0 ).
  else.
    try .
        cl_salv_table=>factory(
          exporting
            list_display = if_salv_c_bool_sap=>true
          importing
            r_salv_table = salv_table
          changing
            t_table      = out_tab
        ).
* Otimizar largura da coluna
        columns = salv_table->get_columns( ).
        if ( columns is bound ).
          columns->set_optimize( cl_salv_display_settings=>true ).
        endif .
* Usando Status
        salv_table->set_screen_status(
          pfstatus      = 'STANDARD_FULLSCREEN'
          report        = 'SAPLKKBL'
          set_functions = salv_table->c_functions_all
        ).
* Layout de Zebra
        display = salv_table->get_display_settings( ) .
        if ( display is bound ).
          display->set_striped_pattern( cl_salv_display_settings=>true ) .
        endif .
        salv_table->display( ).
      catch cx_salv_msg .
      catch cx_salv_not_found .
      catch cx_salv_existing .
      catch cx_salv_data_error .
      catch cx_salv_object_not_found .
    endtry.
  endif .
endmethod .
```

## Conclusão

Neste post, usamos poucas linhas de código para contemplar uma rotina para buscar dados, organizá-los e exibi-los com SAP ALV Reports.

O objetivo foi apresentar uma ideia básica de como a maioria dos relatórios é desenvolvida. Com um framework robusto como o SAP ALV Reports, as rotinas seguem os três passos básicos e as sub-rotinas necessárias (ou opcionais) dentro de cada etapa. O código foi desenvolvido de forma simplificada e está disponível no GitHub para melhorias, atualizações e correções, juntamente com sua documentação.

Espero que, de alguma forma, este conteúdo tenha sido produtivo para você.


