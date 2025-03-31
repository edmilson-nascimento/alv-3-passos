# Descrevendo os Passos Principais e o Fluxo de Desenvolvimento para Exibição/Utilização do Robusto SAP ALV Report

## Escopo da solução proposta

Eu vou criar um relatório, utilizando tecnologias SAP como ALV Report (em forma de método neste caso mas também pode ser feito com funções como `REUSE_ALV_GRID_DISPLAY` e outras). Os ALV Report da SAP são uma das soluções mais interessantes para relatórios que já trabalhei e acho bem interessante como funciona a parte de layouts, subtotais e link (o termo técnico seria *hotspot*) para transações, o que eleva bastante o nível de entendimento das informações exibidas.

Sobre os dados para exibição, vou usar tabelas EPM (`snwd*`) que podem ser geradas pela transação `SEPM_DG` (conforme post de explicação). Acho que esse grupo de tabelas é interessante para criação de exemplos e testes ABAP gerais.

## SRP no SAP?

Cada um faz seu trabalho que tudo vai dar certo no final.

Muito se fala sobre o conceito de SRP (*Single Responsibility Principle*) e várias aplicações para o mesmo. Não vou falar sobre isso em detalhes e nem expor essa utilização na programação ABAP. Quero apenas trazer e deixar em mente a seguinte mensagem: **Princípio da Responsabilidade Única**. Isso quer dizer que uma subrotina (método, função, form, define, module e etc) vai fazer apenas o que eu programar ela para fazer. Reforço que, conforme o tempo de atendimento, escopo e disponibilidade (assunto para outro post), é possível fazer muita utilização interessante desses conceitos no programa ABAP.

Métodos são fragmentos de códigos que têm responsabilidades únicas.

## Dados x Informações

Eu nunca me esqueço de uma aula na faculdade sobre Dados e Informações, onde entendi que informações não são nada mais que dados organizados de forma a prover tomada de decisões (claro que essa é a minha interpretação e entendimento, podendo ser melhorada/corrigida/completada por qualquer um). Um entendimento/conceito simples, mas que faz uma grande diferença ao desenvolver uma solução. Sem organização, os dados não passam de um monte de valores em colunas. Prover uma boa interpretação é algo que tem grande peso e significado, principalmente em relatório, como no caso da nossa solução desse post.

## Três principais passos para esse ALV

1.  Buscar Dados
2.  Processar Dados
3.  Exibir Informações

### Buscar Dados

Nessa rotina, vai ser feita uma busca simples que, para o nosso exemplo, trará dados de duas tabelas diferentes: `snwd_bpa` (EPM: Business Partners) e `snwd_ad` (EPM: Address Table). Faremos uma busca na tabela de Parceiros e também na tabela de Endereço, ao final exibindo o endereço dos respectivos.

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
Processar Dados
Depois de termos os dados, vamos organizar de forma que nossa saída de relatório tenha as informações correspondentes de cada parceiro. Nesta parte, vamos organizar os dados e para isso usaremos um comando bem particular (pelo menos com esse mesmo propósito) da linguagem de programação SAP ABAP, o read table.

O read table nos permite fazer um acesso a um único registro, usando termos de igualdade e retornando um registro de uma matriz (no caso do ABAP, uma Work Area de uma Internal Table).

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
          out_line-postal_code  = ad_line-postal_code .
          out_line-street       = ad_line-street .
          append out_line to out_tab .
          clear out_line .
        endif .
      endloop .
    endif .
```

### Exibir dados
Eu gosto de utilizar o método factory da classe `cl_salv_table` porque eu tenho um melhor acesso às funcionalidades ALV via métodos e o código fica menos verboso.

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
Neste post, usamos poucas linhas de código para contemplar uma rotina para buscar dados, uma de organizá-los e outra para exibi-las com SAP ALV Reports.

O intuito foi ter uma ideia básica de como é feita a maioria dos relatórios. Por termos um framework robusto como SAP ALV Reports, as rotinas usam os três passos básicos e as sub-rotinas necessárias (ou não) dentro de cada um deles. Claro que o código foi feito de maneira mais simples e está disponível no GitHub para melhorias/atualizações/correções, assim como a documentação do mesmo.


Espero que, de alguma forma, este conteúdo tenha sido produtivo para alguém.


