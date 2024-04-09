REPORT zmodel_alv.
TABLES: ekko, ekpo, COBL.

selection-screen begin of block blc01 with frame title text-001.
    select-options:
        so_ebeln for ekko-ebeln,
        so_bedat for ekko-bedat,
        so_pspnr for COBL-PS_POSID.
    selection-screen: uline.

selection-screen end of block blc01.

TYPES:
      BEGIN OF ty_ekko_range,
        ebeln TYPE ekko-ebeln,
      END OF ty_ekko_range.

    TYPES:
      tt_range  TYPE TABLE OF ty_ekko_range.

CLASS class_report DEFINITION .

  PUBLIC SECTION .

    TYPES:
      BEGIN OF ty_out,
        mandt         type ekko-mandt,
        ebeln         type ekko-ebeln, " Doc.compras
        bukrs         type ekko-bukrs, " Empresa
        bsart         type ekko-bsart, " Tipo de Doc.
        bedat         type ekko-bedat, " Data Doc.
        lifnr         type ekko-lifnr, " Id Fornecedor
        ebelp         type ekpo-ebelp, " Item pedido
        matnr         type ekpo-matnr, " Id Material
        txz01         type ekpo-txz01, " Texto Breve
        menge         type ekpo-menge, " Qtd.do pedido
        meins         type ekpo-meins, " UM pedido
        bprme         type ekpo-bprme, " Unid. Preço Pedido
        netwr         type ekpo-netwr, " Valor Liq.
        brtwr         type ekpo-brtwr, " Valor Bruto
        loekz         type ekpo-loekz, " Cód. eliminação
        pspnr         type COBL-PS_POSID, " Elemento PEP

      END OF ty_out,

      tab_out        TYPE TABLE OF ty_out,
      range_ebeln_id TYPE RANGE OF ekko-ebeln,
      tab_ekko       TYPE TABLE OF ekko, " Cabeçalho do documento de compra
      tab_ekpo       TYPE TABLE OF ekpo. " Item do documento de compra



    METHODS buscar_dados
      IMPORTING
        !ekko_id TYPE class_report=>range_ebeln_id
      CHANGING
        !ekko_tab TYPE  class_report=>tab_ekko
        !ekpo_tab TYPE  class_report=>tab_ekpo .

    METHODS processar_dados
      IMPORTING
        !ekko_tab TYPE  class_report=>tab_ekko
        !ekpo_tab TYPE  class_report=>tab_ekpo
      CHANGING
        !out_tab TYPE class_report=>tab_out .

    METHODS exibir_informacoes
      CHANGING
        !out_tab TYPE class_report=>tab_out .

  PROTECTED SECTION .

  PRIVATE SECTION .

ENDCLASS .


CLASS class_report IMPLEMENTATION .

  METHOD buscar_dados.

    REFRESH:
      ekko_tab, ekpo_tab.

    " Retorna quantidade de linhs de uma tabela interna
    DATA(lv_quantidade_de_lines) = lines( ekko_id ).

    " IF ( lines( bp_id ) = 0 ).
    IF ( lv_quantidade_de_lines = 0 ).
      RETURN.
    ENDIF.

    SELECT * INTO TABLE ekko_tab
      FROM ekko
      WHERE ebeln IN so_ebeln.

    IF ( sy-subrc <> 0 ).
      RETURN.
    ENDIF.
" filtros where
"so_ebeln - somente este está implementado
"so_bedat
"so_pspnr

    SELECT * INTO TABLE ekpo_tab
      FROM ekpo
      "FOR ALL ENTRIES IN ekpo_tab
      "WHERE ebeln = ekko_tab-ebeln.
      WHERE ebeln in so_ebeln.

  ENDMETHOD.


  METHOD processar_dados .

    DATA:
      out_line TYPE class_report=>ty_out .

    REFRESH out_tab .

    IF ( lines( ekko_tab ) GT 0 ) AND
       ( lines( ekpo_tab ) GT 0 ) .

      LOOP AT ekpo_tab INTO DATA(ekpo_line) .

        out_line-ebelp = ekpo_line-ebelp.
        out_line-matnr = ekpo_line-matnr.
        out_line-txz01 = ekpo_line-txz01.
        out_line-menge = ekpo_line-menge.
        out_line-meins = ekpo_line-meins.
        out_line-bprme = ekpo_line-bprme.
        out_line-netwr = ekpo_line-netwr.
        out_line-brtwr = ekpo_line-brtwr.
        out_line-loekz = ekpo_line-loekz.



        READ TABLE ekko_tab INTO DATA(ekko_line)
          WITH KEY ebeln = ekpo_line-ebeln .

        IF ( sy-subrc EQ 0 ) .
            out_line-mandt = ekko_line-mandt.
            out_line-ebeln = ekko_line-ebeln.
            out_line-bukrs = ekko_line-bukrs.
            out_line-bsart = ekko_line-bsart.
            out_line-bedat = ekko_line-bedat.
            out_line-lifnr = ekko_line-lifnr.

            APPEND out_line TO out_tab .
            CLEAR  out_line .
        ENDIF .

      ENDLOOP .

    ENDIF .

  ENDMETHOD .


  METHOD exibir_informacoes.

    DATA:
      salv_table TYPE REF TO cl_salv_table,
      columns    TYPE REF TO cl_salv_columns_table,
      display    TYPE REF TO cl_salv_display_settings.


    IF ( lines( out_tab ) = 0 ).
    ELSE.

      TRY.

          cl_salv_table=>factory( IMPORTING r_salv_table = salv_table
                                  CHANGING  t_table      = out_tab ).

          " Otimizar largura da columa
          columns = salv_table->get_columns( ).
          IF ( columns IS BOUND ).
            columns->set_optimize( cl_salv_display_settings=>true ).
          ENDIF.

          " Usando Status
          salv_table->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN'
                                         report        = 'SAPLKKBL'
                                         set_functions = salv_table->c_functions_all ).

          " Layout de Zebra
          display = salv_table->get_display_settings( ).
          IF ( display IS BOUND ).
            display->set_striped_pattern( cl_salv_display_settings=>true ).
          ENDIF.

          salv_table->display( ).

        CATCH cx_salv_msg.
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
        CATCH cx_salv_object_not_found.

      ENDTRY.

    ENDIF.

  ENDMETHOD.


ENDCLASS .

" Declaracoes globais
DATA:
  filtro            TYPE class_report=>range_ebeln_id,
  alv_global_object TYPE REF TO class_report,
  ekko_table         TYPE class_report=>tab_ekko,
  ekpo_table          TYPE class_report=>tab_ekpo,
  out_table         TYPE class_report=>tab_out.



INITIALIZATION.
  " antes de aparecer os filtros
  " informar uma data padrao por exemplo

  " Essa opcao pode ser substituida por um parametro de selecao
  filtro =
    VALUE #( sign   = 'I'
             option = 'EQ'
              ( low = '4500257904' )
              ( low = '4500257905' )
              ( low = '4500257906' )
              ( low = '4500257907' )
              ( low = '4500257908' ) ).


START-OF-SELECTION.
  " clicou para executar o relatorio (F8)
  " depois de informar os filtros

  " Passo para se buscar os dados

  " Criando objeto
  alv_global_object = NEW class_report( ).

  " Verificando se foi criado o objeto
  IF ( alv_global_object IS BOUND ).

    alv_global_object->buscar_dados( EXPORTING ekko_id  = filtro
                                     CHANGING  ekko_tab = ekko_table
                                               ekpo_tab = ekpo_table ).

    alv_global_object->processar_dados( EXPORTING ekko_tab = ekko_table
                                                  ekpo_tab = ekpo_table
                                        CHANGING  out_tab  = out_table ).
  ENDIF.

END-OF-SELECTION.
  " apos o evento START-
  " ainda antes de exibir o ALV

  alv_global_object->exibir_informacoes( CHANGING out_tab = out_table ).
