REPORT zmodel_alv.
TABLES: ekko, ekpo, ekkn, KONP.

selection-screen begin of block blc01 with frame title text-001.
    select-options:
        so_ebeln for ekko-ebeln,
        so_bedat for ekko-bedat,
        so_pspnr for ekkn-PS_PSP_PNR.
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
        pspnr         type ekkn-PS_PSP_PNR, " Elemento PEP

      END OF ty_out,

    tab_out TYPE TABLE OF ty_out.

    METHODS buscar_dados
      IMPORTING
        !ekko_id TYPE FIP_T_EBELN_RANGE
        !ekko_bedat type WRF_PBAS_BEDAT_RTTY
        !ekkn_pspnr type HRPP_SEL_PS_PSP_PNR
      CHANGING
        !ekko_tab TYPE me_ekko
        !ekpo_tab TYPE ME_EKPO
        !ekkn_tab type ME_EKKN.

    METHODS processar_dados
      IMPORTING
        !ekko_tab TYPE me_ekko
        !ekpo_tab TYPE ME_EKPO
        !ekkn_tab type ME_EKKN
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
      ekko_tab, ekpo_tab, ekkn_tab.

    " Retorna quantidade de linhs de uma tabela interna
    DATA(lv_quantidade_de_lines) = lines( ekko_id ).

    " IF ( lines( bp_id ) = 0 ).
    IF ( lv_quantidade_de_lines = 0 ).
      RETURN.
    ENDIF.

    SELECT * INTO TABLE ekko_tab
      FROM ekko
      WHERE ebeln IN ekko_id
        and bedat in ekko_bedat.

    IF ( sy-subrc <> 0 ).
      RETURN.
    ENDIF.
" filtros where
"so_ebeln - somente este está implementado
"so_bedat
"so_pspnr

    SELECT * INTO TABLE ekpo_tab
      FROM ekpo
      FOR ALL ENTRIES IN ekko_tab
      WHERE ebeln = ekko_tab-ebeln.

    SELECT * INTO TABLE ekkn_tab
      FROM ekkn
      FOR ALL ENTRIES IN ekko_tab
      WHERE ebeln = ekko_tab-ebeln.

  ENDMETHOD.


  METHOD processar_dados .

    DATA:
      out_line TYPE class_report=>ty_out,
      W_TAX TYPE TAXCOM.

    data tkomv type TABLE OF komv.

    REFRESH out_tab .

    IF ( lines( ekko_tab ) GT 0 ) AND
       ( lines( ekpo_tab ) GT 0 ) .

      LOOP AT ekpo_tab INTO DATA(ekpo_line) .
        CLEAR W_TAX.

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

            READ TABLE ekkn_tab INTO DATA(ekkn_line)
             WITH KEY ebeln = ekpo_line-ebeln
                       ebelp = ekpo_line-ebelp .

             IF ( sy-subrc EQ 0 ) .
               out_line-pspnr = ekkn_line-PS_PSP_PNR.
             endif.

*            -------------BUSCAR PREÇO BRUTO--------------------------*
             W_TAX-bukrs = ekpo_line-bukrs.
             W_TAX-budat = ekko_line-bedat.
             W_TAX-waers = ekko_line-waers.
             W_TAX-kposn = ekpo_line-ebelp.
             W_TAX-mwskz = ekpo_line-mwskz.
             W_TAX-txjcd = ekpo_line-txjcd.
             W_TAX-ebeln = ekpo_line-ebeln.                              "N1427028
             W_TAX-ebelp = ekpo_line-ebelp.                              "N1427028
             W_TAX-shkzg = 'H'.
             W_TAX-xmwst = 'X'.
             IF ekpo_line-bstyp EQ 'F'.
               W_TAX-wrbtr = ekpo_line-netwr.
             ELSE.
               W_TAX-wrbtr = ekpo_line-zwert.
             ENDIF.
             W_TAX-lifnr = ekko_line-lifnr.
             W_TAX-land1 = ekko_line-lands.                              "WIA
             W_TAX-ekorg = ekko_line-ekorg.
*  DEPOIS VER SE NECESSITA           -- W_TAX-hwaer = t001-waers.
             W_TAX-llief = ekko_line-llief.
             W_TAX-bldat = ekko_line-bedat.
             W_TAX-matnr = ekpo_line-matnr.         "HTN-Abwicklung
             W_TAX-werks = ekpo_line-werks.
             W_TAX-bwtar = ekpo_line-bwtar.
             W_TAX-matkl = ekpo_line-matkl.
             W_TAX-meins = ekpo_line-meins.
*             - Mengen richtig fuellen ---------------------------------------------*
             IF ekko_line-bstyp EQ 'F'.
               W_TAX-mglme = ekpo_line-menge.
             ELSE.
                IF ekko_line-bstyp EQ 'K' AND ekpo_line-abmng GT 0.
                  W_TAX-mglme = ekpo_line-abmng.
                ELSE.
                  W_TAX-mglme = ekpo_line-ktmng.
                ENDIF.
             ENDIF.
             IF W_TAX-mglme EQ 0.  "falls keine Menge gesetzt --> auf 1 setzen
               W_TAX-mglme = 1000.  "z.B. bestellte Banf nochmal bestellt
             ENDIF.

             W_TAX-mtart = ekpo_line-mtart.

             CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
               EXPORTING
                 bukrs                = ekpo_line-bukrs
                 component            = 'BR'
               EXCEPTIONS
                 component_not_active = 02.

             IF sy-subrc IS INITIAL.
               CALL FUNCTION 'J_1B_NF_PO_DISCOUNTS'
                 EXPORTING
                   i_kalsm = ekko_line-kalsm
                   i_ekpo  = ekpo_line
                 IMPORTING
                   e_ekpo  = ekpo_line
                 TABLES
                   i_konv  = tkomv                                   "#EC *
                 EXCEPTIONS
                   OTHERS  = 1.

                IF NOT ekko_line-llief IS INITIAL.
                  w_tax-lifnr = ekko_line-llief.
                ENDIF.
             ENDIF.

             CALL FUNCTION 'CALCULATE_TAX_ITEM'
                EXPORTING
                     dialog       = ' '
                     display_only = 'X'
                     i_taxcom     = W_tax
                 importing
                   E_TAXCOM       = w_tax
                 TABLES
                   t_xkomv        = tkomv.


              if ekpo_line-packno IS NOT INITIAL.
                  ekpo_line-brtwr = W_TAX-WRBTR.
              else.
                  ekpo_line-brtwr = W_TAX-WMWST + W_TAX-WRBTR.
              endif.


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
  alv_global_object TYPE REF TO class_report,
  ekko_table        TYPE me_ekko,
  ekpo_table        TYPE ME_EKPO,
  ekkn_table        type me_ekkn,
  out_table         TYPE class_report=>tab_out.



INITIALIZATION.
  " antes de aparecer os filtros
  " informar uma data padrao por exemplo


START-OF-SELECTION.
  " clicou para executar o relatorio (F8)
  " depois de informar os filtros

  " Passo para se buscar os dados

  " Criando objeto
  alv_global_object = NEW class_report( ).

  " Verificando se foi criado o objeto
  IF ( alv_global_object IS BOUND ).

    alv_global_object->buscar_dados( EXPORTING ekko_id  = so_ebeln[]
                                               ekko_bedat = so_bedat[]
                                               ekkn_pspnr = so_pspnr[]
                                     CHANGING  ekko_tab = ekko_table
                                               ekpo_tab = ekpo_table
                                               ekkn_tab = ekkn_table ).

    alv_global_object->processar_dados( EXPORTING ekko_tab = ekko_table
                                                  ekpo_tab = ekpo_table
                                                  ekkn_tab = ekkn_table
                                        CHANGING  out_tab  = out_table ).
  ENDIF.

END-OF-SELECTION.
  " apos o evento START-
  " ainda antes de exibir o ALV

  alv_global_object->exibir_informacoes( CHANGING out_tab = out_table ).