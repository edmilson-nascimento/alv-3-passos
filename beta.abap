
REPORT ytest .


CLASS class_report DEFINITION .

  PUBLIC SECTION .

    TYPES:
      BEGIN OF ty_out,
        bp_id         TYPE snwd_bpa-bp_id,
        company_name  TYPE snwd_bpa-company_name,
        currency_code TYPE snwd_bpa-currency_code,
        web_address   TYPE snwd_bpa-web_address,
        email_address TYPE snwd_bpa-email_address,
        country       TYPE snwd_ad-country,
        city          TYPE snwd_ad-city,
        postal_code   TYPE snwd_ad-postal_code,
        street        TYPE snwd_ad-street,
      END OF ty_out,

      tab_out     TYPE TABLE OF ty_out,
      range_bp_id TYPE RANGE OF snwd_bpa-bp_id,
      tab_bpa     TYPE TABLE OF snwd_bpa, " Address Table
      tab_ad      TYPE TABLE OF snwd_ad.  " Business Partners

    METHODS buscar_dados
      IMPORTING
        !bp_id   TYPE class_report=>range_bp_id
      CHANGING
        !bpa_tab TYPE class_report=>tab_bpa
        !ad_tab  TYPE class_report=>tab_ad .

    METHODS processar_dados
      IMPORTING
        !bpa_tab TYPE class_report=>tab_bpa
        !ad_tab  TYPE class_report=>tab_ad
      CHANGING
        !out_tab TYPE class_report=>tab_out .

    METHODS exibir_informacoes
      CHANGING
        !out_tab TYPE class_report=>tab_out .

  PROTECTED SECTION .

  PRIVATE SECTION .

ENDCLASS .


CLASS class_report IMPLEMENTATION .

  METHOD buscar_dados .

    REFRESH:
      bpa_tab, ad_tab .

    IF ( lines( bp_id ) EQ 0 ) .
    ELSE .

      SELECT *
        INTO TABLE bpa_tab
        FROM snwd_bpa
       WHERE bp_id IN bp_id .

      IF ( sy-subrc EQ 0 ) .

        SELECT *
          INTO TABLE ad_tab
          FROM snwd_ad
           FOR ALL ENTRIES IN bpa_tab
         WHERE node_key EQ bpa_tab-address_guid .

        IF ( sy-subrc EQ 0 ) .
        ENDIF .

      ENDIF .

    ENDIF .

  ENDMETHOD .


  METHOD processar_dados .

    DATA:
      out_line TYPE class_report=>ty_out .

    REFRESH out_tab .

    IF ( lines( bpa_tab ) GT 0 ) AND
       ( lines( ad_tab )  GT 0 ) .

      LOOP AT bpa_tab INTO DATA(bpa_line) .

        out_line-bp_id         = bpa_line-bp_id .
        out_line-company_name  = bpa_line-company_name .
        out_line-currency_code = bpa_line-currency_code .
        out_line-web_address   = bpa_line-web_address .
        out_line-email_address = bpa_line-email_address .

        READ TABLE ad_tab INTO DATA(ad_line)
          WITH KEY node_key = bpa_line-address_guid .

        IF ( sy-subrc EQ 0 ) .

          out_line-country     = ad_line-country .
          out_line-city        = ad_line-city .
          out_line-postal_code = ad_line-postal_code .
          out_line-street      = ad_line-street .

          APPEND out_line TO out_tab .
          CLEAR  out_line .

        ENDIF .

      ENDLOOP .

    ENDIF .

  ENDMETHOD .


  METHOD exibir_informacoes .

    DATA:
      salv_table TYPE REF TO cl_salv_table,
      columns    TYPE REF TO cl_salv_columns_table,
      display    TYPE REF TO cl_salv_display_settings.


    IF ( lines( out_tab ) EQ 0 ) .
      RETURN .
    ENDIF .

    TRY .

        cl_salv_table=>factory(
*             exporting
*             list_display = if_salv_c_bool_sap=>true
          IMPORTING
            r_salv_table = salv_table
          CHANGING
            t_table      = out_tab
        ) .

*         Otimizar largura da columa
        columns = salv_table->get_columns( ) .
        IF ( columns IS BOUND ) .
          columns->set_optimize( cl_salv_display_settings=>true ).
        ENDIF .

*          data(column) =

*         Usando Status
        salv_table->set_screen_status(
          pfstatus      = 'STANDARD_FULLSCREEN'
          report        = 'SAPLKKBL'
          set_functions = salv_table->c_functions_all
        ).

*         Layout de Zebra
        display = salv_table->get_display_settings( ) .
        IF ( display IS BOUND ) .
          display->set_striped_pattern( cl_salv_display_settings=>true ) .
        ENDIF .

        salv_table->display( ).

      CATCH cx_salv_msg .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
      CATCH cx_salv_object_not_found .

    ENDTRY.

  ENDMETHOD .


ENDCLASS .

* Evento para chamada dos metodos
INITIALIZATION .


  DATA:
    filtro     TYPE class_report=>range_bp_id,
    alv_report TYPE REF TO class_report,
    bpa_table  TYPE class_report=>tab_bpa,
    ad_table   TYPE class_report=>tab_ad,
    out_table  TYPE class_report=>tab_out.

* Essa opcao pode ser substituida por um parametro de selecao
  filtro =
    VALUE #(
     ( sign = 'I' option = 'EQ' low = '0100000000' )
     ( sign = 'I' option = 'EQ' low = '0100000001' )
     ( sign = 'I' option = 'EQ' low = '0100000002' )
     ( sign = 'I' option = 'EQ' low = '0100000003' )
     ( sign = 'I' option = 'EQ' low = '0100000004' )
     ( sign = 'I' option = 'EQ' low = '0100000005' )
    ) .


* Criando objeto
  alv_report = NEW class_report( ) .

* Verificando se foi criado o objeto
  IF ( alv_report IS BOUND ) .

    alv_report->buscar_dados(
      EXPORTING
        bp_id   = filtro
      CHANGING
        bpa_tab = bpa_table
        ad_tab  = ad_table
    ).

    alv_report->processar_dados(
      EXPORTING
        bpa_tab = bpa_table
        ad_tab  = ad_table
      CHANGING
        out_tab = out_table
    ).

    alv_report->exibir_informacoes(
      CHANGING
        out_tab = out_table
    ).

  ENDIF .