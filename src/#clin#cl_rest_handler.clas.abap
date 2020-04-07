class /CLIN/CL_REST_HANDLER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.

  methods GET_REST
    importing
      !IO_SERVER type ref to IF_HTTP_SERVER
      !IV_LINE type INT8 optional
      !IV_FETCHTYPE type STRING optional
    returning
      value(EO_REST) type ref to /CLIN/IF_SLG1REST .
ENDCLASS.



CLASS /CLIN/CL_REST_HANDLER IMPLEMENTATION.


  METHOD get_rest.
***************************************************************************
    " VARIABLES
***************************************************************************
    DATA: lv_class_name           TYPE seoclsname.
    DATA: lv_request_method       TYPE string.
*    Commeted by Akash for replacing parameter check by line to fetchType
*    CASE iv_line.
*
*      WHEN 5.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS'.
*      WHEN 6.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS'.
*      WHEN 7.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS1'.
*      WHEN 8.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS1'.
*      WHEN 2.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS2'.
*      WHEN 3.
*        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS3'.
*      WHEN OTHERS.
*
*    ENDCASE.

    CASE iv_fetchtype.

      WHEN 'aggregate_logs'.
        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS'.
      WHEN 'filtered_logs'.
        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS1'.
      WHEN 'filter_log_number'.
        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS3'.
      WHEN 'fetch_long_text'.
        lv_class_name = '/CLIN/CL_SEARCH_APP_LOGS4'.
      WHEN OTHERS.

    ENDCASE.

    TRY.
        CREATE OBJECT eo_rest
        TYPE (lv_class_name)
        EXPORTING
        io_request   = io_server->request
        io_response  = io_server->response.
      CATCH cx_sy_create_object_error.
    ENDTRY.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    DATA: lo_rest_class      TYPE REF TO /clin/if_slg1rest,
          lo_error           TYPE REF TO cx_root,
          lv_reason          TYPE string,
          lv_action          TYPE string,
          ls_response        TYPE /clin/st_longtext,
          lo_json_serializer TYPE REF TO cl_trex_json_serializer,
          l_json             TYPE string,
          lt_srt             TYPE tihttpnvp,
          ls_srt             TYPE ihttpnvp,
          lv_srt             TYPE tihttpnvp,
          lv_line            TYPE int8,
          lv_replication     TYPE char15,
          path_info          TYPE string,
          lt_data            TYPE TABLE OF string,
          lv_fetchType       type string.

    TRY.

        lv_action = server->request->get_header_field( name = '~request_method').
        IF lv_action NE 'GET'.
          CALL METHOD server->response->set_status( code = '405' reason = 'Action not allowed' ).
          RETURN.
        ENDIF.
        path_info = server->request->get_header_field( name = '~path_info' ).
        server->request->get_form_fields_cs( EXPORTING search_option = 0 CHANGING fields =  lt_srt  ).
        server->request->get_uri_parameter( EXPORTING name = 'logObj' RECEIVING value =  path_info  ).
        loop at lt_srt into ls_srt.
          if ls_srt-name eq 'fetchType'.
            lv_fetchType = ls_srt-value.
            endif.
        endloop.
        DESCRIBE TABLE lt_srt LINES lv_line .

        lo_rest_class ?= get_rest( io_server = server iv_line = lv_line iv_fetchType = lv_fetchtype ).
        lo_rest_class->handle_request( ).


      CATCH cx_root INTO lo_error.
        lv_reason = lo_error->get_text( ).
        server->response->set_status( code = 500
        reason = lv_reason ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
