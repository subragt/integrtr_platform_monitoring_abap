class /CLIN/CL_SEARCH_APP_LOGS2 definition
  public
  final
  create public .

public section.

  interfaces /CLIN/IF_SLG1REST .

  data MS_LOG_FILTER type BAL_S_LFIL .
  data MT_LOG_HANDLE type BAL_T_LOGH .
  data MT_MSG_HANDLE type BAL_T_MSGH .
  data MS_MESSAGE_HANDLE type BALMSGHNDL .
  data MS_ACTUALLOGS type /CLIN/ST_BAPIRET1 .
  data MT_ACTUALLOGS type /CLIN/TT_BAPIRET1 .
  data MT_LOG_HEADER_TAB type BALHDR_T .
  data MS_LOG_HEADER type BALHDR .

  methods CONSTRUCTOR
    importing
      !IO_REQUEST type ref to IF_HTTP_REQUEST
      !IO_RESPONSE type ref to IF_HTTP_RESPONSE .
  methods READ_FROM_URI
    returning
      value(RS_LOG_FILTER) type BAL_S_LFIL .
  methods READ_LOGS_FROMDB .
  methods READ_TEXT .
protected section.
private section.
ENDCLASS.



CLASS /CLIN/CL_SEARCH_APP_LOGS2 IMPLEMENTATION.


  METHOD /clin/if_slg1rest~handle_request.
    DATA:
      lv_string_writer   TYPE REF TO cl_sxml_string_writer,
      lv_xstring         TYPE xstring,
      ls_actuallogs_prev TYPE /clin/st_bapiret1,
      lo_json_serializer TYPE REF TO cl_trex_json_serializer,
      l_json             TYPE string.

*   Read values from te URI query=( logNum=00000000000001438779)
    CALL METHOD me->read_from_uri
      RECEIVING
        rs_log_filter = ms_log_filter.

*   Search on DB for these logs
    CALL METHOD me->read_logs_fromdb.

*   Read text out of msg handle
    CALL METHOD me->read_text.

*   Sort and delete to ensure redundant entries with the combination-msg class,msg number,message text and
*   employee number are deleted from int. table
    SORT mt_actuallogs BY id number extnumber.
    DELETE ADJACENT DUPLICATES FROM mt_actuallogs COMPARING id number extnumber.

    TRY.
*        CREATE OBJECT lo_json_serializer
*          EXPORTING
*            data = mt_actuallogs.
*        CALL METHOD lo_json_serializer->serialize.
*
*        CALL METHOD lo_json_serializer->get_data
*          RECEIVING
*            rval = l_json.
*        CALL METHOD me->/clin/if_slg1rest~response->set_header_field( name = 'Content-type' value = 'application/json; charset=iso-8859-1' ).
*        CALL METHOD me->/clin/if_slg1rest~response->set_cdata( data = l_json ) .

**       Convert output to json
        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id SOURCE tab =  mt_actuallogs RESULT XML lv_string_writer.
        lv_xstring = lv_string_writer->get_output( ).
        me->/clin/if_slg1rest~response->set_data( data = lv_xstring ).
      CATCH cx_root.
*     xxx
    ENDTRY.

  ENDMETHOD.


  METHOD /CLIN/IF_SLG1REST~SET_RESPONSE.

    CALL METHOD me->/clin/if_slg1rest~response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.


  METHOD CONSTRUCTOR.
    me->/clin/if_slg1rest~response = io_response.
    me->/clin/if_slg1rest~request = io_request.

  ENDMETHOD.


  METHOD read_from_uri.

    DATA: ls_bal_s_logn TYPE bal_s_logn,
          lt_bal_s_logn TYPE bal_r_logn.

*   Read values from te URI query=( logNum=00000000000001438779)
    ls_bal_s_logn-sign   = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_bal_s_logn-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_bal_s_logn-low    = me->/clin/if_slg1rest~request->get_form_field('logNum').
    APPEND ls_bal_s_logn TO lt_bal_s_logn.

    rs_log_filter-lognumber = lt_bal_s_logn.
  ENDMETHOD.


  method READ_LOGS_FROMDB.

*   Search on DB for these logs
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = ms_log_filter
      IMPORTING
        e_t_log_header     = mt_log_header_tab
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   Load logs from DB
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = mt_log_header_tab
      IMPORTING
        e_t_log_handle     = mt_log_handle
        e_t_msg_handle     = mt_msg_handle
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  endmethod.


  METHOD READ_TEXT.
    DATA:ls_message     TYPE bal_s_msg,
         ls_return      TYPE bapiret1,
         ls_return_prev TYPE bapiret1.

    LOOP AT mt_msg_handle INTO ms_message_handle.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = ms_message_handle
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = ls_message
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2.

      IF sy-subrc = 0.
          CALL FUNCTION 'BALW_BAPIRETURN_GET1'
            EXPORTING
              type       = ls_message-msgty
              cl         = ls_message-msgid
              number     = ls_message-msgno
              par1       = ls_message-msgv1
              par2       = ls_message-msgv2
              par3       = ls_message-msgv3
              par4       = ls_message-msgv4
            IMPORTING
              bapireturn = ls_return.

          ls_return_prev = ls_return.
          MOVE-CORRESPONDING ls_return TO ms_actuallogs.
          ms_actuallogs-log_handle = ms_message_handle-log_handle.
          ms_actuallogs-msgnumber = ms_message_handle-msgnumber.
*         Read the corresponding long handle in order to derive at the external number
          READ TABLE mt_log_header_tab INTO ms_log_header  WITH KEY log_handle = ms_actuallogs-log_handle .
          IF sy-subrc = 0.
            ms_actuallogs-extnumber = ms_log_header-extnumber.
            ms_actuallogs-object = ms_log_header-object.
            ms_actuallogs-subobject = ms_log_header-subobject.
          ENDIF.
          APPEND ms_actuallogs TO mt_actuallogs.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
