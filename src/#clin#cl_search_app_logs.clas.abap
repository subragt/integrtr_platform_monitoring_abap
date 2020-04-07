class /CLIN/CL_SEARCH_APP_LOGS definition
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
  data MV_CHECK type CHAR1 .
  data MV_FILTER type STRING .

  methods CONSTRUCTOR
    importing
      !IO_REQUEST type ref to IF_HTTP_REQUEST
      !IO_RESPONSE type ref to IF_HTTP_RESPONSE .
  methods READ_FROM_URI
    exporting
      value(RS_FILTER_STRING) type STRING
    returning
      value(RS_LOG_FILTER) type BAL_S_LFIL .
  methods READ_LOGS_FROMDB .
  methods READ_TEXT .
protected section.
private section.
ENDCLASS.



CLASS /CLIN/CL_SEARCH_APP_LOGS IMPLEMENTATION.


  METHOD /clin/if_slg1rest~handle_request.
    DATA:

      ls_st_logstr       TYPE /clin/st_logstr,
      lt_st_logstr       TYPE TABLE OF /clin/st_logstr,
      ls_st_filterstr    TYPE /clin/st_logstr,
      lt_st_filterstr    TYPE TABLE OF /clin/st_logstr,
      ls_actuallogs_prev TYPE /clin/st_bapiret1,
      lv_line            TYPE int8,
      lo_json_serializer TYPE REF TO cl_trex_json_serializer,
      l_json             TYPE string,
      lv_string_writer   TYPE REF TO cl_sxml_string_writer,
      lv_xstring         TYPE xstring,
      lv_filter          TYPE string,
      lt_bal_log         TYPE bal_r_obj,
      ls_bal_log         TYPE bal_s_obj,
      lt_filter          TYPE TABLE OF string,
      lt_filter_string   TYPE TABLE OF string,
      lt_filter_param    TYPE string, " using it for looping statement ergo making it lt.
      lv_filter_param1   TYPE string,
      lv_filter_param2   TYPE string,
      lv_filter_param3   TYPE string,
      lv_filter_lines    TYPE int8.

*   Read values from the URI query
    CALL METHOD me->read_from_uri
      IMPORTING
        rs_filter_string = lv_filter
      RECEIVING
        rs_log_filter    = ms_log_filter.



    lt_bal_log = ms_log_filter-object.

*    LOOP at lt_bal_log into ls_bal_log .
*    lv_filter = ls_bal_log-high.
*    endloop.


*   Search on DB for these logs
    CALL METHOD me->read_logs_fromdb.

*   Read text out of msg handles
    CALL METHOD me->read_text.
    CHECK mv_check IS INITIAL.
*   Sort and delete to ensure redundant entries with the combination-msg class,msg number and
*   employee number are deleted from int. table
    SORT mt_actuallogs BY id number extnumber.
    DELETE ADJACENT DUPLICATES FROM mt_actuallogs COMPARING id number extnumber.
    ls_st_logstr-count = 1.

    LOOP AT  mt_actuallogs INTO ms_actuallogs.

      IF ms_actuallogs-id EQ ls_actuallogs_prev-id AND ms_actuallogs-number EQ ls_actuallogs_prev-number AND sy-tabix NE 1.
        ls_st_logstr-count = ls_st_logstr-count + 1.
        MODIFY  lt_st_logstr INDEX lv_line  FROM  ls_st_logstr TRANSPORTING count.
      ELSE.
        MOVE-CORRESPONDING ms_actuallogs TO   ls_st_logstr.
        ls_st_logstr-count = 1.
        APPEND ls_st_logstr   TO lt_st_logstr .
        DESCRIBE TABLE lt_st_logstr LINES lv_line.
      ENDIF.
      ls_actuallogs_prev = ms_actuallogs.
    ENDLOOP.

    TRY.
*        CREATE OBJECT lo_json_serializer
*          EXPORTING
*            data = lt_st_logstr.
*        CALL METHOD lo_json_serializer->serialize.
*
*        CALL METHOD lo_json_serializer->get_data
*          RECEIVING
*            rval = l_json.
*        CALL METHOD me->/clin/if_slg1rest~response->set_header_field( name = 'Content-type' value = 'application/json; charset=iso-8859-1' ).
*        CALL METHOD me->/clin/if_slg1rest~response->set_cdata( data = l_json ) .

**       Convert output to json
        SPLIT lv_filter AT 'and' INTO TABLE lt_filter_string.
        LOOP AT lt_filter_string INTO lt_filter_param.
          SPLIT lt_filter_param AT `'` INTO TABLE lt_filter.

          IF lt_filter IS NOT INITIAL.
            DESCRIBE TABLE lt_filter LINES lv_filter_lines.

            LOOP AT lt_filter INTO lt_filter_param.
              SHIFT lt_filter_param LEFT DELETING LEADING space .
              CONDENSE lt_filter_param.
              IF lt_filter_param EQ 'count le' OR lt_filter_param EQ 'count lt' OR lt_filter_param EQ 'count gt' OR lt_filter_param EQ 'count ge' OR lt_filter_param EQ 'count eq' OR lt_filter_param EQ 'count ne' OR lt_filter_param EQ 'message eq' OR
lt_filter_param EQ 'id eq' OR lt_filter_param EQ 'number gt' OR lt_filter_param EQ 'number ge' OR lt_filter_param EQ 'number eq' OR lt_filter_param EQ 'number lt' OR lt_filter_param EQ 'number le' .
                lv_filter_param1 = lt_filter_param.
              ELSEIF lt_filter_param <> 'and' OR lt_filter_param <> 'or'.
                IF lv_filter_param3 IS INITIAL.
                  lv_filter_param3 = lt_filter_param.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
          " if the filter is count with operator being greater than.
          IF lv_filter_param1 EQ 'count gt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-count GT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          " if the filter is count with operator being greater than or equal to.
          IF lv_filter_param1 EQ 'count ge'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-count GE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.


          " if the filter is count with operater being lesser than
          IF lv_filter_param1 EQ 'count lt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-count LT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.


          " if the filter is count with operater being lesser than or equal
          IF lv_filter_param1 EQ 'count le'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-count LE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          IF lv_filter_param1 EQ 'count eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-count EQ lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          IF lv_filter_param1 EQ 'message eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              CONDENSE ls_st_logstr-message.
              IF ls_st_logstr-message EQ lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          IF lv_filter_param1 EQ 'id eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              CONDENSE ls_st_logstr-id.
              IF ls_st_logstr-message EQ lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.



          " if the filter is count with operator being greater than.
          IF lv_filter_param1 EQ 'number gt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-number GT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          " if the filter is count with operator being greater than or equal to.
          IF lv_filter_param1 EQ 'number ge'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-number GE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.


          " if the filter is count with operater being lesser than
          IF lv_filter_param1 EQ 'number lt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-number LT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.


          " if the filter is count with operater being lesser than or equal
          IF lv_filter_param1 EQ 'number le'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr .
              IF ls_st_logstr-number LE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.
          ENDIF.

          CLEAR lv_filter_param1.
          CLEAR lv_filter_param3.

        ENDLOOP.

        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id SOURCE tab =  lt_st_logstr RESULT XML lv_string_writer.

        lv_xstring = lv_string_writer->get_output( ).
        me->/clin/if_slg1rest~response->set_data( data = lv_xstring ).
      CATCH cx_root.
*     xxx
    ENDTRY.

  ENDMETHOD.


  METHOD /clin/if_slg1rest~set_response.

    CALL METHOD me->/clin/if_slg1rest~response->set_data
      EXPORTING
        data = is_data.

  ENDMETHOD.


  METHOD constructor.
    me->/clin/if_slg1rest~response = io_response.
    me->/clin/if_slg1rest~request = io_request.

  ENDMETHOD.


  METHOD read_from_uri.

    DATA: ls_log_obj_wa  TYPE bal_s_obj,
          lt_log_obj_tab TYPE bal_r_obj,
          lv_fdatetime   TYPE string,
          lv_tdatetime   TYPE string,
          lv_fdt         TYPE sy-datum,
          lv_tdt         TYPE sy-datum,
          lv_diff        TYPE i,
          lv_filter      TYPE string.

*   Read values from te URI query
    ls_log_obj_wa-sign   = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_log_obj_wa-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_log_obj_wa-low    = me->/clin/if_slg1rest~request->get_form_field('logObj').
    ls_log_obj_wa-high     = me->/clin/if_slg1rest~request->get_form_field('filter') .
    APPEND ls_log_obj_wa TO lt_log_obj_tab.

    lv_fdatetime = me->/clin/if_slg1rest~request->get_form_field('fromDatetime').
    lv_tdatetime = me->/clin/if_slg1rest~request->get_form_field('toDatetime').
    lv_filter    = me->/clin/if_slg1rest~request->get_form_field('filter') .
    RS_FILTER_STRING = lv_filter.

    CONCATENATE lv_fdatetime+0(4) lv_fdatetime+5(2) lv_fdatetime+8(2) INTO rs_log_filter-date_time-date_from.
    CONCATENATE lv_fdatetime+11(2) lv_fdatetime+14(2) lv_fdatetime+17(2) INTO rs_log_filter-date_time-time_from .
    lv_fdt = rs_log_filter-date_time-date_from.

*    rs_log_filter-date_time-date_from = lv_fdatetime+0(8).
*    rs_log_filter-date_time-time_from = lv_fdatetime+8(6).
*    rs_log_filter-date_time-date_to = lv_tdatetime+0(8).
*    rs_log_filter-date_time-time_to = lv_tdatetime+8(6).

    CONCATENATE lv_tdatetime+0(4) lv_tdatetime+5(2) lv_tdatetime+8(2) INTO rs_log_filter-date_time-date_to.
    CONCATENATE lv_tdatetime+11(2) lv_tdatetime+14(2) lv_tdatetime+17(2) INTO rs_log_filter-date_time-time_to .
    lv_tdt = rs_log_filter-date_time-date_to.
    rs_log_filter-object = lt_log_obj_tab.


    CALL FUNCTION 'C14B_DIFF_BT_2_DATES'
      EXPORTING
        i_date_from               = lv_fdt
        i_date_to                 = lv_tdt
      IMPORTING
        e_days                    = lv_diff
*       E_MONTHS                  =
*       E_YEARS                   =
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lv_diff > 120 .
      CALL METHOD me->/clin/if_slg1rest~response->set_status( code = '400' reason = 'The server cannot or will not process the request -Date range is high,range has to be 4 months max' ).
      mv_check = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD read_logs_fromdb.

    CHECK mv_check IS INITIAL.
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

  ENDMETHOD.


  METHOD read_text.
    DATA:ls_message     TYPE bal_s_msg,
         ls_return      TYPE bapiret1,
         ls_return_prev TYPE bapiret1.

    CHECK mv_check IS INITIAL.
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
        IF ls_message-msgty EQ me->/clin/if_slg1rest~request->get_form_field('TYPE') .
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
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
