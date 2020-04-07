class /CLIN/CL_SEARCH_APP_LOGS1 definition
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
  data MT_MESSAGE_HANDLE type BAL_T_MSGH .
  data MS_MESSAGE_FILTER type BAL_S_MFIL .
  data MS_STRC_FIL type /CLIN/ST_FIL .
  data MV_CHECK type CHAR1 .

  methods CONSTRUCTOR
    importing
      !IO_REQUEST type ref to IF_HTTP_REQUEST
      !IO_RESPONSE type ref to IF_HTTP_RESPONSE .
  methods READ_FROM_URI
    exporting
      !RS_FILTER_STRING type STRING
      !RS_SORT_STRING type STRING
      !RS_OFFSET type INT8
      !RS_TOP type INT8
      !RS_SESSION_ID type STRING
      !RS_SESSION_ID_ERROR type STRING
    returning
      value(RS_STRC_FIL) type /CLIN/ST_FIL .
  methods READ_LOGS_FROMDB
    importing
      !LV_OFFSET type INT8
      !LV_TOP type INT8 .
  methods READ_TEXT .
  methods VALIDATE
    importing
      !IV_DIFF type I .
protected section.
private section.

  data MT_ST_LOGSTR type /CLIN/TT_LOGSTRPR .
ENDCLASS.



CLASS /CLIN/CL_SEARCH_APP_LOGS1 IMPLEMENTATION.


  METHOD /clin/if_slg1rest~handle_request.
    DATA:
      ls_st_logstr       TYPE /clin/st_logstrpr,
      lt_st_logstr       TYPE TABLE OF /clin/st_logstrpr,
      lt_st_logstr1      TYPE TABLE OF /clin/st_logstrpr,
      ls_st_filterstr    TYPE /clin/st_logstrpr,
      lt_st_filterstr    TYPE TABLE OF /clin/st_logstrpr,
      lo_json_serializer TYPE REF TO cl_trex_json_serializer,
      l_json             TYPE string,
      lv_string_writer   TYPE REF TO cl_sxml_string_writer,
      lv_xstring         TYPE xstring,
      lt_filter_string   TYPE TABLE OF string,
      lt_filter_param    TYPE string, " using it for looping statement ergo making it lt.
      lt_filter          TYPE TABLE OF string,
      lv_filter_param1   TYPE string,
      lv_filter_param2   TYPE string,
      lv_filter_param3   TYPE string,
      lv_filter_lines    TYPE int8,
      lv_filter          TYPE string,
      lv_sort            TYPE string,
      lt_sort_string     TYPE TABLE OF string,
      lt_sort_param      TYPE string,
      lt_sort            TYPE TABLE OF string,
      lv_sort_param1     TYPE string,
      lv_sort_param2     TYPE string,
      lv_offset          TYPE int8,
      lv_top             TYPE int8,
      lv_sessionid       type string.

*   Read values from the URI query ID and NUMBER
    CALL METHOD me->read_from_uri
      IMPORTING
        rs_filter_string = lv_filter
        rs_sort_string   = lv_sort
        rs_offset        = lv_offset
        rs_top           = lv_top
        rs_session_id    = lv_sessionid
      RECEIVING
        rs_strc_fil      = ms_strc_fil.


*   Search on DB for these logs
    if sy-subrc eq 0.
    CALL METHOD me->read_logs_fromdb
      EXPORTING
        lv_offset = lv_offset
        lv_top    = lv_top.
    endif.
*   Read text out of msg handle
    CALL METHOD me->read_text.

*   Sort and delete to ensure redundant entries with the combination-msg class,msg number,message text and
*   employee number are deleted from int. table
    CHECK mv_check IS INITIAL.
    SORT mt_actuallogs BY id number extnumber.
    DELETE ADJACENT DUPLICATES FROM mt_actuallogs COMPARING id number extnumber.

    LOOP AT  mt_actuallogs INTO ms_actuallogs.
      MOVE-CORRESPONDING ms_actuallogs TO   ls_st_logstr.
      APPEND ls_st_logstr   TO lt_st_logstr .
    ENDLOOP.

    EXPORT mt_st_logstr = lt_st_logstr TO DATABASE indx(ar) CLIENT sy-mandt ID 'ZCRPROP2'.

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
*       Convert output to json

*       Start the conditions for filter
        SPLIT lv_filter AT 'and' INTO TABLE lt_filter_string.
        LOOP AT lt_filter_string INTO lt_filter_param.
          SPLIT lt_filter_param AT `'` INTO TABLE lt_filter.

          IF lt_filter IS NOT INITIAL.
*            DESCRIBE TABLE lt_filter LINES lv_filer_lines.
            LOOP AT lt_filter INTO lt_filter_param.
              SHIFT lt_filter_param LEFT DELETING LEADING space.
              CONDENSE lt_filter_param.

              IF lt_filter_param EQ 'LOG_NO eq' OR lt_filter_param EQ 'external_number eq' OR lt_filter_param EQ 'external_number gt' OR lt_filter_param EQ 'external_number ge' OR lt_filter_param EQ 'external_number lt' OR lt_filter_param EQ
'external_number le' OR lt_filter_param EQ 'message eq'.
                lv_filter_param1 = lt_filter_param.
              ELSEIF lt_filter_param <> 'and' OR lt_filter_param <> 'or'.
                IF lv_filter_param3 IS INITIAL.
                  lv_filter_param3 = lt_filter_param.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF lv_filter_param1 EQ 'external_number eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-extnumber EQ lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'external_number gt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-extnumber GT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'external_number ge'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-extnumber GE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'external_number lt'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-extnumber LT lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'external_number le'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-extnumber LE lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'LOG_NO eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
              IF ls_st_logstr-log_no EQ lv_filter_param3.
                ls_st_filterstr = ls_st_logstr.
                APPEND ls_st_filterstr TO lt_st_filterstr.
                CLEAR ls_st_filterstr.
              ENDIF.
            ENDLOOP.
            CLEAR lt_st_logstr.
            LOOP AT lt_st_filterstr INTO ls_st_filterstr.
              APPEND ls_st_filterstr TO lt_st_logstr.
            ENDLOOP.

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

          IF lv_filter_param1 EQ 'message eq'.
            CLEAR lt_st_filterstr.
            LOOP AT lt_st_logstr INTO ls_st_logstr.
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

            CLEAR lv_filter_param1.
            CLEAR lv_filter_param3.
          ENDIF.

        ENDLOOP.
*       End the conditions for filter

*       Start sorting of table
        IF lv_sort IS NOT INITIAL.
          CASE lv_sort.

            WHEN 'external_number by desc'.
              SORT lt_st_logstr DESCENDING BY extnumber.
            WHEN 'external_number by asc'.
              SORT lt_st_logstr ASCENDING BY extnumber.
            WHEN 'LOG_NO by desc'.
              SORT lt_st_logstr DESCENDING BY log_no.
            WHEN 'LOG_NO by asc'.
              SORT lt_st_logstr ASCENDING BY log_no.
            WHEN OTHERS.
          ENDCASE.

        ENDIF.
*       End sorting of table

*       start of pagination
*        IF lv_offset IS INITIAL.
*          lv_offset = 1.
*        ENDIF.
*        IF lv_top IS INITIAL.
*          lv_top = 9999 .
*        ENDIF.
*        APPEND LINES OF lt_st_logstr FROM lv_offset TO lv_top TO lt_st_filterstr.
*        CLEAR lt_st_logstr.
*        APPEND LINES OF lt_st_filterstr TO lt_st_logstr.
*        select * from lt_st_filterstr into table lt_st_logstr offset 0 up to 10 rows .
*       end of pagination

        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id SOURCE tab =  lt_st_logstr RESULT XML lv_string_writer.
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

    DATA: ls_bal_s_msid  TYPE bal_s_msid,
          lt_bal_s_msid  TYPE bal_r_msid,
          ls_bal_s_msno  TYPE bal_s_msno,
          lt_bal_s_msno  TYPE bal_r_msno,
          ls_log_obj_wa  TYPE bal_s_obj,
          lt_log_obj_tab TYPE bal_r_obj,
          ls_bal_s_msty  TYPE bal_s_msty,
          lt_bal_s_msty  TYPE bal_r_msty,
          lv_fdatetime   TYPE string,
          lv_tdatetime   TYPE string,
          lv_fdt         TYPE sy-datum,
          lv_tdt         TYPE sy-datum,
          lv_diff        TYPE i,
          lv_filter      TYPE string,
          lv_sort        TYPE string,
          lv_offset      TYPE int8,
          lv_top         TYPE int8,
          lv_fetch_size  TYPE int8,
          lv_sessionid   type string.


*    Read values from te URI query=( logObj eq ‘ECPAO_INʼ AND fromDate gt 01.01.2020 and toDate lt 31.03.2020
    ls_log_obj_wa-sign   = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_log_obj_wa-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_log_obj_wa-low    = me->/clin/if_slg1rest~request->get_form_field('logObj').

    APPEND ls_log_obj_wa TO lt_log_obj_tab.
    rs_strc_fil-lfil-object = lt_log_obj_tab.

    ls_bal_s_msid-sign = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_bal_s_msid-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_bal_s_msid-low = me->/clin/if_slg1rest~request->get_form_field('ID').
    APPEND  ls_bal_s_msid TO lt_bal_s_msid.

    ls_bal_s_msno-sign = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_bal_s_msno-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_bal_s_msno-low = me->/clin/if_slg1rest~request->get_form_field('NUMBER').
    APPEND  ls_bal_s_msno TO lt_bal_s_msno.

    ls_bal_s_msty-sign = cl_ehsgbc_constants=>gc_rangetable_sign_include. "I
    ls_bal_s_msty-option = cl_ehsgbc_constants=>gc_rangetable_option_equal. "EQ
    ls_bal_s_msty-low = me->/clin/if_slg1rest~request->get_form_field('TYPE').
    APPEND  ls_bal_s_msty TO lt_bal_s_msty.

    rs_strc_fil-mfil-msgid = lt_bal_s_msid[].
    rs_strc_fil-mfil-msgno = lt_bal_s_msno[].
    rs_strc_fil-mfil-msgty = lt_bal_s_msty[].

    rs_session_id = me->/clin/if_slg1rest~request->get_form_field('session').

    lv_fdatetime = me->/clin/if_slg1rest~request->get_form_field('fromDatetime').
    lv_tdatetime = me->/clin/if_slg1rest~request->get_form_field('toDatetime').

    CONCATENATE lv_fdatetime+0(4) lv_fdatetime+5(2) lv_fdatetime+8(2) INTO  rs_strc_fil-lfil-date_time-date_from.
    CONCATENATE lv_fdatetime+11(2) lv_fdatetime+14(2) lv_fdatetime+17(2) INTO rs_strc_fil-lfil-date_time-time_from.
    lv_fdt = rs_strc_fil-lfil-date_time-date_from.

    lv_filter = me->/clin/if_slg1rest~request->get_form_field('filter').
    lv_sort   = me->/clin/if_slg1rest~request->get_form_field('sort').
    lv_offset = me->/clin/if_slg1rest~request->get_form_field('offset').
    lv_top    = me->/clin/if_slg1rest~request->get_form_field('top').
    rs_filter_string = lv_filter.
    rs_sort_string   = lv_sort.
    rs_offset        = lv_offset.
    rs_top           = lv_top.

    lv_fetch_size = lv_offset + lv_top.
    rs_strc_fil-lfil-max_nr_logs = lv_fetch_size.

    CONCATENATE lv_tdatetime+0(4) lv_tdatetime+5(2) lv_tdatetime+8(2) INTO rs_strc_fil-lfil-date_time-date_to.
    CONCATENATE lv_tdatetime+11(2) lv_tdatetime+14(2) lv_tdatetime+17(2) INTO rs_strc_fil-lfil-date_time-time_to .
    lv_tdt = rs_strc_fil-lfil-date_time-date_to.

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
    DATA total_size TYPE int8.

    CHECK mv_check IS INITIAL.
    IMPORT mt_st_logstr = mt_st_logstr FROM DATABASE indx(ar) CLIENT sy-mandt ID 'ZCRPROP2'.
    CHECK mt_st_logstr IS INITIAL.

*    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'.

*   Search on DB for these logs
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = ms_strc_fil-lfil
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
         ls_return_prev TYPE bapiret1,
         ls_bal_s_msid  TYPE bal_s_msid,
         ls_bal_s_msno  TYPE bal_s_msno,
         ls_bal_s_msty  TYPE bal_s_msty.

    CHECK mv_check IS INITIAL.
    CHECK mt_st_logstr IS INITIAL.
    READ TABLE ms_strc_fil-mfil-msgid INTO ls_bal_s_msid INDEX 1.
    READ TABLE ms_strc_fil-mfil-msgno INTO ls_bal_s_msno INDEX 1.
    READ TABLE ms_strc_fil-mfil-msgty INTO ls_bal_s_msty INDEX 1.

    LOOP AT  mt_msg_handle INTO ms_message_handle.
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
        IF ls_message-msgid EQ ls_bal_s_msid-low AND ls_message-msgno EQ ls_bal_s_msno-low AND ls_message-msgty EQ ls_bal_s_msty-low .
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
            ms_actuallogs-object    = ms_log_header-object.
            ms_actuallogs-subobject = ms_log_header-subobject.
            ms_actuallogs-log_no = ms_log_header-lognumber.
          ENDIF.
          APPEND ms_actuallogs TO mt_actuallogs.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  method VALIDATE.

  endmethod.
ENDCLASS.
