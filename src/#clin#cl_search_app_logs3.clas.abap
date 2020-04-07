class /CLIN/CL_SEARCH_APP_LOGS3 definition
  public
  final
  create public .

public section.

  interfaces /CLIN/IF_SLG1REST .

  data MS_LOG_FILTER type BAL_S_LFIL .
  data MV_MESSAGEID type BAPIRET2-ID .
  data MV_MSGNUMBER type BAPIRET2-NUMBER .
  data MT_BAPI_MSG type RSTCOIQM_T_TEXT .
  data MS_LONGTEXT type /CLIN/ST_LONGTEXT .

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



CLASS /CLIN/CL_SEARCH_APP_LOGS3 IMPLEMENTATION.


  METHOD /clin/if_slg1rest~handle_request.
    DATA:
      lv_string_writer   TYPE REF TO cl_sxml_string_writer,
      lv_xstring         TYPE xstring,
      lo_json_serializer TYPE REF TO cl_trex_json_serializer,
      l_json             TYPE string.

*   Read values from te URI query=( logNum=00000000000001438779)
    CALL METHOD me->read_from_uri
      RECEIVING
        rs_log_filter = ms_log_filter.

*   Search on DB for these logs
    CALL METHOD me->read_logs_fromdb.

*   Read texts out of msg handles
    CALL METHOD me->read_text.

    TRY.
        CREATE OBJECT lo_json_serializer
          EXPORTING
            data = ms_longtext.
        CALL METHOD lo_json_serializer->serialize.

        CALL METHOD lo_json_serializer->get_data
          RECEIVING
            rval = l_json.
        CALL METHOD me->/clin/if_slg1rest~response->set_header_field( name = 'Content-type' value = 'application/json; charset=iso-8859-1' ).
        CALL METHOD me->/clin/if_slg1rest~response->set_cdata( data = l_json ) .

*       Convert output to json
        lv_string_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
        CALL TRANSFORMATION id SOURCE tab =  ms_longtext RESULT XML lv_string_writer.
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

*   Read values from te URI query=( ID=PG, NUMBER=057)
    mv_messageid   = me->/clin/if_slg1rest~request->get_form_field('ID'). "ID
    mv_msgnumber = me->/clin/if_slg1rest~request->get_form_field('NUMBER'). "NUMBER

  ENDMETHOD.


  METHOD read_logs_fromdb.

    DATA: lv_bapi_msg TYPE bapi_msg,
          lv_return   TYPE bapiret2.

* Read Message long text
    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = mv_messageid
        number     = mv_msgnumber
        language   = sy-langu
        textformat = 'ASC'
      IMPORTING
        message    = lv_bapi_msg
        return     = lv_return
      TABLES
        text       = mt_bapi_msg.


  ENDMETHOD.


  METHOD read_text.

    ms_longtext-id = mv_messageid.
    ms_longtext-number = mv_msgnumber.
    ms_longtext-longtext = mt_bapi_msg.

  ENDMETHOD.
ENDCLASS.
