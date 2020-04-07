interface /CLIN/IF_SLG1REST
  public .


  data REQUEST type ref to IF_HTTP_REQUEST .
  data RESPONSE type ref to IF_HTTP_RESPONSE .

  methods HANDLE_REQUEST .
  methods SET_RESPONSE
    importing
      !IS_DATA type XSTRING .
endinterface.
