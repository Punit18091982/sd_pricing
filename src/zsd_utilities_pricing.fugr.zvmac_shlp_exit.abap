function zvmac_shlp_exit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  types: begin of ls_result,
           inum type znumber,
         end of ls_result.

  data lt_result type table of ls_result.
  data i type znumber.

  check callcontrol-step = 'DISP'.

  i = 10000.

  while i <= 20000.
    append initial line to lt_result assigning field-symbol(<lv_result>).
    <lv_result>-inum = i.
    i = i + 1.
  endwhile.

  call function 'F4UT_RESULTS_MAP'
    tables
      shlp_tab          = shlp_tab
      record_tab        = record_tab
      source_tab        = lt_result
    changing
      shlp              = shlp
      callcontrol       = callcontrol
    exceptions
      illegal_structure = 1
      others            = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  callcontrol-maxexceed = 'X'.

endfunction.
