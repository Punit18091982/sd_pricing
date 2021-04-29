FUNCTION Z_WM_TO_DN_IDOC_CREATE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_ZTODNHR) TYPE  ZTODNHR
*"     VALUE(IT_ZTODNIT) TYPE  ZTODNIT_TAB
*"     VALUE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     VALUE(IV_START) TYPE  FLAG OPTIONAL
*"     VALUE(IV_BTCH_USER) TYPE  XUBNAME DEFAULT SY-UNAME
*"  EXPORTING
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_T
*"     REFERENCE(EV_IDOCNUM) TYPE  EDIDC-DOCNUM
*"--------------------------------------------------------------------
************************************************************************
*  Program Title       : IDOC for Transfer Order processing            *
*  Author              : Amit K Jaura(SAP)                             *
*  Developer           : Atul (SAP)                                    *
*  WRICEF ID           : DS_E81                                        *
*  Description:        : IDOC for Transfer Order processing            *
*  Type:               : Enhancement                                   *
*  Run Frequency       : NA                                            *
************************************************************************
*Data Declaration
  DATA:lv_msg          TYPE bdidocattr-message,
       lv_error        TYPE edi_slight,
       ls_idoc_ctrl    TYPE edidc,
       ls_idoc_data    TYPE edidd,
       ls_extension    TYPE z1mm_auts,
       lv_logsys       TYPE logsys,
       ls_process_data TYPE tede2,
       ls_return       TYPE bapiret2,
       lt_idoc_ctrl    TYPE STANDARD TABLE OF edidc,
       lt_idoc_data    TYPE STANDARD TABLE OF edidd.

  CLEAR:ev_idocnum,et_return.

* Get Logical system
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.

  IF sy-subrc <> 0.
    ls_return-type = sy-msgty.
    ls_return-id = sy-msgid.
    ls_return-number = sy-msgno.
    ls_return-message_v1 = sy-msgv1.
    ls_return-message_v2 = sy-msgv2.
    ls_return-message_v3 = sy-msgv3.
    ls_return-message_v4 = sy-msgv4.
    APPEND ls_return TO et_return.
    CLEAR:ls_return.
    RETURN.
  ENDIF.

*Fill IDOC control inforamtion
*message type
  ls_idoc_ctrl-mestyp = gc_msgtyp_zwmtodn."'ZWMTODN'.
  ls_idoc_ctrl-direct = gc_2.
*IDOC type
  ls_idoc_ctrl-idoctp = gc_idoctyp_zwmtodn."'ZWMTODN1'.
  "ls_idoc_ctrl-cimtyp = gc_idocext_zwmtodn1.

*Receiver information
*Port
  CONCATENATE gc_sap sy-sysid(3) INTO ls_idoc_ctrl-rcvpor.
*Partner type
  ls_idoc_ctrl-rcvprt = gc_ls.             "     'LS'.
*Logical System
  ls_idoc_ctrl-rcvprn = lv_logsys.

*Sender information
*Port
  ls_idoc_ctrl-sndpor = ls_idoc_ctrl-rcvpor.
*Logical System
  ls_idoc_ctrl-sndprn = lv_logsys.
*Partner Type
  ls_idoc_ctrl-sndprt = gc_ls.              "'LS'.

*Filling IDOC data
*--------------------------------------------------------------------*
*ZTODNHR
*--------------------------------------------------------------------*
  ls_idoc_data-segnam    = gc_seg_to_header.      "'ZTODNHR'.
  ls_idoc_data-sdata     = is_ztodnhr.
  APPEND ls_idoc_data TO lt_idoc_data.
  CLEAR: ls_idoc_data.

*--------------------------------------------------------------------*
*ZTODNIT
*--------------------------------------------------------------------*
  LOOP AT it_ztodnit INTO DATA(ls_ztodnit).
    ls_idoc_data-segnam =  gc_seg_to_item.        "'ZTODNIT'.
    ls_idoc_data-sdata = ls_ztodnit.
    APPEND ls_idoc_data TO lt_idoc_data.
    CLEAR: ls_idoc_data,ls_ztodnit.
  ENDLOOP.

*--------------------------------------------------------------------*
* Function Call for IDOC Generation
*--------------------------------------------------------------------*

  CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
    EXPORTING
      pi_do_handle_error      = abap_true
    IMPORTING
      pe_idoc_number          = ls_idoc_ctrl-docnum
      pe_inbound_process_data = ls_process_data
    TABLES
      t_data_records          = lt_idoc_data
    CHANGING
      pc_control_record       = ls_idoc_ctrl
    EXCEPTIONS
      idoc_not_saved          = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    ls_return-type = gc_error.
    ls_return-message = TEXT-e00."'IDOC could not be processed'.
    APPEND ls_return TO et_return.
    CLEAR ls_return.
    RETURN.
  ENDIF.

*Exporting the IDOC number
  ev_idocnum = ls_idoc_ctrl-docnum.

  IF iv_start = abap_true.    "Process IDOC
*--------------------------------------------------------------------*
* Function Call to Start inbound processing
*--------------------------------------------------------------------*
    APPEND ls_idoc_ctrl TO lt_idoc_ctrl.
    ls_process_data-mandt  = sy-mandt .     "client
    ls_process_data-evcode = gc_code.       "'ZWMTODN'"Process code
    ls_process_data-edivr2 = gc_6."         '6'.

    CALL FUNCTION 'IDOC_START_INBOUND' "IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        pi_inbound_process_data       = ls_process_data
      TABLES
        t_control_records             = lt_idoc_ctrl
      EXCEPTIONS
        invalid_document_number       = 1
        error_before_call_application = 2
        inbound_process_not_possible  = 3
        old_wf_start_failed           = 4
        wf_task_error                 = 5
        serious_inbound_error         = 6
        OTHERS                        = 7.
    IF sy-subrc <> 0.
      ls_return-type       = sy-msgty.
      ls_return-id         = sy-msgid.
      ls_return-number     = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO et_return.
      CLEAR:ls_return,ev_idocnum.
      RETURN.
    ENDIF.

*A COMMIT WORK must be dispatched in the calling program, otherwise the
*IDocs may not be dispatched.
    IF iv_commit = abap_true.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

  ELSE.

*A COMMIT WORK must be dispatched in the calling program, otherwise the
*IDocs may not be dispatched.
    IF iv_commit = abap_true.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

*--Submit Background job to start IDOC processing..
    PERFORM process_batch_job_idoc USING ev_idocnum iv_btch_user CHANGING et_return.

  ENDIF.

*Getting the IDOC status(Error/Success/Warning)
  CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
    EXPORTING
      x_docnum   = ev_idocnum
    IMPORTING
      y_stalight = lv_error.
  CASE lv_error.
    WHEN '3'.
      ls_return-type = gc_error."'E'.
    WHEN '2'.
      ls_return-type = gc_success."'S'.
    WHEN '1'.
      ls_return-type = gc_warning."'W'.
  ENDCASE.

*Getting the IDOC message
  CALL FUNCTION 'IDOC_GET_MESSAGE_ATTRIBUTE'
    EXPORTING
      idoc_number  = ev_idocnum
    IMPORTING
      idoc_message = lv_msg.

  ls_return-message = lv_msg.
  APPEND ls_return TO et_return.
  CLEAR ls_return.


*----------------------------------------------------------------------*
*        FORM process_batch_job_idoc                                 *
*----------------------------------------------------------------------*
*        Start IDOC processing in background                           *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM process_batch_job_idoc USING    fp_v_docnum TYPE edidc-docnum
                                     fp_v_buser  TYPE xubname
                            CHANGING fp_t_return TYPE bapiret2_t.

  DATA: lt_params           TYPE STANDARD TABLE OF rsparams,
        lv_print_parameters TYPE pri_params,
        lv_jobname          TYPE btcjob,
        lv_jobcount         TYPE btcjobcnt,
        ls_return           TYPE bapiret2.

*--> Set Report parameters
  APPEND VALUE rsparams( selname = gc_docnum  kind = gc_kind_s sign = 'I' option = 'EQ' low = fp_v_docnum ) TO lt_params.
  CONCATENATE gc_jobpfix fp_v_docnum INTO lv_jobname.

*--Submission of IDOC batch job in background.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc = 0.

    SUBMIT rbdapp01 USER fp_v_buser TO SAP-SPOOL SPOOL
                    PARAMETERS lv_print_parameters WITHOUT SPOOL DYNPRO
                    WITH SELECTION-TABLE lt_params VIA JOB lv_jobname
                    NUMBER lv_jobcount
                    AND RETURN.
    IF sy-subrc <> 0.
      MESSAGE e738(zp2p) WITH 'Submit:Batch' lv_jobname lv_jobcount INTO DATA(lv_dummy).

      CLEAR:ls_return.
      ls_return-type       = sy-msgty.
      ls_return-id         = sy-msgid.
      ls_return-number     = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      ls_return-message    = lv_dummy.
      APPEND ls_return TO fp_t_return.
      RETURN.

    ELSE.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = lv_jobname
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          invalid_time_zone    = 9
          OTHERS               = 10.

      IF sy-subrc <> 0.

        MESSAGE e738(zp2p) WITH 'Close:Batch' lv_jobname lv_jobcount INTO lv_dummy.
        CLEAR:ls_return.
        ls_return-type       = sy-msgty.
        ls_return-id         = sy-msgid.
        ls_return-number     = sy-msgno.
        ls_return-message_v1 = sy-msgv1.
        ls_return-message_v2 = sy-msgv2.
        ls_return-message_v3 = sy-msgv3.
        ls_return-message_v4 = sy-msgv4.
        ls_return-message    = lv_dummy.
        APPEND ls_return TO fp_t_return.

      ELSE.
*--Job successfully started..
        MESSAGE s739(zp2p) WITH lv_jobname lv_jobcount INTO lv_dummy.
        CLEAR:ls_return.
        ls_return-type       = sy-msgty.
        ls_return-id         = sy-msgid.
        ls_return-number     = sy-msgno.
        ls_return-message_v1 = sy-msgv1.
        ls_return-message_v2 = sy-msgv2.
        ls_return-message_v3 = sy-msgv3.
        ls_return-message_v4 = sy-msgv4.
        ls_return-message    = lv_dummy.
        APPEND ls_return TO fp_t_return.

      ENDIF.

    ENDIF.

  ELSE.

    MESSAGE e738(zp2p) WITH 'Open:Batch' lv_jobname lv_jobcount INTO lv_dummy.
    CLEAR:ls_return.
    ls_return-type       = sy-msgty.
    ls_return-id         = sy-msgid.
    ls_return-number     = sy-msgno.
    ls_return-message_v1 = sy-msgv1.
    ls_return-message_v2 = sy-msgv2.
    ls_return-message_v3 = sy-msgv3.
    ls_return-message_v4 = sy-msgv4.
    ls_return-message    = lv_dummy.
    APPEND ls_return TO fp_t_return.

  ENDIF.

ENDFORM.     "process_batch_job_idoc





ENDFUNCTION.
