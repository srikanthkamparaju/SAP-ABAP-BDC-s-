```
FUNCTION zps__network_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CTU) TYPE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) TYPE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) TYPE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) TYPE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) TYPE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) TYPE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) TYPE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) TYPE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(IV_PROFID) TYPE  CAUFVD-PROFID DEFAULT '0000001'
*"     VALUE(IV_PS_AUFART) TYPE  AUFPAR-PS_AUFART DEFAULT 'PS01'
*"     VALUE(IV_WERKS) TYPE  CAUFVD-WERKS DEFAULT '1710'
*"     VALUE(IV_DISPO) TYPE  CAUFVD-DISPO DEFAULT '001'
*"     VALUE(IV_STDNR) TYPE  CAUFVD-STDNR OPTIONAL
*"     VALUE(IV_KTEXT) TYPE  CAUFVD-KTEXT DEFAULT 'WTG Configuration #'
*"     VALUE(IV_PRONR) TYPE  PS_PSPID DEFAULT 'PS01'
*"     VALUE(IV_PLGRP) TYPE  CAUFVD-PLGRP DEFAULT '1'
*"  EXPORTING
*"     VALUE(SUBRC) TYPE  SYST_SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL
*"      MSGTAB STRUCTURE  BAPI_METH_MESSAGE
*"----------------------------------------------------------------------
*Create Network: CN21 transaction

DATA: lt_method_project   TYPE TABLE OF bapi_method_project,
      lt_network          TYPE TABLE OF bapi_network,
      lt_activity         TYPE TABLE OF  bapi_network_activity,
      lt_activity_element TYPE TABLE OF  bapi_act_element,
      lt_return           LIKE bapireturn1.


CONSTANTS: c_objectnetwork     TYPE string VALUE 'NETWORK',
           c_objectactivity    TYPE string VALUE 'NETWORKACTIVITY',
           c_objectelement     TYPE string VALUE 'NETWORKACTIVITYELEMENT',
           c_objectkey         TYPE string VALUE 'NEWNETWORK01',
           c_refnumber         TYPE string VALUE '000001',
           c_activity          TYPE string VALUE '0020',
           c_element           TYPE string VALUE '0030',
           c_control_key       TYPE char4  VALUE 'PS01',
           c_method_create     TYPE string VALUE 'CREATE',
           c_save              TYPE char30 VALUE 'SAVE'.


Data(lv_objectkey) = c_objectkey && c_activity.

Data(lv_objectkey1) = lv_objectkey && c_element.


  lt_method_project = VALUE #( ( objecttype = c_objectnetwork
                                 method     = c_method_create
                                 objectkey  = c_objectkey
                                 refnumber  = c_refnumber
                               )
                                ( method     = c_save )
                                ( objecttype = c_objectactivity
                                  method     = c_method_create
                                  objectkey  = lv_objectkey
                                  refnumber  = c_refnumber
                                )
                                ( objecttype = c_objectelement
                                   method     = c_method_create
                                   objectkey  = lv_objectkey1
                                   refnumber  = c_refnumber
                                )
                             ).


  lt_network = VALUE #( ( network            = c_objectkey
                          profile            = iv_profid
                          network_type       = iv_ps_aufart
                          plant              = iv_werks
                          mrp_controller     = iv_dispo
                          short_text         = iv_ktext
                          project_definition = iv_pronr
                        )
                      ).



  lt_activity = VALUE #( ( network           = c_objectkey
                           activity          = c_activity
                           plant             = iv_werks
                           control_key       = c_control_key
                         )
                       ).


  lt_activity_element = VALUE #( ( network           = c_objectkey
                                   activity          = c_activity
                                   element           = c_element
                                   control_key       = c_control_key
                                  )
                                ).


CALL FUNCTION 'BAPI_NETWORK_MAINTAIN'
 IMPORTING
   RETURN                            = lt_return
  TABLES
    i_method_project                 = lt_method_project
    i_network                        = lt_network
    i_activity                       = lt_activity
    i_activity_element               = lt_activity_element
    e_message_table                  = msgtab
          .


  IF line_exists( msgtab[ MESSAGE_TYPE = 'E' ] ).
     EXIT.

    ELSE.

     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      exporting
*      wait          =
          .
  ENDIF.


ENDFUNCTION .



*  ls_method_project-refnumber  = '000001'.
*  ls_method_project-objecttype = 'Network'.
*  ls_method_project-method     = 'Create'.
*  ls_method_project-objectkey  = 'NewNetwork01'.
*
*  APPEND ls_method_project TO lt_method_project.

*  ls_method_project-METHOD     = 'Save'.
*
*  APPEND ls_method_project TO lt_method_project.
*
*  ls_network-network = 'NewNetwork01'.
*  ls_network-profile            = iv_profid.
*  ls_network-network_type       = iv_ps_aufart.
*  ls_network-plant              = iv_werks.
*  ls_network-mrp_controller     = iv_dispo.
**ls_network-network            = iv_stdnr.
*  ls_network-short_text         = iv_ktext.
*  ls_network-project_definition = iv_pronr.
**ls_network-                   = iv_plgrp.
*
*  APPEND ls_network TO lt_network.

```