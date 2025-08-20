```
FUNCTION zps__network_update1 .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_COSTACTELEMENT_TAB) TYPE  ZPS_COSTACTELEMENT_TAB1
*"  EXPORTING
*"     REFERENCE(E_SUBRC) TYPE  SYST-SUBRC
*"     REFERENCE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
*Change Network: CN22 Transaction

  DATA: lv_objectkey               TYPE bapi_method_project-objectkey,
        lv_objecttype              TYPE bapi_method_project-objecttype,
        lt_method_project          TYPE TABLE OF  bapi_method_project,
        lt_activity                TYPE TABLE OF bapi_network_activity,
        lt_activity_update         TYPE TABLE OF bapi_network_activity_up,
        lt_message_table           TYPE TABLE OF bapi_meth_message,
        lt_activity_element        TYPE TABLE OF bapi_act_element,
        lt_activity_element_update TYPE TABLE OF bapi_act_element_upd,
        lt_return                  LIKE bapireturn1.


  LOOP AT i_costactelement_tab ASSIGNING FIELD-SYMBOL(<costactelement>).
    CLEAR: lt_method_project[], lt_activity[], lt_activity_update[], lt_message_table[],
           lt_activity_element[], lt_activity_element_update[].


   IF <costactelement>-element IS INITIAL.
     "If Element Is Blank, Update Activity.
     lv_objecttype  = 'NetworkActivity'.
     lv_objectkey   = |{ <costactelement>-network } | & | { <costactelement>-activity }| .
   ELSE.
     "If Element is not Blank, Update Element.
     lv_objecttype = 'NetworkActivityElement' .
     lv_objectkey  = |{ <costactelement>-network } | & | { <costactelement>-activity }| & | {
<costactelement>-element } |.
   ENDIF.


    lt_method_project  = VALUE #( ( objecttype   = lv_objecttype
                                    method       = 'Update'
                                    refnumber    = '000001'
                                    objectkey    = lv_objectkey )
                                 (  method       = 'Save' ) ).


    IF <costactelement>-element IS INITIAL.
      lt_activity  = VALUE #( (   network     = <costactelement>-network
                                  activity    = <costactelement>-activity
                                  description = <costactelement>-description
                              )
                            ).

      lt_activity_update  = VALUE #( (  "network         = 'X'
                                        "activity        = 'X'
                                         description     = 'X' ) ).

    ELSE.

      lt_activity_element  = VALUE #( (   network         = <costactelement>-network
                                          activity        = <costactelement>-activity
                                          element         = <costactelement>-element
                                          description     = <costactelement>-description ) ).

      lt_activity_element_update  = VALUE #( (   "network         = 'X'
                                                 "activity        = 'X'
                                                 "element         = 'X'
                                                  description     = 'X' ) ).
    ENDIF.


    CALL FUNCTION 'BAPI_NETWORK_MAINTAIN'
      IMPORTING
        return                    = lt_return
      TABLES
        i_method_project          = lt_method_project
        i_activity                = lt_activity
        i_activity_update         = lt_activity_update
        e_message_table           = lt_message_table
        i_activity_element        = lt_activity_element
        i_activity_element_update = lt_activity_element_update.


   IF line_exists( lt_message_table[ MESSAGE_TYPE = 'E' ] ).
     EXIT.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      exporting
*        wait =
            .
   ENDIF.

  ENDLOOP.


ENDFUNCTION.

```