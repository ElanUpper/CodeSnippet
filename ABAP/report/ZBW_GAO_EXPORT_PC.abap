*&---------------------------------------------------------------------*
*& Report ZEXPORT_PC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zbw_gao_export_pc.

**********************************************************************************
*
* Date        Programmer     ChangeType          Reason
*
* 2017/11/06  WangYang       Create(New)         List each step of Process Chains
*
******************************************************************************

types:

    begin of ty_export_chain ,
      stepname type string   ,
      curr_id  type btcevtparm ,
      next_id  type btcevtparm ,
      objtype  type string ,      " type
      savevar  type rspc_variant, " variable name
    end of ty_export_chain ,

    begin of ty_final_export_tier ,
      layernub type i ,
      stepname type string ,
      icon     type icon_d,
      objtype  type string ,
      id       type string ,
      scol     type lvc_t_scol, " color
    end of ty_final_export_tier ,

    begin of ty_final_export ,
      stepname type string ,
      icon     type icon_d,
      objtype  type string ,
      id       type string ,
      scol     type lvc_t_scol, " color
    end of ty_final_export ,

    tt_final_export_tier type table of ty_final_export_tier with empty key ,
    tt_export_chain type table of ty_export_chain with empty key ,
    tt_final_export type table of ty_final_export with empty key .


data: gv_curr_layer   type i value 1 ,
      wa_export_chain type ty_export_chain ,
      it_export_chain type tt_export_chain ,
      wa_final_export type ty_final_export ,
      it_final_export type tt_final_export ,
      wa_final_export_tier type ty_final_export_tier ,
      it_final_export_tier type tt_final_export_tier .

ranges:
      r_dtp   for wa_export_chain-savevar , " execute dtp
      r_abap  for wa_export_chain-savevar , " execute ABAP
      r_psa   for wa_export_chain-savevar . " store current level


parameters: p_chain type rspc_chain obligatory .

class lcl_event_handler definition.

 public section.

    methods:
      on_double_click
        for event double_click of cl_salv_events_table
        importing row column  ,

      on_user_command for event added_function of cl_salv_events_table
        importing
          e_salv_function.

endclass.

class lcl_event_handler implementation.

 method on_double_click.

   if column = 'ID' .

     data: lv_chain type rspc_chain .

     " double click
     read table it_final_export assigning field-symbol(<fs_final>) index row .
     if sy-subrc = 0 and <fs_final>-objtype = 'CHAIN' .
        lv_chain = <fs_final>-id .
        " delete data
        delete it_final_export_tier where layernub = gv_curr_layer .
        " backup the data to global internal table
        loop at it_final_export assigning <fs_final> .
          append initial line to it_final_export_tier assigning field-symbol(<fs_final_export_tier>) .
          <fs_final_export_tier>-layernub = gv_curr_layer .
          move-corresponding <fs_final> to <fs_final_export_tier> .
        endloop.
       " each available double click mean layer plus one
       gv_curr_layer = gv_curr_layer + 1 .
       " start next around
       perform output_data using lv_chain .
     endif.
   endif.

 endmethod.

 method on_user_command .
   clear it_final_export .
   " recover the raw data
   delete it_final_export_tier where layernub = gv_curr_layer .
   gv_curr_layer = gv_curr_layer - 1 .
   loop at it_final_export_tier assigning field-symbol(<fs_final_export_tier>)
   where layernub = gv_curr_layer.
      append initial line to it_final_export assigning field-symbol(<fs_final>) .
      move-corresponding <fs_final_export_tier> to <fs_final> .
   endloop.

   set screen 0 .
   leave screen .
 endmethod .

endclass.

start-of-selection .

  " confirm the process chain exist
  select single * into @data(wa_chain) from rspcchaint where chain_id = @p_chain  .
  if sy-subrc <> 0 .
    return .
  endif .

**********************************************************************
*
*  get the table name which store the process chain metadata
*
**********************************************************************

*  data: lv_selist  type string ,
*        lv_gplist  type string ,
*        lv_table type string ,
*        lv_condt type string ,
*        lv_group type string ,
*        lv_retrn type table of string .
*
*  " tables
*  select distinct dd02l~tabname into table @data(it_chain)
*    from dd02l
*    inner join dd03l on dd02l~tabname = dd03l~tabname
*  where dd02l~as4local = 'A'
*    and dd03l~as4local = 'A'
*    and dd02l~tabclass = 'TRANSP'
*    and dd03l~fieldname = 'CHAIN_ID'
*    and dd02l~tabname not like '/%'
*  .
*
*  lv_condt  = `CHAIN_ID = '` && p_chain && `'`.
*  lv_selist = `CHAIN_ID`.
*  lv_gplist = `CHAIN_ID` .
*  lv_group  = 'count(*) > 1' .
*
*  loop at it_chain assigning field-symbol(<fs_chain>) .
*
*    lv_table = <fs_chain>-tabname .
*
**    test for sql string
**    data(lv_str) =
**      | select { lv_selist } into table lv_retrn from { lv_table } where { lv_condt } group by { lv_gplist } having { lv_group } | .
*
*    select (lv_selist)
*      into table lv_retrn
*      from (lv_table)
*     where (lv_condt)
*     group by (lv_gplist)
*    having (lv_group)
*    .
*    if sy-subrc <> 0 .
*      clear <fs_chain> .
*    endif .
*
*  endloop.
*
*  delete it_chain where tabname is initial .

**********************************************************************


start-of-selection .
  " Display & Export data
  perform output_data using p_chain .


form output_data using iv_chain type rspc_chain .
  clear: it_export_chain, it_final_export .
  perform get_process_chain_tech_info  tables it_export_chain using iv_chain  .
  perform get_process_chain_extend_inf tables it_export_chain it_final_export .
  perform set_chain_format tables it_final_export .
  perform display_data tables it_final_export .
endform .

form display_data tables it_exp_chain type tt_final_export .

  data:
    lo_column       type ref to cl_salv_column_list ,
    lo_columns      type ref to cl_salv_columns_table,
    lo_err          type ref to cx_salv_error,
    lr_functions    type ref to cl_salv_functions,
    lr_alv          type ref to cl_salv_table,
    lr_display      type ref to cl_salv_display_settings,
    lv_salv_msg     type ref to cx_salv_msg ,
    lv_alv_events   type ref to cl_salv_events_table ,
    lv_events       type ref to lcl_event_handler . " event

    if lr_alv is not bound.
      try.
        " factory
        cl_salv_table=>factory(
          importing
            r_salv_table = lr_alv
          changing
            t_table      = it_final_export
        ) .
      catch cx_salv_msg into lv_salv_msg.
        message lv_salv_msg type 'E'.
      catch cx_salv_error into lo_err.
        message lo_err type 'I' display like 'E'.
     endtry.
   endif.

  " set descriptions of columns
  lo_columns = lr_alv->get_columns( ).
  lo_columns->set_optimize('X') .
  lo_column ?= lo_columns->get_column( 'STEPNAME' ).
  lo_column->set_long_text('Step number').

  lo_column ?= lo_columns->get_column( 'OBJTYPE' ).
  lo_column->set_long_text('Object type').

  lo_column ?= lo_columns->get_column( 'ID' ).
  lo_column->set_long_text( 'Detail infomation' ).

  " set icon
  lo_column ?= lo_columns->get_column( 'ICON' ).
  lo_column->set_icon(  if_salv_c_bool_sap=>true ).
  lo_column->set_long_text( 'ICON' ).

  " set color
  lo_columns->set_color_column( 'SCOL' ).

  " process events
  lv_alv_events = lr_alv->get_event( ). " get events
  create object lv_events.
  set handler lv_events->on_double_click for lv_alv_events.
  set handler lv_events->on_user_command for lv_alv_events.

  call method lr_alv->set_screen_status(
    exporting
      report        = sy-repid
      pfstatus      = 'SALV_STANDARD'
      set_functions = lr_alv->c_functions_all ).

  lr_display = lr_alv->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display->set_list_header( 'Process Chain Technical Infomation' ).

  " display
  lr_alv->display( ).

endform .


form set_chain_format tables it_chain_data type tt_final_export .

  data:
    wa_color        type lvc_s_scol ,
    it_color        type lvc_t_scol .

  clear: wa_color, it_color[] .
  " color setting
  wa_color-fname = 'ID'.
  wa_color-color-col = col_positive.
  wa_color-color-int = 0.
  wa_color-color-inv = 0.
  append wa_color to it_color.

  loop at it_chain_data assigning field-symbol(<fs_chain_data>) .

    case <fs_chain_data>-objtype .
      when 'CHAIN' .
        <fs_chain_data>-icon = icon_bw_apd.
        <fs_chain_data>-scol = it_color . " set color only object type equal CHAIN
      when 'AND'.
      when 'TRIGGER'.
      when 'LOADING'.
        <fs_chain_data>-icon = icon_bw_datasource.
      when 'DTP_LOAD'.
        <fs_chain_data>-icon = icon_bw_dtp_active.
      when 'ODSACTIVAT'.
        <fs_chain_data>-icon = icon_activate.
      when 'ABAP'.
        <fs_chain_data>-icon = icon_abap.
      when 'ROLLUP'.
        <fs_chain_data>-icon = icon_sum.
      when 'COMPRESS'.
        <fs_chain_data>-icon = icon_data_area_collapse.
    endcase.

  endloop .

endform .

form get_process_chain_extend_inf
  tables it_raw_chain type tt_export_chain
         it_exp_chain type tt_final_export .

  " DTP
  if r_dtp[] is not initial.
    select
*           a~dtp    as adtp,
*           a~tgttp  as atgttp,
*           a~tgt    as atgt,
*           a~srctp  as asrctp,
*           a~src    as asrc
           a~dtp ,
           a~src && '[' && a~srctp && '] --> ' && a~tgt && '[' && a~tgttp && ']' as id
      from rsbkdtp as a
      into table @data(it_dtps)
     where a~dtp in @r_dtp
     .
  endif.

  " InfoPackage
  if r_psa[] is not initial.
   select
*          'Datasource'  as targettype,
*          a~oltpsource  as targetname,
*          'InfoPackage' as sourcetype,
*          a~logdpid     as sourcename,
*          b~text        as sourcedesc,
          a~logdpid ,
          b~text && '[INFOPACK] --> ' && a~oltpsource && '[DTASRC]' as id
     into table @data(it_infopackages)
     from rsldpio as a
     left join rsldpiot as b
       on a~logdpid = b~logdpid
   where a~objvers = 'A'
     and b~objvers = 'A'
     and a~logdpid in @r_psa
    .
  endif.


  " ABAP
  if r_abap[] is not initial.

    data:
       begin of it_abaps occurs 0,
         variant type rspc_variant ,
         id      type string ,
       end of it_abaps .

    select variante, fnam, low
      from rspcvariant
      into table @data(it_abaps_raw)
     where objvers = 'A'
       and fnam in ('PROGRAM', 'VARIANT')
       and variante in @r_abap
     .
    loop at it_abaps_raw assigning field-symbol(<it_abap_raw>) where fnam = 'PROGRAM' .
      read table it_abaps_raw into data(wa_abap) with key fnam = 'VARIANT'
                                                          variante = <it_abap_raw>-variante .
      if sy-subrc = 0.
        append initial line to it_abaps assigning field-symbol(<fs_abap>) .
        <fs_abap>-variant = <it_abap_raw>-variante .
        <fs_abap>-id = <it_abap_raw>-low && '(' && wa_abap-low && ')' .
      endif.
    endloop.
  endif.


  loop at it_raw_chain assigning field-symbol(<fs_raw_chain>).
    append initial line to it_exp_chain assigning field-symbol(<fs_exp_chain>) .
    case <fs_raw_chain>-objtype.
      when 'DTP_LOAD'.   " execute dtp
        read table it_dtps assigning field-symbol(<fs_dtp>)
        with key dtp = <fs_raw_chain>-savevar .
        if sy-subrc = 0 .
          <fs_exp_chain>-stepname = <fs_raw_chain>-stepname .
          <fs_exp_chain>-objtype  = <fs_raw_chain>-objtype .
          <fs_exp_chain>-id       = <fs_dtp>-id .
        endif.
      when 'LOADING'.    " execute InfoPackage
        read table it_infopackages assigning field-symbol(<fs_infopackage>)
        with key logdpid = <fs_raw_chain>-savevar .
        if sy-subrc = 0 .
          <fs_exp_chain>-stepname = <fs_raw_chain>-stepname .
          <fs_exp_chain>-objtype  = <fs_raw_chain>-objtype .
          <fs_exp_chain>-id       = <fs_infopackage>-id .
        endif.
      when 'ABAP'.       " call abap program
        read table it_abaps assigning <fs_abap>
        with key variant = <fs_raw_chain>-savevar .
        if sy-subrc = 0.
          <fs_exp_chain>-stepname = <fs_raw_chain>-stepname .
          <fs_exp_chain>-objtype  = <fs_raw_chain>-objtype .
          <fs_exp_chain>-id       = <fs_abap>-id .
        endif.
*      when 'ODSACTIVAT'. " Active DSO
*      when 'PSADELETE'.  " delete PSA
      when others.
        <fs_exp_chain>-stepname = <fs_raw_chain>-stepname .
        <fs_exp_chain>-objtype  = <fs_raw_chain>-objtype .
        <fs_exp_chain>-id       = <fs_raw_chain>-savevar .
    endcase.
  endloop.

endform .

form get_process_chain_tech_info
  tables it_chain type tt_export_chain
  using iv_chain  type rspc_chain  .

  data: lv_point_number type i ,       " count the child nodes
        lv_temp_faname  type string ,  " temp save father name
        it_all_nodes    type tt_export_chain .  " nodes list

  ranges: r_save_curr  for wa_export_chain-curr_id ,  " temp save
          r_curr_level for wa_export_chain-curr_id . " store current level

  select eventp_start as curr_id ,
         eventp_green as next_id ,
         type         as objtype ,
         variante     as savevar
   into corresponding fields of table @it_all_nodes
    from rspcchain
   where chain_id = @iv_chain
     and objvers = 'A'.

  sort it_all_nodes by curr_id .

  " insert root node
  read table it_all_nodes assigning field-symbol(<fs_all_nodes>)
  with key curr_id = space .
  if sy-subrc = 0 .
    " store root node to export list
    append initial line to it_chain assigning field-symbol(<fs_chain>) .
    <fs_chain>-stepname = '1' .
    <fs_chain>-curr_id  = <fs_all_nodes>-curr_id .
    <fs_chain>-next_id  = <fs_all_nodes>-next_id .
    <fs_chain>-objtype  = <fs_all_nodes>-objtype .
    <fs_chain>-savevar  = <fs_all_nodes>-savevar .
    " save node to next round
    r_curr_level-option = 'EQ' .
    r_curr_level-sign   = 'I' .
    r_curr_level-low    = <fs_all_nodes>-next_id .
    append r_curr_level .
  else .
    " empty
    return .
  endif.

  while r_curr_level[] is not initial .
    clear: lv_point_number, r_save_curr[] .
    " traverse each nodes
    loop at it_all_nodes assigning <fs_all_nodes> where curr_id in r_curr_level .
      lv_point_number = lv_point_number + 1 .
      " init
      append initial line to it_chain assigning <fs_chain> .
      " get father's node step name
      read table it_chain assigning field-symbol(<fs_chain_tmp>)
      with key next_id = <fs_all_nodes>-curr_id .
      if lv_temp_faname <> <fs_chain_tmp>-stepname .
        lv_point_number = 1 .
        lv_temp_faname  = <fs_chain_tmp>-stepname .
      endif.
      <fs_chain>-stepname = <fs_chain_tmp>-stepname && '-' && lv_point_number .
      <fs_chain>-curr_id  = <fs_all_nodes>-curr_id .
      <fs_chain>-next_id  = <fs_all_nodes>-next_id .
      <fs_chain>-objtype  = <fs_all_nodes>-objtype .
      <fs_chain>-savevar  = <fs_all_nodes>-savevar .
      " save node and prepare next round
      r_save_curr-option = 'EQ' .
      r_save_curr-sign   = 'I' .
      r_save_curr-low    = <fs_all_nodes>-next_id .
      append r_save_curr .
      " prepare
      case <fs_chain>-objtype.
        when 'DTP_LOAD'.   " execute dtp
          r_dtp-option = 'EQ' .
          r_dtp-sign   = 'I' .
          r_dtp-low    = <fs_all_nodes>-savevar .
          append r_dtp .
        when 'LOADING'.    " execute InfoPackage
          r_psa-option = 'EQ' .
          r_psa-sign   = 'I' .
          r_psa-low    = <fs_all_nodes>-savevar .
          append r_psa .
        when 'ABAP'.       " call abap program
          r_abap-option = 'EQ' .
          r_abap-sign   = 'I' .
          r_abap-low    = <fs_all_nodes>-savevar .
          append r_abap .
        when 'ODSACTIVAT'. " Active DSO
        when 'PSADELETE'.  " delete PSA
        when others.
      endcase.
    endloop.
    clear: r_curr_level[] .
    r_curr_level[] = r_save_curr[]   .

    " avoid redundant data
    sort r_curr_level by low .
    delete r_curr_level where low is initial .
    delete adjacent duplicates from r_curr_level comparing all fields .
    sort it_chain by curr_id .
    delete adjacent duplicates from it_chain comparing all fields .

  endwhile.

  sort it_export_chain by stepname .

endform .