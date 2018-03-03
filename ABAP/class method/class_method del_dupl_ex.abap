" 处理问题
"    BW使用open hub向外输出数据的时候，如果是DSO并且输出不需要recordmode这个字段，会导致数据冗余
" class使用方法
"    tr start routine中，输入request以及source package即可

  class-methods del_dupl_ex
    importing
      !i_request type ref to if_rsbk_request_admintab_view
    changing
      !source_package type table .

  method del_dupl_ex.



    data lv_src_tabname type  rsbksrcnm.
    data lv_src         type  rsbksrcnm.
    data lv_srctp       type rsbksrctp.

    lv_src   = i_request->get_src( ).
    lv_srctp = i_request->get_srctp( ).

    check lv_srctp = 'ODSO'.

    data: lv_delflag       type c,
          lv_deleted       type c,
          lv_kcount        type i,
          lv_index         type numc2,
          lv_field_list    type string ,
          lv_where         type string ,
          lv_singleentry   type string ,
          lv_forallentries type string ,
          lv_key_fieldname type char50,
          lv_concat_word   type char1,
          ls_otab          type abap_sortorder ,
          ls_ref           type ref to data ,
          lt_otab          type abap_sortorder_tab ,
          lt_ref           type ref to data ,
          lr_descr_type    type ref to cl_abap_typedescr
          .

    data:lv_field_01 type fieldname ,
         lv_field_02 type fieldname ,
         lv_field_03 type fieldname ,
         lv_field_04 type fieldname ,
         lv_field_05 type fieldname ,
         lv_field_06 type fieldname ,
         lv_field_07 type fieldname ,
         lv_field_08 type fieldname ,
         lv_field_09 type fieldname ,
         lv_field_10 type fieldname ,
         lv_field_11 type fieldname ,
         lv_field_12 type fieldname ,
         lv_field_13 type fieldname ,
         lv_field_14 type fieldname ,
         lv_field_15 type fieldname ,
         lv_field_16 type fieldname ,
         lv_field_17 type fieldname ,
         lv_field_18 type fieldname ,
         lv_field_19 type fieldname ,
         lv_field_20 type fieldname .

    field-symbols:
         <fs_field>    type any ,
         <fs_field_1>  type any ,
         <fs_field_2>  type any ,
         <fs_field_3>  type any ,
         <fs_field_4>  type any ,
         <fs_field_5>  type any ,
         <fs_field_6>  type any ,
         <fs_field_7>  type any ,
         <fs_field_8>  type any ,
         <fs_field_9>  type any ,
         <fs_field_10> type any ,
         <fs_field_11> type any ,
         <fs_field_12> type any ,
         <fs_field_13> type any ,
         <fs_field_14> type any ,
         <fs_field_15> type any ,
         <fs_field_16> type any ,
         <fs_field_17> type any ,
         <fs_field_18> type any ,
         <fs_field_19> type any ,
         <fs_field_20> type any .

    field-symbols: <fs_key> type any ,
                   <wa>     type any ,
                   <tab>    type any table .

    if gt_dsos[] is initial.

      select odsobject as dso_name,
             '/BIC/A' && odsobject && '00' as dso_active
        into table @gt_dsos
        from rsdodso
       where objvers = 'A' .

      select adsonm as dso_name,
             '/BIC/A' && adsonm && '2' as dso_active
       appending table @gt_dsos
            from rsoadso
           where objvers = 'A' .

    endif.

    read table gt_dsos into data(wa_dso)
    with key dso_name = lv_src .
    if sy-subrc = 0.
      lv_src_tabname = wa_dso-dso_active .
    endif .

    select fieldname as name from dd03l
      into corresponding fields of table lt_otab
      where tabname = lv_src_tabname
      and as4local = 'A'
      and ( keyflag  = 'X' or fieldname = 'RECORDMODE') .
    if sy-subrc = 0.
      clear: lv_delflag, lv_field_list, lv_singleentry .
      loop at lt_otab into ls_otab where name <> 'RECORDMODE'.
        lv_index = lv_index + 1.
        concatenate 'LV_FIELD' lv_index into lv_key_fieldname separated by '_'.
        assign (lv_key_fieldname) to <fs_key>.
        if sy-subrc eq 0.
          <fs_key>      = ls_otab-name.
          concatenate lv_field_list ls_otab-name into lv_field_list separated by space.
          lv_singleentry = 'source_package-' && ls_otab-name .
          concatenate ls_otab-name '=' lv_singleentry into lv_singleentry separated by space .
          if lv_index = 1.
            lv_forallentries = lv_singleentry .
          else.
            concatenate lv_forallentries 'and' lv_singleentry into lv_forallentries separated by space .
          endif.
        endif.
      endloop.
      " select list
      lv_field_list = lv_field_list+1.
      read table lt_otab with key name = 'RECORDMODE' transporting no fields.
      if sy-subrc = 0.
        lv_delflag = 'Y'.
        delete lt_otab where name = 'RECORDMODE' .
      endif.
      ls_otab-name       = 'RECORD'.
      ls_otab-descending = 'X'.
      append ls_otab to lt_otab.
    endif.

    sort source_package by (lt_otab).
    delete adjacent duplicates from source_package comparing
              (lv_field_01)
              (lv_field_02)
              (lv_field_03)
              (lv_field_04)
              (lv_field_05)
              (lv_field_06)
              (lv_field_07)
              (lv_field_08)
              (lv_field_09)
              (lv_field_10)
              (lv_field_11)
              (lv_field_12)
              (lv_field_13)
              (lv_field_14)
              (lv_field_15)
              (lv_field_16)
              (lv_field_17)
              (lv_field_18)
              (lv_field_19)
              (lv_field_20).

     " handle data is deleted
     if lv_delflag = 'Y'.

      " prepare dync data
      create data ls_ref type (lv_src_tabname).
      assign ls_ref->* to <wa> .

      if gt_sor_tab is initial .
        create data gt_sor_tab type table of (lv_src_tabname).
      endif.

      " avoid reload data several times
      assign gt_sor_tab->* to <tab> .
      "if <tab>[] is initial .
        " push data
        select distinct (lv_field_list)
          into table <tab>
          from (lv_src_tabname)
           for all entries in source_package
         where (lv_forallentries)
        .
      "endif.

      loop at source_package assigning field-symbol(<souce_fields>) .
        clear: lv_deleted, lv_where.
        " fetch each data
        do lv_index times.
          lv_where = zbw_finance_library=>get_where_condition( lt_otab   = lt_otab
                                                               lv_index  = sy-index
                                                               lv_source = <souce_fields>
                                                               lv_where  = lv_where ) .
        enddo.
        " assume the data is deleted
        lv_deleted = 'Y' .
        loop at <tab> assigning <wa> where (lv_where) .
          free lv_deleted .   " fix bug: can't change data
          lv_deleted = 'N' .
          exit .
        endloop.
        assign component 'RECORDMODE' of structure <souce_fields> to <fs_field> .
        if <fs_field> is assigned.
         if lv_deleted = 'Y'.
           <fs_field> = 'D' .
         elseif lv_deleted = 'N'.
           <fs_field> = 'N' .
         endif.
        endif .

      endloop .

     endif .


  endmethod.