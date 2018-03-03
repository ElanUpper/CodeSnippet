" 处理问题
"    加入模型A读取了主数据B的一些字段，但是如果主数据数据变更不会反应到模型A中；需要反向更新
" class使用方法
"    加入主数据tr中，输入模型A的active table技术名称以及internal table即可

  class-methods upd_target_data
    importing
      !i_target_name type tabname
    changing
      !c_source_package type any table .

  method upd_target_data.

    data: ls_target     type dd02l-tabname ,
          ls_tab        type ref to data ,
          lt_tab        type ref to data .

    field-symbols: <ls> type any ,
                   <lt> type any table .

    ls_target = i_target_name .
    condense ls_target no-gaps .
    translate ls_target to upper case .

    select single tabname
      into @data(p_tab)
     from dd02l
     where tabname  = @ls_target
       and as4local = 'A' .

    check p_tab is not initial .

    " prepare dync data
    create data ls_tab type (ls_target).
    assign ls_tab->* to <ls> .
    create data lt_tab type table of (ls_target).
    assign lt_tab->* to <lt> .

    " get keys & fields
    select fieldname into table @data(it_fields)
      from dd03l
     where tabname  = @ls_target
       and as4local = 'A'
       and fieldname not in ( 'RECORDMODE', 'MANDT' )
       order by position .

     loop at c_source_package assigning field-symbol(<fs_souce>).
       clear: <ls> .
       loop at it_fields assigning field-symbol(<fs_field>).
         assign component <fs_field> of structure <fs_souce> to field-symbol(<fs_wa>) .
         assign component <fs_field> of structure <ls> to field-symbol(<fs_wa1>) .
         if <fs_wa> is assigned and <fs_wa1> is assigned.
           <fs_wa1> = <fs_wa> .
         endif.
       endloop.
       assign component 'MANDT' of structure <ls> to <fs_wa1> .
       <fs_wa1> = sy-mandt .
       insert <ls> into table <lt> .
       unassign <fs_wa1> .
     endloop.

     if <lt>[] is not initial.
       update (ls_target) from table <lt> .
       if sy-subrc = 0.
         commit work .
       else.
         rollback work .
       endif.
     endif.

  endmethod.