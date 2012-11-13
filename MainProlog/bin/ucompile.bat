@echo generates Jinni bytecode application using BinProlog
@echo use ucompile(['file.pl'],'wam.bp','file.bp')
bp.exe bpco.wam "and(ucompile(['%1.pl'],'wam.bp','%1.bp'),halt)"
