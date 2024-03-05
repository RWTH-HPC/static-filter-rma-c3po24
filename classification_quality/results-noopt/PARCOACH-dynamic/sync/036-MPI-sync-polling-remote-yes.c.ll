; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/sync/036-MPI-sync-polling-remote-yes.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/sync/036-MPI-sync-polling-remote-yes.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@.str.1 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !7

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !35 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca ptr, align 8
  %13 = alloca i32, align 4
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca ptr, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !42, metadata !DIExpression()), !dbg !43
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !44, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.declare(metadata ptr %6, metadata !46, metadata !DIExpression()), !dbg !47
  call void @llvm.dbg.declare(metadata ptr %7, metadata !48, metadata !DIExpression()), !dbg !49
  call void @llvm.dbg.declare(metadata ptr %8, metadata !50, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.declare(metadata ptr %9, metadata !55, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.declare(metadata ptr %10, metadata !58, metadata !DIExpression()), !dbg !59
  store i32 1, ptr %10, align 4, !dbg !59
  call void @llvm.dbg.declare(metadata ptr %11, metadata !60, metadata !DIExpression()), !dbg !61
  store i32 2, ptr %11, align 4, !dbg !61
  call void @llvm.dbg.declare(metadata ptr %12, metadata !62, metadata !DIExpression()), !dbg !63
  store ptr %10, ptr %12, align 8, !dbg !63
  call void @llvm.dbg.declare(metadata ptr %13, metadata !64, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.declare(metadata ptr %14, metadata !66, metadata !DIExpression()), !dbg !67
  store i32 42, ptr %14, align 4, !dbg !67
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !68
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !69
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !70
  %21 = load i32, ptr %7, align 4, !dbg !71
  %22 = icmp ne i32 %21, 2, !dbg !73
  br i1 %22, label %23, label %27, !dbg !74

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !75
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !77
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !78
  br label %27, !dbg !79

27:                                               ; preds = %23, %2
  %28 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !80
  call void @llvm.dbg.declare(metadata ptr %15, metadata !81, metadata !DIExpression()), !dbg !83
  store i32 0, ptr %15, align 4, !dbg !83
  br label %29, !dbg !84

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !85
  %31 = icmp slt i32 %30, 10, !dbg !87
  br i1 %31, label %32, label %40, !dbg !88

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !89
  %34 = load i32, ptr %15, align 4, !dbg !91
  %35 = sext i32 %34 to i64, !dbg !89
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !89
  store i32 0, ptr %36, align 4, !dbg !92
  br label %37, !dbg !93

37:                                               ; preds = %32
  %38 = load i32, ptr %15, align 4, !dbg !94
  %39 = add nsw i32 %38, 1, !dbg !94
  store i32 %39, ptr %15, align 4, !dbg !94
  br label %29, !dbg !95, !llvm.loop !96

40:                                               ; preds = %29
  %41 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !99
  %42 = load i32, ptr %6, align 4, !dbg !100
  %43 = icmp eq i32 %42, 0, !dbg !102
  br i1 %43, label %44, label %51, !dbg !103

44:                                               ; preds = %40
  %45 = load ptr, ptr %8, align 8, !dbg !104
  %46 = call i32 @MPI_Win_lock(i32 noundef 1, i32 noundef 1, i32 noundef 0, ptr noundef %45), !dbg !106
  call void @llvm.dbg.declare(metadata ptr %16, metadata !107, metadata !DIExpression()), !dbg !108
  store i32 1, ptr %16, align 4, !dbg !108
  %47 = load ptr, ptr %8, align 8, !dbg !109
  %48 = call i32 @MPI_Put(ptr noundef %16, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %47), !dbg !110
  %49 = load ptr, ptr %8, align 8, !dbg !111
  %50 = call i32 @MPI_Win_unlock(i32 noundef 1, ptr noundef %49), !dbg !112
  br label %65, !dbg !113

51:                                               ; preds = %40
  call void @llvm.dbg.declare(metadata ptr %17, metadata !114, metadata !DIExpression()), !dbg !118
  %52 = load ptr, ptr %9, align 8, !dbg !119
  %53 = getelementptr inbounds i32, ptr %52, i64 0, !dbg !119
  store ptr %53, ptr %17, align 8, !dbg !118
  br label %54, !dbg !120

54:                                               ; preds = %58, %51
  %55 = load ptr, ptr %17, align 8, !dbg !121
  %56 = load volatile i32, ptr %55, align 4, !dbg !122
  %57 = icmp ne i32 %56, 1, !dbg !123
  br i1 %57, label %58, label %64, !dbg !120

58:                                               ; preds = %54
  %59 = call i32 (i32, ...) @sleep(i32 noundef 1), !dbg !124
  %60 = load ptr, ptr %8, align 8, !dbg !126
  %61 = call i32 @MPI_Win_lock(i32 noundef 1, i32 noundef 1, i32 noundef 0, ptr noundef %60), !dbg !127
  %62 = load ptr, ptr %8, align 8, !dbg !128
  %63 = call i32 @MPI_Win_unlock(i32 noundef 1, ptr noundef %62), !dbg !129
  br label %54, !dbg !120, !llvm.loop !130

64:                                               ; preds = %54
  br label %65

65:                                               ; preds = %64, %44
  %66 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !132
  %67 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !133
  %68 = load i32, ptr %6, align 4, !dbg !134
  %69 = load ptr, ptr %12, align 8, !dbg !135
  %70 = load i32, ptr %69, align 4, !dbg !136
  %71 = load i32, ptr %11, align 4, !dbg !137
  %72 = load ptr, ptr %9, align 8, !dbg !138
  %73 = getelementptr inbounds i32, ptr %72, i64 0, !dbg !138
  %74 = load i32, ptr %73, align 4, !dbg !138
  %75 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %68, i32 noundef %70, i32 noundef %71, i32 noundef %74), !dbg !139
  %76 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !140
  %77 = call i32 @MPI_Finalize(), !dbg !141
  ret i32 0, !dbg !142
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_lock(i32 noundef, i32 noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_unlock(i32 noundef, ptr noundef) #2

declare i32 @sleep(...) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!26, !27, !28, !29, !30, !31, !32, !33}
!llvm.ident = !{!34}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 44, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/sync/036-MPI-sync-polling-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "85e6c85010cb9d4936a455d6140bf525")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 78, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 94)
!12 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !13, globals: !25, splitDebugInlining: false, nameTableKind: None)
!13 = !{!14, !18, !19, !22}
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Comm", file: !15, line: 419, baseType: !16)
!15 = !DIFile(filename: "/usr/lib/x86_64-linux-gnu/openmpi/include/mpi.h", directory: "", checksumkind: CSK_MD5, checksum: "c3ca5be6f2c6a6e16d01846df47c0b49")
!16 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!17 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_communicator_t", file: !15, line: 419, flags: DIFlagFwdDecl)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Info", file: !15, line: 424, baseType: !20)
!20 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !21, size: 64)
!21 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_info_t", file: !15, line: 424, flags: DIFlagFwdDecl)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Datatype", file: !15, line: 420, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !24, size: 64)
!24 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_datatype_t", file: !15, line: 420, flags: DIFlagFwdDecl)
!25 = !{!0, !7}
!26 = !{i32 7, !"Dwarf Version", i32 5}
!27 = !{i32 2, !"Debug Info Version", i32 3}
!28 = !{i32 1, !"wchar_size", i32 4}
!29 = !{i32 7, !"openmp", i32 50}
!30 = !{i32 7, !"PIC Level", i32 2}
!31 = !{i32 7, !"PIE Level", i32 2}
!32 = !{i32 7, !"uwtable", i32 2}
!33 = !{i32 7, !"frame-pointer", i32 2}
!34 = !{!"Debian clang version 15.0.6"}
!35 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 30, type: !36, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!36 = !DISubroutineType(types: !37)
!37 = !{!38, !38, !39}
!38 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!39 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !40, size: 64)
!40 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!41 = !{}
!42 = !DILocalVariable(name: "argc", arg: 1, scope: !35, file: !2, line: 30, type: !38)
!43 = !DILocation(line: 30, column: 14, scope: !35)
!44 = !DILocalVariable(name: "argv", arg: 2, scope: !35, file: !2, line: 30, type: !39)
!45 = !DILocation(line: 30, column: 27, scope: !35)
!46 = !DILocalVariable(name: "rank", scope: !35, file: !2, line: 31, type: !38)
!47 = !DILocation(line: 31, column: 7, scope: !35)
!48 = !DILocalVariable(name: "size", scope: !35, file: !2, line: 31, type: !38)
!49 = !DILocation(line: 31, column: 13, scope: !35)
!50 = !DILocalVariable(name: "win", scope: !35, file: !2, line: 32, type: !51)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !52)
!52 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64)
!53 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!54 = !DILocation(line: 32, column: 11, scope: !35)
!55 = !DILocalVariable(name: "win_base", scope: !35, file: !2, line: 33, type: !56)
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !38, size: 64)
!57 = !DILocation(line: 33, column: 8, scope: !35)
!58 = !DILocalVariable(name: "value", scope: !35, file: !2, line: 34, type: !38)
!59 = !DILocation(line: 34, column: 7, scope: !35)
!60 = !DILocalVariable(name: "value2", scope: !35, file: !2, line: 34, type: !38)
!61 = !DILocation(line: 34, column: 18, scope: !35)
!62 = !DILocalVariable(name: "buf", scope: !35, file: !2, line: 35, type: !56)
!63 = !DILocation(line: 35, column: 8, scope: !35)
!64 = !DILocalVariable(name: "result", scope: !35, file: !2, line: 36, type: !38)
!65 = !DILocation(line: 36, column: 7, scope: !35)
!66 = !DILocalVariable(name: "token", scope: !35, file: !2, line: 37, type: !38)
!67 = !DILocation(line: 37, column: 7, scope: !35)
!68 = !DILocation(line: 39, column: 3, scope: !35)
!69 = !DILocation(line: 40, column: 3, scope: !35)
!70 = !DILocation(line: 41, column: 3, scope: !35)
!71 = !DILocation(line: 43, column: 7, scope: !72)
!72 = distinct !DILexicalBlock(scope: !35, file: !2, line: 43, column: 7)
!73 = !DILocation(line: 43, column: 12, scope: !72)
!74 = !DILocation(line: 43, column: 7, scope: !35)
!75 = !DILocation(line: 44, column: 65, scope: !76)
!76 = distinct !DILexicalBlock(scope: !72, file: !2, line: 43, column: 25)
!77 = !DILocation(line: 44, column: 5, scope: !76)
!78 = !DILocation(line: 45, column: 5, scope: !76)
!79 = !DILocation(line: 46, column: 3, scope: !76)
!80 = !DILocation(line: 48, column: 3, scope: !35)
!81 = !DILocalVariable(name: "i", scope: !82, file: !2, line: 50, type: !38)
!82 = distinct !DILexicalBlock(scope: !35, file: !2, line: 50, column: 3)
!83 = !DILocation(line: 50, column: 12, scope: !82)
!84 = !DILocation(line: 50, column: 8, scope: !82)
!85 = !DILocation(line: 50, column: 19, scope: !86)
!86 = distinct !DILexicalBlock(scope: !82, file: !2, line: 50, column: 3)
!87 = !DILocation(line: 50, column: 21, scope: !86)
!88 = !DILocation(line: 50, column: 3, scope: !82)
!89 = !DILocation(line: 51, column: 5, scope: !90)
!90 = distinct !DILexicalBlock(scope: !86, file: !2, line: 50, column: 38)
!91 = !DILocation(line: 51, column: 14, scope: !90)
!92 = !DILocation(line: 51, column: 17, scope: !90)
!93 = !DILocation(line: 52, column: 3, scope: !90)
!94 = !DILocation(line: 50, column: 34, scope: !86)
!95 = !DILocation(line: 50, column: 3, scope: !86)
!96 = distinct !{!96, !88, !97, !98}
!97 = !DILocation(line: 52, column: 3, scope: !82)
!98 = !{!"llvm.loop.mustprogress"}
!99 = !DILocation(line: 54, column: 3, scope: !35)
!100 = !DILocation(line: 56, column: 7, scope: !101)
!101 = distinct !DILexicalBlock(scope: !35, file: !2, line: 56, column: 7)
!102 = !DILocation(line: 56, column: 12, scope: !101)
!103 = !DILocation(line: 56, column: 7, scope: !35)
!104 = !DILocation(line: 57, column: 44, scope: !105)
!105 = distinct !DILexicalBlock(scope: !101, file: !2, line: 56, column: 18)
!106 = !DILocation(line: 57, column: 5, scope: !105)
!107 = !DILocalVariable(name: "value", scope: !105, file: !2, line: 59, type: !38)
!108 = !DILocation(line: 59, column: 9, scope: !105)
!109 = !DILocation(line: 61, column: 51, scope: !105)
!110 = !DILocation(line: 61, column: 5, scope: !105)
!111 = !DILocation(line: 62, column: 23, scope: !105)
!112 = !DILocation(line: 62, column: 5, scope: !105)
!113 = !DILocation(line: 63, column: 3, scope: !105)
!114 = !DILocalVariable(name: "flag", scope: !115, file: !2, line: 65, type: !116)
!115 = distinct !DILexicalBlock(scope: !101, file: !2, line: 63, column: 10)
!116 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !117, size: 64)
!117 = !DIDerivedType(tag: DW_TAG_volatile_type, baseType: !38)
!118 = !DILocation(line: 65, column: 19, scope: !115)
!119 = !DILocation(line: 65, column: 27, scope: !115)
!120 = !DILocation(line: 67, column: 5, scope: !115)
!121 = !DILocation(line: 67, column: 13, scope: !115)
!122 = !DILocation(line: 67, column: 12, scope: !115)
!123 = !DILocation(line: 67, column: 18, scope: !115)
!124 = !DILocation(line: 68, column: 7, scope: !125)
!125 = distinct !DILexicalBlock(scope: !115, file: !2, line: 67, column: 24)
!126 = !DILocation(line: 70, column: 46, scope: !125)
!127 = !DILocation(line: 70, column: 7, scope: !125)
!128 = !DILocation(line: 71, column: 25, scope: !125)
!129 = !DILocation(line: 71, column: 7, scope: !125)
!130 = distinct !{!130, !120, !131, !98}
!131 = !DILocation(line: 72, column: 5, scope: !115)
!132 = !DILocation(line: 75, column: 3, scope: !35)
!133 = !DILocation(line: 77, column: 3, scope: !35)
!134 = !DILocation(line: 80, column: 10, scope: !35)
!135 = !DILocation(line: 80, column: 17, scope: !35)
!136 = !DILocation(line: 80, column: 16, scope: !35)
!137 = !DILocation(line: 80, column: 22, scope: !35)
!138 = !DILocation(line: 80, column: 30, scope: !35)
!139 = !DILocation(line: 78, column: 3, scope: !35)
!140 = !DILocation(line: 82, column: 3, scope: !35)
!141 = !DILocation(line: 83, column: 3, scope: !35)
!142 = !DILocation(line: 85, column: 3, scope: !35)
