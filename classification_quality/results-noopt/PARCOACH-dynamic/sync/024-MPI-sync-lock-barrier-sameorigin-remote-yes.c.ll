; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/sync/024-MPI-sync-lock-barrier-sameorigin-remote-yes.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/sync/024-MPI-sync-lock-barrier-sameorigin-remote-yes.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@.str.1 = private unnamed_addr constant [19 x i8] c"win_base[0] is %d\0A\00", align 1, !dbg !7
@.str.2 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !12

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !40 {
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
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !47, metadata !DIExpression()), !dbg !48
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !49, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.declare(metadata ptr %6, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.declare(metadata ptr %7, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.declare(metadata ptr %8, metadata !55, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.declare(metadata ptr %9, metadata !60, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.declare(metadata ptr %10, metadata !63, metadata !DIExpression()), !dbg !64
  store i32 1, ptr %10, align 4, !dbg !64
  call void @llvm.dbg.declare(metadata ptr %11, metadata !65, metadata !DIExpression()), !dbg !66
  store i32 2, ptr %11, align 4, !dbg !66
  call void @llvm.dbg.declare(metadata ptr %12, metadata !67, metadata !DIExpression()), !dbg !68
  store ptr %10, ptr %12, align 8, !dbg !68
  call void @llvm.dbg.declare(metadata ptr %13, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.declare(metadata ptr %14, metadata !71, metadata !DIExpression()), !dbg !72
  store i32 42, ptr %14, align 4, !dbg !72
  %16 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !73
  %17 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !74
  %18 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !75
  %19 = load i32, ptr %7, align 4, !dbg !76
  %20 = icmp ne i32 %19, 2, !dbg !78
  br i1 %20, label %21, label %25, !dbg !79

21:                                               ; preds = %2
  %22 = load i32, ptr %7, align 4, !dbg !80
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %22, i32 noundef 2), !dbg !82
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !83
  br label %25, !dbg !84

25:                                               ; preds = %21, %2
  %26 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !85
  call void @llvm.dbg.declare(metadata ptr %15, metadata !86, metadata !DIExpression()), !dbg !88
  store i32 0, ptr %15, align 4, !dbg !88
  br label %27, !dbg !89

27:                                               ; preds = %35, %25
  %28 = load i32, ptr %15, align 4, !dbg !90
  %29 = icmp slt i32 %28, 10, !dbg !92
  br i1 %29, label %30, label %38, !dbg !93

30:                                               ; preds = %27
  %31 = load ptr, ptr %9, align 8, !dbg !94
  %32 = load i32, ptr %15, align 4, !dbg !96
  %33 = sext i32 %32 to i64, !dbg !94
  %34 = getelementptr inbounds i32, ptr %31, i64 %33, !dbg !94
  store i32 0, ptr %34, align 4, !dbg !97
  br label %35, !dbg !98

35:                                               ; preds = %30
  %36 = load i32, ptr %15, align 4, !dbg !99
  %37 = add nsw i32 %36, 1, !dbg !99
  store i32 %37, ptr %15, align 4, !dbg !99
  br label %27, !dbg !100, !llvm.loop !101

38:                                               ; preds = %27
  %39 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !104
  %40 = load i32, ptr %6, align 4, !dbg !105
  %41 = icmp eq i32 %40, 0, !dbg !107
  br i1 %41, label %42, label %51, !dbg !108

42:                                               ; preds = %38
  store i32 1, ptr %10, align 4, !dbg !109
  %43 = load ptr, ptr %8, align 8, !dbg !111
  %44 = call i32 @MPI_Win_lock(i32 noundef 2, i32 noundef 1, i32 noundef 0, ptr noundef %43), !dbg !112
  %45 = load ptr, ptr %8, align 8, !dbg !113
  %46 = call i32 @MPI_Put(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %45), !dbg !114
  %47 = load ptr, ptr %8, align 8, !dbg !115
  %48 = call i32 @MPI_Get(ptr noundef %11, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %47), !dbg !116
  %49 = load ptr, ptr %8, align 8, !dbg !117
  %50 = call i32 @MPI_Win_unlock(i32 noundef 1, ptr noundef %49), !dbg !118
  br label %51, !dbg !119

51:                                               ; preds = %42, %38
  %52 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !120
  %53 = load i32, ptr %6, align 4, !dbg !121
  %54 = icmp eq i32 %53, 1, !dbg !123
  br i1 %54, label %55, label %60, !dbg !124

55:                                               ; preds = %51
  %56 = load ptr, ptr %9, align 8, !dbg !125
  %57 = getelementptr inbounds i32, ptr %56, i64 0, !dbg !125
  %58 = load i32, ptr %57, align 4, !dbg !125
  %59 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %58), !dbg !127
  br label %60, !dbg !128

60:                                               ; preds = %55, %51
  %61 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !129
  %62 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !130
  %63 = load i32, ptr %6, align 4, !dbg !131
  %64 = load ptr, ptr %12, align 8, !dbg !132
  %65 = load i32, ptr %64, align 4, !dbg !133
  %66 = load i32, ptr %11, align 4, !dbg !134
  %67 = load ptr, ptr %9, align 8, !dbg !135
  %68 = getelementptr inbounds i32, ptr %67, i64 0, !dbg !135
  %69 = load i32, ptr %68, align 4, !dbg !135
  %70 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %63, i32 noundef %65, i32 noundef %66, i32 noundef %69), !dbg !136
  %71 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !137
  %72 = call i32 @MPI_Finalize(), !dbg !138
  ret i32 0, !dbg !139
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

declare i32 @MPI_Get(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_unlock(i32 noundef, ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!17}
!llvm.module.flags = !{!31, !32, !33, !34, !35, !36, !37, !38}
!llvm.ident = !{!39}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 42, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/sync/024-MPI-sync-lock-barrier-sameorigin-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "92ffcc8968b66ea92c9ba02d4b2600f4")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 66, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 152, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 19)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 72, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 94)
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !18, globals: !30, splitDebugInlining: false, nameTableKind: None)
!18 = !{!19, !23, !24, !27}
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Comm", file: !20, line: 419, baseType: !21)
!20 = !DIFile(filename: "/usr/lib/x86_64-linux-gnu/openmpi/include/mpi.h", directory: "", checksumkind: CSK_MD5, checksum: "c3ca5be6f2c6a6e16d01846df47c0b49")
!21 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !22, size: 64)
!22 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_communicator_t", file: !20, line: 419, flags: DIFlagFwdDecl)
!23 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Info", file: !20, line: 424, baseType: !25)
!25 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !26, size: 64)
!26 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_info_t", file: !20, line: 424, flags: DIFlagFwdDecl)
!27 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Datatype", file: !20, line: 420, baseType: !28)
!28 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !29, size: 64)
!29 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_datatype_t", file: !20, line: 420, flags: DIFlagFwdDecl)
!30 = !{!0, !7, !12}
!31 = !{i32 7, !"Dwarf Version", i32 5}
!32 = !{i32 2, !"Debug Info Version", i32 3}
!33 = !{i32 1, !"wchar_size", i32 4}
!34 = !{i32 7, !"openmp", i32 50}
!35 = !{i32 7, !"PIC Level", i32 2}
!36 = !{i32 7, !"PIE Level", i32 2}
!37 = !{i32 7, !"uwtable", i32 2}
!38 = !{i32 7, !"frame-pointer", i32 2}
!39 = !{!"Debian clang version 15.0.6"}
!40 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 28, type: !41, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!41 = !DISubroutineType(types: !42)
!42 = !{!43, !43, !44}
!43 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !45, size: 64)
!45 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!46 = !{}
!47 = !DILocalVariable(name: "argc", arg: 1, scope: !40, file: !2, line: 28, type: !43)
!48 = !DILocation(line: 28, column: 14, scope: !40)
!49 = !DILocalVariable(name: "argv", arg: 2, scope: !40, file: !2, line: 28, type: !44)
!50 = !DILocation(line: 28, column: 27, scope: !40)
!51 = !DILocalVariable(name: "rank", scope: !40, file: !2, line: 29, type: !43)
!52 = !DILocation(line: 29, column: 7, scope: !40)
!53 = !DILocalVariable(name: "size", scope: !40, file: !2, line: 29, type: !43)
!54 = !DILocation(line: 29, column: 13, scope: !40)
!55 = !DILocalVariable(name: "win", scope: !40, file: !2, line: 30, type: !56)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !57)
!57 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !58, size: 64)
!58 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!59 = !DILocation(line: 30, column: 11, scope: !40)
!60 = !DILocalVariable(name: "win_base", scope: !40, file: !2, line: 31, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!62 = !DILocation(line: 31, column: 8, scope: !40)
!63 = !DILocalVariable(name: "value", scope: !40, file: !2, line: 32, type: !43)
!64 = !DILocation(line: 32, column: 7, scope: !40)
!65 = !DILocalVariable(name: "value2", scope: !40, file: !2, line: 32, type: !43)
!66 = !DILocation(line: 32, column: 18, scope: !40)
!67 = !DILocalVariable(name: "buf", scope: !40, file: !2, line: 33, type: !61)
!68 = !DILocation(line: 33, column: 8, scope: !40)
!69 = !DILocalVariable(name: "result", scope: !40, file: !2, line: 34, type: !43)
!70 = !DILocation(line: 34, column: 7, scope: !40)
!71 = !DILocalVariable(name: "token", scope: !40, file: !2, line: 35, type: !43)
!72 = !DILocation(line: 35, column: 7, scope: !40)
!73 = !DILocation(line: 37, column: 3, scope: !40)
!74 = !DILocation(line: 38, column: 3, scope: !40)
!75 = !DILocation(line: 39, column: 3, scope: !40)
!76 = !DILocation(line: 41, column: 7, scope: !77)
!77 = distinct !DILexicalBlock(scope: !40, file: !2, line: 41, column: 7)
!78 = !DILocation(line: 41, column: 12, scope: !77)
!79 = !DILocation(line: 41, column: 7, scope: !40)
!80 = !DILocation(line: 42, column: 65, scope: !81)
!81 = distinct !DILexicalBlock(scope: !77, file: !2, line: 41, column: 25)
!82 = !DILocation(line: 42, column: 5, scope: !81)
!83 = !DILocation(line: 43, column: 5, scope: !81)
!84 = !DILocation(line: 44, column: 3, scope: !81)
!85 = !DILocation(line: 46, column: 3, scope: !40)
!86 = !DILocalVariable(name: "i", scope: !87, file: !2, line: 48, type: !43)
!87 = distinct !DILexicalBlock(scope: !40, file: !2, line: 48, column: 3)
!88 = !DILocation(line: 48, column: 12, scope: !87)
!89 = !DILocation(line: 48, column: 8, scope: !87)
!90 = !DILocation(line: 48, column: 19, scope: !91)
!91 = distinct !DILexicalBlock(scope: !87, file: !2, line: 48, column: 3)
!92 = !DILocation(line: 48, column: 21, scope: !91)
!93 = !DILocation(line: 48, column: 3, scope: !87)
!94 = !DILocation(line: 49, column: 5, scope: !95)
!95 = distinct !DILexicalBlock(scope: !91, file: !2, line: 48, column: 38)
!96 = !DILocation(line: 49, column: 14, scope: !95)
!97 = !DILocation(line: 49, column: 17, scope: !95)
!98 = !DILocation(line: 50, column: 3, scope: !95)
!99 = !DILocation(line: 48, column: 34, scope: !91)
!100 = !DILocation(line: 48, column: 3, scope: !91)
!101 = distinct !{!101, !93, !102, !103}
!102 = !DILocation(line: 50, column: 3, scope: !87)
!103 = !{!"llvm.loop.mustprogress"}
!104 = !DILocation(line: 52, column: 3, scope: !40)
!105 = !DILocation(line: 54, column: 7, scope: !106)
!106 = distinct !DILexicalBlock(scope: !40, file: !2, line: 54, column: 7)
!107 = !DILocation(line: 54, column: 12, scope: !106)
!108 = !DILocation(line: 54, column: 7, scope: !40)
!109 = !DILocation(line: 55, column: 11, scope: !110)
!110 = distinct !DILexicalBlock(scope: !106, file: !2, line: 54, column: 18)
!111 = !DILocation(line: 56, column: 41, scope: !110)
!112 = !DILocation(line: 56, column: 5, scope: !110)
!113 = !DILocation(line: 58, column: 51, scope: !110)
!114 = !DILocation(line: 58, column: 5, scope: !110)
!115 = !DILocation(line: 60, column: 52, scope: !110)
!116 = !DILocation(line: 60, column: 5, scope: !110)
!117 = !DILocation(line: 61, column: 23, scope: !110)
!118 = !DILocation(line: 61, column: 5, scope: !110)
!119 = !DILocation(line: 62, column: 3, scope: !110)
!120 = !DILocation(line: 64, column: 3, scope: !40)
!121 = !DILocation(line: 65, column: 7, scope: !122)
!122 = distinct !DILexicalBlock(scope: !40, file: !2, line: 65, column: 7)
!123 = !DILocation(line: 65, column: 12, scope: !122)
!124 = !DILocation(line: 65, column: 7, scope: !40)
!125 = !DILocation(line: 66, column: 35, scope: !126)
!126 = distinct !DILexicalBlock(scope: !122, file: !2, line: 65, column: 18)
!127 = !DILocation(line: 66, column: 5, scope: !126)
!128 = !DILocation(line: 67, column: 3, scope: !126)
!129 = !DILocation(line: 69, column: 3, scope: !40)
!130 = !DILocation(line: 71, column: 3, scope: !40)
!131 = !DILocation(line: 74, column: 10, scope: !40)
!132 = !DILocation(line: 74, column: 17, scope: !40)
!133 = !DILocation(line: 74, column: 16, scope: !40)
!134 = !DILocation(line: 74, column: 22, scope: !40)
!135 = !DILocation(line: 74, column: 30, scope: !40)
!136 = !DILocation(line: 72, column: 3, scope: !40)
!137 = !DILocation(line: 76, column: 3, scope: !40)
!138 = !DILocation(line: 77, column: 3, scope: !40)
!139 = !DILocation(line: 79, column: 3, scope: !40)
