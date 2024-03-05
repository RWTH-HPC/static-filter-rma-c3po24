; ModuleID = 'results-20240305-201640/PARCOACH-static/conflict/009-MPI-conflict-acc-load-local-no.c'
source_filename = "results-20240305-201640/PARCOACH-static/conflict/009-MPI-conflict-acc-load-local-no.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque
%struct.ompi_predefined_op_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@ompi_mpi_op_sum = external global %struct.ompi_predefined_op_t, align 1
@.str.1 = private unnamed_addr constant [13 x i8] c"value is %d\0A\00", align 1, !dbg !7
@.str.2 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !12

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !43 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !50, metadata !DIExpression()), !dbg !51
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !52, metadata !DIExpression()), !dbg !53
  call void @llvm.dbg.declare(metadata ptr %6, metadata !54, metadata !DIExpression()), !dbg !55
  call void @llvm.dbg.declare(metadata ptr %7, metadata !56, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.declare(metadata ptr %8, metadata !58, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.declare(metadata ptr %9, metadata !63, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.declare(metadata ptr %10, metadata !66, metadata !DIExpression()), !dbg !67
  store i32 1, ptr %10, align 4, !dbg !67
  call void @llvm.dbg.declare(metadata ptr %11, metadata !68, metadata !DIExpression()), !dbg !69
  store i32 2, ptr %11, align 4, !dbg !69
  call void @llvm.dbg.declare(metadata ptr %12, metadata !70, metadata !DIExpression()), !dbg !71
  store ptr %10, ptr %12, align 8, !dbg !71
  call void @llvm.dbg.declare(metadata ptr %13, metadata !72, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.declare(metadata ptr %14, metadata !74, metadata !DIExpression()), !dbg !75
  store i32 42, ptr %14, align 4, !dbg !75
  %16 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !76
  %17 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !77
  %18 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !78
  %19 = load i32, ptr %7, align 4, !dbg !79
  %20 = icmp ne i32 %19, 2, !dbg !81
  br i1 %20, label %21, label %25, !dbg !82

21:                                               ; preds = %2
  %22 = load i32, ptr %7, align 4, !dbg !83
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %22, i32 noundef 2), !dbg !85
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !86
  br label %25, !dbg !87

25:                                               ; preds = %21, %2
  %26 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !88
  call void @llvm.dbg.declare(metadata ptr %15, metadata !89, metadata !DIExpression()), !dbg !91
  store i32 0, ptr %15, align 4, !dbg !91
  br label %27, !dbg !92

27:                                               ; preds = %35, %25
  %28 = load i32, ptr %15, align 4, !dbg !93
  %29 = icmp slt i32 %28, 10, !dbg !95
  br i1 %29, label %30, label %38, !dbg !96

30:                                               ; preds = %27
  %31 = load ptr, ptr %9, align 8, !dbg !97
  %32 = load i32, ptr %15, align 4, !dbg !99
  %33 = sext i32 %32 to i64, !dbg !97
  %34 = getelementptr inbounds i32, ptr %31, i64 %33, !dbg !97
  store i32 0, ptr %34, align 4, !dbg !100
  br label %35, !dbg !101

35:                                               ; preds = %30
  %36 = load i32, ptr %15, align 4, !dbg !102
  %37 = add nsw i32 %36, 1, !dbg !102
  store i32 %37, ptr %15, align 4, !dbg !102
  br label %27, !dbg !103, !llvm.loop !104

38:                                               ; preds = %27
  %39 = load ptr, ptr %8, align 8, !dbg !107
  %40 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %39), !dbg !108
  %41 = load i32, ptr %6, align 4, !dbg !109
  %42 = icmp eq i32 %41, 0, !dbg !111
  br i1 %42, label %43, label %49, !dbg !112

43:                                               ; preds = %38
  %44 = load ptr, ptr %8, align 8, !dbg !113
  %45 = call i32 @MPI_Accumulate(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef @ompi_mpi_op_sum, ptr noundef %44), !dbg !115
  %46 = load ptr, ptr %12, align 8, !dbg !116
  %47 = load i32, ptr %46, align 4, !dbg !117
  %48 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %47), !dbg !118
  br label %49, !dbg !119

49:                                               ; preds = %43, %38
  %50 = load ptr, ptr %8, align 8, !dbg !120
  %51 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %50), !dbg !121
  %52 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !122
  %53 = load i32, ptr %6, align 4, !dbg !123
  %54 = load ptr, ptr %12, align 8, !dbg !124
  %55 = load i32, ptr %54, align 4, !dbg !125
  %56 = load i32, ptr %11, align 4, !dbg !126
  %57 = load ptr, ptr %9, align 8, !dbg !127
  %58 = getelementptr inbounds i32, ptr %57, i64 0, !dbg !127
  %59 = load i32, ptr %58, align 4, !dbg !127
  %60 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %53, i32 noundef %55, i32 noundef %56, i32 noundef %59), !dbg !128
  %61 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !129
  %62 = call i32 @MPI_Finalize(), !dbg !130
  ret i32 0, !dbg !131
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

declare i32 @MPI_Accumulate(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!17}
!llvm.module.flags = !{!34, !35, !36, !37, !38, !39, !40, !41}
!llvm.ident = !{!42}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 41, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-static/conflict/009-MPI-conflict-acc-load-local-no.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "ca53bd2213f55e0b7f3a0620ce598282")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 54, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 104, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 13)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 59, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 94)
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !18, globals: !33, splitDebugInlining: false, nameTableKind: None)
!18 = !{!19, !23, !24, !27, !30}
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
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Op", file: !20, line: 425, baseType: !31)
!31 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !32, size: 64)
!32 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_op_t", file: !20, line: 425, flags: DIFlagFwdDecl)
!33 = !{!0, !7, !12}
!34 = !{i32 7, !"Dwarf Version", i32 5}
!35 = !{i32 2, !"Debug Info Version", i32 3}
!36 = !{i32 1, !"wchar_size", i32 4}
!37 = !{i32 7, !"openmp", i32 50}
!38 = !{i32 7, !"PIC Level", i32 2}
!39 = !{i32 7, !"PIE Level", i32 2}
!40 = !{i32 7, !"uwtable", i32 2}
!41 = !{i32 7, !"frame-pointer", i32 2}
!42 = !{!"Debian clang version 15.0.6"}
!43 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 27, type: !44, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !49)
!44 = !DISubroutineType(types: !45)
!45 = !{!46, !46, !47}
!46 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!47 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !48, size: 64)
!48 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!49 = !{}
!50 = !DILocalVariable(name: "argc", arg: 1, scope: !43, file: !2, line: 27, type: !46)
!51 = !DILocation(line: 27, column: 14, scope: !43)
!52 = !DILocalVariable(name: "argv", arg: 2, scope: !43, file: !2, line: 27, type: !47)
!53 = !DILocation(line: 27, column: 27, scope: !43)
!54 = !DILocalVariable(name: "rank", scope: !43, file: !2, line: 28, type: !46)
!55 = !DILocation(line: 28, column: 7, scope: !43)
!56 = !DILocalVariable(name: "size", scope: !43, file: !2, line: 28, type: !46)
!57 = !DILocation(line: 28, column: 13, scope: !43)
!58 = !DILocalVariable(name: "win", scope: !43, file: !2, line: 29, type: !59)
!59 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !60)
!60 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !61, size: 64)
!61 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!62 = !DILocation(line: 29, column: 11, scope: !43)
!63 = !DILocalVariable(name: "win_base", scope: !43, file: !2, line: 30, type: !64)
!64 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !46, size: 64)
!65 = !DILocation(line: 30, column: 8, scope: !43)
!66 = !DILocalVariable(name: "value", scope: !43, file: !2, line: 31, type: !46)
!67 = !DILocation(line: 31, column: 7, scope: !43)
!68 = !DILocalVariable(name: "value2", scope: !43, file: !2, line: 31, type: !46)
!69 = !DILocation(line: 31, column: 18, scope: !43)
!70 = !DILocalVariable(name: "buf", scope: !43, file: !2, line: 32, type: !64)
!71 = !DILocation(line: 32, column: 8, scope: !43)
!72 = !DILocalVariable(name: "result", scope: !43, file: !2, line: 33, type: !46)
!73 = !DILocation(line: 33, column: 7, scope: !43)
!74 = !DILocalVariable(name: "token", scope: !43, file: !2, line: 34, type: !46)
!75 = !DILocation(line: 34, column: 7, scope: !43)
!76 = !DILocation(line: 36, column: 3, scope: !43)
!77 = !DILocation(line: 37, column: 3, scope: !43)
!78 = !DILocation(line: 38, column: 3, scope: !43)
!79 = !DILocation(line: 40, column: 7, scope: !80)
!80 = distinct !DILexicalBlock(scope: !43, file: !2, line: 40, column: 7)
!81 = !DILocation(line: 40, column: 12, scope: !80)
!82 = !DILocation(line: 40, column: 7, scope: !43)
!83 = !DILocation(line: 41, column: 65, scope: !84)
!84 = distinct !DILexicalBlock(scope: !80, file: !2, line: 40, column: 25)
!85 = !DILocation(line: 41, column: 5, scope: !84)
!86 = !DILocation(line: 42, column: 5, scope: !84)
!87 = !DILocation(line: 43, column: 3, scope: !84)
!88 = !DILocation(line: 45, column: 3, scope: !43)
!89 = !DILocalVariable(name: "i", scope: !90, file: !2, line: 47, type: !46)
!90 = distinct !DILexicalBlock(scope: !43, file: !2, line: 47, column: 3)
!91 = !DILocation(line: 47, column: 12, scope: !90)
!92 = !DILocation(line: 47, column: 8, scope: !90)
!93 = !DILocation(line: 47, column: 19, scope: !94)
!94 = distinct !DILexicalBlock(scope: !90, file: !2, line: 47, column: 3)
!95 = !DILocation(line: 47, column: 21, scope: !94)
!96 = !DILocation(line: 47, column: 3, scope: !90)
!97 = !DILocation(line: 48, column: 5, scope: !98)
!98 = distinct !DILexicalBlock(scope: !94, file: !2, line: 47, column: 38)
!99 = !DILocation(line: 48, column: 14, scope: !98)
!100 = !DILocation(line: 48, column: 17, scope: !98)
!101 = !DILocation(line: 49, column: 3, scope: !98)
!102 = !DILocation(line: 47, column: 34, scope: !94)
!103 = !DILocation(line: 47, column: 3, scope: !94)
!104 = distinct !{!104, !96, !105, !106}
!105 = !DILocation(line: 49, column: 3, scope: !90)
!106 = !{!"llvm.loop.mustprogress"}
!107 = !DILocation(line: 51, column: 20, scope: !43)
!108 = !DILocation(line: 51, column: 3, scope: !43)
!109 = !DILocation(line: 52, column: 7, scope: !110)
!110 = distinct !DILexicalBlock(scope: !43, file: !2, line: 52, column: 7)
!111 = !DILocation(line: 52, column: 12, scope: !110)
!112 = !DILocation(line: 52, column: 7, scope: !43)
!113 = !DILocation(line: 53, column: 67, scope: !114)
!114 = distinct !DILexicalBlock(scope: !110, file: !2, line: 52, column: 18)
!115 = !DILocation(line: 53, column: 5, scope: !114)
!116 = !DILocation(line: 54, column: 30, scope: !114)
!117 = !DILocation(line: 54, column: 29, scope: !114)
!118 = !DILocation(line: 54, column: 5, scope: !114)
!119 = !DILocation(line: 55, column: 3, scope: !114)
!120 = !DILocation(line: 56, column: 20, scope: !43)
!121 = !DILocation(line: 56, column: 3, scope: !43)
!122 = !DILocation(line: 58, column: 3, scope: !43)
!123 = !DILocation(line: 61, column: 10, scope: !43)
!124 = !DILocation(line: 61, column: 17, scope: !43)
!125 = !DILocation(line: 61, column: 16, scope: !43)
!126 = !DILocation(line: 61, column: 22, scope: !43)
!127 = !DILocation(line: 61, column: 30, scope: !43)
!128 = !DILocation(line: 59, column: 3, scope: !43)
!129 = !DILocation(line: 63, column: 3, scope: !43)
!130 = !DILocation(line: 64, column: 3, scope: !43)
!131 = !DILocation(line: 66, column: 3, scope: !43)
