; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/conflict/026-MPI-conflict-put-acc-remote-yes.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/conflict/026-MPI-conflict-put-acc-remote-yes.c"
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
@.str.1 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !7

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !38 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !45, metadata !DIExpression()), !dbg !46
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !47, metadata !DIExpression()), !dbg !48
  call void @llvm.dbg.declare(metadata ptr %6, metadata !49, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.declare(metadata ptr %7, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.declare(metadata ptr %8, metadata !53, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.declare(metadata ptr %9, metadata !58, metadata !DIExpression()), !dbg !60
  call void @llvm.dbg.declare(metadata ptr %10, metadata !61, metadata !DIExpression()), !dbg !62
  store i32 1, ptr %10, align 4, !dbg !62
  call void @llvm.dbg.declare(metadata ptr %11, metadata !63, metadata !DIExpression()), !dbg !64
  store i32 2, ptr %11, align 4, !dbg !64
  call void @llvm.dbg.declare(metadata ptr %12, metadata !65, metadata !DIExpression()), !dbg !66
  store ptr %10, ptr %12, align 8, !dbg !66
  call void @llvm.dbg.declare(metadata ptr %13, metadata !67, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.declare(metadata ptr %14, metadata !69, metadata !DIExpression()), !dbg !70
  store i32 42, ptr %14, align 4, !dbg !70
  %16 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !71
  %17 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !72
  %18 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !73
  %19 = load i32, ptr %7, align 4, !dbg !74
  %20 = icmp ne i32 %19, 3, !dbg !76
  br i1 %20, label %21, label %25, !dbg !77

21:                                               ; preds = %2
  %22 = load i32, ptr %7, align 4, !dbg !78
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %22, i32 noundef 3), !dbg !80
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !81
  br label %25, !dbg !82

25:                                               ; preds = %21, %2
  %26 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !83
  call void @llvm.dbg.declare(metadata ptr %15, metadata !84, metadata !DIExpression()), !dbg !86
  store i32 0, ptr %15, align 4, !dbg !86
  br label %27, !dbg !87

27:                                               ; preds = %35, %25
  %28 = load i32, ptr %15, align 4, !dbg !88
  %29 = icmp slt i32 %28, 10, !dbg !90
  br i1 %29, label %30, label %38, !dbg !91

30:                                               ; preds = %27
  %31 = load ptr, ptr %9, align 8, !dbg !92
  %32 = load i32, ptr %15, align 4, !dbg !94
  %33 = sext i32 %32 to i64, !dbg !92
  %34 = getelementptr inbounds i32, ptr %31, i64 %33, !dbg !92
  store i32 0, ptr %34, align 4, !dbg !95
  br label %35, !dbg !96

35:                                               ; preds = %30
  %36 = load i32, ptr %15, align 4, !dbg !97
  %37 = add nsw i32 %36, 1, !dbg !97
  store i32 %37, ptr %15, align 4, !dbg !97
  br label %27, !dbg !98, !llvm.loop !99

38:                                               ; preds = %27
  %39 = load ptr, ptr %8, align 8, !dbg !102
  %40 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %39), !dbg !103
  %41 = load i32, ptr %6, align 4, !dbg !104
  %42 = icmp eq i32 %41, 0, !dbg !106
  br i1 %42, label %43, label %47, !dbg !107

43:                                               ; preds = %38
  %44 = load ptr, ptr %12, align 8, !dbg !108
  %45 = load ptr, ptr %8, align 8, !dbg !110
  %46 = call i32 @MPI_Put(ptr noundef %44, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %45), !dbg !111
  br label %47, !dbg !112

47:                                               ; preds = %43, %38
  %48 = load i32, ptr %6, align 4, !dbg !113
  %49 = icmp eq i32 %48, 2, !dbg !115
  br i1 %49, label %50, label %53, !dbg !116

50:                                               ; preds = %47
  store i32 2, ptr %10, align 4, !dbg !117
  %51 = load ptr, ptr %8, align 8, !dbg !119
  %52 = call i32 @MPI_Accumulate(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef @ompi_mpi_op_sum, ptr noundef %51), !dbg !120
  br label %53, !dbg !121

53:                                               ; preds = %50, %47
  %54 = load ptr, ptr %8, align 8, !dbg !122
  %55 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %54), !dbg !123
  %56 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !124
  %57 = load i32, ptr %6, align 4, !dbg !125
  %58 = load ptr, ptr %12, align 8, !dbg !126
  %59 = load i32, ptr %58, align 4, !dbg !127
  %60 = load i32, ptr %11, align 4, !dbg !128
  %61 = load ptr, ptr %9, align 8, !dbg !129
  %62 = getelementptr inbounds i32, ptr %61, i64 0, !dbg !129
  %63 = load i32, ptr %62, align 4, !dbg !129
  %64 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %57, i32 noundef %59, i32 noundef %60, i32 noundef %63), !dbg !130
  %65 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !131
  %66 = call i32 @MPI_Finalize(), !dbg !132
  ret i32 0, !dbg !133
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

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Accumulate(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!29, !30, !31, !32, !33, !34, !35, !36}
!llvm.ident = !{!37}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 43, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/conflict/026-MPI-conflict-put-acc-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "20a3732fcbbb2641b44285cd5b0eb424")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 70, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 94)
!12 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !13, globals: !28, splitDebugInlining: false, nameTableKind: None)
!13 = !{!14, !18, !19, !22, !25}
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
!25 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Op", file: !15, line: 425, baseType: !26)
!26 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !27, size: 64)
!27 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_op_t", file: !15, line: 425, flags: DIFlagFwdDecl)
!28 = !{!0, !7}
!29 = !{i32 7, !"Dwarf Version", i32 5}
!30 = !{i32 2, !"Debug Info Version", i32 3}
!31 = !{i32 1, !"wchar_size", i32 4}
!32 = !{i32 7, !"openmp", i32 50}
!33 = !{i32 7, !"PIC Level", i32 2}
!34 = !{i32 7, !"PIE Level", i32 2}
!35 = !{i32 7, !"uwtable", i32 2}
!36 = !{i32 7, !"frame-pointer", i32 2}
!37 = !{!"Debian clang version 15.0.6"}
!38 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !39, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !44)
!39 = !DISubroutineType(types: !40)
!40 = !{!41, !41, !42}
!41 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!42 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!44 = !{}
!45 = !DILocalVariable(name: "argc", arg: 1, scope: !38, file: !2, line: 29, type: !41)
!46 = !DILocation(line: 29, column: 14, scope: !38)
!47 = !DILocalVariable(name: "argv", arg: 2, scope: !38, file: !2, line: 29, type: !42)
!48 = !DILocation(line: 29, column: 27, scope: !38)
!49 = !DILocalVariable(name: "rank", scope: !38, file: !2, line: 30, type: !41)
!50 = !DILocation(line: 30, column: 7, scope: !38)
!51 = !DILocalVariable(name: "size", scope: !38, file: !2, line: 30, type: !41)
!52 = !DILocation(line: 30, column: 13, scope: !38)
!53 = !DILocalVariable(name: "win", scope: !38, file: !2, line: 31, type: !54)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !55)
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !56, size: 64)
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!57 = !DILocation(line: 31, column: 11, scope: !38)
!58 = !DILocalVariable(name: "win_base", scope: !38, file: !2, line: 32, type: !59)
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !41, size: 64)
!60 = !DILocation(line: 32, column: 8, scope: !38)
!61 = !DILocalVariable(name: "value", scope: !38, file: !2, line: 33, type: !41)
!62 = !DILocation(line: 33, column: 7, scope: !38)
!63 = !DILocalVariable(name: "value2", scope: !38, file: !2, line: 33, type: !41)
!64 = !DILocation(line: 33, column: 18, scope: !38)
!65 = !DILocalVariable(name: "buf", scope: !38, file: !2, line: 34, type: !59)
!66 = !DILocation(line: 34, column: 8, scope: !38)
!67 = !DILocalVariable(name: "result", scope: !38, file: !2, line: 35, type: !41)
!68 = !DILocation(line: 35, column: 7, scope: !38)
!69 = !DILocalVariable(name: "token", scope: !38, file: !2, line: 36, type: !41)
!70 = !DILocation(line: 36, column: 7, scope: !38)
!71 = !DILocation(line: 38, column: 3, scope: !38)
!72 = !DILocation(line: 39, column: 3, scope: !38)
!73 = !DILocation(line: 40, column: 3, scope: !38)
!74 = !DILocation(line: 42, column: 7, scope: !75)
!75 = distinct !DILexicalBlock(scope: !38, file: !2, line: 42, column: 7)
!76 = !DILocation(line: 42, column: 12, scope: !75)
!77 = !DILocation(line: 42, column: 7, scope: !38)
!78 = !DILocation(line: 43, column: 65, scope: !79)
!79 = distinct !DILexicalBlock(scope: !75, file: !2, line: 42, column: 25)
!80 = !DILocation(line: 43, column: 5, scope: !79)
!81 = !DILocation(line: 44, column: 5, scope: !79)
!82 = !DILocation(line: 45, column: 3, scope: !79)
!83 = !DILocation(line: 47, column: 3, scope: !38)
!84 = !DILocalVariable(name: "i", scope: !85, file: !2, line: 49, type: !41)
!85 = distinct !DILexicalBlock(scope: !38, file: !2, line: 49, column: 3)
!86 = !DILocation(line: 49, column: 12, scope: !85)
!87 = !DILocation(line: 49, column: 8, scope: !85)
!88 = !DILocation(line: 49, column: 19, scope: !89)
!89 = distinct !DILexicalBlock(scope: !85, file: !2, line: 49, column: 3)
!90 = !DILocation(line: 49, column: 21, scope: !89)
!91 = !DILocation(line: 49, column: 3, scope: !85)
!92 = !DILocation(line: 50, column: 5, scope: !93)
!93 = distinct !DILexicalBlock(scope: !89, file: !2, line: 49, column: 38)
!94 = !DILocation(line: 50, column: 14, scope: !93)
!95 = !DILocation(line: 50, column: 17, scope: !93)
!96 = !DILocation(line: 51, column: 3, scope: !93)
!97 = !DILocation(line: 49, column: 34, scope: !89)
!98 = !DILocation(line: 49, column: 3, scope: !89)
!99 = distinct !{!99, !91, !100, !101}
!100 = !DILocation(line: 51, column: 3, scope: !85)
!101 = !{!"llvm.loop.mustprogress"}
!102 = !DILocation(line: 53, column: 20, scope: !38)
!103 = !DILocation(line: 53, column: 3, scope: !38)
!104 = !DILocation(line: 55, column: 7, scope: !105)
!105 = distinct !DILexicalBlock(scope: !38, file: !2, line: 55, column: 7)
!106 = !DILocation(line: 55, column: 12, scope: !105)
!107 = !DILocation(line: 55, column: 7, scope: !38)
!108 = !DILocation(line: 58, column: 13, scope: !109)
!109 = distinct !DILexicalBlock(scope: !105, file: !2, line: 55, column: 18)
!110 = !DILocation(line: 58, column: 48, scope: !109)
!111 = !DILocation(line: 58, column: 5, scope: !109)
!112 = !DILocation(line: 59, column: 3, scope: !109)
!113 = !DILocation(line: 61, column: 7, scope: !114)
!114 = distinct !DILexicalBlock(scope: !38, file: !2, line: 61, column: 7)
!115 = !DILocation(line: 61, column: 12, scope: !114)
!116 = !DILocation(line: 61, column: 7, scope: !38)
!117 = !DILocation(line: 62, column: 11, scope: !118)
!118 = distinct !DILexicalBlock(scope: !114, file: !2, line: 61, column: 18)
!119 = !DILocation(line: 64, column: 67, scope: !118)
!120 = !DILocation(line: 64, column: 5, scope: !118)
!121 = !DILocation(line: 65, column: 3, scope: !118)
!122 = !DILocation(line: 67, column: 20, scope: !38)
!123 = !DILocation(line: 67, column: 3, scope: !38)
!124 = !DILocation(line: 69, column: 3, scope: !38)
!125 = !DILocation(line: 72, column: 10, scope: !38)
!126 = !DILocation(line: 72, column: 17, scope: !38)
!127 = !DILocation(line: 72, column: 16, scope: !38)
!128 = !DILocation(line: 72, column: 22, scope: !38)
!129 = !DILocation(line: 72, column: 30, scope: !38)
!130 = !DILocation(line: 70, column: 3, scope: !38)
!131 = !DILocation(line: 74, column: 3, scope: !38)
!132 = !DILocation(line: 75, column: 3, scope: !38)
!133 = !DILocation(line: 77, column: 3, scope: !38)
