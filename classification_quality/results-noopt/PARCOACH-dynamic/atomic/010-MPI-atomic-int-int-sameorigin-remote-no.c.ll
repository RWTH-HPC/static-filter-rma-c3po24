; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque
%struct.ompi_predefined_op_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@__const.main.value1 = private unnamed_addr constant [4 x i32] [i32 1, i32 1, i32 1, i32 1], align 16
@__const.main.value2 = private unnamed_addr constant [4 x i32] [i32 1, i32 2, i32 3, i32 4], align 16
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
  %16 = alloca [4 x i32], align 16
  %17 = alloca [4 x i32], align 16
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
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !71
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !72
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !73
  %21 = load i32, ptr %7, align 4, !dbg !74
  %22 = icmp ne i32 %21, 2, !dbg !76
  br i1 %22, label %23, label %27, !dbg !77

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !78
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !80
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !81
  br label %27, !dbg !82

27:                                               ; preds = %23, %2
  %28 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !83
  call void @llvm.dbg.declare(metadata ptr %15, metadata !84, metadata !DIExpression()), !dbg !86
  store i32 0, ptr %15, align 4, !dbg !86
  br label %29, !dbg !87

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !88
  %31 = icmp slt i32 %30, 10, !dbg !90
  br i1 %31, label %32, label %40, !dbg !91

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !92
  %34 = load i32, ptr %15, align 4, !dbg !94
  %35 = sext i32 %34 to i64, !dbg !92
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !92
  store i32 0, ptr %36, align 4, !dbg !95
  br label %37, !dbg !96

37:                                               ; preds = %32
  %38 = load i32, ptr %15, align 4, !dbg !97
  %39 = add nsw i32 %38, 1, !dbg !97
  store i32 %39, ptr %15, align 4, !dbg !97
  br label %29, !dbg !98, !llvm.loop !99

40:                                               ; preds = %29
  %41 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !102
  %42 = load ptr, ptr %8, align 8, !dbg !103
  %43 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %42), !dbg !104
  %44 = load i32, ptr %6, align 4, !dbg !105
  %45 = icmp eq i32 %44, 0, !dbg !107
  br i1 %45, label %46, label %53, !dbg !108

46:                                               ; preds = %40
  call void @llvm.dbg.declare(metadata ptr %16, metadata !109, metadata !DIExpression()), !dbg !114
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %16, ptr align 16 @__const.main.value1, i64 16, i1 false), !dbg !114
  call void @llvm.dbg.declare(metadata ptr %17, metadata !115, metadata !DIExpression()), !dbg !116
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %17, ptr align 16 @__const.main.value2, i64 16, i1 false), !dbg !116
  %47 = getelementptr inbounds [4 x i32], ptr %16, i64 0, i64 0, !dbg !117
  %48 = load ptr, ptr %8, align 8, !dbg !118
  %49 = call i32 @MPI_Accumulate(ptr noundef %47, i32 noundef 4, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 4, ptr noundef @ompi_mpi_int, ptr noundef @ompi_mpi_op_sum, ptr noundef %48), !dbg !119
  %50 = getelementptr inbounds [4 x i32], ptr %17, i64 0, i64 0, !dbg !120
  %51 = load ptr, ptr %8, align 8, !dbg !121
  %52 = call i32 @MPI_Accumulate(ptr noundef %50, i32 noundef 4, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 4, ptr noundef @ompi_mpi_int, ptr noundef @ompi_mpi_op_sum, ptr noundef %51), !dbg !122
  br label %53, !dbg !123

53:                                               ; preds = %46, %40
  %54 = load ptr, ptr %8, align 8, !dbg !124
  %55 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %54), !dbg !125
  %56 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !126
  %57 = load i32, ptr %6, align 4, !dbg !127
  %58 = load ptr, ptr %12, align 8, !dbg !128
  %59 = load i32, ptr %58, align 4, !dbg !129
  %60 = load i32, ptr %11, align 4, !dbg !130
  %61 = load ptr, ptr %9, align 8, !dbg !131
  %62 = getelementptr inbounds i32, ptr %61, i64 0, !dbg !131
  %63 = load i32, ptr %62, align 4, !dbg !131
  %64 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %57, i32 noundef %59, i32 noundef %60, i32 noundef %63), !dbg !132
  %65 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !133
  %66 = call i32 @MPI_Finalize(), !dbg !134
  ret i32 0, !dbg !135
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

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

; Function Attrs: argmemonly nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #3

declare i32 @MPI_Accumulate(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { argmemonly nocallback nofree nounwind willreturn }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!29, !30, !31, !32, !33, !34, !35, !36}
!llvm.ident = !{!37}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 41, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/atomic/010-MPI-atomic-int-int-sameorigin-remote-no.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "711f8687ec4f7101e8f9f56977213a53")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 63, type: !9, isLocal: true, isDefinition: true)
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
!38 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 27, type: !39, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !44)
!39 = !DISubroutineType(types: !40)
!40 = !{!41, !41, !42}
!41 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!42 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!44 = !{}
!45 = !DILocalVariable(name: "argc", arg: 1, scope: !38, file: !2, line: 27, type: !41)
!46 = !DILocation(line: 27, column: 14, scope: !38)
!47 = !DILocalVariable(name: "argv", arg: 2, scope: !38, file: !2, line: 27, type: !42)
!48 = !DILocation(line: 27, column: 27, scope: !38)
!49 = !DILocalVariable(name: "rank", scope: !38, file: !2, line: 28, type: !41)
!50 = !DILocation(line: 28, column: 7, scope: !38)
!51 = !DILocalVariable(name: "size", scope: !38, file: !2, line: 28, type: !41)
!52 = !DILocation(line: 28, column: 13, scope: !38)
!53 = !DILocalVariable(name: "win", scope: !38, file: !2, line: 29, type: !54)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !55)
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !56, size: 64)
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!57 = !DILocation(line: 29, column: 11, scope: !38)
!58 = !DILocalVariable(name: "win_base", scope: !38, file: !2, line: 30, type: !59)
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !41, size: 64)
!60 = !DILocation(line: 30, column: 8, scope: !38)
!61 = !DILocalVariable(name: "value", scope: !38, file: !2, line: 31, type: !41)
!62 = !DILocation(line: 31, column: 7, scope: !38)
!63 = !DILocalVariable(name: "value2", scope: !38, file: !2, line: 31, type: !41)
!64 = !DILocation(line: 31, column: 18, scope: !38)
!65 = !DILocalVariable(name: "buf", scope: !38, file: !2, line: 32, type: !59)
!66 = !DILocation(line: 32, column: 8, scope: !38)
!67 = !DILocalVariable(name: "result", scope: !38, file: !2, line: 33, type: !41)
!68 = !DILocation(line: 33, column: 7, scope: !38)
!69 = !DILocalVariable(name: "token", scope: !38, file: !2, line: 34, type: !41)
!70 = !DILocation(line: 34, column: 7, scope: !38)
!71 = !DILocation(line: 36, column: 3, scope: !38)
!72 = !DILocation(line: 37, column: 3, scope: !38)
!73 = !DILocation(line: 38, column: 3, scope: !38)
!74 = !DILocation(line: 40, column: 7, scope: !75)
!75 = distinct !DILexicalBlock(scope: !38, file: !2, line: 40, column: 7)
!76 = !DILocation(line: 40, column: 12, scope: !75)
!77 = !DILocation(line: 40, column: 7, scope: !38)
!78 = !DILocation(line: 41, column: 65, scope: !79)
!79 = distinct !DILexicalBlock(scope: !75, file: !2, line: 40, column: 25)
!80 = !DILocation(line: 41, column: 5, scope: !79)
!81 = !DILocation(line: 42, column: 5, scope: !79)
!82 = !DILocation(line: 43, column: 3, scope: !79)
!83 = !DILocation(line: 45, column: 3, scope: !38)
!84 = !DILocalVariable(name: "i", scope: !85, file: !2, line: 47, type: !41)
!85 = distinct !DILexicalBlock(scope: !38, file: !2, line: 47, column: 3)
!86 = !DILocation(line: 47, column: 12, scope: !85)
!87 = !DILocation(line: 47, column: 8, scope: !85)
!88 = !DILocation(line: 47, column: 19, scope: !89)
!89 = distinct !DILexicalBlock(scope: !85, file: !2, line: 47, column: 3)
!90 = !DILocation(line: 47, column: 21, scope: !89)
!91 = !DILocation(line: 47, column: 3, scope: !85)
!92 = !DILocation(line: 48, column: 5, scope: !93)
!93 = distinct !DILexicalBlock(scope: !89, file: !2, line: 47, column: 38)
!94 = !DILocation(line: 48, column: 14, scope: !93)
!95 = !DILocation(line: 48, column: 17, scope: !93)
!96 = !DILocation(line: 49, column: 3, scope: !93)
!97 = !DILocation(line: 47, column: 34, scope: !89)
!98 = !DILocation(line: 47, column: 3, scope: !89)
!99 = distinct !{!99, !91, !100, !101}
!100 = !DILocation(line: 49, column: 3, scope: !85)
!101 = !{!"llvm.loop.mustprogress"}
!102 = !DILocation(line: 51, column: 3, scope: !38)
!103 = !DILocation(line: 53, column: 20, scope: !38)
!104 = !DILocation(line: 53, column: 3, scope: !38)
!105 = !DILocation(line: 54, column: 7, scope: !106)
!106 = distinct !DILexicalBlock(scope: !38, file: !2, line: 54, column: 7)
!107 = !DILocation(line: 54, column: 12, scope: !106)
!108 = !DILocation(line: 54, column: 7, scope: !38)
!109 = !DILocalVariable(name: "value1", scope: !110, file: !2, line: 55, type: !111)
!110 = distinct !DILexicalBlock(scope: !106, file: !2, line: 54, column: 18)
!111 = !DICompositeType(tag: DW_TAG_array_type, baseType: !41, size: 128, elements: !112)
!112 = !{!113}
!113 = !DISubrange(count: 4)
!114 = !DILocation(line: 55, column: 9, scope: !110)
!115 = !DILocalVariable(name: "value2", scope: !110, file: !2, line: 56, type: !111)
!116 = !DILocation(line: 56, column: 9, scope: !110)
!117 = !DILocation(line: 57, column: 20, scope: !110)
!118 = !DILocation(line: 57, column: 67, scope: !110)
!119 = !DILocation(line: 57, column: 5, scope: !110)
!120 = !DILocation(line: 58, column: 20, scope: !110)
!121 = !DILocation(line: 58, column: 67, scope: !110)
!122 = !DILocation(line: 58, column: 5, scope: !110)
!123 = !DILocation(line: 59, column: 3, scope: !110)
!124 = !DILocation(line: 60, column: 20, scope: !38)
!125 = !DILocation(line: 60, column: 3, scope: !38)
!126 = !DILocation(line: 62, column: 3, scope: !38)
!127 = !DILocation(line: 65, column: 10, scope: !38)
!128 = !DILocation(line: 65, column: 17, scope: !38)
!129 = !DILocation(line: 65, column: 16, scope: !38)
!130 = !DILocation(line: 65, column: 22, scope: !38)
!131 = !DILocation(line: 65, column: 30, scope: !38)
!132 = !DILocation(line: 63, column: 3, scope: !38)
!133 = !DILocation(line: 67, column: 3, scope: !38)
!134 = !DILocation(line: 68, column: 3, scope: !38)
!135 = !DILocation(line: 70, column: 3, scope: !38)
