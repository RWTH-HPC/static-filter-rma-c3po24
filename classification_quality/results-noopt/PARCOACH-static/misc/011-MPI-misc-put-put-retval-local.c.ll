; ModuleID = 'results-20240305-201640/PARCOACH-static/misc/011-MPI-misc-put-put-retval-local.c'
source_filename = "results-20240305-201640/PARCOACH-static/misc/011-MPI-misc-put-put-retval-local.c"
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
define dso_local ptr @aliasgenerator(ptr noundef %0) #0 !dbg !35 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !42, metadata !DIExpression()), !dbg !43
  %3 = load ptr, ptr %2, align 8, !dbg !44
  %4 = load ptr, ptr %3, align 8, !dbg !45
  ret ptr %4, !dbg !46
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !47 {
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
  %16 = alloca ptr, align 8
  %17 = alloca ptr, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !52, metadata !DIExpression()), !dbg !53
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !54, metadata !DIExpression()), !dbg !55
  call void @llvm.dbg.declare(metadata ptr %6, metadata !56, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.declare(metadata ptr %7, metadata !58, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.declare(metadata ptr %8, metadata !60, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.declare(metadata ptr %9, metadata !65, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.declare(metadata ptr %10, metadata !67, metadata !DIExpression()), !dbg !68
  store i32 1, ptr %10, align 4, !dbg !68
  call void @llvm.dbg.declare(metadata ptr %11, metadata !69, metadata !DIExpression()), !dbg !70
  store i32 2, ptr %11, align 4, !dbg !70
  call void @llvm.dbg.declare(metadata ptr %12, metadata !71, metadata !DIExpression()), !dbg !72
  store ptr %10, ptr %12, align 8, !dbg !72
  call void @llvm.dbg.declare(metadata ptr %13, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.declare(metadata ptr %14, metadata !75, metadata !DIExpression()), !dbg !76
  store i32 42, ptr %14, align 4, !dbg !76
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !77
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !78
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !79
  %21 = load i32, ptr %7, align 4, !dbg !80
  %22 = icmp ne i32 %21, 2, !dbg !82
  br i1 %22, label %23, label %27, !dbg !83

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !84
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !86
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !87
  br label %27, !dbg !88

27:                                               ; preds = %23, %2
  %28 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !89
  call void @llvm.dbg.declare(metadata ptr %15, metadata !90, metadata !DIExpression()), !dbg !92
  store i32 0, ptr %15, align 4, !dbg !92
  br label %29, !dbg !93

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !94
  %31 = icmp slt i32 %30, 10, !dbg !96
  br i1 %31, label %32, label %40, !dbg !97

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !98
  %34 = load i32, ptr %15, align 4, !dbg !100
  %35 = sext i32 %34 to i64, !dbg !98
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !98
  store i32 0, ptr %36, align 4, !dbg !101
  br label %37, !dbg !102

37:                                               ; preds = %32
  %38 = load i32, ptr %15, align 4, !dbg !103
  %39 = add nsw i32 %38, 1, !dbg !103
  store i32 %39, ptr %15, align 4, !dbg !103
  br label %29, !dbg !104, !llvm.loop !105

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !108
  %42 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %41), !dbg !109
  call void @llvm.dbg.declare(metadata ptr %16, metadata !110, metadata !DIExpression()), !dbg !111
  call void @llvm.dbg.declare(metadata ptr %17, metadata !112, metadata !DIExpression()), !dbg !113
  %43 = call ptr @aliasgenerator(ptr noundef %12), !dbg !114
  store ptr %43, ptr %16, align 8, !dbg !115
  %44 = call ptr @aliasgenerator(ptr noundef %9), !dbg !116
  store ptr %44, ptr %17, align 8, !dbg !117
  %45 = load i32, ptr %6, align 4, !dbg !118
  %46 = icmp eq i32 %45, 0, !dbg !120
  br i1 %46, label %47, label %54, !dbg !121

47:                                               ; preds = %40
  %48 = load ptr, ptr %16, align 8, !dbg !122
  %49 = load ptr, ptr %8, align 8, !dbg !124
  %50 = call i32 @MPI_Put(ptr noundef %48, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %49), !dbg !125
  %51 = load ptr, ptr %16, align 8, !dbg !126
  %52 = load ptr, ptr %8, align 8, !dbg !127
  %53 = call i32 @MPI_Put(ptr noundef %51, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 1, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %52), !dbg !128
  br label %54, !dbg !129

54:                                               ; preds = %47, %40
  %55 = load ptr, ptr %8, align 8, !dbg !130
  %56 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %55), !dbg !131
  %57 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !132
  %58 = load i32, ptr %6, align 4, !dbg !133
  %59 = load ptr, ptr %12, align 8, !dbg !134
  %60 = load i32, ptr %59, align 4, !dbg !135
  %61 = load i32, ptr %11, align 4, !dbg !136
  %62 = load ptr, ptr %9, align 8, !dbg !137
  %63 = getelementptr inbounds i32, ptr %62, i64 0, !dbg !137
  %64 = load i32, ptr %63, align 4, !dbg !137
  %65 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %58, i32 noundef %60, i32 noundef %61, i32 noundef %64), !dbg !138
  %66 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !139
  %67 = call i32 @MPI_Finalize(), !dbg !140
  ret i32 0, !dbg !141
}

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!26, !27, !28, !29, !30, !31, !32, !33}
!llvm.ident = !{!34}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 43, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-static/misc/011-MPI-misc-put-put-retval-local.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "f7172c164ec88d1a79c1daf62af49b8e")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 69, type: !9, isLocal: true, isDefinition: true)
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
!35 = distinct !DISubprogram(name: "aliasgenerator", scope: !2, file: !2, line: 24, type: !36, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!36 = !DISubroutineType(types: !37)
!37 = !{!38, !40}
!38 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !39, size: 64)
!39 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!40 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !38, size: 64)
!41 = !{}
!42 = !DILocalVariable(name: "x", arg: 1, scope: !35, file: !2, line: 24, type: !40)
!43 = !DILocation(line: 24, column: 53, scope: !35)
!44 = !DILocation(line: 24, column: 66, scope: !35)
!45 = !DILocation(line: 24, column: 65, scope: !35)
!46 = !DILocation(line: 24, column: 58, scope: !35)
!47 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !48, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!48 = !DISubroutineType(types: !49)
!49 = !{!39, !39, !50}
!50 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !51, size: 64)
!51 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!52 = !DILocalVariable(name: "argc", arg: 1, scope: !47, file: !2, line: 29, type: !39)
!53 = !DILocation(line: 29, column: 14, scope: !47)
!54 = !DILocalVariable(name: "argv", arg: 2, scope: !47, file: !2, line: 29, type: !50)
!55 = !DILocation(line: 29, column: 27, scope: !47)
!56 = !DILocalVariable(name: "rank", scope: !47, file: !2, line: 30, type: !39)
!57 = !DILocation(line: 30, column: 7, scope: !47)
!58 = !DILocalVariable(name: "size", scope: !47, file: !2, line: 30, type: !39)
!59 = !DILocation(line: 30, column: 13, scope: !47)
!60 = !DILocalVariable(name: "win", scope: !47, file: !2, line: 31, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !62)
!62 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !63, size: 64)
!63 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!64 = !DILocation(line: 31, column: 11, scope: !47)
!65 = !DILocalVariable(name: "win_base", scope: !47, file: !2, line: 32, type: !38)
!66 = !DILocation(line: 32, column: 8, scope: !47)
!67 = !DILocalVariable(name: "value", scope: !47, file: !2, line: 33, type: !39)
!68 = !DILocation(line: 33, column: 7, scope: !47)
!69 = !DILocalVariable(name: "value2", scope: !47, file: !2, line: 33, type: !39)
!70 = !DILocation(line: 33, column: 18, scope: !47)
!71 = !DILocalVariable(name: "buf", scope: !47, file: !2, line: 34, type: !38)
!72 = !DILocation(line: 34, column: 8, scope: !47)
!73 = !DILocalVariable(name: "result", scope: !47, file: !2, line: 35, type: !39)
!74 = !DILocation(line: 35, column: 7, scope: !47)
!75 = !DILocalVariable(name: "token", scope: !47, file: !2, line: 36, type: !39)
!76 = !DILocation(line: 36, column: 7, scope: !47)
!77 = !DILocation(line: 38, column: 3, scope: !47)
!78 = !DILocation(line: 39, column: 3, scope: !47)
!79 = !DILocation(line: 40, column: 3, scope: !47)
!80 = !DILocation(line: 42, column: 7, scope: !81)
!81 = distinct !DILexicalBlock(scope: !47, file: !2, line: 42, column: 7)
!82 = !DILocation(line: 42, column: 12, scope: !81)
!83 = !DILocation(line: 42, column: 7, scope: !47)
!84 = !DILocation(line: 43, column: 65, scope: !85)
!85 = distinct !DILexicalBlock(scope: !81, file: !2, line: 42, column: 25)
!86 = !DILocation(line: 43, column: 5, scope: !85)
!87 = !DILocation(line: 44, column: 5, scope: !85)
!88 = !DILocation(line: 45, column: 3, scope: !85)
!89 = !DILocation(line: 47, column: 3, scope: !47)
!90 = !DILocalVariable(name: "i", scope: !91, file: !2, line: 49, type: !39)
!91 = distinct !DILexicalBlock(scope: !47, file: !2, line: 49, column: 3)
!92 = !DILocation(line: 49, column: 12, scope: !91)
!93 = !DILocation(line: 49, column: 8, scope: !91)
!94 = !DILocation(line: 49, column: 19, scope: !95)
!95 = distinct !DILexicalBlock(scope: !91, file: !2, line: 49, column: 3)
!96 = !DILocation(line: 49, column: 21, scope: !95)
!97 = !DILocation(line: 49, column: 3, scope: !91)
!98 = !DILocation(line: 50, column: 5, scope: !99)
!99 = distinct !DILexicalBlock(scope: !95, file: !2, line: 49, column: 38)
!100 = !DILocation(line: 50, column: 14, scope: !99)
!101 = !DILocation(line: 50, column: 17, scope: !99)
!102 = !DILocation(line: 51, column: 3, scope: !99)
!103 = !DILocation(line: 49, column: 34, scope: !95)
!104 = !DILocation(line: 49, column: 3, scope: !95)
!105 = distinct !{!105, !97, !106, !107}
!106 = !DILocation(line: 51, column: 3, scope: !91)
!107 = !{!"llvm.loop.mustprogress"}
!108 = !DILocation(line: 53, column: 20, scope: !47)
!109 = !DILocation(line: 53, column: 3, scope: !47)
!110 = !DILocalVariable(name: "buf_alias", scope: !47, file: !2, line: 55, type: !38)
!111 = !DILocation(line: 55, column: 8, scope: !47)
!112 = !DILocalVariable(name: "win_base_alias", scope: !47, file: !2, line: 56, type: !38)
!113 = !DILocation(line: 56, column: 8, scope: !47)
!114 = !DILocation(line: 58, column: 15, scope: !47)
!115 = !DILocation(line: 58, column: 13, scope: !47)
!116 = !DILocation(line: 59, column: 20, scope: !47)
!117 = !DILocation(line: 59, column: 18, scope: !47)
!118 = !DILocation(line: 61, column: 7, scope: !119)
!119 = distinct !DILexicalBlock(scope: !47, file: !2, line: 61, column: 7)
!120 = !DILocation(line: 61, column: 12, scope: !119)
!121 = !DILocation(line: 61, column: 7, scope: !47)
!122 = !DILocation(line: 62, column: 13, scope: !123)
!123 = distinct !DILexicalBlock(scope: !119, file: !2, line: 61, column: 18)
!124 = !DILocation(line: 62, column: 54, scope: !123)
!125 = !DILocation(line: 62, column: 5, scope: !123)
!126 = !DILocation(line: 63, column: 13, scope: !123)
!127 = !DILocation(line: 63, column: 54, scope: !123)
!128 = !DILocation(line: 63, column: 5, scope: !123)
!129 = !DILocation(line: 64, column: 3, scope: !123)
!130 = !DILocation(line: 66, column: 20, scope: !47)
!131 = !DILocation(line: 66, column: 3, scope: !47)
!132 = !DILocation(line: 68, column: 3, scope: !47)
!133 = !DILocation(line: 71, column: 10, scope: !47)
!134 = !DILocation(line: 71, column: 17, scope: !47)
!135 = !DILocation(line: 71, column: 16, scope: !47)
!136 = !DILocation(line: 71, column: 22, scope: !47)
!137 = !DILocation(line: 71, column: 30, scope: !47)
!138 = !DILocation(line: 69, column: 3, scope: !47)
!139 = !DILocation(line: 73, column: 3, scope: !47)
!140 = !DILocation(line: 74, column: 3, scope: !47)
!141 = !DILocation(line: 76, column: 3, scope: !47)
