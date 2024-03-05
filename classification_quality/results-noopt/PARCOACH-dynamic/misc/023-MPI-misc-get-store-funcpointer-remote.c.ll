; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_datatype_t = type opaque
%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque

@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@.str.1 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !7

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @rank0(ptr noundef %0, ptr noundef %1, ptr noundef %2) #0 !dbg !35 {
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !44, metadata !DIExpression()), !dbg !45
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !46, metadata !DIExpression()), !dbg !47
  store ptr %2, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !48, metadata !DIExpression()), !dbg !49
  %7 = load ptr, ptr %4, align 8, !dbg !50
  %8 = load ptr, ptr %5, align 8, !dbg !51
  %9 = call i32 @MPI_Get(ptr noundef %7, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %8), !dbg !52
  ret void, !dbg !53
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @MPI_Get(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @rank1(ptr noundef %0, ptr noundef %1, ptr noundef %2) #0 !dbg !54 {
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !55, metadata !DIExpression()), !dbg !56
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !57, metadata !DIExpression()), !dbg !58
  store ptr %2, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !59, metadata !DIExpression()), !dbg !60
  %7 = load ptr, ptr %6, align 8, !dbg !61
  %8 = getelementptr inbounds i32, ptr %7, i64 0, !dbg !61
  store i32 42, ptr %8, align 4, !dbg !62
  ret void, !dbg !63
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !64 {
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
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !69, metadata !DIExpression()), !dbg !70
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !71, metadata !DIExpression()), !dbg !72
  call void @llvm.dbg.declare(metadata ptr %6, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.declare(metadata ptr %7, metadata !75, metadata !DIExpression()), !dbg !76
  call void @llvm.dbg.declare(metadata ptr %8, metadata !77, metadata !DIExpression()), !dbg !78
  call void @llvm.dbg.declare(metadata ptr %9, metadata !79, metadata !DIExpression()), !dbg !80
  call void @llvm.dbg.declare(metadata ptr %10, metadata !81, metadata !DIExpression()), !dbg !82
  store i32 1, ptr %10, align 4, !dbg !82
  call void @llvm.dbg.declare(metadata ptr %11, metadata !83, metadata !DIExpression()), !dbg !84
  store i32 2, ptr %11, align 4, !dbg !84
  call void @llvm.dbg.declare(metadata ptr %12, metadata !85, metadata !DIExpression()), !dbg !86
  store ptr %10, ptr %12, align 8, !dbg !86
  call void @llvm.dbg.declare(metadata ptr %13, metadata !87, metadata !DIExpression()), !dbg !88
  call void @llvm.dbg.declare(metadata ptr %14, metadata !89, metadata !DIExpression()), !dbg !90
  store i32 42, ptr %14, align 4, !dbg !90
  %17 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !91
  %18 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !92
  %19 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !93
  %20 = load i32, ptr %7, align 4, !dbg !94
  %21 = icmp ne i32 %20, 2, !dbg !96
  br i1 %21, label %22, label %26, !dbg !97

22:                                               ; preds = %2
  %23 = load i32, ptr %7, align 4, !dbg !98
  %24 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %23, i32 noundef 2), !dbg !100
  %25 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !101
  br label %26, !dbg !102

26:                                               ; preds = %22, %2
  %27 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !103
  call void @llvm.dbg.declare(metadata ptr %15, metadata !104, metadata !DIExpression()), !dbg !106
  store i32 0, ptr %15, align 4, !dbg !106
  br label %28, !dbg !107

28:                                               ; preds = %36, %26
  %29 = load i32, ptr %15, align 4, !dbg !108
  %30 = icmp slt i32 %29, 10, !dbg !110
  br i1 %30, label %31, label %39, !dbg !111

31:                                               ; preds = %28
  %32 = load ptr, ptr %9, align 8, !dbg !112
  %33 = load i32, ptr %15, align 4, !dbg !114
  %34 = sext i32 %33 to i64, !dbg !112
  %35 = getelementptr inbounds i32, ptr %32, i64 %34, !dbg !112
  store i32 0, ptr %35, align 4, !dbg !115
  br label %36, !dbg !116

36:                                               ; preds = %31
  %37 = load i32, ptr %15, align 4, !dbg !117
  %38 = add nsw i32 %37, 1, !dbg !117
  store i32 %38, ptr %15, align 4, !dbg !117
  br label %28, !dbg !118, !llvm.loop !119

39:                                               ; preds = %28
  call void @llvm.dbg.declare(metadata ptr %16, metadata !122, metadata !DIExpression()), !dbg !124
  %40 = load ptr, ptr %8, align 8, !dbg !125
  %41 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %40), !dbg !126
  %42 = load i32, ptr %6, align 4, !dbg !127
  %43 = icmp eq i32 %42, 0, !dbg !129
  br i1 %43, label %44, label %45, !dbg !130

44:                                               ; preds = %39
  store ptr @rank0, ptr %16, align 8, !dbg !131
  br label %46, !dbg !133

45:                                               ; preds = %39
  store ptr @rank1, ptr %16, align 8, !dbg !134
  br label %46

46:                                               ; preds = %45, %44
  %47 = load ptr, ptr %16, align 8, !dbg !136
  %48 = load ptr, ptr %12, align 8, !dbg !137
  %49 = load ptr, ptr %8, align 8, !dbg !138
  %50 = load ptr, ptr %9, align 8, !dbg !139
  call void %47(ptr noundef %48, ptr noundef %49, ptr noundef %50), !dbg !140
  %51 = load ptr, ptr %8, align 8, !dbg !141
  %52 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %51), !dbg !142
  %53 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !143
  %54 = load i32, ptr %6, align 4, !dbg !144
  %55 = load ptr, ptr %12, align 8, !dbg !145
  %56 = load i32, ptr %55, align 4, !dbg !146
  %57 = load i32, ptr %11, align 4, !dbg !147
  %58 = load ptr, ptr %9, align 8, !dbg !148
  %59 = getelementptr inbounds i32, ptr %58, i64 0, !dbg !148
  %60 = load i32, ptr %59, align 4, !dbg !148
  %61 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %54, i32 noundef %56, i32 noundef %57, i32 noundef %60), !dbg !149
  %62 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !150
  %63 = call i32 @MPI_Finalize(), !dbg !151
  ret i32 0, !dbg !152
}

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

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
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 54, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/misc/023-MPI-misc-get-store-funcpointer-remote.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "92f3d16fcad96f44815c9bddfb15d2ff")
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
!14 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Datatype", file: !15, line: 420, baseType: !16)
!15 = !DIFile(filename: "/usr/lib/x86_64-linux-gnu/openmpi/include/mpi.h", directory: "", checksumkind: CSK_MD5, checksum: "c3ca5be6f2c6a6e16d01846df47c0b49")
!16 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!17 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_datatype_t", file: !15, line: 420, flags: DIFlagFwdDecl)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Comm", file: !15, line: 419, baseType: !20)
!20 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !21, size: 64)
!21 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_communicator_t", file: !15, line: 419, flags: DIFlagFwdDecl)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Info", file: !15, line: 424, baseType: !23)
!23 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !24, size: 64)
!24 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_info_t", file: !15, line: 424, flags: DIFlagFwdDecl)
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
!35 = distinct !DISubprogram(name: "rank0", scope: !2, file: !2, line: 26, type: !36, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !43)
!36 = !DISubroutineType(types: !37)
!37 = !{null, !38, !40, !38}
!38 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !39, size: 64)
!39 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !41)
!41 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !42, size: 64)
!42 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!43 = !{}
!44 = !DILocalVariable(name: "buf", arg: 1, scope: !35, file: !2, line: 26, type: !38)
!45 = !DILocation(line: 26, column: 17, scope: !35)
!46 = !DILocalVariable(name: "win", arg: 2, scope: !35, file: !2, line: 26, type: !40)
!47 = !DILocation(line: 26, column: 30, scope: !35)
!48 = !DILocalVariable(name: "win_base", arg: 3, scope: !35, file: !2, line: 26, type: !38)
!49 = !DILocation(line: 26, column: 40, scope: !35)
!50 = !DILocation(line: 29, column: 11, scope: !35)
!51 = !DILocation(line: 29, column: 46, scope: !35)
!52 = !DILocation(line: 29, column: 3, scope: !35)
!53 = !DILocation(line: 30, column: 1, scope: !35)
!54 = distinct !DISubprogram(name: "rank1", scope: !2, file: !2, line: 32, type: !36, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !43)
!55 = !DILocalVariable(name: "buf", arg: 1, scope: !54, file: !2, line: 32, type: !38)
!56 = !DILocation(line: 32, column: 17, scope: !54)
!57 = !DILocalVariable(name: "win", arg: 2, scope: !54, file: !2, line: 32, type: !40)
!58 = !DILocation(line: 32, column: 30, scope: !54)
!59 = !DILocalVariable(name: "win_base", arg: 3, scope: !54, file: !2, line: 32, type: !38)
!60 = !DILocation(line: 32, column: 40, scope: !54)
!61 = !DILocation(line: 34, column: 3, scope: !54)
!62 = !DILocation(line: 34, column: 15, scope: !54)
!63 = !DILocation(line: 35, column: 1, scope: !54)
!64 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 40, type: !65, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !43)
!65 = !DISubroutineType(types: !66)
!66 = !{!39, !39, !67}
!67 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !68, size: 64)
!68 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!69 = !DILocalVariable(name: "argc", arg: 1, scope: !64, file: !2, line: 40, type: !39)
!70 = !DILocation(line: 40, column: 14, scope: !64)
!71 = !DILocalVariable(name: "argv", arg: 2, scope: !64, file: !2, line: 40, type: !67)
!72 = !DILocation(line: 40, column: 27, scope: !64)
!73 = !DILocalVariable(name: "rank", scope: !64, file: !2, line: 41, type: !39)
!74 = !DILocation(line: 41, column: 7, scope: !64)
!75 = !DILocalVariable(name: "size", scope: !64, file: !2, line: 41, type: !39)
!76 = !DILocation(line: 41, column: 13, scope: !64)
!77 = !DILocalVariable(name: "win", scope: !64, file: !2, line: 42, type: !40)
!78 = !DILocation(line: 42, column: 11, scope: !64)
!79 = !DILocalVariable(name: "win_base", scope: !64, file: !2, line: 43, type: !38)
!80 = !DILocation(line: 43, column: 8, scope: !64)
!81 = !DILocalVariable(name: "value", scope: !64, file: !2, line: 44, type: !39)
!82 = !DILocation(line: 44, column: 7, scope: !64)
!83 = !DILocalVariable(name: "value2", scope: !64, file: !2, line: 44, type: !39)
!84 = !DILocation(line: 44, column: 18, scope: !64)
!85 = !DILocalVariable(name: "buf", scope: !64, file: !2, line: 45, type: !38)
!86 = !DILocation(line: 45, column: 8, scope: !64)
!87 = !DILocalVariable(name: "result", scope: !64, file: !2, line: 46, type: !39)
!88 = !DILocation(line: 46, column: 7, scope: !64)
!89 = !DILocalVariable(name: "token", scope: !64, file: !2, line: 47, type: !39)
!90 = !DILocation(line: 47, column: 7, scope: !64)
!91 = !DILocation(line: 49, column: 3, scope: !64)
!92 = !DILocation(line: 50, column: 3, scope: !64)
!93 = !DILocation(line: 51, column: 3, scope: !64)
!94 = !DILocation(line: 53, column: 7, scope: !95)
!95 = distinct !DILexicalBlock(scope: !64, file: !2, line: 53, column: 7)
!96 = !DILocation(line: 53, column: 12, scope: !95)
!97 = !DILocation(line: 53, column: 7, scope: !64)
!98 = !DILocation(line: 54, column: 65, scope: !99)
!99 = distinct !DILexicalBlock(scope: !95, file: !2, line: 53, column: 25)
!100 = !DILocation(line: 54, column: 5, scope: !99)
!101 = !DILocation(line: 55, column: 5, scope: !99)
!102 = !DILocation(line: 56, column: 3, scope: !99)
!103 = !DILocation(line: 58, column: 3, scope: !64)
!104 = !DILocalVariable(name: "i", scope: !105, file: !2, line: 60, type: !39)
!105 = distinct !DILexicalBlock(scope: !64, file: !2, line: 60, column: 3)
!106 = !DILocation(line: 60, column: 12, scope: !105)
!107 = !DILocation(line: 60, column: 8, scope: !105)
!108 = !DILocation(line: 60, column: 19, scope: !109)
!109 = distinct !DILexicalBlock(scope: !105, file: !2, line: 60, column: 3)
!110 = !DILocation(line: 60, column: 21, scope: !109)
!111 = !DILocation(line: 60, column: 3, scope: !105)
!112 = !DILocation(line: 61, column: 5, scope: !113)
!113 = distinct !DILexicalBlock(scope: !109, file: !2, line: 60, column: 38)
!114 = !DILocation(line: 61, column: 14, scope: !113)
!115 = !DILocation(line: 61, column: 17, scope: !113)
!116 = !DILocation(line: 62, column: 3, scope: !113)
!117 = !DILocation(line: 60, column: 34, scope: !109)
!118 = !DILocation(line: 60, column: 3, scope: !109)
!119 = distinct !{!119, !111, !120, !121}
!120 = !DILocation(line: 62, column: 3, scope: !105)
!121 = !{!"llvm.loop.mustprogress"}
!122 = !DILocalVariable(name: "rankfunc", scope: !64, file: !2, line: 64, type: !123)
!123 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !36, size: 64)
!124 = !DILocation(line: 64, column: 10, scope: !64)
!125 = !DILocation(line: 66, column: 20, scope: !64)
!126 = !DILocation(line: 66, column: 3, scope: !64)
!127 = !DILocation(line: 68, column: 7, scope: !128)
!128 = distinct !DILexicalBlock(scope: !64, file: !2, line: 68, column: 7)
!129 = !DILocation(line: 68, column: 12, scope: !128)
!130 = !DILocation(line: 68, column: 7, scope: !64)
!131 = !DILocation(line: 69, column: 14, scope: !132)
!132 = distinct !DILexicalBlock(scope: !128, file: !2, line: 68, column: 18)
!133 = !DILocation(line: 70, column: 3, scope: !132)
!134 = !DILocation(line: 71, column: 14, scope: !135)
!135 = distinct !DILexicalBlock(scope: !128, file: !2, line: 70, column: 10)
!136 = !DILocation(line: 73, column: 5, scope: !64)
!137 = !DILocation(line: 73, column: 15, scope: !64)
!138 = !DILocation(line: 73, column: 20, scope: !64)
!139 = !DILocation(line: 73, column: 25, scope: !64)
!140 = !DILocation(line: 73, column: 3, scope: !64)
!141 = !DILocation(line: 75, column: 20, scope: !64)
!142 = !DILocation(line: 75, column: 3, scope: !64)
!143 = !DILocation(line: 77, column: 3, scope: !64)
!144 = !DILocation(line: 80, column: 10, scope: !64)
!145 = !DILocation(line: 80, column: 17, scope: !64)
!146 = !DILocation(line: 80, column: 16, scope: !64)
!147 = !DILocation(line: 80, column: 22, scope: !64)
!148 = !DILocation(line: 80, column: 30, scope: !64)
!149 = !DILocation(line: 78, column: 3, scope: !64)
!150 = !DILocation(line: 82, column: 3, scope: !64)
!151 = !DILocation(line: 83, column: 3, scope: !64)
!152 = !DILocation(line: 85, column: 3, scope: !64)
