; ModuleID = 'results-20240305-201640/PARCOACH-static/sync/001-MPI-sync-fence-local-yes.c'
source_filename = "results-20240305-201640/PARCOACH-static/sync/001-MPI-sync-fence-local-yes.c"
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
  %16 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !68
  %17 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !69
  %18 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !70
  %19 = load i32, ptr %7, align 4, !dbg !71
  %20 = icmp ne i32 %19, 2, !dbg !73
  br i1 %20, label %21, label %25, !dbg !74

21:                                               ; preds = %2
  %22 = load i32, ptr %7, align 4, !dbg !75
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %22, i32 noundef 2), !dbg !77
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !78
  br label %25, !dbg !79

25:                                               ; preds = %21, %2
  %26 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !80
  call void @llvm.dbg.declare(metadata ptr %15, metadata !81, metadata !DIExpression()), !dbg !83
  store i32 0, ptr %15, align 4, !dbg !83
  br label %27, !dbg !84

27:                                               ; preds = %35, %25
  %28 = load i32, ptr %15, align 4, !dbg !85
  %29 = icmp slt i32 %28, 10, !dbg !87
  br i1 %29, label %30, label %38, !dbg !88

30:                                               ; preds = %27
  %31 = load ptr, ptr %9, align 8, !dbg !89
  %32 = load i32, ptr %15, align 4, !dbg !91
  %33 = sext i32 %32 to i64, !dbg !89
  %34 = getelementptr inbounds i32, ptr %31, i64 %33, !dbg !89
  store i32 0, ptr %34, align 4, !dbg !92
  br label %35, !dbg !93

35:                                               ; preds = %30
  %36 = load i32, ptr %15, align 4, !dbg !94
  %37 = add nsw i32 %36, 1, !dbg !94
  store i32 %37, ptr %15, align 4, !dbg !94
  br label %27, !dbg !95, !llvm.loop !96

38:                                               ; preds = %27
  %39 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !99
  %40 = load ptr, ptr %8, align 8, !dbg !100
  %41 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %40), !dbg !101
  %42 = load i32, ptr %6, align 4, !dbg !102
  %43 = icmp eq i32 %42, 0, !dbg !104
  br i1 %43, label %44, label %47, !dbg !105

44:                                               ; preds = %38
  %45 = load ptr, ptr %8, align 8, !dbg !106
  %46 = call i32 @MPI_Put(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %45), !dbg !108
  store i32 42, ptr %10, align 4, !dbg !109
  br label %47, !dbg !110

47:                                               ; preds = %44, %38
  %48 = load ptr, ptr %8, align 8, !dbg !111
  %49 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %48), !dbg !112
  %50 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !113
  %51 = load i32, ptr %6, align 4, !dbg !114
  %52 = load ptr, ptr %12, align 8, !dbg !115
  %53 = load i32, ptr %52, align 4, !dbg !116
  %54 = load i32, ptr %11, align 4, !dbg !117
  %55 = load ptr, ptr %9, align 8, !dbg !118
  %56 = getelementptr inbounds i32, ptr %55, i64 0, !dbg !118
  %57 = load i32, ptr %56, align 4, !dbg !118
  %58 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %51, i32 noundef %53, i32 noundef %54, i32 noundef %57), !dbg !119
  %59 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !120
  %60 = call i32 @MPI_Finalize(), !dbg !121
  ret i32 0, !dbg !122
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

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!26, !27, !28, !29, !30, !31, !32, !33}
!llvm.ident = !{!34}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 42, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-static/sync/001-MPI-sync-fence-local-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "0b1ac9bfa6b9bbd54253adb4d02b5a14")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 65, type: !9, isLocal: true, isDefinition: true)
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
!35 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 28, type: !36, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!36 = !DISubroutineType(types: !37)
!37 = !{!38, !38, !39}
!38 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!39 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !40, size: 64)
!40 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!41 = !{}
!42 = !DILocalVariable(name: "argc", arg: 1, scope: !35, file: !2, line: 28, type: !38)
!43 = !DILocation(line: 28, column: 14, scope: !35)
!44 = !DILocalVariable(name: "argv", arg: 2, scope: !35, file: !2, line: 28, type: !39)
!45 = !DILocation(line: 28, column: 27, scope: !35)
!46 = !DILocalVariable(name: "rank", scope: !35, file: !2, line: 29, type: !38)
!47 = !DILocation(line: 29, column: 7, scope: !35)
!48 = !DILocalVariable(name: "size", scope: !35, file: !2, line: 29, type: !38)
!49 = !DILocation(line: 29, column: 13, scope: !35)
!50 = !DILocalVariable(name: "win", scope: !35, file: !2, line: 30, type: !51)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !52)
!52 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64)
!53 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!54 = !DILocation(line: 30, column: 11, scope: !35)
!55 = !DILocalVariable(name: "win_base", scope: !35, file: !2, line: 31, type: !56)
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !38, size: 64)
!57 = !DILocation(line: 31, column: 8, scope: !35)
!58 = !DILocalVariable(name: "value", scope: !35, file: !2, line: 32, type: !38)
!59 = !DILocation(line: 32, column: 7, scope: !35)
!60 = !DILocalVariable(name: "value2", scope: !35, file: !2, line: 32, type: !38)
!61 = !DILocation(line: 32, column: 18, scope: !35)
!62 = !DILocalVariable(name: "buf", scope: !35, file: !2, line: 33, type: !56)
!63 = !DILocation(line: 33, column: 8, scope: !35)
!64 = !DILocalVariable(name: "result", scope: !35, file: !2, line: 34, type: !38)
!65 = !DILocation(line: 34, column: 7, scope: !35)
!66 = !DILocalVariable(name: "token", scope: !35, file: !2, line: 35, type: !38)
!67 = !DILocation(line: 35, column: 7, scope: !35)
!68 = !DILocation(line: 37, column: 3, scope: !35)
!69 = !DILocation(line: 38, column: 3, scope: !35)
!70 = !DILocation(line: 39, column: 3, scope: !35)
!71 = !DILocation(line: 41, column: 7, scope: !72)
!72 = distinct !DILexicalBlock(scope: !35, file: !2, line: 41, column: 7)
!73 = !DILocation(line: 41, column: 12, scope: !72)
!74 = !DILocation(line: 41, column: 7, scope: !35)
!75 = !DILocation(line: 42, column: 65, scope: !76)
!76 = distinct !DILexicalBlock(scope: !72, file: !2, line: 41, column: 25)
!77 = !DILocation(line: 42, column: 5, scope: !76)
!78 = !DILocation(line: 43, column: 5, scope: !76)
!79 = !DILocation(line: 44, column: 3, scope: !76)
!80 = !DILocation(line: 46, column: 3, scope: !35)
!81 = !DILocalVariable(name: "i", scope: !82, file: !2, line: 48, type: !38)
!82 = distinct !DILexicalBlock(scope: !35, file: !2, line: 48, column: 3)
!83 = !DILocation(line: 48, column: 12, scope: !82)
!84 = !DILocation(line: 48, column: 8, scope: !82)
!85 = !DILocation(line: 48, column: 19, scope: !86)
!86 = distinct !DILexicalBlock(scope: !82, file: !2, line: 48, column: 3)
!87 = !DILocation(line: 48, column: 21, scope: !86)
!88 = !DILocation(line: 48, column: 3, scope: !82)
!89 = !DILocation(line: 49, column: 5, scope: !90)
!90 = distinct !DILexicalBlock(scope: !86, file: !2, line: 48, column: 38)
!91 = !DILocation(line: 49, column: 14, scope: !90)
!92 = !DILocation(line: 49, column: 17, scope: !90)
!93 = !DILocation(line: 50, column: 3, scope: !90)
!94 = !DILocation(line: 48, column: 34, scope: !86)
!95 = !DILocation(line: 48, column: 3, scope: !86)
!96 = distinct !{!96, !88, !97, !98}
!97 = !DILocation(line: 50, column: 3, scope: !82)
!98 = !{!"llvm.loop.mustprogress"}
!99 = !DILocation(line: 52, column: 3, scope: !35)
!100 = !DILocation(line: 54, column: 20, scope: !35)
!101 = !DILocation(line: 54, column: 3, scope: !35)
!102 = !DILocation(line: 55, column: 7, scope: !103)
!103 = distinct !DILexicalBlock(scope: !35, file: !2, line: 55, column: 7)
!104 = !DILocation(line: 55, column: 12, scope: !103)
!105 = !DILocation(line: 55, column: 7, scope: !35)
!106 = !DILocation(line: 58, column: 51, scope: !107)
!107 = distinct !DILexicalBlock(scope: !103, file: !2, line: 55, column: 18)
!108 = !DILocation(line: 58, column: 5, scope: !107)
!109 = !DILocation(line: 60, column: 11, scope: !107)
!110 = !DILocation(line: 61, column: 3, scope: !107)
!111 = !DILocation(line: 62, column: 20, scope: !35)
!112 = !DILocation(line: 62, column: 3, scope: !35)
!113 = !DILocation(line: 64, column: 3, scope: !35)
!114 = !DILocation(line: 67, column: 10, scope: !35)
!115 = !DILocation(line: 67, column: 17, scope: !35)
!116 = !DILocation(line: 67, column: 16, scope: !35)
!117 = !DILocation(line: 67, column: 22, scope: !35)
!118 = !DILocation(line: 67, column: 30, scope: !35)
!119 = !DILocation(line: 65, column: 3, scope: !35)
!120 = !DILocation(line: 69, column: 3, scope: !35)
!121 = !DILocation(line: 70, column: 3, scope: !35)
!122 = !DILocation(line: 72, column: 3, scope: !35)
