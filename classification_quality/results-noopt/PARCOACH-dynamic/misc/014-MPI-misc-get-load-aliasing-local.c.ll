; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/misc/014-MPI-misc-get-load-aliasing-local.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/misc/014-MPI-misc-get-load-aliasing-local.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@.str.1 = private unnamed_addr constant [13 x i8] c"value is %d\0A\00", align 1, !dbg !7
@.str.2 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !12

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @aliasgenerator(ptr noundef %0, ptr noundef %1) #0 !dbg !40 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !47, metadata !DIExpression()), !dbg !48
  store ptr %1, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !49, metadata !DIExpression()), !dbg !50
  %5 = load ptr, ptr %3, align 8, !dbg !51
  %6 = load ptr, ptr %5, align 8, !dbg !52
  %7 = load ptr, ptr %4, align 8, !dbg !53
  store ptr %6, ptr %7, align 8, !dbg !54
  ret void, !dbg !55
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !56 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !61, metadata !DIExpression()), !dbg !62
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.declare(metadata ptr %6, metadata !65, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.declare(metadata ptr %7, metadata !67, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.declare(metadata ptr %8, metadata !69, metadata !DIExpression()), !dbg !73
  call void @llvm.dbg.declare(metadata ptr %9, metadata !74, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.declare(metadata ptr %10, metadata !76, metadata !DIExpression()), !dbg !77
  store i32 1, ptr %10, align 4, !dbg !77
  call void @llvm.dbg.declare(metadata ptr %11, metadata !78, metadata !DIExpression()), !dbg !79
  store i32 2, ptr %11, align 4, !dbg !79
  call void @llvm.dbg.declare(metadata ptr %12, metadata !80, metadata !DIExpression()), !dbg !81
  store ptr %10, ptr %12, align 8, !dbg !81
  call void @llvm.dbg.declare(metadata ptr %13, metadata !82, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.declare(metadata ptr %14, metadata !84, metadata !DIExpression()), !dbg !85
  store i32 42, ptr %14, align 4, !dbg !85
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !86
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !87
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !88
  %21 = load i32, ptr %7, align 4, !dbg !89
  %22 = icmp ne i32 %21, 2, !dbg !91
  br i1 %22, label %23, label %27, !dbg !92

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !93
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !95
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !96
  br label %27, !dbg !97

27:                                               ; preds = %23, %2
  %28 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !98
  call void @llvm.dbg.declare(metadata ptr %15, metadata !99, metadata !DIExpression()), !dbg !101
  store i32 0, ptr %15, align 4, !dbg !101
  br label %29, !dbg !102

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !103
  %31 = icmp slt i32 %30, 10, !dbg !105
  br i1 %31, label %32, label %40, !dbg !106

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !107
  %34 = load i32, ptr %15, align 4, !dbg !109
  %35 = sext i32 %34 to i64, !dbg !107
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !107
  store i32 0, ptr %36, align 4, !dbg !110
  br label %37, !dbg !111

37:                                               ; preds = %32
  %38 = load i32, ptr %15, align 4, !dbg !112
  %39 = add nsw i32 %38, 1, !dbg !112
  store i32 %39, ptr %15, align 4, !dbg !112
  br label %29, !dbg !113, !llvm.loop !114

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !117
  %42 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %41), !dbg !118
  call void @llvm.dbg.declare(metadata ptr %16, metadata !119, metadata !DIExpression()), !dbg !120
  call void @llvm.dbg.declare(metadata ptr %17, metadata !121, metadata !DIExpression()), !dbg !122
  call void @aliasgenerator(ptr noundef %12, ptr noundef %16), !dbg !123
  call void @aliasgenerator(ptr noundef %9, ptr noundef %17), !dbg !124
  %43 = load i32, ptr %6, align 4, !dbg !125
  %44 = icmp eq i32 %43, 0, !dbg !127
  br i1 %44, label %45, label %52, !dbg !128

45:                                               ; preds = %40
  %46 = load ptr, ptr %16, align 8, !dbg !129
  %47 = load ptr, ptr %8, align 8, !dbg !131
  %48 = call i32 @MPI_Get(ptr noundef %46, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %47), !dbg !132
  %49 = load ptr, ptr %16, align 8, !dbg !133
  %50 = load i32, ptr %49, align 4, !dbg !134
  %51 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %50), !dbg !135
  br label %52, !dbg !136

52:                                               ; preds = %45, %40
  %53 = load ptr, ptr %8, align 8, !dbg !137
  %54 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %53), !dbg !138
  %55 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !139
  %56 = load i32, ptr %6, align 4, !dbg !140
  %57 = load ptr, ptr %12, align 8, !dbg !141
  %58 = load i32, ptr %57, align 4, !dbg !142
  %59 = load i32, ptr %11, align 4, !dbg !143
  %60 = load ptr, ptr %9, align 8, !dbg !144
  %61 = getelementptr inbounds i32, ptr %60, i64 0, !dbg !144
  %62 = load i32, ptr %61, align 4, !dbg !144
  %63 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %56, i32 noundef %58, i32 noundef %59, i32 noundef %62), !dbg !145
  %64 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !146
  %65 = call i32 @MPI_Finalize(), !dbg !147
  ret i32 0, !dbg !148
}

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

declare i32 @MPI_Get(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!17}
!llvm.module.flags = !{!31, !32, !33, !34, !35, !36, !37, !38}
!llvm.ident = !{!39}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 45, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/misc/014-MPI-misc-get-load-aliasing-local.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "68ae12ba289d5aac3ddfc855ad9cf407")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 68, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 104, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 13)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 74, type: !14, isLocal: true, isDefinition: true)
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
!40 = distinct !DISubprogram(name: "aliasgenerator", scope: !2, file: !2, line: 26, type: !41, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!41 = !DISubroutineType(types: !42)
!42 = !{null, !43, !43}
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !44, size: 64)
!44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !45, size: 64)
!45 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!46 = !{}
!47 = !DILocalVariable(name: "x", arg: 1, scope: !40, file: !2, line: 26, type: !43)
!48 = !DILocation(line: 26, column: 53, scope: !40)
!49 = !DILocalVariable(name: "y", arg: 2, scope: !40, file: !2, line: 26, type: !43)
!50 = !DILocation(line: 26, column: 62, scope: !40)
!51 = !DILocation(line: 26, column: 73, scope: !40)
!52 = !DILocation(line: 26, column: 72, scope: !40)
!53 = !DILocation(line: 26, column: 68, scope: !40)
!54 = !DILocation(line: 26, column: 70, scope: !40)
!55 = !DILocation(line: 26, column: 76, scope: !40)
!56 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 31, type: !57, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!57 = !DISubroutineType(types: !58)
!58 = !{!45, !45, !59}
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !60, size: 64)
!60 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!61 = !DILocalVariable(name: "argc", arg: 1, scope: !56, file: !2, line: 31, type: !45)
!62 = !DILocation(line: 31, column: 14, scope: !56)
!63 = !DILocalVariable(name: "argv", arg: 2, scope: !56, file: !2, line: 31, type: !59)
!64 = !DILocation(line: 31, column: 27, scope: !56)
!65 = !DILocalVariable(name: "rank", scope: !56, file: !2, line: 32, type: !45)
!66 = !DILocation(line: 32, column: 7, scope: !56)
!67 = !DILocalVariable(name: "size", scope: !56, file: !2, line: 32, type: !45)
!68 = !DILocation(line: 32, column: 13, scope: !56)
!69 = !DILocalVariable(name: "win", scope: !56, file: !2, line: 33, type: !70)
!70 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !71)
!71 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !72, size: 64)
!72 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!73 = !DILocation(line: 33, column: 11, scope: !56)
!74 = !DILocalVariable(name: "win_base", scope: !56, file: !2, line: 34, type: !44)
!75 = !DILocation(line: 34, column: 8, scope: !56)
!76 = !DILocalVariable(name: "value", scope: !56, file: !2, line: 35, type: !45)
!77 = !DILocation(line: 35, column: 7, scope: !56)
!78 = !DILocalVariable(name: "value2", scope: !56, file: !2, line: 35, type: !45)
!79 = !DILocation(line: 35, column: 18, scope: !56)
!80 = !DILocalVariable(name: "buf", scope: !56, file: !2, line: 36, type: !44)
!81 = !DILocation(line: 36, column: 8, scope: !56)
!82 = !DILocalVariable(name: "result", scope: !56, file: !2, line: 37, type: !45)
!83 = !DILocation(line: 37, column: 7, scope: !56)
!84 = !DILocalVariable(name: "token", scope: !56, file: !2, line: 38, type: !45)
!85 = !DILocation(line: 38, column: 7, scope: !56)
!86 = !DILocation(line: 40, column: 3, scope: !56)
!87 = !DILocation(line: 41, column: 3, scope: !56)
!88 = !DILocation(line: 42, column: 3, scope: !56)
!89 = !DILocation(line: 44, column: 7, scope: !90)
!90 = distinct !DILexicalBlock(scope: !56, file: !2, line: 44, column: 7)
!91 = !DILocation(line: 44, column: 12, scope: !90)
!92 = !DILocation(line: 44, column: 7, scope: !56)
!93 = !DILocation(line: 45, column: 65, scope: !94)
!94 = distinct !DILexicalBlock(scope: !90, file: !2, line: 44, column: 25)
!95 = !DILocation(line: 45, column: 5, scope: !94)
!96 = !DILocation(line: 46, column: 5, scope: !94)
!97 = !DILocation(line: 47, column: 3, scope: !94)
!98 = !DILocation(line: 49, column: 3, scope: !56)
!99 = !DILocalVariable(name: "i", scope: !100, file: !2, line: 51, type: !45)
!100 = distinct !DILexicalBlock(scope: !56, file: !2, line: 51, column: 3)
!101 = !DILocation(line: 51, column: 12, scope: !100)
!102 = !DILocation(line: 51, column: 8, scope: !100)
!103 = !DILocation(line: 51, column: 19, scope: !104)
!104 = distinct !DILexicalBlock(scope: !100, file: !2, line: 51, column: 3)
!105 = !DILocation(line: 51, column: 21, scope: !104)
!106 = !DILocation(line: 51, column: 3, scope: !100)
!107 = !DILocation(line: 52, column: 5, scope: !108)
!108 = distinct !DILexicalBlock(scope: !104, file: !2, line: 51, column: 38)
!109 = !DILocation(line: 52, column: 14, scope: !108)
!110 = !DILocation(line: 52, column: 17, scope: !108)
!111 = !DILocation(line: 53, column: 3, scope: !108)
!112 = !DILocation(line: 51, column: 34, scope: !104)
!113 = !DILocation(line: 51, column: 3, scope: !104)
!114 = distinct !{!114, !106, !115, !116}
!115 = !DILocation(line: 53, column: 3, scope: !100)
!116 = !{!"llvm.loop.mustprogress"}
!117 = !DILocation(line: 55, column: 20, scope: !56)
!118 = !DILocation(line: 55, column: 3, scope: !56)
!119 = !DILocalVariable(name: "buf_alias", scope: !56, file: !2, line: 57, type: !44)
!120 = !DILocation(line: 57, column: 8, scope: !56)
!121 = !DILocalVariable(name: "win_base_alias", scope: !56, file: !2, line: 58, type: !44)
!122 = !DILocation(line: 58, column: 8, scope: !56)
!123 = !DILocation(line: 60, column: 3, scope: !56)
!124 = !DILocation(line: 61, column: 3, scope: !56)
!125 = !DILocation(line: 63, column: 7, scope: !126)
!126 = distinct !DILexicalBlock(scope: !56, file: !2, line: 63, column: 7)
!127 = !DILocation(line: 63, column: 12, scope: !126)
!128 = !DILocation(line: 63, column: 7, scope: !56)
!129 = !DILocation(line: 66, column: 13, scope: !130)
!130 = distinct !DILexicalBlock(scope: !126, file: !2, line: 63, column: 18)
!131 = !DILocation(line: 66, column: 54, scope: !130)
!132 = !DILocation(line: 66, column: 5, scope: !130)
!133 = !DILocation(line: 68, column: 30, scope: !130)
!134 = !DILocation(line: 68, column: 29, scope: !130)
!135 = !DILocation(line: 68, column: 5, scope: !130)
!136 = !DILocation(line: 69, column: 3, scope: !130)
!137 = !DILocation(line: 71, column: 20, scope: !56)
!138 = !DILocation(line: 71, column: 3, scope: !56)
!139 = !DILocation(line: 73, column: 3, scope: !56)
!140 = !DILocation(line: 76, column: 10, scope: !56)
!141 = !DILocation(line: 76, column: 17, scope: !56)
!142 = !DILocation(line: 76, column: 16, scope: !56)
!143 = !DILocation(line: 76, column: 22, scope: !56)
!144 = !DILocation(line: 76, column: 30, scope: !56)
!145 = !DILocation(line: 74, column: 3, scope: !56)
!146 = !DILocation(line: 78, column: 3, scope: !56)
!147 = !DILocation(line: 79, column: 3, scope: !56)
!148 = !DILocation(line: 81, column: 3, scope: !56)
