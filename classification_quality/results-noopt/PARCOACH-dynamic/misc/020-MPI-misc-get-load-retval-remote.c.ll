; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/misc/020-MPI-misc-get-load-retval-remote.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/misc/020-MPI-misc-get-load-retval-remote.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque

@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !0
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@.str.1 = private unnamed_addr constant [25 x i8] c"win_base_alias[0] is %d\0A\00", align 1, !dbg !7
@.str.2 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !12

; Function Attrs: noinline nounwind optnone uwtable
define dso_local ptr @aliasgenerator(ptr noundef %0) #0 !dbg !40 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !47, metadata !DIExpression()), !dbg !48
  %3 = load ptr, ptr %2, align 8, !dbg !49
  %4 = load ptr, ptr %3, align 8, !dbg !50
  ret ptr %4, !dbg !51
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !52 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !57, metadata !DIExpression()), !dbg !58
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !59, metadata !DIExpression()), !dbg !60
  call void @llvm.dbg.declare(metadata ptr %6, metadata !61, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.declare(metadata ptr %7, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.declare(metadata ptr %8, metadata !65, metadata !DIExpression()), !dbg !69
  call void @llvm.dbg.declare(metadata ptr %9, metadata !70, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.declare(metadata ptr %10, metadata !72, metadata !DIExpression()), !dbg !73
  store i32 1, ptr %10, align 4, !dbg !73
  call void @llvm.dbg.declare(metadata ptr %11, metadata !74, metadata !DIExpression()), !dbg !75
  store i32 2, ptr %11, align 4, !dbg !75
  call void @llvm.dbg.declare(metadata ptr %12, metadata !76, metadata !DIExpression()), !dbg !77
  store ptr %10, ptr %12, align 8, !dbg !77
  call void @llvm.dbg.declare(metadata ptr %13, metadata !78, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.declare(metadata ptr %14, metadata !80, metadata !DIExpression()), !dbg !81
  store i32 42, ptr %14, align 4, !dbg !81
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !82
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !83
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !84
  %21 = load i32, ptr %7, align 4, !dbg !85
  %22 = icmp ne i32 %21, 2, !dbg !87
  br i1 %22, label %23, label %27, !dbg !88

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !89
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !91
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !92
  br label %27, !dbg !93

27:                                               ; preds = %23, %2
  %28 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !94
  call void @llvm.dbg.declare(metadata ptr %15, metadata !95, metadata !DIExpression()), !dbg !97
  store i32 0, ptr %15, align 4, !dbg !97
  br label %29, !dbg !98

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !99
  %31 = icmp slt i32 %30, 10, !dbg !101
  br i1 %31, label %32, label %40, !dbg !102

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !103
  %34 = load i32, ptr %15, align 4, !dbg !105
  %35 = sext i32 %34 to i64, !dbg !103
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !103
  store i32 0, ptr %36, align 4, !dbg !106
  br label %37, !dbg !107

37:                                               ; preds = %32
  %38 = load i32, ptr %15, align 4, !dbg !108
  %39 = add nsw i32 %38, 1, !dbg !108
  store i32 %39, ptr %15, align 4, !dbg !108
  br label %29, !dbg !109, !llvm.loop !110

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !113
  %42 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %41), !dbg !114
  call void @llvm.dbg.declare(metadata ptr %16, metadata !115, metadata !DIExpression()), !dbg !116
  call void @llvm.dbg.declare(metadata ptr %17, metadata !117, metadata !DIExpression()), !dbg !118
  %43 = call ptr @aliasgenerator(ptr noundef %12), !dbg !119
  store ptr %43, ptr %16, align 8, !dbg !120
  %44 = call ptr @aliasgenerator(ptr noundef %9), !dbg !121
  store ptr %44, ptr %17, align 8, !dbg !122
  %45 = load i32, ptr %6, align 4, !dbg !123
  %46 = icmp eq i32 %45, 0, !dbg !125
  br i1 %46, label %47, label %51, !dbg !126

47:                                               ; preds = %40
  %48 = load ptr, ptr %16, align 8, !dbg !127
  %49 = load ptr, ptr %8, align 8, !dbg !129
  %50 = call i32 @MPI_Get(ptr noundef %48, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %49), !dbg !130
  br label %56, !dbg !131

51:                                               ; preds = %40
  %52 = load ptr, ptr %17, align 8, !dbg !132
  %53 = getelementptr inbounds i32, ptr %52, i64 0, !dbg !132
  %54 = load i32, ptr %53, align 4, !dbg !132
  %55 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %54), !dbg !134
  br label %56

56:                                               ; preds = %51, %47
  %57 = load ptr, ptr %8, align 8, !dbg !135
  %58 = call i32 @MPI_Win_fence(i32 noundef 0, ptr noundef %57), !dbg !136
  %59 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !137
  %60 = load i32, ptr %6, align 4, !dbg !138
  %61 = load ptr, ptr %12, align 8, !dbg !139
  %62 = load i32, ptr %61, align 4, !dbg !140
  %63 = load i32, ptr %11, align 4, !dbg !141
  %64 = load ptr, ptr %9, align 8, !dbg !142
  %65 = getelementptr inbounds i32, ptr %64, i64 0, !dbg !142
  %66 = load i32, ptr %65, align 4, !dbg !142
  %67 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %60, i32 noundef %62, i32 noundef %63, i32 noundef %66), !dbg !143
  %68 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !144
  %69 = call i32 @MPI_Finalize(), !dbg !145
  ret i32 0, !dbg !146
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
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 43, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/misc/020-MPI-misc-get-load-retval-remote.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "f9bba6310e3f49e2db5676beb5e17e37")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 64, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 200, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 25)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 70, type: !14, isLocal: true, isDefinition: true)
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
!40 = distinct !DISubprogram(name: "aliasgenerator", scope: !2, file: !2, line: 24, type: !41, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!41 = !DISubroutineType(types: !42)
!42 = !{!43, !45}
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !44, size: 64)
!44 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!45 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!46 = !{}
!47 = !DILocalVariable(name: "x", arg: 1, scope: !40, file: !2, line: 24, type: !45)
!48 = !DILocation(line: 24, column: 53, scope: !40)
!49 = !DILocation(line: 24, column: 66, scope: !40)
!50 = !DILocation(line: 24, column: 65, scope: !40)
!51 = !DILocation(line: 24, column: 58, scope: !40)
!52 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !53, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!53 = !DISubroutineType(types: !54)
!54 = !{!44, !44, !55}
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !56, size: 64)
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!57 = !DILocalVariable(name: "argc", arg: 1, scope: !52, file: !2, line: 29, type: !44)
!58 = !DILocation(line: 29, column: 14, scope: !52)
!59 = !DILocalVariable(name: "argv", arg: 2, scope: !52, file: !2, line: 29, type: !55)
!60 = !DILocation(line: 29, column: 27, scope: !52)
!61 = !DILocalVariable(name: "rank", scope: !52, file: !2, line: 30, type: !44)
!62 = !DILocation(line: 30, column: 7, scope: !52)
!63 = !DILocalVariable(name: "size", scope: !52, file: !2, line: 30, type: !44)
!64 = !DILocation(line: 30, column: 13, scope: !52)
!65 = !DILocalVariable(name: "win", scope: !52, file: !2, line: 31, type: !66)
!66 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !67)
!67 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !68, size: 64)
!68 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!69 = !DILocation(line: 31, column: 11, scope: !52)
!70 = !DILocalVariable(name: "win_base", scope: !52, file: !2, line: 32, type: !43)
!71 = !DILocation(line: 32, column: 8, scope: !52)
!72 = !DILocalVariable(name: "value", scope: !52, file: !2, line: 33, type: !44)
!73 = !DILocation(line: 33, column: 7, scope: !52)
!74 = !DILocalVariable(name: "value2", scope: !52, file: !2, line: 33, type: !44)
!75 = !DILocation(line: 33, column: 18, scope: !52)
!76 = !DILocalVariable(name: "buf", scope: !52, file: !2, line: 34, type: !43)
!77 = !DILocation(line: 34, column: 8, scope: !52)
!78 = !DILocalVariable(name: "result", scope: !52, file: !2, line: 35, type: !44)
!79 = !DILocation(line: 35, column: 7, scope: !52)
!80 = !DILocalVariable(name: "token", scope: !52, file: !2, line: 36, type: !44)
!81 = !DILocation(line: 36, column: 7, scope: !52)
!82 = !DILocation(line: 38, column: 3, scope: !52)
!83 = !DILocation(line: 39, column: 3, scope: !52)
!84 = !DILocation(line: 40, column: 3, scope: !52)
!85 = !DILocation(line: 42, column: 7, scope: !86)
!86 = distinct !DILexicalBlock(scope: !52, file: !2, line: 42, column: 7)
!87 = !DILocation(line: 42, column: 12, scope: !86)
!88 = !DILocation(line: 42, column: 7, scope: !52)
!89 = !DILocation(line: 43, column: 65, scope: !90)
!90 = distinct !DILexicalBlock(scope: !86, file: !2, line: 42, column: 25)
!91 = !DILocation(line: 43, column: 5, scope: !90)
!92 = !DILocation(line: 44, column: 5, scope: !90)
!93 = !DILocation(line: 45, column: 3, scope: !90)
!94 = !DILocation(line: 47, column: 3, scope: !52)
!95 = !DILocalVariable(name: "i", scope: !96, file: !2, line: 49, type: !44)
!96 = distinct !DILexicalBlock(scope: !52, file: !2, line: 49, column: 3)
!97 = !DILocation(line: 49, column: 12, scope: !96)
!98 = !DILocation(line: 49, column: 8, scope: !96)
!99 = !DILocation(line: 49, column: 19, scope: !100)
!100 = distinct !DILexicalBlock(scope: !96, file: !2, line: 49, column: 3)
!101 = !DILocation(line: 49, column: 21, scope: !100)
!102 = !DILocation(line: 49, column: 3, scope: !96)
!103 = !DILocation(line: 50, column: 5, scope: !104)
!104 = distinct !DILexicalBlock(scope: !100, file: !2, line: 49, column: 38)
!105 = !DILocation(line: 50, column: 14, scope: !104)
!106 = !DILocation(line: 50, column: 17, scope: !104)
!107 = !DILocation(line: 51, column: 3, scope: !104)
!108 = !DILocation(line: 49, column: 34, scope: !100)
!109 = !DILocation(line: 49, column: 3, scope: !100)
!110 = distinct !{!110, !102, !111, !112}
!111 = !DILocation(line: 51, column: 3, scope: !96)
!112 = !{!"llvm.loop.mustprogress"}
!113 = !DILocation(line: 53, column: 20, scope: !52)
!114 = !DILocation(line: 53, column: 3, scope: !52)
!115 = !DILocalVariable(name: "buf_alias", scope: !52, file: !2, line: 55, type: !43)
!116 = !DILocation(line: 55, column: 8, scope: !52)
!117 = !DILocalVariable(name: "win_base_alias", scope: !52, file: !2, line: 56, type: !43)
!118 = !DILocation(line: 56, column: 8, scope: !52)
!119 = !DILocation(line: 58, column: 15, scope: !52)
!120 = !DILocation(line: 58, column: 13, scope: !52)
!121 = !DILocation(line: 59, column: 20, scope: !52)
!122 = !DILocation(line: 59, column: 18, scope: !52)
!123 = !DILocation(line: 61, column: 7, scope: !124)
!124 = distinct !DILexicalBlock(scope: !52, file: !2, line: 61, column: 7)
!125 = !DILocation(line: 61, column: 12, scope: !124)
!126 = !DILocation(line: 61, column: 7, scope: !52)
!127 = !DILocation(line: 62, column: 13, scope: !128)
!128 = distinct !DILexicalBlock(scope: !124, file: !2, line: 61, column: 18)
!129 = !DILocation(line: 62, column: 54, scope: !128)
!130 = !DILocation(line: 62, column: 5, scope: !128)
!131 = !DILocation(line: 63, column: 3, scope: !128)
!132 = !DILocation(line: 64, column: 41, scope: !133)
!133 = distinct !DILexicalBlock(scope: !124, file: !2, line: 63, column: 10)
!134 = !DILocation(line: 64, column: 5, scope: !133)
!135 = !DILocation(line: 67, column: 20, scope: !52)
!136 = !DILocation(line: 67, column: 3, scope: !52)
!137 = !DILocation(line: 69, column: 3, scope: !52)
!138 = !DILocation(line: 72, column: 10, scope: !52)
!139 = !DILocation(line: 72, column: 17, scope: !52)
!140 = !DILocation(line: 72, column: 16, scope: !52)
!141 = !DILocation(line: 72, column: 22, scope: !52)
!142 = !DILocation(line: 72, column: 30, scope: !52)
!143 = !DILocation(line: 70, column: 3, scope: !52)
!144 = !DILocation(line: 74, column: 3, scope: !52)
!145 = !DILocation(line: 75, column: 3, scope: !52)
!146 = !DILocation(line: 77, column: 3, scope: !52)
