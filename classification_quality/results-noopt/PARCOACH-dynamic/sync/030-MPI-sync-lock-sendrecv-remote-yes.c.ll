; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/sync/030-MPI-sync-lock-sendrecv-remote-yes.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/sync/030-MPI-sync-lock-sendrecv-remote-yes.c"
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
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !53 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !59, metadata !DIExpression()), !dbg !60
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !61, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.declare(metadata ptr %6, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.declare(metadata ptr %7, metadata !65, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.declare(metadata ptr %8, metadata !67, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.declare(metadata ptr %9, metadata !72, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.declare(metadata ptr %10, metadata !75, metadata !DIExpression()), !dbg !76
  store i32 1, ptr %10, align 4, !dbg !76
  call void @llvm.dbg.declare(metadata ptr %11, metadata !77, metadata !DIExpression()), !dbg !78
  store i32 2, ptr %11, align 4, !dbg !78
  call void @llvm.dbg.declare(metadata ptr %12, metadata !79, metadata !DIExpression()), !dbg !80
  store ptr %10, ptr %12, align 8, !dbg !80
  call void @llvm.dbg.declare(metadata ptr %13, metadata !81, metadata !DIExpression()), !dbg !82
  call void @llvm.dbg.declare(metadata ptr %14, metadata !83, metadata !DIExpression()), !dbg !84
  store i32 42, ptr %14, align 4, !dbg !84
  %16 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !85
  %17 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !86
  %18 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !87
  %19 = load i32, ptr %7, align 4, !dbg !88
  %20 = icmp ne i32 %19, 2, !dbg !90
  br i1 %20, label %21, label %25, !dbg !91

21:                                               ; preds = %2
  %22 = load i32, ptr %7, align 4, !dbg !92
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %22, i32 noundef 2), !dbg !94
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !95
  br label %25, !dbg !96

25:                                               ; preds = %21, %2
  %26 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !97
  call void @llvm.dbg.declare(metadata ptr %15, metadata !98, metadata !DIExpression()), !dbg !100
  store i32 0, ptr %15, align 4, !dbg !100
  br label %27, !dbg !101

27:                                               ; preds = %35, %25
  %28 = load i32, ptr %15, align 4, !dbg !102
  %29 = icmp slt i32 %28, 10, !dbg !104
  br i1 %29, label %30, label %38, !dbg !105

30:                                               ; preds = %27
  %31 = load ptr, ptr %9, align 8, !dbg !106
  %32 = load i32, ptr %15, align 4, !dbg !108
  %33 = sext i32 %32 to i64, !dbg !106
  %34 = getelementptr inbounds i32, ptr %31, i64 %33, !dbg !106
  store i32 0, ptr %34, align 4, !dbg !109
  br label %35, !dbg !110

35:                                               ; preds = %30
  %36 = load i32, ptr %15, align 4, !dbg !111
  %37 = add nsw i32 %36, 1, !dbg !111
  store i32 %37, ptr %15, align 4, !dbg !111
  br label %27, !dbg !112, !llvm.loop !113

38:                                               ; preds = %27
  %39 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !116
  %40 = load i32, ptr %6, align 4, !dbg !117
  %41 = icmp eq i32 %40, 0, !dbg !119
  br i1 %41, label %42, label %50, !dbg !120

42:                                               ; preds = %38
  store i32 1, ptr %10, align 4, !dbg !121
  %43 = load ptr, ptr %8, align 8, !dbg !123
  %44 = call i32 @MPI_Win_lock(i32 noundef 2, i32 noundef 1, i32 noundef 0, ptr noundef %43), !dbg !124
  %45 = load ptr, ptr %8, align 8, !dbg !125
  %46 = call i32 @MPI_Put(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %45), !dbg !126
  %47 = load ptr, ptr %8, align 8, !dbg !127
  %48 = call i32 @MPI_Win_unlock(i32 noundef 1, ptr noundef %47), !dbg !128
  %49 = call i32 @MPI_Send(ptr noundef %14, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i32 noundef 0, ptr noundef @ompi_mpi_comm_world), !dbg !129
  br label %50, !dbg !130

50:                                               ; preds = %42, %38
  %51 = load i32, ptr %6, align 4, !dbg !131
  %52 = icmp eq i32 %51, 1, !dbg !133
  br i1 %52, label %53, label %59, !dbg !134

53:                                               ; preds = %50
  %54 = load ptr, ptr %9, align 8, !dbg !135
  %55 = getelementptr inbounds i32, ptr %54, i64 0, !dbg !135
  %56 = load i32, ptr %55, align 4, !dbg !135
  %57 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %56), !dbg !137
  %58 = call i32 @MPI_Recv(ptr noundef %14, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 0, i32 noundef 0, ptr noundef @ompi_mpi_comm_world, ptr noundef null), !dbg !138
  br label %59, !dbg !139

59:                                               ; preds = %53, %50
  %60 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !140
  %61 = load i32, ptr %6, align 4, !dbg !141
  %62 = load ptr, ptr %12, align 8, !dbg !142
  %63 = load i32, ptr %62, align 4, !dbg !143
  %64 = load i32, ptr %11, align 4, !dbg !144
  %65 = load ptr, ptr %9, align 8, !dbg !145
  %66 = getelementptr inbounds i32, ptr %65, i64 0, !dbg !145
  %67 = load i32, ptr %66, align 4, !dbg !145
  %68 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %61, i32 noundef %63, i32 noundef %64, i32 noundef %67), !dbg !146
  %69 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !147
  %70 = call i32 @MPI_Finalize(), !dbg !148
  ret i32 0, !dbg !149
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

declare i32 @MPI_Send(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Recv(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!17}
!llvm.module.flags = !{!44, !45, !46, !47, !48, !49, !50, !51}
!llvm.ident = !{!52}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 42, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/sync/030-MPI-sync-lock-sendrecv-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "dd42143821d8ebfd2226b92636313e30")
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
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 71, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 94)
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !18, globals: !43, splitDebugInlining: false, nameTableKind: None)
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
!30 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !31, size: 64)
!31 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Status", file: !20, line: 428, baseType: !32)
!32 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_status_public_t", file: !20, line: 438, size: 192, elements: !33)
!33 = !{!34, !36, !37, !38, !39}
!34 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_SOURCE", scope: !32, file: !20, line: 441, baseType: !35, size: 32)
!35 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!36 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_TAG", scope: !32, file: !20, line: 442, baseType: !35, size: 32, offset: 32)
!37 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_ERROR", scope: !32, file: !20, line: 443, baseType: !35, size: 32, offset: 64)
!38 = !DIDerivedType(tag: DW_TAG_member, name: "_cancelled", scope: !32, file: !20, line: 448, baseType: !35, size: 32, offset: 96)
!39 = !DIDerivedType(tag: DW_TAG_member, name: "_ucount", scope: !32, file: !20, line: 449, baseType: !40, size: 64, offset: 128)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !41, line: 46, baseType: !42)
!41 = !DIFile(filename: "/usr/lib/llvm-15/lib/clang/15.0.6/include/stddef.h", directory: "", checksumkind: CSK_MD5, checksum: "b76978376d35d5cd171876ac58ac1256")
!42 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!43 = !{!0, !7, !12}
!44 = !{i32 7, !"Dwarf Version", i32 5}
!45 = !{i32 2, !"Debug Info Version", i32 3}
!46 = !{i32 1, !"wchar_size", i32 4}
!47 = !{i32 7, !"openmp", i32 50}
!48 = !{i32 7, !"PIC Level", i32 2}
!49 = !{i32 7, !"PIE Level", i32 2}
!50 = !{i32 7, !"uwtable", i32 2}
!51 = !{i32 7, !"frame-pointer", i32 2}
!52 = !{!"Debian clang version 15.0.6"}
!53 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 28, type: !54, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !58)
!54 = !DISubroutineType(types: !55)
!55 = !{!35, !35, !56}
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !57, size: 64)
!57 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!58 = !{}
!59 = !DILocalVariable(name: "argc", arg: 1, scope: !53, file: !2, line: 28, type: !35)
!60 = !DILocation(line: 28, column: 14, scope: !53)
!61 = !DILocalVariable(name: "argv", arg: 2, scope: !53, file: !2, line: 28, type: !56)
!62 = !DILocation(line: 28, column: 27, scope: !53)
!63 = !DILocalVariable(name: "rank", scope: !53, file: !2, line: 29, type: !35)
!64 = !DILocation(line: 29, column: 7, scope: !53)
!65 = !DILocalVariable(name: "size", scope: !53, file: !2, line: 29, type: !35)
!66 = !DILocation(line: 29, column: 13, scope: !53)
!67 = !DILocalVariable(name: "win", scope: !53, file: !2, line: 30, type: !68)
!68 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !69)
!69 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !70, size: 64)
!70 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!71 = !DILocation(line: 30, column: 11, scope: !53)
!72 = !DILocalVariable(name: "win_base", scope: !53, file: !2, line: 31, type: !73)
!73 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !35, size: 64)
!74 = !DILocation(line: 31, column: 8, scope: !53)
!75 = !DILocalVariable(name: "value", scope: !53, file: !2, line: 32, type: !35)
!76 = !DILocation(line: 32, column: 7, scope: !53)
!77 = !DILocalVariable(name: "value2", scope: !53, file: !2, line: 32, type: !35)
!78 = !DILocation(line: 32, column: 18, scope: !53)
!79 = !DILocalVariable(name: "buf", scope: !53, file: !2, line: 33, type: !73)
!80 = !DILocation(line: 33, column: 8, scope: !53)
!81 = !DILocalVariable(name: "result", scope: !53, file: !2, line: 34, type: !35)
!82 = !DILocation(line: 34, column: 7, scope: !53)
!83 = !DILocalVariable(name: "token", scope: !53, file: !2, line: 35, type: !35)
!84 = !DILocation(line: 35, column: 7, scope: !53)
!85 = !DILocation(line: 37, column: 3, scope: !53)
!86 = !DILocation(line: 38, column: 3, scope: !53)
!87 = !DILocation(line: 39, column: 3, scope: !53)
!88 = !DILocation(line: 41, column: 7, scope: !89)
!89 = distinct !DILexicalBlock(scope: !53, file: !2, line: 41, column: 7)
!90 = !DILocation(line: 41, column: 12, scope: !89)
!91 = !DILocation(line: 41, column: 7, scope: !53)
!92 = !DILocation(line: 42, column: 65, scope: !93)
!93 = distinct !DILexicalBlock(scope: !89, file: !2, line: 41, column: 25)
!94 = !DILocation(line: 42, column: 5, scope: !93)
!95 = !DILocation(line: 43, column: 5, scope: !93)
!96 = !DILocation(line: 44, column: 3, scope: !93)
!97 = !DILocation(line: 46, column: 3, scope: !53)
!98 = !DILocalVariable(name: "i", scope: !99, file: !2, line: 48, type: !35)
!99 = distinct !DILexicalBlock(scope: !53, file: !2, line: 48, column: 3)
!100 = !DILocation(line: 48, column: 12, scope: !99)
!101 = !DILocation(line: 48, column: 8, scope: !99)
!102 = !DILocation(line: 48, column: 19, scope: !103)
!103 = distinct !DILexicalBlock(scope: !99, file: !2, line: 48, column: 3)
!104 = !DILocation(line: 48, column: 21, scope: !103)
!105 = !DILocation(line: 48, column: 3, scope: !99)
!106 = !DILocation(line: 49, column: 5, scope: !107)
!107 = distinct !DILexicalBlock(scope: !103, file: !2, line: 48, column: 38)
!108 = !DILocation(line: 49, column: 14, scope: !107)
!109 = !DILocation(line: 49, column: 17, scope: !107)
!110 = !DILocation(line: 50, column: 3, scope: !107)
!111 = !DILocation(line: 48, column: 34, scope: !103)
!112 = !DILocation(line: 48, column: 3, scope: !103)
!113 = distinct !{!113, !105, !114, !115}
!114 = !DILocation(line: 50, column: 3, scope: !99)
!115 = !{!"llvm.loop.mustprogress"}
!116 = !DILocation(line: 52, column: 3, scope: !53)
!117 = !DILocation(line: 54, column: 7, scope: !118)
!118 = distinct !DILexicalBlock(scope: !53, file: !2, line: 54, column: 7)
!119 = !DILocation(line: 54, column: 12, scope: !118)
!120 = !DILocation(line: 54, column: 7, scope: !53)
!121 = !DILocation(line: 55, column: 11, scope: !122)
!122 = distinct !DILexicalBlock(scope: !118, file: !2, line: 54, column: 18)
!123 = !DILocation(line: 56, column: 41, scope: !122)
!124 = !DILocation(line: 56, column: 5, scope: !122)
!125 = !DILocation(line: 58, column: 51, scope: !122)
!126 = !DILocation(line: 58, column: 5, scope: !122)
!127 = !DILocation(line: 59, column: 23, scope: !122)
!128 = !DILocation(line: 59, column: 5, scope: !122)
!129 = !DILocation(line: 61, column: 5, scope: !122)
!130 = !DILocation(line: 62, column: 3, scope: !122)
!131 = !DILocation(line: 64, column: 7, scope: !132)
!132 = distinct !DILexicalBlock(scope: !53, file: !2, line: 64, column: 7)
!133 = !DILocation(line: 64, column: 12, scope: !132)
!134 = !DILocation(line: 64, column: 7, scope: !53)
!135 = !DILocation(line: 66, column: 35, scope: !136)
!136 = distinct !DILexicalBlock(scope: !132, file: !2, line: 64, column: 18)
!137 = !DILocation(line: 66, column: 5, scope: !136)
!138 = !DILocation(line: 67, column: 5, scope: !136)
!139 = !DILocation(line: 68, column: 3, scope: !136)
!140 = !DILocation(line: 70, column: 3, scope: !53)
!141 = !DILocation(line: 73, column: 10, scope: !53)
!142 = !DILocation(line: 73, column: 17, scope: !53)
!143 = !DILocation(line: 73, column: 16, scope: !53)
!144 = !DILocation(line: 73, column: 22, scope: !53)
!145 = !DILocation(line: 73, column: 30, scope: !53)
!146 = !DILocation(line: 71, column: 3, scope: !53)
!147 = !DILocation(line: 75, column: 3, scope: !53)
!148 = !DILocation(line: 76, column: 3, scope: !53)
!149 = !DILocation(line: 78, column: 3, scope: !53)
