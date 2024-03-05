; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/sync/034-MPI-sync-pscw-remote-no.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/sync/034-MPI-sync-pscw-remote-no.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque
%struct.ompi_status_public_t = type { i32, i32, i32, i32, i64 }

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
  %16 = alloca i32, align 4
  %17 = alloca i32, align 4
  %18 = alloca %struct.ompi_status_public_t, align 8
  %19 = alloca ptr, align 8
  %20 = alloca ptr, align 8
  %21 = alloca ptr, align 8
  %22 = alloca i32, align 4
  %23 = alloca ptr, align 8
  %24 = alloca i32, align 4
  %25 = alloca i32, align 4
  %26 = alloca ptr, align 8
  %27 = alloca i32, align 4
  %28 = alloca i32, align 4
  %29 = alloca ptr, align 8
  %30 = alloca i32, align 4
  %31 = alloca ptr, align 8
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
  %32 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !68
  %33 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !69
  %34 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !70
  %35 = load i32, ptr %7, align 4, !dbg !71
  %36 = icmp ne i32 %35, 3, !dbg !73
  br i1 %36, label %37, label %41, !dbg !74

37:                                               ; preds = %2
  %38 = load i32, ptr %7, align 4, !dbg !75
  %39 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %38, i32 noundef 3), !dbg !77
  %40 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !78
  br label %41, !dbg !79

41:                                               ; preds = %37, %2
  %42 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !80
  call void @llvm.dbg.declare(metadata ptr %15, metadata !81, metadata !DIExpression()), !dbg !83
  store i32 0, ptr %15, align 4, !dbg !83
  br label %43, !dbg !84

43:                                               ; preds = %51, %41
  %44 = load i32, ptr %15, align 4, !dbg !85
  %45 = icmp slt i32 %44, 10, !dbg !87
  br i1 %45, label %46, label %54, !dbg !88

46:                                               ; preds = %43
  %47 = load ptr, ptr %9, align 8, !dbg !89
  %48 = load i32, ptr %15, align 4, !dbg !91
  %49 = sext i32 %48 to i64, !dbg !89
  %50 = getelementptr inbounds i32, ptr %47, i64 %49, !dbg !89
  store i32 0, ptr %50, align 4, !dbg !92
  br label %51, !dbg !93

51:                                               ; preds = %46
  %52 = load i32, ptr %15, align 4, !dbg !94
  %53 = add nsw i32 %52, 1, !dbg !94
  store i32 %53, ptr %15, align 4, !dbg !94
  br label %43, !dbg !95, !llvm.loop !96

54:                                               ; preds = %43
  call void @llvm.dbg.declare(metadata ptr %16, metadata !99, metadata !DIExpression()), !dbg !100
  call void @llvm.dbg.declare(metadata ptr %17, metadata !101, metadata !DIExpression()), !dbg !102
  call void @llvm.dbg.declare(metadata ptr %18, metadata !103, metadata !DIExpression()), !dbg !115
  call void @llvm.dbg.declare(metadata ptr %19, metadata !116, metadata !DIExpression()), !dbg !120
  call void @llvm.dbg.declare(metadata ptr %20, metadata !121, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.declare(metadata ptr %21, metadata !123, metadata !DIExpression()), !dbg !127
  %55 = call i32 @MPI_Comm_group(ptr noundef @ompi_mpi_comm_world, ptr noundef %21), !dbg !128
  %56 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !129
  %57 = load i32, ptr %6, align 4, !dbg !130
  %58 = icmp eq i32 %57, 0, !dbg !132
  br i1 %58, label %59, label %69, !dbg !133

59:                                               ; preds = %54
  call void @llvm.dbg.declare(metadata ptr %22, metadata !134, metadata !DIExpression()), !dbg !136
  store i32 2, ptr %22, align 4, !dbg !136
  call void @llvm.dbg.declare(metadata ptr %23, metadata !137, metadata !DIExpression()), !dbg !138
  %60 = load ptr, ptr %21, align 8, !dbg !139
  %61 = call i32 @MPI_Group_incl(ptr noundef %60, i32 noundef 1, ptr noundef %22, ptr noundef %23), !dbg !140
  %62 = load ptr, ptr %23, align 8, !dbg !141
  %63 = load ptr, ptr %8, align 8, !dbg !142
  %64 = call i32 @MPI_Win_start(ptr noundef %62, i32 noundef 0, ptr noundef %63), !dbg !143
  call void @llvm.dbg.declare(metadata ptr %24, metadata !144, metadata !DIExpression()), !dbg !145
  store i32 42, ptr %24, align 4, !dbg !145
  %65 = load ptr, ptr %8, align 8, !dbg !146
  %66 = call i32 @MPI_Put(ptr noundef %24, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 2, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %65), !dbg !147
  %67 = load ptr, ptr %8, align 8, !dbg !148
  %68 = call i32 @MPI_Win_complete(ptr noundef %67), !dbg !149
  br label %102, !dbg !150

69:                                               ; preds = %54
  %70 = load i32, ptr %6, align 4, !dbg !151
  %71 = icmp eq i32 %70, 1, !dbg !153
  br i1 %71, label %72, label %82, !dbg !154

72:                                               ; preds = %69
  call void @llvm.dbg.declare(metadata ptr %25, metadata !155, metadata !DIExpression()), !dbg !157
  store i32 2, ptr %25, align 4, !dbg !157
  call void @llvm.dbg.declare(metadata ptr %26, metadata !158, metadata !DIExpression()), !dbg !159
  %73 = load ptr, ptr %21, align 8, !dbg !160
  %74 = call i32 @MPI_Group_incl(ptr noundef %73, i32 noundef 1, ptr noundef %25, ptr noundef %26), !dbg !161
  %75 = load ptr, ptr %26, align 8, !dbg !162
  %76 = load ptr, ptr %8, align 8, !dbg !163
  %77 = call i32 @MPI_Win_start(ptr noundef %75, i32 noundef 0, ptr noundef %76), !dbg !164
  call void @llvm.dbg.declare(metadata ptr %27, metadata !165, metadata !DIExpression()), !dbg !166
  %78 = load ptr, ptr %8, align 8, !dbg !167
  %79 = call i32 @MPI_Get(ptr noundef %27, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 2, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %78), !dbg !168
  %80 = load ptr, ptr %8, align 8, !dbg !169
  %81 = call i32 @MPI_Win_complete(ptr noundef %80), !dbg !170
  br label %101, !dbg !171

82:                                               ; preds = %69
  %83 = load i32, ptr %6, align 4, !dbg !172
  %84 = icmp eq i32 %83, 2, !dbg !174
  br i1 %84, label %85, label %100, !dbg !175

85:                                               ; preds = %82
  call void @llvm.dbg.declare(metadata ptr %28, metadata !176, metadata !DIExpression()), !dbg !178
  store i32 0, ptr %28, align 4, !dbg !178
  call void @llvm.dbg.declare(metadata ptr %29, metadata !179, metadata !DIExpression()), !dbg !180
  %86 = load ptr, ptr %21, align 8, !dbg !181
  %87 = call i32 @MPI_Group_incl(ptr noundef %86, i32 noundef 1, ptr noundef %28, ptr noundef %29), !dbg !182
  call void @llvm.dbg.declare(metadata ptr %30, metadata !183, metadata !DIExpression()), !dbg !184
  store i32 1, ptr %30, align 4, !dbg !184
  call void @llvm.dbg.declare(metadata ptr %31, metadata !185, metadata !DIExpression()), !dbg !186
  %88 = load ptr, ptr %21, align 8, !dbg !187
  %89 = call i32 @MPI_Group_incl(ptr noundef %88, i32 noundef 1, ptr noundef %30, ptr noundef %31), !dbg !188
  %90 = load ptr, ptr %29, align 8, !dbg !189
  %91 = load ptr, ptr %8, align 8, !dbg !190
  %92 = call i32 @MPI_Win_post(ptr noundef %90, i32 noundef 0, ptr noundef %91), !dbg !191
  %93 = load ptr, ptr %8, align 8, !dbg !192
  %94 = call i32 @MPI_Win_wait(ptr noundef %93), !dbg !193
  %95 = load ptr, ptr %31, align 8, !dbg !194
  %96 = load ptr, ptr %8, align 8, !dbg !195
  %97 = call i32 @MPI_Win_post(ptr noundef %95, i32 noundef 0, ptr noundef %96), !dbg !196
  %98 = load ptr, ptr %8, align 8, !dbg !197
  %99 = call i32 @MPI_Win_wait(ptr noundef %98), !dbg !198
  br label %100, !dbg !199

100:                                              ; preds = %85, %82
  br label %101

101:                                              ; preds = %100, %72
  br label %102

102:                                              ; preds = %101, %59
  %103 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !200
  %104 = load i32, ptr %6, align 4, !dbg !201
  %105 = load ptr, ptr %12, align 8, !dbg !202
  %106 = load i32, ptr %105, align 4, !dbg !203
  %107 = load i32, ptr %11, align 4, !dbg !204
  %108 = load ptr, ptr %9, align 8, !dbg !205
  %109 = getelementptr inbounds i32, ptr %108, i64 0, !dbg !205
  %110 = load i32, ptr %109, align 4, !dbg !205
  %111 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %104, i32 noundef %106, i32 noundef %107, i32 noundef %110), !dbg !206
  %112 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !207
  %113 = call i32 @MPI_Finalize(), !dbg !208
  ret i32 0, !dbg !209
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_group(ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Group_incl(ptr noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_start(ptr noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_complete(ptr noundef) #2

declare i32 @MPI_Get(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_post(ptr noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Win_wait(ptr noundef) #2

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
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/sync/034-MPI-sync-pscw-remote-no.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "9643a772cc7ceacb4bb264a8e621239a")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 96, type: !9, isLocal: true, isDefinition: true)
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
!99 = !DILocalVariable(name: "send_buf", scope: !35, file: !2, line: 51, type: !38)
!100 = !DILocation(line: 51, column: 7, scope: !35)
!101 = !DILocalVariable(name: "recv_buf", scope: !35, file: !2, line: 51, type: !38)
!102 = !DILocation(line: 51, column: 17, scope: !35)
!103 = !DILocalVariable(name: "status", scope: !35, file: !2, line: 52, type: !104)
!104 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Status", file: !15, line: 428, baseType: !105)
!105 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_status_public_t", file: !15, line: 438, size: 192, elements: !106)
!106 = !{!107, !108, !109, !110, !111}
!107 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_SOURCE", scope: !105, file: !15, line: 441, baseType: !38, size: 32)
!108 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_TAG", scope: !105, file: !15, line: 442, baseType: !38, size: 32, offset: 32)
!109 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_ERROR", scope: !105, file: !15, line: 443, baseType: !38, size: 32, offset: 64)
!110 = !DIDerivedType(tag: DW_TAG_member, name: "_cancelled", scope: !105, file: !15, line: 448, baseType: !38, size: 32, offset: 96)
!111 = !DIDerivedType(tag: DW_TAG_member, name: "_ucount", scope: !105, file: !15, line: 449, baseType: !112, size: 64, offset: 128)
!112 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !113, line: 46, baseType: !114)
!113 = !DIFile(filename: "/usr/lib/llvm-15/lib/clang/15.0.6/include/stddef.h", directory: "", checksumkind: CSK_MD5, checksum: "b76978376d35d5cd171876ac58ac1256")
!114 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!115 = !DILocation(line: 52, column: 14, scope: !35)
!116 = !DILocalVariable(name: "request", scope: !35, file: !2, line: 53, type: !117)
!117 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Request", file: !15, line: 426, baseType: !118)
!118 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !119, size: 64)
!119 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_request_t", file: !15, line: 426, flags: DIFlagFwdDecl)
!120 = !DILocation(line: 53, column: 15, scope: !35)
!121 = !DILocalVariable(name: "info", scope: !35, file: !2, line: 54, type: !19)
!122 = !DILocation(line: 54, column: 12, scope: !35)
!123 = !DILocalVariable(name: "world_group", scope: !35, file: !2, line: 56, type: !124)
!124 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Group", file: !15, line: 423, baseType: !125)
!125 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !126, size: 64)
!126 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_group_t", file: !15, line: 423, flags: DIFlagFwdDecl)
!127 = !DILocation(line: 56, column: 13, scope: !35)
!128 = !DILocation(line: 57, column: 3, scope: !35)
!129 = !DILocation(line: 59, column: 3, scope: !35)
!130 = !DILocation(line: 61, column: 7, scope: !131)
!131 = distinct !DILexicalBlock(scope: !35, file: !2, line: 61, column: 7)
!132 = !DILocation(line: 61, column: 12, scope: !131)
!133 = !DILocation(line: 61, column: 7, scope: !35)
!134 = !DILocalVariable(name: "destrank", scope: !135, file: !2, line: 62, type: !38)
!135 = distinct !DILexicalBlock(scope: !131, file: !2, line: 61, column: 18)
!136 = !DILocation(line: 62, column: 9, scope: !135)
!137 = !DILocalVariable(name: "destgroup", scope: !135, file: !2, line: 63, type: !124)
!138 = !DILocation(line: 63, column: 15, scope: !135)
!139 = !DILocation(line: 64, column: 20, scope: !135)
!140 = !DILocation(line: 64, column: 5, scope: !135)
!141 = !DILocation(line: 66, column: 19, scope: !135)
!142 = !DILocation(line: 66, column: 33, scope: !135)
!143 = !DILocation(line: 66, column: 5, scope: !135)
!144 = !DILocalVariable(name: "value", scope: !135, file: !2, line: 67, type: !38)
!145 = !DILocation(line: 67, column: 9, scope: !135)
!146 = !DILocation(line: 68, column: 51, scope: !135)
!147 = !DILocation(line: 68, column: 5, scope: !135)
!148 = !DILocation(line: 69, column: 22, scope: !135)
!149 = !DILocation(line: 69, column: 5, scope: !135)
!150 = !DILocation(line: 70, column: 3, scope: !135)
!151 = !DILocation(line: 70, column: 14, scope: !152)
!152 = distinct !DILexicalBlock(scope: !131, file: !2, line: 70, column: 14)
!153 = !DILocation(line: 70, column: 19, scope: !152)
!154 = !DILocation(line: 70, column: 14, scope: !131)
!155 = !DILocalVariable(name: "destrank", scope: !156, file: !2, line: 71, type: !38)
!156 = distinct !DILexicalBlock(scope: !152, file: !2, line: 70, column: 25)
!157 = !DILocation(line: 71, column: 9, scope: !156)
!158 = !DILocalVariable(name: "destgroup", scope: !156, file: !2, line: 72, type: !124)
!159 = !DILocation(line: 72, column: 15, scope: !156)
!160 = !DILocation(line: 73, column: 20, scope: !156)
!161 = !DILocation(line: 73, column: 5, scope: !156)
!162 = !DILocation(line: 74, column: 19, scope: !156)
!163 = !DILocation(line: 74, column: 33, scope: !156)
!164 = !DILocation(line: 74, column: 5, scope: !156)
!165 = !DILocalVariable(name: "value", scope: !156, file: !2, line: 75, type: !38)
!166 = !DILocation(line: 75, column: 9, scope: !156)
!167 = !DILocation(line: 76, column: 51, scope: !156)
!168 = !DILocation(line: 76, column: 5, scope: !156)
!169 = !DILocation(line: 77, column: 22, scope: !156)
!170 = !DILocation(line: 77, column: 5, scope: !156)
!171 = !DILocation(line: 79, column: 3, scope: !156)
!172 = !DILocation(line: 79, column: 14, scope: !173)
!173 = distinct !DILexicalBlock(scope: !152, file: !2, line: 79, column: 14)
!174 = !DILocation(line: 79, column: 19, scope: !173)
!175 = !DILocation(line: 79, column: 14, scope: !152)
!176 = !DILocalVariable(name: "srcrank0", scope: !177, file: !2, line: 80, type: !38)
!177 = distinct !DILexicalBlock(scope: !173, file: !2, line: 79, column: 25)
!178 = !DILocation(line: 80, column: 9, scope: !177)
!179 = !DILocalVariable(name: "srcgroup0", scope: !177, file: !2, line: 81, type: !124)
!180 = !DILocation(line: 81, column: 15, scope: !177)
!181 = !DILocation(line: 82, column: 20, scope: !177)
!182 = !DILocation(line: 82, column: 5, scope: !177)
!183 = !DILocalVariable(name: "srcrank1", scope: !177, file: !2, line: 84, type: !38)
!184 = !DILocation(line: 84, column: 9, scope: !177)
!185 = !DILocalVariable(name: "srcgroup1", scope: !177, file: !2, line: 85, type: !124)
!186 = !DILocation(line: 85, column: 15, scope: !177)
!187 = !DILocation(line: 86, column: 20, scope: !177)
!188 = !DILocation(line: 86, column: 5, scope: !177)
!189 = !DILocation(line: 88, column: 18, scope: !177)
!190 = !DILocation(line: 88, column: 32, scope: !177)
!191 = !DILocation(line: 88, column: 5, scope: !177)
!192 = !DILocation(line: 89, column: 18, scope: !177)
!193 = !DILocation(line: 89, column: 5, scope: !177)
!194 = !DILocation(line: 91, column: 18, scope: !177)
!195 = !DILocation(line: 91, column: 32, scope: !177)
!196 = !DILocation(line: 91, column: 5, scope: !177)
!197 = !DILocation(line: 92, column: 18, scope: !177)
!198 = !DILocation(line: 92, column: 5, scope: !177)
!199 = !DILocation(line: 93, column: 3, scope: !177)
!200 = !DILocation(line: 95, column: 3, scope: !35)
!201 = !DILocation(line: 98, column: 10, scope: !35)
!202 = !DILocation(line: 98, column: 17, scope: !35)
!203 = !DILocation(line: 98, column: 16, scope: !35)
!204 = !DILocation(line: 98, column: 22, scope: !35)
!205 = !DILocation(line: 98, column: 30, scope: !35)
!206 = !DILocation(line: 96, column: 3, scope: !35)
!207 = !DILocation(line: 100, column: 3, scope: !35)
!208 = !DILocation(line: 101, column: 3, scope: !35)
!209 = !DILocation(line: 103, column: 3, scope: !35)
