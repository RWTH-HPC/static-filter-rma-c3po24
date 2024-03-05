; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c.ll'
source_filename = "results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c"
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
@0 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@1 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@2 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@3 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@4 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@5 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@6 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@7 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@8 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@9 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@10 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@11 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@12 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@13 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@14 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@15 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@16 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@17 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@18 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1
@19 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !40 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !47, metadata !DIExpression()), !dbg !48
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !49, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.declare(metadata ptr %6, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.declare(metadata ptr %7, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.declare(metadata ptr %8, metadata !55, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.declare(metadata ptr %9, metadata !60, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.declare(metadata ptr %10, metadata !63, metadata !DIExpression()), !dbg !64
  store i32 1, ptr %10, align 4, !dbg !64
  call void @llvm.dbg.declare(metadata ptr %11, metadata !65, metadata !DIExpression()), !dbg !66
  store i32 2, ptr %11, align 4, !dbg !66
  call void @llvm.dbg.declare(metadata ptr %12, metadata !67, metadata !DIExpression()), !dbg !68
  store ptr %10, ptr %12, align 8, !dbg !68
  call void @llvm.dbg.declare(metadata ptr %13, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.declare(metadata ptr %14, metadata !71, metadata !DIExpression()), !dbg !72
  store i32 42, ptr %14, align 4, !dbg !72
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !73
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !74
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !75
  %21 = load i32, ptr %7, align 4, !dbg !76
  %22 = icmp ne i32 %21, 2, !dbg !78
  br i1 %22, label %23, label %27, !dbg !79

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !80
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !82
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !83
  br label %27, !dbg !84

27:                                               ; preds = %23, %2
  %28 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 4, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 45, ptr @0), !dbg !85
  call void @llvm.dbg.declare(metadata ptr %15, metadata !86, metadata !DIExpression()), !dbg !88
  store i32 0, ptr %15, align 4, !dbg !88
  br label %29, !dbg !89

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !90
  %31 = icmp slt i32 %30, 10, !dbg !92
  br i1 %31, label %32, label %40, !dbg !93

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !94
  %34 = load i32, ptr %15, align 4, !dbg !96
  %35 = sext i32 %34 to i64, !dbg !94
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !94
  store i32 0, ptr %36, align 4, !dbg !97
  br label %37, !dbg !98

37:                                               ; preds = %32
  call void @parcoach_rma_load(ptr %15, i64 32, i32 47, ptr @18), !dbg !99
  %38 = load i32, ptr %15, align 4, !dbg !99
  %39 = add nsw i32 %38, 1, !dbg !99
  call void @parcoach_rma_store(ptr %15, i64 32, i32 47, ptr @19), !dbg !99
  store i32 %39, ptr %15, align 4, !dbg !99
  br label %29, !dbg !100, !llvm.loop !101

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !104
  %42 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %41, i32 51, ptr @1), !dbg !105
  call void @llvm.dbg.declare(metadata ptr %16, metadata !106, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.declare(metadata ptr %17, metadata !108, metadata !DIExpression()), !dbg !109
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %12, i64 8, i1 false), !dbg !110
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %9, i64 8, i1 false), !dbg !111
  call void @parcoach_rma_load(ptr %6, i64 32, i32 59, ptr @2), !dbg !112
  %43 = load i32, ptr %6, align 4, !dbg !112
  %44 = icmp eq i32 %43, 0, !dbg !114
  br i1 %44, label %45, label %52, !dbg !115

45:                                               ; preds = %40
  call void @parcoach_rma_load(ptr %16, i64 64, i32 60, ptr @3), !dbg !116
  %46 = load ptr, ptr %16, align 8, !dbg !116
  call void @parcoach_rma_load(ptr %8, i64 64, i32 60, ptr @4), !dbg !118
  %47 = load ptr, ptr %8, align 8, !dbg !118
  %48 = call i32 @parcoach_rma_MPI_Put(ptr %46, i32 1, ptr @ompi_mpi_int, i32 1, i64 0, i32 1, ptr @ompi_mpi_int, ptr %47, i32 60, ptr @5), !dbg !119
  call void @parcoach_rma_load(ptr %16, i64 64, i32 61, ptr @6), !dbg !120
  %49 = load ptr, ptr %16, align 8, !dbg !120
  call void @parcoach_rma_load(ptr %49, i64 32, i32 61, ptr @7), !dbg !121
  %50 = load i32, ptr %49, align 4, !dbg !121
  %51 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %50), !dbg !122
  br label %52, !dbg !123

52:                                               ; preds = %45, %40
  call void @parcoach_rma_load(ptr %8, i64 64, i32 64, ptr @8), !dbg !124
  %53 = load ptr, ptr %8, align 8, !dbg !124
  %54 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %53, i32 64, ptr @9), !dbg !125
  %55 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 66, ptr @10), !dbg !126
  call void @parcoach_rma_load(ptr %6, i64 32, i32 69, ptr @11), !dbg !127
  %56 = load i32, ptr %6, align 4, !dbg !127
  call void @parcoach_rma_load(ptr %12, i64 64, i32 69, ptr @12), !dbg !128
  %57 = load ptr, ptr %12, align 8, !dbg !128
  call void @parcoach_rma_load(ptr %57, i64 32, i32 69, ptr @13), !dbg !129
  %58 = load i32, ptr %57, align 4, !dbg !129
  call void @parcoach_rma_load(ptr %11, i64 32, i32 69, ptr @14), !dbg !130
  %59 = load i32, ptr %11, align 4, !dbg !130
  call void @parcoach_rma_load(ptr %9, i64 64, i32 69, ptr @15), !dbg !131
  %60 = load ptr, ptr %9, align 8, !dbg !131
  %61 = getelementptr inbounds i32, ptr %60, i64 0, !dbg !131
  call void @parcoach_rma_load(ptr %61, i64 32, i32 69, ptr @16), !dbg !131
  %62 = load i32, ptr %61, align 4, !dbg !131
  %63 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %56, i32 noundef %58, i32 noundef %59, i32 noundef %62), !dbg !132
  %64 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 71, ptr @17), !dbg !133
  %65 = call i32 @MPI_Finalize(), !dbg !134
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

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

; Function Attrs: argmemonly nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #3

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

declare i32 @parcoach_rma_MPI_Win_allocate(i64, i32, ptr, ptr, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_fence(i32, ptr, i32, ptr)

declare void @parcoach_rma_load(ptr, i64, i32, ptr)

declare i32 @parcoach_rma_MPI_Put(ptr, i32, ptr, i32, i64, i32, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Barrier(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_free(ptr, i32, ptr)

declare void @parcoach_rma_store(ptr, i64, i32, ptr)

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { argmemonly nocallback nofree nounwind willreturn }

!llvm.dbg.cu = !{!17}
!llvm.module.flags = !{!31, !32, !33, !34, !35, !36, !37, !38}
!llvm.ident = !{!39}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 41, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/misc/004-MPI-misc-put-load-memcpy-local.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "7e603e43a7083b2efa520c1d8d626bc2")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 61, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 104, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 13)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 67, type: !14, isLocal: true, isDefinition: true)
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
!40 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 27, type: !41, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !46)
!41 = !DISubroutineType(types: !42)
!42 = !{!43, !43, !44}
!43 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!44 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !45, size: 64)
!45 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!46 = !{}
!47 = !DILocalVariable(name: "argc", arg: 1, scope: !40, file: !2, line: 27, type: !43)
!48 = !DILocation(line: 27, column: 14, scope: !40)
!49 = !DILocalVariable(name: "argv", arg: 2, scope: !40, file: !2, line: 27, type: !44)
!50 = !DILocation(line: 27, column: 27, scope: !40)
!51 = !DILocalVariable(name: "rank", scope: !40, file: !2, line: 28, type: !43)
!52 = !DILocation(line: 28, column: 7, scope: !40)
!53 = !DILocalVariable(name: "size", scope: !40, file: !2, line: 28, type: !43)
!54 = !DILocation(line: 28, column: 13, scope: !40)
!55 = !DILocalVariable(name: "win", scope: !40, file: !2, line: 29, type: !56)
!56 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !20, line: 429, baseType: !57)
!57 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !58, size: 64)
!58 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !20, line: 429, flags: DIFlagFwdDecl)
!59 = !DILocation(line: 29, column: 11, scope: !40)
!60 = !DILocalVariable(name: "win_base", scope: !40, file: !2, line: 30, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!62 = !DILocation(line: 30, column: 8, scope: !40)
!63 = !DILocalVariable(name: "value", scope: !40, file: !2, line: 31, type: !43)
!64 = !DILocation(line: 31, column: 7, scope: !40)
!65 = !DILocalVariable(name: "value2", scope: !40, file: !2, line: 31, type: !43)
!66 = !DILocation(line: 31, column: 18, scope: !40)
!67 = !DILocalVariable(name: "buf", scope: !40, file: !2, line: 32, type: !61)
!68 = !DILocation(line: 32, column: 8, scope: !40)
!69 = !DILocalVariable(name: "result", scope: !40, file: !2, line: 33, type: !43)
!70 = !DILocation(line: 33, column: 7, scope: !40)
!71 = !DILocalVariable(name: "token", scope: !40, file: !2, line: 34, type: !43)
!72 = !DILocation(line: 34, column: 7, scope: !40)
!73 = !DILocation(line: 36, column: 3, scope: !40)
!74 = !DILocation(line: 37, column: 3, scope: !40)
!75 = !DILocation(line: 38, column: 3, scope: !40)
!76 = !DILocation(line: 40, column: 7, scope: !77)
!77 = distinct !DILexicalBlock(scope: !40, file: !2, line: 40, column: 7)
!78 = !DILocation(line: 40, column: 12, scope: !77)
!79 = !DILocation(line: 40, column: 7, scope: !40)
!80 = !DILocation(line: 41, column: 65, scope: !81)
!81 = distinct !DILexicalBlock(scope: !77, file: !2, line: 40, column: 25)
!82 = !DILocation(line: 41, column: 5, scope: !81)
!83 = !DILocation(line: 42, column: 5, scope: !81)
!84 = !DILocation(line: 43, column: 3, scope: !81)
!85 = !DILocation(line: 45, column: 3, scope: !40)
!86 = !DILocalVariable(name: "i", scope: !87, file: !2, line: 47, type: !43)
!87 = distinct !DILexicalBlock(scope: !40, file: !2, line: 47, column: 3)
!88 = !DILocation(line: 47, column: 12, scope: !87)
!89 = !DILocation(line: 47, column: 8, scope: !87)
!90 = !DILocation(line: 47, column: 19, scope: !91)
!91 = distinct !DILexicalBlock(scope: !87, file: !2, line: 47, column: 3)
!92 = !DILocation(line: 47, column: 21, scope: !91)
!93 = !DILocation(line: 47, column: 3, scope: !87)
!94 = !DILocation(line: 48, column: 5, scope: !95)
!95 = distinct !DILexicalBlock(scope: !91, file: !2, line: 47, column: 38)
!96 = !DILocation(line: 48, column: 14, scope: !95)
!97 = !DILocation(line: 48, column: 17, scope: !95)
!98 = !DILocation(line: 49, column: 3, scope: !95)
!99 = !DILocation(line: 47, column: 34, scope: !91)
!100 = !DILocation(line: 47, column: 3, scope: !91)
!101 = distinct !{!101, !93, !102, !103}
!102 = !DILocation(line: 49, column: 3, scope: !87)
!103 = !{!"llvm.loop.mustprogress"}
!104 = !DILocation(line: 51, column: 20, scope: !40)
!105 = !DILocation(line: 51, column: 3, scope: !40)
!106 = !DILocalVariable(name: "buf_alias", scope: !40, file: !2, line: 53, type: !61)
!107 = !DILocation(line: 53, column: 8, scope: !40)
!108 = !DILocalVariable(name: "win_base_alias", scope: !40, file: !2, line: 54, type: !61)
!109 = !DILocation(line: 54, column: 8, scope: !40)
!110 = !DILocation(line: 56, column: 3, scope: !40)
!111 = !DILocation(line: 57, column: 3, scope: !40)
!112 = !DILocation(line: 59, column: 7, scope: !113)
!113 = distinct !DILexicalBlock(scope: !40, file: !2, line: 59, column: 7)
!114 = !DILocation(line: 59, column: 12, scope: !113)
!115 = !DILocation(line: 59, column: 7, scope: !40)
!116 = !DILocation(line: 60, column: 13, scope: !117)
!117 = distinct !DILexicalBlock(scope: !113, file: !2, line: 59, column: 18)
!118 = !DILocation(line: 60, column: 54, scope: !117)
!119 = !DILocation(line: 60, column: 5, scope: !117)
!120 = !DILocation(line: 61, column: 30, scope: !117)
!121 = !DILocation(line: 61, column: 29, scope: !117)
!122 = !DILocation(line: 61, column: 5, scope: !117)
!123 = !DILocation(line: 62, column: 3, scope: !117)
!124 = !DILocation(line: 64, column: 20, scope: !40)
!125 = !DILocation(line: 64, column: 3, scope: !40)
!126 = !DILocation(line: 66, column: 3, scope: !40)
!127 = !DILocation(line: 69, column: 10, scope: !40)
!128 = !DILocation(line: 69, column: 17, scope: !40)
!129 = !DILocation(line: 69, column: 16, scope: !40)
!130 = !DILocation(line: 69, column: 22, scope: !40)
!131 = !DILocation(line: 69, column: 30, scope: !40)
!132 = !DILocation(line: 67, column: 3, scope: !40)
!133 = !DILocation(line: 71, column: 3, scope: !40)
!134 = !DILocation(line: 72, column: 3, scope: !40)
!135 = !DILocation(line: 74, column: 3, scope: !40)
