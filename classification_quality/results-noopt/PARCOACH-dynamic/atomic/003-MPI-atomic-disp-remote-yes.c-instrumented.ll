; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c.ll'
source_filename = "results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c"
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
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@ompi_mpi_op_sum = external global %struct.ompi_predefined_op_t, align 1
@__const.main.value2 = private unnamed_addr constant [4 x i32] [i32 1, i32 2, i32 3, i32 4], align 16
@.str.1 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !7
@0 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@1 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@2 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@3 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@4 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@5 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@6 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@7 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@8 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@9 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@10 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@11 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@12 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@13 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@14 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@15 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@16 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@17 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@18 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@19 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1
@20 = private unnamed_addr constant [81 x i8] c"results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c\00", align 1

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
  %22 = icmp ne i32 %21, 3, !dbg !76
  br i1 %22, label %23, label %27, !dbg !77

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !78
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 3), !dbg !80
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !81
  br label %27, !dbg !82

27:                                               ; preds = %23, %2
  %28 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 1, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 46, ptr @0), !dbg !83
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
  call void @parcoach_rma_load(ptr %15, i64 32, i32 48, ptr @19), !dbg !97
  %38 = load i32, ptr %15, align 4, !dbg !97
  %39 = add nsw i32 %38, 1, !dbg !97
  call void @parcoach_rma_store(ptr %15, i64 32, i32 48, ptr @20), !dbg !97
  store i32 %39, ptr %15, align 4, !dbg !97
  br label %29, !dbg !98, !llvm.loop !99

40:                                               ; preds = %29
  %41 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 52, ptr @1), !dbg !102
  %42 = load ptr, ptr %8, align 8, !dbg !103
  %43 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %42, i32 54, ptr @2), !dbg !104
  call void @parcoach_rma_load(ptr %6, i64 32, i32 55, ptr @3), !dbg !105
  %44 = load i32, ptr %6, align 4, !dbg !105
  %45 = icmp eq i32 %44, 0, !dbg !107
  br i1 %45, label %46, label %50, !dbg !108

46:                                               ; preds = %40
  call void @llvm.dbg.declare(metadata ptr %16, metadata !109, metadata !DIExpression()), !dbg !114
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %16, ptr align 16 @__const.main.value1, i64 16, i1 false), !dbg !114
  %47 = getelementptr inbounds [4 x i32], ptr %16, i64 0, i64 0, !dbg !115
  call void @parcoach_rma_load(ptr %8, i64 64, i32 58, ptr @4), !dbg !116
  %48 = load ptr, ptr %8, align 8, !dbg !116
  %49 = call i32 @parcoach_rma_MPI_Accumulate(ptr %47, i32 4, ptr @ompi_mpi_int, i32 1, i64 0, i32 4, ptr @ompi_mpi_int, ptr @ompi_mpi_op_sum, ptr %48, i32 58, ptr @5), !dbg !117
  br label %50, !dbg !118

50:                                               ; preds = %46, %40
  call void @parcoach_rma_load(ptr %6, i64 32, i32 60, ptr @6), !dbg !119
  %51 = load i32, ptr %6, align 4, !dbg !119
  %52 = icmp eq i32 %51, 2, !dbg !121
  br i1 %52, label %53, label %57, !dbg !122

53:                                               ; preds = %50
  call void @llvm.dbg.declare(metadata ptr %17, metadata !123, metadata !DIExpression()), !dbg !125
  call void @llvm.memcpy.p0.p0.i64(ptr align 16 %17, ptr align 16 @__const.main.value2, i64 16, i1 false), !dbg !125
  %54 = getelementptr inbounds [4 x i32], ptr %17, i64 0, i64 0, !dbg !126
  call void @parcoach_rma_load(ptr %8, i64 64, i32 63, ptr @7), !dbg !127
  %55 = load ptr, ptr %8, align 8, !dbg !127
  %56 = call i32 @parcoach_rma_MPI_Accumulate(ptr %54, i32 4, ptr @ompi_mpi_int, i32 1, i64 1, i32 4, ptr @ompi_mpi_int, ptr @ompi_mpi_op_sum, ptr %55, i32 63, ptr @8), !dbg !128
  br label %57, !dbg !129

57:                                               ; preds = %53, %50
  call void @parcoach_rma_load(ptr %8, i64 64, i32 65, ptr @9), !dbg !130
  %58 = load ptr, ptr %8, align 8, !dbg !130
  %59 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %58, i32 65, ptr @10), !dbg !131
  %60 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 67, ptr @11), !dbg !132
  call void @parcoach_rma_load(ptr %6, i64 32, i32 70, ptr @12), !dbg !133
  %61 = load i32, ptr %6, align 4, !dbg !133
  call void @parcoach_rma_load(ptr %12, i64 64, i32 70, ptr @13), !dbg !134
  %62 = load ptr, ptr %12, align 8, !dbg !134
  call void @parcoach_rma_load(ptr %62, i64 32, i32 70, ptr @14), !dbg !135
  %63 = load i32, ptr %62, align 4, !dbg !135
  call void @parcoach_rma_load(ptr %11, i64 32, i32 70, ptr @15), !dbg !136
  %64 = load i32, ptr %11, align 4, !dbg !136
  call void @parcoach_rma_load(ptr %9, i64 64, i32 70, ptr @16), !dbg !137
  %65 = load ptr, ptr %9, align 8, !dbg !137
  %66 = getelementptr inbounds i32, ptr %65, i64 0, !dbg !137
  call void @parcoach_rma_load(ptr %66, i64 32, i32 70, ptr @17), !dbg !137
  %67 = load i32, ptr %66, align 4, !dbg !137
  %68 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %61, i32 noundef %63, i32 noundef %64, i32 noundef %67), !dbg !138
  %69 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 72, ptr @18), !dbg !139
  %70 = call i32 @MPI_Finalize(), !dbg !140
  ret i32 0, !dbg !141
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

declare i32 @parcoach_rma_MPI_Win_allocate(i64, i32, ptr, ptr, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Barrier(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_fence(i32, ptr, i32, ptr)

declare void @parcoach_rma_load(ptr, i64, i32, ptr)

declare i32 @parcoach_rma_MPI_Accumulate(ptr, i32, ptr, i32, i64, i32, ptr, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_free(ptr, i32, ptr)

declare void @parcoach_rma_store(ptr, i64, i32, ptr)

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { argmemonly nocallback nofree nounwind willreturn }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!29, !30, !31, !32, !33, !34, !35, !36}
!llvm.ident = !{!37}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 42, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/atomic/003-MPI-atomic-disp-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "35b3c089ed70ea0db2ef81d0d43b0cf1")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 68, type: !9, isLocal: true, isDefinition: true)
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
!38 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 28, type: !39, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !44)
!39 = !DISubroutineType(types: !40)
!40 = !{!41, !41, !42}
!41 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!42 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !43, size: 64)
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!44 = !{}
!45 = !DILocalVariable(name: "argc", arg: 1, scope: !38, file: !2, line: 28, type: !41)
!46 = !DILocation(line: 28, column: 14, scope: !38)
!47 = !DILocalVariable(name: "argv", arg: 2, scope: !38, file: !2, line: 28, type: !42)
!48 = !DILocation(line: 28, column: 27, scope: !38)
!49 = !DILocalVariable(name: "rank", scope: !38, file: !2, line: 29, type: !41)
!50 = !DILocation(line: 29, column: 7, scope: !38)
!51 = !DILocalVariable(name: "size", scope: !38, file: !2, line: 29, type: !41)
!52 = !DILocation(line: 29, column: 13, scope: !38)
!53 = !DILocalVariable(name: "win", scope: !38, file: !2, line: 30, type: !54)
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !55)
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !56, size: 64)
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!57 = !DILocation(line: 30, column: 11, scope: !38)
!58 = !DILocalVariable(name: "win_base", scope: !38, file: !2, line: 31, type: !59)
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !41, size: 64)
!60 = !DILocation(line: 31, column: 8, scope: !38)
!61 = !DILocalVariable(name: "value", scope: !38, file: !2, line: 32, type: !41)
!62 = !DILocation(line: 32, column: 7, scope: !38)
!63 = !DILocalVariable(name: "value2", scope: !38, file: !2, line: 32, type: !41)
!64 = !DILocation(line: 32, column: 18, scope: !38)
!65 = !DILocalVariable(name: "buf", scope: !38, file: !2, line: 33, type: !59)
!66 = !DILocation(line: 33, column: 8, scope: !38)
!67 = !DILocalVariable(name: "result", scope: !38, file: !2, line: 34, type: !41)
!68 = !DILocation(line: 34, column: 7, scope: !38)
!69 = !DILocalVariable(name: "token", scope: !38, file: !2, line: 35, type: !41)
!70 = !DILocation(line: 35, column: 7, scope: !38)
!71 = !DILocation(line: 37, column: 3, scope: !38)
!72 = !DILocation(line: 38, column: 3, scope: !38)
!73 = !DILocation(line: 39, column: 3, scope: !38)
!74 = !DILocation(line: 41, column: 7, scope: !75)
!75 = distinct !DILexicalBlock(scope: !38, file: !2, line: 41, column: 7)
!76 = !DILocation(line: 41, column: 12, scope: !75)
!77 = !DILocation(line: 41, column: 7, scope: !38)
!78 = !DILocation(line: 42, column: 65, scope: !79)
!79 = distinct !DILexicalBlock(scope: !75, file: !2, line: 41, column: 25)
!80 = !DILocation(line: 42, column: 5, scope: !79)
!81 = !DILocation(line: 43, column: 5, scope: !79)
!82 = !DILocation(line: 44, column: 3, scope: !79)
!83 = !DILocation(line: 46, column: 3, scope: !38)
!84 = !DILocalVariable(name: "i", scope: !85, file: !2, line: 48, type: !41)
!85 = distinct !DILexicalBlock(scope: !38, file: !2, line: 48, column: 3)
!86 = !DILocation(line: 48, column: 12, scope: !85)
!87 = !DILocation(line: 48, column: 8, scope: !85)
!88 = !DILocation(line: 48, column: 19, scope: !89)
!89 = distinct !DILexicalBlock(scope: !85, file: !2, line: 48, column: 3)
!90 = !DILocation(line: 48, column: 21, scope: !89)
!91 = !DILocation(line: 48, column: 3, scope: !85)
!92 = !DILocation(line: 49, column: 5, scope: !93)
!93 = distinct !DILexicalBlock(scope: !89, file: !2, line: 48, column: 38)
!94 = !DILocation(line: 49, column: 14, scope: !93)
!95 = !DILocation(line: 49, column: 17, scope: !93)
!96 = !DILocation(line: 50, column: 3, scope: !93)
!97 = !DILocation(line: 48, column: 34, scope: !89)
!98 = !DILocation(line: 48, column: 3, scope: !89)
!99 = distinct !{!99, !91, !100, !101}
!100 = !DILocation(line: 50, column: 3, scope: !85)
!101 = !{!"llvm.loop.mustprogress"}
!102 = !DILocation(line: 52, column: 3, scope: !38)
!103 = !DILocation(line: 54, column: 20, scope: !38)
!104 = !DILocation(line: 54, column: 3, scope: !38)
!105 = !DILocation(line: 55, column: 7, scope: !106)
!106 = distinct !DILexicalBlock(scope: !38, file: !2, line: 55, column: 7)
!107 = !DILocation(line: 55, column: 12, scope: !106)
!108 = !DILocation(line: 55, column: 7, scope: !38)
!109 = !DILocalVariable(name: "value1", scope: !110, file: !2, line: 56, type: !111)
!110 = distinct !DILexicalBlock(scope: !106, file: !2, line: 55, column: 18)
!111 = !DICompositeType(tag: DW_TAG_array_type, baseType: !41, size: 128, elements: !112)
!112 = !{!113}
!113 = !DISubrange(count: 4)
!114 = !DILocation(line: 56, column: 9, scope: !110)
!115 = !DILocation(line: 58, column: 20, scope: !110)
!116 = !DILocation(line: 58, column: 67, scope: !110)
!117 = !DILocation(line: 58, column: 5, scope: !110)
!118 = !DILocation(line: 59, column: 3, scope: !110)
!119 = !DILocation(line: 60, column: 7, scope: !120)
!120 = distinct !DILexicalBlock(scope: !38, file: !2, line: 60, column: 7)
!121 = !DILocation(line: 60, column: 12, scope: !120)
!122 = !DILocation(line: 60, column: 7, scope: !38)
!123 = !DILocalVariable(name: "value2", scope: !124, file: !2, line: 61, type: !111)
!124 = distinct !DILexicalBlock(scope: !120, file: !2, line: 60, column: 18)
!125 = !DILocation(line: 61, column: 9, scope: !124)
!126 = !DILocation(line: 63, column: 20, scope: !124)
!127 = !DILocation(line: 63, column: 67, scope: !124)
!128 = !DILocation(line: 63, column: 5, scope: !124)
!129 = !DILocation(line: 64, column: 3, scope: !124)
!130 = !DILocation(line: 65, column: 20, scope: !38)
!131 = !DILocation(line: 65, column: 3, scope: !38)
!132 = !DILocation(line: 67, column: 3, scope: !38)
!133 = !DILocation(line: 70, column: 10, scope: !38)
!134 = !DILocation(line: 70, column: 17, scope: !38)
!135 = !DILocation(line: 70, column: 16, scope: !38)
!136 = !DILocation(line: 70, column: 22, scope: !38)
!137 = !DILocation(line: 70, column: 30, scope: !38)
!138 = !DILocation(line: 68, column: 3, scope: !38)
!139 = !DILocation(line: 72, column: 3, scope: !38)
!140 = !DILocation(line: 73, column: 3, scope: !38)
!141 = !DILocation(line: 75, column: 3, scope: !38)
