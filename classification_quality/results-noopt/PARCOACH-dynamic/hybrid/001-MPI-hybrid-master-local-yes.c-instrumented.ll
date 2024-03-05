; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c.ll'
source_filename = "results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ident_t = type { i32, i32, i32, i32, ptr }
%struct.ompi_predefined_datatype_t = type opaque

@.str = private unnamed_addr constant [35 x i8] c"MPI_THREAD_MULTIPLE not supported\0A\00", align 1, !dbg !0
@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str.1 = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !7
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@0 = private unnamed_addr constant [95 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c;main;64;1;;\00", align 1
@1 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 94, ptr @0 }, align 8
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@.str.2 = private unnamed_addr constant [19 x i8] c"win_base[0] is %d\0A\00", align 1, !dbg !12
@2 = private unnamed_addr constant [96 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c;main;62;22;;\00", align 1
@3 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 95, ptr @2 }, align 8
@4 = private unnamed_addr constant [95 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c;main;62;1;;\00", align 1
@5 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 94, ptr @4 }, align 8
@.str.3 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !17
@6 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@7 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@8 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@9 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@10 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@11 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@12 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@13 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@14 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@15 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@16 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@17 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1
@18 = private unnamed_addr constant [82 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c\00", align 1

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
  %16 = alloca i32, align 4
  %17 = call i32 @__kmpc_global_thread_num(ptr @3)
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !60, metadata !DIExpression()), !dbg !61
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !62, metadata !DIExpression()), !dbg !63
  call void @llvm.dbg.declare(metadata ptr %6, metadata !64, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.declare(metadata ptr %7, metadata !66, metadata !DIExpression()), !dbg !67
  call void @llvm.dbg.declare(metadata ptr %8, metadata !68, metadata !DIExpression()), !dbg !72
  call void @llvm.dbg.declare(metadata ptr %9, metadata !73, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.declare(metadata ptr %10, metadata !76, metadata !DIExpression()), !dbg !77
  store i32 1, ptr %10, align 4, !dbg !77
  call void @llvm.dbg.declare(metadata ptr %11, metadata !78, metadata !DIExpression()), !dbg !79
  store i32 2, ptr %11, align 4, !dbg !79
  call void @llvm.dbg.declare(metadata ptr %12, metadata !80, metadata !DIExpression()), !dbg !81
  store ptr %10, ptr %12, align 8, !dbg !81
  call void @llvm.dbg.declare(metadata ptr %13, metadata !82, metadata !DIExpression()), !dbg !83
  call void @llvm.dbg.declare(metadata ptr %14, metadata !84, metadata !DIExpression()), !dbg !85
  store i32 42, ptr %14, align 4, !dbg !85
  call void @llvm.dbg.declare(metadata ptr %15, metadata !86, metadata !DIExpression()), !dbg !87
  %18 = call i32 @MPI_Init_thread(ptr noundef %4, ptr noundef %5, i32 noundef 3, ptr noundef %15), !dbg !88
  %19 = load i32, ptr %15, align 4, !dbg !89
  %20 = icmp slt i32 %19, 3, !dbg !91
  br i1 %20, label %21, label %24, !dbg !92

21:                                               ; preds = %2
  %22 = call i32 (ptr, ...) @printf(ptr noundef @.str), !dbg !93
  %23 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !95
  br label %24, !dbg !96

24:                                               ; preds = %21, %2
  %25 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !97
  %26 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !98
  %27 = load i32, ptr %7, align 4, !dbg !99
  %28 = icmp ne i32 %27, 2, !dbg !101
  br i1 %28, label %29, label %33, !dbg !102

29:                                               ; preds = %24
  %30 = load i32, ptr %7, align 4, !dbg !103
  %31 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %30, i32 noundef 2), !dbg !105
  %32 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !106
  br label %33, !dbg !107

33:                                               ; preds = %29, %24
  %34 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 4, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 53, ptr @6), !dbg !108
  call void @llvm.dbg.declare(metadata ptr %16, metadata !109, metadata !DIExpression()), !dbg !111
  store i32 0, ptr %16, align 4, !dbg !111
  br label %35, !dbg !112

35:                                               ; preds = %43, %33
  %36 = load i32, ptr %16, align 4, !dbg !113
  %37 = icmp slt i32 %36, 10, !dbg !115
  br i1 %37, label %38, label %46, !dbg !116

38:                                               ; preds = %35
  %39 = load ptr, ptr %9, align 8, !dbg !117
  %40 = load i32, ptr %16, align 4, !dbg !119
  %41 = sext i32 %40 to i64, !dbg !117
  %42 = getelementptr inbounds i32, ptr %39, i64 %41, !dbg !117
  store i32 0, ptr %42, align 4, !dbg !120
  br label %43, !dbg !121

43:                                               ; preds = %38
  %44 = load i32, ptr %16, align 4, !dbg !122
  %45 = add nsw i32 %44, 1, !dbg !122
  store i32 %45, ptr %16, align 4, !dbg !122
  br label %35, !dbg !123, !llvm.loop !124

46:                                               ; preds = %35
  %47 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 59, ptr @7), !dbg !127
  %48 = load i32, ptr %6, align 4, !dbg !128
  %49 = icmp eq i32 %48, 0, !dbg !130
  br i1 %49, label %50, label %52, !dbg !131

50:                                               ; preds = %46
  call void @__kmpc_push_num_threads(ptr @3, i32 %17, i32 2), !dbg !132
  call void (ptr, i32, ptr, ...) @__kmpc_fork_call(ptr @5, i32 2, ptr @.omp_outlined., ptr %8, ptr %9), !dbg !132
  %51 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 75, ptr @8), !dbg !134
  br label %52, !dbg !135

52:                                               ; preds = %50, %46
  %53 = load i32, ptr %6, align 4, !dbg !136
  %54 = icmp eq i32 %53, 1, !dbg !138
  br i1 %54, label %55, label %61, !dbg !139

55:                                               ; preds = %52
  %56 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 79, ptr @9), !dbg !140
  %57 = load ptr, ptr %9, align 8, !dbg !142
  %58 = getelementptr inbounds i32, ptr %57, i64 0, !dbg !142
  %59 = load i32, ptr %58, align 4, !dbg !142
  %60 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %59), !dbg !143
  br label %61, !dbg !144

61:                                               ; preds = %55, %52
  %62 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 83, ptr @10), !dbg !145
  %63 = load i32, ptr %6, align 4, !dbg !146
  %64 = load ptr, ptr %12, align 8, !dbg !147
  %65 = load i32, ptr %64, align 4, !dbg !148
  %66 = load i32, ptr %11, align 4, !dbg !149
  %67 = load ptr, ptr %9, align 8, !dbg !150
  %68 = getelementptr inbounds i32, ptr %67, i64 0, !dbg !150
  %69 = load i32, ptr %68, align 4, !dbg !150
  %70 = call i32 (ptr, ...) @printf(ptr noundef @.str.3, i32 noundef %63, i32 noundef %65, i32 noundef %66, i32 noundef %69), !dbg !151
  %71 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 88, ptr @11), !dbg !152
  %72 = call i32 @MPI_Finalize(), !dbg !153
  ret i32 0, !dbg !154
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @MPI_Init_thread(ptr noundef, ptr noundef, i32 noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

; Function Attrs: noinline norecurse nounwind optnone uwtable
define internal void @.omp_outlined._debug__(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 8 dereferenceable(8) %2, ptr noundef nonnull align 8 dereferenceable(8) %3) #3 !dbg !155 {
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !164, metadata !DIExpression()), !dbg !165
  store ptr %1, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !166, metadata !DIExpression()), !dbg !165
  store ptr %2, ptr %7, align 8
  call void @llvm.dbg.declare(metadata ptr %7, metadata !167, metadata !DIExpression()), !dbg !168
  store ptr %3, ptr %8, align 8
  call void @llvm.dbg.declare(metadata ptr %8, metadata !169, metadata !DIExpression()), !dbg !170
  %9 = load ptr, ptr %7, align 8, !dbg !171
  %10 = load ptr, ptr %8, align 8, !dbg !171
  %11 = load ptr, ptr %5, align 8, !dbg !172
  %12 = load i32, ptr %11, align 4, !dbg !172
  %13 = call i32 @__kmpc_master(ptr @1, i32 %12), !dbg !172
  %14 = icmp ne i32 %13, 0, !dbg !172
  br i1 %14, label %15, label %24, !dbg !172

15:                                               ; preds = %4
  %16 = load ptr, ptr %9, align 8, !dbg !175
  %17 = call i32 @parcoach_rma_MPI_Win_lock(i32 1, i32 1, i32 0, ptr %16, i32 66, ptr @12), !dbg !177
  call void @parcoach_rma_load(ptr %10, i64 64, i32 68, ptr @13), !dbg !178
  %18 = load ptr, ptr %10, align 8, !dbg !178
  %19 = getelementptr inbounds i32, ptr %18, i64 0, !dbg !178
  call void @parcoach_rma_load(ptr %9, i64 64, i32 68, ptr @14), !dbg !179
  %20 = load ptr, ptr %9, align 8, !dbg !179
  %21 = call i32 @parcoach_rma_MPI_Get(ptr %19, i32 1, ptr @ompi_mpi_int, i32 1, i64 0, i32 1, ptr @ompi_mpi_int, ptr %20, i32 68, ptr @15), !dbg !180
  %22 = load ptr, ptr %9, align 8, !dbg !181
  %23 = call i32 @parcoach_rma_MPI_Win_unlock(i32 1, ptr %22, i32 69, ptr @16), !dbg !182
  call void @__kmpc_end_master(ptr @1, i32 %12), !dbg !183
  br label %24, !dbg !183

24:                                               ; preds = %15, %4
  call void @parcoach_rma_load(ptr %10, i64 64, i32 73, ptr @17), !dbg !184
  %25 = load ptr, ptr %10, align 8, !dbg !184
  %26 = getelementptr inbounds i32, ptr %25, i64 0, !dbg !184
  call void @parcoach_rma_load(ptr %26, i64 32, i32 73, ptr @18), !dbg !184
  %27 = load i32, ptr %26, align 4, !dbg !184
  %28 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %27), !dbg !185
  ret void, !dbg !186
}

; Function Attrs: nounwind
declare i32 @__kmpc_master(ptr, i32) #4

; Function Attrs: nounwind
declare void @__kmpc_end_master(ptr, i32) #4

declare i32 @MPI_Win_lock(i32 noundef, i32 noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Get(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_unlock(i32 noundef, ptr noundef) #2

; Function Attrs: noinline norecurse nounwind optnone uwtable
define internal void @.omp_outlined.(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 8 dereferenceable(8) %2, ptr noundef nonnull align 8 dereferenceable(8) %3) #3 !dbg !187 {
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !188, metadata !DIExpression()), !dbg !189
  store ptr %1, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !190, metadata !DIExpression()), !dbg !189
  store ptr %2, ptr %7, align 8
  call void @llvm.dbg.declare(metadata ptr %7, metadata !191, metadata !DIExpression()), !dbg !189
  store ptr %3, ptr %8, align 8
  call void @llvm.dbg.declare(metadata ptr %8, metadata !192, metadata !DIExpression()), !dbg !189
  %9 = load ptr, ptr %7, align 8, !dbg !193
  %10 = load ptr, ptr %8, align 8, !dbg !193
  %11 = load ptr, ptr %5, align 8, !dbg !193
  %12 = load ptr, ptr %6, align 8, !dbg !193
  %13 = load ptr, ptr %7, align 8, !dbg !193
  %14 = load ptr, ptr %8, align 8, !dbg !193
  call void @.omp_outlined._debug__(ptr %11, ptr %12, ptr %13, ptr %14) #4, !dbg !193
  ret void, !dbg !193
}

; Function Attrs: nounwind
declare i32 @__kmpc_global_thread_num(ptr) #4

; Function Attrs: nounwind
declare void @__kmpc_push_num_threads(ptr, i32, i32) #4

; Function Attrs: nounwind
declare !callback !194 void @__kmpc_fork_call(ptr, i32, ptr, ...) #4

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

declare i32 @parcoach_rma_MPI_Win_allocate(i64, i32, ptr, ptr, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Barrier(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_free(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_lock(i32, i32, i32, ptr, i32, ptr)

declare void @parcoach_rma_load(ptr, i64, i32, ptr)

declare i32 @parcoach_rma_MPI_Get(ptr, i32, ptr, i32, i64, i32, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_unlock(i32, ptr, i32, ptr)

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noinline norecurse nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind }

!llvm.dbg.cu = !{!22}
!llvm.module.flags = !{!44, !45, !46, !47, !48, !49, !50, !51}
!llvm.ident = !{!52}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 42, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/hybrid/001-MPI-hybrid-master-local-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "91cd2ecaaa3012551c8014afe8a4c319")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 280, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 35)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 49, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 49)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 73, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 152, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 19)
!17 = !DIGlobalVariableExpression(var: !18, expr: !DIExpression())
!18 = distinct !DIGlobalVariable(scope: null, file: !2, line: 84, type: !19, isLocal: true, isDefinition: true)
!19 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !20)
!20 = !{!21}
!21 = !DISubrange(count: 94)
!22 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !23, retainedTypes: !32, globals: !43, splitDebugInlining: false, nameTableKind: None)
!23 = !{!24}
!24 = !DICompositeType(tag: DW_TAG_enumeration_type, file: !25, line: 708, baseType: !26, size: 32, elements: !27)
!25 = !DIFile(filename: "/usr/lib/x86_64-linux-gnu/openmpi/include/mpi.h", directory: "", checksumkind: CSK_MD5, checksum: "c3ca5be6f2c6a6e16d01846df47c0b49")
!26 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!27 = !{!28, !29, !30, !31}
!28 = !DIEnumerator(name: "MPI_THREAD_SINGLE", value: 0)
!29 = !DIEnumerator(name: "MPI_THREAD_FUNNELED", value: 1)
!30 = !DIEnumerator(name: "MPI_THREAD_SERIALIZED", value: 2)
!31 = !DIEnumerator(name: "MPI_THREAD_MULTIPLE", value: 3)
!32 = !{!33, !36, !37, !40}
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Comm", file: !25, line: 419, baseType: !34)
!34 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !35, size: 64)
!35 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_communicator_t", file: !25, line: 419, flags: DIFlagFwdDecl)
!36 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Info", file: !25, line: 424, baseType: !38)
!38 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !39, size: 64)
!39 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_info_t", file: !25, line: 424, flags: DIFlagFwdDecl)
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Datatype", file: !25, line: 420, baseType: !41)
!41 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !42, size: 64)
!42 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_datatype_t", file: !25, line: 420, flags: DIFlagFwdDecl)
!43 = !{!0, !7, !12, !17}
!44 = !{i32 7, !"Dwarf Version", i32 5}
!45 = !{i32 2, !"Debug Info Version", i32 3}
!46 = !{i32 1, !"wchar_size", i32 4}
!47 = !{i32 7, !"openmp", i32 50}
!48 = !{i32 7, !"PIC Level", i32 2}
!49 = !{i32 7, !"PIE Level", i32 2}
!50 = !{i32 7, !"uwtable", i32 2}
!51 = !{i32 7, !"frame-pointer", i32 2}
!52 = !{!"Debian clang version 15.0.6"}
!53 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 30, type: !54, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !59)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !56, !57}
!56 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!57 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !58, size: 64)
!58 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!59 = !{}
!60 = !DILocalVariable(name: "argc", arg: 1, scope: !53, file: !2, line: 30, type: !56)
!61 = !DILocation(line: 30, column: 14, scope: !53)
!62 = !DILocalVariable(name: "argv", arg: 2, scope: !53, file: !2, line: 30, type: !57)
!63 = !DILocation(line: 30, column: 27, scope: !53)
!64 = !DILocalVariable(name: "rank", scope: !53, file: !2, line: 31, type: !56)
!65 = !DILocation(line: 31, column: 7, scope: !53)
!66 = !DILocalVariable(name: "size", scope: !53, file: !2, line: 31, type: !56)
!67 = !DILocation(line: 31, column: 13, scope: !53)
!68 = !DILocalVariable(name: "win", scope: !53, file: !2, line: 32, type: !69)
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !25, line: 429, baseType: !70)
!70 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !71, size: 64)
!71 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !25, line: 429, flags: DIFlagFwdDecl)
!72 = !DILocation(line: 32, column: 11, scope: !53)
!73 = !DILocalVariable(name: "win_base", scope: !53, file: !2, line: 33, type: !74)
!74 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !56, size: 64)
!75 = !DILocation(line: 33, column: 8, scope: !53)
!76 = !DILocalVariable(name: "value", scope: !53, file: !2, line: 34, type: !56)
!77 = !DILocation(line: 34, column: 7, scope: !53)
!78 = !DILocalVariable(name: "value2", scope: !53, file: !2, line: 34, type: !56)
!79 = !DILocation(line: 34, column: 18, scope: !53)
!80 = !DILocalVariable(name: "buf", scope: !53, file: !2, line: 35, type: !74)
!81 = !DILocation(line: 35, column: 8, scope: !53)
!82 = !DILocalVariable(name: "result", scope: !53, file: !2, line: 36, type: !56)
!83 = !DILocation(line: 36, column: 7, scope: !53)
!84 = !DILocalVariable(name: "token", scope: !53, file: !2, line: 37, type: !56)
!85 = !DILocation(line: 37, column: 7, scope: !53)
!86 = !DILocalVariable(name: "provided", scope: !53, file: !2, line: 39, type: !56)
!87 = !DILocation(line: 39, column: 7, scope: !53)
!88 = !DILocation(line: 40, column: 3, scope: !53)
!89 = !DILocation(line: 41, column: 7, scope: !90)
!90 = distinct !DILexicalBlock(scope: !53, file: !2, line: 41, column: 7)
!91 = !DILocation(line: 41, column: 16, scope: !90)
!92 = !DILocation(line: 41, column: 7, scope: !53)
!93 = !DILocation(line: 42, column: 5, scope: !94)
!94 = distinct !DILexicalBlock(scope: !90, file: !2, line: 41, column: 39)
!95 = !DILocation(line: 43, column: 5, scope: !94)
!96 = !DILocation(line: 44, column: 3, scope: !94)
!97 = !DILocation(line: 45, column: 3, scope: !53)
!98 = !DILocation(line: 46, column: 3, scope: !53)
!99 = !DILocation(line: 48, column: 7, scope: !100)
!100 = distinct !DILexicalBlock(scope: !53, file: !2, line: 48, column: 7)
!101 = !DILocation(line: 48, column: 12, scope: !100)
!102 = !DILocation(line: 48, column: 7, scope: !53)
!103 = !DILocation(line: 49, column: 65, scope: !104)
!104 = distinct !DILexicalBlock(scope: !100, file: !2, line: 48, column: 25)
!105 = !DILocation(line: 49, column: 5, scope: !104)
!106 = !DILocation(line: 50, column: 5, scope: !104)
!107 = !DILocation(line: 51, column: 3, scope: !104)
!108 = !DILocation(line: 53, column: 3, scope: !53)
!109 = !DILocalVariable(name: "i", scope: !110, file: !2, line: 55, type: !56)
!110 = distinct !DILexicalBlock(scope: !53, file: !2, line: 55, column: 3)
!111 = !DILocation(line: 55, column: 12, scope: !110)
!112 = !DILocation(line: 55, column: 8, scope: !110)
!113 = !DILocation(line: 55, column: 19, scope: !114)
!114 = distinct !DILexicalBlock(scope: !110, file: !2, line: 55, column: 3)
!115 = !DILocation(line: 55, column: 21, scope: !114)
!116 = !DILocation(line: 55, column: 3, scope: !110)
!117 = !DILocation(line: 56, column: 5, scope: !118)
!118 = distinct !DILexicalBlock(scope: !114, file: !2, line: 55, column: 38)
!119 = !DILocation(line: 56, column: 14, scope: !118)
!120 = !DILocation(line: 56, column: 17, scope: !118)
!121 = !DILocation(line: 57, column: 3, scope: !118)
!122 = !DILocation(line: 55, column: 34, scope: !114)
!123 = !DILocation(line: 55, column: 3, scope: !114)
!124 = distinct !{!124, !116, !125, !126}
!125 = !DILocation(line: 57, column: 3, scope: !110)
!126 = !{!"llvm.loop.mustprogress"}
!127 = !DILocation(line: 59, column: 3, scope: !53)
!128 = !DILocation(line: 61, column: 7, scope: !129)
!129 = distinct !DILexicalBlock(scope: !53, file: !2, line: 61, column: 7)
!130 = !DILocation(line: 61, column: 12, scope: !129)
!131 = !DILocation(line: 61, column: 7, scope: !53)
!132 = !DILocation(line: 62, column: 1, scope: !133)
!133 = distinct !DILexicalBlock(scope: !129, file: !2, line: 61, column: 18)
!134 = !DILocation(line: 75, column: 5, scope: !133)
!135 = !DILocation(line: 76, column: 3, scope: !133)
!136 = !DILocation(line: 78, column: 7, scope: !137)
!137 = distinct !DILexicalBlock(scope: !53, file: !2, line: 78, column: 7)
!138 = !DILocation(line: 78, column: 12, scope: !137)
!139 = !DILocation(line: 78, column: 7, scope: !53)
!140 = !DILocation(line: 79, column: 5, scope: !141)
!141 = distinct !DILexicalBlock(scope: !137, file: !2, line: 78, column: 18)
!142 = !DILocation(line: 80, column: 35, scope: !141)
!143 = !DILocation(line: 80, column: 5, scope: !141)
!144 = !DILocation(line: 81, column: 3, scope: !141)
!145 = !DILocation(line: 83, column: 3, scope: !53)
!146 = !DILocation(line: 86, column: 10, scope: !53)
!147 = !DILocation(line: 86, column: 17, scope: !53)
!148 = !DILocation(line: 86, column: 16, scope: !53)
!149 = !DILocation(line: 86, column: 22, scope: !53)
!150 = !DILocation(line: 86, column: 30, scope: !53)
!151 = !DILocation(line: 84, column: 3, scope: !53)
!152 = !DILocation(line: 88, column: 3, scope: !53)
!153 = !DILocation(line: 89, column: 3, scope: !53)
!154 = !DILocation(line: 91, column: 3, scope: !53)
!155 = distinct !DISubprogram(name: ".omp_outlined._debug__", scope: !2, file: !2, line: 63, type: !156, scopeLine: 63, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !59)
!156 = !DISubroutineType(types: !157)
!157 = !{null, !158, !158, !162, !163}
!158 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !159)
!159 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !160)
!160 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !161, size: 64)
!161 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !56)
!162 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !69, size: 64)
!163 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !74, size: 64)
!164 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !155, type: !158, flags: DIFlagArtificial)
!165 = !DILocation(line: 0, scope: !155)
!166 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !155, type: !158, flags: DIFlagArtificial)
!167 = !DILocalVariable(name: "win", arg: 3, scope: !155, file: !2, line: 32, type: !162)
!168 = !DILocation(line: 32, column: 11, scope: !155)
!169 = !DILocalVariable(name: "win_base", arg: 4, scope: !155, file: !2, line: 33, type: !163)
!170 = !DILocation(line: 33, column: 8, scope: !155)
!171 = !DILocation(line: 63, column: 5, scope: !155)
!172 = !DILocation(line: 64, column: 1, scope: !173)
!173 = distinct !DILexicalBlock(scope: !174, file: !2, line: 64, column: 1)
!174 = distinct !DILexicalBlock(scope: !155, file: !2, line: 63, column: 5)
!175 = !DILocation(line: 66, column: 48, scope: !176)
!176 = distinct !DILexicalBlock(scope: !173, file: !2, line: 65, column: 7)
!177 = !DILocation(line: 66, column: 9, scope: !176)
!178 = !DILocation(line: 68, column: 18, scope: !176)
!179 = !DILocation(line: 68, column: 61, scope: !176)
!180 = !DILocation(line: 68, column: 9, scope: !176)
!181 = !DILocation(line: 69, column: 27, scope: !176)
!182 = !DILocation(line: 69, column: 9, scope: !176)
!183 = !DILocation(line: 70, column: 7, scope: !176)
!184 = !DILocation(line: 73, column: 37, scope: !174)
!185 = !DILocation(line: 73, column: 7, scope: !174)
!186 = !DILocation(line: 74, column: 5, scope: !155)
!187 = distinct !DISubprogram(name: ".omp_outlined.", scope: !2, file: !2, line: 62, type: !156, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !59)
!188 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !187, type: !158, flags: DIFlagArtificial)
!189 = !DILocation(line: 0, scope: !187)
!190 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !187, type: !158, flags: DIFlagArtificial)
!191 = !DILocalVariable(name: "win", arg: 3, scope: !187, type: !162, flags: DIFlagArtificial)
!192 = !DILocalVariable(name: "win_base", arg: 4, scope: !187, type: !163, flags: DIFlagArtificial)
!193 = !DILocation(line: 62, column: 1, scope: !187)
!194 = !{!195}
!195 = !{i64 2, i64 -1, i64 -1, i1 true}
