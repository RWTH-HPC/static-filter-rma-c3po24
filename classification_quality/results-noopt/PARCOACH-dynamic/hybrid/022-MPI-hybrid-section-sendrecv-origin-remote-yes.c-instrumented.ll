; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c.ll'
source_filename = "results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c"
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
@0 = private unnamed_addr constant [113 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c;main;62;1;;\00", align 1
@1 = private unnamed_addr constant %struct.ident_t { i32 0, i32 1026, i32 0, i32 112, ptr @0 }, align 8
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@2 = private unnamed_addr constant [114 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c;main;62;21;;\00", align 1
@3 = private unnamed_addr constant %struct.ident_t { i32 0, i32 1026, i32 0, i32 113, ptr @2 }, align 8
@4 = private unnamed_addr constant %struct.ident_t { i32 0, i32 194, i32 0, i32 112, ptr @0 }, align 8
@5 = private unnamed_addr constant [114 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c;main;60;22;;\00", align 1
@6 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 113, ptr @5 }, align 8
@7 = private unnamed_addr constant [113 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c;main;60;1;;\00", align 1
@8 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 112, ptr @7 }, align 8
@.str.2 = private unnamed_addr constant [19 x i8] c"win_base[0] is %d\0A\00", align 1, !dbg !12
@.str.3 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !17
@9 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@10 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@11 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@12 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@13 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@14 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@15 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@16 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@17 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@18 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1
@19 = private unnamed_addr constant [100 x i8] c"results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !66 {
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
  %18 = call i32 @__kmpc_global_thread_num(ptr @6)
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !72, metadata !DIExpression()), !dbg !73
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !74, metadata !DIExpression()), !dbg !75
  call void @llvm.dbg.declare(metadata ptr %6, metadata !76, metadata !DIExpression()), !dbg !77
  call void @llvm.dbg.declare(metadata ptr %7, metadata !78, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.declare(metadata ptr %8, metadata !80, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.declare(metadata ptr %9, metadata !85, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.declare(metadata ptr %10, metadata !88, metadata !DIExpression()), !dbg !89
  store i32 1, ptr %10, align 4, !dbg !89
  call void @llvm.dbg.declare(metadata ptr %11, metadata !90, metadata !DIExpression()), !dbg !91
  store i32 2, ptr %11, align 4, !dbg !91
  call void @llvm.dbg.declare(metadata ptr %12, metadata !92, metadata !DIExpression()), !dbg !93
  store ptr %10, ptr %12, align 8, !dbg !93
  call void @llvm.dbg.declare(metadata ptr %13, metadata !94, metadata !DIExpression()), !dbg !95
  call void @llvm.dbg.declare(metadata ptr %14, metadata !96, metadata !DIExpression()), !dbg !97
  store i32 42, ptr %14, align 4, !dbg !97
  call void @llvm.dbg.declare(metadata ptr %15, metadata !98, metadata !DIExpression()), !dbg !99
  %19 = call i32 @MPI_Init_thread(ptr noundef %4, ptr noundef %5, i32 noundef 3, ptr noundef %15), !dbg !100
  %20 = load i32, ptr %15, align 4, !dbg !101
  %21 = icmp slt i32 %20, 3, !dbg !103
  br i1 %21, label %22, label %25, !dbg !104

22:                                               ; preds = %2
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str), !dbg !105
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !107
  br label %25, !dbg !108

25:                                               ; preds = %22, %2
  %26 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !109
  %27 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !110
  %28 = load i32, ptr %7, align 4, !dbg !111
  %29 = icmp ne i32 %28, 2, !dbg !113
  br i1 %29, label %30, label %34, !dbg !114

30:                                               ; preds = %25
  %31 = load i32, ptr %7, align 4, !dbg !115
  %32 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %31, i32 noundef 2), !dbg !117
  %33 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !118
  br label %34, !dbg !119

34:                                               ; preds = %30, %25
  %35 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 4, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 52, ptr @9), !dbg !120
  call void @llvm.dbg.declare(metadata ptr %16, metadata !121, metadata !DIExpression()), !dbg !123
  store i32 0, ptr %16, align 4, !dbg !123
  br label %36, !dbg !124

36:                                               ; preds = %44, %34
  %37 = load i32, ptr %16, align 4, !dbg !125
  %38 = icmp slt i32 %37, 10, !dbg !127
  br i1 %38, label %39, label %47, !dbg !128

39:                                               ; preds = %36
  %40 = load ptr, ptr %9, align 8, !dbg !129
  %41 = load i32, ptr %16, align 4, !dbg !131
  %42 = sext i32 %41 to i64, !dbg !129
  %43 = getelementptr inbounds i32, ptr %40, i64 %42, !dbg !129
  store i32 0, ptr %43, align 4, !dbg !132
  br label %44, !dbg !133

44:                                               ; preds = %39
  %45 = load i32, ptr %16, align 4, !dbg !134
  %46 = add nsw i32 %45, 1, !dbg !134
  store i32 %46, ptr %16, align 4, !dbg !134
  br label %36, !dbg !135, !llvm.loop !136

47:                                               ; preds = %36
  %48 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 57, ptr @10), !dbg !139
  %49 = load i32, ptr %6, align 4, !dbg !140
  %50 = icmp eq i32 %49, 0, !dbg !142
  br i1 %50, label %51, label %52, !dbg !143

51:                                               ; preds = %47
  call void @__kmpc_push_num_threads(ptr @6, i32 %18, i32 2), !dbg !144
  call void (ptr, i32, ptr, ...) @__kmpc_fork_call(ptr @8, i32 1, ptr @.omp_outlined., ptr %8), !dbg !144
  br label %52, !dbg !146

52:                                               ; preds = %51, %47
  %53 = load i32, ptr %6, align 4, !dbg !147
  %54 = icmp eq i32 %53, 1, !dbg !149
  br i1 %54, label %55, label %61, !dbg !150

55:                                               ; preds = %52
  call void @llvm.dbg.declare(metadata ptr %17, metadata !151, metadata !DIExpression()), !dbg !154
  %56 = call i32 @MPI_Recv(ptr noundef %17, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_comm_world, ptr noundef null), !dbg !155
  %57 = load ptr, ptr %9, align 8, !dbg !156
  %58 = getelementptr inbounds i32, ptr %57, i64 0, !dbg !156
  %59 = load i32, ptr %58, align 4, !dbg !156
  %60 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %59), !dbg !157
  br label %61, !dbg !158

61:                                               ; preds = %55, %52
  %62 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 93, ptr @11), !dbg !159
  %63 = load i32, ptr %6, align 4, !dbg !160
  %64 = load ptr, ptr %12, align 8, !dbg !161
  %65 = load i32, ptr %64, align 4, !dbg !162
  %66 = load i32, ptr %11, align 4, !dbg !163
  %67 = load ptr, ptr %9, align 8, !dbg !164
  %68 = getelementptr inbounds i32, ptr %67, i64 0, !dbg !164
  %69 = load i32, ptr %68, align 4, !dbg !164
  %70 = call i32 (ptr, ...) @printf(ptr noundef @.str.3, i32 noundef %63, i32 noundef %65, i32 noundef %66, i32 noundef %69), !dbg !165
  %71 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 98, ptr @12), !dbg !166
  %72 = call i32 @MPI_Finalize(), !dbg !167
  ret i32 0, !dbg !168
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
define internal void @.omp_outlined._debug__(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 8 dereferenceable(8) %2) #3 !dbg !169 {
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !177, metadata !DIExpression()), !dbg !178
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !179, metadata !DIExpression()), !dbg !178
  store ptr %2, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !180, metadata !DIExpression()), !dbg !181
  %14 = load ptr, ptr %6, align 8, !dbg !182
  store i32 0, ptr %7, align 4, !dbg !183
  store i32 1, ptr %8, align 4, !dbg !183
  store i32 1, ptr %9, align 4, !dbg !183
  store i32 0, ptr %10, align 4, !dbg !183
  %15 = load ptr, ptr %4, align 8, !dbg !183
  %16 = load i32, ptr %15, align 4, !dbg !183
  call void @__kmpc_for_static_init_4(ptr @1, i32 %16, i32 34, ptr %10, ptr %7, ptr %8, ptr %9, i32 1, i32 1), !dbg !185
  %17 = load i32, ptr %8, align 4, !dbg !183
  %18 = icmp slt i32 %17, 1, !dbg !183
  %19 = select i1 %18, i32 %17, i32 1, !dbg !183
  store i32 %19, ptr %8, align 4, !dbg !183
  %20 = load i32, ptr %7, align 4, !dbg !183
  store i32 %20, ptr %11, align 4, !dbg !183
  br label %21, !dbg !183

21:                                               ; preds = %38, %3
  %22 = load i32, ptr %11, align 4, !dbg !185
  %23 = load i32, ptr %8, align 4, !dbg !185
  %24 = icmp sle i32 %22, %23, !dbg !185
  br i1 %24, label %25, label %41, !dbg !183

25:                                               ; preds = %21
  %26 = load i32, ptr %11, align 4, !dbg !183
  switch i32 %26, label %37 [
    i32 0, label %27
    i32 1, label %34
  ], !dbg !183

27:                                               ; preds = %25
  call void @llvm.dbg.declare(metadata ptr %12, metadata !187, metadata !DIExpression()), !dbg !190
  store i32 42, ptr %12, align 4, !dbg !190
  %28 = load ptr, ptr %14, align 8, !dbg !191
  %29 = call i32 @parcoach_rma_MPI_Win_lock(i32 1, i32 1, i32 0, ptr %28, i32 67, ptr @13), !dbg !192
  call void @parcoach_rma_load(ptr %14, i64 64, i32 70, ptr @14), !dbg !193
  %30 = load ptr, ptr %14, align 8, !dbg !193
  %31 = call i32 @parcoach_rma_MPI_Put(ptr %12, i32 1, ptr @ompi_mpi_int, i32 1, i64 0, i32 1, ptr @ompi_mpi_int, ptr %30, i32 69, ptr @15), !dbg !194
  %32 = load ptr, ptr %14, align 8, !dbg !195
  %33 = call i32 @parcoach_rma_MPI_Win_unlock(i32 1, ptr %32, i32 71, ptr @16), !dbg !196
  br label %37, !dbg !197

34:                                               ; preds = %25
  %35 = call i32 (i32, ...) @sleep(i32 noundef 1), !dbg !198
  call void @llvm.dbg.declare(metadata ptr %13, metadata !201, metadata !DIExpression()), !dbg !202
  call void @parcoach_rma_store(ptr %13, i64 32, i32 77, ptr @17), !dbg !202
  store i32 1, ptr %13, align 4, !dbg !202
  %36 = call i32 @MPI_Send(ptr noundef %13, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i32 noundef 1, ptr noundef @ompi_mpi_comm_world), !dbg !203
  br label %37, !dbg !204

37:                                               ; preds = %34, %27, %25
  br label %38, !dbg !204

38:                                               ; preds = %37
  call void @parcoach_rma_load(ptr %11, i64 32, i32 62, ptr @18), !dbg !185
  %39 = load i32, ptr %11, align 4, !dbg !185
  %40 = add nsw i32 %39, 1, !dbg !185
  call void @parcoach_rma_store(ptr %11, i64 32, i32 62, ptr @19), !dbg !185
  store i32 %40, ptr %11, align 4, !dbg !185
  br label %21, !dbg !204, !llvm.loop !205

41:                                               ; preds = %21
  call void @__kmpc_for_static_fini(ptr @3, i32 %16), !dbg !206
  call void @__kmpc_barrier(ptr @4, i32 %16), !dbg !206
  ret void, !dbg !207
}

declare void @__kmpc_for_static_init_4(ptr, i32, i32, ptr, ptr, ptr, ptr, i32, i32)

declare i32 @MPI_Win_lock(i32 noundef, i32 noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_unlock(i32 noundef, ptr noundef) #2

declare i32 @sleep(...) #2

declare i32 @MPI_Send(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i32 noundef, ptr noundef) #2

; Function Attrs: nounwind
declare void @__kmpc_for_static_fini(ptr, i32) #4

; Function Attrs: convergent nounwind
declare void @__kmpc_barrier(ptr, i32) #5

; Function Attrs: noinline norecurse nounwind optnone uwtable
define internal void @.omp_outlined.(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 8 dereferenceable(8) %2) #3 !dbg !208 {
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !209, metadata !DIExpression()), !dbg !210
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !211, metadata !DIExpression()), !dbg !210
  store ptr %2, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !212, metadata !DIExpression()), !dbg !210
  %7 = load ptr, ptr %6, align 8, !dbg !213
  %8 = load ptr, ptr %4, align 8, !dbg !213
  %9 = load ptr, ptr %5, align 8, !dbg !213
  %10 = load ptr, ptr %6, align 8, !dbg !213
  call void @.omp_outlined._debug__(ptr %8, ptr %9, ptr %10) #4, !dbg !213
  ret void, !dbg !213
}

; Function Attrs: nounwind
declare i32 @__kmpc_global_thread_num(ptr) #4

; Function Attrs: nounwind
declare void @__kmpc_push_num_threads(ptr, i32, i32) #4

; Function Attrs: nounwind
declare !callback !214 void @__kmpc_fork_call(ptr, i32, ptr, ...) #4

declare i32 @MPI_Recv(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

declare i32 @parcoach_rma_MPI_Win_allocate(i64, i32, ptr, ptr, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Barrier(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_free(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_lock(i32, i32, i32, ptr, i32, ptr)

declare void @parcoach_rma_load(ptr, i64, i32, ptr)

declare i32 @parcoach_rma_MPI_Put(ptr, i32, ptr, i32, i64, i32, ptr, ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_unlock(i32, ptr, i32, ptr)

declare void @parcoach_rma_store(ptr, i64, i32, ptr)

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noinline norecurse nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { nounwind }
attributes #5 = { convergent nounwind }

!llvm.dbg.cu = !{!22}
!llvm.module.flags = !{!57, !58, !59, !60, !61, !62, !63, !64}
!llvm.ident = !{!65}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 41, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/hybrid/022-MPI-hybrid-section-sendrecv-origin-remote-yes.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "6422b769712da1a6117d9997fc5a0f1e")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 280, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 35)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 48, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 49)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 90, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 152, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 19)
!17 = !DIGlobalVariableExpression(var: !18, expr: !DIExpression())
!18 = distinct !DIGlobalVariable(scope: null, file: !2, line: 94, type: !19, isLocal: true, isDefinition: true)
!19 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 752, elements: !20)
!20 = !{!21}
!21 = !DISubrange(count: 94)
!22 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: "Debian clang version 15.0.6", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !23, retainedTypes: !32, globals: !56, splitDebugInlining: false, nameTableKind: None)
!23 = !{!24}
!24 = !DICompositeType(tag: DW_TAG_enumeration_type, file: !25, line: 708, baseType: !26, size: 32, elements: !27)
!25 = !DIFile(filename: "/usr/lib/x86_64-linux-gnu/openmpi/include/mpi.h", directory: "", checksumkind: CSK_MD5, checksum: "c3ca5be6f2c6a6e16d01846df47c0b49")
!26 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!27 = !{!28, !29, !30, !31}
!28 = !DIEnumerator(name: "MPI_THREAD_SINGLE", value: 0)
!29 = !DIEnumerator(name: "MPI_THREAD_FUNNELED", value: 1)
!30 = !DIEnumerator(name: "MPI_THREAD_SERIALIZED", value: 2)
!31 = !DIEnumerator(name: "MPI_THREAD_MULTIPLE", value: 3)
!32 = !{!33, !36, !37, !40, !43}
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
!43 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !44, size: 64)
!44 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Status", file: !25, line: 428, baseType: !45)
!45 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_status_public_t", file: !25, line: 438, size: 192, elements: !46)
!46 = !{!47, !49, !50, !51, !52}
!47 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_SOURCE", scope: !45, file: !25, line: 441, baseType: !48, size: 32)
!48 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!49 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_TAG", scope: !45, file: !25, line: 442, baseType: !48, size: 32, offset: 32)
!50 = !DIDerivedType(tag: DW_TAG_member, name: "MPI_ERROR", scope: !45, file: !25, line: 443, baseType: !48, size: 32, offset: 64)
!51 = !DIDerivedType(tag: DW_TAG_member, name: "_cancelled", scope: !45, file: !25, line: 448, baseType: !48, size: 32, offset: 96)
!52 = !DIDerivedType(tag: DW_TAG_member, name: "_ucount", scope: !45, file: !25, line: 449, baseType: !53, size: 64, offset: 128)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !54, line: 46, baseType: !55)
!54 = !DIFile(filename: "/usr/lib/llvm-15/lib/clang/15.0.6/include/stddef.h", directory: "", checksumkind: CSK_MD5, checksum: "b76978376d35d5cd171876ac58ac1256")
!55 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!56 = !{!0, !7, !12, !17}
!57 = !{i32 7, !"Dwarf Version", i32 5}
!58 = !{i32 2, !"Debug Info Version", i32 3}
!59 = !{i32 1, !"wchar_size", i32 4}
!60 = !{i32 7, !"openmp", i32 50}
!61 = !{i32 7, !"PIC Level", i32 2}
!62 = !{i32 7, !"PIE Level", i32 2}
!63 = !{i32 7, !"uwtable", i32 2}
!64 = !{i32 7, !"frame-pointer", i32 2}
!65 = !{!"Debian clang version 15.0.6"}
!66 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !67, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !71)
!67 = !DISubroutineType(types: !68)
!68 = !{!48, !48, !69}
!69 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !70, size: 64)
!70 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!71 = !{}
!72 = !DILocalVariable(name: "argc", arg: 1, scope: !66, file: !2, line: 29, type: !48)
!73 = !DILocation(line: 29, column: 14, scope: !66)
!74 = !DILocalVariable(name: "argv", arg: 2, scope: !66, file: !2, line: 29, type: !69)
!75 = !DILocation(line: 29, column: 27, scope: !66)
!76 = !DILocalVariable(name: "rank", scope: !66, file: !2, line: 30, type: !48)
!77 = !DILocation(line: 30, column: 7, scope: !66)
!78 = !DILocalVariable(name: "size", scope: !66, file: !2, line: 30, type: !48)
!79 = !DILocation(line: 30, column: 13, scope: !66)
!80 = !DILocalVariable(name: "win", scope: !66, file: !2, line: 31, type: !81)
!81 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !25, line: 429, baseType: !82)
!82 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !83, size: 64)
!83 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !25, line: 429, flags: DIFlagFwdDecl)
!84 = !DILocation(line: 31, column: 11, scope: !66)
!85 = !DILocalVariable(name: "win_base", scope: !66, file: !2, line: 32, type: !86)
!86 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !48, size: 64)
!87 = !DILocation(line: 32, column: 8, scope: !66)
!88 = !DILocalVariable(name: "value", scope: !66, file: !2, line: 33, type: !48)
!89 = !DILocation(line: 33, column: 7, scope: !66)
!90 = !DILocalVariable(name: "value2", scope: !66, file: !2, line: 33, type: !48)
!91 = !DILocation(line: 33, column: 18, scope: !66)
!92 = !DILocalVariable(name: "buf", scope: !66, file: !2, line: 34, type: !86)
!93 = !DILocation(line: 34, column: 8, scope: !66)
!94 = !DILocalVariable(name: "result", scope: !66, file: !2, line: 35, type: !48)
!95 = !DILocation(line: 35, column: 7, scope: !66)
!96 = !DILocalVariable(name: "token", scope: !66, file: !2, line: 36, type: !48)
!97 = !DILocation(line: 36, column: 7, scope: !66)
!98 = !DILocalVariable(name: "provided", scope: !66, file: !2, line: 38, type: !48)
!99 = !DILocation(line: 38, column: 7, scope: !66)
!100 = !DILocation(line: 39, column: 3, scope: !66)
!101 = !DILocation(line: 40, column: 7, scope: !102)
!102 = distinct !DILexicalBlock(scope: !66, file: !2, line: 40, column: 7)
!103 = !DILocation(line: 40, column: 16, scope: !102)
!104 = !DILocation(line: 40, column: 7, scope: !66)
!105 = !DILocation(line: 41, column: 5, scope: !106)
!106 = distinct !DILexicalBlock(scope: !102, file: !2, line: 40, column: 39)
!107 = !DILocation(line: 42, column: 5, scope: !106)
!108 = !DILocation(line: 43, column: 3, scope: !106)
!109 = !DILocation(line: 44, column: 3, scope: !66)
!110 = !DILocation(line: 45, column: 3, scope: !66)
!111 = !DILocation(line: 47, column: 7, scope: !112)
!112 = distinct !DILexicalBlock(scope: !66, file: !2, line: 47, column: 7)
!113 = !DILocation(line: 47, column: 12, scope: !112)
!114 = !DILocation(line: 47, column: 7, scope: !66)
!115 = !DILocation(line: 48, column: 65, scope: !116)
!116 = distinct !DILexicalBlock(scope: !112, file: !2, line: 47, column: 25)
!117 = !DILocation(line: 48, column: 5, scope: !116)
!118 = !DILocation(line: 49, column: 5, scope: !116)
!119 = !DILocation(line: 50, column: 3, scope: !116)
!120 = !DILocation(line: 52, column: 3, scope: !66)
!121 = !DILocalVariable(name: "i", scope: !122, file: !2, line: 54, type: !48)
!122 = distinct !DILexicalBlock(scope: !66, file: !2, line: 54, column: 3)
!123 = !DILocation(line: 54, column: 12, scope: !122)
!124 = !DILocation(line: 54, column: 8, scope: !122)
!125 = !DILocation(line: 54, column: 19, scope: !126)
!126 = distinct !DILexicalBlock(scope: !122, file: !2, line: 54, column: 3)
!127 = !DILocation(line: 54, column: 21, scope: !126)
!128 = !DILocation(line: 54, column: 3, scope: !122)
!129 = !DILocation(line: 55, column: 5, scope: !130)
!130 = distinct !DILexicalBlock(scope: !126, file: !2, line: 54, column: 38)
!131 = !DILocation(line: 55, column: 14, scope: !130)
!132 = !DILocation(line: 55, column: 17, scope: !130)
!133 = !DILocation(line: 56, column: 3, scope: !130)
!134 = !DILocation(line: 54, column: 34, scope: !126)
!135 = !DILocation(line: 54, column: 3, scope: !126)
!136 = distinct !{!136, !128, !137, !138}
!137 = !DILocation(line: 56, column: 3, scope: !122)
!138 = !{!"llvm.loop.mustprogress"}
!139 = !DILocation(line: 57, column: 3, scope: !66)
!140 = !DILocation(line: 59, column: 7, scope: !141)
!141 = distinct !DILexicalBlock(scope: !66, file: !2, line: 59, column: 7)
!142 = !DILocation(line: 59, column: 12, scope: !141)
!143 = !DILocation(line: 59, column: 7, scope: !66)
!144 = !DILocation(line: 60, column: 1, scope: !145)
!145 = distinct !DILexicalBlock(scope: !141, file: !2, line: 59, column: 18)
!146 = !DILocation(line: 82, column: 3, scope: !145)
!147 = !DILocation(line: 84, column: 7, scope: !148)
!148 = distinct !DILexicalBlock(scope: !66, file: !2, line: 84, column: 7)
!149 = !DILocation(line: 84, column: 12, scope: !148)
!150 = !DILocation(line: 84, column: 7, scope: !66)
!151 = !DILocalVariable(name: "dummy", scope: !152, file: !2, line: 86, type: !48)
!152 = distinct !DILexicalBlock(scope: !153, file: !2, line: 85, column: 5)
!153 = distinct !DILexicalBlock(scope: !148, file: !2, line: 84, column: 18)
!154 = !DILocation(line: 86, column: 11, scope: !152)
!155 = !DILocation(line: 87, column: 7, scope: !152)
!156 = !DILocation(line: 90, column: 35, scope: !153)
!157 = !DILocation(line: 90, column: 5, scope: !153)
!158 = !DILocation(line: 91, column: 3, scope: !153)
!159 = !DILocation(line: 93, column: 3, scope: !66)
!160 = !DILocation(line: 96, column: 10, scope: !66)
!161 = !DILocation(line: 96, column: 17, scope: !66)
!162 = !DILocation(line: 96, column: 16, scope: !66)
!163 = !DILocation(line: 96, column: 22, scope: !66)
!164 = !DILocation(line: 96, column: 30, scope: !66)
!165 = !DILocation(line: 94, column: 3, scope: !66)
!166 = !DILocation(line: 98, column: 3, scope: !66)
!167 = !DILocation(line: 99, column: 3, scope: !66)
!168 = !DILocation(line: 101, column: 3, scope: !66)
!169 = distinct !DISubprogram(name: ".omp_outlined._debug__", scope: !2, file: !2, line: 61, type: !170, scopeLine: 61, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !71)
!170 = !DISubroutineType(types: !171)
!171 = !{null, !172, !172, !176}
!172 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !173)
!173 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !174)
!174 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !175, size: 64)
!175 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !48)
!176 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !81, size: 64)
!177 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !169, type: !172, flags: DIFlagArtificial)
!178 = !DILocation(line: 0, scope: !169)
!179 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !169, type: !172, flags: DIFlagArtificial)
!180 = !DILocalVariable(name: "win", arg: 3, scope: !169, file: !2, line: 31, type: !176)
!181 = !DILocation(line: 31, column: 11, scope: !169)
!182 = !DILocation(line: 61, column: 5, scope: !169)
!183 = !DILocation(line: 62, column: 1, scope: !184)
!184 = distinct !DILexicalBlock(scope: !169, file: !2, line: 61, column: 5)
!185 = !DILocation(line: 62, column: 1, scope: !186)
!186 = distinct !DILexicalBlock(scope: !184, file: !2, line: 62, column: 1)
!187 = !DILocalVariable(name: "value", scope: !188, file: !2, line: 66, type: !48)
!188 = distinct !DILexicalBlock(scope: !189, file: !2, line: 65, column: 9)
!189 = distinct !DILexicalBlock(scope: !186, file: !2, line: 64, column: 1)
!190 = !DILocation(line: 66, column: 15, scope: !188)
!191 = !DILocation(line: 67, column: 50, scope: !188)
!192 = !DILocation(line: 67, column: 11, scope: !188)
!193 = !DILocation(line: 70, column: 19, scope: !188)
!194 = !DILocation(line: 69, column: 11, scope: !188)
!195 = !DILocation(line: 71, column: 29, scope: !188)
!196 = !DILocation(line: 71, column: 11, scope: !188)
!197 = !DILocation(line: 64, column: 20, scope: !189)
!198 = !DILocation(line: 76, column: 11, scope: !199)
!199 = distinct !DILexicalBlock(scope: !200, file: !2, line: 75, column: 9)
!200 = distinct !DILexicalBlock(scope: !186, file: !2, line: 74, column: 1)
!201 = !DILocalVariable(name: "dummy", scope: !199, file: !2, line: 77, type: !48)
!202 = !DILocation(line: 77, column: 15, scope: !199)
!203 = !DILocation(line: 78, column: 11, scope: !199)
!204 = !DILocation(line: 74, column: 20, scope: !200)
!205 = distinct !{!205, !185, !206}
!206 = !DILocation(line: 62, column: 21, scope: !186)
!207 = !DILocation(line: 81, column: 5, scope: !169)
!208 = distinct !DISubprogram(name: ".omp_outlined.", scope: !2, file: !2, line: 60, type: !170, scopeLine: 60, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !71)
!209 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !208, type: !172, flags: DIFlagArtificial)
!210 = !DILocation(line: 0, scope: !208)
!211 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !208, type: !172, flags: DIFlagArtificial)
!212 = !DILocalVariable(name: "win", arg: 3, scope: !208, type: !176, flags: DIFlagArtificial)
!213 = !DILocation(line: 60, column: 1, scope: !208)
!214 = !{!215}
!215 = !{i64 2, i64 -1, i64 -1, i1 true}
