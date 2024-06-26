; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c'
source_filename = "results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.ompi_predefined_communicator_t = type opaque
%struct.ompi_predefined_info_t = type opaque
%struct.ompi_predefined_datatype_t = type opaque
%struct.ident_t = type { i32, i32, i32, i32, ptr }
%struct.anon = type { ptr }
%struct.kmp_task_t_with_privates = type { %struct.kmp_task_t }
%struct.kmp_task_t = type { ptr, ptr, i32, %union.kmp_cmplrdata_t, %union.kmp_cmplrdata_t }
%union.kmp_cmplrdata_t = type { ptr }

@.str = private unnamed_addr constant [35 x i8] c"MPI_THREAD_MULTIPLE not supported\0A\00", align 1, !dbg !0
@ompi_mpi_comm_world = external global %struct.ompi_predefined_communicator_t, align 1
@.str.1 = private unnamed_addr constant [49 x i8] c"Wrong number of MPI processes: %d. Expected: %d\0A\00", align 1, !dbg !7
@ompi_mpi_info_null = external global %struct.ompi_predefined_info_t, align 1
@ompi_mpi_int = external global %struct.ompi_predefined_datatype_t, align 1
@0 = private unnamed_addr constant [93 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c;main;85;1;;\00", align 1
@1 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 92, ptr @0 }, align 8
@2 = private unnamed_addr constant [93 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c;main;87;1;;\00", align 1
@3 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 92, ptr @2 }, align 8
@4 = private unnamed_addr constant [93 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c;main;96;1;;\00", align 1
@5 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 92, ptr @4 }, align 8
@.str.2 = private unnamed_addr constant [19 x i8] c"win_base[0] is %d\0A\00", align 1, !dbg !12
@6 = private unnamed_addr constant %struct.ident_t { i32 0, i32 322, i32 0, i32 92, ptr @0 }, align 8
@7 = private unnamed_addr constant [94 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c;main;83;22;;\00", align 1
@8 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 93, ptr @7 }, align 8
@9 = private unnamed_addr constant [93 x i8] c";results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c;main;83;1;;\00", align 1
@10 = private unnamed_addr constant %struct.ident_t { i32 0, i32 2, i32 0, i32 92, ptr @9 }, align 8
@.str.4 = private unnamed_addr constant [94 x i8] c"Process %d: Execution finished, variable contents: value = %d, value2 = %d, win_base[0] = %d\0A\00", align 1, !dbg !17

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @my_signal(ptr noundef %0) #0 !dbg !53 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !59, metadata !DIExpression()), !dbg !60
  %3 = load ptr, ptr %2, align 8, !dbg !61
  %4 = atomicrmw add ptr %3, i32 1 monotonic, align 4, !dbg !63
  ret void, !dbg !64
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @my_wait(ptr noundef %0, i32 noundef %1) #0 !dbg !65 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !68, metadata !DIExpression()), !dbg !69
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !70, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.declare(metadata ptr %5, metadata !72, metadata !DIExpression()), !dbg !73
  store i32 0, ptr %5, align 4, !dbg !73
  br label %6, !dbg !74

6:                                                ; preds = %10, %2
  %7 = call i32 @usleep(i32 noundef 10), !dbg !75
  %8 = load ptr, ptr %3, align 8, !dbg !77
  %9 = load atomic i32, ptr %8 monotonic, align 4, !dbg !79
  store i32 %9, ptr %5, align 4, !dbg !79
  br label %10, !dbg !80

10:                                               ; preds = %6
  %11 = load i32, ptr %5, align 4, !dbg !81
  %12 = load i32, ptr %4, align 4, !dbg !82
  %13 = icmp slt i32 %11, %12, !dbg !83
  br i1 %13, label %6, label %14, !dbg !80, !llvm.loop !84

14:                                               ; preds = %10
  ret void, !dbg !87
}

declare i32 @usleep(i32 noundef) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !88 {
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
  %18 = call i32 @__kmpc_global_thread_num(ptr @8)
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !93, metadata !DIExpression()), !dbg !94
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !95, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.declare(metadata ptr %6, metadata !97, metadata !DIExpression()), !dbg !98
  call void @llvm.dbg.declare(metadata ptr %7, metadata !99, metadata !DIExpression()), !dbg !100
  call void @llvm.dbg.declare(metadata ptr %8, metadata !101, metadata !DIExpression()), !dbg !105
  call void @llvm.dbg.declare(metadata ptr %9, metadata !106, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.declare(metadata ptr %10, metadata !108, metadata !DIExpression()), !dbg !109
  store i32 1, ptr %10, align 4, !dbg !109
  call void @llvm.dbg.declare(metadata ptr %11, metadata !110, metadata !DIExpression()), !dbg !111
  store i32 2, ptr %11, align 4, !dbg !111
  call void @llvm.dbg.declare(metadata ptr %12, metadata !112, metadata !DIExpression()), !dbg !113
  store ptr %10, ptr %12, align 8, !dbg !113
  call void @llvm.dbg.declare(metadata ptr %13, metadata !114, metadata !DIExpression()), !dbg !115
  call void @llvm.dbg.declare(metadata ptr %14, metadata !116, metadata !DIExpression()), !dbg !117
  store i32 42, ptr %14, align 4, !dbg !117
  call void @llvm.dbg.declare(metadata ptr %15, metadata !118, metadata !DIExpression()), !dbg !119
  %19 = call i32 @MPI_Init_thread(ptr noundef %4, ptr noundef %5, i32 noundef 3, ptr noundef %15), !dbg !120
  %20 = load i32, ptr %15, align 4, !dbg !121
  %21 = icmp slt i32 %20, 3, !dbg !123
  br i1 %21, label %22, label %25, !dbg !124

22:                                               ; preds = %2
  %23 = call i32 (ptr, ...) @printf(ptr noundef @.str), !dbg !125
  %24 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !127
  br label %25, !dbg !128

25:                                               ; preds = %22, %2
  %26 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !129
  %27 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !130
  %28 = load i32, ptr %7, align 4, !dbg !131
  %29 = icmp ne i32 %28, 2, !dbg !133
  br i1 %29, label %30, label %34, !dbg !134

30:                                               ; preds = %25
  %31 = load i32, ptr %7, align 4, !dbg !135
  %32 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %31, i32 noundef 2), !dbg !137
  %33 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !138
  br label %34, !dbg !139

34:                                               ; preds = %30, %25
  %35 = call i32 @MPI_Win_allocate(i64 noundef 40, i32 noundef 4, ptr noundef @ompi_mpi_info_null, ptr noundef @ompi_mpi_comm_world, ptr noundef %9, ptr noundef %8), !dbg !140
  call void @llvm.dbg.declare(metadata ptr %16, metadata !141, metadata !DIExpression()), !dbg !143
  store i32 0, ptr %16, align 4, !dbg !143
  br label %36, !dbg !144

36:                                               ; preds = %44, %34
  %37 = load i32, ptr %16, align 4, !dbg !145
  %38 = icmp slt i32 %37, 10, !dbg !147
  br i1 %38, label %39, label %47, !dbg !148

39:                                               ; preds = %36
  %40 = load ptr, ptr %9, align 8, !dbg !149
  %41 = load i32, ptr %16, align 4, !dbg !151
  %42 = sext i32 %41 to i64, !dbg !149
  %43 = getelementptr inbounds i32, ptr %40, i64 %42, !dbg !149
  store i32 0, ptr %43, align 4, !dbg !152
  br label %44, !dbg !153

44:                                               ; preds = %39
  %45 = load i32, ptr %16, align 4, !dbg !154
  %46 = add nsw i32 %45, 1, !dbg !154
  store i32 %46, ptr %16, align 4, !dbg !154
  br label %36, !dbg !155, !llvm.loop !156

47:                                               ; preds = %36
  %48 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !158
  %49 = load i32, ptr %6, align 4, !dbg !159
  %50 = icmp eq i32 %49, 0, !dbg !161
  br i1 %50, label %51, label %59, !dbg !162

51:                                               ; preds = %47
  %52 = load ptr, ptr %8, align 8, !dbg !163
  %53 = call i32 @MPI_Win_lock(i32 noundef 1, i32 noundef 1, i32 noundef 0, ptr noundef %52), !dbg !165
  store i32 42, ptr %10, align 4, !dbg !166
  %54 = load ptr, ptr %8, align 8, !dbg !167
  %55 = call i32 @MPI_Put(ptr noundef %10, i32 noundef 1, ptr noundef @ompi_mpi_int, i32 noundef 1, i64 noundef 0, i32 noundef 1, ptr noundef @ompi_mpi_int, ptr noundef %54), !dbg !168
  %56 = load ptr, ptr %8, align 8, !dbg !169
  %57 = call i32 @MPI_Win_unlock(i32 noundef 1, ptr noundef %56), !dbg !170
  %58 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !171
  br label %59, !dbg !172

59:                                               ; preds = %51, %47
  %60 = load i32, ptr %6, align 4, !dbg !173
  %61 = icmp eq i32 %60, 1, !dbg !175
  br i1 %61, label %62, label %63, !dbg !176

62:                                               ; preds = %59
  call void @llvm.dbg.declare(metadata ptr %17, metadata !177, metadata !DIExpression()), !dbg !179
  store i32 0, ptr %17, align 4, !dbg !179
  call void @__kmpc_push_num_threads(ptr @8, i32 %18, i32 2), !dbg !180
  call void (ptr, i32, ptr, ...) @__kmpc_fork_call(ptr @10, i32 2, ptr @.omp_outlined..3, ptr %17, ptr %9), !dbg !180
  br label %63, !dbg !181

63:                                               ; preds = %62, %59
  %64 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world), !dbg !182
  %65 = load i32, ptr %6, align 4, !dbg !183
  %66 = load ptr, ptr %12, align 8, !dbg !184
  %67 = load i32, ptr %66, align 4, !dbg !185
  %68 = load i32, ptr %11, align 4, !dbg !186
  %69 = load ptr, ptr %9, align 8, !dbg !187
  %70 = getelementptr inbounds i32, ptr %69, i64 0, !dbg !187
  %71 = load i32, ptr %70, align 4, !dbg !187
  %72 = call i32 (ptr, ...) @printf(ptr noundef @.str.4, i32 noundef %65, i32 noundef %67, i32 noundef %68, i32 noundef %71), !dbg !188
  %73 = call i32 @MPI_Win_free(ptr noundef %8), !dbg !189
  %74 = call i32 @MPI_Finalize(), !dbg !190
  ret i32 0, !dbg !191
}

declare i32 @MPI_Init_thread(ptr noundef, ptr noundef, i32 noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Barrier(ptr noundef) #2

declare i32 @MPI_Win_lock(i32 noundef, i32 noundef, i32 noundef, ptr noundef) #2

declare i32 @MPI_Put(ptr noundef, i32 noundef, ptr noundef, i32 noundef, i64 noundef, i32 noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_unlock(i32 noundef, ptr noundef) #2

; Function Attrs: noinline norecurse nounwind optnone uwtable
define internal void @.omp_outlined._debug__(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 4 dereferenceable(4) %2, ptr noundef nonnull align 8 dereferenceable(8) %3) #3 !dbg !192 {
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  %9 = alloca %struct.anon, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !201, metadata !DIExpression()), !dbg !202
  store ptr %1, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !203, metadata !DIExpression()), !dbg !202
  store ptr %2, ptr %7, align 8
  call void @llvm.dbg.declare(metadata ptr %7, metadata !204, metadata !DIExpression()), !dbg !205
  store ptr %3, ptr %8, align 8
  call void @llvm.dbg.declare(metadata ptr %8, metadata !206, metadata !DIExpression()), !dbg !207
  %10 = load ptr, ptr %7, align 8, !dbg !208
  %11 = load ptr, ptr %8, align 8, !dbg !208
  %12 = load ptr, ptr %5, align 8, !dbg !209
  %13 = load i32, ptr %12, align 4, !dbg !209
  %14 = call i32 @__kmpc_single(ptr @1, i32 %13), !dbg !209
  %15 = icmp ne i32 %14, 0, !dbg !209
  br i1 %15, label %16, label %28, !dbg !209

16:                                               ; preds = %4
  %17 = getelementptr inbounds %struct.anon, ptr %9, i32 0, i32 0, !dbg !211
  store ptr %10, ptr %17, align 8, !dbg !211
  %18 = call ptr @__kmpc_omp_task_alloc(ptr @3, i32 %13, i32 1, i64 40, i64 8, ptr @.omp_task_entry.), !dbg !211
  %19 = getelementptr inbounds %struct.kmp_task_t_with_privates, ptr %18, i32 0, i32 0, !dbg !211
  %20 = getelementptr inbounds %struct.kmp_task_t, ptr %19, i32 0, i32 0, !dbg !211
  %21 = load ptr, ptr %20, align 8, !dbg !211
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %21, ptr align 8 %9, i64 8, i1 false), !dbg !211
  %22 = call i32 @__kmpc_omp_task(ptr @3, i32 %13, ptr %18), !dbg !211
  call void @my_wait(ptr noundef %10, i32 noundef 1), !dbg !214
  %23 = call i32 @__kmpc_omp_taskwait(ptr @5, i32 %13), !dbg !215
  %24 = load ptr, ptr %11, align 8, !dbg !216
  %25 = getelementptr inbounds i32, ptr %24, i64 0, !dbg !216
  %26 = load i32, ptr %25, align 4, !dbg !216
  %27 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %26), !dbg !217
  call void @__kmpc_end_single(ptr @1, i32 %13), !dbg !218
  br label %28, !dbg !218

28:                                               ; preds = %16, %4
  call void @__kmpc_barrier(ptr @6, i32 %13), !dbg !219
  ret void, !dbg !220
}

; Function Attrs: convergent nounwind
declare i32 @__kmpc_single(ptr, i32) #4

; Function Attrs: convergent nounwind
declare void @__kmpc_end_single(ptr, i32) #4

; Function Attrs: noinline norecurse nounwind uwtable
define internal i32 @.omp_task_entry.(i32 noundef %0, ptr noalias noundef %1) #5 !dbg !221 {
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  %9 = alloca i32, align 4
  %10 = alloca ptr, align 8
  store i32 %0, ptr %9, align 4
  call void @llvm.dbg.declare(metadata ptr %9, metadata !223, metadata !DIExpression()), !dbg !224
  store ptr %1, ptr %10, align 8
  call void @llvm.dbg.declare(metadata ptr %10, metadata !225, metadata !DIExpression()), !dbg !224
  %11 = load i32, ptr %9, align 4, !dbg !237
  %12 = load ptr, ptr %10, align 8, !dbg !237
  %13 = getelementptr inbounds %struct.kmp_task_t_with_privates, ptr %12, i32 0, i32 0, !dbg !237
  %14 = getelementptr inbounds %struct.kmp_task_t, ptr %13, i32 0, i32 2, !dbg !237
  %15 = getelementptr inbounds %struct.kmp_task_t, ptr %13, i32 0, i32 0, !dbg !237
  %16 = load ptr, ptr %15, align 8, !dbg !237
  call void @llvm.experimental.noalias.scope.decl(metadata !238), !dbg !237
  call void @llvm.experimental.noalias.scope.decl(metadata !241), !dbg !237
  call void @llvm.experimental.noalias.scope.decl(metadata !243), !dbg !237
  call void @llvm.experimental.noalias.scope.decl(metadata !245), !dbg !237
  store i32 %11, ptr %3, align 4, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %3, metadata !248, metadata !DIExpression()), !dbg !264
  store ptr %14, ptr %4, align 8, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %4, metadata !266, metadata !DIExpression()), !dbg !264
  store ptr null, ptr %5, align 8, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %5, metadata !267, metadata !DIExpression()), !dbg !264
  store ptr null, ptr %6, align 8, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %6, metadata !268, metadata !DIExpression()), !dbg !264
  store ptr %12, ptr %7, align 8, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %7, metadata !269, metadata !DIExpression()), !dbg !264
  store ptr %16, ptr %8, align 8, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %8, metadata !270, metadata !DIExpression()), !dbg !264
  %17 = load ptr, ptr %8, align 8, !dbg !271, !noalias !247
  call void @llvm.dbg.declare(metadata ptr %17, metadata !273, metadata !DIExpression(DW_OP_deref)), !dbg !274
  %18 = load ptr, ptr %17, align 8, !dbg !275
  call void @my_signal(ptr noundef %18), !dbg !277
  %19 = call i32 @MPI_Barrier(ptr noundef @ompi_mpi_comm_world) #6, !dbg !278
  ret i32 0, !dbg !237
}

; Function Attrs: nounwind
declare ptr @__kmpc_omp_task_alloc(ptr, i32, i32, i64, i64, ptr) #6

; Function Attrs: argmemonly nocallback nofree nounwind willreturn
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #7

; Function Attrs: nounwind
declare i32 @__kmpc_omp_task(ptr, i32, ptr) #6

; Function Attrs: convergent nounwind
declare i32 @__kmpc_omp_taskwait(ptr, i32) #4

; Function Attrs: convergent nounwind
declare void @__kmpc_barrier(ptr, i32) #4

; Function Attrs: noinline norecurse nounwind optnone uwtable
define internal void @.omp_outlined..3(ptr noalias noundef %0, ptr noalias noundef %1, ptr noundef nonnull align 4 dereferenceable(4) %2, ptr noundef nonnull align 8 dereferenceable(8) %3) #3 !dbg !279 {
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  store ptr %0, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !280, metadata !DIExpression()), !dbg !281
  store ptr %1, ptr %6, align 8
  call void @llvm.dbg.declare(metadata ptr %6, metadata !282, metadata !DIExpression()), !dbg !281
  store ptr %2, ptr %7, align 8
  call void @llvm.dbg.declare(metadata ptr %7, metadata !283, metadata !DIExpression()), !dbg !281
  store ptr %3, ptr %8, align 8
  call void @llvm.dbg.declare(metadata ptr %8, metadata !284, metadata !DIExpression()), !dbg !281
  %9 = load ptr, ptr %7, align 8, !dbg !285
  %10 = load ptr, ptr %8, align 8, !dbg !285
  %11 = load ptr, ptr %5, align 8, !dbg !285
  %12 = load ptr, ptr %6, align 8, !dbg !285
  %13 = load ptr, ptr %7, align 8, !dbg !285
  %14 = load ptr, ptr %8, align 8, !dbg !285
  call void @.omp_outlined._debug__(ptr %11, ptr %12, ptr %13, ptr %14) #6, !dbg !285
  ret void, !dbg !285
}

; Function Attrs: nounwind
declare i32 @__kmpc_global_thread_num(ptr) #6

; Function Attrs: nounwind
declare void @__kmpc_push_num_threads(ptr, i32, i32) #6

; Function Attrs: nounwind
declare !callback !286 void @__kmpc_fork_call(ptr, i32, ptr, ...) #6

declare i32 @MPI_Win_free(ptr noundef) #2

declare i32 @MPI_Finalize() #2

; Function Attrs: inaccessiblememonly nocallback nofree nosync nounwind willreturn
declare void @llvm.experimental.noalias.scope.decl(metadata) #8

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noinline norecurse nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { convergent nounwind }
attributes #5 = { noinline norecurse nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { nounwind }
attributes #7 = { argmemonly nocallback nofree nounwind willreturn }
attributes #8 = { inaccessiblememonly nocallback nofree nosync nounwind willreturn }

!llvm.dbg.cu = !{!22}
!llvm.module.flags = !{!44, !45, !46, !47, !48, !49, !50, !51}
!llvm.ident = !{!52}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 54, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "b670a9f0b2253d6a4f538b7b3dd01c6e")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 280, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 35)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 61, type: !9, isLocal: true, isDefinition: true)
!9 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !10)
!10 = !{!11}
!11 = !DISubrange(count: 49)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression())
!13 = distinct !DIGlobalVariable(scope: null, file: !2, line: 97, type: !14, isLocal: true, isDefinition: true)
!14 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 152, elements: !15)
!15 = !{!16}
!16 = !DISubrange(count: 19)
!17 = !DIGlobalVariableExpression(var: !18, expr: !DIExpression())
!18 = distinct !DIGlobalVariable(scope: null, file: !2, line: 103, type: !19, isLocal: true, isDefinition: true)
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
!53 = distinct !DISubprogram(name: "my_signal", scope: !2, file: !2, line: 25, type: !54, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !58)
!54 = !DISubroutineType(types: !55)
!55 = !{null, !56}
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !57, size: 64)
!57 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!58 = !{}
!59 = !DILocalVariable(name: "s", arg: 1, scope: !53, file: !2, line: 25, type: !56)
!60 = !DILocation(line: 25, column: 21, scope: !53)
!61 = !DILocation(line: 27, column: 5, scope: !62)
!62 = distinct !DILexicalBlock(scope: !53, file: !2, line: 26, column: 1)
!63 = !DILocation(line: 27, column: 3, scope: !62)
!64 = !DILocation(line: 28, column: 1, scope: !53)
!65 = distinct !DISubprogram(name: "my_wait", scope: !2, file: !2, line: 30, type: !66, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !58)
!66 = !DISubroutineType(types: !67)
!67 = !{null, !56, !57}
!68 = !DILocalVariable(name: "s", arg: 1, scope: !65, file: !2, line: 30, type: !56)
!69 = !DILocation(line: 30, column: 19, scope: !65)
!70 = !DILocalVariable(name: "v", arg: 2, scope: !65, file: !2, line: 30, type: !57)
!71 = !DILocation(line: 30, column: 26, scope: !65)
!72 = !DILocalVariable(name: "wait", scope: !65, file: !2, line: 31, type: !57)
!73 = !DILocation(line: 31, column: 7, scope: !65)
!74 = !DILocation(line: 32, column: 3, scope: !65)
!75 = !DILocation(line: 33, column: 5, scope: !76)
!76 = distinct !DILexicalBlock(scope: !65, file: !2, line: 32, column: 6)
!77 = !DILocation(line: 35, column: 14, scope: !78)
!78 = distinct !DILexicalBlock(scope: !76, file: !2, line: 34, column: 1)
!79 = !DILocation(line: 35, column: 5, scope: !78)
!80 = !DILocation(line: 36, column: 3, scope: !76)
!81 = !DILocation(line: 36, column: 12, scope: !65)
!82 = !DILocation(line: 36, column: 19, scope: !65)
!83 = !DILocation(line: 36, column: 17, scope: !65)
!84 = distinct !{!84, !74, !85, !86}
!85 = !DILocation(line: 36, column: 20, scope: !65)
!86 = !{!"llvm.loop.mustprogress"}
!87 = !DILocation(line: 37, column: 1, scope: !65)
!88 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 42, type: !89, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !22, retainedNodes: !58)
!89 = !DISubroutineType(types: !90)
!90 = !{!57, !57, !91}
!91 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !92, size: 64)
!92 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!93 = !DILocalVariable(name: "argc", arg: 1, scope: !88, file: !2, line: 42, type: !57)
!94 = !DILocation(line: 42, column: 14, scope: !88)
!95 = !DILocalVariable(name: "argv", arg: 2, scope: !88, file: !2, line: 42, type: !91)
!96 = !DILocation(line: 42, column: 27, scope: !88)
!97 = !DILocalVariable(name: "rank", scope: !88, file: !2, line: 43, type: !57)
!98 = !DILocation(line: 43, column: 7, scope: !88)
!99 = !DILocalVariable(name: "size", scope: !88, file: !2, line: 43, type: !57)
!100 = !DILocation(line: 43, column: 13, scope: !88)
!101 = !DILocalVariable(name: "win", scope: !88, file: !2, line: 44, type: !102)
!102 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !25, line: 429, baseType: !103)
!103 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !104, size: 64)
!104 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !25, line: 429, flags: DIFlagFwdDecl)
!105 = !DILocation(line: 44, column: 11, scope: !88)
!106 = !DILocalVariable(name: "win_base", scope: !88, file: !2, line: 45, type: !56)
!107 = !DILocation(line: 45, column: 8, scope: !88)
!108 = !DILocalVariable(name: "value", scope: !88, file: !2, line: 46, type: !57)
!109 = !DILocation(line: 46, column: 7, scope: !88)
!110 = !DILocalVariable(name: "value2", scope: !88, file: !2, line: 46, type: !57)
!111 = !DILocation(line: 46, column: 18, scope: !88)
!112 = !DILocalVariable(name: "buf", scope: !88, file: !2, line: 47, type: !56)
!113 = !DILocation(line: 47, column: 8, scope: !88)
!114 = !DILocalVariable(name: "result", scope: !88, file: !2, line: 48, type: !57)
!115 = !DILocation(line: 48, column: 7, scope: !88)
!116 = !DILocalVariable(name: "token", scope: !88, file: !2, line: 49, type: !57)
!117 = !DILocation(line: 49, column: 7, scope: !88)
!118 = !DILocalVariable(name: "provided", scope: !88, file: !2, line: 51, type: !57)
!119 = !DILocation(line: 51, column: 7, scope: !88)
!120 = !DILocation(line: 52, column: 3, scope: !88)
!121 = !DILocation(line: 53, column: 7, scope: !122)
!122 = distinct !DILexicalBlock(scope: !88, file: !2, line: 53, column: 7)
!123 = !DILocation(line: 53, column: 16, scope: !122)
!124 = !DILocation(line: 53, column: 7, scope: !88)
!125 = !DILocation(line: 54, column: 5, scope: !126)
!126 = distinct !DILexicalBlock(scope: !122, file: !2, line: 53, column: 39)
!127 = !DILocation(line: 55, column: 5, scope: !126)
!128 = !DILocation(line: 56, column: 3, scope: !126)
!129 = !DILocation(line: 57, column: 3, scope: !88)
!130 = !DILocation(line: 58, column: 3, scope: !88)
!131 = !DILocation(line: 60, column: 7, scope: !132)
!132 = distinct !DILexicalBlock(scope: !88, file: !2, line: 60, column: 7)
!133 = !DILocation(line: 60, column: 12, scope: !132)
!134 = !DILocation(line: 60, column: 7, scope: !88)
!135 = !DILocation(line: 61, column: 65, scope: !136)
!136 = distinct !DILexicalBlock(scope: !132, file: !2, line: 60, column: 25)
!137 = !DILocation(line: 61, column: 5, scope: !136)
!138 = !DILocation(line: 62, column: 5, scope: !136)
!139 = !DILocation(line: 63, column: 3, scope: !136)
!140 = !DILocation(line: 65, column: 3, scope: !88)
!141 = !DILocalVariable(name: "i", scope: !142, file: !2, line: 67, type: !57)
!142 = distinct !DILexicalBlock(scope: !88, file: !2, line: 67, column: 3)
!143 = !DILocation(line: 67, column: 12, scope: !142)
!144 = !DILocation(line: 67, column: 8, scope: !142)
!145 = !DILocation(line: 67, column: 19, scope: !146)
!146 = distinct !DILexicalBlock(scope: !142, file: !2, line: 67, column: 3)
!147 = !DILocation(line: 67, column: 21, scope: !146)
!148 = !DILocation(line: 67, column: 3, scope: !142)
!149 = !DILocation(line: 68, column: 5, scope: !150)
!150 = distinct !DILexicalBlock(scope: !146, file: !2, line: 67, column: 38)
!151 = !DILocation(line: 68, column: 14, scope: !150)
!152 = !DILocation(line: 68, column: 17, scope: !150)
!153 = !DILocation(line: 69, column: 3, scope: !150)
!154 = !DILocation(line: 67, column: 34, scope: !146)
!155 = !DILocation(line: 67, column: 3, scope: !146)
!156 = distinct !{!156, !148, !157, !86}
!157 = !DILocation(line: 69, column: 3, scope: !142)
!158 = !DILocation(line: 71, column: 3, scope: !88)
!159 = !DILocation(line: 73, column: 7, scope: !160)
!160 = distinct !DILexicalBlock(scope: !88, file: !2, line: 73, column: 7)
!161 = !DILocation(line: 73, column: 12, scope: !160)
!162 = !DILocation(line: 73, column: 7, scope: !88)
!163 = !DILocation(line: 74, column: 44, scope: !164)
!164 = distinct !DILexicalBlock(scope: !160, file: !2, line: 73, column: 18)
!165 = !DILocation(line: 74, column: 5, scope: !164)
!166 = !DILocation(line: 75, column: 11, scope: !164)
!167 = !DILocation(line: 76, column: 51, scope: !164)
!168 = !DILocation(line: 76, column: 5, scope: !164)
!169 = !DILocation(line: 77, column: 23, scope: !164)
!170 = !DILocation(line: 77, column: 5, scope: !164)
!171 = !DILocation(line: 78, column: 5, scope: !164)
!172 = !DILocation(line: 79, column: 3, scope: !164)
!173 = !DILocation(line: 81, column: 7, scope: !174)
!174 = distinct !DILexicalBlock(scope: !88, file: !2, line: 81, column: 7)
!175 = !DILocation(line: 81, column: 12, scope: !174)
!176 = !DILocation(line: 81, column: 7, scope: !88)
!177 = !DILocalVariable(name: "flag", scope: !178, file: !2, line: 82, type: !57)
!178 = distinct !DILexicalBlock(scope: !174, file: !2, line: 81, column: 18)
!179 = !DILocation(line: 82, column: 9, scope: !178)
!180 = !DILocation(line: 83, column: 1, scope: !178)
!181 = !DILocation(line: 100, column: 3, scope: !178)
!182 = !DILocation(line: 102, column: 3, scope: !88)
!183 = !DILocation(line: 105, column: 10, scope: !88)
!184 = !DILocation(line: 105, column: 17, scope: !88)
!185 = !DILocation(line: 105, column: 16, scope: !88)
!186 = !DILocation(line: 105, column: 22, scope: !88)
!187 = !DILocation(line: 105, column: 30, scope: !88)
!188 = !DILocation(line: 103, column: 3, scope: !88)
!189 = !DILocation(line: 107, column: 3, scope: !88)
!190 = !DILocation(line: 108, column: 3, scope: !88)
!191 = !DILocation(line: 110, column: 3, scope: !88)
!192 = distinct !DISubprogram(name: ".omp_outlined._debug__", scope: !2, file: !2, line: 84, type: !193, scopeLine: 84, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !58)
!193 = !DISubroutineType(types: !194)
!194 = !{null, !195, !195, !199, !200}
!195 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !196)
!196 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !197)
!197 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !198, size: 64)
!198 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !57)
!199 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !57, size: 64)
!200 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !56, size: 64)
!201 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !192, type: !195, flags: DIFlagArtificial)
!202 = !DILocation(line: 0, scope: !192)
!203 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !192, type: !195, flags: DIFlagArtificial)
!204 = !DILocalVariable(name: "flag", arg: 3, scope: !192, file: !2, line: 82, type: !199)
!205 = !DILocation(line: 82, column: 9, scope: !192)
!206 = !DILocalVariable(name: "win_base", arg: 4, scope: !192, file: !2, line: 45, type: !200)
!207 = !DILocation(line: 45, column: 8, scope: !192)
!208 = !DILocation(line: 84, column: 5, scope: !192)
!209 = !DILocation(line: 85, column: 1, scope: !210)
!210 = distinct !DILexicalBlock(scope: !192, file: !2, line: 84, column: 5)
!211 = !DILocation(line: 87, column: 1, scope: !212)
!212 = distinct !DILexicalBlock(scope: !213, file: !2, line: 86, column: 7)
!213 = distinct !DILexicalBlock(scope: !210, file: !2, line: 85, column: 1)
!214 = !DILocation(line: 95, column: 9, scope: !212)
!215 = !DILocation(line: 96, column: 1, scope: !212)
!216 = !DILocation(line: 97, column: 39, scope: !212)
!217 = !DILocation(line: 97, column: 9, scope: !212)
!218 = !DILocation(line: 98, column: 7, scope: !212)
!219 = !DILocation(line: 85, column: 19, scope: !213)
!220 = !DILocation(line: 99, column: 5, scope: !192)
!221 = distinct !DISubprogram(linkageName: ".omp_task_entry.", scope: !2, file: !2, line: 87, type: !222, scopeLine: 87, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !58)
!222 = !DISubroutineType(types: !58)
!223 = !DILocalVariable(arg: 1, scope: !221, type: !57, flags: DIFlagArtificial)
!224 = !DILocation(line: 0, scope: !221)
!225 = !DILocalVariable(arg: 2, scope: !221, type: !226, flags: DIFlagArtificial)
!226 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !227)
!227 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !228, size: 64)
!228 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "kmp_task_t_with_privates", size: 320, elements: !229)
!229 = !{!230}
!230 = !DIDerivedType(tag: DW_TAG_member, scope: !228, file: !231, baseType: !232, size: 320)
!231 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/hybrid/016-MPI-hybrid-task-remote-no.c", directory: "/rmaracebench")
!232 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "kmp_task_t", size: 320, elements: !233)
!233 = !{!234, !236}
!234 = !DIDerivedType(tag: DW_TAG_member, scope: !232, file: !231, baseType: !235, size: 64, offset: 192)
!235 = distinct !DICompositeType(tag: DW_TAG_union_type, name: "kmp_cmplrdata_t", size: 64, elements: !58)
!236 = !DIDerivedType(tag: DW_TAG_member, scope: !232, file: !231, baseType: !235, size: 64, offset: 256)
!237 = !DILocation(line: 87, column: 1, scope: !221)
!238 = !{!239}
!239 = distinct !{!239, !240, !".omp_outlined.: argument 0"}
!240 = distinct !{!240, !".omp_outlined."}
!241 = !{!242}
!242 = distinct !{!242, !240, !".omp_outlined.: argument 1"}
!243 = !{!244}
!244 = distinct !{!244, !240, !".omp_outlined.: argument 2"}
!245 = !{!246}
!246 = distinct !{!246, !240, !".omp_outlined.: argument 3"}
!247 = !{!239, !242, !244, !246}
!248 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !249, type: !198, flags: DIFlagArtificial)
!249 = distinct !DISubprogram(name: ".omp_outlined.", scope: !231, file: !231, line: 89, type: !250, scopeLine: 88, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !58)
!250 = !DISubroutineType(types: !251)
!251 = !{null, !198, !195, !252, !254, !259, !260}
!252 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !253)
!253 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !36)
!254 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !255)
!255 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !256)
!256 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !257, size: 64)
!257 = !DISubroutineType(types: !258)
!258 = !{null, !252, null}
!259 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !36)
!260 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !261)
!261 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !262)
!262 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !263, size: 64)
!263 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !2, line: 87, size: 64, elements: !58)
!264 = !DILocation(line: 0, scope: !249, inlinedAt: !265)
!265 = distinct !DILocation(line: 87, column: 1, scope: !221)
!266 = !DILocalVariable(name: ".part_id.", arg: 2, scope: !249, type: !195, flags: DIFlagArtificial)
!267 = !DILocalVariable(name: ".privates.", arg: 3, scope: !249, type: !252, flags: DIFlagArtificial)
!268 = !DILocalVariable(name: ".copy_fn.", arg: 4, scope: !249, type: !254, flags: DIFlagArtificial)
!269 = !DILocalVariable(name: ".task_t.", arg: 5, scope: !249, type: !259, flags: DIFlagArtificial)
!270 = !DILocalVariable(name: "__context", arg: 6, scope: !249, type: !260, flags: DIFlagArtificial)
!271 = !DILocation(line: 88, column: 9, scope: !272, inlinedAt: !265)
!272 = !DILexicalBlockFile(scope: !249, file: !2, discriminator: 0)
!273 = !DILocalVariable(name: "flag", scope: !272, file: !2, line: 82, type: !57)
!274 = !DILocation(line: 82, column: 9, scope: !272, inlinedAt: !265)
!275 = !DILocation(line: 89, column: 22, scope: !276, inlinedAt: !265)
!276 = distinct !DILexicalBlock(scope: !272, file: !2, line: 88, column: 9)
!277 = !DILocation(line: 89, column: 11, scope: !276, inlinedAt: !265)
!278 = !DILocation(line: 90, column: 11, scope: !276, inlinedAt: !265)
!279 = distinct !DISubprogram(name: ".omp_outlined..3", scope: !2, file: !2, line: 83, type: !193, scopeLine: 83, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !22, retainedNodes: !58)
!280 = !DILocalVariable(name: ".global_tid.", arg: 1, scope: !279, type: !195, flags: DIFlagArtificial)
!281 = !DILocation(line: 0, scope: !279)
!282 = !DILocalVariable(name: ".bound_tid.", arg: 2, scope: !279, type: !195, flags: DIFlagArtificial)
!283 = !DILocalVariable(name: "flag", arg: 3, scope: !279, type: !199, flags: DIFlagArtificial)
!284 = !DILocalVariable(name: "win_base", arg: 4, scope: !279, type: !200, flags: DIFlagArtificial)
!285 = !DILocation(line: 83, column: 1, scope: !279)
!286 = !{!287}
!287 = !{i64 2, i64 -1, i64 -1, i1 true}
