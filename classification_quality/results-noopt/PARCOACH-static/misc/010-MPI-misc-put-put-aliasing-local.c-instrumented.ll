; ModuleID = 'results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c.ll'
source_filename = "results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c"
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
@0 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@1 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@2 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@3 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@4 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@5 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@6 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@7 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@8 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@9 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@10 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@11 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@12 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@13 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@14 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@15 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@16 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@17 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@18 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@19 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1
@20 = private unnamed_addr constant [83 x i8] c"results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @aliasgenerator(ptr noundef %0, ptr noundef %1) #0 !dbg !35 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !42, metadata !DIExpression()), !dbg !43
  store ptr %1, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !44, metadata !DIExpression()), !dbg !45
  %5 = load ptr, ptr %3, align 8, !dbg !46
  %6 = load ptr, ptr %5, align 8, !dbg !47
  %7 = load ptr, ptr %4, align 8, !dbg !48
  store ptr %6, ptr %7, align 8, !dbg !49
  ret void, !dbg !50
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !51 {
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
  call void @llvm.dbg.declare(metadata ptr %4, metadata !56, metadata !DIExpression()), !dbg !57
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !58, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.declare(metadata ptr %6, metadata !60, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.declare(metadata ptr %7, metadata !62, metadata !DIExpression()), !dbg !63
  call void @llvm.dbg.declare(metadata ptr %8, metadata !64, metadata !DIExpression()), !dbg !68
  call void @llvm.dbg.declare(metadata ptr %9, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.declare(metadata ptr %10, metadata !71, metadata !DIExpression()), !dbg !72
  store i32 1, ptr %10, align 4, !dbg !72
  call void @llvm.dbg.declare(metadata ptr %11, metadata !73, metadata !DIExpression()), !dbg !74
  store i32 2, ptr %11, align 4, !dbg !74
  call void @llvm.dbg.declare(metadata ptr %12, metadata !75, metadata !DIExpression()), !dbg !76
  store ptr %10, ptr %12, align 8, !dbg !76
  call void @llvm.dbg.declare(metadata ptr %13, metadata !77, metadata !DIExpression()), !dbg !78
  call void @llvm.dbg.declare(metadata ptr %14, metadata !79, metadata !DIExpression()), !dbg !80
  store i32 42, ptr %14, align 4, !dbg !80
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !81
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !82
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !83
  %21 = load i32, ptr %7, align 4, !dbg !84
  %22 = icmp ne i32 %21, 2, !dbg !86
  br i1 %22, label %23, label %27, !dbg !87

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !88
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !90
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !91
  br label %27, !dbg !92

27:                                               ; preds = %23, %2
  %28 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 4, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 47, ptr @0), !dbg !93
  call void @llvm.dbg.declare(metadata ptr %15, metadata !94, metadata !DIExpression()), !dbg !96
  store i32 0, ptr %15, align 4, !dbg !96
  br label %29, !dbg !97

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !98
  %31 = icmp slt i32 %30, 10, !dbg !100
  br i1 %31, label %32, label %40, !dbg !101

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !102
  %34 = load i32, ptr %15, align 4, !dbg !104
  %35 = sext i32 %34 to i64, !dbg !102
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !102
  store i32 0, ptr %36, align 4, !dbg !105
  br label %37, !dbg !106

37:                                               ; preds = %32
  call void @parcoach_rma_load(ptr %15, i64 32, i32 49, ptr @19), !dbg !107
  %38 = load i32, ptr %15, align 4, !dbg !107
  %39 = add nsw i32 %38, 1, !dbg !107
  call void @parcoach_rma_store(ptr %15, i64 32, i32 49, ptr @20), !dbg !107
  store i32 %39, ptr %15, align 4, !dbg !107
  br label %29, !dbg !108, !llvm.loop !109

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !112
  %42 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %41, i32 53, ptr @1), !dbg !113
  call void @llvm.dbg.declare(metadata ptr %16, metadata !114, metadata !DIExpression()), !dbg !115
  call void @llvm.dbg.declare(metadata ptr %17, metadata !116, metadata !DIExpression()), !dbg !117
  call void @aliasgenerator(ptr noundef %12, ptr noundef %16), !dbg !118
  call void @aliasgenerator(ptr noundef %9, ptr noundef %17), !dbg !119
  call void @parcoach_rma_load(ptr %6, i64 32, i32 61, ptr @2), !dbg !120
  %43 = load i32, ptr %6, align 4, !dbg !120
  %44 = icmp eq i32 %43, 0, !dbg !122
  br i1 %44, label %45, label %52, !dbg !123

45:                                               ; preds = %40
  call void @parcoach_rma_load(ptr %16, i64 64, i32 62, ptr @3), !dbg !124
  %46 = load ptr, ptr %16, align 8, !dbg !124
  call void @parcoach_rma_load(ptr %8, i64 64, i32 62, ptr @4), !dbg !126
  %47 = load ptr, ptr %8, align 8, !dbg !126
  %48 = call i32 @parcoach_rma_MPI_Put(ptr %46, i32 1, ptr @ompi_mpi_int, i32 1, i64 0, i32 1, ptr @ompi_mpi_int, ptr %47, i32 62, ptr @5), !dbg !127
  call void @parcoach_rma_load(ptr %16, i64 64, i32 63, ptr @6), !dbg !128
  %49 = load ptr, ptr %16, align 8, !dbg !128
  call void @parcoach_rma_load(ptr %8, i64 64, i32 63, ptr @7), !dbg !129
  %50 = load ptr, ptr %8, align 8, !dbg !129
  %51 = call i32 @parcoach_rma_MPI_Put(ptr %49, i32 1, ptr @ompi_mpi_int, i32 1, i64 1, i32 1, ptr @ompi_mpi_int, ptr %50, i32 63, ptr @8), !dbg !130
  br label %52, !dbg !131

52:                                               ; preds = %45, %40
  call void @parcoach_rma_load(ptr %8, i64 64, i32 66, ptr @9), !dbg !132
  %53 = load ptr, ptr %8, align 8, !dbg !132
  %54 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %53, i32 66, ptr @10), !dbg !133
  %55 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 68, ptr @11), !dbg !134
  call void @parcoach_rma_load(ptr %6, i64 32, i32 71, ptr @12), !dbg !135
  %56 = load i32, ptr %6, align 4, !dbg !135
  call void @parcoach_rma_load(ptr %12, i64 64, i32 71, ptr @13), !dbg !136
  %57 = load ptr, ptr %12, align 8, !dbg !136
  call void @parcoach_rma_load(ptr %57, i64 32, i32 71, ptr @14), !dbg !137
  %58 = load i32, ptr %57, align 4, !dbg !137
  call void @parcoach_rma_load(ptr %11, i64 32, i32 71, ptr @15), !dbg !138
  %59 = load i32, ptr %11, align 4, !dbg !138
  call void @parcoach_rma_load(ptr %9, i64 64, i32 71, ptr @16), !dbg !139
  %60 = load ptr, ptr %9, align 8, !dbg !139
  %61 = getelementptr inbounds i32, ptr %60, i64 0, !dbg !139
  call void @parcoach_rma_load(ptr %61, i64 32, i32 71, ptr @17), !dbg !139
  %62 = load i32, ptr %61, align 4, !dbg !139
  %63 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %56, i32 noundef %58, i32 noundef %59, i32 noundef %62), !dbg !140
  %64 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 73, ptr @18), !dbg !141
  %65 = call i32 @MPI_Finalize(), !dbg !142
  ret i32 0, !dbg !143
}

declare i32 @MPI_Init(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_rank(ptr noundef, ptr noundef) #2

declare i32 @MPI_Comm_size(ptr noundef, ptr noundef) #2

declare i32 @printf(ptr noundef, ...) #2

declare i32 @MPI_Abort(ptr noundef, i32 noundef) #2

declare i32 @MPI_Win_allocate(i64 noundef, i32 noundef, ptr noundef, ptr noundef, ptr noundef, ptr noundef) #2

declare i32 @MPI_Win_fence(i32 noundef, ptr noundef) #2

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

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!26, !27, !28, !29, !30, !31, !32, !33}
!llvm.ident = !{!34}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 43, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-static/misc/010-MPI-misc-put-put-aliasing-local.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "79b8d320f4145e08982019bc13dcc81c")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 69, type: !9, isLocal: true, isDefinition: true)
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
!35 = distinct !DISubprogram(name: "aliasgenerator", scope: !2, file: !2, line: 24, type: !36, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!36 = !DISubroutineType(types: !37)
!37 = !{null, !38, !38}
!38 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !39, size: 64)
!39 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !40, size: 64)
!40 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!41 = !{}
!42 = !DILocalVariable(name: "x", arg: 1, scope: !35, file: !2, line: 24, type: !38)
!43 = !DILocation(line: 24, column: 53, scope: !35)
!44 = !DILocalVariable(name: "y", arg: 2, scope: !35, file: !2, line: 24, type: !38)
!45 = !DILocation(line: 24, column: 62, scope: !35)
!46 = !DILocation(line: 24, column: 73, scope: !35)
!47 = !DILocation(line: 24, column: 72, scope: !35)
!48 = !DILocation(line: 24, column: 68, scope: !35)
!49 = !DILocation(line: 24, column: 70, scope: !35)
!50 = !DILocation(line: 24, column: 76, scope: !35)
!51 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !52, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!52 = !DISubroutineType(types: !53)
!53 = !{!40, !40, !54}
!54 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !55, size: 64)
!55 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!56 = !DILocalVariable(name: "argc", arg: 1, scope: !51, file: !2, line: 29, type: !40)
!57 = !DILocation(line: 29, column: 14, scope: !51)
!58 = !DILocalVariable(name: "argv", arg: 2, scope: !51, file: !2, line: 29, type: !54)
!59 = !DILocation(line: 29, column: 27, scope: !51)
!60 = !DILocalVariable(name: "rank", scope: !51, file: !2, line: 30, type: !40)
!61 = !DILocation(line: 30, column: 7, scope: !51)
!62 = !DILocalVariable(name: "size", scope: !51, file: !2, line: 30, type: !40)
!63 = !DILocation(line: 30, column: 13, scope: !51)
!64 = !DILocalVariable(name: "win", scope: !51, file: !2, line: 31, type: !65)
!65 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !66)
!66 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !67, size: 64)
!67 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!68 = !DILocation(line: 31, column: 11, scope: !51)
!69 = !DILocalVariable(name: "win_base", scope: !51, file: !2, line: 32, type: !39)
!70 = !DILocation(line: 32, column: 8, scope: !51)
!71 = !DILocalVariable(name: "value", scope: !51, file: !2, line: 33, type: !40)
!72 = !DILocation(line: 33, column: 7, scope: !51)
!73 = !DILocalVariable(name: "value2", scope: !51, file: !2, line: 33, type: !40)
!74 = !DILocation(line: 33, column: 18, scope: !51)
!75 = !DILocalVariable(name: "buf", scope: !51, file: !2, line: 34, type: !39)
!76 = !DILocation(line: 34, column: 8, scope: !51)
!77 = !DILocalVariable(name: "result", scope: !51, file: !2, line: 35, type: !40)
!78 = !DILocation(line: 35, column: 7, scope: !51)
!79 = !DILocalVariable(name: "token", scope: !51, file: !2, line: 36, type: !40)
!80 = !DILocation(line: 36, column: 7, scope: !51)
!81 = !DILocation(line: 38, column: 3, scope: !51)
!82 = !DILocation(line: 39, column: 3, scope: !51)
!83 = !DILocation(line: 40, column: 3, scope: !51)
!84 = !DILocation(line: 42, column: 7, scope: !85)
!85 = distinct !DILexicalBlock(scope: !51, file: !2, line: 42, column: 7)
!86 = !DILocation(line: 42, column: 12, scope: !85)
!87 = !DILocation(line: 42, column: 7, scope: !51)
!88 = !DILocation(line: 43, column: 65, scope: !89)
!89 = distinct !DILexicalBlock(scope: !85, file: !2, line: 42, column: 25)
!90 = !DILocation(line: 43, column: 5, scope: !89)
!91 = !DILocation(line: 44, column: 5, scope: !89)
!92 = !DILocation(line: 45, column: 3, scope: !89)
!93 = !DILocation(line: 47, column: 3, scope: !51)
!94 = !DILocalVariable(name: "i", scope: !95, file: !2, line: 49, type: !40)
!95 = distinct !DILexicalBlock(scope: !51, file: !2, line: 49, column: 3)
!96 = !DILocation(line: 49, column: 12, scope: !95)
!97 = !DILocation(line: 49, column: 8, scope: !95)
!98 = !DILocation(line: 49, column: 19, scope: !99)
!99 = distinct !DILexicalBlock(scope: !95, file: !2, line: 49, column: 3)
!100 = !DILocation(line: 49, column: 21, scope: !99)
!101 = !DILocation(line: 49, column: 3, scope: !95)
!102 = !DILocation(line: 50, column: 5, scope: !103)
!103 = distinct !DILexicalBlock(scope: !99, file: !2, line: 49, column: 38)
!104 = !DILocation(line: 50, column: 14, scope: !103)
!105 = !DILocation(line: 50, column: 17, scope: !103)
!106 = !DILocation(line: 51, column: 3, scope: !103)
!107 = !DILocation(line: 49, column: 34, scope: !99)
!108 = !DILocation(line: 49, column: 3, scope: !99)
!109 = distinct !{!109, !101, !110, !111}
!110 = !DILocation(line: 51, column: 3, scope: !95)
!111 = !{!"llvm.loop.mustprogress"}
!112 = !DILocation(line: 53, column: 20, scope: !51)
!113 = !DILocation(line: 53, column: 3, scope: !51)
!114 = !DILocalVariable(name: "buf_alias", scope: !51, file: !2, line: 55, type: !39)
!115 = !DILocation(line: 55, column: 8, scope: !51)
!116 = !DILocalVariable(name: "win_base_alias", scope: !51, file: !2, line: 56, type: !39)
!117 = !DILocation(line: 56, column: 8, scope: !51)
!118 = !DILocation(line: 58, column: 3, scope: !51)
!119 = !DILocation(line: 59, column: 3, scope: !51)
!120 = !DILocation(line: 61, column: 7, scope: !121)
!121 = distinct !DILexicalBlock(scope: !51, file: !2, line: 61, column: 7)
!122 = !DILocation(line: 61, column: 12, scope: !121)
!123 = !DILocation(line: 61, column: 7, scope: !51)
!124 = !DILocation(line: 62, column: 13, scope: !125)
!125 = distinct !DILexicalBlock(scope: !121, file: !2, line: 61, column: 18)
!126 = !DILocation(line: 62, column: 54, scope: !125)
!127 = !DILocation(line: 62, column: 5, scope: !125)
!128 = !DILocation(line: 63, column: 13, scope: !125)
!129 = !DILocation(line: 63, column: 54, scope: !125)
!130 = !DILocation(line: 63, column: 5, scope: !125)
!131 = !DILocation(line: 64, column: 3, scope: !125)
!132 = !DILocation(line: 66, column: 20, scope: !51)
!133 = !DILocation(line: 66, column: 3, scope: !51)
!134 = !DILocation(line: 68, column: 3, scope: !51)
!135 = !DILocation(line: 71, column: 10, scope: !51)
!136 = !DILocation(line: 71, column: 17, scope: !51)
!137 = !DILocation(line: 71, column: 16, scope: !51)
!138 = !DILocation(line: 71, column: 22, scope: !51)
!139 = !DILocation(line: 71, column: 30, scope: !51)
!140 = !DILocation(line: 69, column: 3, scope: !51)
!141 = !DILocation(line: 73, column: 3, scope: !51)
!142 = !DILocation(line: 74, column: 3, scope: !51)
!143 = !DILocation(line: 76, column: 3, scope: !51)
