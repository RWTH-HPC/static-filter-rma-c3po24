; ModuleID = 'results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c.ll'
source_filename = "results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c"
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
@0 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@1 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@2 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@3 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@4 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@5 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@6 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@7 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@8 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@9 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@10 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@11 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@12 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@13 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@14 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@15 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@16 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@17 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@18 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1
@19 = private unnamed_addr constant [84 x i8] c"results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c\00", align 1

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
  %16 = alloca ptr, align 8
  %17 = alloca ptr, align 8
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
  %18 = call i32 @MPI_Init(ptr noundef %4, ptr noundef %5), !dbg !68
  %19 = call i32 @MPI_Comm_rank(ptr noundef @ompi_mpi_comm_world, ptr noundef %6), !dbg !69
  %20 = call i32 @MPI_Comm_size(ptr noundef @ompi_mpi_comm_world, ptr noundef %7), !dbg !70
  %21 = load i32, ptr %7, align 4, !dbg !71
  %22 = icmp ne i32 %21, 2, !dbg !73
  br i1 %22, label %23, label %27, !dbg !74

23:                                               ; preds = %2
  %24 = load i32, ptr %7, align 4, !dbg !75
  %25 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %24, i32 noundef 2), !dbg !77
  %26 = call i32 @MPI_Abort(ptr noundef @ompi_mpi_comm_world, i32 noundef 1), !dbg !78
  br label %27, !dbg !79

27:                                               ; preds = %23, %2
  %28 = call i32 @parcoach_rma_MPI_Win_allocate(i64 40, i32 4, ptr @ompi_mpi_info_null, ptr @ompi_mpi_comm_world, ptr %9, ptr %8, i32 47, ptr @0), !dbg !80
  call void @llvm.dbg.declare(metadata ptr %15, metadata !81, metadata !DIExpression()), !dbg !83
  store i32 0, ptr %15, align 4, !dbg !83
  br label %29, !dbg !84

29:                                               ; preds = %37, %27
  %30 = load i32, ptr %15, align 4, !dbg !85
  %31 = icmp slt i32 %30, 10, !dbg !87
  br i1 %31, label %32, label %40, !dbg !88

32:                                               ; preds = %29
  %33 = load ptr, ptr %9, align 8, !dbg !89
  %34 = load i32, ptr %15, align 4, !dbg !91
  %35 = sext i32 %34 to i64, !dbg !89
  %36 = getelementptr inbounds i32, ptr %33, i64 %35, !dbg !89
  store i32 0, ptr %36, align 4, !dbg !92
  br label %37, !dbg !93

37:                                               ; preds = %32
  call void @parcoach_rma_load(ptr %15, i64 32, i32 49, ptr @18), !dbg !94
  %38 = load i32, ptr %15, align 4, !dbg !94
  %39 = add nsw i32 %38, 1, !dbg !94
  call void @parcoach_rma_store(ptr %15, i64 32, i32 49, ptr @19), !dbg !94
  store i32 %39, ptr %15, align 4, !dbg !94
  br label %29, !dbg !95, !llvm.loop !96

40:                                               ; preds = %29
  %41 = load ptr, ptr %8, align 8, !dbg !99
  %42 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %41, i32 53, ptr @1), !dbg !100
  call void @llvm.dbg.declare(metadata ptr %16, metadata !101, metadata !DIExpression()), !dbg !102
  call void @llvm.dbg.declare(metadata ptr %17, metadata !103, metadata !DIExpression()), !dbg !104
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %16, ptr align 8 %12, i64 8, i1 false), !dbg !105
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %17, ptr align 8 %9, i64 8, i1 false), !dbg !106
  call void @parcoach_rma_load(ptr %6, i64 32, i32 61, ptr @2), !dbg !107
  %43 = load i32, ptr %6, align 4, !dbg !107
  %44 = icmp eq i32 %43, 0, !dbg !109
  br i1 %44, label %45, label %50, !dbg !110

45:                                               ; preds = %40
  call void @parcoach_rma_load(ptr %16, i64 64, i32 64, ptr @3), !dbg !111
  %46 = load ptr, ptr %16, align 8, !dbg !111
  call void @parcoach_rma_load(ptr %8, i64 64, i32 64, ptr @4), !dbg !113
  %47 = load ptr, ptr %8, align 8, !dbg !113
  %48 = call i32 @parcoach_rma_MPI_Put(ptr %46, i32 1, ptr @ompi_mpi_int, i32 1, i64 0, i32 1, ptr @ompi_mpi_int, ptr %47, i32 64, ptr @5), !dbg !114
  call void @parcoach_rma_load(ptr %16, i64 64, i32 66, ptr @6), !dbg !115
  %49 = load ptr, ptr %16, align 8, !dbg !115
  call void @parcoach_rma_store(ptr %49, i64 32, i32 66, ptr @7), !dbg !116
  store i32 42, ptr %49, align 4, !dbg !116
  br label %50, !dbg !117

50:                                               ; preds = %45, %40
  call void @parcoach_rma_load(ptr %8, i64 64, i32 69, ptr @8), !dbg !118
  %51 = load ptr, ptr %8, align 8, !dbg !118
  %52 = call i32 @parcoach_rma_MPI_Win_fence(i32 0, ptr %51, i32 69, ptr @9), !dbg !119
  %53 = call i32 @parcoach_rma_MPI_Barrier(ptr @ompi_mpi_comm_world, i32 71, ptr @10), !dbg !120
  call void @parcoach_rma_load(ptr %6, i64 32, i32 74, ptr @11), !dbg !121
  %54 = load i32, ptr %6, align 4, !dbg !121
  call void @parcoach_rma_load(ptr %12, i64 64, i32 74, ptr @12), !dbg !122
  %55 = load ptr, ptr %12, align 8, !dbg !122
  call void @parcoach_rma_load(ptr %55, i64 32, i32 74, ptr @13), !dbg !123
  %56 = load i32, ptr %55, align 4, !dbg !123
  call void @parcoach_rma_load(ptr %11, i64 32, i32 74, ptr @14), !dbg !124
  %57 = load i32, ptr %11, align 4, !dbg !124
  call void @parcoach_rma_load(ptr %9, i64 64, i32 74, ptr @15), !dbg !125
  %58 = load ptr, ptr %9, align 8, !dbg !125
  %59 = getelementptr inbounds i32, ptr %58, i64 0, !dbg !125
  call void @parcoach_rma_load(ptr %59, i64 32, i32 74, ptr @16), !dbg !125
  %60 = load i32, ptr %59, align 4, !dbg !125
  %61 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %54, i32 noundef %56, i32 noundef %57, i32 noundef %60), !dbg !126
  %62 = call i32 @parcoach_rma_MPI_Win_free(ptr %8, i32 76, ptr @17), !dbg !127
  %63 = call i32 @MPI_Finalize(), !dbg !128
  ret i32 0, !dbg !129
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

declare void @parcoach_rma_store(ptr, i64, i32, ptr)

declare i32 @parcoach_rma_MPI_Barrier(ptr, i32, ptr)

declare i32 @parcoach_rma_MPI_Win_free(ptr, i32, ptr)

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { argmemonly nocallback nofree nounwind willreturn }

!llvm.dbg.cu = !{!12}
!llvm.module.flags = !{!26, !27, !28, !29, !30, !31, !32, !33}
!llvm.ident = !{!34}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 43, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "results-20240305-201640/PARCOACH-dynamic/misc/008-MPI-misc-put-store-memcpy-local.c", directory: "/rmaracebench", checksumkind: CSK_MD5, checksum: "6c57101220d9e1db2850243c747584fd")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 392, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 49)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 72, type: !9, isLocal: true, isDefinition: true)
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
!35 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 29, type: !36, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !12, retainedNodes: !41)
!36 = !DISubroutineType(types: !37)
!37 = !{!38, !38, !39}
!38 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!39 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !40, size: 64)
!40 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!41 = !{}
!42 = !DILocalVariable(name: "argc", arg: 1, scope: !35, file: !2, line: 29, type: !38)
!43 = !DILocation(line: 29, column: 14, scope: !35)
!44 = !DILocalVariable(name: "argv", arg: 2, scope: !35, file: !2, line: 29, type: !39)
!45 = !DILocation(line: 29, column: 27, scope: !35)
!46 = !DILocalVariable(name: "rank", scope: !35, file: !2, line: 30, type: !38)
!47 = !DILocation(line: 30, column: 7, scope: !35)
!48 = !DILocalVariable(name: "size", scope: !35, file: !2, line: 30, type: !38)
!49 = !DILocation(line: 30, column: 13, scope: !35)
!50 = !DILocalVariable(name: "win", scope: !35, file: !2, line: 31, type: !51)
!51 = !DIDerivedType(tag: DW_TAG_typedef, name: "MPI_Win", file: !15, line: 429, baseType: !52)
!52 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64)
!53 = !DICompositeType(tag: DW_TAG_structure_type, name: "ompi_win_t", file: !15, line: 429, flags: DIFlagFwdDecl)
!54 = !DILocation(line: 31, column: 11, scope: !35)
!55 = !DILocalVariable(name: "win_base", scope: !35, file: !2, line: 32, type: !56)
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !38, size: 64)
!57 = !DILocation(line: 32, column: 8, scope: !35)
!58 = !DILocalVariable(name: "value", scope: !35, file: !2, line: 33, type: !38)
!59 = !DILocation(line: 33, column: 7, scope: !35)
!60 = !DILocalVariable(name: "value2", scope: !35, file: !2, line: 33, type: !38)
!61 = !DILocation(line: 33, column: 18, scope: !35)
!62 = !DILocalVariable(name: "buf", scope: !35, file: !2, line: 34, type: !56)
!63 = !DILocation(line: 34, column: 8, scope: !35)
!64 = !DILocalVariable(name: "result", scope: !35, file: !2, line: 35, type: !38)
!65 = !DILocation(line: 35, column: 7, scope: !35)
!66 = !DILocalVariable(name: "token", scope: !35, file: !2, line: 36, type: !38)
!67 = !DILocation(line: 36, column: 7, scope: !35)
!68 = !DILocation(line: 38, column: 3, scope: !35)
!69 = !DILocation(line: 39, column: 3, scope: !35)
!70 = !DILocation(line: 40, column: 3, scope: !35)
!71 = !DILocation(line: 42, column: 7, scope: !72)
!72 = distinct !DILexicalBlock(scope: !35, file: !2, line: 42, column: 7)
!73 = !DILocation(line: 42, column: 12, scope: !72)
!74 = !DILocation(line: 42, column: 7, scope: !35)
!75 = !DILocation(line: 43, column: 65, scope: !76)
!76 = distinct !DILexicalBlock(scope: !72, file: !2, line: 42, column: 25)
!77 = !DILocation(line: 43, column: 5, scope: !76)
!78 = !DILocation(line: 44, column: 5, scope: !76)
!79 = !DILocation(line: 45, column: 3, scope: !76)
!80 = !DILocation(line: 47, column: 3, scope: !35)
!81 = !DILocalVariable(name: "i", scope: !82, file: !2, line: 49, type: !38)
!82 = distinct !DILexicalBlock(scope: !35, file: !2, line: 49, column: 3)
!83 = !DILocation(line: 49, column: 12, scope: !82)
!84 = !DILocation(line: 49, column: 8, scope: !82)
!85 = !DILocation(line: 49, column: 19, scope: !86)
!86 = distinct !DILexicalBlock(scope: !82, file: !2, line: 49, column: 3)
!87 = !DILocation(line: 49, column: 21, scope: !86)
!88 = !DILocation(line: 49, column: 3, scope: !82)
!89 = !DILocation(line: 50, column: 5, scope: !90)
!90 = distinct !DILexicalBlock(scope: !86, file: !2, line: 49, column: 38)
!91 = !DILocation(line: 50, column: 14, scope: !90)
!92 = !DILocation(line: 50, column: 17, scope: !90)
!93 = !DILocation(line: 51, column: 3, scope: !90)
!94 = !DILocation(line: 49, column: 34, scope: !86)
!95 = !DILocation(line: 49, column: 3, scope: !86)
!96 = distinct !{!96, !88, !97, !98}
!97 = !DILocation(line: 51, column: 3, scope: !82)
!98 = !{!"llvm.loop.mustprogress"}
!99 = !DILocation(line: 53, column: 20, scope: !35)
!100 = !DILocation(line: 53, column: 3, scope: !35)
!101 = !DILocalVariable(name: "buf_alias", scope: !35, file: !2, line: 55, type: !56)
!102 = !DILocation(line: 55, column: 8, scope: !35)
!103 = !DILocalVariable(name: "win_base_alias", scope: !35, file: !2, line: 56, type: !56)
!104 = !DILocation(line: 56, column: 8, scope: !35)
!105 = !DILocation(line: 58, column: 3, scope: !35)
!106 = !DILocation(line: 59, column: 3, scope: !35)
!107 = !DILocation(line: 61, column: 7, scope: !108)
!108 = distinct !DILexicalBlock(scope: !35, file: !2, line: 61, column: 7)
!109 = !DILocation(line: 61, column: 12, scope: !108)
!110 = !DILocation(line: 61, column: 7, scope: !35)
!111 = !DILocation(line: 64, column: 13, scope: !112)
!112 = distinct !DILexicalBlock(scope: !108, file: !2, line: 61, column: 18)
!113 = !DILocation(line: 64, column: 54, scope: !112)
!114 = !DILocation(line: 64, column: 5, scope: !112)
!115 = !DILocation(line: 66, column: 6, scope: !112)
!116 = !DILocation(line: 66, column: 16, scope: !112)
!117 = !DILocation(line: 67, column: 3, scope: !112)
!118 = !DILocation(line: 69, column: 20, scope: !35)
!119 = !DILocation(line: 69, column: 3, scope: !35)
!120 = !DILocation(line: 71, column: 3, scope: !35)
!121 = !DILocation(line: 74, column: 10, scope: !35)
!122 = !DILocation(line: 74, column: 17, scope: !35)
!123 = !DILocation(line: 74, column: 16, scope: !35)
!124 = !DILocation(line: 74, column: 22, scope: !35)
!125 = !DILocation(line: 74, column: 30, scope: !35)
!126 = !DILocation(line: 72, column: 3, scope: !35)
!127 = !DILocation(line: 76, column: 3, scope: !35)
!128 = !DILocation(line: 77, column: 3, scope: !35)
!129 = !DILocation(line: 79, column: 3, scope: !35)
