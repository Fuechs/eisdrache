; ModuleID = 'test compiler'
source_filename = "test compiler"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-darwin23.2.0"

%vector = type { ptr, i64, i64, i64 }

declare ptr @malloc(i64)

declare void @free(ptr)

declare ptr @memcpy(ptr, ptr, i64)

define ptr @vector_get_buffer(ptr %this) {
entry:
  %buffer_ptr = getelementptr %vector, ptr %this, i32 0, i32 0
  %buffer = load ptr, ptr %buffer_ptr, align 8
  ret ptr %buffer
}

define void @vector_set_buffer(ptr %this, ptr %buffer) {
entry:
  %buffer_ptr = getelementptr %vector, ptr %this, i32 0, i32 0
  store ptr %buffer, ptr %buffer_ptr, align 8
  ret void
}

define i64 @vector_get_size(ptr %this) {
entry:
  %size_ptr = getelementptr %vector, ptr %this, i32 0, i32 1
  %size = load i64, ptr %size_ptr, align 8
  ret i64 %size
}

define void @vector_set_size(ptr %this, i64 %size) {
entry:
  %size_ptr = getelementptr %vector, ptr %this, i32 0, i32 1
  store i64 %size, ptr %size_ptr, align 8
  ret void
}

define i64 @vector_get_max(ptr %this) {
entry:
  %max_ptr = getelementptr %vector, ptr %this, i32 0, i32 2
  %max = load i64, ptr %max_ptr, align 8
  ret i64 %max
}

define void @vector_set_max(ptr %this, i64 %max) {
entry:
  %max_ptr = getelementptr %vector, ptr %this, i32 0, i32 1
  store i64 %max, ptr %max_ptr, align 8
  ret void
}

define i64 @vector_get_factor(ptr %this) {
entry:
  %factor_ptr = getelementptr %vector, ptr %this, i32 0, i32 3
  %factor = load i64, ptr %factor_ptr, align 8
  ret i64 %factor
}

define void @vector_set_factor(ptr %this, i64 %factor) {
entry:
  %factor_ptr = getelementptr %vector, ptr %this, i32 0, i32 1
  store i64 %factor, ptr %factor_ptr, align 8
  ret void
}

; Function Attrs: nounwind
define fastcc void @vector_constructor(ptr %this) #0 {
entry:
  call void @vector_set_buffer(ptr %this, ptr null)
  call void @vector_set_size(ptr %this, i64 0)
  call void @vector_set_max(ptr %this, i64 0)
  call void @vector_set_factor(ptr %this, i64 16)
  ret void
}

define void @vector_constructor_size(ptr %this, i64 %size) {
entry:
  %bytes = mul i64 %size, 8
  %buffer = call ptr @malloc(i64 %bytes)
  call void @vector_set_buffer(ptr %this, ptr %buffer)
  call void @vector_set_size(ptr %this, i64 %size)
  call void @vector_set_max(ptr %this, i64 0)
  call void @vector_set_factor(ptr %this, i64 16)
  ret void
}

define void @vector_constructor_copy(ptr %this, ptr %original) {
entry:
  ret void
}

; Function Attrs: nounwind
define fastcc void @vector_destructor(ptr %this) #0 {
entry:
  %buffer = call ptr @vector_get_buffer(ptr %this)
  %cond = icmp eq ptr %buffer, null
  br i1 %cond, label %free_close, label %free_begin

free_begin:                                       ; preds = %entry
  call void @free(ptr %buffer)
  br label %free_close

free_close:                                       ; preds = %free_begin, %entry
  ret void
}

define void @vector_resize(ptr %this, i64 %new_size) {
entry:
  %bytes = mul i64 %new_size, 8
  %new_buffer = call ptr @malloc(i64 %bytes)
  %buffer = call ptr @vector_get_buffer(ptr %this)
  %size = call i64 @vector_get_size(ptr %this)
  %cond = icmp eq ptr %buffer, null
  br i1 %cond, label %empty, label %copy

copy:                                             ; preds = %entry
  %0 = call ptr @memcpy(ptr %new_buffer, ptr %buffer, i64 %size)
  call void @free(ptr %buffer)
  br label %end

empty:                                            ; preds = %entry
  store ptr null, ptr %new_buffer, align 8
  br label %end

end:                                              ; preds = %empty, %copy
  call void @vector_set_buffer(ptr %this, ptr %new_buffer)
  %max_ptr = getelementptr %vector, ptr %this, i32 0, i32 3
  store i64 %new_size, ptr %max_ptr, align 8
  ret void
}

define i1 @vector_is_valid_index(ptr %this, i64 %index) {
entry:
  %max = call i64 @vector_get_max(ptr %this)
  %equals = icmp ult i64 %index, %max
  ret i1 %equals
}

define i64 @vector_get_at_index(ptr %this, i32 %index) {
entry:
  %buffer = call ptr @vector_get_buffer(ptr %this)
  %element_ptr = getelementptr ptr, ptr %buffer, i32 %index
  %element = load i64, ptr %element_ptr, align 8
  ret i64 %element
}

define void @vector_set_at_index(ptr %this, i32 %index, i64 %value) {
entry:
  %buffer = call ptr @vector_get_buffer(ptr %this)
  %element_ptr = getelementptr ptr, ptr %buffer, i32 %index
  store i64 %value, ptr %element_ptr, align 8
  ret void
}

define i64 @main(i64 %argc, ptr %argv) {
entry:
  %list = alloca %vector, align 8
  call void @vector_constructor_size(ptr %list, i64 2)
  %buffer = call ptr @vector_get_buffer(ptr %list)
  %a = alloca double, align 8
  %b = alloca double, align 8
  store double 3.450000e+01, ptr %a, align 8
  %a_lhs_load = load double, ptr %a, align 8
  store double 2.000000e+00, ptr %b, align 8
  %b_rhs_load = load double, ptr %b, align 8
  %multmp = fmul double %a_lhs_load, %b_rhs_load
  call void @vector_constructor_size(ptr %list, i64 2)
  %typecast = fptoui double %multmp to i64
  call void @vector_set_at_index(ptr %list, i32 0, i64 %typecast)
  %fst = call i64 @vector_get_at_index(ptr %list, i32 0)
  call void @vector_destructor(ptr %list)
  ret i64 %fst
}

attributes #0 = { nounwind }
