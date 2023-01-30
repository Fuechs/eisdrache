; ModuleID = 'test'
source_filename = "test"

%int_array = type { ptr, i64, i64, i64 }

declare ptr @malloc(i64)

declare void @free(ptr)

declare ptr @memcpy(ptr, ptr, i64)

define ptr @int_array_get_buffer(ptr %0) {
entry:
  %buffer_ptr = getelementptr %int_array, ptr %0, i64 0, i32 0
  %buffer = load ptr, ptr %buffer_ptr, align 8
  ret ptr %buffer
}

define void @int_array_set_buffer(ptr %0, ptr %1) {
entry:
  %buffer_ptr = getelementptr %int_array, ptr %0, i64 0, i32 0
  store ptr %1, ptr %buffer_ptr, align 8
  ret void
}

define i64 @int_array_get_length(ptr %0) {
entry:
  %length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 1
  %length = load i64, ptr %length_ptr, align 4
  ret i64 %length
}

define void @int_array_set_length(ptr %0, i64 %1) {
entry:
  %length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 1
  store i64 %1, ptr %length_ptr, align 4
  ret void
}

define i64 @int_array_get_max_length(ptr %0) {
entry:
  %max_length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 2
  %max_length = load i64, ptr %max_length_ptr, align 4
  ret i64 %max_length
}

define void @int_array_set_max_length(ptr %0, i64 %1) {
entry:
  %max_length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 2
  store i64 %1, ptr %max_length_ptr, align 4
  ret void
}

define i64 @int_array_get_factor(ptr %0) {
entry:
  %factor_ptr = getelementptr %int_array, ptr %0, i64 0, i32 3
  %factor = load i64, ptr %factor_ptr, align 4
  ret i64 %factor
}

define void @int_array_set_factor(ptr %0, i64 %1) {
entry:
  %factor_ptr = getelementptr %int_array, ptr %0, i64 0, i32 3
  store i64 %1, ptr %factor_ptr, align 4
  ret void
}

define fastcc void @int_array_resize(ptr %0, i64 %1) {
entry:
  %output = call ptr @malloc(i64 %1)
  %buffer_ptr = getelementptr %int_array, ptr %0, i64 0, i32 0
  %buffer = load ptr, ptr %buffer_ptr, align 8
  %2 = call i64 @int_array_get_length(ptr %0)
  %3 = call ptr @memcpy(ptr %output, ptr %buffer, i64 %2)
  call void @free(ptr %buffer)
  store ptr %output, ptr %buffer_ptr, align 8
  call void @int_array_set_max_length(ptr %0, i64 %1)
  ret void
}

; Function Attrs: nounwind
define fastcc void @int_array_create_empty(ptr %0) #0 {
entry:
  call void @int_array_set_buffer(ptr %0, ptr null)
  call void @int_array_set_length(ptr %0, i64 0)
  call void @int_array_set_max_length(ptr %0, i64 0)
  call void @int_array_set_factor(ptr %0, i64 16)
  ret void
}

; Function Attrs: nounwind
define fastcc void @int_array_create_copy(ptr %0, ptr %1) #0 {
entry:
  %dest_buffer = call ptr @int_array_get_buffer(ptr %0)
  %source_buffer = call ptr @int_array_get_buffer(ptr %1)
  %source_length = call i64 @int_array_get_length(ptr %1)
  %source_max_length = call i64 @int_array_get_max_length(ptr %1)
  %source_factor = call i64 @int_array_get_factor(ptr %1)
  call void @int_array_resize(ptr %0, i64 %source_max_length)
  %2 = call ptr @memcpy(ptr %dest_buffer, ptr %source_buffer, i64 %source_length)
  call void @int_array_set_length(ptr %0, i64 %source_length)
  call void @int_array_set_factor(ptr %0, i64 %source_factor)
  ret void
}

; Function Attrs: nounwind
define fastcc void @int_array_delete_array(ptr %0) #0 {
entry:
  %buffer = call ptr @int_array_get_buffer(ptr %0)
  %comp = icmp ne ptr %buffer, null
  br i1 %comp, label %free_begin, label %free_close

free_begin:                                       ; preds = %entry
  call void @free(ptr %buffer)
  br label %free_close

free_close:                                       ; preds = %free_begin, %entry
  ret void
}

define i64 @main() {
entry:
  %vector = alloca %int_array, align 8
  call void @int_array_create_empty(ptr %vector)
  %vector_2 = alloca %int_array, align 8
  call void @int_array_create_copy(ptr %vector_2, ptr %vector)
  call void @int_array_delete_array(ptr %vector)
  ret i64 0
}