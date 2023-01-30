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

define i64 @int_array_get_length(ptr %0) {
entry:
  %length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 1
  %length = load i64, ptr %length_ptr, align 8
  ret i64 %length
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
  %maxlength_ptr = getelementptr %int_array, ptr %0, i64 0, i32 2
  store i64 %1, ptr %maxlength_ptr, align 4
  ret void
}

; Function Attrs: nounwind
define fastcc void @int_array_create_empty(ptr %0) #0 {
entry:
  %buffer_ptr = getelementptr %int_array, ptr %0, i64 0, i32 0
  %length_ptr = getelementptr %int_array, ptr %0, i64 0, i32 1
  %maxlength_ptr = getelementptr %int_array, ptr %0, i64 0, i32 2
  %factor_ptr = getelementptr %int_array, ptr %0, i64 0, i32 3
  store ptr null, ptr %buffer_ptr, align 8
  store i64 0, ptr %length_ptr, align 4
  store i64 0, ptr %maxlength_ptr, align 4
  store i64 16, ptr %factor_ptr, align 4
  ret void
}

declare void @int_array_create_copy(ptr, ptr)

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

; define i64 @main() {
; entry:
;  %vector = alloca %int_array, align 8
;  call void @int_array_create_empty(ptr %vector)
;  %vector_2 = alloca %int_array, align 8
;  call void @int_array_create_copy(ptr %vector_2, ptr %vector)
;  call void @int_array_delete_array(ptr %vector)
;  ret i64 0
; }

attributes #0 = { nounwind }