; ModuleID = 'test'
source_filename = "test"

%test_array = type { ptr, i64, i64, i64 }

declare ptr @malloc(i64)

declare void @free(ptr)

declare ptr @memcpy(ptr, ptr, i64)

; Function Attrs: nounwind
define fastcc void @test_array_create_empty(ptr %0) #0 {
entry:
  %buffer_ptr = getelementptr %test_array, ptr %0, i64 0, i32 0
  %length_ptr = getelementptr %test_array, ptr %0, i64 0, i32 1
  %maxlength_ptr = getelementptr %test_array, ptr %0, i64 0, i32 2
  %factor_ptr = getelementptr %test_array, ptr %0, i64 0, i32 3
  store ptr null, ptr %buffer_ptr, align 8
  store i64 0, ptr %length_ptr, align 4
  store i64 0, ptr %maxlength_ptr, align 4
  store i64 16, ptr %maxlength_ptr, align 4
  ret void
}

define ptr @test_array_get_buffer(ptr %0) {
entry:
  %buffer_ptr = getelementptr %test_array, ptr %0, i64 0, i32 0
  %buffer = load ptr, ptr %buffer_ptr, align 8
  ret ptr %buffer
}

; Function Attrs: nounwind
define fastcc void @test_array_delete_array(ptr %0) #0 {
entry:
  %buffer = call ptr @test_array_get_buffer(ptr %0)
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
; %vector = alloca %test_array, align 8
;   call void @test_array_create_empty(ptr %vector)
;   call void @test_array_delete_array(ptr %vector)
;   ret i64 0
; }

attributes #0 = { nounwind }