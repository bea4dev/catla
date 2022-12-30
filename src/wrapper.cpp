#include <setjmp.h>
#include <stdlib.h>
#include <stdint.h>


extern "C" void* add_jump_buffer(void* vm_thread);
extern "C" uint64_t run_function(void* vm_thread, void* module, void* function, void* arguments);


extern "C" void* create_jump_buffer_wrapped() {
    return malloc(sizeof(jmp_buf));
}

extern "C" uint64_t run_function_for_set_jump_branch(void* vm_thread, void* module, void* function1, void* argument1, void* function2, void* argument2) {
    auto* jump_buffer_field_ptr = (jmp_buf*) add_jump_buffer(vm_thread);
    volatile uint64_t result;

    if (setjmp(*jump_buffer_field_ptr) == 0) {
        result = run_function(vm_thread, module, function1, argument1);
    } else {
        result = run_function(vm_thread, module, function2, argument2);
    }

    return result;
}

extern "C" void long_jump_wrapped(void* buffer, size_t value) {
    longjmp(*(jmp_buf*)buffer, (int) value);
}