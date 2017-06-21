#include <stdio.h>
#include "ruby.h"
#include "HsFFI.h"
#include "PandocRb_stub.h"

VALUE PandocRb = Qnil;

void Init_pandoc_rb();

int hs_inited = 0;

VALUE method_convert_init(VALUE self);
VALUE method_convert_exit(VALUE self);
VALUE method_convert_raw(VALUE self, VALUE in_format, VALUE out_format, VALUE input, VALUE extract_media_path);

void Init_pandoc_rb() {
  PandocRb = rb_define_module("PandocRb");
  rb_define_module_function(PandocRb, "convert_init", method_convert_raw, 0);
  rb_define_module_function(PandocRb, "convert_exit", method_convert_raw, 0);
  rb_define_module_function(PandocRb, "convert_raw", method_convert_raw, 4);
}

VALUE method_convert_init(VALUE self) {
  if (hs_inited) {
    printf("pandoc-rb warning: Called convert_init after init\n");
  } else {
    hs_init(NULL, NULL);
  }
  return Qnil;
}

VALUE method_convert_exit(VALUE self) {
  if (hs_inited) {
    hs_exit();
  } else {
    printf("pandoc-rb warning: Called convert_exit before init\n");
  }
  return Qnil;
}

// Only checks whether inputs are strings, has no defaults and niether inits nor exits
VALUE method_convert_raw(VALUE self, VALUE in_format, VALUE out_format, VALUE input, VALUE extract_media_path) {
  void * in_format_ptr;
  void * out_format_ptr;
  void * input_ptr;
  void * extract_media_path_ptr;
  void * output_ptr;

  int success;
  VALUE result_str;

  hs_init(NULL, NULL);

  in_format_ptr          = new_rstringlen(RSTRING_PTR(in_format), RSTRING_LEN(in_format));
  out_format_ptr         = new_rstringlen(RSTRING_PTR(out_format), RSTRING_LEN(out_format));
  input_ptr              = new_rstringlen(RSTRING_PTR(input), RSTRING_LEN(input));
  extract_media_path_ptr = new_rstringlen(RSTRING_PTR(extract_media_path), RSTRING_LEN(extract_media_path));

  output_ptr = convert_hs(in_format_ptr, out_format_ptr, input_ptr, extract_media_path_ptr);

  success = result_success(output_ptr);
  result_str = rb_str_new(result_ptr(output_ptr), result_len(output_ptr));

  // free_rstringlen(in_format_ptr);
  // free_rstringlen(out_format_ptr);
  // free_rstringlen(input_ptr);
  // free_rstringlen(extract_media_path_ptr);

  return rb_ary_new_from_args(2, success, result_str);
}     

