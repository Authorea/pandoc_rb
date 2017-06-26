#include <stdio.h>
#include "ruby.h"
#include "HsFFI.h"
#include "PandocRb_stub.h"

VALUE PandocRb = Qnil;

void Init_pandoc_rb();

// a global designating whether the haskell runtime has been initialized
int hs_inited = 0;

VALUE method_convert_init(VALUE self);
VALUE method_convert_exit(VALUE self);
VALUE method_convert_raw(VALUE self, VALUE in_format, VALUE out_format, VALUE input, VALUE extract_media_path);

void Init_pandoc_rb() {
  PandocRb = rb_define_module("PandocRb");
  rb_define_module_function(PandocRb, "convert_init", method_convert_init, 0);
  rb_define_module_function(PandocRb, "convert_exit", method_convert_exit, 0);
  rb_define_module_function(PandocRb, "convert_raw" , method_convert_raw , 4);
}

VALUE method_convert_init(VALUE self) {
  if (hs_inited) {
    printf("pandoc_rb warning: Called convert_init after init\n");
  } else {
    hs_init(NULL, NULL);
    hs_inited = 1;
  }
  return Qnil;
}

VALUE method_convert_exit(VALUE self) {
  if (hs_inited) {
    hs_exit();
    hs_inited = 0;
  } else {
    printf("pandoc_rb warning: Called convert_exit before init\n");
  }
  return Qnil;
}

// Only checks whether inputs are strings, has no defaults and niether inits nor exits
VALUE method_convert_raw(VALUE self, VALUE in_format, VALUE out_format, VALUE input, VALUE extract_media_path) {
  char * in_format_ptr;
  char * out_format_ptr;
  char * input_ptr;
  char * extract_media_path_ptr;
  long in_format_len;
  long out_format_len;
  long input_len;
  long extract_media_path_len;

  void * output_ptr;

  VALUE success;
  VALUE result_str;
  VALUE result_pair;

  SafeStringValue(in_format);
  SafeStringValue(out_format);
  SafeStringValue(input);
  SafeStringValue(extract_media_path);
  in_format_ptr          = StringValuePtr(in_format);
  out_format_ptr         = StringValuePtr(out_format);
  input_ptr              = StringValuePtr(input);
  extract_media_path_ptr = StringValuePtr(extract_media_path);
  in_format_len          = RSTRING_LEN(in_format);
  out_format_len         = RSTRING_LEN(out_format);
  input_len              = RSTRING_LEN(input);
  extract_media_path_len = RSTRING_LEN(extract_media_path);

  output_ptr = convert_hs(in_format_ptr,          in_format_len,
                          out_format_ptr,         out_format_len,
                          input_ptr,              input_len,
                          extract_media_path_ptr, extract_media_path_len);

  RB_GC_GUARD(in_format);
  RB_GC_GUARD(out_format);
  RB_GC_GUARD(input);
  RB_GC_GUARD(extract_media_path);

  if (result_success(output_ptr)) {
    success = Qtrue;
  } else {
    success = Qfalse;
  }
  result_str = rb_utf8_str_new(result_ptr(output_ptr), result_len(output_ptr));

  free_result(output_ptr);

  result_pair = rb_ary_new_from_args(2, success, result_str);

  RB_GC_GUARD(success);
  RB_GC_GUARD(result_str);

  return result_pair;
}     

