require 'ffi'
require "pandoc_rb/version"
require 'pandoc_rb/string'
require 'pandoc_rb/return'

module PandocRB
  extend FFI::Library
  unless File.exist? File.expand_path("Text_Pandoc_C.so")
    raise "File does not exist"
  end
  ffi_lib File.expand_path("Text_Pandoc_C.so")
  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :convert_hs, [PandocRB::String, PandocRB::String, PandocRB::String], PandocRB::Return
  attach_function :freeHaskellFunPtr, [:pointer], :void
  attach_function :hs_exit, [], :void

  def self.convert(in_format_str, out_format_str, input_str)
    unless self.instance_variable_get :@PandocRB_loaded
      self.hs_init FFI::Pointer::NULL, FFI::Pointer::NULL
      Kernel.at_exit do
        PandocRB.hs_exit
      end
      self.instance_variable_set :@PandocRB_loaded, true
    end

    begin
      in_format  = PandocRB::String.from_str in_format_str
      out_format = PandocRB::String.from_str out_format_str
      input      = PandocRB::String.from_str input_str

      result_pointer = self.convert_hs in_format, out_format, input
      result         = PandocRB::Return.get_result result_pointer
      return result
    end
  end
end

class PandocRB::Return
  def self.get_result(result_pointer)
    result = self.new result_pointer
    result_str = result[:str_ptr].read_string_length result[:length]
    free_me    = FFI::Function.new :void, [:pointer], result[:free_me]
    free_me.call result_pointer
    PandocRB.freeHaskellFunPtr result[:free_me]
    [result[:success] == 1, result_str]
  end
end

