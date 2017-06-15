require 'ffi'
require 'json'
require 'pandoc_rb/return'
require 'pandoc_rb/string'
require 'pandoc_rb/version'

require 'pry'

module PandocRB
  class Error < Exception
  end
end

module PandocRB
  class ParseFailure < PandocRB::Error
    def initialize(json)
      self.etc = json
    end
  end
end

module PandocRB
  class ParsecError < PandocRB::Error
    attr_accessor :input, :source_name, :line, :column, :messages

    def initialize(json)
      self.input, parse_error = json
      source_pos, self.messages = parse_error
      self.source_name, self.line, self.column = source_pos
    end
  end
end

module PandocRB
  extend FFI::Library
  unless File.exist? File.expand_path("Text_Pandoc_C.so")
    raise "File does not exist"
  end
  ffi_lib File.expand_path("Text_Pandoc_C.so")
  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :convert_hs, [PandocRB::String, PandocRB::String, PandocRB::String], PandocRB::Return
  attach_function :freeResult, [:pointer], :void
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

      result_pointer  = self.convert_hs in_format, out_format, input
      success, result = PandocRB::Return.get_result result_pointer
      unless success
        if    /^Unknown reader: / === result
          raise ArgumentError, result
        elsif /^Unknown writer: / === result
          raise ArgumentError, result
        end
        result = JSON.parse result
        if    result['tag'] == 'ParseFailure'
          raise PandocRB::ParseFailure.new(result['contents'])
        elsif result['tag'] == 'ParsecError'
          raise PandocRB::ParsecError.new( result['contents'])
        else
          raise "Unknown error type returned from pandoc: #{result}"
        end
      end
      return [success, result]
    end
  end

end


class PandocRB::Return
  def self.get_result(result_pointer)
    result     = self.new result_pointer
    result_str = result[:str_ptr].read_string_length result[:length]
    result_str.force_encoding "utf-8"
    success    = result[:success]
    p 'freeing'
    PandocRB.freeResult result_pointer
    p 'freed'
    [success == 1, result_str]
  end
end

