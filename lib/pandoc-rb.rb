require 'ffi'
require 'json'
require 'pandoc-rb/error'
require 'pandoc-rb/parse_failure'
require 'pandoc-rb/parsec_error'
require 'pandoc-rb/readers'
require 'pandoc-rb/return'
require 'pandoc-rb/string'
require 'pandoc-rb/version'
require 'pandoc-rb/writers'
require 'timeout'


module PandocRb
  extend FFI::Library
  unless File.exist? File.expand_path("#{__dir__}/Text_Pandoc_C.so")
    binding.pry
    raise "File does not exist"
  end
  ffi_lib File.expand_path("#{__dir__}/Text_Pandoc_C.so")
  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :convert_hs, [PandocRb::String, PandocRb::String, PandocRb::String, PandocRb::String], PandocRb::Return
  attach_function :hs_exit, [], :void

  def self.raise_exception(result)
    if    /^Unknown reader: / === result
      raise ArgumentError, result
    elsif /^Unknown writer: / === result
      raise ArgumentError, result
    elsif /^Pandoc timed out/ === result
      raise Timeout::Error
    elsif /^Pandoc internal / === result
      raise PandocRb::Error, result.sub(/^Pandoc internal error: /, '')
    end
    result = JSON.parse result
    if    result['tag'] == 'ParseFailure'
      raise PandocRb::ParseFailure.new(result['contents'])
    elsif result['tag'] == 'ParsecError'
      raise PandocRb::ParsecError.new( result['contents'])
    else
      raise "Unknown error type returned from pandoc: #{result}"
    end
  end

  def self.convert(in_format_str, out_format_str, input_str, extract_media_path='')
    unless @PandocRb_loaded
      self.hs_init FFI::Pointer::NULL, FFI::Pointer::NULL
      Kernel.at_exit do
        PandocRb.hs_exit
      end
      @PandocRb_loaded = true
    end

    if in_format_str.to_s == 'docx'
      input_str.force_encoding("UTF-8")
    end

    begin
      @in_format     = PandocRb::String.from_str in_format_str.to_s.encode("UTF-8")
      @out_format    = PandocRb::String.from_str out_format_str.to_s.encode("UTF-8")
      @input         = PandocRb::String.from_str input_str
      @extract_media = PandocRb::String.from_str extract_media_path.to_s.encode("UTF-8")

      @result_pointer  = self.convert_hs @in_format, @out_format, @input, @extract_media

      @result     = PandocRb::Return.new @result_pointer
      result_str = @result[:str_ptr].read_string_length @result[:length]
      result_str.force_encoding "utf-8"

      self.raise_exception result_str unless (@result[:success] == 1)
      result_str
    ensure
      @result_pointer&.autorelease       = false
      @result&.to_ptr&.autorelease       = false
      @result&.[](:str_ptr)&.autorelease = false

      @in_format&.str_mem_ptr&.free
      @out_format&.str_mem_ptr&.free
      # @input&.str_mem_ptr&.free # THIS KILLS THE PROGRAM!
      @extract_media&.str_mem_ptr&.free

      @in_format&.to_ptr&.free
      @out_format&.to_ptr&.free
      @input&.to_ptr&.free
      @extract_media&.to_ptr&.free

      # Suppress warnings
      original_verbose, $VERBOSE = $VERBOSE, nil

      @result&.[](:str_ptr)&.free
      @result_pointer&.free

      # renable warnings
      $VERBOSE = original_verbose
    end
  end

end

