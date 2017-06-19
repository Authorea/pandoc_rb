require 'ffi'

module PandocRb
  class String < FFI::Struct
    attr_accessor :str_mem_ptr
    layout :str_ptr, :pointer,
           :length,  :long

    def self.from_str(string)
      unless string.encoding == Encoding::UTF_8
        raise ArgumentError, "Expected an input encoded with UTF-8, got one encoded with: #{string.encoding}"
      end
      new_hs_string = self.new
      new_hs_string.to_ptr.autorelease = false
      new_hs_string.str_mem_ptr = FFI::MemoryPointer.from_string string
      new_hs_string.str_mem_ptr.autorelease = false
      new_hs_string[:str_ptr] = new_hs_string.str_mem_ptr
      new_hs_string[:length]  = string.bytesize
      new_hs_string
    end

    def to_s
      self[:str_ptr]
        .read_string_length(self[:length])
        .force_encoding("utf-8")
    end
  end
end

