require 'ffi'

module PandocRB
  class Return < FFI::Struct
    layout :success, :int,
           :str_ptr, :pointer,
           :length,  :long
  end
end

