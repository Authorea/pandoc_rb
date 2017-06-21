require 'pandoc_rb' # the C version
require 'json'
require 'pandoc-rb/error'
require 'pandoc-rb/parse_failure'
require 'pandoc-rb/parsec_error'
require 'pandoc-rb/readers'
require 'pandoc-rb/version'
require 'pandoc-rb/writers'
require 'timeout'


module PandocRb
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
      self.convert_init
      Kernel.at_exit do
        PandocRb.convert_exit
      end
      @PandocRb_loaded = true
    end

    if in_format_str.to_s == 'docx'
      input_str.force_encoding("UTF-8")
    end

    begin
      in_format          = in_format_str.to_s.encode "UTF-8"
      out_format         = out_format_str.to_s.encode "UTF-8"
      input              = input_str.to_s.encode "UTF-8"
      extract_media_path = extract_media_path.to_s.encode "UTF-8"
      success, result    = PandocRb.convert_raw in_format, out_format, input, extract_media_path
      result.force_encoding "utf-8"

      self.raise_exception result unless (success == 1)
      result
    end
  end

end

