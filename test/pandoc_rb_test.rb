require 'test_helper'
require 'pry'
require 'json'
require 'open-uri'
require 'base64'


def valid_utf8?(char_num)
  !(/[^[:print:]]/ === [char_num].pack("U*")) rescue false
end

class Enumerator
  def build_ranges
    Enumerator.new do |y|
      y << Range.new(*self.reduce([]) do |state, x|
        last_a, last_b = state
        if last_a.nil?
          [x     , x]
        elsif last_b + 1 == x || last_b == x
          [last_a, x]
        else
          y << (last_a..last_b)
          [x     , x]
        end
      end) rescue nil
    end
  end
end

class Range
  def filter(&block)
    self.lazy.select(&block).build_ranges
  end
end

def big_list_of_naughty_strings
  @big_list_of_naughty_strings ||= JSON.parse(open('https://raw.githubusercontent.com/minimaxir/big-list-of-naughty-strings/master/blns.base64.json').read).map do |encoded|
    Base64.decode64(encoded).force_encoding('UTF-8')
  end
end


class PandocRbTest < Minitest::Test

  # test_unicode_preserved
  (1..105).each do |n|
    define_method "test_unicode_preserved_#{n}".to_sym do
      ((n-1)*10_000..n*10_000).filter{|x| valid_utf8?(x)}.each do |range|
        range.each do |char_num|
          str = [char_num].pack("U*")
          result_str = PandocRb.convert 'html', 'html', str
          assert_equal Encoding::UTF_8, result_str.encoding
        end
      end
    end
  end

  # test_conversions_working_for_format
  (PandocRb::Readers & PandocRb::Writers).each do |format|
    define_method "test_conversions_working_for_#{format}".to_sym do
      big_list_of_naughty_strings.each do |str|
        begin
          first_conversion = PandocRb.convert format, format, str
          assert_equal Encoding::UTF_8, first_conversion.encoding
          second_conversion = PandocRb.convert format, format, first_conversion
          assert_equal Encoding::UTF_8, second_conversion.encoding
          unless first_conversion == second_conversion
            third_conversion = PandocRb.convert format, format, second_conversion
            assert_equal Encoding::UTF_8, third_conversion.encoding
          end
        rescue PandocRb::Error
        end
      end
    end
  end

  def test_that_it_has_a_version_number
    refute_nil ::PandocRb::VERSION
  end

  def test_that_it_has_a_readers_list
    refute_nil ::PandocRb::Readers
  end

  def test_that_it_has_a_writers_list
    refute_nil ::PandocRb::Writers
  end

  def test_consistent_results
    100.times do
      in_format_str = "markdown"
      out_format_str = "latex"
      input_str  = "hi there\nhow's it going?"
      output_str = "hi there how's it going?"
      assert_equal output_str, PandocRb::convert(in_format_str, out_format_str, input_str)
    end
  end

  def test_run_haskell_tests
    assert system('stack test')
  end

  def test_invalid_reader
    reader_error = assert_raises ArgumentError do
      PandocRb.convert('invalidReader', 'latex', "")
    end
    assert_equal 'Unknown reader: invalidReader', reader_error.message
  end

  def test_invalid_writer
    writer_error = assert_raises ArgumentError do
      PandocRb.convert('latex', 'invalidWriter', "")
    end
    assert_equal 'Unknown writer: invalidWriter', writer_error.message
  end

  def test_syntax_error
    parse_error = assert_raises PandocRb::ParsecError do
      PandocRb.convert('latex', 'latex', "}")
    end
    assert_equal "}"     , parse_error.input
    assert_equal "source", parse_error.source_name
    assert_equal 1       , parse_error.line
    assert_equal 1       , parse_error.column
    assert_equal 70      , parse_error.messages.length
  end

  def test_previous_known_failure
    [ ["odt",  "odt",  "UG93ZXLZhNmP2YTZj9i12ZHYqNmP2YTZj9mE2LXZkdio2Y/Ysdix2Ysg4KWj\nIOClo2gg4KWjIOCl\n"],
      ["opml", "opml", "UG93ZXLZhNmP2YTZj9i12ZHYqNmP2YTZj9mE2LXZkdio2Y/Ysdix2Ysg4KWj\nIOClo2gg4KWjIOCl\n"],
      ["org",  "org",  "UG93ZXLZhNmP2YTZj9i12ZHYqNmP2YTZj9mE2LXZkdio2Y/Ysdix2Ysg4KWj\nIOClo2gg4KWjIOCl\n"]
    ].each do |format, encoded_str|
      begin
        PandocRb.convert format, format, Base64.decode64(encoded_str).force_encoding('UTF-8')
        GC.start
      rescue PandocRb::Error
        GC.start
      end
    end
  end

end

