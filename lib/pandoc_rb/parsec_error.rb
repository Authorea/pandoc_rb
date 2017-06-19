
module PandocRb
  class ParsecError < PandocRb::Error
    attr_accessor :input, :source_name, :line, :column, :messages

    def initialize(json)
      self.input, parse_error = json
      source_pos, self.messages = parse_error
      self.source_name, self.line, self.column = source_pos
    end
  end
end

