
module PandocRb
  class ParseFailure < PandocRb::Error
    attr_accessor :json

    def initialize(json)
      self.json = json
    end
  end
end

