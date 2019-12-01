require "spec_helper"

RSpec.describe Person do
  context "#name" do
    it "returns the correct name" do
      person = Person.new
      expect(person.name).to eq "Jane Doe"
    end
  end

  context "#age" do
    it "returns the correct age" do
      person = Person.new
      expect(person.age).to eq 40
    end
  end
end
