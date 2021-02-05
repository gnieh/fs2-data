def modules_by_type
  @items.find_all("/documentation/*/index.md").group_by do |item|
    item[:type]
  end
end
