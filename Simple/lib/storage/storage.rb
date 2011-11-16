module Storage

  require './lib/neosql'

  def self.store(data)
    
    if is_raw? data
      store_raw data
    end

  end


  def self.store_raw(data)

    desc_tag = description_tag(data)
    root     = find_or_create(:type => "root", 
                              :url => data[:root])

    root_idx = NeoSQL::Index.get_index("root") || NeoSQL::Index.create("root")
    root_idx.add(root, "url", data[:root])

    data[:content].each do |entry|

      entry.merge!(:type => "entry",
                   :url  => entry[data[:link_tag]])

      links = links_in(desc_tag, entry)
      node  = find_or_create(entry)

      node_idx = NeoSQL::Index.get_index("article") || NeoSQL::Index.create("article")
      node_idx.add(node, "url", entry[data[:link_tag]])

      set_relations_given(node, links)
      root.relationship_with(node, "content")

    end

  end


  def self.find_or_create(hash)

    case hash[:type]

    when "root"

      nary = NeoSQL::Index.find_exact("root", "url", hash[:url])

      if nary.empty?
        node = NeoSQL::Node.new
        node.set(hash)
        return node
      else
        return nary[0]
      end

    when "entry"

      nary = NeoSQL::Index.find_exact("article", "url", hash[:url])

      if nary.empty?
        node = NeoSQL::Node.new
        node.set(hash)
        return node
      else

        node = nary[0]

        case node.type
        when "placeholder"
          node.update(hash)
          return node
        else
          return node
        end

      end

    else
      return nil
    end
          
  end


  def self.set_relations_given(node, links)

    links.group_by {|l| l}.each do |link, ary|
      
      idx = NeoSQL::Index.find_exact("article", "url", link)

      if idx.empty?
        new_node = NeoSQL::Node.new
        new_node.set({:type => "placeholder", :url => link})
        node.relationship_with(new_node, "hyperlink", {:strength => ary.length.to_s})
      else
        node.relationship_with(idx[0], "hyperlink", {:strength => ary.length.to_s})
      end

    end

  end


  def self.description_tag(data)
    
    key  = data.keys[0]

    tags = data[key].collect do |v|
      longest_tag(v)
    end

    tags.group_by {|e| e}.max_by {|x| x.length}.first

  end


  def self.links_in(desc_tag, entry)
    entry[desc_tag].scan(/(https?:\/\/.*?)(?:"|')/)
  end


  def self.is_raw?(data)
    if data.keys.include?(:root) then true else false end
  end

end
