class Detritus < ActiveRecord::Base

  class << self

    def receive(strio)

      acc = []

      strio.each_line do |s|
        acc << CGI::unescape(s)
      end

      acc.join("").split(",").each do |l|
        d = Detritus.new({:link => l,
                          :processed => 0})
        d.save!
      end

    end


    def pull(amt)

      rec = case amt
              when :large    then order('id ASC').limit(1000).find_all_by_processed(0)
              when :standard then order('id ASC').limit(100).find_all_by_processed(0)
              else []
            end
      
      rec.map {|r| r.link}

    end

  end

end
