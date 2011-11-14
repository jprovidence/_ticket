class Detritus < ActiveRecord::Base

  class << self

    def pull(amt)

      rec = case amt
              when :large    then order('id ASC').limit(1000).find_all_by_processed(0)
              when :stardard then order('id ASC').limit(100).find_all_by_processed(0)
              else []
            end
      
      rec.map {|r| r.link}

    end

  end

end
