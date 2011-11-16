class ArborageController < ApplicationController

  require './lib/refinery/refinery'
  require './lib/storage/storage'

  def receive
    ref = Refinery::refine(params[:raw])
    Storage::store(ref)
  end

end
