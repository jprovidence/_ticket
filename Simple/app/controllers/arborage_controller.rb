class ArborageController < ApplicationController

  require 'refinery'
  require 'storage'

  def receive
    ref = Refinery::refine(params[:raw])
    Storage::store(ref)
  end 

end
