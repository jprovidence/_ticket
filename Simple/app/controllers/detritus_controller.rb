class DetritusController < ApplicationController

  def pull

    response = Detritus.pull(params[:size].intern)

    respond_to do |format|
      format.html {render :html => "hey"}
      format.json {render :json => response}
    end

  end


  def receive
    
    Detritus.receive(request.body)

    respond_to do 
      render :nothing => true
    end

  end

end
