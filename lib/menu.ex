alias PhotonUI.Widgets.IconGridView
alias PhotonUI.Widgets.Container
alias PhotonUI.UIServer

defmodule UI.Menu do
  @ui [
    %Container{
      name: :cont,
      x: 0,
      y: 0,
      width: 320,
      height: 240,
      children: [
        %IconGridView{
          name: :grid,
          x: 0,
          y: 0,
          height: 240,
          width: 320,
          icon_size: 64,
          cell_width: 80,
          cell_height: 100
        }
      ]
    }
  ]

  @menu_model [
    %{
      source: {:pocket_os, "icons/apps/terminal.rgba"},
      text: "ALisp",
      app: UI.Terminal
    }
  ]

  def start_link(args, opts) do
    UIServer.start_link(__MODULE__, args, opts)
  end

  def init(_opts) do
    {:ok, {@ui, %{}}, nil}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, ui, state) do
    updated_ui =
      UIServer.begin_widget_state_update(ui)
      |> UIServer.update_property!(:grid, :model, @menu_model)
      |> UIServer.apply_widget_state_update(ui)

    {:noreply, updated_ui, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end
end
