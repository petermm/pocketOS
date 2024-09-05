alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.HorizontalLayout
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.UIServer

defmodule UI.Settings do
  @ui [
    vl: %VerticalLayout{
      x: 5,
      y: 5,
      width: 315,
      height: 235,
      spacing: 8,
      children: [
        essid_layout: %HorizontalLayout{
          x: 0,
          y: 0,
          width: 315,
          height: 16,
          children: [
            essid_label: %Text{
              text: "ESSID: ",
              x: 0,
              y: 0,
              width: byte_size("ESSID: ") * 8,
              height: 16
            },
            essid_input: %TextInput{
              x: 0,
              y: 0,
              width: 60,
              height: 16
            }
          ]
        },
        password: %HorizontalLayout{
          x: 0,
          y: 0,
          height: 16,
          width: 315,
          children: [
            password_label: %Text{
              text: "Password:",
              x: 0,
              y: 0,
              width: byte_size("Password: ") * 8,
              height: 16
            },
            password_input: %TextInput{
              x: 0,
              y: 0,
              width: 60,
              height: 16
            }
          ]
        },
        ok_button: %Button{
          text: "Ok",
          x: 0,
          y: 0,
          width: 64,
          height: 32
        }
      ]
    },
    counter_input: %TextInput{
      x: 156,
      y: 112,
      width: 100,
      height: 16
    }
  ]

  def start_link(args, opts) do
    PhotonUI.UIServer.start_link(__MODULE__, args, opts)
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

  def handle_info({:update_counter, n}, ui, state) do
    :erlang.send_after(1000, self(), {:update_counter, n + 1})

    updated_ui =
      UIServer.begin_widget_state_update(ui)
      |> UIServer.update_property!(:counter_input, :text, n)
      |> UIServer.apply_widget_state_update(ui)

    {:noreply, updated_ui, state}
  end

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, _ui, state) do
    :erlang.send_after(1000, self(), {:update_counter, 0})
    {:noreply, state}
  end

  def handle_event(:ok_button, :clicked, ui, state) do
    IO.puts("Got click. ESSID: #{UIServer.get_property!(ui, :essid_input, :text)}")
    {:noreply, state}
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end
end
