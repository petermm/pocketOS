alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.ButtonState
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.HorizontalLayout
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.Widgets.TextInputState
alias PhotonUI.Widgets.VerticalLayout

defmodule PhotonUI.Widgets.Button do
  defstruct [:text, :x, :y, :width, :height]
end

defmodule PhotonUI.Widgets.ButtonState do
  defstruct state: :normal

  def handle_input(button, {:mouse, :pressed, :left, _x, _y}, _ts) do
    %ButtonState{button | state: :pressed}
  end

  def handle_input(button, {:mouse, :released, :left, _x, _y}, _ts) do
    {%ButtonState{button | state: :released}, [event: :clicked]}
  end

  def handle_input(button, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    %ButtonState{button | state: :pressed}
  end

  def handle_input(button, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    {%ButtonState{button | state: :released}, [event: :clicked]}
  end

  def handle_input(button, event, ts) do
    PhotonUI.UIServer.default_input_handler(button, event, ts)
  end
end

defmodule PhotonUI.Widgets.Container do
  defstruct [:x, :y, :width, :height, :children]
end

defmodule PhotonUI.Widgets.HorizontalLayout do
  defstruct [:x, :y, :width, :height, :children, spacing: 0]
end

defmodule PhotonUI.Widgets.Text do
  defstruct [:x, :y, :width, :height, :text]
end

defmodule PhotonUI.Widgets.TextInput do
  defstruct [:x, :y, :width, :height]
end

defmodule PhotonUI.Widgets.TextInputState do
  defstruct text: "",
            cursor_pos: 0

  def update_property(%TextInputState{} = s, property, value) do
    case property do
      :text ->
        %TextInputState{s | text: to_string(value), cursor_pos: 0}
    end
  end

  def handle_input(_text_input_state, enter, _ts)
      when enter in [{:keyboard, :down, 10}, {:keyboard, :down, 13}] do
    :none
  end

  def handle_input(_text_input_state, enter, _ts)
      when enter in [{:keyboard, :up, 10}, {:keyboard, :up, 13}] do
    :release_focus
  end

  def handle_input(text_input_state, {:keyboard, :down, code}, _ts) do
    %TextInputState{text: old_text, cursor_pos: old_cursor_pos} = text_input_state

    {updated_cursor_pos, updated_text} = update_text(old_text, code, old_cursor_pos)

    {%TextInputState{text_input_state | text: updated_text, cursor_pos: updated_cursor_pos},
     [event: {:text_input, updated_text}]}
  end

  def handle_input(text_input_state, event, ts) do
    PhotonUI.UIServer.default_input_handler(text_input_state, event, ts)
  end

  defp update_text(text, char, cursor_pos) do
    case char do
      8 ->
        case text do
          <<pre::binary-size(cursor_pos - 1), _remove::size(8), rest::binary>> ->
            {cursor_pos - 1, <<pre::binary, rest::binary>>}

          <<>> ->
            {0, <<>>}
        end

      _ ->
        <<pre::binary-size(cursor_pos), rest::binary>> = text
        {cursor_pos + 1, <<pre::binary, char::size(8), rest::binary>>}
    end
  end
end

defmodule PhotonUI.Widgets.VerticalLayout do
  defstruct [:x, :y, :width, :height, :children, spacing: 0]
end

defmodule PhotonUI.UIServer do
  require Record
  Record.defrecord(:ui_server_state, [:module, :ui, :custom_state])

  @compile {:no_warn_undefined, :port}

  @bg_color 0xFFFFFF

  def start_link(module, args, opts) do
    :avm_scene.start_link(__MODULE__, [{module, opts} | args], opts)
  end

  def init([{module, opts} | args]) do
    {:port, disp} = opts[:display_server]
    :port.call(disp, {:subscribe_input, :all})

    case opts[:keyboard_server] do
      nil ->
        :ok

      server ->
        IO.puts("Subscribed to keyboard server.")
        :gen_server.call(server, {:subscribe_input, :all})
    end

    {:ok, {widgets, _initial_widget_state}, custom_state} = module.init(args)

    focus_list = build_focus_list(widgets)
    focused_item = Enum.at(focus_list, 0)

    mouse_area_list = build_mouse_area_list(widgets)

    # TODO: merge with provided widget initial state
    initial_state =
      make_initial_state(widgets, %{
        width: opts[:width],
        height: opts[:height],
        visible: false,
        "$focused_item": focused_item,
        "$focus_list": focus_list,
        "$mouse_area_list": mouse_area_list
      })

    s =
      ui_server_state(
        module: module,
        ui: {widgets, initial_state},
        custom_state: custom_state
      )

    {:ok, s}
  end

  def handle_call({:ui_server, :show}, _from, state) do
    module = ui_server_state(state, :module)
    {widgets, widget_state} = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    shown_ui = {widgets, Map.put(widget_state, :visible, true)}

    case module.handle_event(:ui, :shown, shown_ui, custom_state) do
      {:noreply, new_custom_state} ->
        {:reply, :ok, shown_ui, new_custom_state}

      {:noreply, new_ui, new_custom_state} ->
        {:reply, :ok, new_ui, new_custom_state}
    end
    |> to_avm_scene_result(state)
  end

  def handle_call(msg, from, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_call(msg, from, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_cast(msg, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_cast(msg, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_info(msg, state) do
    module = ui_server_state(state, :module)
    ui = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    module.handle_info(msg, ui, custom_state)
    |> to_avm_scene_result(state)
  end

  def handle_input(event_data, ts, _pid, state) do
    {_widgets, widget_state} = ui_server_state(state, :ui)

    case event_data do
      {:mouse, _mouse_evt, _button, x, y} ->
        # TODO: propagate widget coordinates instead of screen coordinates
        # this can be easily done by subtracting hit mouse area x and y
        find_mouse_area(widget_state[:"$mouse_area_list"], x, y)
        |> dispatch_input(event_data, ts, state)

      {:keyboard, _up_down, _code} ->
        widget_state[:"$focused_item"]
        |> dispatch_input(event_data, ts, state)

      _ ->
        {:noreply, state}
    end
  end

  defp dispatch_input(widget_name, event_data, ts, state) do
    module = ui_server_state(state, :module)
    {widgets, widget_state} = ui_server_state(state, :ui)
    custom_state = ui_server_state(state, :custom_state)

    case widget_state[widget_name] do
      nil ->
        {:noreply, state}

      %wdg_state_type{} = wdg_state ->
        case wdg_state_type.handle_input(wdg_state, event_data, ts) do
          %^wdg_state_type{} = new_wdg_state ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)
            display_list = render(widgets, new_widget_state)

            {:noreply, ui_server_state(state, ui: {widgets, new_widget_state}),
             [push: display_list]}

          {%^wdg_state_type{} = new_wdg_state, [event: wdg_event]} ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)

            case module.handle_event(
                   widget_name,
                   wdg_event,
                   {widgets, new_widget_state},
                   custom_state
                 ) do
              {:noreply, new_custom_state} ->
                {:noreply, {widgets, new_widget_state}, new_custom_state}

              {:noreply, _new_ui, _new_custom_state} = t ->
                t
            end
            |> to_avm_scene_result(state)

          {%^wdg_state_type{} = new_wdg_state, :release_focus} ->
            new_widget_state = Map.put(widget_state, widget_name, new_wdg_state)
            focus_next(ui_server_state(ui: {widgets, new_widget_state}))

          :release_focus ->
            focus_next(state)

          :none ->
            {:noreply, state}
        end

      _other ->
        {:noreply, state}
    end
  end

  def default_input_handler(_widget, event, _ts) do
    case event do
      # SDL down arrow
      {:keyboard, :up, 274} -> :release_focus
      _ -> :none
    end
  end

  defp focus_next(state) do
    {items, widget_state} = ui_server_state(state, :ui)

    new_focused_item =
      find_next_focusable(widget_state[:"$focus_list"], widget_state[:"$focused_item"])

    new_state = Map.put(widget_state, :"$focused_item", new_focused_item)

    display_list = render(items, new_state)

    {:noreply, ui_server_state(state, ui: {items, new_state}), [push: display_list]}
  end

  defp make_initial_state([], acc) do
    acc
  end

  defp make_initial_state([{name, %Button{}} | t], acc) do
    make_initial_state(t, Map.put(acc, name, %ButtonState{}))
  end

  defp make_initial_state([{name, %TextInput{}} | t], acc) do
    make_initial_state(t, Map.put(acc, name, %TextInputState{}))
  end

  defp make_initial_state([{_name, %{children: _children} = container} | t], acc) do
    children_acc = make_initial_state(container.children, acc)
    make_initial_state(t, children_acc)
  end

  defp make_initial_state([{_name, _item} | t], acc) do
    make_initial_state(t, acc)
  end

  defp items_to_list([], _state, _origin_x, _origin_y, acc) do
    acc
  end

  defp items_to_list([{name, %Button{} = item} | t], state, origin_x, origin_y, acc) do
    %Button{text: text, x: x, y: y, width: width, height: height} = item

    x_pos = origin_x + x
    y_pos = origin_y + y

    # assuming default 16 px
    text_width = byte_size(text) * 8

    button_color =
      case state[name][:state] do
        :pressed ->
          0x0000FF

        _ ->
          if state[:"$focused_item"] == name do
            0x8080FF
          else
            0xFFFFFF
          end
      end

    list = [
      {:text, x_pos + 1 + div(width - text_width, 2), y_pos + div(height - 16, 2), :default16px,
       0x000000, button_color, text},
      {:rect, x_pos + 1, y_pos + 1, width - 2, height - 2, button_color},
      {:rect, x_pos, y_pos, width, height, 0x000000}
      | acc
    ]

    items_to_list(t, state, origin_x, origin_y, list)
  end

  defp items_to_list(
         [{_name, %Text{text: text, x: x, y: y}} | t],
         state,
         origin_x,
         origin_y,
         acc
       ) do
    display_item = {:text, origin_x + x, origin_y + y, :default16px, 0x000000, @bg_color, text}
    list = [display_item | acc]
    items_to_list(t, state, origin_x, origin_y, list)
  end

  defp items_to_list(
         [{name, %TextInput{} = item} | t],
         state,
         origin_x,
         origin_y,
         acc
       ) do
    %{x: x, y: y} = item
    %{cursor_pos: cursor_pos, text: text} = state[name]

    x_pos = origin_x + x
    y_pos = origin_y + y

    list = [
      {:text, x_pos, y_pos, :default16px, 0x000000, @bg_color, text}
      | acc
    ]

    maybe_focused_list =
      if state[:"$focused_item"] == name do
        [{:rect, x_pos + cursor_pos * 8, y_pos, 2, 16, 0x000000} | list]
      else
        list
      end

    items_to_list(t, state, origin_x, origin_y, maybe_focused_list)
  end

  defp items_to_list(
         [{_name, %Container{children: children, x: x, y: y}} | t],
         state,
         origin_x,
         origin_y,
         acc
       ) do
    children_list = items_to_list(children, state, origin_x + x, origin_y + y, [])
    items_to_list(t, state, origin_x, origin_y, children_list ++ acc)
  end

  defp items_to_list(
         [{_name, %HorizontalLayout{children: children, x: x, y: y, spacing: spacing}} | t],
         state,
         origin_x,
         origin_y,
         acc
       ) do
    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_x}, fn {_wn, %{x: wx, width: ww}} = item,
                                                {rendered, x_off} ->
        {items_to_list([item], state, x_off + x, origin_y + y, rendered),
         x_off + wx + ww + spacing}
      end)

    items_to_list(t, state, origin_x, origin_y, acc_with_children)
  end

  defp items_to_list(
         [{_name, %VerticalLayout{children: children, x: x, y: y, spacing: spacing}} | t],
         state,
         origin_x,
         origin_y,
         acc
       ) do
    {acc_with_children, _final_off} =
      Enum.reduce(children, {acc, origin_y}, fn {_wn, %{y: wy, height: wh}} = item,
                                                {rendered, y_off} ->
        {items_to_list([item], state, origin_x + x, y_off + y, rendered),
         y_off + wy + wh + spacing}
      end)

    items_to_list(t, state, origin_x, origin_y, acc_with_children)
  end

  def build_mouse_area_list(widgets) do
    build_mouse_area_list(widgets, 0, 0, [])
  end

  def build_mouse_area_list([], _origin_x, _origin_y, acc) do
    Enum.reverse(acc)
  end

  def build_mouse_area_list([{item_name, item} | t], origin_x, origin_y, acc) do
    case item do
      %HorizontalLayout{children: children, x: x, y: y, spacing: spacing} ->
        {acc_with_children, _final_off} =
          Enum.reduce(children, {acc, origin_x}, fn {_wn, %{x: wx, width: ww}} = item,
                                                    {mouse_area_acc, x_off} ->
            {build_mouse_area_list([item], x_off + x, origin_y + y, mouse_area_acc),
             x_off + wx + ww + spacing}
          end)

        build_mouse_area_list(t, origin_x, origin_y, acc_with_children)

      %VerticalLayout{children: children, x: x, y: y, spacing: spacing} ->
        {acc_with_children, _final_off} =
          Enum.reduce(children, {acc, origin_y}, fn {_wn, %{y: wy, height: wh}} = item,
                                                    {mouse_area_acc, y_off} ->
            {build_mouse_area_list([item], origin_x + x, y_off + y, mouse_area_acc),
             y_off + wy + wh + spacing}
          end)

        build_mouse_area_list(t, origin_x, origin_y, acc_with_children)

      %{children: children, x: x, y: y} ->
        children_focus_list = build_mouse_area_list(children, origin_x + x, origin_y + y, acc)
        build_mouse_area_list(t, origin_x, origin_y, children_focus_list)

      %focusable_type{x: x, y: y, width: width, height: height}
      when focusable_type in [Button, TextInput] ->
        build_mouse_area_list(t, origin_x, origin_y, [
          {item_name, origin_x + x, origin_y + y, width, height} | acc
        ])

      _ ->
        build_mouse_area_list(t, origin_x, origin_y, acc)
    end
  end

  def find_mouse_area(mouse_area_list, x, y) do
    ### TODO: find/2 !!!
    {widget_name, _w_x, _w_y, _widget_width, _widget_height} =
      Enum.find(mouse_area_list, {nil, 0, 0, 0, 0}, fn {_wn, wx, wy, ww, wh} ->
        x >= wx and x < wx + ww and y >= wy and y < wy + wh
      end)

    widget_name
  end

  def build_focus_list(widgets) do
    build_focus_list(widgets, [])
  end

  def build_focus_list([], acc) do
    Enum.reverse(acc)
  end

  def build_focus_list([{item_name, item} | t], acc) do
    case item do
      %{children: children} ->
        children_focus_list = Enum.reverse(build_focus_list(children, []))
        build_focus_list(t, children_focus_list ++ acc)

      %focusable_type{} when focusable_type in [Button, TextInput] ->
        build_focus_list(t, [item_name | acc])

      _ ->
        build_focus_list(t, acc)
    end
  end

  def find_next_focusable(focus_list, focused) do
    index = Enum.find_index(focus_list, fn item -> item == focused end)

    case Enum.at(focus_list, index + 1) do
      nil -> Enum.at(focus_list, 0)
      next -> next
    end
  end

  defp to_avm_scene_result(result, state) do
    case result do
      {:reply, reply, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:reply, reply, updated_state, [{:push, rendered_items}]}

      {:reply, reply, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:reply, reply, updated_state}

      {:noreply, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:noreply, updated_state, [{:push, rendered_items}]}

      {:noreply, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:noreply, updated_state}

      {:stop, reason, {new_widgets, new_widget_state}, custom_state} ->
        {updated_state, rendered_items} =
          update_ui_and_state(state, new_widgets, new_widget_state, custom_state)

        {:stop, reason, updated_state, [{:push, rendered_items}]}

      {:stop, reason, custom_state} ->
        updated_state = ui_server_state(state, custom_state: custom_state)
        {:stop, reason, updated_state}
    end
  end

  defp update_ui_and_state(old_state, new_widgets, new_widget_state, custom_state) do
    rendered_items = render(new_widgets, new_widget_state)

    updated_state =
      ui_server_state(old_state, ui: {new_widgets, new_widget_state}, custom_state: custom_state)

    {updated_state, rendered_items}
  end

  def begin_widget_state_update({_widgets, widget_state}) do
    widget_state
  end

  def update_property!(widget_state, name, property, value)
      when is_map(widget_state) and is_atom(property) do
    %{^name => %widget_type{} = individual_widget_state} = widget_state

    updated_individual_widget_state =
      widget_type.update_property(individual_widget_state, :text, value)

    %{widget_state | name => updated_individual_widget_state}
  end

  def apply_widget_state_update(updated_widget_state, {widgets, _widget_state}) do
    {widgets, updated_widget_state}
  end

  def get_widget_state({_widgets, widget_state}) do
    widget_state
  end

  def get_property!(ui, name, property) when is_atom(property) do
    case ui do
      {_widgets, %{^name => %{^property => value}}} ->
        value

      {widgets, _widget_state} ->
        %_struct{^property => value} = find_widget(widgets, name)
        value
    end
  end

  def find_widget({widgets, _widgets_name}, name) do
    find_widget(widgets, name)
  end

  def find_widget([], _name) do
    nil
  end

  def find_widget([{name, widget} | _t], name) do
    widget
  end

  def find_widget([_widget_name, %{children: children} | t], name) do
    case find_widget(children, name) do
      nil -> find_widget(t, name)
      found_in_children -> found_in_children
    end
  end

  def find_widget([{_widget_name, _widget} | t], name) do
    find_widget(t, name)
  end

  def render(widgets, widget_state) do
    if widget_state[:visible] do
      items_to_list(widgets, widget_state, 0, 0, [
        {:rect, 0, 0, widget_state[:width], widget_state[:height], @bg_color}
      ])
    else
      []
    end
  end

  # Public API
  def show(ui_server) do
    :gen_server.call(ui_server, {:ui_server, :show})
  end
end
