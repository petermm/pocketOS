alias UI.Terminal.TermWidgetState

defmodule UI.Terminal.TermWidget do
  @enforce_keys [:name]
  defstruct [:name, :x, :y, :width, :height]

  @bg_color 0xFFFFFF
  @state_struct TermWidgetState

  def can_be_focused?(_widget, _state), do: true
  def state_struct_module(), do: @state_struct

  def render(term_widget, name, ui_state, origin_x, origin_y, acc) do
    %__MODULE__{x: x, y: y} = term_widget

    %{buffer: buffer, cursor_col_pos: cursor_col_pos, accepting_input: accepting_input} =
      ui_state[name]

    x_pos = origin_x + x
    y_pos = origin_y + y

    row_count = length(buffer)
    last_row = max(row_count - 1, 0)

    {_counter, list} =
      Enum.reduce(buffer, {last_row, acc}, fn text, {row_index, rows_acc} ->
        {row_index - 1,
         [
           {:text, x_pos, y_pos + 16 * row_index, :default16px, 0x000000, @bg_color, text}
           | rows_acc
         ]}
      end)

    cursor_color =
      if ui_state[:"$focused_item"] == name do
        if accepting_input do
          0x000000
        else
          0x808080
        end
      else
        0x202020
      end

    [{:rect, x_pos + cursor_col_pos * 8, y_pos + last_row * 16, 8, 16, cursor_color} | list]
  end
end

defmodule UI.Terminal.TermWidgetState do
  defstruct buffer: [""],
            cursor_col_pos: 0,
            accepting_input: false

  def update_property(%__MODULE__{} = s, property, value) do
    case property do
      :buffer ->
        %__MODULE__{s | buffer: value}

      :cursor_col_pos ->
        %__MODULE__{s | cursor_col_pos: value}

      :accepting_input ->
        %__MODULE__{s | accepting_input: value}
    end
  end

  def handle_input(term_state, {:keyboard, :down, code}, _ts) when is_integer(code) do
    {term_state, [event: {:got_char, code}]}
  end

  def handle_input(text_input_state, event, ts) do
    PhotonUI.UIServer.default_input_handler(text_input_state, event, ts)
  end
end
