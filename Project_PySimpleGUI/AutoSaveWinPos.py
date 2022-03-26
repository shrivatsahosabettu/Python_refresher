import PySimpleGUI as sg


layout = [[sg.T('Window that Auto-saves position', font='_ 25')],
          [sg.B('Ok'), sg.B('Exit')]]

window = sg.Window('Auto-saves Location', layout, enable_close_attempted_event=True,
                   location=sg.user_settings_get_entry('-location-', (None, None)))


while True:
    event, values = window.read()
    print(event, values)
    if event in ('Exit', sg.WINDOW_CLOSE_ATTEMPTED_EVENT):
        sg.user_settings_set_entry('-location-', window.current_location())
        break

window.close()