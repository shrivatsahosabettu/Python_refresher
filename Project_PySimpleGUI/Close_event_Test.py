import PySimpleGUI as sg

layout = [[sg.T('Close confirmation demo')],
          [sg.T('Try closing window with the "X"')],
          [sg.B('Go'), sg.B('Exit')]]

window = sg.Window('Window Title', layout, enable_close_attempted_event=True)

while True:
    event, values = window.read()
    print(event, values)
    if(event == sg.WINDOW_CLOSE_ATTEMPTED_EVENT or event == 'Exit') and sg.popup_yes_no('Do you really want to exit?') == 'Yes':
        break

window.close()