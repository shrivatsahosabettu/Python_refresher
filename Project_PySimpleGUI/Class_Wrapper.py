import PySimpleGUI as sg


class SampleGUI():

    def __init__(self):
        self.layout = [[sg.T('My layout')],
                       [sg.I(key='-IN-')],
                       [sg.B('Go'), sg.B('Exit')]]

        self.window = sg.Window('My new window', self.layout)

    def run(self):
        while True:
            self.event, self.values = self.window.read()
            if self.event in (sg.WIN_CLOSED, 'Exit'):
                break
            if self.event == 'Go':
                self.button_go()

        self.window.close()

    def button_go(self):
        sg.popup('Go button clicked', 'Input value:', self.values['-IN-'])


# Create the class
my_gui = SampleGUI()
# run the event loop
my_gui.run()