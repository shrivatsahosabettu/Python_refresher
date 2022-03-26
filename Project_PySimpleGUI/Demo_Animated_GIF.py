from PIL import Image, ImageTk, ImageSequence
import PySimpleGUI as sg


gif_filename = r'exampleGIF.gif'

layout = [[sg.T('Happy Thursday!', background_color='#A37A3B', text_color='#FFF000', justification='c', key = '-T-',
                font=('Bodoni MT', 40))], [sg.Image(key='-IMAGE-')]]

window = sg.Window('Window Title', layout, element_justification='c', margins=(0, 0), element_padding=(0, 0),
                   finalize=True)
window['-T-'].expand(True, True, True)  #Make the Text expand to take up all available space

interframe_duration = Image.open(gif_filename).info['duration'] # get how long to delay between frames


while True:
    for frame in ImageSequence.Iterator(Image.open(gif_filename)):
        event, values = window.read(timeout=interframe_duration)
        if event == sg.WIN_CLOSED:
            exit(0)
        window['-IMAGE-'].update(data=ImageTk.PhotoImage(frame))