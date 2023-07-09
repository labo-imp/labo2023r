import datetime

def proximo_anio_bisiesto():
    hoy = datetime.date.today()
    anio_actual = hoy.year

    while True:
        anio_actual += 1
        if (anio_actual % 4 == 0 and anio_actual % 100 != 0) or anio_actual % 400 == 0:
            return anio_actual

proximo_anio = proximo_anio_bisiesto()
print("El próximo año bisiesto es:", proximo_anio)
