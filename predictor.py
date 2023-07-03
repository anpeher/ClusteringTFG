import numpy as np
import sys
import joblib 
from csv import writer
import warnings
from datetime import datetime


warnings.filterwarnings('ignore')

gitrepo = "C:/Users/Antonio Pelluz/Desktop/Cluster3/"


def CrearNulo(x): #para cambiar las variables numéricas nulas
    if x == 0: 
        return ""
    return x

def CrearNuloCaracter(x): #para cambiar las variables categóricas nulas
    if x == "nan": 
        return ""
    return x

def SeleccionNombre(nombre): #si no se nos proporciona un nombre usaremos el nombre de fecha y hora del día que se pide
    if nombre == "nan": 
        return datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    return nombre

def numeric_yearbuild(x):
    if x=='11th Century onwards':
        return 1200
    if x=='1862-1875':
        return 1868
    if x=='1888-1890':
        return 1889
    if x=='1898-1902':
        return 1900
    if x=='1903-1906':
        return 1904
    if x=='1913-1915':
        return 1914
    if x=='1919-1945':
        return 1932
    if x=='1945-1966':
        return 1955
    if x=='1967-1976':
        return 1971
    if x=='post 1976':
        return 1982
    if x=='Post 1976':
        return 1982
    if x=='pre 1919':
        return 1910
    if x=='Pre 1919':
        return 1910
    if x=='Post 2000':
        return 2011
    if x=='nan':
        return 0
    return(x)

cat_index = [1,2,3,5,7,8,10] #variable categóricas
modelF = joblib.load("modelF.pkl") # Cargamos del modelo.
yearbuilt = numeric_yearbuild(sys.argv[10])
entrada = [float(sys.argv[1]),sys.argv[2].replace("_"," "),sys.argv[3].replace("_"," "),int(sys.argv[4]),int(sys.argv[5]),sys.argv[6],float(sys.argv[7]),sys.argv[8].replace("_"," "),sys.argv[9],yearbuilt,sys.argv[11],float(sys.argv[12]),float(sys.argv[13])]#obtenemos la predicción de R y preparamos los datos
#entradaExcel = ["PrimClass_Everett","31/12/12 23:00","01/01/12 00:00",float(sys.argv[1]),sys.argv[2],sys.argv[3],"Electric",sys.argv[4],int(sys.argv[5]),sys.argv[6],float(sys.argv[7]),float(sys.argv[7])*10.7639,sys.argv[8],sys.argv[9],"Everett",sys.argv[10],sys.argv[11],"weather12.csv"]
nombreEntrada = SeleccionNombre(sys.argv[14]) #si no se obtiene ningún nombrese pone la fecha y la hora de la ejecución
entradaExcel = [nombreEntrada,"","",CrearNulo(float(sys.argv[1])),CrearNuloCaracter(sys.argv[2].replace("_"," ")),CrearNuloCaracter(sys.argv[3].replace("_"," ")),"",CrearNulo(int(sys.argv[4])),CrearNulo(int(sys.argv[5])),"",CrearNuloCaracter(sys.argv[6]),"",CrearNulo(float(sys.argv[7])),CrearNuloCaracter(sys.argv[8].replace("_"," ")),CrearNuloCaracter(sys.argv[9]),CrearNuloCaracter(sys.argv[10]),"",CrearNuloCaracter(sys.argv[11]),"",CrearNulo(float(sys.argv[12])),CrearNulo(float(sys.argv[13]))] #entrada del csv
#TOTAL: uid,dataend,datastart,energystarscore,heatingtype,industry,mainheatingtype,numberoffloors,occupants,primaryspaceusage,rating,sqft,sqm,subindustry,timezone,yearbuilt,nickname,primaryspaceuse_abbrev,newweatherfilename
#USADOS: energystarscore,heatingtype,industry,numberoffloors,occupants,rating,sqm,subindustry,timezone,yearbuilt,primaryspaceuse_abbrev,electric_savings,gas_savings
x = np.array([entrada])
print(int(modelF.predict(x, categorical=list(cat_index))))#realizamos la predicción
#escribimos el nuevo edificio que nos llega en nuestro dataset
with open('meta_opentrynew.csv', 'a', newline='') as f_object:
    writer_object = writer(f_object)
    writer_object.writerow(entradaExcel)
    f_object.close()
