/*
  %%(file-name-base).cpp
  Autor: %%(plist-get (car (auth-source-search :host "info")) :realname)
  Fecha: %%(format-time-string "%d-%m-%Y")
*/


// Libreria de windows
#ifdef _WIN32
#include <windows.h>
#endif

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include <stdlib.h>
// #include <cmath>

void init ()
{
    glMatrixMode(GL_PROJECTION);
    int a = 1; // Dimension del espacio de trabajo
    gluOrtho2D(-a,a,-a,a);
    glClearColor(0,0,0,0);
}

void display (void)
{
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT);




    //glFlush();
    glutSwapBuffers();
}

int main (int argc, char *argv[])
{
    glutInit(&argc, argv); // Inicializa la ventana de Opengl
    glutInitWindowSize(400,400);// tamaño de la ventana
    glutInitWindowPosition(10,10);// posicion de la ventana
    
    // Especifica el tipo de modo de visualizacion al crear
    // una ventana (activar el buffer de color RGB)
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
    
    //Nombre que se le da a la ventana
    glutCreateWindow("%%(read-from-minibuffer "Nombre de la ventana: ")");
    init();
    glutDisplayFunc(display);// nombre de la funcion
    glutMainLoop();// hace que aparezca la ventana

    return EXIT_SUCCESS; // puede colocarse tambien return 0
}
