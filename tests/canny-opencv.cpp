
// Convert an RGB image to greyscale and apply the X&Y Sobel operators.
// Produce an image containing the magnitude of the vector at each point.
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <opencv2/core/core.hpp>
#include <opencv2/core/mat.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>

typedef unsigned long long ull;

inline ull rdtsc() {
    unsigned int lo, hi;
    asm volatile ( "rdtsc\n" : "=a" (lo), "=d" (hi) );
    return ((ull)hi << 32) | lo;
}

int main(int argc, char *argv[])
{
    if(argc != 3) {
        printf("Usage: cv-canny <in.bmp> <out.bmp>\n");
        exit(0);
    }
    char*   fileNameIn  = argv[1];
    char*   fileNameOut = argv[2];

    // Load source image.  
    cv::Mat src = cv::imread(fileNameIn);
    if(src.data == NULL) {
        printf("Could not load image file: %s\n", fileNameIn);
        exit(0);
    }
    int height  = src.rows;
    int width   = src.cols;
    int channels    = src.channels();
    assert (channels == 3);

    ull t1 = rdtsc();
    // Get luminance of source image as word8s
    cv::Mat srcLum  (src.rows, src.cols, CV_8UC1);

    for(int i = 0; i < height; i++) {
        uchar* rowSrc       = src.ptr(i);
        uchar* rowSrcLum    = (uchar*)srcLum.ptr(i);
    
        for(int j = 0; j < width; j++) {
            float r = (float)rowSrc[j * channels + 0];
            float g = (float)rowSrc[j * channels + 1];
            float b = (float)rowSrc[j * channels + 2];
            float x = ((r * 0.3) + (g * 0.59) + (b * 0.11));
        
            rowSrcLum[j] = (uchar)x;
        }
    }


    // Blur image
    cv::Mat srcBlur = srcLum.clone();
    cv::Size ksize;
    ksize.width = 5;
    ksize.height = 5;
    cv::GaussianBlur(srcLum, srcBlur, ksize, 1, 1, cv::BORDER_REPLICATE);


    // Apply canny algorithm to result
    cv::Mat edges   = srcLum.clone();
    cv::Canny(srcBlur, edges, 60, 70);


    // Create output greyscale image.
    //   The imwrite function doesn't handle float data.
    cv::Mat matOut (src.rows, src.cols, CV_8U);

    for(int i = 0; i < height; i++) {
        uchar* rowEdges     = (uchar*)edges.ptr(i);
        uchar* rowOut       = matOut.ptr(i);

        for(int j = 0; j < width; j++) {
            rowOut[j]   = rowEdges[j];
        }
    }

    ull t = rdtsc() - t1;
    printf("%.2f\n", (t * 1.0) / height / width );
    // Write out the data to a new image.
    cv::imwrite(fileNameOut, matOut);
    
    return 0;
}
