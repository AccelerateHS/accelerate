
// Convert an RGB image to greyscale and apply the X&Y Sobel operators.
// Produce an image containing the magnitude of the vector at each point.
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <opencv2/core/core.hpp>
#include <opencv2/core/mat.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/gpu/gpu.hpp>                  // GPU structures and methods

#include "Timing.h"

int main(int argc, char *argv[])
{
        if(argc != 4) {
                printf("Usage: canny-gpu <iterations> <in.bmp> <out.bmp>\n");
                exit(0);
        }
        int     iterations      = atoi(argv[1]);
        char*   fileNameIn      = argv[2];
        char*   fileNameOut     = argv[3];

        // Load source image.
        cv::Mat src_host        = cv::imread(fileNameIn);
        if(src_host.data == NULL) {
                printf("Could not load image file: %s\n", fileNameIn);
                exit(0);
        }
        int height      = src_host.rows;
        int width       = src_host.cols;
        int channels    = src_host.channels();
        assert (channels == 3);

        // Upload the source image to the GPU. Do this before benchmarking
        // begins because Accelerate effectively caches the image on the device.
        cv::gpu::GpuMat src;
        src.upload(src_host);

        // Get luminance of source image as word8s
        cv::gpu::GpuMat srcLum(src.rows, src.cols, CV_8UC1);

        struct benchtime *btTotal = bench_begin();
        struct benchtime *btGrey  = bench_begin();

        for(int iters = 0; iters < iterations; iters++) {
            cv::gpu::cvtColor(src, srcLum, CV_RGB2GRAY);
        }
        printf("* GREYSCALE\n");
        bench_done(btGrey);

        // Blur image
        cv::gpu::GpuMat srcBlur = srcLum.clone();
        struct benchtime *btBlur = bench_begin();
        cv::Size ksize;
        ksize.width = 5;
        ksize.height = 5;
        for(int iters = 0; iters < iterations; iters++) {
                cv::gpu::GaussianBlur(srcLum, srcBlur, ksize, 1, 1, cv::BORDER_REPLICATE);
        }
        printf("* BLUR\n");
        bench_done(btBlur);

        // Apply canny algorithm to result
        cv::gpu::GpuMat edges   = srcBlur.clone();
        struct benchtime *btCanny = bench_begin();
        for(int iters = 0; iters < iterations; iters++) {
                cv::gpu::Canny(srcBlur, edges, 60, 70);
        }
        printf("* CANNY\n");
        bench_done(btCanny);

        printf("\nTOTAL\n");
        bench_done(btTotal);

        // Copy the result back to the host
        cv::Mat edges_host;
        edges.download(edges_host);

        // Create output greyscale image.
        //   The imwrite function doesn't handle float data.
        cv::Mat matOut (src_host.rows, src_host.cols, CV_8U);

        for(int i = 0; i < height; i++) {
                uchar* rowEdges         = (uchar*)edges_host.ptr(i);
                uchar* rowOut           = matOut.ptr(i);

                for(int j = 0; j < width; j++) {
                        rowOut[j]       = rowEdges[j];
                }
        }

        // Write out the data to a new image.
        cv::imwrite(fileNameOut, matOut);

        return 0;
}

