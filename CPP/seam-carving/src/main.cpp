#include "Image.h"
#include "SeamCarver.h"

#include <cmath>
#include <fstream>
#include <iostream>

namespace {
std::vector<std::vector<Image::Pixel>> ReadImageFromCSV(std::ifstream & input)
{
    size_t width, height;
    input >> width >> height;
    std::vector<std::vector<Image::Pixel>> table;
    for (size_t columnId = 0; columnId < width; ++columnId) {
        std::vector<Image::Pixel> column;
        for (size_t rowId = 0; rowId < height; ++rowId) {
            size_t red, green, blue;
            input >> red >> green >> blue;
            column.emplace_back(red, green, blue);
        }
        table.emplace_back(std::move(column));
    }
    return table;
}

void WriteImageToCSV(const SeamCarver & carver, std::ofstream & output)
{
    const size_t width = carver.GetImageWidth();
    const size_t height = carver.GetImageHeight();
    output << width << " " << height << "\n";
    const Image & image = carver.GetImage();
    for (size_t columnId = 0; columnId < width; ++columnId) {
        for (size_t rowId = 0; rowId < height; ++rowId) {
            const Image::Pixel & pixel = image.GetPixel(columnId, rowId);
            output << pixel.m_red << " " << pixel.m_green << " " << pixel.m_blue << std::endl;
        }
    }
}
} // namespace

bool regressionTest1()
{
    const std::vector<Image::Pixel> col0 = {Image::Pixel(255, 0, 0), Image::Pixel(50, 0, 0), Image::Pixel(89, 0, 0), Image::Pixel(24, 0, 0)};
    const std::vector<Image::Pixel> col1 = {Image::Pixel(218, 0, 0), Image::Pixel(145, 0, 0), Image::Pixel(255, 0, 0), Image::Pixel(4, 0, 0)};
    const std::vector<Image::Pixel> col2 = {Image::Pixel(123, 0, 0), Image::Pixel(56, 0, 0), Image::Pixel(54, 0, 0), Image::Pixel(27, 0, 0)};

    const Image image({col0, col1, col2});
    SeamCarver carver(image);

    std::vector<size_t> seam = carver.FindHorizontalSeam();

    std::vector<std::vector<double>> EnergyMap = {{std::sqrt(9701), std::sqrt(35477), std::sqrt(41077), std::sqrt(28085)},
                                                  {std::sqrt(37305), std::sqrt(1405), std::sqrt(21106), std::sqrt(1378)},
                                                  {std::sqrt(2210), std::sqrt(13786), std::sqrt(28397), std::sqrt(5161)}};

    std::vector<size_t> right_seam = {0, 1, 0};

    return seam == right_seam;
}

bool regressionTest2()
{
    const std::vector<Image::Pixel> col0 = {Image::Pixel(209, 30, 184), Image::Pixel(235, 145, 94)};
    const std::vector<Image::Pixel> col1 = {Image::Pixel(214, 127, 216), Image::Pixel(212, 59, 224)};
    const std::vector<Image::Pixel> col2 = {Image::Pixel(192, 229, 164), Image::Pixel(231, 238, 67)};

    const Image image({col0, col1, col2});
    SeamCarver carver(image);

    std::vector<size_t> seam = carver.FindVerticalSeam();

    std::vector<size_t> right_seam = {0, 1};

    return seam == right_seam;
}

int main(int argc, char * argv[])
{
    //regression test
    if (!regressionTest1() or !regressionTest2()) {
        return 1;
    }

    // Check command line arguments
    const size_t expectedAmountOfArgs = 3;
    if (argc != expectedAmountOfArgs) {
        std::cout << "Wrong amount of arguments. Provide filenames as arguments. See example below:\n";
        std::cout << "seam-carving data/tower.csv data/tower_updated.csv" << std::endl;
        return 0;
    }
    // Check csv file
    std::ifstream inputFile(argv[1]);
    if (!inputFile.good()) {
        std::cout << "Can't open source file " << argv[1] << ". Verify that the file exists." << std::endl;
    }
    else {
        auto imageSource = ReadImageFromCSV(inputFile);
        SeamCarver carver(std::move(imageSource));
        std::cout << "Image: " << carver.GetImageWidth() << "x" << carver.GetImageHeight() << std::endl;
        const size_t pixelsToDelete = 150;
        for (size_t i = 0; i < pixelsToDelete; ++i) {
            std::vector<size_t> seam = carver.FindVerticalSeam();
            carver.RemoveVerticalSeam(seam);
            std::cout << "width = " << carver.GetImageWidth() << ", height = " << carver.GetImageHeight() << std::endl;
        }
        std::ofstream outputFile(argv[2]);
        WriteImageToCSV(carver, outputFile);
        std::cout << "Updated image is written to " << argv[2] << "." << std::endl;
    }
    return 0;
}