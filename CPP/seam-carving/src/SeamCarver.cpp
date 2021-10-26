#include "SeamCarver.h"

#include <algorithm>
#include <cmath>
#include <limits>

SeamCarver::SeamCarver(Image image)
    : m_image(std::move(image))
{
}

const Image & SeamCarver::GetImage() const
{
    return m_image;
}

size_t SeamCarver::GetImageWidth() const
{
    return m_image.m_table.size();
}

size_t SeamCarver::GetImageHeight() const
{
    return m_image.m_table.empty() ? 0 : m_image.m_table[0].size();
}

double SeamCarver::GetPixelEnergy(size_t columnId, size_t rowId) const
{
    const size_t W = GetImageWidth();
    const size_t H = GetImageHeight();
    auto P1 = m_image.GetPixel((columnId + 1) % W, rowId);
    auto P2 = m_image.GetPixel((W + columnId - 1) % W, rowId);

    double R = P1.m_red - P2.m_red;
    double G = P1.m_green - P2.m_green;
    double B = P1.m_blue - P2.m_blue;

    const double deltaX = R * R + G * G + B * B;

    P1 = m_image.GetPixel(columnId, (rowId + 1) % H);
    P2 = m_image.GetPixel(columnId, (H + rowId - 1) % H);

    R = P1.m_red - P2.m_red;
    G = P1.m_green - P2.m_green;
    B = P1.m_blue - P2.m_blue;

    const double deltaY = R * R + G * G + B * B;

    return std::sqrt(deltaX + deltaY);
}

SeamCarver::Seam SeamCarver::FindSeam(const bool vertical) const
{
    const size_t W = (vertical ? GetImageHeight() : GetImageWidth());
    const size_t H = (vertical ? GetImageWidth() : GetImageHeight());

    std::vector<std::vector<double>> EnergyTable(W, std::vector<double>(H));
    for (size_t j = 0; j < H; j++) {
        for (size_t i = 0; i < W; i++) {
            EnergyTable[i][j] = GetPixelEnergy((vertical ? j : i), (vertical ? i : j));
        }
    }

    std::vector<size_t> seam(W);

    for (size_t col = 1; col < W; col++) {
        for (size_t row = 0; row < H; row++) {
            if (row == 0) {
                EnergyTable[col][row] += std::min(EnergyTable[col - 1][row], EnergyTable[col - 1][row + 1]);
            }
            else if (row == H - 1) {
                EnergyTable[col][row] += std::min(EnergyTable[col - 1][row], EnergyTable[col - 1][row - 1]);
            }
            else {
                EnergyTable[col][row] += std::min({EnergyTable[col - 1][row - 1], EnergyTable[col - 1][row], EnergyTable[col - 1][row + 1]});
            }
        }
    }

    std::vector<double>::iterator min_val = std::min_element(EnergyTable[W - 1].begin(), EnergyTable[W - 1].end());
    size_t min_ind = std::distance(EnergyTable[W - 1].begin(), min_val);

    seam[W - 1] = min_ind;

    size_t col = W - 1;
    size_t count = 0;
    while (col > 0) {
        col--;
        if (min_ind == 0) {
            count = (EnergyTable[col][min_ind] <= EnergyTable[col][min_ind + 1]) ? 0 : 1;
        }
        else if (min_ind == H - 1) {
            count = (EnergyTable[col][min_ind] <= EnergyTable[col][min_ind - 1]) ? 0 : -1;
        }
        else {
            if (EnergyTable[col][min_ind - 1] < EnergyTable[col][min_ind] && EnergyTable[col][min_ind - 1] < EnergyTable[col][min_ind + 1]) {
                count = -1;
            }
            else if (EnergyTable[col][min_ind + 1] < EnergyTable[col][min_ind]) {
                count = 1;
            }
            else {
                count = 0;
            }
        }
        seam[col] = min_ind + count;
        min_ind = seam[col];
    }

    return seam;
}

SeamCarver::Seam SeamCarver::FindHorizontalSeam() const
{
    return FindSeam(false);
}

SeamCarver::Seam SeamCarver::FindVerticalSeam() const
{
    return FindSeam(true);
}

void SeamCarver::RemoveHorizontalSeam(const Seam & seam)
{
    const size_t W = GetImageWidth();
    const size_t H = GetImageHeight();

    for (size_t i = 0; i < W; i++) {
        for (size_t j = seam[i]; j < H - 1; j++) {
            if (seam[i] <= j) {
                m_image.m_table[i][j] = m_image.m_table[i][j + 1];
            }
        }
    }
    for (size_t i = 0; i < W; i++) {
        m_image.m_table[i].pop_back();
    }
}

void SeamCarver::RemoveVerticalSeam(const Seam & seam)
{
    size_t H = GetImageHeight();
    size_t W = GetImageWidth();

    for (size_t row = 0; row < H; row++) {
        for (size_t col = seam[row]; col < W - 1; col++) {
            m_image.m_table[col][row] = m_image.m_table[col + 1][row];
        }
    }

    m_image.m_table.pop_back();
}
