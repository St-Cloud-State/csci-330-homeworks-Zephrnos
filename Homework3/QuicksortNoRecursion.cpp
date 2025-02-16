#include <iostream>
#include <stack>
#include <vector>
#include <algorithm> // For std::swap

// Partition function: chooses the last element as pivot.
int partition(std::vector<int>& arr, int low, int high) {
    int pivot = arr[high];
    int i = low - 1; // index of smaller element

    // Rearranging elements: all elements <= pivot on left.
    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            std::swap(arr[i], arr[j]);
        }
    }
    // Place pivot in its correct position.
    std::swap(arr[i + 1], arr[high]);
    return i + 1;
}

// Non-recursive quicksort using an explicit stack
void quicksort(std::vector<int>& arr) {
    int n = arr.size();
    if (n < 2)
        return;

    // Each stack element holds a pair {low, high} of a subarray.
    std::stack<std::pair<int, int>> stack;
    stack.push({0, n - 1});

    while (!stack.empty()) {
        auto [low, high] = stack.top();
        stack.pop();

        // Partition the subarray and get the pivot index.
        int pivotIndex = partition(arr, low, high);

        // If there are elements on the left, push them.
        if (pivotIndex - 1 > low)
            stack.push({low, pivotIndex - 1});

        // If there are elements on the right, push them.
        if (pivotIndex + 1 < high)
            stack.push({pivotIndex + 1, high});
    }
}

int main() {
    std::vector<int> data = {5, 3, 8, 4, 2, 7, 1, 10, 6};
    std::cout << "Before sort: ";
    for (int num : data)
        std::cout << num << " ";
    std::cout << "\n";

    quicksort(data);

    std::cout << "After sort: ";
    for (int num : data)
        std::cout << num << " ";
    std::cout << "\n";

    return 0;
}