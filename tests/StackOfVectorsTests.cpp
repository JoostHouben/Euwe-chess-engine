#include "chess-engine-lib/StackOfVectors.h"

#include "gtest/gtest.h"

namespace StackOfVectorsTests {

TEST(StackOfVectors, basicTest) {
    StackOfVectors<int> stackOfVectors;

    StackVector<int> stackVector1 = stackOfVectors.makeStackVector();
    stackVector1.push_back(1);
    stackVector1.emplace_back(2);
    stackVector1.push_back(3);
    stackVector1.pop_back();
    stackVector1.push_back(std::move(4));
    stackVector1.lock();

    StackVector<int> stackVector2 = stackOfVectors.makeStackVector();
    stackVector2.push_back(5);
    stackVector2.emplace_back(6);
    stackVector2.push_back(7);
    stackVector2.pop_back();
    stackVector2.push_back(8);
    stackVector2.lock();

    EXPECT_EQ(stackOfVectors.size(), 6);
    EXPECT_EQ(stackVector1.size(), 3);
    EXPECT_EQ(stackVector2.size(), 3);

    EXPECT_EQ(stackVector1[0], 1);
    EXPECT_EQ(stackVector1[1], 2);
    EXPECT_EQ(stackVector1[2], 4);

    EXPECT_EQ(stackVector2[0], 5);
    EXPECT_EQ(stackVector2[1], 6);
    EXPECT_EQ(stackVector2[2], 8);

    EXPECT_EQ(stackVector1.front(), 1);
    EXPECT_EQ(stackVector1.back(), 4);
    EXPECT_EQ(stackVector2.front(), 5);
    EXPECT_EQ(stackVector2.back(), 8);

    EXPECT_EQ(*stackVector1.begin(), 1);
    EXPECT_EQ(*(stackVector1.end() - 1), 4);
    EXPECT_EQ(*stackVector2.begin(), 5);
    EXPECT_EQ(*(stackVector2.end() - 1), 8);

    StackVector<int> stackVector2Moved = std::move(stackVector2);
    EXPECT_EQ(stackVector2Moved.size(), 3);
    EXPECT_EQ(stackVector2Moved[0], 5);
    EXPECT_EQ(stackVector2Moved[1], 6);
    EXPECT_EQ(stackVector2Moved[2], 8);
}

TEST(StackOfVectors, nestedRangeBasedForLoop) {
    StackOfVectors<int> stackOfVectors;

    StackVector<int> stackVector1 = stackOfVectors.makeStackVector();
    stackVector1.push_back(1);
    stackVector1.push_back(2);
    stackVector1.lock();

    for (int i : stackVector1) {
        StackVector<int> stackVector2 = stackOfVectors.makeStackVector();
        stackVector2.push_back(i * 1);
        stackVector2.push_back(i * 2);
        stackVector2.lock();
        for (int j : stackVector2) {
            StackVector<int> stackVector3 = stackOfVectors.makeStackVector();
            stackVector3.push_back(j * 1);
            stackVector3.push_back(j * 2);
            stackVector3.lock();
            EXPECT_EQ(stackVector1[0], 1);
            EXPECT_EQ(stackVector1[1], 2);
            EXPECT_EQ(stackVector2[0], i);
            EXPECT_EQ(stackVector2[1], i * 2);
            EXPECT_EQ(stackVector3[0], j);
            EXPECT_EQ(stackVector3[1], j * 2);
            EXPECT_EQ(stackOfVectors.size(), 6);
        }
        EXPECT_EQ(stackVector1[0], 1);
        EXPECT_EQ(stackVector1[1], 2);
        EXPECT_EQ(stackVector2[0], i);
        EXPECT_EQ(stackVector2[1], i * 2);
        EXPECT_EQ(stackOfVectors.size(), 4);
    }
    EXPECT_EQ(stackVector1[0], 1);
    EXPECT_EQ(stackVector1[1], 2);
    EXPECT_EQ(stackOfVectors.size(), 2);
}

}  // namespace StackOfVectorsTests
