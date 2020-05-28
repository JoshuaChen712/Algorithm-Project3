#include "common.h"

using namespace std;

void gauss(double **&a, double *&x, int n);

//You should only code here.Don't edit any other files in this
int func1(int amount, vector<int> &coins)
{
	int *dp = new int[amount + 1];
	for (int i = 0; i <= amount; i++)
		dp[i] = 0;
	dp[0] = 1;
	vector<int>::iterator i;
	for (long long unsigned int i = 0; i < coins.size(); i++)
	{
		for (int j = coins[i]; j <= amount; j++)
			dp[j] += dp[j - coins[i]];
	}
	int tmp = dp[amount];
	delete[] dp;

	return tmp;
}

int func2(int amount, vector<vector<int>> &conquer)
{
	int **dp = new int *[amount];
	for (int i = 0; i < amount; i++)
		dp[i] = new int[amount];

	for (int i = 0; i < amount; i++)
	{
		for (int j = 0; j < amount; j++)
		{
			dp[i][j] = 0;
		}
	}

	for (int i = 0; i < amount; i++)
	{
		dp[i][(i + 1) % amount] = 1;
	}

	for (int len = 2; len <= amount; len++)
	{
		for (int j = 0; j < amount; j++)
		{
			for (int e = (j + 1) % amount; e != (j + len) % amount; e = (e + 1) % amount)
			{
				dp[j][(len + j) % amount] = (dp[j][(len + j) % amount] || (dp[j][e] && dp[e][(len + j) % amount] && (conquer[j][e] || conquer[(len + j) % amount][e])));
			}
		}
	}

	int ans = 0;
	for (int i = 0; i < amount; i++)
	{
		if (dp[i][i])
		{
			ans++;
		}
	}

	for (int i = 0; i < amount; i++)
		delete[] dp[i];
	delete[] dp;

	return ans;
}

double func3(int n, int hp, vector<int> &damage, vector<int> &edges)
{
	double **dp = new double *[hp + 1]; //dp矩阵
	for (int i = 0; i <= hp; i++)
		dp[i] = new double[n + 1];

	for (int i = 0; i <= hp; i++)
		for (int j = 0; j <= n; j++)
			dp[i][j] = 0;

	vector<int> *prev = new vector<int>[n + 1]; //后继结点

	for (long long unsigned i = 0; i < edges.size(); i += 2)
	{
		int node1 = edges[i];
		int node2 = edges[i + 1];

		prev[node1].push_back(node2);
		prev[node2].push_back(node1);
	}

	for (int i = hp; i > 0; i--)
	{
		double **a = new double *[n]; //系数矩阵
		for (int i = 0; i < n; i++)
			a[i] = new double[n + 1];
		for (int i = 0; i < n; i++)
		{
			for (int j = 0; j < n + 1; j++)
			{
				a[i][j] = 0;
			}
		}
		for (int j = 1; j < n; j++)
		{
			a[j][j] = 1;
			if (damage[j - 1] == 0)
			{

				for (long long unsigned k = 0; k < prev[j].size(); k++)
				{

					if (prev[j][k] == n)
						continue;
					a[j][prev[j][k]] = -1 / double(prev[prev[j][k]].size());
					if (hp == i && j == 1)
						a[j][n] = 1;
				}
			}

			else if (damage[j - 1] > 0)
			{
				if (i + damage[j - 1] > hp)
				{
					a[j][n] = 0;
				}
				else
				{
					for (long long unsigned k = 0; k < prev[j].size(); k++)
					{
						if (prev[j][k] == n)
							continue;
						a[j][n] += dp[damage[j - 1] + i][prev[j][k]] / double(prev[prev[j][k]].size());
					}
				}
			}
		}
		double *answer = new double[n];
		gauss(a, answer, n);
		for (int num = 1; num < n; num++)
			dp[i][num] = answer[num];
		delete answer;

		for (int i = 0; i < n; i++)
			delete[] a[i];
		delete[] a;
	}
	double final_answer = 0;

	for (int i = hp; i > 0; i--)
		for (long long unsigned j = 0; j < prev[n].size(); j++)
		{
			final_answer += dp[i][prev[n][j]] / double((prev[prev[n][j]].size()));
		}

	for (int i = 0; i <= hp; i++)
		delete[] dp[i];
	delete[] dp;
	delete[] prev;

	return final_answer;
}

void gauss(double **&a, double *&x, int n)
{
	int i, j, k;
	double *c = new double[n]; //存储初等行变换的系数，用于行的相减
	for (k = 1; k < n - 1; k++)
	{
		//求出第K次初等行变换的系数
		for (i = k + 1; i < n; i++)
			c[i] = a[i][k] / a[k][k];

		//第K次的消元计算
		for (i = k + 1; i < n; i++)
		{
			for (j = 0; j < n; j++)
			{
				a[i][j] = a[i][j] - c[i] * a[k][j];
			}
			a[i][n] = a[i][n] - c[i] * a[k][n];
		}
	}

	//先计算出最后一个未知数；
	x[n - 1] = a[n - 1][n] / a[n - 1][n - 1];
	//求出每个未知数的值
	for (i = n - 2; i >= 0; i--)
	{
		double sum = 0;
		for (j = i + 1; j < n; j++)
		{
			sum += a[i][j] * x[j];
		}
		x[i] = (a[i][n] - sum) / a[i][i];
	}

	delete[] c;
}
