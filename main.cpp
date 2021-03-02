#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <queue>

#include <cassert>
#define MAX 10000.0


float Solve(float **Cost, const int N, const int M, const int MODE, float *assignment_index);
void genmat(int n, int m, float *cost);
void dumpmat(int n, int m, float *cost);
void dumpmat(int n, int m, bool *assign);
void row_operation(int n, int m, float *cost);
void column_operation(int n, int m, float *cost);
bool assignment_check(int n, int m, float *cost, bool *assign);


void row_operation(int n, int m, float *cost){
	for (int i=0; i<n; i++){
		float minCost = MAX;
		for(int j=0; j<m; j++){
			if (cost[i*m+j] < minCost)
			{
				minCost = cost[i*m+j];
			}
		}
		for(int j=0; j<m; j++){
			cost[i*m+j] -= minCost;	
		}
	}
}

void column_operation(int n, int m, float *cost){
	for(int j=0; j<m; j++){
		float minCost = MAX;
		for (int i=0; i<n; i++){
			if (cost[i*m+j] < minCost)
			{
				minCost = cost[i*m+j];
			}
		}
		for (int i=0; i<n; i++){
			cost[i*m+j] -= minCost;
		}
	}
}


bool assignment_check(int n, int m, float *cost, bool *assign){
	bool *assigned_rows = new bool[n];
	bool *assigned_cols = new bool[m];
	memset(assigned_rows, false, sizeof(bool)*n);
	memset(assigned_cols, false, sizeof(bool)*m);

	memset(assign, false, sizeof(bool)*n*m);

	int *zeros_rows = new int[n];
	int *zeros_cols = new int[m];
	memset(zeros_rows, 0, sizeof(int)*n);
	memset(zeros_cols, 0, sizeof(int)*m);
	
	bool chk =true;
	while (chk){
		chk = false;
		for(int i=0; i<n; i++){
			if (assigned_rows[i]) continue;
			int cnt = 0;
			int tj = -1;
			for(int j=0; j<m; j++) {
				if (cost[i*m+j]==0 && assigned_cols[j]==false) {
					cnt++;
					tj = j;
				}
				if (cnt > 1) break;
			}
			if (cnt == 1 && tj != -1){
				assign[i*m+tj]=true;
				assigned_rows[i]=true;
				assigned_cols[tj]=true;
				chk=true;
			}
		}
	
		for(int j=0; j<m; j++){
			if (assigned_cols[j]) continue;
			int cnt = 0;
			int ti = -1;
			for(int i=0; i<n; i++) {
				if (cost[i*m+j]==0 && assigned_rows[i]==false) {
					cnt++;
					ti = i;
				}
				if (cnt > 1) break;
			}
			if (cnt == 1 && ti != -1){
				assign[ti*m+j]=true;
				assigned_rows[ti]=true;
				assigned_cols[j]=true;
				chk=true;
			}
		}
		
		if (chk==false){
			memset(zeros_rows, 0, sizeof(int)*n);
			memset(zeros_cols, 0, sizeof(int)*m);
			for(int i=0; i<n; i++){
				int cnt =0;
				for (int j=0; j<m; j++){
					if (cost[i*m+j] == 0 && !assigned_rows[i] && !assigned_cols[j]) cnt++;
				}
				zeros_rows[i] = cnt;
			}
			for(int j=0; j<m; j++){
				int cnt =0;
				for (int i=0; i<n; i++){
					if (cost[i*m+j] == 0 && !assigned_rows[i] && !assigned_cols[j]) cnt++;
				}
				zeros_cols[j] = cnt;
			}

			int minZeros = MAX;
			int ti = -1, tj = -1;
			for (int j=0; j<m; j++){
				if(assigned_cols[j]) continue;
				for (int i=0; i<m; i++){
					if(assigned_rows[i]) continue;
					if(cost[i*m+j]==0){
						if( minZeros < zeros_rows[i]+zeros_cols[j] ){
							minZeros = zeros_rows[i]+zeros_cols[j];
							ti = i;
							tj = j;
						}
						break;
					}
				}
			}
			if(ti != -1 && tj != -1){
				assign[ti*m+tj]=true;
				assigned_rows[ti]=true;
				assigned_cols[tj]=true;
				chk=true;
			}
			else{
				for (int j=0; j<m; j++){
					if(chk) break;
					if(assigned_cols[j]) continue;
					for (int i=0; i<m; i++){
						if(assigned_rows[i]) continue;
						if(cost[i*m+j]==0){
							assign[i*m+j]=true;
							assigned_rows[i]=true;
							assigned_cols[j]=true;
							chk=true;
							break;
						}
					}
				}
			}	
		}
	}
	
	
	int assigned_rows_cnt = 0;
	for (int i=0; i<n; i++)
		if (assigned_rows[i]) assigned_rows_cnt++;

	if (assigned_rows_cnt==n) {
		delete[] assigned_rows;
		delete[] assigned_cols;
		delete[] zeros_rows;
		delete[] zeros_cols;
		return true;
	}


	// draw lines
	bool *marked_rows = new bool[n];
	bool *marked_cols = new bool[m];
	memset(marked_rows, false, sizeof(bool)*n);
	memset(marked_cols, false, sizeof(bool)*m);

	std::queue<int> new_rows;
	std::queue<int> new_cols;
	
	//1. 할당되지않은 모든 행 표시
	for (int i=0; i<n; i++){
		if(assigned_rows[i]) continue;
		if(marked_rows[i]) continue;
		marked_rows[i] = true;
		new_rows.push(i);
	}

	while(!new_rows.empty() || !new_cols.empty()){ // 새로운 추가가 있으면 loop
		while(!new_rows.empty()){
			int i = new_rows.front();
			new_rows.pop();
			for(int j=0; j<m; j++){
				if(cost[i*m+j] == 0){
					if (marked_cols[j]) continue;
					marked_cols[j] = true;
					new_cols.push(j); 
				}
			}
		}
		while(!new_cols.empty()){
			int j = new_cols.front();
			new_cols.pop();
			for(int i=0; i<n; i++){
				if(assign[i*m+j]){
					if(marked_rows[i]) continue;
					marked_rows[i] = true;
					new_rows.push(i);
				}
			}
		}
	}

	float minCost= MAX;
	for(int i=0; i<n; i++) for(int j=0; j<m; j++) 
		if(marked_rows[i]&& !marked_cols[j]) minCost=minCost>cost[i*m+j]? cost[i*m+j]:minCost;

	for(int i=0; i<n; i++) for(int j=0; j<m; j++){
		if(marked_rows[i]&& !marked_cols[j]) cost[i*m+j]-=minCost;
		else if(!marked_rows[i]&& marked_cols[j]) cost[i*m+j]+=minCost;
	}

	delete[] assigned_rows;
	delete[] assigned_cols;
	delete[] marked_rows;
	delete[] marked_cols;
	delete[] zeros_rows;
	delete[] zeros_cols;

	return assignment_check(n, m, cost, assign); 
}


float Solve(float **Cost, const int n, const int m, const int MODE, float *assignment_index){

	// non_square -> square array
	int N = n;	
	//temporary
	int M = n > m ? n : m;	


	// Cost, assign  MODE ==1 일경우 , cost *-1
	float* cost = new float[N*M];
	bool* assign = new bool[N*M];
	memset(assign, false, sizeof(bool)*N*M);
	for (int i=0; i<N; i++) for (int j=0; j<M; j++)
		cost[i*M+j] = MODE==1? -MAX:MAX;
	for (int i=0; i<n; i++)
		memcpy(cost+i*M, *Cost+i*m, sizeof(float)*m);
	if (MODE==1){
		for (int i=0; i<N; i++) for (int j=0; j<M; j++)
			cost[i*M+j] *= -1;
	}

	row_operation(N, M, cost);
	column_operation(N, M, cost);
	assignment_check(N, M, cost, assign); 

	float result = 0;
	for(int i=0; i<n; i++){
		for(int j=0; j<m; j++){
			if(assign[i*M+j]){
				result += (*Cost)[i*m+j];
				assignment_index[i] = j;	
				break;
			}
		}
	}
	

	delete[] cost;
	delete[] assign;

	return result;
}

void genmat(int n, int m, float *cost){
	srand(time(0));

	// 다양한 cost 출력 0~100+m
	// 중복 선택 불가능하여, 한 작업은 각 인원에게 모두 다른 cost
	int max_range = m + m;
	float *candi = new float[max_range];
	for (int i=0; i<max_range; i++)
		candi[i] = i+1;

	int select, max_idx;
	for (int j=0; j<m; j++) {
		max_idx = max_range;
		for (int i=0; i<n; i++)
		{
			select = max_idx==0? 0:rand()%max_idx; // 0~(m-1) -> 0~m-2 -> ..(if m=4) 0~3(4) 0~2 0~1 0(1)  random out
			
			cost[i*m+j]=candi[select];
			candi[select] = candi[max_idx-1];
			candi[max_idx-1] = cost[i*m+j];	
			max_idx--;
		}
	}

	delete[] candi;
}

void dumpmat(int n, int m, float *cost){
	for (int i=0; i<n; i++) {
		for (int j=0; j< m; j++) 
			printf("%.0f\t", cost[i*m+j]);
		printf("\n");	
	}
}
void dumpmat(int n, int m, bool *assign){
	for (int i=0; i<n; i++) {
		for (int j=0; j< m; j++){
			if (assign[i*m+j]) printf("1 ");
			else printf("0 ");
		}
		printf("\n");	
	}
}
int main(){

	int n = 4, m = 4;

	float* cost = new float[n*m];
	float* assignment = new float[n];
	
	memset(cost, 0, sizeof(float)*n*m);
	genmat(n, m, cost);
	
	printf("예시)\n N=%d / M=%d\n\n", n, m);
	printf("-입력:\n");
	dumpmat(n, m, cost);
	printf("\n");

	for (int mode=0; mode<2; mode++){
		for(int i=0; i<n; i++)
			assignment[i] = -1;

		float result = Solve(&cost, n, m, mode, assignment);
		printf("Mode %d인 경우, Total Cost : %.2f, ", mode, result);
		printf("assignment = {");
		for (int i =0; i< n; i++){
			if(i!=0) printf(", ");
			printf("%.0f", assignment[i]);
		}
		printf("}\n");
	}
	delete[] cost;
	delete[] assignment;
	
	return 0;
}
