#include <Rcpp.h>
#include <list>
#include <vector>
//#include <gperftools/profiler.h>

using namespace Rcpp;
using std::list;
using std::vector;

class Quadrant
{
    private:
        list<unsigned int> nodes;
        list<Quadrant *> subQuadrants;
        double center_x, center_y;
        double mass;
        double size;
        unsigned int num_nodes;
        const NumericMatrix &lay;
        const NumericVector &nodes_mass;
    
    
        void apply_repulsion_one_sided(unsigned int i, const NumericMatrix &lay, NumericMatrix &dvec,
                                       float mass_prod, float sum_sizes, float x, float y, double krep, bool prevent_overlap);
    
        inline void apply_repulsion_two_nodes(unsigned int i, unsigned int j, const NumericMatrix &lay,
                                          NumericMatrix &dvec, const NumericVector &nodes_size, double krep, bool prevent_overlap);
    
    
        inline void create_subquadrant(const list<unsigned int> &nodes);
        void calculate_geometry(void);
        void partition(void);
    
    public:
        Quadrant(const list<unsigned int> &nodes, const NumericMatrix &_lay, const NumericVector &_nodes_mass);
    void Apply_repulsion(unsigned int node, const NumericMatrix &lay, NumericMatrix &dvec, const NumericVector &nodes_sizes, double krep, bool prevent_overlap, float theta);
        ~Quadrant(void) {}
        void Clear(void);
    
};


Quadrant::Quadrant(const list<unsigned int> &_nodes, const NumericMatrix &_lay, const NumericVector &_nodes_mass) : nodes(_nodes), lay(_lay), nodes_mass(_nodes_mass)
{
    center_x = center_y = mass = size = 0.0;
    num_nodes = nodes.size();
    calculate_geometry();
    partition();
}

void Quadrant::Clear(void)
{
    if(subQuadrants.size())
    {
        list<Quadrant *>::iterator x;
        for(x = subQuadrants.begin(); x != subQuadrants.end(); x++)
        {
            (*x)->Clear();
            delete *x;
        }
    }
        
}

void Quadrant::calculate_geometry(void)
{
    double sum_x, sum_y; sum_x = sum_y = 0.0;
    list<unsigned int>::const_iterator i;
    
    for(i = nodes.begin(); i != nodes.end(); i++)
    {
        sum_x += (lay(*i, 0) * nodes_mass[*i]);
        sum_y += (lay(*i, 1) * nodes_mass[*i]);
        mass += nodes_mass[*i];
    }
    
    center_x = sum_x / mass;
    center_y = sum_y / mass;
    
    //ATTENZIONE!!!!!
    //mass /= num_nodes;
    
    for(i = nodes.begin(); i != nodes.end(); i++)
    {
        double node_x, node_y;
        node_x = lay(*i, 0);
        node_y = lay(*i, 1);
        
        double dist = sqrt((node_x - center_x) * (node_x - center_x) + (node_y - center_y) * (node_y - center_y));
        dist *= 2;
        if(dist > size)
            size = dist;
    }
}

inline void Quadrant::create_subquadrant(const list<unsigned int> &_nodes)
{
    Quadrant *q = new Quadrant(_nodes, lay, nodes_mass);
    subQuadrants.push_back(q);
}

void Quadrant::partition(void)
{
    if(num_nodes > 1)
    {
        list<unsigned int>::const_iterator i;
        list<unsigned int> lower_left, upper_left, lower_right, upper_right, left, right;
        
        for(i = nodes.begin(); i != nodes.end(); i++)
        {
            if(lay(*i, 0) < center_x)
                left.push_back(*i);
            else
                right.push_back(*i);
        }
        
        for(i = left.begin(); i != left.end(); i++)
        {
            if(lay(*i, 1) < center_y)
                lower_left.push_back(*i);
            else
                upper_left.push_back(*i);
        }
        for(i = right.begin(); i != right.end(); i++)
        {
            if(lay(*i, 1) < center_y)
                lower_right.push_back(*i);
            else
                upper_right.push_back(*i);
        }
        //Rprintf("UR: %d\n", upper_right.size());
        //Rprintf("UL: %d\n", upper_left.size());
        //Rprintf("LR: %d\n", lower_right.size());
        //Rprintf("LL: %d\n", lower_left.size());
        
        
        
        if(lower_right.size()) create_subquadrant(lower_right);
        if(upper_right.size()) create_subquadrant(upper_right);
        if(lower_left.size()) create_subquadrant(lower_left);
        if(upper_left.size()) create_subquadrant(upper_left);
    }
}

void Quadrant::Apply_repulsion(unsigned int node, const NumericMatrix &lay, NumericMatrix &dvec, const NumericVector &nodes_size, double krep, bool prevent_overlap, float theta)
{
    if(num_nodes == 1)
    {
        if(node != nodes.front())
        {
            unsigned int j = nodes.front();
            float _mass_prod = nodes_mass[node] * nodes_mass[j];
            float _sum_sizes = nodes_size[node] + nodes_size[j];
            apply_repulsion_one_sided(node, lay, dvec, _mass_prod, _sum_sizes, lay(j, 0), lay(j, 1), krep, prevent_overlap);
        }
    }
    else
    {
        float node_x = lay(node, 0);
        float node_y = lay(node, 1);
        float D = sqrt((node_x - center_x) * (node_x - center_x) + (node_y - center_y) * (node_y - center_y));
        
        if(D * theta > size)
        {
    
            //Rprintf("Mass region: %f\n", nodes_mass[node] * mass);
            //Rprintf("## num_nodes: %d, a: %d, b: %d, x center: %f, y center: %f\n", nodes.size(), node, nodes.front(), center_x, center_y);
            //Rprintf("--- %f, %f, %f\n", nodes_mass[node], mass, nodes_mass[node] * mass);
            //Ignore sum_sizes
            apply_repulsion_one_sided(node, lay, dvec, nodes_mass[node] * mass, -1, center_x, center_y, krep, prevent_overlap);
        }
        else
        {
            list<Quadrant *>::const_iterator x;
            for(x = subQuadrants.begin(); x != subQuadrants.end(); x++)
                (*x)->Apply_repulsion(node, lay, dvec, nodes_size, krep, prevent_overlap, theta);
        }
    }
    
}

inline void Quadrant::apply_repulsion_one_sided(unsigned int i, const NumericMatrix &lay, NumericMatrix &dvec, float mass_prod, float sum_sizes, float x, float y, double krep, bool prevent_overlap)
{
    float square_dist = (lay(i, 0) - x) * (lay(i, 0) - x) +
        (lay(i, 1) - y) * (lay(i, 1) - y);
    
    float f_rep = krep * (mass_prod / square_dist);
    
    if(prevent_overlap && sum_sizes > 0)
    {
        float dist = sqrt(square_dist) - sum_sizes;
        if(dist < 0)
            f_rep = 100 * krep * mass_prod;
        else
            f_rep = krep * (mass_prod / (dist * dist));
    }
//  Rprintf("Sum_sizes: %f, Frep: %f, x: %f, y: %f, mass_prod: %f, square_dist: %f\n", sum_sizes, f_rep, x, y, mass_prod, square_dist);
    dvec(i, 0) += (lay(i, 0) - x) * f_rep;
    dvec(i, 1) += (lay(i, 1) - y) * f_rep;
}

inline void Quadrant::apply_repulsion_two_nodes(unsigned int i, unsigned int j, const NumericMatrix &lay,
                                      NumericMatrix &dvec, const NumericVector &nodes_size, double krep, bool prevent_overlap)
{
    float square_dist = (lay(i, 0) - lay(j, 0)) * (lay(i, 0) - lay(j, 0)) +
        (lay(i, 1) - lay(j, 1)) * (lay(i, 1) - lay(j, 1));
    
    float mass_prod = nodes_mass[i] * nodes_mass[j];
    float f_rep = krep * (mass_prod / square_dist);
    
    if(prevent_overlap)
    {
        float dist = sqrt(square_dist) - (nodes_size[i] + nodes_size[j]);
        if(dist < 0)
            f_rep = 100 * krep * mass_prod;
        else
            f_rep = krep * (mass_prod / (dist * dist));
    }
    
    dvec(i, 0) += (lay(i, 0) - lay(j, 0)) * f_rep;
    dvec(i, 1) += (lay(i, 1) - lay(j, 1)) * f_rep;
    dvec(j, 0) += (lay(j, 0) - lay(i, 0)) * f_rep;
    dvec(j, 1) += (lay(j, 1) - lay(i, 1)) * f_rep;

    
}

void apply_repulsion_barnes_hut(const list<unsigned int> &nodes, const NumericMatrix &lay, NumericMatrix &dvec, const NumericVector &nodes_mass, const NumericVector &nodes_size, double krep, bool prevent_overlap, float theta)
{
    Quadrant *Q = new Quadrant(nodes, lay, nodes_mass);
    
    for(int i = 0; i < lay.nrow(); i++)
    {
        Q->Apply_repulsion(i, lay, dvec, nodes_size, krep, prevent_overlap, theta);
    }
    
    Q->Clear();
    
    delete Q;
}

double get_tolerance(const NumericMatrix &lay)
{
	if(lay.nrow() < 5000)
		return 0.1; 
	else if(lay.nrow() < 50000)
		return 1;
	else
		return 10;
}

void apply_repulsion(const NumericMatrix &lay, NumericMatrix &dvec, const NumericVector &mass, const NumericVector &nodes_size, double krep, bool prevent_overlap)
{
	unsigned int nrow = lay.nrow();
    
	for(unsigned int i = 0; i < (nrow - 1); i++)
		for(unsigned int j = i + 1; j < nrow; j++)
		{
			float square_dist = (lay(i, 0) - lay(j, 0)) * (lay(i, 0) - lay(j, 0)) +
                (lay(i, 1) - lay(j, 1)) * (lay(i, 1) - lay(j, 1));
			
            float mass_prod = mass[i] * mass[j];
            float f_rep = krep * (mass_prod / square_dist);
            
            if(prevent_overlap)
            {
                float dist = sqrt(square_dist) - (nodes_size[i] + nodes_size[j]);
                if(dist < 0)
                    f_rep = 100 * krep * mass_prod;
                else
                    f_rep = krep * (mass_prod / (dist * dist));
            }

            dvec(i, 0) += (lay(i, 0) - lay(j, 0)) * f_rep;
            dvec(i, 1) += (lay(i, 1) - lay(j, 1)) * f_rep;
            dvec(j, 0) += (lay(j, 0) - lay(i, 0)) * f_rep;
            dvec(j, 1) += (lay(j, 1) - lay(i, 1)) * f_rep;
            
		}
}


NumericMatrix operator*(double k, const NumericMatrix &m)
{
	NumericMatrix ret = clone(m);
	for(int i = 0; i < ret.nrow(); i++)
		for(int j = 0; j < ret.ncol(); j++)
			ret(i, j) *= k;
	return ret;
}


// [[Rcpp::export]]
void layout_forceatlas2Cpp(NumericMatrix lay, NumericVector F_att_orig, NumericVector mass, NumericVector nodes_size, NumericMatrix edge_list,
                           NumericVector avg_displ, double kgrav, unsigned int iter, bool prevent_overlap, LogicalVector fixed,
						   NumericVector max_displ, float stopping_tolerance, bool barnes_hut)
{
   // ProfilerStart("forceatlas.prof");
    double krep = lay.nrow() >= 100 ? 2 : 10;
	double tolerance = get_tolerance(lay);
	double speed = 1;
	NumericMatrix old_dvec(lay.nrow(), 2);
	NumericVector F_att = -1 * clone(F_att_orig);
    unsigned int cur_it = 0;
    unsigned int nrow = lay.nrow();
    float theta = 1.2;
    
    list<unsigned int> nodes; for(unsigned int i = 0; i < nrow; i++) nodes.push_back(i);
    
    if(barnes_hut) Rprintf("Using Barnes-Hut approximation\n");
    Rprintf("Stopping tolerance: %f\n", stopping_tolerance);
    
	for(cur_it = 0; cur_it < iter; cur_it++)
	{
        NumericMatrix dvec(nrow, 2);
        
        if(barnes_hut)
            apply_repulsion_barnes_hut(nodes, lay, dvec, mass, nodes_size, krep, prevent_overlap, theta);
        else
            apply_repulsion(lay, dvec, mass, nodes_size, krep, prevent_overlap);
        
        for(int k = 0; k < edge_list.nrow(); k++)
        {
            unsigned int i, j;
            i = edge_list(k, 0); j = edge_list(k, 1);
            double f_att = F_att[k];
            
            if(prevent_overlap)
            {
                float dist = sqrt(lay(i, 0) - lay(j, 0)) * (lay(i, 0) - lay(j, 0)) +
                    (lay(i, 1) - lay(j, 1)) * (lay(i, 1) - lay(j, 1));
                dist -= (nodes_size[i] + nodes_size[j]);
                
                if(dist < 0)
                    f_att = 0;
            }

            dvec(i, 0) += (lay(i, 0) - lay(j, 0)) * f_att;
            dvec(i, 1) += (lay(i, 1) - lay(j, 1)) * f_att;
            dvec(j, 0) += (lay(j, 0) - lay(i, 0)) * f_att;
            dvec(j, 1) += (lay(j, 1) - lay(i, 1)) * f_att;
            
        }
    
        float tot_traction, tot_swinging; tot_traction = tot_swinging = 0.0;
        NumericVector swinging(nrow);

        for(unsigned int i = 0; i < nrow; i++)
        {
            float node_swinging, node_traction;
            node_swinging = node_traction = 0.0;
            
            if(!fixed[i])
            {
                double F_gravity = kgrav * mass[i] / (sqrt(lay(i, 0) * lay(i, 0) + lay(i, 1) * lay(i, 1)));
                dvec(i, 0) = dvec(i, 0) - (lay(i, 0) * F_gravity);
                dvec(i, 1) = dvec(i, 1) - (lay(i, 1) * F_gravity);
                
                for(unsigned int j = 0; j < 2; j++)
                {
                    double a = dvec(i, j);
                    double b = old_dvec(i, j);
                    node_swinging += (a - b) * (a - b);
                    node_traction += (a + b) * (a + b);
                }
                
                node_swinging = sqrt(node_swinging);
                node_traction = sqrt(node_traction);
                
                swinging[i] = node_swinging;
                tot_swinging += node_swinging * mass[i];
                tot_traction += node_traction * mass[i];
                
            }
            else
                dvec(i, 0) = dvec(i, 1) = 0.0;
            
        }
    
        double target_speed = (tolerance * tolerance) * (tot_traction / tot_swinging);
		double max_rise = 0.5;
		
		speed = speed + ((target_speed - speed) < (max_rise * speed) ? target_speed - speed : max_rise * speed);
		
		NumericVector local_speed(nrow);
		
		if(prevent_overlap)
		{
            for(unsigned int i = 0; i < nrow; i++)
            {
                local_speed[i] = 0.1 * (speed / (1 + (speed * sqrt(swinging[i]))));
                float df = sqrt(dvec(i, 0) * dvec(i, 0) + dvec(i, 1) * dvec(i, 1));
                double temp = 10.0 / df;
				if(temp < local_speed[i])
					local_speed[i] = temp;
            }
		}
		else
        {
            for(unsigned int i = 0; i < nrow; i++)
                local_speed[i] = (speed / (1 + (speed * sqrt(swinging[i]))));
        }
        
        old_dvec = clone(dvec);
        float _max, cumsum; _max = cumsum = 0.0;
        
        for(unsigned int i = 0; i < nrow; i++)
        {
            dvec(i, 0) *= local_speed[i];
            dvec(i, 1) *= local_speed[i];
            
            lay(i, 0) = lay(i, 0) + dvec(i, 0);
            lay(i, 1) = lay(i, 1) + dvec(i, 1);
            float abs_x = fabs(dvec(i, 0));
            float abs_y = fabs(dvec(i, 1));
            
            cumsum = cumsum + (abs_x + abs_y);
            if(abs_x > _max)
                _max = abs_x;
            if(abs_y > _max)
                _max = abs_y;
		}
        
		avg_displ[cur_it] = cumsum / (nrow * 2.0);
        max_displ[cur_it] = _max;
		
		if(avg_displ[cur_it] < stopping_tolerance)
            break;
	}
//	ProfilerStop();
    Rprintf("Total number of iterations: %d\n", cur_it);
    
}

