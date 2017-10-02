/*
        Program is testing out a prototype for
        game tree solvers. Tick Tack Toe is 
        especially nice as it's super simple,
        but has some interesting characteristsics
           - Game tree has many nodes
             which are invarant, ie one
             node can be represented by
             a rotation or mirror of 
             another node, which means 
             can have a system which "merges"
             nodes
           - We know a-prori that their is
             a solved solution, which we 
             can visually check
           - We can create the static game 
             tree, not need to worry about
             the complexity
          

        One thing to consider with these sorts
        or designs, is that we done want to
        have redendant data, 


                   
        <start> -- (0,0) -- (1,1) -- ...
                \- (1,0)
                ...
                \- (3,3) -- (1,1)

        Once we have this static game tree, we
        can then work backwards to figure 
        out of an optmial strategy
                

 */

#include <iostream>
#include <tuple>
#include <map>
#include <cassert>
#include <sstream>
#include <random>
#include <algorithm>
#include <vector>
#include <set>
#include <bitset>
#include <boost/lexical_cast.hpp>

#define PRINT(X) do{ std::cout << #X << " = " << ( X ) << "\n"; }while(0)

/*
        Hero villian is arbitary,
        just convention, with Hero
        begin player who does the first
        move
 */
enum Player{
        Player_NotAPlayer,
        Player_Hero,         // first
        Player_Villian       // second
};
inline char PlayerToken(Player p){
        switch (p) {
        case Player_NotAPlayer:
                return ' ';
        case Player_Hero:
                return 'X';
        case Player_Villian:
                return 'O';
        }
}

template<class To, class From>
inline To Cast(From ptr){
        //PRINT( ptr->GetType() );
        //PRINT( std::remove_pointer_t<To>::__type__() );
        if( ptr->GetType() == std::remove_pointer_t<To>::__type__())
                return reinterpret_cast<To>(ptr);
        std::stringstream fmt;
        fmt << "casting " << ptr->GetType() << " to " << std::remove_pointer_t<To>::__type__();
        throw std::domain_error(fmt.str());
}

enum Eval{
        Eval_Win   = 0x01,
        Eval_Draw  = 0x02,
        Eval_Lose  = 0x04,
};
std::string EvalToString(Eval e) {
        switch (e) {
        case Eval_Win:
                return "Eval_Win";
        case Eval_Draw:
                return "Eval_Draw";
        case Eval_Lose:
                return "Eval_Lose";
        }
}


namespace Detail{
        struct PictureBox{
                PictureBox(){
                        lines_.emplace_back();
                }
                void NewLine(){
                        lines_.emplace_back();
                }
                void Append(std::string const& s){
                        lines_.back() += s;
                }
                void Append(char c){
                        lines_.back() += c;
                }
                void Display(){
                        for( size_t i=lines_.size();i!=0;){
                                --i;
                                std::cout << lines_[i] << "\n";
                        }
                }
                PictureBox& operator+=(PictureBox const& that){
                       for(; lines_.size() < that.lines_.size();)
                              NewLine(); 
                       for(size_t i=0;i!=std::min(lines_.size(), that.lines_.size());++i){
                               lines_[i] += that.lines_[i];
                       }
                       return *this;
                }
                static PictureBox Make(std::string const& s){
                        PictureBox result;
                        for( char c : s){
                                switch(c){
                                case '\n':
                                        result.NewLine();
                                        break;
                                default:
                                        result.Append(c);
                                        break;
                                }
                        }
                        return std::move(result);
                }
        private:
                std::vector<std::string> lines_;
        };
} // Detail

struct Board{
        /*
                For each pair of bits, we have

                +----+-----------+
                |Mask|    Data   |
                +----+-----------+
                | 00 | NotAPlayer|
                | 01 |  Hero     |
                | 10 | Villian   |
                +----+-----------+
         */
        Player GetTile(int x, int y)const{
                auto offset = Map_(x,y);
                return static_cast<Player>(( mask_ & ( 0x3 << offset ) ) >> offset);
        }
        void SetTile(int x, int y, Player p){
                assert( GetTile(x,y) == 0 && "Tile already set");
                auto offset = Map_(x,y);
                mask_ |= ( p << offset );
                assert( GetTile(x,y) == p  && "post condition failed");
        }

        friend std::ostream& operator<<(std::ostream& ostr, Board const& board){
                static const char* lineBreak = "+---+---+---+";
                for(int y=3;y!=0;){
                        --y;

                        if( y == 2 ){
                                ostr << lineBreak << "\n";
                        }

                        for(int x=0;x!=3;++x){
                                ostr << "| " << PlayerToken(board.GetTile(x,y)) << " ";
                                if( x == 2 )
                                        ostr << "|\n";
                        }
                        ostr << lineBreak;
                        if( y != 0 )
                                ostr << "\n";
                }
                return ostr;
        }
        std::string ToString()const{
                std::stringstream sstr;
                sstr << *this;
                return sstr.str();
        }
        auto GetMask()const{ return mask_; }

        auto GetInvariant()const{
                /*
                        a b c
                        d e f
                        g h i
                 */
                auto a = 0;
                auto b = 1;
                auto c = 2;
                auto d = 3;
                auto e = 4;
                auto f = 5;
                auto g = 6;
                auto h = 7;
                auto i = 8;

                std::vector<std::vector<int> > perms = {
                        /*
                        Normal Case (Rotate 0)
                         */
                        { a, b, c,
                          d, e, f,
                          g, h, i },
                        /*
                        Rotate 90
                         */
                        {  g, d, a,
                           h, e, b,
                           i, f, c },
                        
                        /*
                        Rotate 180
                         */
                        {  i, h, g,
                           f, e, d,
                           c, b, a },
                        /*
                        Rotate 270
                         */
                        {  c, f, i,
                           b, e, h,
                           a, d, g },
                        /*
                        Mirror Above
                         */
                        {  g, h, i,
                           d, e, f,
                           a, b, c },
                        
                        /*
                        Mirror Center
                         */
                        {  c, b, a,
                           f, e, d,
                           i, h, g } 
                };

                std::vector<std::uint32_t> masks;
                for( auto const& p : perms ){
                        static std::vector<std::pair<int, int> > cord = {
                                { 0, 0},
                                { 1, 0},
                                { 2, 0},
                                { 0, 1},
                                { 1, 1},
                                { 2, 1},
                                { 0, 2},
                                { 1, 2},
                                { 2, 2}
                        };
                        Board aux;
                        for(size_t idx=0;idx!=9;++idx){
                                auto tok = GetTile(cord[idx].first, cord[idx].second);
                                aux.SetTile(cord[p[idx]].first, cord[p[idx]].second, tok);
                        }
                        masks.push_back(aux.GetMask());
                }
                return *std::min_element(masks.begin(), masks.end());
        }
private:
        static int Map_(int x, int y){
                return 2 * ( x * 3 + y );
        }
        std::uint32_t mask_ = 0;
};

/*
        Only one thing in the context, whos turn is it
        Obviously the Board is part of the context,
        but we have 2-tuples
                (B,C),
        to represent this
*/

enum GameCtrl{
        GameCtrl_Continue,
        GameCtrl_HeroWins,
        GameCtrl_VillianWins,
        GameCtrl_Draw
};
const char* GameCtrlToString(GameCtrl e) {
        switch (e) {
        case GameCtrl_Continue:
                return "GameCtrl_Continue";
        case GameCtrl_HeroWins:
                return "GameCtrl_HeroWins";
        case GameCtrl_VillianWins:
                return "GameCtrl_VillianWins";
        case GameCtrl_Draw:
                return "GameCtrl_Draw";
        }
}

struct GameContext{
        explicit GameContext(Board board = Board{}):board_{std::move(board)}{}
        explicit GameContext(Player active):active_{active}{}
        auto const& GetBoard()const{ return board_; }
        auto& GetBoard(){ return board_; }
        auto ActivePlayer()const{ return active_; }
        char ActivePlayerToken()const{
                return ( ActivePlayer() == 0 ? 'x' : 'o' );
        }
        void NextPlayer(){
                switch(active_){
                case Player_Hero:
                        active_ = Player_Villian;
                        break;
                case Player_Villian:
                        active_ = Player_Hero;
                        break;
                // stop compiler warning
                case Player_NotAPlayer:
                        break;
                }
        }
        auto GetCtrl()const{ return ctrl_; }
        auto SetCtrl(GameCtrl ctrl){ ctrl_ = ctrl; }
        friend bool operator<(GameContext const& lp, GameContext const& rp){
                #if 1
                return std::make_tuple(lp.board_.GetInvariant(), lp.ctrl_, lp.active_ )
                     < std::make_tuple(rp.board_.GetInvariant(), rp.ctrl_, rp.active_ );
                #else
                return std::make_tuple(lp.board_.GetMask(), lp.ctrl_, lp.active_ )
                     < std::make_tuple(rp.board_.GetMask(), rp.ctrl_, rp.active_ );
                #endif
        }
private:
        Board board_;
        GameCtrl ctrl_{ GameCtrl_Continue };
        Player active_{ Player_Hero };
};

struct TickTackToeLogic{
        GameContext Next(GameContext ctx, size_t x, size_t y){
                assert( ctx.GetCtrl()  == GameCtrl_Continue && "precondition failed");
                assert( ctx.GetBoard().GetTile(x,y)    == 0          && "precondition failed");

                ctx.GetBoard().SetTile(x,y, ctx.ActivePlayer() );

                // First need to check to see if someone has made a line
                auto triple = [&](auto x0, auto y0,
                                 auto x1, auto y1,
                                 auto x2, auto y2){
                        auto tok = ctx.GetBoard().GetTile(x0, y0);
                        if( tok == Player_NotAPlayer )
                                return false;
                        return 
                                tok == ctx.GetBoard().GetTile(x1,y1) &&
                                tok == ctx.GetBoard().GetTile(x2,y2);
                };

                static std::vector< std::vector<size_t> > protos = 
                {
                        { 0, 0, 0, 1, 0, 2}, 
                        { 1, 0, 1, 1, 1, 2}, 
                        { 2, 0, 2, 1, 2, 2}, 
                        { 0, 0, 1, 0, 2, 0}, 
                        { 0, 1, 1, 1, 2, 1}, 
                        { 0, 2, 1, 2, 2, 2}, 
                        { 0, 0, 1, 1, 2, 2}, 
                        { 0, 2, 1, 1, 2, 0}
                };

                for( auto const& p : protos){
                        if( triple( p[0], p[1], p[2], p[3], p[4], p[5]) ){
                                if( ctx.GetBoard().GetTile(p[0], p[1]) == Player_Hero){
                                        ctx.SetCtrl( GameCtrl_HeroWins );
                                } else {
                                        ctx.SetCtrl( GameCtrl_VillianWins );
                                }
                                return std::move(ctx);
                        }
                }

                // If we get here, their isn't a winner
                auto is_full = [&](){
                        for( size_t i=0;i!=3;++i){
                                for( size_t j=0;j!=3;++j){
                                        if( ctx.GetBoard().GetTile(i,j) == 0 )
                                                return false;

                                }
                        }
                        return true;
                };

                if( is_full() )
                        ctx.SetCtrl( GameCtrl_Draw );
                else
                        ctx.NextPlayer();
                return std::move(ctx);
        }
};


struct Node{
        enum Type{
                Type_Choice,
                Type_Payoff
        };
public:
        Type GetType()const{ return type_; }
        GameContext const& GetContext()const{ return ctx_; }
protected:
        explicit Node(Type type, GameContext ctx):
                type_{type},
                ctx_{std::move(ctx)}
        {}
private:
        Type type_;
        GameContext ctx_;
};

struct ChoiceNode : Node{
        explicit ChoiceNode(GameContext ctx):
                Node{Type_Choice, std::move(ctx)}
        {}
        // only when Choice
        void RegisterChild(Node* ptr){
                next_.push_back(ptr);
        }
        auto NumChildren()const{
                return next_.size();
        }
        auto begin()const{ return next_.begin(); }
        auto end()const{ return next_.end(); }
        static Type __type__(){ return Type_Choice; }
private:
        std::vector<Node*> next_;
};

struct PayoffNode : Node{
        explicit PayoffNode(GameContext ctx, Eval eval):
                Node{Type_Payoff, std::move(ctx)}
                ,eval_{eval}
        {}
        auto GetPayoff()const{ return eval_; }
        static Type __type__(){ return Type_Payoff; }
private:
        Eval eval_;
};

// TODO, rather than create a new graph, use this
struct NodeReference{
private:
        Node* ptr_;
        std::vector<Node*> next_;
};

struct GameTree{
        GameTree(Node* root):root_{root}{
                Register(root);
        }
        void AppendTerminal(Node* ptr){
                terminals_.push_back(ptr);
        }
        auto GetRoot()const{
                return root_;
        }
        auto const& GetTerminals()const{ return terminals_; }
        void Register(Node* ptr){
                world_.emplace(ptr->GetContext(), ptr);
                aux_.push_back(ptr);
        }
        Node const* Lookup(GameContext const& ctx)const{
                auto iter = world_.find(ctx);
                if( iter == world_.end())
                        return nullptr;
                return iter->second;
        }
        auto begin()const{ return aux_.begin(); }
        auto end()const{ return aux_.end(); }
private:
        Node* root_;
        std::vector<Node*> terminals_;
        std::map< GameContext, Node* > world_;
        std::vector<Node*> aux_;
};


/*
        Idea here, is that when creating a board
        which is invariant to anoher already 
        created, we can return the reference
 */
struct NodeFactory{
        std::pair<bool, Node*> Make(GameContext ctx){
                auto iter = world_.find(ctx);
                if( iter != world_.end() )
                        return std::make_pair(false, iter->second);

                Node* ptr = nullptr;
                switch(ctx.GetCtrl()){
                case GameCtrl_Continue:
                        ptr = new ChoiceNode{ctx};
                        break;
                case GameCtrl_HeroWins:
                        ptr = new PayoffNode{ctx, Eval_Win};
                        break;
                case GameCtrl_VillianWins:
                        ptr = new PayoffNode{ctx, Eval_Lose};
                        break;
                case GameCtrl_Draw:
                        ptr = new PayoffNode{ctx, Eval_Draw};
                        break;
                }
                world_.emplace(ctx, ptr);
                return std::make_pair(true, ptr);
        }

private:
        std::map< GameContext, Node* > world_;
};

/*
        this is used to abstrauct the construction of
        the game tree
*/
struct GameTreeBuilder{
        void Generate(GameTree& tree, ChoiceNode* parent, GameContext ctx){
                static TickTackToeLogic logic;
                for( int x=0;x!=3;++x){
                        for( int y=0;y!=3;++y){
                                if( ctx.GetBoard().GetTile(x,y) == 0 ){
                                        
                                        auto nextCtx = logic.Next(ctx, x,y);

                                        auto makeRet = fac_.Make(nextCtx);
                                        auto ptr = makeRet.second;
                                        if( makeRet.first){
                                                // New node

                                                // First time here
                                                tree.Register(ptr);

                                                switch(nextCtx.GetCtrl()){
                                                case GameCtrl_Continue:
                                                        Generate(tree, Cast<ChoiceNode*>(ptr), nextCtx);
                                                        break;
                                                case GameCtrl_HeroWins:
                                                case GameCtrl_VillianWins:
                                                case GameCtrl_Draw:
                                                        tree.AppendTerminal(ptr);
                                                        break;
                                                }
                                        }
                                        parent->RegisterChild(ptr);
                                }
                        }
                }
        }
        GameTree Make(GameContext ctx){
                auto ptr = new ChoiceNode{ctx};
                GameTree tree{ptr};
                // Assume board isn't finished
                Generate(tree, ptr, ctx);
                return std::move(tree);
        }
private:
        NodeFactory fac_;
};


void test0(){
        GameContext ctx;
        Board board;
        TickTackToeLogic logic;

        std::vector< std::tuple<int, int> > todo = {
                { 0, 0 },
                { 1, 1 },
                { 0, 1 },
                { 0, 2 },
                { 1, 2 },
                { 2, 0 }
        };

        for( auto const& m : todo ){
                ctx = logic.Next(ctx, std::get<0>(m), std::get<1>(m) );
                PRINT(GameCtrlToString(ctx.GetCtrl()));
                //std::cout << board;
        }


}

void DisplayImpl(std::vector<Node*> history){
        if( history.size() == 0 )
                return;
        auto target = history.back();
        size_t indent_width = ( history.size() - 1 ) * 17;
        Detail::PictureBox indent;
        for(size_t i=7;i!=0;){
                --i;
                indent.Append(std::string(indent_width, ' ') );
                if( i != 0 )
                        indent.NewLine();
        }
        
        if( history.back()->GetType() == Node::Type_Payoff ){
                static auto spacer = [](){
                        Detail::PictureBox proto;
                        proto.Append("    ");
                        proto.NewLine();
                        proto.Append("    ");
                        proto.NewLine();
                        proto.Append("    ");
                        proto.NewLine();
                        proto.Append(" => ");
                        proto.NewLine();
                        proto.Append("    ");
                        proto.NewLine();
                        proto.Append("    ");
                        proto.NewLine();
                        proto.Append("    ");
                        return std::move(proto);
                }();

                Detail::PictureBox picture;
                #if 0
                picture += indent;
                picture += Detail::PictureBox::Make( history.back()->GetContext().GetBoard().ToString() );
                #endif
                for( auto ptr : history){
                        picture += Detail::PictureBox::Make( ptr->GetContext().GetBoard().ToString() );
                        picture += spacer;

                }
                picture += spacer;

                Detail::PictureBox leaf;
                leaf.NewLine();
                leaf.NewLine();
                leaf.NewLine();
                leaf.Append("Payoff " + boost::lexical_cast<std::string>(Cast<PayoffNode*>(target)->GetPayoff()));

                picture += leaf;
                picture.Display();

        } else {
                #if 0
                Detail::PictureBox picture;
                picture += indent;
                picture += Detail::PictureBox::Make( history.back()->GetContext().GetBoard().ToString() );
                picture.Display();
                #endif
                for( auto ptr : *Cast<ChoiceNode*>(target)){
                        history.push_back(ptr);
                        DisplayImpl(history);
                        history.pop_back();

                }
        }

}
void Display(Node* ptr){
        std::vector<Node*> history = {ptr};
        DisplayImpl(history);
}


/*
        Need to iterate the game tree, and evaluate each node.
        For nodes where it's the player, need to decide 
        which is the best move by backwards induction


        From a payoff node, We find the most recent move the 
        player made, and mark is a Never.


        Idea here is that for every move we do, we try to
        take the move with is gurenteed a win, else we take
        the move with a either a win or draw (depending on opp), 
        then we take move with is only draw, then we take move
        which is win or lose, then take move with is win lose draw


        Note that in the game theory sense of a strategy, we
        need a move for every possible board, so this includeds
        taking a move we know will lose, even though we won't get
        their following out path.

        On nodes where it's the opponents move, we basically 
        assume that he is going to take the optimum move, if their
        is a move we can take, which results in a node with
 */


struct EvalMetric{
        struct Result{
                unsigned Mask()const{ return mask_; }
                auto begin()const{ return next_.begin(); }
                auto end()const{ return next_.end(); }
        private:
                friend struct EvalMetric;
                unsigned mask_;
                std::vector<Node const*> next_;
        };
        static std::unique_ptr<EvalMetric> MakeForHero(GameTree const& tree){
                auto ptr = std::make_unique<EvalMetric>();
                ptr->Populate_(Player_Hero, tree.GetRoot());
                return std::move(ptr);
        }
        auto const& operator()(GameContext const& ctx)const{
                auto iter = eval_.find(ctx);
                if( iter == eval_.end()){
                        throw std::domain_error("not in tree");
                }
                return iter->second;
        }
private:


        unsigned Populate_(Player p, Node const* node){
                auto ctx = node->GetContext();
                if( eval_.count(ctx) != 0 )
                        return eval_[ctx].mask_;

                if( node->GetType() == Node::Type_Payoff ){
                        static std::map<Eval,Eval> invMap = {{Eval_Lose, Eval_Win}, {Eval_Draw, Eval_Draw}, {Eval_Win, Eval_Lose}};
                        auto payoff = Cast<PayoffNode const*>(node)->GetPayoff();
                        if( p  == Player_Villian ){
                                payoff = invMap[payoff];
                        }
                        eval_[ctx].mask_ = payoff;
                        return eval_[ctx].mask_;
                }

                
                auto choicePtr = Cast<ChoiceNode const*>(node);


                if(ctx.ActivePlayer() == p ){
                        [&](){
                                /*
                                        Want to find the path in this order,
                                        first we want to find nodes where we win,
                                        then win or draw (depending on opps move),
                                        etc
                                 */
                                static std::vector<unsigned> ticker = {
                                        Eval_Win,
                                        Eval_Win | Eval_Draw,
                                        Eval_Draw,
                                        Eval_Win | Eval_Lose,
                                        Eval_Win | Eval_Draw | Eval_Lose,
                                        Eval_Draw | Eval_Lose,
                                        Eval_Lose
                                };
                                for( auto t : ticker ){
                                        bool found = false;
                                        for( auto child : *choicePtr){
                                                if( Populate_(p, child) == t ){
                                                        // Their may be more than one
                                                        found = true;
                                                        eval_[ctx].mask_ = t;
                                                        eval_[ctx].next_.push_back(child);
                                                }
                                        }
                                        if( found )
                                                return;
                                }
                                assert( choicePtr->NumChildren() == 0 );
                        }();
                        return eval_[ctx].mask_;
                } else {
                        unsigned e = 0;
                        auto& ref = eval_[ctx];
                        for( auto child : *choicePtr){
                                e |= Populate_(p, child);
                                ref.next_.push_back(child);
                        }
                        ref.mask_ = e;
                        return e;
                }
        }
        std::map<GameContext, Result> eval_;
};

struct StrategyBuilder{
        GameTree Build(GameTree const& tree){
                NodeFactory fac;

                auto rootRet = fac.Make(tree.GetRoot()->GetContext());
                auto root = rootRet.second;

                GameTree stratTree{root};

                auto metric = EvalMetric::MakeForHero(tree);

                std::vector<Node*> stack;
                stack.push_back(root);
                for(;stack.size();){
                        auto ptr = stack.back();
                        stack.pop_back();

                        if( ptr->GetType() == Node::Type_Payoff)
                                continue;

                        auto choicePtr = Cast<ChoiceNode*>(ptr);

                        switch(ptr->GetContext().ActivePlayer()){
                        case Player_Hero:{

                                auto item = (*metric)(ptr->GetContext());

                                for( auto branch : item ){
                                        auto makeRet =fac.Make(branch->GetContext()); 
                                        auto next = makeRet.second;
                                        if( makeRet.first ){
                                                stratTree.Register(next);
                                                stack.push_back(next);
                                        }
                                        choicePtr->RegisterChild(next);
                                        break;
                                }
                                break;
                        }
                        /*
                                For villian we just need to copy
                                every possible move
                        */
                        case Player_Villian: {
                                auto aux = tree.Lookup(ptr->GetContext());
                                if( aux == nullptr ){
                                        std::cerr << "Can't find :(\n";
                                        break;
                                }
                                for( auto child : *Cast<ChoiceNode const*>(aux)){

                                        auto makeRet = fac.Make(child->GetContext());
                                        auto next = makeRet.second;
                                        if( makeRet.first ){
                                                stratTree.Register(next);
                                                stack.push_back(next);
                                        }
                                        choicePtr->RegisterChild(next);
                                }
                                break;
                        }
                        default:
                                break;
                        }
                }

                return std::move(stratTree);
        }
};

void RenderToDot(GameTree const& tree){
        std::cout << "digraph structs{\n";
        std::cout << "node [shape=record]\n";

        unsigned terminalMask = 0;

        auto tag = [](Node const* ptr){
                return "Node" + boost::lexical_cast<std::string>(ptr);
        };

        for( auto ptr : tree){
                auto t = tag(ptr);
                auto const& b = ptr->GetContext().GetBoard();
                std::cout 
                        << t << " [shape=record,label=\""
                        << "{"
                                << "{"
                                << PlayerToken(b.GetTile(0,0)) << "|"
                                << PlayerToken(b.GetTile(1,0)) << "|"
                                << PlayerToken(b.GetTile(2,0))
                                << "}"
                        << "|"
                                << "{"
                                << PlayerToken(b.GetTile(0,1)) << "|"
                                << PlayerToken(b.GetTile(1,1)) << "|"
                                << PlayerToken(b.GetTile(2,1))
                                << "}"
                        << "|"
                                << "{"
                                << PlayerToken(b.GetTile(0,2)) << "|"
                                << PlayerToken(b.GetTile(1,2)) << "|"
                                << PlayerToken(b.GetTile(2,2))
                                << "}"
                        << "}\"];\n";
                if( ptr->GetType() == Node::Type_Choice ){
                        for( auto next : *Cast<ChoiceNode*>(ptr)){
                                std::cout << t << " -> " << tag(next) << ";\n";
                        }
                }

                if( ptr->GetType() == Node::Type_Payoff ){
                        auto payoff = Cast<PayoffNode*>(ptr)->GetPayoff();
                        std::cout << t << " -> " << EvalToString(payoff) << ";\n";
                        terminalMask |= payoff;
                }
        }

        std::cout << "}";
}

enum RenderOpt{
        RenderOpt_Solve,
};
void render(Player p, RenderOpt opt){

        TickTackToeLogic logic;
        GameTreeBuilder builder;
        GameContext ctx(p);
        auto tree = builder.Make(ctx);
        
        switch(opt){
        case RenderOpt_Solve:{
                StrategyBuilder sb;
                auto ret = sb.Build(tree);
                RenderToDot(ret);
        }
                break;
        }
}

void test2(){
        Detail::PictureBox first, second;
        first.Append("hello");
        first.NewLine();
        first.Append("world");
        second.Append(" oh ");
        second.NewLine();
        second.Append("yeah");
        first += second;
        first.Display();
}


void driver(){
        GameContext ctx{Player_Villian};
        GameTreeBuilder builder;
        TickTackToeLogic logic;
        auto tree = builder.Make(ctx);

        StrategyBuilder sb;
        auto strat = sb.Build(tree);

        std::random_device gen;
        std::uniform_int_distribution<int> dist(0,666);
        

        for(;ctx.GetCtrl() == GameCtrl_Continue;){
                std::cout << ctx.GetBoard() << "\n";

                switch(ctx.ActivePlayer()){
                case Player_Hero: {
                        auto move = Cast<ChoiceNode const*>(strat.Lookup(ctx));

                        assert( !! move );

                        // dedeuce move
                        auto randomOffset = dist(gen) % move->NumChildren();
                        auto nextMove = *std::next(move->begin(), randomOffset );
                        PRINT(move->NumChildren());
                        auto t = [&](){
                                for(size_t i=0;i!=3;++i){
                                        for(size_t j=0;j!=3;++j){
                                                if(     move->GetContext().GetBoard().GetTile(i,j) !=
                                                    nextMove->GetContext().GetBoard().GetTile(i,j) ){
                                                        return std::make_tuple(i,j);
                                                }

                                        }
                                }
                                throw std::domain_error("not move");
                        }();
                        
                        ctx = logic.Next(ctx, std::get<0>(t), std::get<1>(t));
                        break;
                }
                case Player_Villian: {
                        char x_s, y_s;
                        std::cin >> x_s >> y_s;
                        do{
                                if(! ( std::isdigit(x_s) && std::isdigit(y_s))){
                                        std::cerr << "Invalid Move\n";
                                        break;
                                }
                                int x = x_s - '0';
                                int y = y_s - '0';
                                if( ctx.GetBoard().GetTile(x,y) != Player_NotAPlayer ){
                                        std::cerr << "Invalid Move\n";
                                        break;
                                }
                                ctx = logic.Next(ctx, x - '0', y - '0');
                        }while(0);
                        break;
                }
                default:
                        break;
                }
        }
        std::cout << ctx.GetBoard() << "\n";
        switch(ctx.GetCtrl()){
        case GameCtrl_Continue:
                break;
        case GameCtrl_HeroWins:
                std::cout << "Computer Won\n";
                break;
        case GameCtrl_VillianWins:
                std::cout << "You Won\n";
                break;
        case GameCtrl_Draw:
                std::cout << "Draw\n";
                break;
        default:
                return;
        }
}



int main(int argc, char* argv[]){
        try{
                switch(argc){
                case 1:
                        driver();
                        break;
                case 2: {
                        std::string arg = argv[1];
                        if( arg == "--hero" ){
                                render(Player_Hero, RenderOpt_Solve);
                        } else if(arg == "--villian"){
                                render(Player_Villian, RenderOpt_Solve);
                        } else if(arg == "--driver"){
                                driver();
                        } else{
                        }
                        break;
                }
                default:
                        std::cerr  << "unknown args\n";
                        return EXIT_FAILURE;
                }
        } catch(std::exception const& e){
                std::cerr << e.what() << "\n";
                return EXIT_FAILURE;
        }
}

