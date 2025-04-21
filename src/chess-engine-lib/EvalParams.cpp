#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,     1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        1.076343f,     0.199990f,    0.166418f,    0.371016f,    0.575089f,    1.000000f,
        -0.005937f,    0.127334f,    0.064202f,    0.031037f,    218.191772f,  -36.727123f,
        196.701660f,   402.951477f,  339.637573f,  405.509064f,  426.234283f,  539.939636f,
        775.047119f,   1502.353394f, 1048.176758f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -71.541542f,  192.337692f,  -78.056221f,
        190.070892f,   -71.979340f,  206.803299f,  -70.224030f,  177.627823f,  -48.966961f,
        199.120392f,   -17.647322f,  198.875580f,  8.220877f,    185.286728f,  -65.051468f,
        173.890396f,   -50.924335f,  189.498825f,  -58.581158f,  195.277771f,  -60.065533f,
        192.220932f,   -33.504314f,  199.870987f,  -27.625925f,  205.878494f,  -22.165112f,
        203.179596f,   -15.111739f,  195.562622f,  -29.833143f,  179.904877f,  -40.637333f,
        192.769241f,   -47.661800f,  202.229553f,  -31.884033f,  193.057938f,  -23.839041f,
        214.217590f,   -32.184742f,  216.354645f,  -26.902340f,  213.143143f,  -46.386711f,
        217.092667f,   -31.490955f,  192.847153f,  -26.737007f,  223.828522f,  -32.872906f,
        230.424683f,   -28.111446f,  217.028610f,  -0.633032f,   206.822357f,  2.404150f,
        208.426895f,   -26.692768f,  225.114548f,  -70.048546f,  220.859482f,  -12.727957f,
        197.169144f,   -39.769226f,  233.679428f,  -32.116135f,  258.490112f,  1.361720f,
        232.973129f,   42.957130f,   202.460968f,  29.887243f,   337.874786f,  70.682373f,
        194.348068f,   51.397404f,   238.936401f,  48.173481f,   167.455231f,  0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     284.245361f,  354.652313f,  353.911133f,  267.144531f,  356.543243f,
        320.777252f,   357.605286f,  359.179138f,  383.619904f,  324.276520f,  384.057281f,
        313.650757f,   325.492249f,  287.156281f,  378.261475f,  302.834137f,  368.544067f,
        308.373291f,   292.664398f,  371.074615f,  400.717957f,  329.276337f,  400.588287f,
        331.667999f,   400.395386f,  325.240265f,  432.479797f,  318.566376f,  417.466461f,
        313.405029f,   355.686493f,  336.808319f,  360.939026f,  338.258972f,  407.818848f,
        321.372253f,   382.390381f,  331.880463f,  408.122345f,  359.901520f,  438.033569f,
        356.944702f,   411.418152f,  308.256378f,  424.617981f,  302.187073f,  353.239105f,
        312.639740f,   369.725464f,  369.133728f,  381.662994f,  361.161560f,  424.332581f,
        365.782166f,   412.585999f,  373.644531f,  422.409424f,  365.223053f,  432.024170f,
        342.451996f,   429.575806f,  334.977875f,  398.537140f,  334.262695f,  391.982697f,
        355.968201f,   406.973633f,  370.781952f,  437.836212f,  360.534149f,  495.803558f,
        344.203186f,   420.474854f,  366.545044f,  487.975464f,  344.003235f,  430.195618f,
        366.536499f,   438.524109f,  329.129761f,  335.863495f,  339.199768f,  432.537903f,
        343.189392f,   448.984100f,  340.096649f,  481.134583f,  351.941833f,  499.523590f,
        337.270050f,   543.912048f,  330.423828f,  447.142639f,  341.113251f,  434.113678f,
        339.028046f,   399.752594f,  336.256165f,  402.004425f,  365.522156f,  410.249878f,
        332.752716f,   457.694946f,  360.832855f,  436.250153f,  347.668884f,  516.581482f,
        325.592133f,   415.279388f,  327.593750f,  253.747360f,  356.166962f,  219.264313f,
        313.494446f,   344.264862f,  334.727051f,  310.921112f,  355.624329f,  346.322632f,
        346.215149f,   449.728760f,  310.150024f,  339.823242f,  350.299835f,  302.757874f,
        348.524231f,   333.006592f,  221.812881f,  343.380890f,  425.873108f,  391.000763f,
        401.401947f,   371.936707f,  428.114014f,  390.175720f,  443.116516f,  373.010132f,
        448.082245f,   375.152069f,  399.272064f,  351.814270f,  429.688995f,  373.947632f,
        452.799255f,   405.313965f,  381.232697f,  395.865143f,  409.749451f,  396.719452f,
        420.203583f,   391.441040f,  439.527344f,  405.541168f,  429.300079f,  416.446930f,
        419.836853f,   455.395752f,  399.412201f,  377.501007f,  381.663483f,  396.478058f,
        421.289520f,   424.119019f,  395.763916f,  414.080261f,  439.133820f,  428.662994f,
        422.970520f,   418.725616f,  432.199677f,  436.904388f,  422.484802f,  433.340210f,
        394.410828f,   425.677643f,  383.614624f,  412.726410f,  396.673096f,  402.955078f,
        432.773071f,   423.876251f,  431.604858f,  425.402618f,  437.864532f,  443.786224f,
        426.042725f,   421.914185f,  433.195160f,  432.898773f,  413.216278f,  437.571411f,
        362.633148f,   401.039398f,  427.772369f,  427.764374f,  445.466705f,  426.799500f,
        428.600555f,   460.448944f,  423.120056f,  446.728577f,  437.301086f,  452.893250f,
        432.966553f,   409.857971f,  439.648102f,  401.661377f,  415.398468f,  433.211212f,
        418.997253f,   448.989960f,  434.531250f,  463.342621f,  439.923981f,  497.578796f,
        389.344147f,   494.260529f,  414.970551f,  511.096954f,  412.079315f,  447.331024f,
        445.017334f,   465.497131f,  401.555817f,  393.341339f,  414.726044f,  399.451324f,
        464.552887f,   446.025970f,  430.438171f,  407.481354f,  456.074402f,  408.481659f,
        437.022644f,   411.351593f,  447.212433f,  439.343719f,  423.846985f,  358.295258f,
        360.899139f,   364.423309f,  449.917084f,  421.704163f,  411.482697f,  293.060364f,
        470.240936f,   350.113892f,  454.760895f,  417.783966f,  434.004852f,  313.056427f,
        464.969452f,   462.103546f,  396.314026f,  472.658112f,  394.989746f,  552.359924f,
        759.880310f,   543.735413f,  760.895630f,  535.174133f,  770.918274f,  544.407410f,
        762.583801f,   541.377136f,  757.839661f,  525.107727f,  772.035217f,  509.260956f,
        775.603882f,   530.672729f,  728.197266f,  534.264282f,  776.083252f,  525.331970f,
        756.330505f,   521.177979f,  776.402283f,  525.515015f,  777.539368f,  542.703674f,
        763.417786f,   547.029602f,  747.449219f,  526.602051f,  766.155212f,  440.881989f,
        793.725830f,   496.871490f,  784.904297f,  493.322784f,  770.399963f,  493.628387f,
        780.207886f,   532.695435f,  768.661255f,  543.132629f,  752.998413f,  531.728638f,
        761.813843f,   546.077942f,  759.989624f,  543.298767f,  750.237244f,  495.968262f,
        788.372314f,   514.243530f,  785.299438f,  517.963196f,  787.132080f,  529.740784f,
        775.338379f,   525.350708f,  777.640930f,  515.130310f,  782.880188f,  530.059875f,
        784.179321f,   508.238556f,  784.112671f,  528.593384f,  793.473572f,  544.144653f,
        780.139832f,   548.928894f,  781.266663f,  552.126160f,  770.977478f,  510.900238f,
        790.119080f,   545.129150f,  776.481384f,  529.451416f,  782.779480f,  511.416840f,
        784.785095f,   517.574219f,  804.164673f,  531.548889f,  781.545776f,  541.650879f,
        780.620178f,   555.809509f,  774.236816f,  601.944397f,  758.399780f,  572.103394f,
        770.285278f,   570.822937f,  779.652222f,  591.301575f,  767.109863f,  549.349365f,
        801.752319f,   540.582336f,  803.664917f,  549.347595f,  803.555603f,  581.399353f,
        790.796570f,   541.604187f,  808.668457f,  612.969910f,  777.157593f,  572.116333f,
        790.005310f,   635.212341f,  779.323792f,  555.464966f,  767.182190f,  575.071716f,
        777.231934f,   586.482483f,  784.693237f,  554.774292f,  795.013611f,  559.659119f,
        797.238159f,   547.338745f,  800.649963f,  563.512878f,  795.876465f,  576.221313f,
        795.648010f,   1512.479492f, 1026.412720f, 1512.391724f, 1036.415649f, 1498.174438f,
        1014.759277f,  1495.063110f, 1033.604126f, 1493.504517f, 1021.234558f, 1453.241821f,
        1000.856689f,  1396.631714f, 1027.773071f, 1278.869263f, 1224.663452f, 1514.553467f,
        1023.841797f,  1504.519897f, 1019.653076f, 1511.922729f, 1035.671509f, 1496.940918f,
        1059.389648f,  1517.632568f, 992.914856f,  1509.611572f, 999.528687f,  1466.309692f,
        954.414246f,   1469.317871f, 990.077881f,  1507.705322f, 1066.581177f, 1502.150146f,
        1060.016357f,  1480.871704f, 1117.139526f, 1499.946045f, 1052.833862f, 1492.770020f,
        1070.141846f,  1509.238647f, 1052.867310f, 1518.377563f, 1006.225525f, 1462.089966f,
        1093.774658f,  1506.546143f, 1055.746460f, 1506.304077f, 1052.960815f, 1505.445801f,
        1038.364746f,  1507.994141f, 1057.380737f, 1501.089478f, 1088.012695f, 1495.770630f,
        1078.560913f,  1481.515381f, 1073.948853f, 1500.947144f, 1061.857422f, 1509.833130f,
        1073.794434f,  1478.992676f, 1125.444092f, 1480.037720f, 1104.121094f, 1510.313354f,
        1076.239258f,  1519.607178f, 1046.551758f, 1513.660400f, 1078.338257f, 1509.616699f,
        1061.978027f,  1499.265869f, 1044.525146f, 1518.597534f, 1004.010803f, 1539.202148f,
        1019.003723f,  1510.920410f, 1069.784546f, 1515.606812f, 1051.338257f, 1524.561401f,
        1050.150269f,  1633.106689f, 978.369385f,  1593.161133f, 980.008118f,  1531.490112f,
        1026.495117f,  1496.746826f, 1062.827759f, 1491.558350f, 1092.517700f, 1510.497070f,
        1068.246216f,  1536.005615f, 1030.392212f, 1563.965088f, 1013.344177f, 1534.705688f,
        1058.612549f,  1581.744263f, 1001.276489f, 1612.899780f, 951.609436f,  1471.830811f,
        1135.265015f,  1512.875366f, 1063.048584f, 1494.352295f, 1094.882690f, 1497.702271f,
        1060.539429f,  1526.078247f, 1078.054443f, 1541.313721f, 1066.540283f, 1559.121704f,
        1026.187866f,  1467.346191f, 1166.003662f, 29.622597f,   -1.977096f,   27.688520f,
        -8.291667f,    -14.736216f,  16.242617f,   -87.823402f,  23.375650f,   -58.374344f,
        11.090128f,    -67.217125f,  35.644180f,   28.432402f,   19.142130f,   35.749878f,
        9.962098f,     4.595098f,    -1.583763f,   18.318214f,   5.548011f,    -23.448875f,
        -18.673046f,   -96.410713f,  16.359623f,   -89.595879f,  10.573442f,   -60.112885f,
        8.372259f,     14.823907f,   -2.877357f,   26.468758f,   -2.125103f,   -40.292160f,
        -11.436866f,   5.325642f,    -22.858433f,  -89.473534f,  -5.525002f,   -48.785748f,
        -12.293105f,   -73.373116f,  -2.753592f,   -95.156929f,  1.126166f,    -37.739258f,
        -21.189867f,   -59.284637f,  -19.902758f,  -32.638741f,  -15.185832f,  59.598671f,
        -37.407337f,   -4.238693f,   -27.703209f,  -53.726978f,  -19.472099f,  -43.231949f,
        -17.634983f,   -38.804127f,  -21.267208f,  -11.220178f,  -37.401730f,  -111.732132f,
        -31.343781f,   3.076405f,    -7.878559f,   14.819692f,   -0.390140f,   -31.579390f,
        4.623309f,     5.126553f,    -11.524752f,  -36.655704f,  -23.417770f,  59.745224f,
        -43.171238f,   99.205742f,   -42.804924f,  -38.034264f,  -33.151962f,  -45.639759f,
        28.454672f,    -15.196225f,  35.264732f,   28.346954f,   24.244209f,   14.675988f,
        -0.119092f,    79.242249f,   -23.404694f,  68.440247f,   -3.106639f,   60.941063f,
        15.316668f,    -42.057850f,  -5.106042f,   35.357594f,   11.186973f,   -20.505865f,
        33.708576f,    44.117008f,   24.884819f,   68.822372f,   9.956961f,    64.266800f,
        -5.490068f,    -48.076458f,  -2.873628f,   -63.738049f,  42.926556f,   274.234436f,
        -87.866005f,   15.337870f,   -85.996208f,  44.440983f,   3.010387f,    -18.174738f,
        36.993587f,    -7.492702f,   14.369035f,   42.315952f,   -7.633682f,   68.550171f,
        -26.511801f,   -506.789520f, 101.440582f,  -548.402893f, -21.792147f,  0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -15.293011f,  85.779594f,   -60.983967f,
        115.685738f,   -49.890850f,  107.254570f,  -171.428391f, 166.938858f,  36.459629f,
        52.406921f,    12.162865f,   64.901489f,   22.517584f,   71.780991f,   -45.026756f,
        63.219830f,    -6.034483f,   107.882378f,  -32.349548f,  124.269341f,  -29.136921f,
        117.512779f,   -5.942831f,   89.606232f,   0.956510f,    97.639427f,   -18.209003f,
        118.619644f,   -21.629063f,  119.280396f,  25.873011f,   65.083649f,   41.443340f,
        149.372833f,   7.664853f,    161.613037f,  -20.292797f,  152.515488f,  10.874971f,
        119.796692f,   -1.608470f,   144.849716f,  25.401360f,   137.826111f,  -8.928198f,
        163.147018f,   36.806225f,   136.500916f,  95.492676f,   225.320602f,  62.220924f,
        234.677490f,   64.280945f,   218.920151f,  64.289948f,   184.026169f,  29.243872f,
        169.697556f,   52.855476f,   173.898010f,  47.591427f,   207.250565f,  51.097061f,
        185.208115f,   167.319077f,  320.422821f,  145.458298f,  330.526489f,  153.030365f,
        306.660217f,   134.227448f,  249.379578f,  128.493011f,  216.543961f,  197.135162f,
        189.365311f,   138.698822f,  261.159607f,  211.649704f,  200.250916f,  374.349579f,
        318.523956f,   305.362488f,  390.100189f,  282.048187f,  393.130920f,  273.534332f,
        317.065155f,   205.021957f,  311.111633f,  74.006927f,   337.809814f,  -38.536751f,
        400.428650f,   184.547256f,  320.991730f,  0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,     14.112044f,   60.667198f,   38.214825f,   8.735842f,    7.195354f,
        25.171293f,    14.946903f,   13.737808f,   31.867029f,   -11.911849f,  21.174004f,
        -6.733141f,    8.211151f,    -5.591413f,   0.000000f,    0.000000f,    -3.203707f,
        -1.488651f,    -12.791639f,  -8.437404f,   -46.715755f,  -96.202446f,  11.527020f,
        -243.740051f,  7.159967f,    8.483311f,    34.336521f,   -10.637037f,  21.646334f,
        -12.176571f,   5.340552f,    3.767783f,    -0.000000f,   0.000000f,    -0.964290f,
        -9.735566f,    6.083906f,    -10.042339f,  -53.583431f,  53.282047f,   38.451260f,
        159.323044f,   28.145958f,   75.224915f,   0.806411f,    16.259390f,   -12.359270f,
        -45.727406f,   18.632263f,   21.950661f,   37.898769f,   2.209513f,    -578.351196f,
        150.612106f,   5.188698f,    -48.803211f,  -19.574703f,  -24.464779f,  17.680639f,
        -14.256597f,   0.000000f,    0.000000f,    4.085333f,    14.195953f,   -1.150621f,
        31.111097f,    -3.663159f,   51.524723f,   -30.852291f,  136.482620f,  -496.277374f,
        169.752869f,   -131.509262f, 22.405785f,   -38.369030f,  -7.925778f,   -2.216400f,
        -19.093889f,   0.000000f,    0.000000f,    5.298405f,    20.020605f,   -15.493353f,
        58.907219f,    -14.012553f,  90.377693f,   -120.842834f, 176.383286f,  1035.980713f,
        -1051.734741f, -6.806986f,   -130.182205f, -46.728405f,  -44.157047f,  -55.446922f,
        -24.826288f,   0.000000f,    0.000000f,    58.511169f,   14.511680f,   194.594879f,
        -90.758675f,   290.515015f,  -158.022308f, 656.261597f,  -597.013184f, 2.385482f,
        0.085605f,     0.000000f,    0.000000f,    2.067800f,    6.051476f,    3.012396f,
        5.727601f,     2.530389f,    4.377070f,    1.394840f,    6.964653f,    0.000000f,
        0.000000f,     5.442059f,    -0.676395f,   -0.439612f,   4.395302f,    2.088063f,
        -0.455386f,    1.742284f,    -1.122583f,   3.395664f,    -3.057412f,   10.748663f,
        -2.697602f,    0.789050f,    4.307094f,    3.853422f,    16.404560f,   -1.302983f,
        -5.389112f,    0.238401f,    1.315699f,    -4.165914f,   -0.879250f,   6.426821f,
        -4.060582f,    -0.313470f,   10.352173f,   -3.881603f,   -3.961602f,   2.857405f,
        -9.634846f,    -4.533622f,   -11.905900f,  20.000000f,   10.000000f,   2.206979f,
        -3.896899f,    -7.921833f,   -51.248837f,  11.871791f,   -17.187860f,  12.854836f,
        5.601895f,     -54.191597f,  -31.532150f,  0.282048f,    -15.348310f,  0.733209f,
        12.609509f,    -47.596523f,  -38.778809f,  -22.125446f,  -5.591977f,   1.680509f,
        13.190703f,    -68.441536f,  -2.447833f,   -9.104825f,   8.012392f,    2.378443f,
        25.893475f,    -92.460632f,  -1.056770f,   -60.855129f,  25.649540f,   0.000000f,
        0.000000f,     -78.827812f,  -13.859468f,  -72.479958f,  -13.338516f,  -66.290390f,
        -11.046489f,   -46.086346f,  -25.139606f,  -14.950346f,  -39.315479f,  20.076031f,
        -51.129566f,   51.636730f,   -52.842426f,  146.968613f,  -96.692436f,  0.000000f,
        0.000000f,     114.999260f,  4.425962f,    131.892853f,  -2.772556f,   135.951294f,
        8.558196f,     131.579605f,  3.646377f,    129.444550f,  16.073416f,   0.000000f,
        0.000000f,     0.000000f,    0.000000f,    -54.734215f,  -0.308034f,   -171.936172f,
        -6.519996f,    -246.548691f, -18.485020f,  -337.581390f, -14.570363f,  -426.974030f,
        -17.450344f,   -11.580270f,  -0.179622f,   -2.884967f,   -88.451042f,  -0.561403f,
        -55.651997f,   -98.446411f,  2.097410f,    -37.840935f,  -13.712972f,  -18.255953f,
        20.243410f,    -7.659439f,   8.560320f,    -23.487850f,  40.767841f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kPassedPawnPstIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getTropismPieceTypeName(int idx) {
    if (idx == EvalParams::kDoubledPawnTropismIdx) {
        return "Doubled pawn";
    } else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
        return "Isolated pawn";
    } else if (idx == EvalParams::kPassedPawnTropismIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

void writeSquareTable(const EvalParams::SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(
        const EvalParams::PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)pieceSquareTables.size(); ++pieceIdx) {
        out << "\t" << getPstPieceTypeName(pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

void writeTropismTable(
        const std::array<TaperedTerm, EvalParams::kNumTropismPieceTypes>& tropism,
        std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumTropismPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n",
                getTropismPieceTypeName(pieceIdx),
                taperedTermToString(tropism[pieceIdx]));
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(taperedTermToString) | joinToString(", ");
    return "{" + inner + "}";
}

template <std::size_t N>
std::string arrayToString(const std::array<EvalCalcT, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(evalCalcTToString) | joinToString(", ");
    return "{" + inner + "}";
}

void writeTaperedTermPerPiece(
        const std::array<TaperedTerm, kNumPieceTypes>& terms, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n", pieceToString((Piece)pieceIdx), taperedTermToString(terms[pieceIdx]));
    }
}

}  // namespace

EvalParams EvalParams::getEmptyParams() {
    return EvalParams{};
}

EvalParams EvalParams::getDefaultParams() {
    return evalParamsFromArray(kDefaultParams);
}

EvalParamArray evalParamsToArray(const EvalParams& params) {
    EvalParamArray array;
    std::memcpy(array.data(), &params, sizeof(EvalParams));
    return array;
}

EvalParams evalParamsFromArray(const EvalParamArray& array) {
    EvalParams params = EvalParams::getEmptyParams();
    std::memcpy(&params, array.data(), sizeof(EvalParams));
    return params;
}

std::string evalParamsToString(const EvalParams& params) {
    std::ostringstream oss;

    oss << "phaseMaterialValues: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumPstPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                getPstPieceTypeName(pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << std::format(
            "\noppositeColoredBishopFactor:\n\t{}",
            arrayToString(params.oppositeColoredBishopFactor));

    oss << std::format("\nsingleMinorFactor:\n\t{}", evalCalcTToString(params.singleMinorFactor));
    oss << std::format("\ntwoKnightsFactor:\n\t{}", evalCalcTToString(params.twoKnightsFactor));
    oss << std::format("\nrookVsMinorFactor:\n\t{}", evalCalcTToString(params.rookVsMinorFactor));
    oss << std::format(
            "\nrookAndMinorVsRookFactor:\n\t{}",
            evalCalcTToString(params.rookAndMinorVsRookFactor));

    oss << std::format("\nhasUnstoppablePawn:\n\t{}", evalCalcTToString(params.hasUnstoppablePawn));

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTables: {\n";
    writePieceSquareTables(params.pieceSquareTables, oss);
    oss << "}\n";

    oss << std::format(
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\nbishopPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopPawnSameColorAdjustment));

    oss << std::format(
            "\nbishopEnemyPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopEnemyPawnSameColorAdjustment));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << std::format(
            "\nkingVirtualMobilityPenalty:\n\t{}",
            taperedTermToString(params.kingVirtualMobilityPenalty));

    oss << "\n\nmobilityBonus: {\n";
    writeTaperedTermPerPiece(params.mobilityBonus, oss);
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nnumKingAttackersAdjustment:\n\t{}",
            arrayToString(params.numKingAttackersAdjustment));

    oss << "\n\npiecePinnedAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.piecePinnedAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nkingOpenFileAdjustment:\n\t{}", taperedTermToString(params.kingOpenFileAdjustment));
    oss << std::format(
            "\nkingFlankOpenFileAdjustment:\n\t{}",
            taperedTermToString(params.kingFlankOpenFileAdjustment));
    oss << std::format(
            "\npassedPawnOutsideKingSquare:\n\t{}",
            taperedTermToString(params.passedPawnOutsideKingSquare));

    return oss.str();
}
