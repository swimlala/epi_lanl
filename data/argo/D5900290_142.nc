CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   B   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2006-12-19T04:50:44Z creation;2015-04-24T02:21:50Z conversion to V3.1;2019-05-08T04:23:55Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       :�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  ;�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       <4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  =<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  >�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       >�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  ?�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  D  A    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       Ad   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       Bl   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       Ct   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  D|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   E   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   N   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   W   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  `   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    `�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    `�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    `�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    `�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  `�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    `�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    `�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    `�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         a    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         a   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        a   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    aArgo profile    3.1 1.2 19500101000000  20061219045044  20190630031516  5900290 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  A4_16476_142                    2C  D   APEX                            738                             072602                          846 @�Q��c�1   @�Q�̜A @($�/��dJ���m1   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   AQ��A���A�33B  BC��Bl  B�33B���B�ffB���B�  B�  CL�CL�C� C� C)�C3L�C=33CG  C[L�CoL�C�� C�� C�� C��fC�ffC��fC�� Cǌ�C���C۳3C噚C�fC�� D� D�fD�3D��D�3DٚD� D$�fD)�3D.� D3�3D8�3D=�fDBٚDG�3DN�DT9�DZ� D`�fDg3DmFfDs��Dy� D�)�D�i�D��3D��3D�0 D�c3D��3D�vf111111111111111111111111111111111111111111111111111111111111111111  @���A&ffA���A�B ffBH��Bs33B�  B���B�33B�ffB�ffB�  C� C�3C�3C L�C*� C4ffC>33CR� Cf� Cz�3C�Y�C��C�@ C�  C�@ C��C�&fC�ffC�L�C�33C�@ C��C��D�3D	� D�fD� D�fD��D"�3D'� D,��D1� D6� D;�3D@�fDE� DK�fDRfDXL�D^�3Dd� Dk3DqffDw��D� D�P D���D�ɚD�fD�I�D�ɚD�\�111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A���AվwAռjAղ-AΧ�A��RA���A���A�n�A�7LA��A��uAuoAd�9AM/A> �A6ĜA(�A�FA��AC�A
�AA��A ��@���@�hs@�O�@��@���@߅@ج@ҧ�@�n�@�G�@°!@�?}@�S�@��T@��!@�o@�/@�@���@�r�@���@���@��@�V@�dZ@�;d@�|�@�A�@�Q�@�G�@~��@r��@jJ@bM�@X�`@O�P@CS�@8b111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A���AվwAռjAղ-AΧ�A��RA���A���A�n�A�7LA��A��uAuoAd�9AM/A> �A6ĜA(�A�FA��AC�A
�AA��A ��@���@�hs@�O�@��@���@߅@ج@ҧ�@�n�@�G�@°!@�?}@�S�@��T@��!@�o@�/@�@���@�r�@���@���@��@�V@�dZ@�;d@�|�@�A�@�Q�@�G�@~��@r��@jJ@bM�@X�`@O�P@CS�@8b111111111111111111111111111111111111111111111111111111111111111111  B	!�B	!�B	"�B	"�B	"�B	#�B	#�B	�
B=qB�B�TB�B�%B#�B
49B	gmB	uBɺB�B��B�9B	hB	%�B	x�B	�^B	�HB
1B
DB
  B
JB
JB
uB
�B
hB
JB
JB
DB
	7B
B
+B
+B
bB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
%�B
'�B
.B
2-B
5?B
:^B
=qB
C�B
H�B
L�B
Q�B
W
B
[#B
e`B
o�111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	B	�9B7fB�B�5B��B� B"�B
3hB	c�B	�B�B�_B�'B�}B	�B	!B	qB	��B	�eB
UB
aB	�B
mB
gB
�B
�B

�B
gB
gB
aB
UB	�6B	�HB	�HB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
&2B
*KB
-]B
2|B
5�B
;�B
@�B
D�B
J	B
O(B
S@B
]~B
g�111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?DC<#�
<#�
<#�
<#�
<#�
<?�<?D<#�
<.�l<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by WJO; PSAL_ADJ_ERR=max(RecalS & CTM & WJO error , 0.01(PSS-78))                                                                                                                   SP=8.8(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0002), deepest deltaS=0.008(+-0.008)(PSS-78); Mapping scale = 8/4,4/2; Length of sliding calibration window is +-20 profiles;                                                                                                                      Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            WJO(Ver.2) salinity adjustment is adopted                                                                                                                                                                                                                       200701011750252007010117502520070101175025200701261515532007012615155320070126151553201709132230282017091322302820170913223028  JA  ARFMfmtp2.3                                                                 20061219045044  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061219045045  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20061219050642                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20061223033807  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20061223033807  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20061223033807  CV  TIME            G�O�G�O�F��<                JA  ARGQrelo2.1                                                                 20061223033807  CV  LAT$            G�O�G�O�A@                JA  ARGQrelo2.1                                                                 20061223033807  CV  LON$            G�O�G�O��"[�                JA  ARUP                                                                        20061223035349                      G�O�G�O�G�O�                JA  ARFMdecpA4_a                                                                20090331053632  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090331062013  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090331062014  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090331062014  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090331062014  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090331062015  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090331062015  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090331062015  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090331062015  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090331062015  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090331062902                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424022150                      G�O�G�O�G�O�                JA  ARUP                                                                        20150430200519                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20061222082019  CV  JULD            G�O�G�O�F��%                JM  ARCAJMQC2.0                                                                 20070101085025  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20070101085025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126061553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQWJO 2   SeHyD1.0                                                        20170913133028  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190630031516                      G�O�G�O�G�O�                