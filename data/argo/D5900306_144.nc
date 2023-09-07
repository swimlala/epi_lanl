CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   I   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-01-14T10:50:46Z creation;2009-03-18T05:34:49Z update;2015-06-08T19:05:28Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   F<   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O<   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X<   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a<   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b    HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b0   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b4   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b8   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900306 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20070114105046  20150617140519  A4_14747_144                    2C  D   APEX                            702                             072602                          846 @�X
I��1   @�X
�F��@;)7KƧ��d��\(��1   ARGOS   A   B   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33A��A���A�ffB33BF  BlffB�33B���B�  Bƙ�B�  B���C��C�C  CL�C(��C333C=ffCG��C[� CoL�C��3C���C���C�� C�� C�� C���Cǳ3Cр C۳3C��C���C���D�fD��D� DٚD��D��D��D$��D)�3D.ٚD3��D8ٚD=��DBٚDG�3DN�DTY�DZ��D`� Dg  DmY�Ds� Dy��D�&fD�l�D�� D��fD�0 D�s3D�� D�s3D��3D�l�D��fD�\�D���D�C31111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���Ap  A���BffB533B[��B���B�33B���B�33Bљ�B�ffB���C�fC��C�C$��C/  C933CCffCWL�Ck�C33C�s3C�� C��fC��fC��fC�� Cř�C�ffCٙ�C�s3C��3C�� D ��D� D
�3D��D� D� D� D#� D(�fD-��D2� D7��D<� DA��DF�fDM�DSL�DY��D_�3Df3DlL�Dr�3Dx��D�� D��fD�)�D�` D���D���D�i�D���D�\�D��fD�` D��fD�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA���A���A��-A�(�A��
A���A��A�dZA�VA�;dA��FA���A�ƨA�9XA�bA��!A��A�XA�ƨA}�FAs\)Ai�-Ab$�AZ�jAN�AC��A@r�A7VA/oA)�7A"�A�A��AƨA��Aȴ@��@���@噚@�l�@ϥ�@�dZ@��@�t�@���@�G�@�-@�=q@�ff@�O�@���@�5?@�"�@���@�;@|z�@x  @q�@fff@]�h@R�\@N��@HĜ@B�H@7�;@.ff@"�H@��@��@�/?�1?�11111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA���A���A��-A�(�A��
A���A��A�dZA�VA�;dA��FA���A�ƨA�9XA�bA��!A��A�XA�ƨA}�FAs\)Ai�-Ab$�AZ�jAN�AC��A@r�A7VA/oA)�7A"�A�A��AƨA��Aȴ@��@���@噚@�l�@ϥ�@�dZ@��@�t�@���@�G�@�-@�=q@�ff@�O�@���@�5?@�"�@���@�;@|z�@x  @q�@fff@]�h@R�\@N��@HĜ@B�H@7�;@.ff@"�HG�O�@��@�/?�1?�11111111111111111111111111111111111111111111111111111111111111111111141111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;oB�BuBVBVBPBhBbBbBbBbBJB�B[#B��B>wB{B
��B
��B
�qB
��B
z�B
R�B
-B
B	�B	�oB	\)B	J�B	�B�B��B�XB��B�DBr�B]/BS�BE�B?}B8RB6FB9XB;dB@�BVBcTB�B��B�-B��B��B��B	\B	)�B	F�B	k�B	r�B	�B	�7B	��B	�RB	ȴB	��B	�TB	�B
o    B
:^B
[#B
[#B
e`B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111114111111   B�B�B\B\BVBoBhBhBhBhBVB�B`BB�#BB�B�B
��B
�B
��B
��B
}�B
VB
0!B
B	�B	��B	_;B	M�B	�B��B�
B�jB��B�VBu�B`BBW
BH�BB�B;dB9XB;dB>wBC�BXBffB�B��B�9BB�
B��B	hB	,B	H�B	m�B	t�B	�B	�DB	��B	�^B	��B	��B	�`B	��B
{G�O�B
<jG�O�B
]/B
gmB
r�B
r�1111111111111111111111111111111111111111111111111111111111111111114141111   <#�
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
G�O�<#�
G�O�<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 4.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200701271705472007012717054720070127170547200706232036422007062320364220070623203642200809300000002008093000000020080930000000  JA  ARFMfmtp2.3                                                                 20070114105046  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.6                                                                 20070114105047  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrqcp2.6                                                                 20070114105047  QCF$                G�O�G�O�G�O�           10200JA  ARUP                                                                        20070114110801                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20070118153855  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.6                                                                 20070118153855  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrqcp2.6                                                                 20070118153855  QCF$                G�O�G�O�G�O�           10200JA  ARUP                                                                        20070118155423                      G�O�G�O�G�O�                JA  ARGQrqcpt19b                                                                20070928080412  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070928080412  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20070928080412  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070928090506                      G�O�G�O�G�O�                JM  ARSQJMQC1.0                                                                 20070117171751  CF  TEMP            D�l�D�l�G�O�                JM  ARSQJMQC1.0                                                                 20070117171751  CF  PSAL            D�l�D�l�G�O�                JM  ARCAJMQC1.0                                                                 20070127170547  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070127170547  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070623203642  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080930000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20081008083915  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20081008100209                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114258  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318053119  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318053449                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608190518                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617140519                      G�O�G�O�G�O�                