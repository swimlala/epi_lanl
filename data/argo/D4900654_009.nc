CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-12-08T00:57:14Z creation;2012-10-19T06:14:58Z update;2015-06-07T03:18:12Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  ;\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  4900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA  20051208005714  20150615220511  A5_24187_009                    2C  D   APEX                            1142                            061703                          846 @��N��� 1   @��RB�\)@C�z�G��d	&�x��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA  A`  A�33A�  A�ffB	��BffB0ffBE��BZ  Bm33B�ffB���B�  B���B�  B���B�ffB���B���B�33B䙚B���B�  C�CL�C� CffCffC  CL�C$�C(�fC-�fC3  C833C=ffCB�CF��CQ�C[ffCe��Co� CyffC���C��fC���C�� C�� C���C���C�� C��3C�s3C��fC���C��fC�ffCǀ C̦fCѳ3C֌�C�� C�fC�3C�3CC�� C�� D��D�3D��D�fD�3D�D"Y�D(��D.��D5�D;` DA��DG�3DN3DTY�DZ��D`�3Dg  DmffDs��Dy�fD�#3D�p D���D�� D�)�D�p D��3D���D�0 D�\�D��fD���D��D�p Dڰ D�� D��D�` D�3D�p 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA  A`  A�33A�  A�ffB	��BffB0ffBE��BZ  Bm33B�ffB���B�  B���B�  B���B�ffB���B���B�33B䙚B���B�  C�CL�C� CffCffC  CL�C$�C(�fC-�fC3  C833C=ffCB�CF��CQ�C[ffCe��Co� CyffC���C��fC���C�� C�� C���C���C�� C��3C�s3C��fC���C��fC�ffCǀ C̦fCѳ3C֌�C�� C�fC�3C�3CC�� C�� D��D�3D��D�fD�3D�D"Y�D(��D.��D5�D;` DA��DG�3DN3DTY�DZ��D`�3Dg  DmffDs��Dy�fD�#3D�p D���D�� D�)�D�p D��3D���D�0 D�\�D��fD���D��D�p Dڰ D�� D��D�` D�3D�p 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AtM�AtA�AtE�AtM�AtQ�AtVAtQ�AtQ�AtI�AtZAt  Ar�/AH1'AA`BA=�A;x�A6ĜA5�A3`BA/�mA/�hA/x�A.�jA-�#A-x�A-G�A,�A,ZA,�RA,A�A+��A*�!A)��A(��A(�DA(~�A(bA&��A&JA%�A#33A ~�Az�A��A�A�`A�Al�AQ�A~�A�\A(�A&�A	�AbA(�A �`@��y@��R@�O�@��y@�l�@��@�o@�l�@�x�@ǝ�@ēu@��#@��#@��w@�@��;@�J@�1@��;@���@��h@��#@���@���@�~�@~��@z�@up�@p1'@k�m@f�y@b��@]�@Yx�@Q��@K@C�
@=?}@7�@1��@.@)��@#�@��@(�@  @��@x�@$�@C�@b@/@�H@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AtM�AtA�AtE�AtM�AtQ�AtVAtQ�AtQ�AtI�AtZAt  Ar�/AH1'AA`BA=�A;x�A6ĜA5�A3`BA/�mA/�hA/x�A.�jA-�#A-x�A-G�A,�A,ZA,�RA,A�A+��A*�!A)��A(��A(�DA(~�A(bA&��A&JA%�A#33A ~�Az�A��A�A�`A�Al�AQ�A~�A�\A(�A&�A	�AbA(�A �`@��y@��R@�O�@��y@�l�@��@�o@�l�@�x�@ǝ�@ēu@��#@��#@��w@�@��;@�J@�1@��;@���@��h@��#@���@���@�~�@~��@z�@up�@p1'@k�m@f�y@b��@]�@Yx�@Q��@K@C�
@=?}@7�@1��@.@)��@#�@��@(�@  @��@x�@$�@C�@b@/@�H@ �222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BB�;B�BB�;B�BB�;B�;B�BB�BB�;B�5BƨBbNBm�BffB[#B33B?}B33B�B49B8RB8RB6FB9XB<jB?}BB�BQ�B[#B[#B[#BW
B^5BiyBw�B�+B�hB�{B�oB�hB�JB�+B~�Bx�Bp�BgmBe`B`BBVBK�BA�B5?B,B%�B!�B�B\B	7BB��B��B�B�yB�NB�NB�ZB�fB�mB�B�B��B  BbB�B#�B.BD�BW
Bp�B�%B��B�RBȴB�B�yB��B	+B	{B	"�B	0!B	H�B	\)B	q�B	�B	��B	�B	�^B	ȴB	�B	�B	��B
B
uB
�B
+B
49B
?}B
H�B
O�B
W
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BB�;B�BB�;B�BB�;B�;B�BB�BB�;B�BB�)BffBo�BgmB]/B49B@�B5?B�B49B8RB8RB6FB9XB<jB?}BB�BQ�B[#B\)B\)BXB^5BiyBw�B�1B�oB��B�uB�oB�PB�1B� By�Bq�BgmBe`BaHBW
BL�BB�B6FB-B&�B"�B�BbB
=BB��B��B�B�B�TB�TB�ZB�fB�sB�B�B��B  BhB�B#�B.BD�BW
Bp�B�%B��B�RBȴB�B�yB��B	+B	{B	"�B	0!B	H�B	\)B	q�B	�B	��B	�B	�^B	ȴB	�B	�B	��B
B
uB
�B
+B
49B
?}B
H�B
O�B
W
222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
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
<�1<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200512202249222005122022492220051220224922201107090102322011070901023220110709010232200808290000002008082900000020080829000000  JA  ARFMfmtp2.2                                                                 20051208005714  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051208005715  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051208011942                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20051211065340  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051211065342  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051211070055                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20051220224922  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20051220224922  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127163122  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080829000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080908023654  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080908032845                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20121003105754  CV  JULD            G�O�G�O�F��y                JM  AREQREJM1.0                                                                 20121003105754  CF  PRES_ADJUSTED_QC@�ffD�p G�O�                JM  AREQREJM1.0                                                                 20121003105754  CF  TEMP_ADJUSTED_QC@�ffD�p G�O�                JM  AREQREJM1.0                                                                 20121003105754  CF  PSAL_ADJUSTED_QC@�ffD�p G�O�                JA  RFMTcnvd2.1                                                                 20121019061100  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20121019061458                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607031805                      G�O�G�O�G�O�                JA  ARDU                                                                        20150615220511                      G�O�G�O�G�O�                