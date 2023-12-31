CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   G   N_CALIB       	STRING256         	N_HISTORY                     <   	DATA_TYPE                   comment       	Data type      
_FillValue                    0�   FORMAT_VERSION                 comment       File format version    
_FillValue                    0�   HANDBOOK_VERSION               comment       Data handbook version      
_FillValue                    0�   REFERENCE_DATE_TIME                comment       !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    1    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    1   PROJECT_NAME                  comment       Name of the project    
_FillValue                  @  1   PI_NAME                   comment       "Name of the principal investigator     
_FillValue                  @  1X   STATION_PARAMETERS                        	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  1�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       <0..N, 0 : launch cycle (if exists), 1 : first complete cycle   
_FillValue         ��        1�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    1�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    1�   DATE_CREATION                  comment       Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     1�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    2   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    2   INST_REFERENCE                    	long_name         Instrument type    conventions       Brand, type, serial number     
_FillValue                  @  2   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    2\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2`   JULD_QC                	long_name         Quality on Date and Time   conventions       Argo reference table 2     
_FillValue                    2h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             2t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             2|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    2�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    2�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    2�   PRES         	      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       2�   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  3�   PRES_ADJUSTED            	      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       4    PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  5   PRES_ADJUSTED_ERROR          	         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       5d   TEMP         	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       6�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  7�   TEMP_ADJUSTED            	      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       7�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9    TEMP_ADJUSTED_ERROR          	         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       9H   PSAL         	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       :d   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;�   PSAL_ADJUSTED            	      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ;�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <�   PSAL_ADJUSTED_ERROR          	         	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       =,   	PARAMETER            
                	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  >H   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    >x   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    Ax   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    Dx   CALIBRATION_DATE         
               
_FillValue                  ,  Gx   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    G�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    G�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    G�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    G�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  G�   HISTORY_DATE                     	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    G�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    H   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        H    HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    H$Argo profile    2.2 1.2 19500101000000  5900461 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               9A   AO  20060119101553  20070214145708  0682_27623_057                  2C  D   APEX_SBE_1234                                                   846 @���F�� 1   @��ډJ� @&�"�   �c��   1   ARGOS   A   B   B   @�  @�  A$��AfffA�33A�33A�ffBffBffB.��BE33BXffBnffB���B�33B���B���B���B���B�  B���B�ffBۙ�B�  B�33B���C33C� C33CffC#L�C+�C4�C>�CH33CQ��C]L�ChffCtffC��C�&fC�33C�ٚC�  C���C�� C�  C�� C���C�L�C�� C�  C�&fDy�D�3D  DY�D�D&FfD.ٚD7�3DAFfDK  DU�3D`��Dl  Dx3D�ffD�,�D�)�D���11111111111111111111111111111111111111111111111111111111111111111111111 @y��@���A#33Ad��A�ffA�ffA陙B  B  B.fgBD��BX  Bn  B�fgB�  B���B�fgB�fgB���B���Bř�B�33B�fgB���B�  B���C�CffC�CL�C#33C+  C4  C>  CH�CQ�3C]33ChL�CtL�C��C��C�&fC���C��3C�� C�s3C��3C�s3C�� C�@ C޳3C��3C��Ds4D��D��DS4D4D&@ D.�4D7��DA@ DK�DU��D`�4Dl�Dx�D�c3D�)�D�&gD���11111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�5?A�5?A�/A�/A�9XA�9XA�=qA�;dA�A�A�A�A�+A�(�A��mA��A�  A�;dA���A���A���A��9A���A�Q�A��+A��A`BAghsAU��ARM�AMAG��ADr�AB�9A@�A<1'A;XA8r�A4��A.�\A+hsA)/A'S�A%��A#K�A ��A�
A�+AK�A�Az�A��AS�A
(�AM�A��@��P@�&�@�R@�(�@��H@ʇ+@§�@�33@�l�@�33@���@�?}@���@��@�Ĝ@��P11111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�5?A�5?A�/A�/A�9XA�9XA�=qA�;dA�A�A�A�A�+A�(�A��mG�O�A�  A�;dA���A���A���A��9A���A�Q�A��+A��A`BAghsAU��ARM�AMAG��ADr�AB�9A@�A<1'A;XA8r�A4��A.�\A+hsA)/A'S�A%��A#K�A ��A�
A�+AK�A�Az�A��AS�A
(�AM�A��@��P@�&�@�R@�(�@��H@ʇ+@§�@�33@�l�@�33@���@�?}@���@��@�Ĝ@��P11111111111111411111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B �B �B��BgmB�JB��B�?Be`B5?B�B
�B
��B
D�B	�mB	��B	A�B	bB		7B	�B	�hB	�RB	�B
%B
K�B
�B
��B
�B
�!B
�B
�B
�9B
�-B
�B
��B
��B
��B
�oB
�DB
�B
x�B
k�B
`BB
R�B
G�B
?}B
33B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
+B
2-B
7LB
9X11111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B �B"�B�/G�O�B��B�mB��Bi�B8B3B
��B
�<B
L(B	��B	��B	G,B	�B	
�B	�B	�FB	��B	��B
+B
LB
��B
��B
�pB
��B
��B
�yB
��B
��B
��B
�^B
�	B
�"B
��B
��B
��B
yPB
k�B
`�B
S[B
HB
@B
4	B
 B
"B
�B
�B
�B
�B
�B
 B
#B
(-B
+HB
2qB
7sB
9p11111111111111411111111111111111111111111111111111111111111111111111111 ;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;�`B;�`BG�O�;�`B;�`B<t�<o;�`B;�`B<t�<#�
<#�
<o<D��<o;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Morison et al,1994,JAOT & effects of pressure adjustments                                                                                                                                                          PADJ REPORTED_SURFACE_PRESSURE =0.1 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to the correction                                                                                                                                                                                               Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal lag                                                                                                                                                                                                               20070123191829  AO  ARGQ                                                                        20060119101553  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20060119101553  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.0                                                                20060127112122  QC  PRES            @�  D���@                  PM  ARSQCTL V1.0                                                                20060127112122  QC  PSAL            @�  D���@                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20060707163024  CF  TEMP            B�  B�  ?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20060707163024  CF  PSAL            B�  B�  ?�                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20070124111511  IP                  G�O�G�O�G�O�                