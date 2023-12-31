CDF      
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
_FillValue                    H$Argo profile    2.2 1.2 19500101000000  5900461 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               8A   AO  20060109102006  20070214145706  0682_27623_056                  2C  D   APEX_SBE_1234                                                   846 @��+��� 1   @��-׀ @'�@   �c�    1   ARGOS   A   A   A   @�  @�ffA&ffAi��A�  A�ffA�ffB
ffB  B133BE��BY33Bn��B�ffB���B���B���B���B�ffB�ffB���B���B�  B���B�ffB���CffC��C��C��C#L�C+L�C433C>� CH33CQ�fC\�3ChL�CtffC�&fC�33C�  C�  C�&fC�ٚC��fC��3C�Y�CɦfC��Cޙ�C�33C�  D�fD3D  DY�D�D&L�D.ٚD7��DAY�DJ��DU�fD`� Dl  Dx3D�ffD�  D��D�P 11111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A#33AffgA�ffA���A���B	��B33B0ffBD��BXffBn  B�  B�fgB�fgB�fgB�fgB�  B�  B�fgB�fgBٙ�B�fgB�  B�fgC33CfgCfgCfgC#�C+�C4  C>L�CH  CQ�3C\� Ch�Ct33C��C��C��fC��fC��C�� C���C�ٙC�@ CɌ�C��3Cހ C��C��fDy�DfD3DL�D�D&@ D.��D7� DAL�DJ��DUy�D`�3Dl3DxfD�` D��D�4D�I�11111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA�ĜA��HA��#A��
A��/A��A��yA��A�%A�&�A�1'A�hsA�r�AΩ�A̧�A��#A���A��A�M�A���A�Q�A�A�/A���A�A�Q�A���A�VAw33Aa��AQ
=AH1AD��A?��A>�9A:�A8�+A3
=A,�\A+/A* �A)+A&�`A$�!A"ZA JAȴA��Av�A��A{A�!A��Al�A�@��@�$�@�J@�=q@�@��H@��
@���@���@���@��u@��\@�^5@��u@�I�11111111111111111111111111111111111111111111111111111111111111111111111 A��TA�ĜA��HA��#A��
A��/A��A��yA��A�%A�&�A�1'A�hsA�r�AΩ�A̧�A��#A���A��A�M�A���A�Q�A�A�/A���A�A�Q�A���A�VAw33Aa��AQ
=AH1AD��A?��A>�9A:�A8�+A3
=A,�\A+/A* �A)+A&�`A$�!A"ZA JAȴA��Av�A��A{A�!A��Al�A�@��@�$�@�J@�=q@�@��H@��
@���@���@���@��u@��\@�^5@��u@�I�11111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B#�B&�B$�B$�B#�B'�B&�B&�B2-BB�BH�Bp�B�B
�B
v�B�+B��B��By�BiyB>wB�B
�B
�`B
��B
N�B	�B	��B	u�B	33B	DB	dZB	��B
1B
p�B
�{B
��B
�?B
��B
��B
�B
�3B
�3B
�!B
�B
��B
��B
��B
�%B
z�B
s�B
iyB
\)B
N�B
C�B
8RB
1'B
&�B
�B
�B
�B
�B
�B
�B
 �B
$�B
(�B
/B
49B
:^11111111111111111111111111111111111111111111111111111111111111111111111 B'B#�B&�B$�B$�B#�B'�B&�B&�B2(BB�BH�Bp�BڃB
�B
�RB��B�rB��B{vBl6BBXB!�B
�!B
�0B
ޜB
T=B	�*B	�wB	|B	88B	B	e@B	�@B
�B
q�B
�B
��B
��B
��B
�B
�TB
��B
��B
��B
��B
�sB
�B
�)B
��B
{NB
t1B
i�B
\�B
OTB
D-B
8�B
1�B
'}B
 IB
B
#B
 B
B
B
!B
%*B
)NB
/cB
4�B
:�11111111111111111111111111111111111111111111111111111111111111111111111 ;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;�`B;�`B<49X;�`B<o<o;�`B;�`B<o;�`B;�`B;�`B<49X<o;�`B<o<t�<o;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;ě�;�`B;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Morison et al,1994,JAOT & effects of pressure adjustments                                                                                                                                                          PADJ REPORTED_SURFACE_PRESSURE =0.2 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to the correction                                                                                                                                                                                               Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal lag                                                                                                                                                                                                               20070123191829  AO  ARGQ                                                                        20060109102006  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20060109102006  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.0                                                                20060127112121  QC  PRES            @�  D�P @                  PM  ARSQCTL V1.0                                                                20060127112121  QC  PSAL            @�  D�P @                  PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20070124111510  IP                  G�O�G�O�G�O�                