CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-27T00:35:43Z creation;2016-10-27T00:35:47Z conversion to V3.1;2019-12-19T08:26:54Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161027003543  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  I2_0576_051                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��s�K 1   @��t����@:���n�d�2a|�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D���D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s�
@��@�AA9AYAyA��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B&p�B.p�B6p�B>p�BFp�BNp�BVp�B^p�Bfp�Bnp�Bvp�B~p�B�k�B�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC�)C�)C�)C�)C	�)C�)C�)C��C��C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D g
D �
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D	g
D	�
D
g
D
�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D g
D �
D!g
D!�
D"g
D"�
D#g
D#�
D$g
D$�
D%g
D%�
D&g
D&�
D'g
D'�
D(g
D(�
D)g
D)�
D*g
D*�qD+g
D+�
D,g
D,�
D-g
D-�
D.g
D.�
D/g
D/�
D0g
D0�
D1g
D1�
D2g
D2�
D3g
D3�
D4g
D4�
D5g
D5�
D6g
D6�
D7g
D7�
D8g
D8�
D9g
D9�
D:g
D:�
D;g
D;�
D<g
D<�
D=g
D=�
D>g
D>�
D?g
D?�
D@g
D@�
DAg
DA�
DBg
DB�
DCg
DC�
DDg
DD�
DEg
DE�
DFg
DF�
DGg
DG�
DHg
DH�
DIg
DI�
DJg
DJ�
DKg
DK�
DLg
DL�
DMg
DM�
DNg
DN�
DOg
DO�
DPg
DP�
DQg
DQ�
DRg
DR�
DSg
DS�
DTg
DT�
DUg
DU�
DVg
DV�
DWg
DW�
DXg
DX�
DYg
DY�
DZg
DZ�
D[g
D[�
D\g
D\�
D]g
D]�
D^g
D^�
D_g
D_�
D`g
D`�
Dag
Da�
Dbg
Db�
Dcg
Dc�
Ddg
Dd�
Deg
De�
Dfg
Df�
Dgg
Dg�
Dhg
Dh�
Dig
Di�
Djg
Dj�
Dkg
Dk�
Dlg
Dl�
Dmg
Dm�
Dng
Dn�
Dog
Do�
Dpg
Dp�
Dqg
Dq�
Drg
Dr�
Dsg
Ds�
Dtg
Dt�
Dug
Du�
Dvg
Dv�
Dwg
Dw�qDxg
Dx�
Dyg
Dy�
Dzg
Dz�
D{g
D{�
D|g
D|�
D}g
D}�
D~g
D~�
Dg
D�
D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�v�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D³�D��D�3�D�s�Dó�D��D�3�D�s�Dĳ�D��D�3�D�s�Dų�D��D�3�D�s�DƳ�D��D�3�D�s�Dǳ�D��D�3�D�s�Dȳ�D��RD�0RD�s�Dɳ�D��D�3�D�s�Dʳ�D��D�3�D�s�D˳�D��D�3�D�s�D̳�D��D�3�D�s�Dͳ�D��D�3�D�s�Dγ�D��D�3�D�s�Dϳ�D��D�3�D�s�Dг�D��D�3�D�s�Dѳ�D��D�3�D�s�Dҳ�D��D�3�D�s�Dӳ�D��D�3�D�s�DԳ�D��D�3�D�s�Dճ�D��D�3�D�s�Dֳ�D��D�3�D�s�D׳�D��D�3�D�s�Dس�D��D�3�D�s�Dٳ�D��D�3�D�s�Dڳ�D��D�3�D�s�D۳�D��D�3�D�s�Dܳ�D��D�3�D�s�Dݳ�D��D�3�D�s�D޳�D��D�3�D�s�D߳�D��D�3�D�s�D೅D��D�3�D�s�D᳅D��D�3�D�s�DⳅD��D�3�D�s�D㳅D��D�3�D�s�D䳅D��D�3�D�s�D峅D��D�3�D�s�D泅D��D�3�D�s�D糅D��D�3�D�s�D賅D��D�3�D�s�D鳅D��D�3�D�s�D곅D��D�3�D�s�D볅D��D�3�D�s�D쳅D��D�3�D�pRD���D��D�3�D�s�DD��D�3�D�s�DﳅD��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�=D�]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A��A��/A��/A��;A��;A��HA��;A��;A��HA��A�?}A��HA�|�A���A˓uA���A�-A�A�/A�$�A���A�ffA��A��A���A�E�A���A�l�A��A�`BA���A�bA�n�A�oA���A��
A�G�A�M�A��yA�bA�r�A��uA�ĜA��7A��/A���A�oA�hsA�A���A��^A��\A�9XA��mA�x�A�VA��mA��#A��wA��\A�dZA�E�A��`A�(�A�`BA��\A�1A���A�Q�A�ȴA���A�I�A�$�A��A�$�A�~�A�I�A��
A��^A���A��A|�Az��Ay�-Ax��AxȴAx��AxZAx5?Ax�Aw�
Aw��Awt�Av��Au��At�/As"�Ap�9Ao7LAm�wAl(�Ai�Ag�wAfVAdĜAc��AcXAbA`ZA_�A^��A\{AY;dAW��AW�7AV1'AT�ASASG�AR�uAQt�AP�`AN�yAM`BAL$�AK�AJ�AJ~�AJA�AJ$�AI�AI�7AI�AHĜAHVAG�AG��AG\)AF�uAE�AD�HAC��AB�DAAG�A@��A@A�A?+A=��A=;dA<�A<I�A;��A:�A8��A7&�A69XA4�A3�A2��A2z�A2�A1oA/�A/hsA.�jA.5?A,��A,Q�A,1A+��A+��A+C�A*~�A)\)A(Q�A'��A'�A%�A#��A"�uA!�A!+A ��A bNAG�A��A�jA��AQ�A��AS�A�jAA�A�A`BAI�A��A�A��Ap�Al�AhsA\)A�AZA�^A%A��A+A��AI�AO�A��A$�A
v�A
�A	��A�RA��A1'A�A+A�A;dA�/AAG�A �uA ffA A�@�hs@�t�@�^5@��@���@�t�@�v�@� �@�$�@�/@��;@�"�@�o@�@�@�G�@�Q�@�
=@�33@�p�@�b@�X@އ+@�%@�  @���@�@��/@Ӿw@��@�E�@���@υ@���@ΰ!@�V@�-@�%@��@�\)@ʗ�@�@��/@�Z@�;d@���@�$�@�;d@�Ĝ@��R@��^@��j@�1@��@��@���@��@���@�A�@�l�@��H@�^5@�V@� �@��P@��@�@�ff@��-@��j@��@�Z@��m@�@�^5@���@��j@�1'@�+@�hs@��@��\@�hs@���@��/@��@�Q�@��@���@���@�M�@���@��D@�  @���@�V@��7@�G�@�%@���@�1'@��@�l�@�+@��!@�V@�{@���@��T@���@�G�@�V@�%@���@�r�@�A�@�bN@���@�+@�^5@�`B@��9@�1'@��@��@�S�@��!@�ff@�E�@��@�{@��@�p�@�/@��@� �@�ƨ@�|�@���@���@���@�5?@�@�p�@��@�z�@�9X@��@�ƨ@��@���@�\)@�v�@�v�@�ff@�M�@�{@��T@��@�7L@���@��D@�A�@�b@��@��@���@���@��+@�E�@���@�p�@���@�Ĝ@��u@�r�@�I�@��;@���@��@�\)@��@���@��\@�M�@�-@�J@���@��7@�?}@��@���@��/@���@���@��u@�z�@�Z@�Z@� �@�1@��@|�@;d@~�@~ff@}�@}?}@|��@|��@{��@{o@z�@z�H@zn�@z-@z�@zJ@y��@x�`@xb@w�;@w|�@v�y@v��@v5?@u�-@uV@tZ@t9X@s�m@s��@s��@sC�@so@s@r�H@r��@r-@q��@qx�@qx�@p�`@p�u@p�@p�@o\)@n��@n5?@n@m�@m��@m�-@mO�@mV@l��@l��@l��@l�/@l��@l�@l�@l�D@l�D@lz�@lZ@l(�@l1@l1@kS�@ko@ko@ko@j��@j�\@j�\@jn�@j-@i�@ihs@i7L@i7L@i&�@h�@h1'@h1'@h �@g�;@g��@g;d@g+@fȴ@f5?@f5?@f5?@fE�@f5?@e�h@d�@d��@c��@c�
@c�
@c�
@c�m@c�m@ct�@c33@b�H@b~�@b��@b�\@a��@a�7@a�@`�u@`bN@` �@`b@`  @_�;@_+@^ȴ@^�R@]�T@]�-@]O�@]/@]/@]?}@]/@\�j@\�D@\j@\1@Z-@Y7L@Y7L@Zn�@[C�@[@Z��@ZM�@Z-@Y��@Y��@YG�@Y7L@X��@XĜ@X�@XbN@XA�@Xb@W�;@W�w@W�w@W�@W|�@W;d@W
=@V�R@Vff@U�T@U�@U`B@T�@T��@T�j@T�@Tj@T(�@S��@S��@St�@SS�@SC�@R�@Rn�@R=q@Q��@Q�^@Q�@P�9@Pr�@P  @O�P@Ol�@OK�@O�@N��@Nȴ@N��@N��@Nv�@NE�@M�@M�@M?}@L�j@L1@KC�@J��@J^5@J�@I�#@I�^@I��@I7L@H�9@H�@H �@G�w@G;d@G�@G�@F��@F�@Fff@F@E��@E`B@D��@D�j@DZ@D�@C�@CS�@C33@B��@Bn�@A��@A��@AG�@A%@A%@A�@A%@@Ĝ@@�u@@r�@@A�@?�w@?�P@?\)@>�y@>v�@>$�@=�@=/@<�j@<z�@<1@;�m@;dZ@;o@:��@:��@9��@9��@9x�@8��@8��@8��@8�u@8bN@7�@7�P@7;d@6�y@6ȴ@6��@6ff@6$�@5��@5?}@4�@4�@4j@49X@3�m@3�F@3��@3��@3dZ@2�H@2~�@2�@1��@1�@1��@1&�@0��@0A�@0  @/��@/�w@/�@/�P@/�P@/l�@/;d@/
=@.�@.ȴ@.��@.�+@.ff@.V@.E�@.@-�T@-�@-/@-V@,�j@,�@,j@,1@+�
@+t�@+"�@+o@*�H@*��@*~�@*n�@*n�@*M�@)�#@)��@)G�@(��@(�`@(��@(�u@(bN@( �@'�w@'�P@'l�@'+@'
=@&��@&ff@%��@%�h@%O�@$�/@$I�@$�@#�m@#ƨ@#��@#�@#t�@#dZ@"�H@"^5@"�@!��@!�7@!x�@!G�@!�@ �`@ bN@ A�@ b@�w@�P@+@��@v�@{@�T@�-@p�@O�@V@(�@��@dZ@33@��@-@��@��@��@�7@G�@�@�@%@�9@bN@�@�w@�P@|�@K�@�@
=@�y@ff@$�@@@�@�@�@�@�@�j@��@z�@Z@(�@1@��@��@�m@ƨ@��@t�@C�@@�@�!@^5@�#@�7@7L@7L@%@��@Ĝ@�9@�u@r�@bN@Q�@A�@1'@ �@�;@�P@\)@\)@K�@�@��@�y@�@ȴ@ȴ@�R@��@�+@ff@5?@�@@�-@�h@p�@`B@`B@O�@`B@`B@O�@O�@/@��@��@��@z�@j@j@Z@(�@�@��@ƨ@��@t�@"�@o@
��@
�\@
~�@
M�@
�@	��@	�#@	��@	��@	��@	hs@	�@	%@�`@�9@�@1'@b@ �@��@�P@l�@\)@;d@�@�@
=@��@��@�@�R@��@��@�+@v�@ff@E�@�@��@�h@�@�@�@�@�@�@p�@O�@/@��@�/@�/@��@�j@��@�@Z@9X@�@�@1@�m@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A��A��/A��/A��;A��;A��HA��;A��;A��HA��A�?}A��HA�|�A���A˓uA���A�-A�A�/A�$�A���A�ffA��A��A���A�E�A���A�l�A��A�`BA���A�bA�n�A�oA���A��
A�G�A�M�A��yA�bA�r�A��uA�ĜA��7A��/A���A�oA�hsA�A���A��^A��\A�9XA��mA�x�A�VA��mA��#A��wA��\A�dZA�E�A��`A�(�A�`BA��\A�1A���A�Q�A�ȴA���A�I�A�$�A��A�$�A�~�A�I�A��
A��^A���A��A|�Az��Ay�-Ax��AxȴAx��AxZAx5?Ax�Aw�
Aw��Awt�Av��Au��At�/As"�Ap�9Ao7LAm�wAl(�Ai�Ag�wAfVAdĜAc��AcXAbA`ZA_�A^��A\{AY;dAW��AW�7AV1'AT�ASASG�AR�uAQt�AP�`AN�yAM`BAL$�AK�AJ�AJ~�AJA�AJ$�AI�AI�7AI�AHĜAHVAG�AG��AG\)AF�uAE�AD�HAC��AB�DAAG�A@��A@A�A?+A=��A=;dA<�A<I�A;��A:�A8��A7&�A69XA4�A3�A2��A2z�A2�A1oA/�A/hsA.�jA.5?A,��A,Q�A,1A+��A+��A+C�A*~�A)\)A(Q�A'��A'�A%�A#��A"�uA!�A!+A ��A bNAG�A��A�jA��AQ�A��AS�A�jAA�A�A`BAI�A��A�A��Ap�Al�AhsA\)A�AZA�^A%A��A+A��AI�AO�A��A$�A
v�A
�A	��A�RA��A1'A�A+A�A;dA�/AAG�A �uA ffA A�@�hs@�t�@�^5@��@���@�t�@�v�@� �@�$�@�/@��;@�"�@�o@�@�@�G�@�Q�@�
=@�33@�p�@�b@�X@އ+@�%@�  @���@�@��/@Ӿw@��@�E�@���@υ@���@ΰ!@�V@�-@�%@��@�\)@ʗ�@�@��/@�Z@�;d@���@�$�@�;d@�Ĝ@��R@��^@��j@�1@��@��@���@��@���@�A�@�l�@��H@�^5@�V@� �@��P@��@�@�ff@��-@��j@��@�Z@��m@�@�^5@���@��j@�1'@�+@�hs@��@��\@�hs@���@��/@��@�Q�@��@���@���@�M�@���@��D@�  @���@�V@��7@�G�@�%@���@�1'@��@�l�@�+@��!@�V@�{@���@��T@���@�G�@�V@�%@���@�r�@�A�@�bN@���@�+@�^5@�`B@��9@�1'@��@��@�S�@��!@�ff@�E�@��@�{@��@�p�@�/@��@� �@�ƨ@�|�@���@���@���@�5?@�@�p�@��@�z�@�9X@��@�ƨ@��@���@�\)@�v�@�v�@�ff@�M�@�{@��T@��@�7L@���@��D@�A�@�b@��@��@���@���@��+@�E�@���@�p�@���@�Ĝ@��u@�r�@�I�@��;@���@��@�\)@��@���@��\@�M�@�-@�J@���@��7@�?}@��@���@��/@���@���@��u@�z�@�Z@�Z@� �@�1@��@|�@;d@~�@~ff@}�@}?}@|��@|��@{��@{o@z�@z�H@zn�@z-@z�@zJ@y��@x�`@xb@w�;@w|�@v�y@v��@v5?@u�-@uV@tZ@t9X@s�m@s��@s��@sC�@so@s@r�H@r��@r-@q��@qx�@qx�@p�`@p�u@p�@p�@o\)@n��@n5?@n@m�@m��@m�-@mO�@mV@l��@l��@l��@l�/@l��@l�@l�@l�D@l�D@lz�@lZ@l(�@l1@l1@kS�@ko@ko@ko@j��@j�\@j�\@jn�@j-@i�@ihs@i7L@i7L@i&�@h�@h1'@h1'@h �@g�;@g��@g;d@g+@fȴ@f5?@f5?@f5?@fE�@f5?@e�h@d�@d��@c��@c�
@c�
@c�
@c�m@c�m@ct�@c33@b�H@b~�@b��@b�\@a��@a�7@a�@`�u@`bN@` �@`b@`  @_�;@_+@^ȴ@^�R@]�T@]�-@]O�@]/@]/@]?}@]/@\�j@\�D@\j@\1@Z-@Y7L@Y7L@Zn�@[C�@[@Z��@ZM�@Z-@Y��@Y��@YG�@Y7L@X��@XĜ@X�@XbN@XA�@Xb@W�;@W�w@W�w@W�@W|�@W;d@W
=@V�R@Vff@U�T@U�@U`B@T�@T��@T�j@T�@Tj@T(�@S��@S��@St�@SS�@SC�@R�@Rn�@R=q@Q��@Q�^@Q�@P�9@Pr�@P  @O�P@Ol�@OK�@O�@N��@Nȴ@N��@N��@Nv�@NE�@M�@M�@M?}@L�j@L1@KC�@J��@J^5@J�@I�#@I�^@I��@I7L@H�9@H�@H �@G�w@G;d@G�@G�@F��@F�@Fff@F@E��@E`B@D��@D�j@DZ@D�@C�@CS�@C33@B��@Bn�@A��@A��@AG�@A%@A%@A�@A%@@Ĝ@@�u@@r�@@A�@?�w@?�P@?\)@>�y@>v�@>$�@=�@=/@<�j@<z�@<1@;�m@;dZ@;o@:��@:��@9��@9��@9x�@8��@8��@8��@8�u@8bN@7�@7�P@7;d@6�y@6ȴ@6��@6ff@6$�@5��@5?}@4�@4�@4j@49X@3�m@3�F@3��@3��@3dZ@2�H@2~�@2�@1��@1�@1��@1&�@0��@0A�@0  @/��@/�w@/�@/�P@/�P@/l�@/;d@/
=@.�@.ȴ@.��@.�+@.ff@.V@.E�@.@-�T@-�@-/@-V@,�j@,�@,j@,1@+�
@+t�@+"�@+o@*�H@*��@*~�@*n�@*n�@*M�@)�#@)��@)G�@(��@(�`@(��@(�u@(bN@( �@'�w@'�P@'l�@'+@'
=@&��@&ff@%��@%�h@%O�@$�/@$I�@$�@#�m@#ƨ@#��@#�@#t�@#dZ@"�H@"^5@"�@!��@!�7@!x�@!G�@!�@ �`@ bN@ A�@ b@�w@�P@+@��@v�@{@�T@�-@p�@O�@V@(�@��@dZ@33@��@-@��@��@��@�7@G�@�@�@%@�9@bN@�@�w@�P@|�@K�@�@
=@�y@ff@$�@@@�@�@�@�@�@�j@��@z�@Z@(�@1@��@��@�m@ƨ@��@t�@C�@@�@�!@^5@�#@�7@7L@7L@%@��@Ĝ@�9@�u@r�@bN@Q�@A�@1'@ �@�;@�P@\)@\)@K�@�@��@�y@�@ȴ@ȴ@�R@��@�+@ff@5?@�@@�-@�h@p�@`B@`B@O�@`B@`B@O�@O�@/@��@��@��@z�@j@j@Z@(�@�@��@ƨ@��@t�@"�@o@
��@
�\@
~�@
M�@
�@	��@	�#@	��@	��@	��@	hs@	�@	%@�`@�9@�@1'@b@ �@��@�P@l�@\)@;d@�@�@
=@��@��@�@�R@��@��@�+@v�@ff@E�@�@��@�h@�@�@�@�@�@�@p�@O�@/@��@�/@�/@��@�j@��@�@Z@9X@�@�@1@�m@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B,B5?BA�B>wB(�B{B1B��B�B�mB�;B��BȴB�wB�!B��B��B�{B�oB�bB�PB�1B�%B�B|�Bn�BQ�B�B{BDBB�B�5BÖB�?B�B��B��B��B��B��B��B��B��B�bB�VB�PB�JB�7B�+B�B}�By�B]/BM�BE�BA�B:^B2-B.B(�B%�B"�B�B
��B
��B
ȴB
ŢB
ÖB
�dB
��B
�B
z�B
s�B
r�B
q�B
o�B
n�B
m�B
k�B
iyB
gmB
cTB
[#B
T�B
H�B
9XB
-B
 �B
uB
B	�B	�sB	�)B	�B	��B	��B	�wB	�dB	�-B	��B	�DB	~�B	�B	� B	v�B	n�B	jB	gmB	^5B	W
B	K�B	C�B	?}B	=qB	:^B	9XB	8RB	7LB	5?B	33B	0!B	.B	-B	+B	)�B	)�B	%�B	#�B	�B	�B	�B	bB	PB	JB	
=B	B	B	  B��B��B��B�B�`B�ZB�;B�/B�B��B��B��B��BȴBƨBÖB��B�jB�^B�XB�XB�RB�LB�-B�B��B��B��B��B��B��B�oB�bB�\B�DB�7B�7B�7B�1B�B�B�B� B|�Bw�Bt�Bq�Bo�Bk�BjBjBjBiyBjBgmBe`BcTB`BB]/B[#BYBXBP�BN�BH�BG�BH�BG�BD�B?}B>wB<jB9XB7LB5?B5?B2-B1'B/B.B.B+B)�B(�B&�B%�B$�B#�B!�B �B �B�B�B�B�B�B�B�B�B�B�BuBuBhBhBbB\B\B\B\BVBVBVB\B\B\B\BhBhBoBoBoBuBuB�B{BuB�B�B�B�B�B�B�B�B�B�B �B!�B%�B(�B)�B.B1'B49B49B49B5?B6FB8RB9XB;dB=qB@�BB�BF�BF�BG�BI�BN�BR�BT�BYBYBYBYBYBZBYBYBYBZB[#B[#B[#B_;BcTBe`BgmBjBm�Bn�Bp�Bq�Bt�Bv�Bw�Bw�Bx�By�B{�B}�B}�B}�B�B�+B�=B�PB�PB�PB�JB�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�?B�RB�^BBŢBǮB��B��B��B��B��B��B��B�B�/B�/B�5B�;B�HB�NB�`B�fB�sB�B�B�B�B��B��B��B��B��B	B	%B	
=B	JB	VB	bB	oB	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	&�B	(�B	+B	/B	1'B	2-B	49B	5?B	6FB	6FB	7LB	9XB	9XB	;dB	<jB	?}B	@�B	A�B	C�B	E�B	H�B	I�B	K�B	L�B	R�B	W
B	XB	XB	YB	ZB	[#B	[#B	\)B	^5B	`BB	aHB	cTB	ffB	gmB	hsB	iyB	k�B	m�B	m�B	o�B	o�B	p�B	q�B	q�B	r�B	s�B	t�B	w�B	z�B	{�B	}�B	~�B	�B	�B	�B	�B	�DB	�VB	�VB	�\B	�\B	�bB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�XB	�^B	�^B	�^B	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�TB	�ZB	�`B	�ZB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
\B
bB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
W
B
W
B
W
B
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�B!B%zB/OB8�BH�BF�B5%BqB"B�B�TB�B��B�,B�B��B�%B�vB��B��B��B��B��B��B��B��B��ButBXB!B�B<B�B�B�nB�%B�B��B��B�zB�NB�bB��B��B��B�mB��B��B��B��B��B��B��B��B}VB_VBO(BFtBB�B;�B2�B/ B)�B'RB%�B=B
�B
�:B
�lB
��B
�tB
�iB
��B
��B
{�B
tB
sB
r-B
pB
n�B
nB
l"B
j0B
h�B
d�B
\�B
WsB
KxB
;dB
/5B
# B
mB
�B	�B	�eB	ݘB	�$B	��B	͹B	� B	�<B	��B	�B	�B	�B	��B	��B	xB	o�B	k�B	h�B	_�B	Y�B	M�B	ESB	@�B	>(B	:�B	9�B	8�B	7�B	6B	3�B	0�B	.�B	-�B	+�B	*�B	+6B	'8B	%zB	!�B	#B	$B	NB	pB	�B	�B	%B	B	 �B�BB��B�fB��B�B�2B�BބB�B�BՁB�NB��B��B��B�9B�oB�"B��B��B�DB��B��B��B�cB�QB��B��B��B��B�SB�uB�NB��B�B��B��B�	B�7B��B�9B�aB�[B~�ByrBu�Br�Bq'BlBj�Bj�BkBjKBk�Bh�Bf�Bd�BabB^B\xBZ�BZ�BS&BP�BI�BH�BJ=BI�BFtB@�B?�B>(B:�B8RB6�B6zB3MB1�B0B0;B/�B,B*�B)�B(
B'B&�B%FB"�B!�B!|BB!BVB 'B�BOBB�B�B�BgB�B�B@B:B�BHBBHBvB\B�B�B�B�BHB:B B@B@BuBFB{B9B�B�BeBB�BkB]B)B]BIBOB�B!�B"�B&�B)�B+B/ B1�B4�B4�B4�B6B7B8�B9�B<B>]BAUBC�BG+BGzBH�BKDBPHBS�BU�BY�BYBY�BY�BY�BZ�BY�BY�BY�B[	B[�B[�B\CB_�Bc�Be�Bh
BkBnBo5Bq'BrGBu?Bw2Bx8BxRByXBz^B|PB~BB~]B~�B�oB�_B��B�"B�<B�<B�B��B��B�B��B�B��B��B�B��B�#B�IB�;B�hB��B��B��B��B��B��B��B�B�?B�KB�DB�<B�BB�NB�:B�[B�{BּB�dB�~BޞBߊB�B��B��B��B��B��B�B�B�GB�`B�>B�JB�jB�wB	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	B	/B	!-B	#:B	%,B	&LB	'RB	)_B	+kB	/iB	1vB	2|B	4�B	5�B	6�B	6�B	7�B	9�B	9�B	;�B	<�B	?�B	@�B	A�B	C�B	F%B	H�B	J#B	L0B	MjB	S[B	W?B	X_B	XyB	YeB	ZkB	[qB	[�B	\�B	^�B	`�B	a�B	c�B	f�B	g�B	h�B	i�B	k�B	m�B	m�B	o�B	o�B	p�B	q�B	q�B	r�B	tB	u%B	x8B	{0B	|6B	~]B	HB	�UB	�[B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	� B	� B	�&B	�,B	�LB	�>B	�B	�QB	�WB	�]B	�cB	�iB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	�%B	�+B	�	B	�)B	�B	��B	�B	�B	�(B	�HB	�.B	�.B	�.B	�B	�@B	�gB	ՁB	�mB	�sB	�_B	�_B	�KB	�QB	�kB	ڠB	�qB	�qB	��B	�dB	�~B	�~B	�dB	�dB	ބB	ߤB	��B	�B	��B	�,B	��B	�tB	�FB	�sB	� B	� B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�8B	�$B	�$B	�B	�*B	�B	�B	�0B	�6B	�PB	�VB	�BB	�.B	�cB
 OB
 4B
;B
UB
[B
[B
[B
aB
gB
gB
�B
�B
tB
tB
zB
�B
�B
	�B
	�B

�B

�B
�B
�B
�B
xB
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
B
	B
�B
B
�B
B
B
B
B
B
 'B
 'B
 B
!B
 �B
"B
!�B
!�B
"B
"B
"B
# B
# B
#:B
$&B
$&B
%FB
%FB
&2B
'RB
'8B
(XB
)DB
)_B
*KB
*KB
*KB
*KB
+QB
+kB
,WB
,WB
-wB
.cB
.cB
.cB
.cB
.}B
/�B
/iB
0oB
0oB
0oB
0oB
0oB
1vB
1�B
2|B
2|B
2|B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
6�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
EB
E�B
E�B
FB
GB
G�B
G�B
G�B
G�B
H�B
G�B
IB
IB
IB
J#B
J	B
KB
KB
J�B
J�B
K)B
L0B
LB
LB
LB
MB
M6B
M6B
N"B
N"B
O(B
O(B
O(B
O(B
O(B
O\B
QNB
Q4B
Q4B
RTB
RTB
S@B
S@B
T,B
TFB
TFB
TFB
T,B
TaB
TFB
TaB
UgB
VSB
VSB
VSB
VSB
VSB
VSB
VSB
WsB
WYB
W?B
X+B
W?B
WYB
WsB
XEB
X_B
YKB
YKB
YeB
ZkB
ZkB
ZkB
ZQB
ZQB
ZQB
ZkB
[qB
[qB
[WB
[WB
[qB
[qB
\xB
\�B
]~B
]dB
]dB
]~B
]~B
]~B
^jB
^�B
^�B
^jB
^jB
^jB
^jB
^�B
^�B
_pB
`�B
`vB
`�B
`�B
`�B
`\B
`vB
`vB
`vB
`vB
`�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
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
<?��<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.39(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610310033502016103100335020161031003350201806221215572018062212155720180622121557201804050408502018040504085020180405040850  JA  ARFMdecpA19c                                                                20161027093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161027003543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161027003544  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161027003545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161027003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161027003546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161027003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161027003546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161027003546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161027003547                      G�O�G�O�G�O�                JA  ARUP                                                                        20161027013009                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161027153748  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161030153350  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161030153350  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190850  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031557  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                