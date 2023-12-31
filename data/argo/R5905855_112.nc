CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:30:47Z creation;2022-06-04T19:30:48Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193047  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @پ��Eg�1   @پ�[��@-���`A��c�+J1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BA33BH��BNffBW��B`  Bh  Bp  Bx  B�  B���B���B���B�  B�  B���B�  B�  B�33B�ffB�33B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C�C33C�fC  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@w
=@�Q�@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B@��BH=qBM�
BW
>B_p�Bgp�Bop�Bwp�Bp�B�Q�B��B��B��RB��RB��B��RB��RB��B��B��B��RB��B��B��B��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB��B�B�B�B�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)CC�)C�)C�)C��C\CC�)C�)C!�)C#�)C%�)C'�)C)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC��CE��CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C���C��D w
D �
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D	w
D	�
D
w
D
�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
Dw
D�
D w
D �
D!w
D!�
D"w
D"�
D#w
D#�
D$w
D$�
D%w
D%�
D&w
D&�
D'w
D'�
D(w
D(�
D)w
D)�
D*w
D*�
D+w
D+�
D,w
D,�
D-w
D-�
D.w
D.�
D/w
D/�
D0w
D0�
D1w
D1�
D2w
D2�
D3w
D3�
D4w
D4�
D5w
D5�
D6w
D6�
D7w
D7�
D8w
D8�
D9w
D9�
D:w
D:�
D;w
D;�
D<w
D<�
D=w
D=�
D>w
D>�
D?w
D?�
D@w
D@�
DAw
DA�
DBw
DB�
DCw
DC�
DDw
DD�
DEw
DE�
DFw
DF�
DGw
DG�
DHw
DH�
DIw
DI�
DJw
DJ�
DKw
DK�
DLw
DL�
DMw
DM�
DNw
DN�
DOw
DO�
DPw
DP�
DQw
DQ�
DRw
DR�
DSw
DS�
DTw
DT�
DUw
DU�
DVw
DV�
DWw
DW�
DXw
DX�
DYw
DY�
DZw
DZ�
D[w
D[�
D\w
D\�
D]w
D]�
D^w
D^�
D_w
D_�
D`w
D`�
Daw
Da�
Dbw
Db�
Dcw
Dc�
Ddw
Dd�
Dew
De�
Dfw
Df�
Dgw
Dg�
Dhw
Dh�
Diw
Di�
Djw
Dj�
Dkw
Dk�
Dlw
Dl�
Dmw
Dm�
Dnw
Dn�
Dow
Do�
Dpw
Dp�
Dqw
Dq�
Drw
Dr�
Dsw
Ds�
Dtw
Dt�
Duw
Du�
Dvw
Dv�
Dww
Dw�
Dxw
Dx�pDyw
Dy�
Dzw
Dz�
D{w
D{�
D|w
D|�
D}w
D}�
D~w
D~�
Dw
D�
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D¾�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�>�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D�޸111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϵtAϸ�AϸRAϮ}AϭCAϭCAϯOAϰ!Aϰ�Aϱ'Aϲ�Aϱ�Aϲ�Aϳ�Aϴ9AϵAϵ�AϷ�AϸAϵtAϴ�AϣAϓAϋ�A�u�A�Z�A�A��|A�S�A�A�r|A��A���A�~Aƅ�A��AĘ�A�B�A�bA�oA���A�A�q�A�~A�&�A���A���A��A�8RA���A���A���A�A�A��EA���A�X�A���A��A�K)A�kA�|�A��{A�&�A��!A�@�A�OA��A�7A���A�u�A�ZA�A���A��%A�A�A�CaA��WA���A��rA�1[A��]A}��Ay�hAt��AqϫAg	Ac�\Aa�jAZ��AXy>AS��AN�xALm�AI��AFl"AE�AC�A@��A?S&A>��A>x�A=�jA=C�A<�#A<
=A:�9A9�A6A3l�A1 iA.�)A,��A+|�A*� A*j�A* iA)��A)�jA(hsA%
=A#/�A"rGA!��A!bA �oA �A ��A �HA ��AJ#A4A͟A�A��AںA��A�cA��Av�A�[AjA�_A�Ap�A��A��A7�A��A�XA�A��A��A�AoAsA2aA5�A�A��Ah
A�AN�A��A%FAA �A�A��A��A�A��AĜA��A6AoiA�hA��A+kArGA^5A�A��Aw2A�AA�A�tA&�A�A4A��A�nA�A�{A�A �A
�A
9XA	ϫA	�VA	t�A	JA�gA�?Ao�A-�A�A��A��A]�A�^A�oA��A��A�kA��A�4Ah
A;dA�A�<A>�A�A�cA�A4nA��A��A��A��A'�A!�A-A��AG�AbA ��A �)A ݘA ��A �~A j�A XA �A �@���@���@�2�@�ں@�-�@�	l@�r�@��j@�RT@��@�=q@�G@�C-@�n/@���@�*�@��@�a�@��@�n�@�p;@��K@��@��A@�/�@�@��@�;@�@@�w�@�Z�@�s�@�@�X@�,�@Ⴊ@�$�@�RT@��K@ޮ}@�E�@���@�s@٠'@�c�@�\�@���@�~�@؄�@ؖ�@��@ْ:@٧�@٩�@٘�@�s�@�e�@�`B@�a@�X�@�0�@��'@�c @��}@�T�@և+@՞�@ԙ1@Ӹ�@�8@��@ҟ�@�B�@��p@���@���@�1'@�ԕ@�{J@��M@̀�@�h�@�B[@˶F@��@�s�@�@ɦ�@�(�@��f@Ȥ�@��
@��@�Q@Ť@@�L�@Ŀ�@��@�5�@���@�@�PH@�-�@��@���@��@�n/@��<@�6@�4@�ԕ@�rG@��@��@�,=@��+@���@�t�@�+�@��_@�ԕ@��@��@�W�@��@�?}@���@�V@��r@��0@��M@�S�@��@���@�?�@��}@���@�+�@�~(@��N@�w2@�8�@�!-@��@��H@�h�@���@��
@���@�7L@��@��u@��@���@���@��q@���@��k@��@�s�@�^�@�7L@�ȴ@�G@���@�zx@�<6@��@��_@�{@�˒@�t�@��@�e@���@��n@�E9@�7L@�-w@��8@���@��K@���@�g8@�+@�2�@��)@���@�C@��`@���@�s�@�\�@��@�J#@���@�b�@���@��!@�n�@�خ@��@�zx@�F�@��@���@��<@���@�q�@�g8@��@��@@��@�S�@��@��r@�g8@�J@���@�e,@�?}@�C@���@���@��@���@���@�\�@�!�@��@��@�?�@��@��a@�t�@�S@��X@���@�c @��@���@�k�@�%F@�;@���@��@�l�@�:�@�%�@���@��@���@�e,@��@��}@�$�@���@�g�@�F�@���@��_@�`�@�C�@�
�@��@��K@�y�@�^�@�'�@�	l@��@���@��]@�q�@�e@� �@�
�@��9@���@��q@��S@�u�@�K�@�҉@��D@��o@�!@���@�s@��@��_@��@�2�@��d@�v`@�Dg@�7L@��@���@�Ov@�J@��d@�c�@�E9@��@��v@��'@��O@��@�u�@�g8@�5?@�	@��K@�x@�B�@�@��<@���@�xl@�ff@��@���@��@���@���@��/@���@�h
@�!�@��g@�hs@�F�@��@��@��z@���@�q@�_�@�:�@�!@��@�k@~�H@~h
@}��@}�@|�@|�9@|4n@{�6@{�:@{qv@z��@zC�@z�@y��@y��@y�~@x��@w�6@w��@w�P@wU�@w'�@v��@v��@v��@v�@vd�@v-@u��@up�@u7L@t�?@t��@s�w@sRT@so@r��@rB[@q��@p��@p�.@pXy@p-�@o�@o;d@o�@n�M@n�@n_�@m�@mf�@m�@l��@l��@l��@lD�@l@k�&@k�@kA�@j͟@jH�@i�@i��@i8�@i#�@h�P@h�E@h��@hm�@g�P@f0U@ezx@e<6@e�@d�/@d��@dm�@de�@dPH@d~@c��@c;d@b��@bc @a�@a��@a�M@aDg@a�@`�@`��@`��@`�Y@`j@`,=@_ƨ@_K�@_O@^��@^H�@^$�@^@]rG@\�|@\��@\�I@\u�@\7�@[�g@[]�@Zȴ@Z)�@Yzx@Y5�@YV@X�Y@X�@Wخ@W��@V��@V{�@V�@U�3@U�"@U7L@T��@T�@S�P@S�@R� @R~�@RV@R	@Q��@QS&@P�`@P�[@Pl"@O�m@O��@O��@O6z@N��@N&�@M�j@M�h@ML�@L��@LH@L~@K�@K��@Kl�@K$t@KS@J��@J�,@J��@I��@I-w@Hm�@HXy@H �@GRT@F��@F�,@F�s@F�!@F{@E��@Ek�@E�@D��@DN�@Cخ@C��@Cn/@C@O@B�s@B�x@B�@A8�@@��@@l"@?��@?E9@?�@>�R@>\�@=��@=(�@<��@<M@;�r@;��@;o�@;@O@;Y@:��@:��@:z@:Ov@:u@9��@9V@8�U@84n@7�Q@7�@7��@7��@7iD@7Mj@71�@7S@6�y@6ȴ@6�!@6��@6q�@6V@6&�@5�H@5c�@5`B@5/@4��@4��@4��@4�@4�@3��@3A�@3@O@3)_@2�8@2�6@2��@1�.@1�S@14@0�@0�I@/��@/�@/t�@/e�@/K�@/$t@.��@.��@.�@.xl@.n�@.p;@.kQ@.?@-��@-:�@,�z@,Xy@,�@+�F@+]�@+J#@++@+(@*��@*1�@)�@)�~@)8�@) \@(��@(��@(r�@(,=@'��@'�:@'H�@'+@&�@&i�@&8�@&�@%�d@%m]@%&�@%�@%�@$�5@$y>@$�@#�@#��@#�$@#o�@#33@"�"@"�h@"��@"q�@"#:@!�o@!��@!��@!s�@![W@! \@ �@ ��@ ��@ H@��@�P@A�@@�H@�@ߤ@�m@�1@z@Ta@=q@4@��@x�@Dg@�@�	@�K@��@��@�I@�@h�@N�@@�Q@�K@�@�[@��@�@�@�<@��@E�@	@�@��@�7@k�@0�@��@��@��@oi@[�@�@��@�@@/�@ȴ@��@��@�+@l�@^5@3�@�t@u�@O�@(�@�E@��@oi@M@�]@��@�,@Z�@�@x�@!�@�P@�	@��@w�@e�@?�@	�@��@|�@X�@H�@'�@��@�@v�@M�@?@�@�@��@F@:�@5�@/@�@��@A�@@��@��@��@��@�k@|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϵtAϸ�AϸRAϮ}AϭCAϭCAϯOAϰ!Aϰ�Aϱ'Aϲ�Aϱ�Aϲ�Aϳ�Aϴ9AϵAϵ�AϷ�AϸAϵtAϴ�AϣAϓAϋ�A�u�A�Z�A�A��|A�S�A�A�r|A��A���A�~Aƅ�A��AĘ�A�B�A�bA�oA���A�A�q�A�~A�&�A���A���A��A�8RA���A���A���A�A�A��EA���A�X�A���A��A�K)A�kA�|�A��{A�&�A��!A�@�A�OA��A�7A���A�u�A�ZA�A���A��%A�A�A�CaA��WA���A��rA�1[A��]A}��Ay�hAt��AqϫAg	Ac�\Aa�jAZ��AXy>AS��AN�xALm�AI��AFl"AE�AC�A@��A?S&A>��A>x�A=�jA=C�A<�#A<
=A:�9A9�A6A3l�A1 iA.�)A,��A+|�A*� A*j�A* iA)��A)�jA(hsA%
=A#/�A"rGA!��A!bA �oA �A ��A �HA ��AJ#A4A͟A�A��AںA��A�cA��Av�A�[AjA�_A�Ap�A��A��A7�A��A�XA�A��A��A�AoAsA2aA5�A�A��Ah
A�AN�A��A%FAA �A�A��A��A�A��AĜA��A6AoiA�hA��A+kArGA^5A�A��Aw2A�AA�A�tA&�A�A4A��A�nA�A�{A�A �A
�A
9XA	ϫA	�VA	t�A	JA�gA�?Ao�A-�A�A��A��A]�A�^A�oA��A��A�kA��A�4Ah
A;dA�A�<A>�A�A�cA�A4nA��A��A��A��A'�A!�A-A��AG�AbA ��A �)A ݘA ��A �~A j�A XA �A �@���@���@�2�@�ں@�-�@�	l@�r�@��j@�RT@��@�=q@�G@�C-@�n/@���@�*�@��@�a�@��@�n�@�p;@��K@��@��A@�/�@�@��@�;@�@@�w�@�Z�@�s�@�@�X@�,�@Ⴊ@�$�@�RT@��K@ޮ}@�E�@���@�s@٠'@�c�@�\�@���@�~�@؄�@ؖ�@��@ْ:@٧�@٩�@٘�@�s�@�e�@�`B@�a@�X�@�0�@��'@�c @��}@�T�@և+@՞�@ԙ1@Ӹ�@�8@��@ҟ�@�B�@��p@���@���@�1'@�ԕ@�{J@��M@̀�@�h�@�B[@˶F@��@�s�@�@ɦ�@�(�@��f@Ȥ�@��
@��@�Q@Ť@@�L�@Ŀ�@��@�5�@���@�@�PH@�-�@��@���@��@�n/@��<@�6@�4@�ԕ@�rG@��@��@�,=@��+@���@�t�@�+�@��_@�ԕ@��@��@�W�@��@�?}@���@�V@��r@��0@��M@�S�@��@���@�?�@��}@���@�+�@�~(@��N@�w2@�8�@�!-@��@��H@�h�@���@��
@���@�7L@��@��u@��@���@���@��q@���@��k@��@�s�@�^�@�7L@�ȴ@�G@���@�zx@�<6@��@��_@�{@�˒@�t�@��@�e@���@��n@�E9@�7L@�-w@��8@���@��K@���@�g8@�+@�2�@��)@���@�C@��`@���@�s�@�\�@��@�J#@���@�b�@���@��!@�n�@�خ@��@�zx@�F�@��@���@��<@���@�q�@�g8@��@��@@��@�S�@��@��r@�g8@�J@���@�e,@�?}@�C@���@���@��@���@���@�\�@�!�@��@��@�?�@��@��a@�t�@�S@��X@���@�c @��@���@�k�@�%F@�;@���@��@�l�@�:�@�%�@���@��@���@�e,@��@��}@�$�@���@�g�@�F�@���@��_@�`�@�C�@�
�@��@��K@�y�@�^�@�'�@�	l@��@���@��]@�q�@�e@� �@�
�@��9@���@��q@��S@�u�@�K�@�҉@��D@��o@�!@���@�s@��@��_@��@�2�@��d@�v`@�Dg@�7L@��@���@�Ov@�J@��d@�c�@�E9@��@��v@��'@��O@��@�u�@�g8@�5?@�	@��K@�x@�B�@�@��<@���@�xl@�ff@��@���@��@���@���@��/@���@�h
@�!�@��g@�hs@�F�@��@��@��z@���@�q@�_�@�:�@�!@��@�k@~�H@~h
@}��@}�@|�@|�9@|4n@{�6@{�:@{qv@z��@zC�@z�@y��@y��@y�~@x��@w�6@w��@w�P@wU�@w'�@v��@v��@v��@v�@vd�@v-@u��@up�@u7L@t�?@t��@s�w@sRT@so@r��@rB[@q��@p��@p�.@pXy@p-�@o�@o;d@o�@n�M@n�@n_�@m�@mf�@m�@l��@l��@l��@lD�@l@k�&@k�@kA�@j͟@jH�@i�@i��@i8�@i#�@h�P@h�E@h��@hm�@g�P@f0U@ezx@e<6@e�@d�/@d��@dm�@de�@dPH@d~@c��@c;d@b��@bc @a�@a��@a�M@aDg@a�@`�@`��@`��@`�Y@`j@`,=@_ƨ@_K�@_O@^��@^H�@^$�@^@]rG@\�|@\��@\�I@\u�@\7�@[�g@[]�@Zȴ@Z)�@Yzx@Y5�@YV@X�Y@X�@Wخ@W��@V��@V{�@V�@U�3@U�"@U7L@T��@T�@S�P@S�@R� @R~�@RV@R	@Q��@QS&@P�`@P�[@Pl"@O�m@O��@O��@O6z@N��@N&�@M�j@M�h@ML�@L��@LH@L~@K�@K��@Kl�@K$t@KS@J��@J�,@J��@I��@I-w@Hm�@HXy@H �@GRT@F��@F�,@F�s@F�!@F{@E��@Ek�@E�@D��@DN�@Cخ@C��@Cn/@C@O@B�s@B�x@B�@A8�@@��@@l"@?��@?E9@?�@>�R@>\�@=��@=(�@<��@<M@;�r@;��@;o�@;@O@;Y@:��@:��@:z@:Ov@:u@9��@9V@8�U@84n@7�Q@7�@7��@7��@7iD@7Mj@71�@7S@6�y@6ȴ@6�!@6��@6q�@6V@6&�@5�H@5c�@5`B@5/@4��@4��@4��@4�@4�@3��@3A�@3@O@3)_@2�8@2�6@2��@1�.@1�S@14@0�@0�I@/��@/�@/t�@/e�@/K�@/$t@.��@.��@.�@.xl@.n�@.p;@.kQ@.?@-��@-:�@,�z@,Xy@,�@+�F@+]�@+J#@++@+(@*��@*1�@)�@)�~@)8�@) \@(��@(��@(r�@(,=@'��@'�:@'H�@'+@&�@&i�@&8�@&�@%�d@%m]@%&�@%�@%�@$�5@$y>@$�@#�@#��@#�$@#o�@#33@"�"@"�h@"��@"q�@"#:@!�o@!��@!��@!s�@![W@! \@ �@ ��@ ��@ H@��@�P@A�@@�H@�@ߤ@�m@�1@z@Ta@=q@4@��@x�@Dg@�@�	@�K@��@��@�I@�@h�@N�@@�Q@�K@�@�[@��@�@�@�<@��@E�@	@�@��@�7@k�@0�@��@��@��@oi@[�@�@��@�@@/�@ȴ@��@��@�+@l�@^5@3�@�t@u�@O�@(�@�E@��@oi@M@�]@��@�,@Z�@�@x�@!�@�P@�	@��@w�@e�@?�@	�@��@|�@X�@H�@'�@��@�@v�@M�@?@�@�@��@F@:�@5�@/@�@��@A�@@��@��@��@��@�k@|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�+B	�zB	�tB	�ZB	�ZB	��B	�tB	�tB	�tB	�tB	�%B	�?B	�%B	�%B	�%B	�B	�?B	�%B	��B	�+B	�UB	��B	�&B	׍B	��B
?.B
�B
�B
�/B
��B
��Bb4B��BȚB�B�?B�!B�B�OB��B�3B�9B�3B�`B�PB��B�B9B�BhB9B�B<BJB�B�B�B�BGB�B�B��B��B��B��B��B��B�JBtTB'8B
�B
�uB
��B
p�B
D�B
;0B
88B
4TB
-�B
�B
�B	�9B	�yB	�B	��B	��B	x�B	b�B	O(B	>BB	+6B	 �B	WB	�B	
�B	�B	�B	�B	'B	-B	�B	�B	EB	B	�B	�B	�B	4B	�B	�B	<B	�B	�B	�B	7B	!�B	&LB	'�B	5B	�B	�B	�B	#B	#TB	*�B	0B	;0B	A�B	K^B	OBB	O\B	RB	S�B	S�B	T�B	O\B	KxB	\CB	`'B	^�B	b�B	raB	�FB	ϫB	յB	��B	�B	��B	�B	�B	�B	��B	�B	�mB	��B	�B	�B	�RB	�^B	�JB	��B	��B	�aB	�mB	��B	��B	��B	�aB	�ZB	��B	�B	�B	��B	�EB	�B	�B	�wB
3B
�B
�B
"NB
"B
 vB
%,B
(XB
'B
%�B
'mB
'RB
&�B
&�B
%�B
$�B
&fB
$�B
$�B
$@B
&�B
&�B
&2B
%FB
(�B
(sB
'RB
&�B
'mB
(>B
&�B
$�B
!�B
!HB
!bB
"B
&�B
(�B
+B
*�B
*0B
)_B
'RB
)B
(�B
(
B
&�B
$�B
&LB
(�B
(�B
(
B
(sB
)�B
)DB
(>B
'�B
'RB
'mB
(
B
(�B
(�B
)B
)B
)B
(�B
(sB
($B
'8B
%�B
&B
&fB
$�B
#�B
# B
$&B
#B
#�B
"�B
 BB
 'B
!�B
!�B
 �B
!B
�B
�B
IB
#B
kB
1B
�B
�B
�B
�B
�B
.B

�B
tB
%B
tB
 �B	��B	��B	�*B	�*B	��B	�ZB	�[B	�oB	��B	��B	�[B	�B	�B	�2B	��B
'B
B
%B
�B
�B
	lB

	B

=B

�B

�B

rB
	�B
	�B
	�B
	lB
�B
1B
KB
KB
KB
�B
xB

�B
�B

�B

�B

�B

rB

rB

=B

	B

#B

XB

XB

�B

XB

rB

rB

#B

#B

XB

#B

�B

=B

rB
	�B

�B

=B

#B

	B

	B

	B
	�B

#B
	�B
	�B

�B
�B
�B
�B
�B
^B
^B
)B
B

�B

�B

�B

#B

XB

=B

�B

#B

rB

�B

�B
^B
�B
�B
B
B
�B
�B
PB
jB
B
B
B
~B
�B
VB
�B
�B
(B
�B
vB
BB
�B
�B
BB
BB
�B
HB
bB
}B
bB
}B
}B
}B
}B
�B
�B
�B
TB
�B
TB
�B
�B
�B
:B
TB
�B
�B
�B
�B
@B
[B
[B
�B
�B
�B
�B
[B
2B
gB
gB
�B
B
9B
�B
�B
SB
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
)B
B
)B
�B
)B
�B
�B
�B
�B
�B
B
�B
~B
�B
jB
�B
�B
�B
�B
B
B
B
B
B
!B
�B
�B
 �B
�B
 B
�B
 vB
!�B
!�B
"4B
"hB
"�B
"�B
"�B
"�B
# B
#�B
#�B
#�B
$B
$�B
$�B
%`B
%�B
%zB
$�B
#�B
$&B
$�B
$@B
#:B
# B
#B
"�B
#B
# B
#nB
#�B
#�B
#�B
$B
$�B
%`B
&B
%�B
%�B
&LB
'�B
(XB
(�B
(�B
)B
)�B
)�B
*eB
*KB
*KB
*�B
*�B
)�B
)yB
(sB
(�B
)yB
)�B
)�B
*B
)�B
*KB
*eB
+B
+B
+�B
,=B
,WB
-]B
.}B
.cB
.}B
/�B
/�B
/�B
0oB
0oB
0�B
0!B
1B
1AB
2B
2�B
2�B
2�B
33B
3�B
4�B
4�B
4nB
4TB
4�B
5%B
5ZB
5�B
6+B
6FB
6`B
6�B
72B
72B
7LB
7fB
7�B
8B
88B
88B
8�B
8lB
8�B
9�B
9�B
9�B
:DB
:xB
:xB
:^B
;JB
;0B
;JB
;dB
;dB
;dB
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=B
="B
="B
=qB
=qB
=qB
=�B
=qB
=�B
=�B
=�B
>(B
>BB
>wB
?HB
?�B
@B
@B
@B
@B
@B
@OB
@B
@iB
@�B
@�B
@�B
@�B
AB
A B
AoB
A�B
A�B
BB
B'B
BuB
B�B
B�B
B�B
CB
C-B
C-B
CB
B�B
B�B
C�B
D�B
EB
ESB
ESB
ESB
E�B
EmB
EmB
E�B
E�B
E�B
E�B
F%B
FtB
F�B
GB
GB
GB
GEB
GB
G+B
F�B
GB
F�B
G+B
GzB
G�B
G+B
H1B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
J	B
J=B
J�B
J�B
KDB
K�B
LB
L0B
L�B
L�B
MB
MB
MPB
M�B
M�B
NVB
N�B
N�B
O(B
OB
O(B
O(B
OBB
OvB
O�B
OvB
O�B
O�B
O�B
O�B
PB
P�B
P�B
Q B
Q4B
QNB
Q�B
R B
R B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
TFB
TFB
TFB
U2B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
W$B
WYB
W�B
W�B
W�B
W�B
X+B
X+B
X�B
YB
Y�B
Y�B
ZQB
Z�B
ZkB
Z�B
Z�B
Z�B
[=B
[WB
[�B
\B
[�B
[�B
\CB
\�B
\�B
]B
]/B
]IB
]dB
]�B
^B
^OB
^�B
_B
_!B
_B
_!B
_;B
_VB
_VB
_pB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`\B
`BB
`vB
`�B
`�B
`�B
`�B
aB
a�B
b�B
b�B
b�B
b�B
cB
b�B
c�B
dB
dZB
dtB
d�B
eFB
ezB
e�B
ezB
ezB
ezB
e�B
e�B
e�B
e�B
fB
e�B
e�B
fB
f2B
gB
g8B
g�B
g�B
h>B
h�B
hsB
h�B
h�B
h�B
iDB
i�B
i�B
j0B
j0B
jKB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m]B
m�B
nB
m�B
m�B
m�B
ncB
n}B
n}B
n�B
n�B
oB
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
q'B
q'B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
sB
shB
s�B
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
t�B
uB
u?B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
vB
vB
vzB
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
yrB
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|jB
|PB
{�B
{�B
{�B
{�B
{B
{B
{�B
|B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~wB
~�B
~�B
~�B
~wB
~�B
cB
�B
�B
�B
� B
� B
�B
�4B
�i111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�+B	�zB	�tB	�ZB	�ZB	��B	�tB	�tB	�tB	�tB	�%B	�?B	�%B	�%B	�%B	�B	�?B	�%B	��B	�+B	�UB	��B	�&B	׍B	��B
?.B
�B
�B
�/B
��B
��Bb4B��BȚB�B�?B�!B�B�OB��B�3B�9B�3B�`B�PB��B�B9B�BhB9B�B<BJB�B�B�B�BGB�B�B��B��B��B��B��B��B�JBtTB'8B
�B
�uB
��B
p�B
D�B
;0B
88B
4TB
-�B
�B
�B	�9B	�yB	�B	��B	��B	x�B	b�B	O(B	>BB	+6B	 �B	WB	�B	
�B	�B	�B	�B	'B	-B	�B	�B	EB	B	�B	�B	�B	4B	�B	�B	<B	�B	�B	�B	7B	!�B	&LB	'�B	5B	�B	�B	�B	#B	#TB	*�B	0B	;0B	A�B	K^B	OBB	O\B	RB	S�B	S�B	T�B	O\B	KxB	\CB	`'B	^�B	b�B	raB	�FB	ϫB	յB	��B	�B	��B	�B	�B	�B	��B	�B	�mB	��B	�B	�B	�RB	�^B	�JB	��B	��B	�aB	�mB	��B	��B	��B	�aB	�ZB	��B	�B	�B	��B	�EB	�B	�B	�wB
3B
�B
�B
"NB
"B
 vB
%,B
(XB
'B
%�B
'mB
'RB
&�B
&�B
%�B
$�B
&fB
$�B
$�B
$@B
&�B
&�B
&2B
%FB
(�B
(sB
'RB
&�B
'mB
(>B
&�B
$�B
!�B
!HB
!bB
"B
&�B
(�B
+B
*�B
*0B
)_B
'RB
)B
(�B
(
B
&�B
$�B
&LB
(�B
(�B
(
B
(sB
)�B
)DB
(>B
'�B
'RB
'mB
(
B
(�B
(�B
)B
)B
)B
(�B
(sB
($B
'8B
%�B
&B
&fB
$�B
#�B
# B
$&B
#B
#�B
"�B
 BB
 'B
!�B
!�B
 �B
!B
�B
�B
IB
#B
kB
1B
�B
�B
�B
�B
�B
.B

�B
tB
%B
tB
 �B	��B	��B	�*B	�*B	��B	�ZB	�[B	�oB	��B	��B	�[B	�B	�B	�2B	��B
'B
B
%B
�B
�B
	lB

	B

=B

�B

�B

rB
	�B
	�B
	�B
	lB
�B
1B
KB
KB
KB
�B
xB

�B
�B

�B

�B

�B

rB

rB

=B

	B

#B

XB

XB

�B

XB

rB

rB

#B

#B

XB

#B

�B

=B

rB
	�B

�B

=B

#B

	B

	B

	B
	�B

#B
	�B
	�B

�B
�B
�B
�B
�B
^B
^B
)B
B

�B

�B

�B

#B

XB

=B

�B

#B

rB

�B

�B
^B
�B
�B
B
B
�B
�B
PB
jB
B
B
B
~B
�B
VB
�B
�B
(B
�B
vB
BB
�B
�B
BB
BB
�B
HB
bB
}B
bB
}B
}B
}B
}B
�B
�B
�B
TB
�B
TB
�B
�B
�B
:B
TB
�B
�B
�B
�B
@B
[B
[B
�B
�B
�B
�B
[B
2B
gB
gB
�B
B
9B
�B
�B
SB
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
)B
B
)B
�B
)B
�B
�B
�B
�B
�B
B
�B
~B
�B
jB
�B
�B
�B
�B
B
B
B
B
B
!B
�B
�B
 �B
�B
 B
�B
 vB
!�B
!�B
"4B
"hB
"�B
"�B
"�B
"�B
# B
#�B
#�B
#�B
$B
$�B
$�B
%`B
%�B
%zB
$�B
#�B
$&B
$�B
$@B
#:B
# B
#B
"�B
#B
# B
#nB
#�B
#�B
#�B
$B
$�B
%`B
&B
%�B
%�B
&LB
'�B
(XB
(�B
(�B
)B
)�B
)�B
*eB
*KB
*KB
*�B
*�B
)�B
)yB
(sB
(�B
)yB
)�B
)�B
*B
)�B
*KB
*eB
+B
+B
+�B
,=B
,WB
-]B
.}B
.cB
.}B
/�B
/�B
/�B
0oB
0oB
0�B
0!B
1B
1AB
2B
2�B
2�B
2�B
33B
3�B
4�B
4�B
4nB
4TB
4�B
5%B
5ZB
5�B
6+B
6FB
6`B
6�B
72B
72B
7LB
7fB
7�B
8B
88B
88B
8�B
8lB
8�B
9�B
9�B
9�B
:DB
:xB
:xB
:^B
;JB
;0B
;JB
;dB
;dB
;dB
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=B
="B
="B
=qB
=qB
=qB
=�B
=qB
=�B
=�B
=�B
>(B
>BB
>wB
?HB
?�B
@B
@B
@B
@B
@B
@OB
@B
@iB
@�B
@�B
@�B
@�B
AB
A B
AoB
A�B
A�B
BB
B'B
BuB
B�B
B�B
B�B
CB
C-B
C-B
CB
B�B
B�B
C�B
D�B
EB
ESB
ESB
ESB
E�B
EmB
EmB
E�B
E�B
E�B
E�B
F%B
FtB
F�B
GB
GB
GB
GEB
GB
G+B
F�B
GB
F�B
G+B
GzB
G�B
G+B
H1B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
J	B
J=B
J�B
J�B
KDB
K�B
LB
L0B
L�B
L�B
MB
MB
MPB
M�B
M�B
NVB
N�B
N�B
O(B
OB
O(B
O(B
OBB
OvB
O�B
OvB
O�B
O�B
O�B
O�B
PB
P�B
P�B
Q B
Q4B
QNB
Q�B
R B
R B
R:B
RoB
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
TFB
TFB
TFB
U2B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
V�B
W$B
WYB
W�B
W�B
W�B
W�B
X+B
X+B
X�B
YB
Y�B
Y�B
ZQB
Z�B
ZkB
Z�B
Z�B
Z�B
[=B
[WB
[�B
\B
[�B
[�B
\CB
\�B
\�B
]B
]/B
]IB
]dB
]�B
^B
^OB
^�B
_B
_!B
_B
_!B
_;B
_VB
_VB
_pB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`\B
`BB
`vB
`�B
`�B
`�B
`�B
aB
a�B
b�B
b�B
b�B
b�B
cB
b�B
c�B
dB
dZB
dtB
d�B
eFB
ezB
e�B
ezB
ezB
ezB
e�B
e�B
e�B
e�B
fB
e�B
e�B
fB
f2B
gB
g8B
g�B
g�B
h>B
h�B
hsB
h�B
h�B
h�B
iDB
i�B
i�B
j0B
j0B
jKB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m]B
m�B
nB
m�B
m�B
m�B
ncB
n}B
n}B
n�B
n�B
oB
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
q'B
q'B
qvB
q�B
q�B
q�B
r-B
r�B
r�B
sB
shB
s�B
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
t�B
uB
u?B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
vB
vB
vzB
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
yrB
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|jB
|PB
{�B
{�B
{�B
{�B
{B
{B
{�B
|B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~wB
~�B
~�B
~�B
~wB
~�B
cB
�B
�B
�B
� B
� B
�B
�4B
�i111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193047  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193048  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193048                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043055  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043055  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                