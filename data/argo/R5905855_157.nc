CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-03T06:41:53Z creation;2023-06-03T06:41:54Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230603064153  20230603065709  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�0���Y1   @�0	{B_@0W
=p���c��Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�ffB�  B�  B���B�  B�  B�  B�  Bܙ�B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bw�
Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��B��B��RBĸRBǅB˸RBϸRBӸRB׸RB�Q�B޸RB�B�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C��C��C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9��C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CKCM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs��Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��D w
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
DE}pDE�
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
Dx�
Dyw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�~�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�]/A�_�A�1'A�.A��A��A��GA�бAΕ�A�l�A�g�A�aA�Z�A�VmA�T�A�Q�A�OA�MjA�LdA�K�A�K)A�I�A�H�A�D�A�C�A�A A�?A�>BA�=�A�>�A�:�A�9$A�8RA�6zA�.�A�!-A��A���AͤtA�n�A��aAʼA��Aɩ�A��yA��AĶ�A�'A��7A��fA���A��6A��TA�/�A��A�ݘA�R A���A�6�A��A��4A�6�A�6zA�^jA�/�A���A�ԕA��EA���A��CA��WA�� A��dA���A�0�A��A���A��A��fA��A�:�A�^A�>�A��zA�iyA���A�
	A��A�~A��A���A�>�A���A�d�A~,�A|�Az�EAz(�Av�AqMAo4Ai�?Ag��Ae�Ad1�Ac�NAcm]Ab�A^FtA\w2A[q�A[1�AZ��AY�cAW��AR��AQ��AQیAP��AL�AI�AHAF��AEB�AB($A@b�A=jA;��A9��A9OA7C�A5��A5OA3�A2\�A0�}A/��A-z�A(��A%r�A#(A �.A"�A!�2A#/�A$xA$�OA$]dA#�A#33A"��A"��A" iA tTA 7�A��AoiAs�A�WAn�A�|A+kA�]A��A��AیAbNA�CA�4A�AC-A(A��A�cA��A�vAخA�BAR�A�&A�mA�WA9XAjA�A#�A+�AA�"A
��A	�WA	d�A�A�hA�0A_�A�Ar�AB[A��A�A��A��AA�A
�A��AC�A�A�A@�A ֡A u�A�An�A ��A A�A �@��[@���@��.@�a�@��@@�a@���@�u�@�8�@��Z@��@�S�@��)@�d�@���@���@�:*@�@�*�@�ی@�_@��6@��@�kQ@�Z�@�.@�X@��@�|�@��"@���@啁@�
=@���@�\�@�e,@�1@��.@��z@�F�@ݼ@�ߤ@�h�@۱[@��@�u%@�0�@ۊ	@ڇ�@�Y�@�q@���@���@�^�@֥z@��@ռ@�RT@���@Ա�@ԕ�@�A�@Ӳ�@�W?@�M@ѥ�@�dZ@��@�Ft@���@���@�E�@ͩ*@�w2@�iD@�J�@��y@�YK@� �@��}@ˢ�@�K�@�.I@�O�@��@ʙ1@�ƨ@���@�z@���@�33@���@Ƭ@�r�@��>@ŗ�@�f�@���@�_@��@�X�@��2@¯O@.@�]d@��@���@�c@��R@�:�@��@��-@�y�@�>�@���@�^5@���@�v`@���@�4n@�� @��@�G@��@��X@�.I@��8@���@��!@�$�@���@�k�@�>�@�ی@�R�@��@���@�}V@�;�@�	@��9@�Dg@�+@��p@��A@��@@�P�@��@��2@���@�~(@�>B@��N@�o @�E9@��?@�j@���@�\�@�(@��@�y>@�6@�.I@��P@���@�x@�^�@��`@���@���@�~(@�v�@�YK@�1'@��@�G@��)@��k@�f�@�K�@���@���@�dZ@�:�@���@��@���@��@���@�w�@�U2@��N@��{@�hs@�*0@��@�tT@�u@���@��@��$@���@��!@�R�@�@�ݘ@���@�x@�IR@���@�e@��]@���@�Z�@��[@���@�$�@���@�X@�;d@��|@�� @�$@��@��P@�o�@�+�@�V@���@�U2@�ԕ@���@�=�@��9@�=q@�3�@��#@�S�@�@��@���@�I�@�&�@�@�@���@�\)@�>�@�6z@���@��o@�~@���@�S�@��P@��y@��O@��?@�K^@��K@� \@��@��9@���@���@�_@��@�@�g�@��	@�]d@��@���@�Z�@� \@��P@�]�@�zx@�Mj@���@��.@�J�@��@��F@���@�O@��@�͟@��o@�K^@��r@���@��4@�rG@�iD@�L�@�*0@��@�h
@�Z�@�V@�L0@�  @�hs@��A@�t�@���@��_@�y>@�h
@�M@���@�(@��@�d�@�?�@��@�y>@�4�@�iD@�*0@��@���@�,=@�J@�($@��m@�\�@�?}@��8@���@���@�]d@���@���@���@�x@�+@��2@�֡@���@�u�@�$�@�!@��@��@�@]�@~� @~@�@~!�@}��@}�-@}/@|�@||�@{�@{l�@{4�@{/�@zں@z��@z��@zQ@z�@yA @xbN@x!@wb�@vq�@vO@u!�@t��@t[�@tx@s�F@sS@rh
@q��@qG�@q:�@p�@p4n@p'R@p  @o�W@o�V@oO@n�'@n��@n}V@n:*@n$�@n($@n)�@m�Z@m�#@m�@m�h@m^�@m^�@mT�@m=�@m/@lی@l~(@l*�@l>B@lFt@l�@k�@ky�@kiD@kiD@k33@kS@j�R@i��@i^�@i�@i�@i5�@h��@h�.@h��@h��@h~(@hZ@g�]@gƨ@g�a@g�[@gW?@g�@f�"@f��@f�8@f��@f�]@f��@f@e�@e�@eB�@d�@d��@d��@d@c�@bZ�@a��@`��@`y>@_خ@_+@^Ta@]��@]q@\�U@\��@\@[��@[�{@[1�@Z�@Z��@Zxl@ZOv@Z�@Y��@Y��@YrG@YG�@X�f@X�4@Xl"@X(�@W��@WS�@Vȴ@Vq�@U�@Uhs@T�@T�p@T�4@TV�@T2�@T	�@S�@S8@R��@R_�@R8�@R	@Q��@Qc�@P��@P��@Pr�@O��@OA�@N�@N�L@Nu%@N0U@M��@MDg@M;@L�@L?�@L!@L�@Kݘ@K��@K\)@K$t@J�]@J��@I�D@I�@Ihs@IB�@I�@H֡@H��@HC-@G�@G�*@G.I@F��@F��@F^5@FC�@E��@E�)@E��@EO�@E8�@D�E@DS�@C�g@C6z@B��@B��@B�<@B��@Bu%@BV@A�#@@�@?�}@?��@?y�@?K�@?=@?
=@>�\@>@�@=�d@=J�@=!�@=�@=%@<�5@<�@<��@<j@<�@<1@<�@;˒@;a@;/�@:��@:YK@:e@9�H@9J�@9�@8w�@8H@8@7��@7x@7Mj@7�@6�M@6��@5�@5c@5#�@4�|@4�`@4�[@4�j@4��@4�Y@4oi@4%�@3�+@3��@3�6@3��@3�@2�R@2�@2�h@2�x@2;�@1�>@1w2@0��@0S�@/�@/��@/�@/t�@/b�@/6z@.ȴ@.��@.u@-��@-�S@-��@-rG@-`B@-O�@-A @-0�@-%F@-�@,Ɇ@,��@,U2@+�g@+��@+=@*�s@*}V@*L0@*u@)��@)`B@)�@(�@(*�@(	�@'��@'s@'U�@'@&�"@&��@&��@&��@&W�@&5?@&	@%��@%��@%��@%:�@$Ɇ@$g8@#�@#�;@#� @#��@#��@#~�@#RT@#�@"��@"��@"n�@"q�@"L0@!�@!&�@!	l@!�@!;@ �P@ �@ �`@ ��@ U2@�+@�
@��@~�@]�@>�@/�@��@d�@&�@�@�@��@o @J�@�	@��@��@|�@c�@Xy@ �@x@�6@y�@�@��@d�@R�@�@�S@e,@��@K^@�
@n/@Mj@J#@6z@��@� @R�@�@�9@��@7L@֡@�e@��@oi@_@I�@9X@*�@b@x@  @�r@�A@�@ƨ@��@S�@͟@��@^5@-@.�@�@��@c�@S&@Dg@2a@�@ѷ@��@w�@Z@1@�@_p@A�@>�@8@�"@��@M�@�@�T@��@��@�=@��@�S@�~@w2@\�@:�@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�]/A�_�A�1'A�.A��A��A��GA�бAΕ�A�l�A�g�A�aA�Z�A�VmA�T�A�Q�A�OA�MjA�LdA�K�A�K)A�I�A�H�A�D�A�C�A�A A�?A�>BA�=�A�>�A�:�A�9$A�8RA�6zA�.�A�!-A��A���AͤtA�n�A��aAʼA��Aɩ�A��yA��AĶ�A�'A��7A��fA���A��6A��TA�/�A��A�ݘA�R A���A�6�A��A��4A�6�A�6zA�^jA�/�A���A�ԕA��EA���A��CA��WA�� A��dA���A�0�A��A���A��A��fA��A�:�A�^A�>�A��zA�iyA���A�
	A��A�~A��A���A�>�A���A�d�A~,�A|�Az�EAz(�Av�AqMAo4Ai�?Ag��Ae�Ad1�Ac�NAcm]Ab�A^FtA\w2A[q�A[1�AZ��AY�cAW��AR��AQ��AQیAP��AL�AI�AHAF��AEB�AB($A@b�A=jA;��A9��A9OA7C�A5��A5OA3�A2\�A0�}A/��A-z�A(��A%r�A#(A �.A"�A!�2A#/�A$xA$�OA$]dA#�A#33A"��A"��A" iA tTA 7�A��AoiAs�A�WAn�A�|A+kA�]A��A��AیAbNA�CA�4A�AC-A(A��A�cA��A�vAخA�BAR�A�&A�mA�WA9XAjA�A#�A+�AA�"A
��A	�WA	d�A�A�hA�0A_�A�Ar�AB[A��A�A��A��AA�A
�A��AC�A�A�A@�A ֡A u�A�An�A ��A A�A �@��[@���@��.@�a�@��@@�a@���@�u�@�8�@��Z@��@�S�@��)@�d�@���@���@�:*@�@�*�@�ی@�_@��6@��@�kQ@�Z�@�.@�X@��@�|�@��"@���@啁@�
=@���@�\�@�e,@�1@��.@��z@�F�@ݼ@�ߤ@�h�@۱[@��@�u%@�0�@ۊ	@ڇ�@�Y�@�q@���@���@�^�@֥z@��@ռ@�RT@���@Ա�@ԕ�@�A�@Ӳ�@�W?@�M@ѥ�@�dZ@��@�Ft@���@���@�E�@ͩ*@�w2@�iD@�J�@��y@�YK@� �@��}@ˢ�@�K�@�.I@�O�@��@ʙ1@�ƨ@���@�z@���@�33@���@Ƭ@�r�@��>@ŗ�@�f�@���@�_@��@�X�@��2@¯O@.@�]d@��@���@�c@��R@�:�@��@��-@�y�@�>�@���@�^5@���@�v`@���@�4n@�� @��@�G@��@��X@�.I@��8@���@��!@�$�@���@�k�@�>�@�ی@�R�@��@���@�}V@�;�@�	@��9@�Dg@�+@��p@��A@��@@�P�@��@��2@���@�~(@�>B@��N@�o @�E9@��?@�j@���@�\�@�(@��@�y>@�6@�.I@��P@���@�x@�^�@��`@���@���@�~(@�v�@�YK@�1'@��@�G@��)@��k@�f�@�K�@���@���@�dZ@�:�@���@��@���@��@���@�w�@�U2@��N@��{@�hs@�*0@��@�tT@�u@���@��@��$@���@��!@�R�@�@�ݘ@���@�x@�IR@���@�e@��]@���@�Z�@��[@���@�$�@���@�X@�;d@��|@�� @�$@��@��P@�o�@�+�@�V@���@�U2@�ԕ@���@�=�@��9@�=q@�3�@��#@�S�@�@��@���@�I�@�&�@�@�@���@�\)@�>�@�6z@���@��o@�~@���@�S�@��P@��y@��O@��?@�K^@��K@� \@��@��9@���@���@�_@��@�@�g�@��	@�]d@��@���@�Z�@� \@��P@�]�@�zx@�Mj@���@��.@�J�@��@��F@���@�O@��@�͟@��o@�K^@��r@���@��4@�rG@�iD@�L�@�*0@��@�h
@�Z�@�V@�L0@�  @�hs@��A@�t�@���@��_@�y>@�h
@�M@���@�(@��@�d�@�?�@��@�y>@�4�@�iD@�*0@��@���@�,=@�J@�($@��m@�\�@�?}@��8@���@���@�]d@���@���@���@�x@�+@��2@�֡@���@�u�@�$�@�!@��@��@�@]�@~� @~@�@~!�@}��@}�-@}/@|�@||�@{�@{l�@{4�@{/�@zں@z��@z��@zQ@z�@yA @xbN@x!@wb�@vq�@vO@u!�@t��@t[�@tx@s�F@sS@rh
@q��@qG�@q:�@p�@p4n@p'R@p  @o�W@o�V@oO@n�'@n��@n}V@n:*@n$�@n($@n)�@m�Z@m�#@m�@m�h@m^�@m^�@mT�@m=�@m/@lی@l~(@l*�@l>B@lFt@l�@k�@ky�@kiD@kiD@k33@kS@j�R@i��@i^�@i�@i�@i5�@h��@h�.@h��@h��@h~(@hZ@g�]@gƨ@g�a@g�[@gW?@g�@f�"@f��@f�8@f��@f�]@f��@f@e�@e�@eB�@d�@d��@d��@d@c�@bZ�@a��@`��@`y>@_خ@_+@^Ta@]��@]q@\�U@\��@\@[��@[�{@[1�@Z�@Z��@Zxl@ZOv@Z�@Y��@Y��@YrG@YG�@X�f@X�4@Xl"@X(�@W��@WS�@Vȴ@Vq�@U�@Uhs@T�@T�p@T�4@TV�@T2�@T	�@S�@S8@R��@R_�@R8�@R	@Q��@Qc�@P��@P��@Pr�@O��@OA�@N�@N�L@Nu%@N0U@M��@MDg@M;@L�@L?�@L!@L�@Kݘ@K��@K\)@K$t@J�]@J��@I�D@I�@Ihs@IB�@I�@H֡@H��@HC-@G�@G�*@G.I@F��@F��@F^5@FC�@E��@E�)@E��@EO�@E8�@D�E@DS�@C�g@C6z@B��@B��@B�<@B��@Bu%@BV@A�#@@�@?�}@?��@?y�@?K�@?=@?
=@>�\@>@�@=�d@=J�@=!�@=�@=%@<�5@<�@<��@<j@<�@<1@<�@;˒@;a@;/�@:��@:YK@:e@9�H@9J�@9�@8w�@8H@8@7��@7x@7Mj@7�@6�M@6��@5�@5c@5#�@4�|@4�`@4�[@4�j@4��@4�Y@4oi@4%�@3�+@3��@3�6@3��@3�@2�R@2�@2�h@2�x@2;�@1�>@1w2@0��@0S�@/�@/��@/�@/t�@/b�@/6z@.ȴ@.��@.u@-��@-�S@-��@-rG@-`B@-O�@-A @-0�@-%F@-�@,Ɇ@,��@,U2@+�g@+��@+=@*�s@*}V@*L0@*u@)��@)`B@)�@(�@(*�@(	�@'��@'s@'U�@'@&�"@&��@&��@&��@&W�@&5?@&	@%��@%��@%��@%:�@$Ɇ@$g8@#�@#�;@#� @#��@#��@#~�@#RT@#�@"��@"��@"n�@"q�@"L0@!�@!&�@!	l@!�@!;@ �P@ �@ �`@ ��@ U2@�+@�
@��@~�@]�@>�@/�@��@d�@&�@�@�@��@o @J�@�	@��@��@|�@c�@Xy@ �@x@�6@y�@�@��@d�@R�@�@�S@e,@��@K^@�
@n/@Mj@J#@6z@��@� @R�@�@�9@��@7L@֡@�e@��@oi@_@I�@9X@*�@b@x@  @�r@�A@�@ƨ@��@S�@͟@��@^5@-@.�@�@��@c�@S&@Dg@2a@�@ѷ@��@w�@Z@1@�@_p@A�@>�@8@�"@��@M�@�@�T@��@��@�=@��@�S@�~@w2@\�@:�@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
UgB
UB
UgB
TaB
T�B
TaB
TaB
TFB
S�B
S@B
S[B
S&B
S&B
S[B
SB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
RoB
R�B
RTB
RTB
RTB
RoB
R B
Q�B
QB
P�B
P�B
R�B
��B
�1B
�yB
�B
�B
�B
��B
�IB
��B�B	7B�B-�BQ�Bm�B��B�B�DB�?B��B�TB�B�B�"B��B͹B�$B޸B�pB�B�HB�%B�B��B��B�'B�DBy�BnBeBZ�BG+B7�B0�B(�BB
�B
�6B
�WB
}"B
u�B
a�B
EB
,�B
OB
eB
�B
jB	�8B	ںB	�rB	�IB	�nB	�1B	��B	��B	�#B	��B	y	B	raB	pB	n�B	l�B	hXB	e�B	W�B	N�B	K�B	G�B	D�B	?�B	?�B	="B	9$B	1'B	+�B	�B	�B	�B	jB	�B		B	�B	1B�(B�zB�OB��B�B�QB�,B�WB�yB�B	/B	5?B	@iB	N�B	RB	V�B	X�B	[=B	]�B	_B	_�B	`�B	`�B	_pB	\�B	[=B	[�B	VB	T,B	R�B	R B	J�B	?�B	@4B	H�B	LJB	M�B	G+B	HKB	V�B	bhB	_;B	Z�B	YB	TFB	X+B	cB	k6B	r�B	sB	|B	� B	�QB	�B	��B	��B	�=B	�WB	�_B	��B	�0B	��B	�GB	�MB	��B	�<B	��B	�hB	��B	��B	�aB	�gB	��B	�!B	��B	��B	��B	��B	��B	�LB	�zB	��B	��B	��B	�B	��B	�4B	�*B	�B	��B	��B	�}B	��B	�KB	�yB	��B	�DB	��B	��B	��B	�FB	�;B	�hB	�4B	� B	�RB	�B	�MB	��B	��B	��B	�B	�B	��B	�oB	�UB	��B	�vB	�9B	��B	��B	�B	�'B	�B	��B	�=B	��B	�eB	��B	��B	�B	�qB	�6B	�B	�JB	��B	�B	��B	��B	ƨB	�B	��B	�(B	��B	� B	�TB	�B	��B	՛B	׍B	ؓB	ؓB	�1B	ؓB	յB	ԯB	�B	�VB	��B	��B	�B	�bB	�|B	�B	��B	�B	�RB	�*B	�B	��B	��B	�/B	�B	� B	��B	� B	�iB	��B	��B	��B	�AB	�B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�nB	��B	�B	��B	��B	��B	��B	�B	��B	�2B	�DB	��B	�B	��B	��B	�B	��B	�]B	�HB
  B
 �B
B
�B
�B
3B
gB
�B
�B
gB
B
�B
9B
�B
YB
�B
+B
_B
�B
B
�B
	B
	RB
	RB
	�B
^B
�B
B
xB
dB
dB
JB
�B
B

�B
	lB
KB

=B
�B
�B
�B
xB
xB
�B
�B
\B
�B
NB
hB
4B
B
TB
�B
�B
�B
2B
�B
�B
gB
2B
�B
FB
FB
FB
B
�B
FB
,B
FB
B
aB
�B
�B
�B
QB
�B
kB
�B
�B
=B
qB
qB
�B
�B
B
�B
�B
�B
�B
�B
QB
B
WB
�B
=B
/B
IB
dB
�B
B
�B
�B
�B
�B
;B
B
pB
�B
 'B
 �B
!bB
"NB
$�B
%zB
,�B
0�B
1�B
3�B
4B
3�B
2�B
2aB
2�B
1�B
2aB
2B
2�B
4B
0�B
1vB
33B
3B
49B
7B
5ZB
4�B
4�B
3�B
2�B
2aB
1�B
1[B
2|B
3�B
4�B
8�B
;�B
<�B
<�B
<�B
>B
>�B
>�B
?B
?�B
?�B
@ B
@OB
@�B
AUB
AUB
A�B
A�B
A�B
A�B
B'B
B'B
B�B
B�B
BuB
BAB
A�B
?�B
>B
:xB
8�B
8�B
8�B
8�B
88B
7�B
8B
8RB
8�B
9rB
:�B
=�B
B�B
D�B
E�B
F%B
F�B
E�B
E�B
HB
H�B
IB
H�B
I7B
IRB
IB
I�B
J	B
JXB
J�B
JrB
J�B
KB
KB
J�B
KxB
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L~B
LdB
L�B
MjB
N�B
NB
M�B
M�B
NVB
N�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
NpB
NpB
M�B
N"B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P}B
PHB
P.B
QhB
QhB
QNB
QNB
Q4B
Q�B
R:B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UB
U2B
U2B
UMB
U�B
U�B
U�B
V�B
W?B
W�B
X+B
YKB
YKB
Y�B
Y�B
YeB
Y1B
XEB
W�B
WsB
W�B
XyB
YKB
ZQB
[qB
[�B
[�B
[�B
\B
[�B
[�B
\�B
^jB
_!B
_;B
_;B
_VB
_�B
_�B
`\B
`\B
_�B
_�B
`\B
`�B
`�B
aB
_�B
^�B
^B
]�B
]�B
\�B
[�B
[WB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
\)B
\xB
\�B
\�B
\�B
]B
]�B
]�B
]�B
]�B
^B
^jB
^B
^5B
^�B
^�B
^�B
_;B
_;B
_�B
`vB
`�B
`�B
a-B
a�B
a�B
a�B
bNB
cB
cTB
c�B
c�B
c�B
d@B
d�B
eFB
e�B
e�B
f2B
ffB
ffB
f�B
ffB
ffB
gB
g�B
g�B
h$B
h$B
h$B
h$B
h>B
hXB
hsB
hsB
hsB
h�B
i�B
jKB
jB
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
l"B
l�B
l�B
l�B
mwB
mCB
mwB
m�B
m�B
n/B
ncB
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p�B
q�B
qvB
qvB
q�B
qvB
qvB
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sB
sB
r�B
s�B
tB
tB
tB
tTB
t�B
t�B
uB
uZB
u�B
u�B
vFB
vzB
w2B
wLB
w�B
w�B
x8B
xRB
xlB
xlB
x�B
yrB
y�B
z*B
zDB
zDB
z^B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{B
z�B
{�B
|B
|B
|B
|B
|PB
|jB
|�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
B
HB
�B
�B
�B
�4B
�4B
�4B
�OB
�iB
�OB
�iB
�iB
��B
��B
��B
�;B
�UB
��B
��B
��B
�[B
��B
��B
��B
��B
�aB
�aB
��B
��B
��B
��B
�B
�B
�B
�SB
��B
��B
��B
��B
�B
�B
�%B
�tB
��B
�+B
�zB
��B
��B
��B
��B
��B
�1B
��B
�fB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�#B
�	B
�XB
��B
�B
�)B
�DB
�^B
��B
��B
�xB
��B
�JB
��B
��B
��B
�B
��B
�PB
��B
��B
��B
��B
��B
��B
�<B
�<B
�<B
��B
��B
�BB
�BB
�BB
�vB
�B
�HB
�B
�NB
��B
��B
� B
�B
��B
�TB
��B
��B
�&B
�&B
��B
��B
�FB
�FB
�aB
�FB
�FB
�aB
�FB
�{B
�aB
�aB
�aB
�aB
�aB
�FB
�{B
�{B
��B
��B
�B
�B
�2B
�2B
��B
��B
��B
��B
��B
��B
�9B
�9B
��B
��B
��B
�
B
�?B
��B
��B
��B
��B
��B
�EB
��B
��B
��B
�B
�1B
�B
�1B
�B
�KB
�eB
�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
UgB
UB
UgB
TaB
T�B
TaB
TaB
TFB
S�B
S@B
S[B
S&B
S&B
S[B
SB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
RoB
R�B
RTB
RTB
RTB
RoB
R B
Q�B
QB
P�B
P�B
R�B
��B
�1B
�yB
�B
�B
�B
��B
�IB
��B�B	7B�B-�BQ�Bm�B��B�B�DB�?B��B�TB�B�B�"B��B͹B�$B޸B�pB�B�HB�%B�B��B��B�'B�DBy�BnBeBZ�BG+B7�B0�B(�BB
�B
�6B
�WB
}"B
u�B
a�B
EB
,�B
OB
eB
�B
jB	�8B	ںB	�rB	�IB	�nB	�1B	��B	��B	�#B	��B	y	B	raB	pB	n�B	l�B	hXB	e�B	W�B	N�B	K�B	G�B	D�B	?�B	?�B	="B	9$B	1'B	+�B	�B	�B	�B	jB	�B		B	�B	1B�(B�zB�OB��B�B�QB�,B�WB�yB�B	/B	5?B	@iB	N�B	RB	V�B	X�B	[=B	]�B	_B	_�B	`�B	`�B	_pB	\�B	[=B	[�B	VB	T,B	R�B	R B	J�B	?�B	@4B	H�B	LJB	M�B	G+B	HKB	V�B	bhB	_;B	Z�B	YB	TFB	X+B	cB	k6B	r�B	sB	|B	� B	�QB	�B	��B	��B	�=B	�WB	�_B	��B	�0B	��B	�GB	�MB	��B	�<B	��B	�hB	��B	��B	�aB	�gB	��B	�!B	��B	��B	��B	��B	��B	�LB	�zB	��B	��B	��B	�B	��B	�4B	�*B	�B	��B	��B	�}B	��B	�KB	�yB	��B	�DB	��B	��B	��B	�FB	�;B	�hB	�4B	� B	�RB	�B	�MB	��B	��B	��B	�B	�B	��B	�oB	�UB	��B	�vB	�9B	��B	��B	�B	�'B	�B	��B	�=B	��B	�eB	��B	��B	�B	�qB	�6B	�B	�JB	��B	�B	��B	��B	ƨB	�B	��B	�(B	��B	� B	�TB	�B	��B	՛B	׍B	ؓB	ؓB	�1B	ؓB	յB	ԯB	�B	�VB	��B	��B	�B	�bB	�|B	�B	��B	�B	�RB	�*B	�B	��B	��B	�/B	�B	� B	��B	� B	�iB	��B	��B	��B	�AB	�B	�AB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�nB	��B	�B	��B	��B	��B	��B	�B	��B	�2B	�DB	��B	�B	��B	��B	�B	��B	�]B	�HB
  B
 �B
B
�B
�B
3B
gB
�B
�B
gB
B
�B
9B
�B
YB
�B
+B
_B
�B
B
�B
	B
	RB
	RB
	�B
^B
�B
B
xB
dB
dB
JB
�B
B

�B
	lB
KB

=B
�B
�B
�B
xB
xB
�B
�B
\B
�B
NB
hB
4B
B
TB
�B
�B
�B
2B
�B
�B
gB
2B
�B
FB
FB
FB
B
�B
FB
,B
FB
B
aB
�B
�B
�B
QB
�B
kB
�B
�B
=B
qB
qB
�B
�B
B
�B
�B
�B
�B
�B
QB
B
WB
�B
=B
/B
IB
dB
�B
B
�B
�B
�B
�B
;B
B
pB
�B
 'B
 �B
!bB
"NB
$�B
%zB
,�B
0�B
1�B
3�B
4B
3�B
2�B
2aB
2�B
1�B
2aB
2B
2�B
4B
0�B
1vB
33B
3B
49B
7B
5ZB
4�B
4�B
3�B
2�B
2aB
1�B
1[B
2|B
3�B
4�B
8�B
;�B
<�B
<�B
<�B
>B
>�B
>�B
?B
?�B
?�B
@ B
@OB
@�B
AUB
AUB
A�B
A�B
A�B
A�B
B'B
B'B
B�B
B�B
BuB
BAB
A�B
?�B
>B
:xB
8�B
8�B
8�B
8�B
88B
7�B
8B
8RB
8�B
9rB
:�B
=�B
B�B
D�B
E�B
F%B
F�B
E�B
E�B
HB
H�B
IB
H�B
I7B
IRB
IB
I�B
J	B
JXB
J�B
JrB
J�B
KB
KB
J�B
KxB
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L~B
LdB
L�B
MjB
N�B
NB
M�B
M�B
NVB
N�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
NpB
NpB
M�B
N"B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P}B
PHB
P.B
QhB
QhB
QNB
QNB
Q4B
Q�B
R:B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UB
U2B
U2B
UMB
U�B
U�B
U�B
V�B
W?B
W�B
X+B
YKB
YKB
Y�B
Y�B
YeB
Y1B
XEB
W�B
WsB
W�B
XyB
YKB
ZQB
[qB
[�B
[�B
[�B
\B
[�B
[�B
\�B
^jB
_!B
_;B
_;B
_VB
_�B
_�B
`\B
`\B
_�B
_�B
`\B
`�B
`�B
aB
_�B
^�B
^B
]�B
]�B
\�B
[�B
[WB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
\)B
\xB
\�B
\�B
\�B
]B
]�B
]�B
]�B
]�B
^B
^jB
^B
^5B
^�B
^�B
^�B
_;B
_;B
_�B
`vB
`�B
`�B
a-B
a�B
a�B
a�B
bNB
cB
cTB
c�B
c�B
c�B
d@B
d�B
eFB
e�B
e�B
f2B
ffB
ffB
f�B
ffB
ffB
gB
g�B
g�B
h$B
h$B
h$B
h$B
h>B
hXB
hsB
hsB
hsB
h�B
i�B
jKB
jB
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
l"B
l�B
l�B
l�B
mwB
mCB
mwB
m�B
m�B
n/B
ncB
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p�B
q�B
qvB
qvB
q�B
qvB
qvB
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sB
sB
r�B
s�B
tB
tB
tB
tTB
t�B
t�B
uB
uZB
u�B
u�B
vFB
vzB
w2B
wLB
w�B
w�B
x8B
xRB
xlB
xlB
x�B
yrB
y�B
z*B
zDB
zDB
z^B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{B
z�B
{�B
|B
|B
|B
|B
|PB
|jB
|�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
B
HB
�B
�B
�B
�4B
�4B
�4B
�OB
�iB
�OB
�iB
�iB
��B
��B
��B
�;B
�UB
��B
��B
��B
�[B
��B
��B
��B
��B
�aB
�aB
��B
��B
��B
��B
�B
�B
�B
�SB
��B
��B
��B
��B
�B
�B
�%B
�tB
��B
�+B
�zB
��B
��B
��B
��B
��B
�1B
��B
�fB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�#B
�	B
�XB
��B
�B
�)B
�DB
�^B
��B
��B
�xB
��B
�JB
��B
��B
��B
�B
��B
�PB
��B
��B
��B
��B
��B
��B
�<B
�<B
�<B
��B
��B
�BB
�BB
�BB
�vB
�B
�HB
�B
�NB
��B
��B
� B
�B
��B
�TB
��B
��B
�&B
�&B
��B
��B
�FB
�FB
�aB
�FB
�FB
�aB
�FB
�{B
�aB
�aB
�aB
�aB
�aB
�FB
�{B
�{B
��B
��B
�B
�B
�2B
�2B
��B
��B
��B
��B
��B
��B
�9B
�9B
��B
��B
��B
�
B
�?B
��B
��B
��B
��B
��B
�EB
��B
��B
��B
�B
�1B
�B
�1B
�B
�KB
�eB
�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230603064147  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230603064153  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230603064154  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230603064154                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230603064155  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230603064155  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230603065709                      G�O�G�O�G�O�                