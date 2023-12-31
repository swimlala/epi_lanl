CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230616190822  20230616190822  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @ٸ�כ�1   @ٸۡ/v@*� ě���c�l�C��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��B��B��B��B��RB��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RCC�)C�)C�)C	�)C�)C�)C�)C�)C�)C��C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
Dp�D�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�>�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�>�D�~�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�>�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��yA��A��A��A��A��A���A��A���A���A���A���A���A�  A���A���A�  A�A�A���A��A��A��yA��A��A��mA��/A��#A���A���A��
A��Aа!AУ�AЅA�S�A�r�A�oA�Aú^A�r�A�Q�A��!A�bNA��A���A�9XA�C�A�XA��!A���A��7A��A�A�A��A�dZA�ƨA�+A~�jA|��Az�DAv�yAu�As�TAn��Am&�AlJAh1Ae��Ac�A`=qA\ �AY7LAW�AQK�AO�mAN�jAK��AI�TAI�AE�#AB5?A@A>��A<�jA;��A;33A:��A8^5A3hsA2bA1O�A/��A.��A.��A.�RA.��A.�/A.��A.^5A.��A-/A,��A,��A,�jA,-A+��A+��A*bNA(��A'l�A'&�A&r�A&^5A&1'A%��A$��A#��A#/A"��A"A�A!�mA!�A �!A ^5A 5?A�wA/A�!A$�A�A�
A�^A�A��A�hAS�A�!A5?AA��A/A��AVAbA��A�!A��At�AdZA|�AO�A�9AbNA�mA�A�A�A��A��A5?A�mA��A|�AdZA;dA�A�A�yA�+A=qA��A��A��A|�A/A��A�uAM�A�A�A��A�A�A~�AE�A�#A�^A��A�A7LA%A��AZA=qA(�A1A�FA�AS�A
�A
�!A
��A
ZA	�#A	`BA	�A�yAĜA�A\)A/AVAA��A��A�A?}A?}A��A��A�7AG�A�\A�\A^5AbA�#AdZA ��A -@��y@���@�@��T@��h@��@��y@�M�@��@�`B@��D@���@�~�@�O�@�r�@�(�@���@��;@�F@�@�@�+@���@��T@��@�(�@��@��@�&�@�bN@��;@�P@�S�@�33@ꗍ@���@�@��@��@�z�@�\)@���@��`@�u@�Z@�(�@�F@�!@��#@��`@���@�l�@�@ް!@�^5@���@�O�@���@�I�@��;@ە�@�o@�ȴ@�5?@�X@�?}@�/@�&�@�%@ؼj@�9X@�1@�  @��m@�ƨ@ׅ@��@�V@�@�X@�V@��/@ԓu@�I�@�  @�C�@ҸR@�@Ѻ^@љ�@ёh@д9@��@�S�@�"�@�ȴ@�V@���@�p�@�%@�z�@�1@˶F@�V@�O�@���@�Z@��m@ǍP@��@Ɵ�@�@Ų-@�O�@ļj@���@�dZ@��H@�ff@��@��-@�x�@�?}@��`@��@�  @���@�;d@��@���@�@���@��^@��h@��@�hs@��@���@���@�j@�  @�ƨ@��P@�o@��!@�$�@��7@�G�@���@�Ĝ@�Q�@�1@��@�S�@��@��R@��!@���@���@�E�@�p�@�(�@�
=@��R@��#@���@�/@���@���@�Q�@���@���@�S�@�o@��\@�^5@�J@���@�X@���@�1@��;@�\)@�
=@��@��+@�~�@�5?@���@���@��h@�/@��@�bN@���@�;d@��H@��\@�J@���@���@�X@��`@�1@���@�-@���@��/@���@��@�j@�A�@��@��m@��F@�dZ@��@�~�@�@�hs@�?}@�&�@��@�V@�%@�%@�%@�V@�V@�%@�%@���@���@���@�z�@�I�@�1@��@��w@�|�@��@�V@�J@��T@�@��@��@�Ĝ@��u@�bN@� �@��m@�ƨ@��F@�S�@��@�n�@�@��#@��7@�/@���@��u@�z�@�r�@�r�@�Q�@� �@�  @��
@��@�"�@��y@�J@��h@�/@��@��@�%@��9@�I�@��
@��@�t�@�+@��H@��+@�ff@�ff@�M�@�-@��^@�x�@��`@��@��u@�1@�t�@�K�@���@��@�@�7L@��@��@�r�@�(�@���@���@���@���@��@�\)@�33@��R@���@���@�M�@��T@���@�p�@�?}@�V@���@���@��9@��@��@��@�S�@�+@��y@��R@���@�n�@�-@���@���@�`B@��j@�Z@�I�@�1'@�  @��@~��@}��@}/@|��@|(�@|�@|1@{33@z=q@y�@y�^@yG�@y7L@x��@x��@xQ�@w�@w;d@v��@v��@vE�@u�T@up�@u?}@t��@tz�@tI�@tI�@tI�@t9X@s��@so@r��@r�\@q�#@q7L@p��@pQ�@p  @o�;@o�P@n��@nff@m@kƨ@j~�@jJ@i�^@iX@i%@h��@hĜ@h�@g
=@d9X@c��@c��@c��@c��@c��@c�m@c�m@cƨ@c��@cC�@b�H@a�@`�`@`1'@`  @_�;@_�w@_�@^�+@^5?@]��@]p�@]?}@]�@\�@\�@[ƨ@[�F@["�@Z�\@Zn�@ZM�@ZJ@Y��@Y�@Xr�@X �@W��@W|�@Vȴ@Vv�@Vv�@V5?@V@U�@U��@U��@UO�@T��@Tj@T9X@S�
@SdZ@SS�@SS�@S33@S@R�H@R�\@R-@RJ@Q��@P��@P��@P�u@Pr�@PbN@PA�@P �@Pb@O�@O�P@Nȴ@NV@M�@M@M/@L��@L��@L��@LI�@Kƨ@K��@Kt�@Kt�@Kt�@KdZ@K"�@J�@J~�@J=q@JJ@Ihs@I�@H�`@H�@Hb@G��@G+@F��@F�@Fȴ@F�R@F�R@F�R@F��@Fv�@E�@E�-@E�@Ep�@E`B@E?}@D��@D��@D�j@D�@D�@Dz�@D�@C��@C��@C�@C�@Ct�@CdZ@CdZ@C33@Co@C@B�@B��@B�\@B=q@A�@A��@AX@A�@@��@@�`@@�`@@��@@�u@@Q�@@b@?�;@?�@?�P@?l�@?K�@>��@>ȴ@>��@>v�@=�@=��@=`B@<��@<��@<�j@<�@<1@;�@;o@:�\@:-@9��@9�@9��@8Ĝ@8�@8r�@8Q�@8A�@8  @7�P@7�P@7K�@7
=@6��@6ȴ@6��@6v�@6ff@6E�@6E�@65?@6{@5��@5��@5�h@5�@5O�@5V@4��@4�j@4�@4��@4�D@4I�@3��@3�F@3�@3o@3@2�@2��@2J@1hs@1%@0Ĝ@0 �@/�w@/�P@/l�@/+@.�@.��@.ff@.5?@-�T@-�h@,��@,�@,�@+��@*�H@*-@)��@)��@)��@)��@)x�@)X@)G�@)G�@)&�@)�@(Ĝ@(1'@'�@'�P@';d@'
=@&ȴ@&v�@&E�@%@%`B@%�@$�@$�@$��@$��@$�D@$Z@#��@#C�@#o@"�H@"��@"�\@"-@"J@!�^@!�@ ��@ bN@  �@�@��@K�@+@
=@�y@��@ff@$�@@�-@�@?}@��@�/@�@j@9X@�m@��@�@dZ@C�@"�@�@��@��@n�@M�@-@-@J@�#@��@�@��@��@�9@�9@�9@�u@r�@1'@�;@��@�P@\)@��@�+@V@E�@5?@$�@@��@p�@O�@�@�@��@�D@z�@z�@Z@�@��@t�@dZ@dZ@S�@"�@�H@�!@��@�\@�\@n�@M�@M�@-@�@�@�@�@�@�@�@�@��@G�@&�@%@��@�`@Ĝ@Ĝ@��@r�@  @��@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��yA��A��A��A��A��A���A��A���A���A���A���A���A�  A���A���A�  A�A�A���A��A��A��yA��A��A��mA��/A��#A���A���A��
A��Aа!AУ�AЅA�S�A�r�A�oA�Aú^A�r�A�Q�A��!A�bNA��A���A�9XA�C�A�XA��!A���A��7A��A�A�A��A�dZA�ƨA�+A~�jA|��Az�DAv�yAu�As�TAn��Am&�AlJAh1Ae��Ac�A`=qA\ �AY7LAW�AQK�AO�mAN�jAK��AI�TAI�AE�#AB5?A@A>��A<�jA;��A;33A:��A8^5A3hsA2bA1O�A/��A.��A.��A.�RA.��A.�/A.��A.^5A.��A-/A,��A,��A,�jA,-A+��A+��A*bNA(��A'l�A'&�A&r�A&^5A&1'A%��A$��A#��A#/A"��A"A�A!�mA!�A �!A ^5A 5?A�wA/A�!A$�A�A�
A�^A�A��A�hAS�A�!A5?AA��A/A��AVAbA��A�!A��At�AdZA|�AO�A�9AbNA�mA�A�A�A��A��A5?A�mA��A|�AdZA;dA�A�A�yA�+A=qA��A��A��A|�A/A��A�uAM�A�A�A��A�A�A~�AE�A�#A�^A��A�A7LA%A��AZA=qA(�A1A�FA�AS�A
�A
�!A
��A
ZA	�#A	`BA	�A�yAĜA�A\)A/AVAA��A��A�A?}A?}A��A��A�7AG�A�\A�\A^5AbA�#AdZA ��A -@��y@���@�@��T@��h@��@��y@�M�@��@�`B@��D@���@�~�@�O�@�r�@�(�@���@��;@�F@�@�@�+@���@��T@��@�(�@��@��@�&�@�bN@��;@�P@�S�@�33@ꗍ@���@�@��@��@�z�@�\)@���@��`@�u@�Z@�(�@�F@�!@��#@��`@���@�l�@�@ް!@�^5@���@�O�@���@�I�@��;@ە�@�o@�ȴ@�5?@�X@�?}@�/@�&�@�%@ؼj@�9X@�1@�  @��m@�ƨ@ׅ@��@�V@�@�X@�V@��/@ԓu@�I�@�  @�C�@ҸR@�@Ѻ^@љ�@ёh@д9@��@�S�@�"�@�ȴ@�V@���@�p�@�%@�z�@�1@˶F@�V@�O�@���@�Z@��m@ǍP@��@Ɵ�@�@Ų-@�O�@ļj@���@�dZ@��H@�ff@��@��-@�x�@�?}@��`@��@�  @���@�;d@��@���@�@���@��^@��h@��@�hs@��@���@���@�j@�  @�ƨ@��P@�o@��!@�$�@��7@�G�@���@�Ĝ@�Q�@�1@��@�S�@��@��R@��!@���@���@�E�@�p�@�(�@�
=@��R@��#@���@�/@���@���@�Q�@���@���@�S�@�o@��\@�^5@�J@���@�X@���@�1@��;@�\)@�
=@��@��+@�~�@�5?@���@���@��h@�/@��@�bN@���@�;d@��H@��\@�J@���@���@�X@��`@�1@���@�-@���@��/@���@��@�j@�A�@��@��m@��F@�dZ@��@�~�@�@�hs@�?}@�&�@��@�V@�%@�%@�%@�V@�V@�%@�%@���@���@���@�z�@�I�@�1@��@��w@�|�@��@�V@�J@��T@�@��@��@�Ĝ@��u@�bN@� �@��m@�ƨ@��F@�S�@��@�n�@�@��#@��7@�/@���@��u@�z�@�r�@�r�@�Q�@� �@�  @��
@��@�"�@��y@�J@��h@�/@��@��@�%@��9@�I�@��
@��@�t�@�+@��H@��+@�ff@�ff@�M�@�-@��^@�x�@��`@��@��u@�1@�t�@�K�@���@��@�@�7L@��@��@�r�@�(�@���@���@���@���@��@�\)@�33@��R@���@���@�M�@��T@���@�p�@�?}@�V@���@���@��9@��@��@��@�S�@�+@��y@��R@���@�n�@�-@���@���@�`B@��j@�Z@�I�@�1'@�  @��@~��@}��@}/@|��@|(�@|�@|1@{33@z=q@y�@y�^@yG�@y7L@x��@x��@xQ�@w�@w;d@v��@v��@vE�@u�T@up�@u?}@t��@tz�@tI�@tI�@tI�@t9X@s��@so@r��@r�\@q�#@q7L@p��@pQ�@p  @o�;@o�P@n��@nff@m@kƨ@j~�@jJ@i�^@iX@i%@h��@hĜ@h�@g
=@d9X@c��@c��@c��@c��@c��@c�m@c�m@cƨ@c��@cC�@b�H@a�@`�`@`1'@`  @_�;@_�w@_�@^�+@^5?@]��@]p�@]?}@]�@\�@\�@[ƨ@[�F@["�@Z�\@Zn�@ZM�@ZJ@Y��@Y�@Xr�@X �@W��@W|�@Vȴ@Vv�@Vv�@V5?@V@U�@U��@U��@UO�@T��@Tj@T9X@S�
@SdZ@SS�@SS�@S33@S@R�H@R�\@R-@RJ@Q��@P��@P��@P�u@Pr�@PbN@PA�@P �@Pb@O�@O�P@Nȴ@NV@M�@M@M/@L��@L��@L��@LI�@Kƨ@K��@Kt�@Kt�@Kt�@KdZ@K"�@J�@J~�@J=q@JJ@Ihs@I�@H�`@H�@Hb@G��@G+@F��@F�@Fȴ@F�R@F�R@F�R@F��@Fv�@E�@E�-@E�@Ep�@E`B@E?}@D��@D��@D�j@D�@D�@Dz�@D�@C��@C��@C�@C�@Ct�@CdZ@CdZ@C33@Co@C@B�@B��@B�\@B=q@A�@A��@AX@A�@@��@@�`@@�`@@��@@�u@@Q�@@b@?�;@?�@?�P@?l�@?K�@>��@>ȴ@>��@>v�@=�@=��@=`B@<��@<��@<�j@<�@<1@;�@;o@:�\@:-@9��@9�@9��@8Ĝ@8�@8r�@8Q�@8A�@8  @7�P@7�P@7K�@7
=@6��@6ȴ@6��@6v�@6ff@6E�@6E�@65?@6{@5��@5��@5�h@5�@5O�@5V@4��@4�j@4�@4��@4�D@4I�@3��@3�F@3�@3o@3@2�@2��@2J@1hs@1%@0Ĝ@0 �@/�w@/�P@/l�@/+@.�@.��@.ff@.5?@-�T@-�h@,��@,�@,�@+��@*�H@*-@)��@)��@)��@)��@)x�@)X@)G�@)G�@)&�@)�@(Ĝ@(1'@'�@'�P@';d@'
=@&ȴ@&v�@&E�@%@%`B@%�@$�@$�@$��@$��@$�D@$Z@#��@#C�@#o@"�H@"��@"�\@"-@"J@!�^@!�@ ��@ bN@  �@�@��@K�@+@
=@�y@��@ff@$�@@�-@�@?}@��@�/@�@j@9X@�m@��@�@dZ@C�@"�@�@��@��@n�@M�@-@-@J@�#@��@�@��@��@�9@�9@�9@�u@r�@1'@�;@��@�P@\)@��@�+@V@E�@5?@$�@@��@p�@O�@�@�@��@�D@z�@z�@Z@�@��@t�@dZ@dZ@S�@"�@�H@�!@��@�\@�\@n�@M�@M�@-@�@�@�@�@�@�@�@�@��@G�@&�@%@��@�`@Ĝ@Ĝ@��@r�@  @��@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A�ƨA�A�A���AؼjAظRAش9Aز-Aذ!AجAة�Aإ�Aأ�A؟�A؝�A؛�Aؙ�Aؗ�Aؕ�AؑhA؏\A؏\A؍PA؏\A؍PA؍PA؋DAؗ�Aؗ�A؅A؟�A��mA�Q�A�bAڸRA�jA�VA�5?A�z�A�XAލPA�ȴA��yA��mA�%A�/A��A��/A��A�bA�ZA�~�A�ȴA��;A��/A��A���A���A��TA�"�A�$�A�JA�(�A�oA�+A�I�A�S�A��A�+A���A��A��A�VA��A��A�z�A���A��A�1'A�p�A�;dA��A�&�A���A�1'A�O�A�^5A�\A��A�FA���A���A��
A��#A��A��A�(�A�=qA�I�A�A�r�A�hsA㝲A���A��A���A��;A�9A��A�%B\)BÖBiyB�)B�-B1'BH�A�XB-BC�B��B�B$�B5?B;dB33B'�B\B�)B��B�B$�BT�BaHB\)BN�BL�BH�B6FBuB-A���A�n�A�jA�G�A��;A�A�VA�Q�A��A��TB�BiyBt�Bx�Bt�BiyBVB;dB"�B�=Be`B�DB�\BM�B�B��A�ffA�^5A���A��A�5?A�`BA��FB2-B49Bl�B� B�VB��B�-B�BuB�B �B(�B?}BdZB�B�\B��B��B�FB�qBƨB��B��B�;B�B�B�sB�B��B��BB%B+BDB\BbBbBbB\B
=B	7B	7B	7B	7BDBJB
=B
=BDBB��B��B�B�`B�HB�HB�HB�NB�sB�B��B�B�B�B�ZB�BB�/B��BǮB��B��B�/B��BhB�B-B9XB>wBC�BF�BH�BK�BL�BN�BO�BO�BP�BS�BS�BS�BVBVBXB\)B_;B_;B`BB_;B_;BaHBcTBffBiyBk�Bm�Bl�Bl�Bm�Bo�Bq�Bu�Bv�Bu�Bv�Bu�Bu�By�Bz�B{�B}�B� B�B�%B�1B�=B�JB�PB�PB�JB�PB�VB�VB�VB�bB�uB��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�7B�B|�Bq�BiyBZBH�B8RB%�B$�B�BPB  B�B�ZB�B�sB�}B�+BQ�B�B�TB��B�JB��B�\B��B1B!�B �B��B�;B�BŢB�9B��BW
B��Bl�B�B+BBB(�BG�BVBYBp�B{�Be`BR�B8RB+B��B�dB��B��B��Bm�B��BVB �
A��9B /B �BL�B��B�!B�+BVB:^B�B�B�BbBS�B�B��B��B��B~�B`BB��B�NBB{B	7B�NB��BA�BBk�BuBp�B�wB��B��B��B��BYB�BVB� B�B��B�%B�A���A��`A�ƨA�?}A�  A��mA�A�A��RB z�B\)B�XBA�B �B�BPB  B�B�B�BG�B_;BgmBhsBgmBcTBF�BbB�LBD�B��B� BC�B��Bn�B ��B ��B33B�VBx�B ǮB {A�bNA�?}A���A�dZA��A���A���A�^A왚A�E�A�ĜA�x�A�~�B 0!B�BBE�B]/B,B��B n�A���A�(�A뙚A���A��TA��mA��A�1A��A�;dA�/A�/A��A� �A��A�JA�JA�VA�C�A���A��;A�bNA�ĜB %B ��BBcTB��B��B�B��B��BB�B%�B�B�/B��B�B e`A��PA��\A�$�A��A�p�A�VA�Q�A�XA�XA�ffA�|�A�jA�hsA�x�A�7A�dZA�A���A�Q�A�
=B �`B�XBw�B��BN�B��B�B5?Bk�B��BƨB�yB
=B �B49BC�BP�B]/BhsBp�Bv�B� B�oB�mB�B��B��BB1BJBhB{B�B�B �B#�B%�B+B0!B>wBJ�BVBaHBjBq�Bx�B�B�1B�VB��B��B��B��B��B�B�B�B�-B�9B�FB�XB�dB�wBBŢBƨBǮBȴB��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�B�#B�#B�/B�5B�;B�;B�BB�NB�TB�TB�ZB�ZB�ZB�`B�`B�`B�fB�mB�mB�sB�yB�yB�yB�sB�mB�mB�mB�mB�fB�`B�fB�`B�ZB�TB�NB�BB�5B�)B�/B�HB�NB�TB�TB�TB�NB�HB�;B�5B�)B�)B�B�
B��B��B��B��BȴBǮBŢBÖBĜBĜBĜBŢBŢBƨBȴB��B��B��B��B��B��B��B��B�B�B�B�B�#B�)B�/B�;B�HB�NB�TB�`B�fB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B  BBBBDBuB�B�B�B!�B)�B.B2-B6FB9XB<jB@�BE�BH�BK�BN�BQ�BS�BVBYB[#B]/B_;BaHBcTBffBhsBiyBm�Bo�Br�Bu�Bw�Bz�B{�B}�B~�B�B�B�B�+B�7B�DB�VB�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�9B�9B�9B�9B�9B�9B�3B�3B�9B�?B�FB�FB�FB�FB�LB�LB�RB�RB�RB�XB�RB�XB�dB�dB�qB�wB�wB�}B��B��BBÖBĜBŢBŢBƨBƨBƨBƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�B�#B�#B�)B�/B�/B�5B�;B�;B�HB�NB�NB�TB�TB�ZB�ZB�`B�fB�fB�mB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBBBBBBBBBBBBBBB%BB444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A؟�A؝�A؛�Aؙ�Aؕ�Aؕ�AؑiA؍PA؍PA؋DA؇+A؃A�~�A�|�A�z�A�v�A�t�A�p�A�n�A�jA�hsA�fgA�d[A�bNA�`BA�\)A�ZA�ZA�XA�ZA�XA�XA�VA�bNA�bNA�O�A�jAز.A��A��#AڃA�5@A��A�  A�E�A�"�A�XAޓuA޴:A޲.A���A���A��aAާ�A��TA��#A�$�A�I�AߓuAߩ�Aߧ�Aߣ�A�ĜA�A߮A��A��A��A��A��0A���A�{A��A�O�A���A���A��aA�kA��A��TA�wA�E�A❳A�r�A���A�;eA�%A�_A��AᗎA���A��A�(�A�ZA�t�A�A♚A⟿A��A��A�RA�_A��A�2A�{A�K�A�=qA�34A�hsA㗎A�FA���A��A�~�A�n�A���BA�B��BN�B��B��B�B.A�"�BnB(�B�=B��B
=B�B �B�BPB��B��B�7B��B
=B:^BF�BA�B49B2-B.B�B��BnAA�9YA�+A�oA��A�PA� �A��A��mA��B��BN�BZB^5BZBN�B;dB �B1Bo�BJ�Bp�Bt�B33B�B�AA�1'A�(�A蛦A��mA�  A�+A�B�B�BQ�Be`Bs�B�B��B�B��BB%BVB$�BI�BffBt�B�B�\B��B��B�B�!B�3BěB��B��B��B��B�5B�TB�lB�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�fB�ZB�B��B��BƨBƨBƨBǮB��B��B�#B�B��B��BɺBŢBB�?B�B�!B�RBB�;B��BBnB�B#�B(�B,B.B1'B2-B49B5?B5?B6EB9XB9XB9XB;dB;dB=pBA�BD�BD�BE�BD�BD�BF�BH�BK�BN�BP�BR�BQ�BQ�BR�BT�BW
B[#B\)B[#B\)B[#B[#B_;B`ABaGBcTBe`BglBk�Bm�Bo�Bq�Br�Br�Bq�Br�Bs�Bs�Bs�Bu�Bx�B|�B~�B~�B}�B� B�B}�By�B{�B}�B}�B� B�B�%B�1B�CB�IB�CB�7B�%B�7B�CB�CB�CB�CB�7B�+B�%B�1B�7B�1B�+B�B� By�Bx�Bv�Bs�Bn�BiyBbNBW
BN�B?}B.B�BCB
=B��B�B�`B��BɺB��B��B��Bl�B7LBBȴB�=Bq�B~�Bt�B�LB�B+B%B�#BěB�pB�B��B�B<jB�;BQ�BBbB�lB�BVB-B;dB>wBVBaGBJ�B8RB�B�B�RB��B�VB�+B�+BR�B�-B�B �jA�~�B {B �nB2-B�IB��Bl�B;dB�B��BglBhrB��B9XBglBz�B�B|�BdZBE�B�3BǮB�yB��B�BǮB�B&�B �lBP�B��BVB��B�^B�B�=B�B>wB�B;dBe`B�}B�XBk�B ��A���A��!A��iA�
>A���A��.A�JA��B `ABA�B��B&�B �BB �B�`B��B�tBB-BD�BL�BM�BL�BH�B,B��B��B)�B�-Be`B(�B�^BS�B �B �PB�Bs�B^5B �A��A�-A�
>A���A�/A���A�fgA���A�A�d[A�cA�]A�C�A�I�B �BŢB+BB�BhB� B S�A�p�A��A�d[A�ȵA�A�.A�_A���A��aA�%A���A���A��TA��A��TA��A��A��A�WA�DA���A�-A��]A��B �B �lBH�B�IB�RB��B�)B�AB�fB��BCB��BB{�BB J�A�XA�ZA��A�wA�;eA� �A��A�"�A�"�A�1'A�G�A�5@A�34A�C�A�S�A�/A�PA���A��A���B ��B��B]/B�;B49B�+B�
B�BP�B�B�B��B�B%B�B(�B6EBB�BM�BVB\)Be`Bw�B��B��B�)B�NB�yB�B�B��B��B��BB%B	7BCBbB�B#�B0!B;dBF�BO�BW
B^5BffBm�Bs�Bz�B�%B�7B�CB�PB�bB�nB�{B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�EB�LB�RB�RB�XB�dB�jB�jB�pB�}B�}B�}B��B��BBÕBěBěBŢBǮBȴBȴBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBǮBŢBÕB��BBƨBǮBȴBȴBȴBǮBƨBěBÕB��B��B�}B�jB�XB�EB�3B�!B�B�B�B��B��B��B��B�B�B�B�B�!B�'B�3B�?B�?B�LB�LB�XB�dB�pB�wB�}B��B��BBěBƨBǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�#B�)B�/B�;B�AB�AB�GB�NB�TB�TB�ZB�`B�fB�lB�yB�B��B��B��BB+B\BtB�B�B�B!�B%�B+B.B1'B49B7LB9XB;dB>wB@�BB�BD�BF�BH�BK�BM�BN�BR�BT�BXB[#B]/B`ABaGBcTBdZBffBhrBjBl�Bn�Bp�Bs�Bu�Bv�Bx�By�B{�B}�B�B�B�B�%B�+B�1B�7B�CB�PB�\B�bB�nB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�3B�3B�3B�?B�EB�LB�RB�XB�^B�^B�dB�dB�jB�pB�pB�wB�}B��B��B��BBBÕBěBěBƨBǮBǮBȴBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�#B�#B�#B�#B�)B�/B�5B�5B�5B�;B�AB�AB�GB�GB�GB�TB�ZB�ZB�ZB�ZB�`B�fB�fB�fB�lB�lB�lB�lB�lB�rB�rB�rB�rB�rB�rB�lB�lB�lB�rB�yB�yB�B�B�B�B�B�B�B�B�yB�y444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190822              20230616190822  AO  ARCAADJP                                                                    20230616190822    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190822    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190822  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190822  QCF$                G�O�G�O�G�O�8800            