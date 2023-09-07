CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-25T08:01:06Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220225080106  20220225080106  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @ټS���s1   @ټT-�� @&�x����d=�-V1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @�ff@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BW��B`  Bh��Bo33Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��AA=A]A|(�A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/�
B7�
B?p�BGp�BOp�BW
>B_p�Bh=qBn��Bw
>Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C��C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
DI�pDJw
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
DV}pDV�
DWw
DW�
DXw
DX�
DY}pDY�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8RD�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D��RD�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�8RD�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�>�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D��RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�p�A�x�A�r�A�l�A�n�A�|�A�z�A�z�A�x�A�hsA�jA�n�A�l�A�ffA�hsA�jA�l�A�jA�hsA�ffA�^5A�K�A��A�Aә�A�VA�bA��HA���A�z�A�ĜA���A��A�?}A�`BA�E�A��RA��^A�hsA���A�ZA���A��A�|�A��A�A���A��hA�ZA�r�A�$�A���A��mA���A�{A�=qA�XA���A�E�A�VA���A��A�+AS�A~I�A}�TA|�AshsAg��A\��AXv�AV=qAQ��AKXAGXAF  AE��AC�PAC%ABjA@��A?�7A>��A>5?A=hsA<�yA<r�A<1A;�PA;G�A;33A;
=A:�HA:�\A:r�A:�A9�TA8z�A6��A6M�A5�
A5G�A4��A45?A3�TA3�-A3t�A3XA3A2�!A2~�A25?A1�^A1��A1�A0�A0n�A0  A/�A.�RA.I�A-?}A,�/A, �A+hsA*�!A*�A)�hA)"�A(�HA(ZA'��A'��A'K�A&�A&��A&Q�A&�A%�A%�7A%+A$��A$r�A#��A#�PA#XA"�/A"�+A"v�A"jA"ZA!�wA!?}A!"�A �HA ��A �uA -A�A�HAĜA�9A��A��A�uA�A �A�^A�PA?}A��A9XA�A`BA%A��A�A��A�jAVAA�A  A��AO�A�`A��A1'A�-AXA/A�RA^5AAdZA
=A�/A�HAA�`A��A�\AA�A��A"�A�yA-A�TA�^A��AC�A^5AG�A�TA/A��A�RAn�A �A�-A?}A
�/A
�A
{A	�^A	;dA��A�A�wA33A�/AE�AA��A�mA��A��A��AQ�A�mA��A��A�PAl�A33A"�AoA�A�AdZA ��A �\A r�A =qA 1@���@�-@���@��@���@�A�@�dZ@��H@�^5@�@��7@�/@���@��@��F@��R@��7@�%@��9@�bN@���@�ƨ@�@�ȴ@�{@�-@��@��D@�1'@�@�K�@�@�!@�J@�?}@�j@�Q�@�A�@���@�t�@�33@��@�&�@�u@�(�@�F@��@�@�P@���@��@�V@��/@��/@���@�Ĝ@�A�@��;@ߥ�@�C�@ݑh@�o@�^5@�`B@��@ؼj@�A�@�@�@ղ-@�`B@��@ԣ�@�ƨ@�
=@�E�@�hs@�&�@�%@У�@Ϯ@�33@��@��H@Χ�@�ff@�M�@���@͡�@�O�@̃@�ƨ@ˍP@�S�@�@ʗ�@�V@�J@���@ɉ7@�O�@Ȭ@�I�@��;@�ƨ@ǶF@�l�@�"�@���@�V@�p�@ģ�@��@���@�|�@��@°!@�@�^5@�$�@���@���@�O�@�A�@�|�@�;d@�n�@�5?@�$�@��-@��j@�z�@�Z@��@���@�ƨ@��F@���@���@���@�dZ@�@���@�~�@�E�@�@��^@�%@��F@���@�~�@�-@�J@���@�p�@��/@��u@�1'@��@��m@���@�+@��+@�@�O�@���@�z�@�9X@���@���@�=q@���@��T@��^@�%@�z�@�Q�@�9X@���@�l�@��@�5?@�{@�@�hs@�&�@���@��@���@�\)@��R@��@��@�j@�(�@�b@���@���@���@�|�@�33@���@��\@��#@���@��h@�`B@�7L@���@�j@��F@�t�@�"�@�ȴ@�=q@���@�?}@�/@��u@�t�@���@�X@�Ĝ@�Z@�1@�S�@��!@�-@�@��h@�G�@�%@���@�A�@��P@��+@��@��#@���@�G�@���@�j@�1'@�ƨ@�K�@�@��H@�ȴ@���@�5?@�@�x�@��@���@�Q�@�9X@���@�ƨ@�dZ@�K�@�C�@�33@��H@���@���@��!@�~�@�E�@��-@��9@��D@�I�@��@��;@�\)@��@���@���@�v�@�M�@�-@�{@��^@�hs@�X@�G�@��@���@��j@���@��@�j@���@�t�@�K�@��@�n�@�-@��@��T@���@���@�p�@�O�@��@���@��/@���@��@�I�@�@~ff@}�-@|��@|��@|�@|9X@|1@|1@{�F@{��@{33@{@z�!@y��@x�9@x �@w��@w�@v��@vV@up�@u�@uV@t�/@tZ@s��@st�@so@r~�@r=q@q��@q�@q��@q&�@p��@o��@o+@n�@n�+@nE�@m�@mp�@l��@k�m@k�
@kdZ@j��@j�\@jn�@jM�@jJ@i�^@iX@h�`@h  @g��@g�@g+@fff@f$�@e��@eV@d(�@c�@b��@b=q@a��@a7L@a%@`��@`�9@`r�@`1'@`1'@` �@_�;@^ȴ@^V@^{@]@]?}@\��@\I�@\�@[�
@[��@[�@[�@[t�@[t�@[o@Z��@Y��@Yhs@Y&�@Y�@X��@W�w@W;d@V�y@Vȴ@V�+@V@U�h@T�D@S��@S��@S"�@Q��@QX@Q7L@P�`@P�9@P�u@PA�@O�@Ol�@OK�@O�@N�y@Nȴ@N��@Nff@N5?@N$�@M�@M@M?}@L�/@LZ@K��@K��@K33@K"�@K"�@J�H@J^5@I��@IX@I�@H��@HbN@HQ�@H1'@G�@Gl�@G;d@G+@F��@Fff@F@E�-@E?}@D��@D��@D��@D9X@C�
@C�@CC�@C33@C33@C@B�\@B-@BJ@A��@AX@@��@@�`@@�`@@��@@��@@�`@@�@@ �@?�;@?|�@>ȴ@>E�@>$�@>@=�-@=�h@=`B@=/@=V@<�@<�j@<Z@;�m@;dZ@;33@;o@:��@:M�@:�@9�#@9�^@9�7@97L@9%@8Ĝ@8r�@81'@8b@7�@7�w@7�@7l�@6�y@6$�@5�@5��@5@5�@4�/@4�@4�D@4I�@3�m@3C�@2�H@1��@1�#@1��@1�^@1��@1��@1%@0�9@0�9@0��@0�@0r�@0�@0�u@0��@0�@0bN@0  @/�;@/l�@.�y@.��@.v�@.E�@-�@-�@-�-@-p�@-V@,�D@,�@+�F@+��@+�@+�@+�@+�@+S�@*��@*J@)�7@)G�@)7L@)&�@)%@(��@(�u@(r�@(A�@'�w@'|�@'K�@'
=@&�@&�+@&V@&5?@&5?@&$�@&$�@&{@&@&@&@&@%�-@%p�@%O�@$�/@$��@$�@$j@$Z@$9X@$9X@$9X@$(�@#�m@#t�@"�H@"�@"J@!��@!G�@!�@ ��@ Ĝ@ �@ Q�@ b@�@|�@;d@+@
=@��@E�@@�h@`B@�@�j@�D@z�@j@j@Z@�@��@�m@�F@S�@"�@@�@��@�\@n�@�@�@�#@��@x�@&�@%@��@A�@��@|�@l�@K�@+@��@��@��@�+@ff@E�@5?@{@�T@��@@�-@��@�h@p�@/@V@�/@�j@Z@�m@��@��@��@��@��@��@�@t�@C�@��@n�@=q@-@J@��@��@��@��@��@�@��@x�@G�@7L@�@�`@bN@ �@b@  @�;@|�@��@v�@E�@�@�-@�h@�@`B@/@�@�@�@��@�@��@z�@j@j@Z@I�@9X@(�@(�@�@1@1@1@��111111111111111111111111111111111111111111111111111111111111144111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�p�A�x�A�r�A�l�A�n�A�|�A�z�A�z�A�x�A�hsA�jA�n�A�l�A�ffA�hsA�jA�l�A�jA�hsA�ffA�^5A�K�A��A�Aә�A�VA�bA��HA���A�z�A�ĜA���A��A�?}A�`BA�E�A��RA��^A�hsA���A�ZA���A��A�|�A��A�A���A��hA�ZA�r�A�$�A���A��mA���A�{A�=qA�XA���A�E�A�VA���A��A�+AS�A~I�A}�TA|�AshsAg��A\��AXv�AV=qAQ��AKXAGXAF  AE��AC�PAC%ABjA@��A?�7A>��A>5?A=hsA<�yA<r�A<1A;�PA;G�A;33A;
=A:�HA:�\A:r�A:�A9�TA8z�A6��A6M�A5�
A5G�A4��A45?A3�TA3�-A3t�A3XA3A2�!A2~�A25?A1�^A1��A1�A0�A0n�A0  A/�A.�RA.I�A-?}A,�/A, �A+hsA*�!A*�A)�hA)"�A(�HA(ZA'��A'��A'K�A&�A&��A&Q�A&�A%�A%�7A%+A$��A$r�A#��A#�PA#XA"�/A"�+A"v�A"jA"ZA!�wA!?}A!"�A �HA ��A �uA -A�A�HAĜA�9A��A��A�uA�A �A�^A�PA?}A��A9XA�A`BA%A��A�A��A�jAVAA�A  A��AO�A�`A��A1'A�-AXA/A�RA^5AAdZA
=A�/A�HAA�`A��A�\AA�A��A"�A�yA-A�TA�^A��AC�A^5AG�A�TA/A��A�RAn�A �A�-A?}A
�/A
�A
{A	�^A	;dA��A�A�wA33A�/AE�AA��A�mA��A��A��AQ�A�mA��A��A�PAl�A33A"�AoA�A�AdZA ��A �\A r�A =qA 1@���@�-@���@��@���@�A�@�dZ@��H@�^5@�@��7@�/@���@��@��F@��R@��7@�%@��9@�bN@���@�ƨ@�@�ȴ@�{@�-@��@��D@�1'@�@�K�@�@�!@�J@�?}@�j@�Q�@�A�@���@�t�@�33@��@�&�@�u@�(�@�F@��@�@�P@���@��@�V@��/@��/@���@�Ĝ@�A�@��;@ߥ�@�C�@ݑh@�o@�^5@�`B@��@ؼj@�A�@�@�@ղ-@�`B@��@ԣ�@�ƨ@�
=@�E�@�hs@�&�@�%@У�@Ϯ@�33@��@��H@Χ�@�ff@�M�@���@͡�@�O�@̃@�ƨ@ˍP@�S�@�@ʗ�@�V@�J@���@ɉ7@�O�@Ȭ@�I�@��;@�ƨ@ǶF@�l�@�"�@���@�V@�p�@ģ�@��@���@�|�@��@°!@�@�^5@�$�@���@���@�O�@�A�@�|�@�;d@�n�@�5?@�$�@��-@��j@�z�@�Z@��@���@�ƨ@��F@���@���@���@�dZ@�@���@�~�@�E�@�@��^@�%@��F@���@�~�@�-@�J@���@�p�@��/@��u@�1'@��@��m@���@�+@��+@�@�O�@���@�z�@�9X@���@���@�=q@���@��T@��^@�%@�z�@�Q�@�9X@���@�l�@��@�5?@�{@�@�hs@�&�@���@��@���@�\)@��R@��@��@�j@�(�@�b@���@���@���@�|�@�33@���@��\@��#@���@��h@�`B@�7L@���@�j@��F@�t�@�"�@�ȴ@�=q@���@�?}@�/@��u@�t�@���@�X@�Ĝ@�Z@�1@�S�@��!@�-@�@��h@�G�@�%@���@�A�@��P@��+@��@��#@���@�G�@���@�j@�1'@�ƨ@�K�@�@��H@�ȴ@���@�5?@�@�x�@��@���@�Q�@�9X@���@�ƨ@�dZ@�K�@�C�@�33@��H@���@���@��!@�~�@�E�@��-@��9@��D@�I�@��@��;@�\)@��@���@���@�v�@�M�@�-@�{@��^@�hs@�X@�G�@��@���@��j@���@��@�j@���@�t�@�K�@��@�n�@�-@��@��T@���@���@�p�@�O�@��@���@��/@���@��@�I�@�@~ff@}�-@|��@|��@|�@|9X@|1@|1@{�F@{��@{33@{@z�!@y��@x�9@x �@w��@w�@v��@vV@up�@u�@uV@t�/@tZ@s��@st�@so@r~�@r=q@q��@q�@q��@q&�@p��@o��@o+@n�@n�+@nE�@m�@mp�@l��@k�m@k�
@kdZ@j��@j�\@jn�@jM�@jJ@i�^@iX@h�`@h  @g��@g�@g+@fff@f$�@e��@eV@d(�@c�@b��@b=q@a��@a7L@a%@`��@`�9@`r�@`1'@`1'@` �@_�;@^ȴ@^V@^{@]@]?}@\��@\I�@\�@[�
@[��@[�@[�@[t�@[t�@[o@Z��@Y��@Yhs@Y&�@Y�@X��@W�w@W;d@V�y@Vȴ@V�+@V@U�h@T�D@S��@S��@S"�@Q��@QX@Q7L@P�`@P�9@P�u@PA�@O�@Ol�@OK�@O�@N�y@Nȴ@N��@Nff@N5?@N$�@M�@M@M?}@L�/@LZ@K��@K��@K33@K"�@K"�@J�H@J^5@I��@IX@I�@H��@HbN@HQ�@H1'@G�@Gl�@G;d@G+@F��@Fff@F@E�-@E?}@D��@D��@D��@D9X@C�
@C�@CC�@C33@C33@C@B�\@B-@BJ@A��@AX@@��@@�`@@�`@@��@@��@@�`@@�@@ �@?�;@?|�@>ȴ@>E�@>$�@>@=�-@=�h@=`B@=/@=V@<�@<�j@<Z@;�m@;dZ@;33@;o@:��@:M�@:�@9�#@9�^@9�7@97L@9%@8Ĝ@8r�@81'@8b@7�@7�w@7�@7l�@6�y@6$�@5�@5��@5@5�@4�/@4�@4�D@4I�@3�m@3C�@2�H@1��@1�#@1��@1�^@1��@1��@1%@0�9@0�9@0��@0�@0r�@0�@0�u@0��@0�@0bN@0  @/�;@/l�@.�y@.��@.v�@.E�@-�@-�@-�-@-p�@-V@,�D@,�@+�F@+��@+�@+�@+�@+�@+S�@*��@*J@)�7@)G�@)7L@)&�@)%@(��@(�u@(r�@(A�@'�w@'|�@'K�@'
=@&�@&�+@&V@&5?@&5?@&$�@&$�@&{@&@&@&@&@%�-@%p�@%O�@$�/@$��@$�@$j@$Z@$9X@$9X@$9X@$(�@#�m@#t�@"�H@"�@"J@!��@!G�@!�@ ��@ Ĝ@ �@ Q�@ b@�@|�@;d@+@
=@��@E�@@�h@`B@�@�j@�D@z�@j@j@Z@�@��@�m@�F@S�@"�@@�@��@�\@n�@�@�@�#@��@x�@&�@%@��@A�@��@|�@l�@K�@+@��@��@��@�+@ff@E�@5?@{@�T@��@@�-@��@�h@p�@/@V@�/@�j@Z@�m@��@��@��@��@��@��@�@t�@C�@��@n�@=q@-@J@��@��@��@��@��@�@��@x�@G�@7L@�@�`@bN@ �@b@  @�;@|�@��@v�@E�@�@�-@�h@�@`B@/@�@�@�@��@�@��@z�@j@j@Z@I�@9X@(�@(�@�@1@1@1@��111111111111111111111111111111111111111111111111111111111111144111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	H�B	H�B	G�B	H�B	I�B	I�B	H�B	H�B	H�B	G�B	H�B	I�B	H�B	H�B	I�B	I�B	I�B	H�B	I�B	H�B	H�B	H�B	G�B	J�B	H�B	F�B	E�B	ȴB	�BVB,BO�BVBA�B �BuBA�BI�BK�B!�B
��B+B
�B
��B
�#B
�/B
ɺB
�'B
��B
�B
�B
��B
�B
`BB
B�B
�B
I�B
P�B
VB
Q�B
@�B
�B	�LB	�B	��B	��B	�5B	�DB	49B	uB	+B	<jB	�B	B	�B	Q�B	jB	ZB	�%B	�VB	�=B	��B	��B	�TB	�B	��B
1B
hB
�B
%�B
0!B
1'B
6FB
9XB
E�B
K�B
XB
T�B
^5B
�\B
�oB
��B
��B
��B
�B
�-B
�FB
�XB
�RB
�dB
��B
B
ÖB
��B
��B
ĜB
ĜB
ŢB
��B
ĜB
ƨB
�dB
ƨB
�}B
�wB
�wB
��B
ÖB
B
ƨB
��B
B
B
ÖB
��B
B
��B
��B
�}B
�jB
�dB
�jB
�LB
�3B
�wB
�jB
�LB
�XB
�}B
�wB
�^B
�3B
�B
�LB
�9B
�-B
�3B
�B
��B
��B
�'B
�-B
�-B
�'B
�!B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�\B
�PB
�\B
�bB
�uB
��B
��B
��B
�oB
�{B
�\B
�7B
�+B
�DB
�%B
�1B
�DB
�1B
~�B
t�B
ffB
e`B
jB
s�B
q�B
o�B
l�B
iyB
hsB
ffB
e`B
cTB
bNB
^5B
ZB
YB
^5B
ZB
ZB
YB
\)B
`BB
^5B
[#B
W
B
O�B
N�B
R�B
W
B
ZB
ZB
W
B
S�B
T�B
Q�B
I�B
E�B
D�B
E�B
L�B
N�B
L�B
J�B
G�B
>wB
?}B
K�B
K�B
G�B
C�B
D�B
D�B
E�B
D�B
C�B
A�B
@�B
;dB
7LB
9XB
<jB
=qB
=qB
;dB
<jB
9XB
49B
33B
5?B
33B
2-B
49B
1'B
33B
33B
1'B
.B
-B
0!B
2-B
5?B
6FB
33B
49B
0!B
$�B
/B
/B
+B
%�B
�B
�B
%�B
&�B
'�B
/B
2-B
0!B
.B
(�B
&�B
%�B
�B
bB
1B
�B
�B
!�B
#�B
�B
�B
�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
"�B
"�B
�B
�B
�B
"�B
$�B
#�B
!�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
PB
VB
�B
hB
�B
�B
{B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
DB
+B
VB
�B
�B
�B
�B
{B
oB
�B
�B
�B
�B
�B
oB
bB
bB
\B
hB
{B
uB
bB
PB
�B
�B
�B
�B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
%�B
&�B
&�B
%�B
%�B
%�B
#�B
$�B
"�B
"�B
'�B
+B
(�B
(�B
'�B
#�B
"�B
&�B
'�B
&�B
$�B
&�B
&�B
(�B
!�B
�B
 �B
�B
%�B
,B
,B
(�B
(�B
+B
.B
/B
.B
,B
)�B
)�B
'�B
%�B
.B
0!B
0!B
.B
,B
1'B
2-B
/B
1'B
33B
6FB
5?B
33B
0!B
1'B
33B
2-B
33B
6FB
9XB
7LB
8RB
7LB
<jB
<jB
;dB
9XB
;dB
<jB
;dB
8RB
6FB
33B
1'B
=qB
<jB
=qB
<jB
:^B
;dB
A�B
A�B
A�B
@�B
A�B
A�B
@�B
@�B
C�B
C�B
B�B
B�B
A�B
C�B
C�B
A�B
>wB
=qB
B�B
@�B
?}B
D�B
F�B
H�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
G�B
C�B
C�B
B�B
H�B
I�B
L�B
M�B
L�B
M�B
N�B
L�B
M�B
L�B
L�B
K�B
I�B
G�B
L�B
M�B
L�B
M�B
N�B
L�B
P�B
Q�B
P�B
N�B
N�B
M�B
N�B
N�B
P�B
P�B
Q�B
P�B
P�B
O�B
N�B
R�B
S�B
S�B
S�B
S�B
Q�B
P�B
Q�B
VB
S�B
R�B
W
B
W
B
W
B
VB
T�B
T�B
S�B
Q�B
T�B
VB
Q�B
Q�B
T�B
S�B
Q�B
Q�B
T�B
T�B
W
B
XB
YB
\)B
]/B
[#B
\)B
\)B
^5B
]/B
ZB
XB
[#B
]/B
\)B
[#B
[#B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
`BB
]/B
]/B
\)B
^5B
`BB
`BB
_;B
[#B
^5B
aHB
aHB
aHB
^5B
_;B
]/B
]/B
bNB
aHB
]/B
bNB
ffB
e`B
ffB
ffB
e`B
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
ffB
gmB
gmB
hsB
hsB
iyB
k�B
jB
iyB
gmB
hsB
hsB
jB
jB
iyB
l�B
k�B
jB
jB
l�B
l�B
k�B
iyB
jB
k�B
k�B
l�B
n�B
o�B
m�B
m�B
n�B
o�B
p�B
p�B
n�B
m�B
n�B
p�B
o�B
n�B
p�B
r�B
s�B
r�B
r�B
q�B
o�B
o�B
o�B
n�B
n�B
o�B
r�B
s�B
r�B
s�B
s�B
r�B
s�B
s�B
r�B
q�B
q�B
q�B
s�B
t�B
r�B
s�B
t�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
v�B
t�B
s�B
w�B
x�B
x�B
w�B
t�B
w�B
v�B
u�B
t�B
s�B
u�B
t�B
x�B
y�B
y�B
y�B
x�B
v�B
x�B
z�B
z�B
{�B
{�B
~�B
}�B
~�B
|�B
|�B
{�B
|�B
z�B
{�B
}�B
~�B
~�B
~�B
� B
~�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
� B
|�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�%B
�B
�+B
�%B
�%B
�+B
�1B
�1B
�1B
�%B
�B
�B
�B
�B
�%B
�B
�B
�%B
�+B
�%B
�%B
�+B
�%B
�%B
�+B
�+B
�1B
�+B
�%B
�%B
�+B
�%B
�1B
�1B
�1B
�=B
�DB
�DB
�DB
�DB
�=B
�DB
�DB
�=B
�7B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�\B
�\B
�\B
�\B
�\B
�VB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�oB
�{B
�{B
�{B
�{B
�uB
�uB
�oB
�hB
�bB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
�{B
�uB
�{B
�uB
��B
�{B
�uB
�oB
�{B
��B
��B
�uB
�oB
�\B
�uB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B	DB
��111111111111111111111111111111111111111111111111111111111111144111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141  B	H�B	H�B	G�B	H�B	I�B	I�B	H�B	H�B	H�B	G�B	H�B	I�B	H�B	H�B	I�B	I�B	I�B	H�B	I�B	H�B	H�B	H�B	G�B	J�B	H�B	F�B	E�B	ȴB	�BVB,BO�BVBA�B �BuBA�BI�BK�B!�B
��B+B
�B
��B
�#B
�/B
ɺB
�'B
��B
�B
�B
��B
�B
`BB
B�B
�B
I�B
P�B
VB
Q�B
@�B
�B	�LB	�B	��B	��B	�5B	�DB	49B	uB	+B	<jB	�B	B	�B	Q�B	jB	ZB	�%B	�VB	�=B	��B	��B	�TB	�B	��B
1B
hB
�B
%�B
0!B
1'B
6FB
9XB
E�B
K�B
XB
T�B
^5B
�\B
�oB
��B
��B
��B
�B
�-B
�FB
�XB
�RB
�dB
��B
B
ÖB
��B
��B
ĜB
ĜB
ŢB
��B
ĜB
ƨB
�dB
ƨB
�}B
�wB
�wB
��B
ÖB
B
ƨB
��B
B
B
ÖB
��B
B
��B
��B
�}B
�jB
�dB
�jB
�LB
�3B
�wB
�jB
�LB
�XB
�}B
�wB
�^B
�3B
�B
�LB
�9B
�-B
�3B
�B
��B
��B
�'B
�-B
�-B
�'B
�!B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�\B
�PB
�\B
�bB
�uB
��B
��B
��B
�oB
�{B
�\B
�7B
�+B
�DB
�%B
�1B
�DB
�1B
~�B
t�B
ffB
e`B
jB
s�B
q�B
o�B
l�B
iyB
hsB
ffB
e`B
cTB
bNB
^5B
ZB
YB
^5B
ZB
ZB
YB
\)B
`BB
^5B
[#B
W
B
O�B
N�B
R�B
W
B
ZB
ZB
W
B
S�B
T�B
Q�B
I�B
E�B
D�B
E�B
L�B
N�B
L�B
J�B
G�B
>wB
?}B
K�B
K�B
G�B
C�B
D�B
D�B
E�B
D�B
C�B
A�B
@�B
;dB
7LB
9XB
<jB
=qB
=qB
;dB
<jB
9XB
49B
33B
5?B
33B
2-B
49B
1'B
33B
33B
1'B
.B
-B
0!B
2-B
5?B
6FB
33B
49B
0!B
$�B
/B
/B
+B
%�B
�B
�B
%�B
&�B
'�B
/B
2-B
0!B
.B
(�B
&�B
%�B
�B
bB
1B
�B
�B
!�B
#�B
�B
�B
�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
"�B
"�B
�B
�B
�B
"�B
$�B
#�B
!�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
PB
VB
�B
hB
�B
�B
{B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
DB
+B
VB
�B
�B
�B
�B
{B
oB
�B
�B
�B
�B
�B
oB
bB
bB
\B
hB
{B
uB
bB
PB
�B
�B
�B
�B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
%�B
&�B
&�B
%�B
%�B
%�B
#�B
$�B
"�B
"�B
'�B
+B
(�B
(�B
'�B
#�B
"�B
&�B
'�B
&�B
$�B
&�B
&�B
(�B
!�B
�B
 �B
�B
%�B
,B
,B
(�B
(�B
+B
.B
/B
.B
,B
)�B
)�B
'�B
%�B
.B
0!B
0!B
.B
,B
1'B
2-B
/B
1'B
33B
6FB
5?B
33B
0!B
1'B
33B
2-B
33B
6FB
9XB
7LB
8RB
7LB
<jB
<jB
;dB
9XB
;dB
<jB
;dB
8RB
6FB
33B
1'B
=qB
<jB
=qB
<jB
:^B
;dB
A�B
A�B
A�B
@�B
A�B
A�B
@�B
@�B
C�B
C�B
B�B
B�B
A�B
C�B
C�B
A�B
>wB
=qB
B�B
@�B
?}B
D�B
F�B
H�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
G�B
C�B
C�B
B�B
H�B
I�B
L�B
M�B
L�B
M�B
N�B
L�B
M�B
L�B
L�B
K�B
I�B
G�B
L�B
M�B
L�B
M�B
N�B
L�B
P�B
Q�B
P�B
N�B
N�B
M�B
N�B
N�B
P�B
P�B
Q�B
P�B
P�B
O�B
N�B
R�B
S�B
S�B
S�B
S�B
Q�B
P�B
Q�B
VB
S�B
R�B
W
B
W
B
W
B
VB
T�B
T�B
S�B
Q�B
T�B
VB
Q�B
Q�B
T�B
S�B
Q�B
Q�B
T�B
T�B
W
B
XB
YB
\)B
]/B
[#B
\)B
\)B
^5B
]/B
ZB
XB
[#B
]/B
\)B
[#B
[#B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
`BB
]/B
]/B
\)B
^5B
`BB
`BB
_;B
[#B
^5B
aHB
aHB
aHB
^5B
_;B
]/B
]/B
bNB
aHB
]/B
bNB
ffB
e`B
ffB
ffB
e`B
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
ffB
gmB
gmB
hsB
hsB
iyB
k�B
jB
iyB
gmB
hsB
hsB
jB
jB
iyB
l�B
k�B
jB
jB
l�B
l�B
k�B
iyB
jB
k�B
k�B
l�B
n�B
o�B
m�B
m�B
n�B
o�B
p�B
p�B
n�B
m�B
n�B
p�B
o�B
n�B
p�B
r�B
s�B
r�B
r�B
q�B
o�B
o�B
o�B
n�B
n�B
o�B
r�B
s�B
r�B
s�B
s�B
r�B
s�B
s�B
r�B
q�B
q�B
q�B
s�B
t�B
r�B
s�B
t�B
t�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
v�B
t�B
s�B
w�B
x�B
x�B
w�B
t�B
w�B
v�B
u�B
t�B
s�B
u�B
t�B
x�B
y�B
y�B
y�B
x�B
v�B
x�B
z�B
z�B
{�B
{�B
~�B
}�B
~�B
|�B
|�B
{�B
|�B
z�B
{�B
}�B
~�B
~�B
~�B
� B
~�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
� B
|�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�%B
�B
�+B
�%B
�%B
�+B
�1B
�1B
�1B
�%B
�B
�B
�B
�B
�%B
�B
�B
�%B
�+B
�%B
�%B
�+B
�%B
�%B
�+B
�+B
�1B
�+B
�%B
�%B
�+B
�%B
�1B
�1B
�1B
�=B
�DB
�DB
�DB
�DB
�=B
�DB
�DB
�=B
�7B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�\B
�\B
�\B
�\B
�\B
�VB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�oB
�{B
�{B
�{B
�{B
�uB
�uB
�oB
�hB
�bB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
�{B
�uB
�{B
�uB
��B
�{B
�uB
�oB
�{B
��B
��B
�uB
�oB
�\B
�uB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B	DB
��111111111111111111111111111111111111111111111111111111111111144111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220225080106                              AO  ARCAADJP                                                                    20220225080106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220225080106  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220225080106  QCF$                G�O�G�O�G�O�4000            