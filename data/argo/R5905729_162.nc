CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-10-03T09:01:25Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20221003090125  20221003090125  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��T��P1   @��UDDN�@*�+I��dwn��P1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BO��BXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ DӼ�D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@�Q�A(�A=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7�
B?p�BGp�BO
>BW�
B_
>Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB��B��BӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
DQ}pDQ�
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
Dm�pDnw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�DӸRD���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�>�D�{�D㻅D���D�;�D�~�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D���D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aߏ\Aߗ�Aߗ�Aߙ�Aߗ�Aߗ�Aߗ�Aߗ�Aߕ�A߬A߰!Aߩ�Aߟ�Aߡ�A߉7A�
=Aޗ�A�^5A�K�A�ȴA܅A�1A۾wA��
Aه+AՍPA��
A��A��Aɗ�AȾwAǏ\A�&�A���A���A¬A��mA��A��A�;dA�  A�(�A�ZA�l�A��/A���A��A��A��A��yA��A�&�A��FA�VA��A�hsA���A�v�A���A��RA��A�bNA���A��-A�+A��A��A�hsA���A�A�E�A|�jAt �Aop�AlJAg��Ac�A`z�A^r�A\�\AZ  AW;dAS��AQx�AN��AL��AKoAH�jAD�HAB�+A@ȴA?A=��A<��A<(�A;\)A9��A7|�A4ĜA4-A4^5A3��A2�9A2�+A2jA2^5A2�A1K�A1�A0��A0��A/�#A.�/A.n�A.E�A.1'A.�A.A-�mA-��A,�9A*{A((�A'�A&��A&n�A&$�A%�PA%%A$bNA"�DA ĜA^5A{An�A�A ��A!A!�7A �A ^5A��A�A��A5?AbA�A�A�jA�DA~�AffA1AK�A%A�DA��A�RA�mA��A`BA/A�A$�A��AXA��A �A�
AdZA
=A�/A�9AffA��At�A\)A&�A��A��An�A�TAdZAAĜAv�A(�A��Al�A�A��AĜA-A��A�-At�A%A
��A
��A
�\A
z�A
jA
M�A
-A	��A	+A	AĜA��Az�AffA�AG�A
=A�`A�jAn�A1A�-Al�AVA�9A�!A�AZA(�A��At�AA��AbNA��A��AO�AA ��A VA I�A   @�33@�ȴ@�n�@���@�p�@���@��@�(�@�b@���@�|�@��@��@�Q�@��
@���@�K�@��@�`B@�&�@���@�Z@�  @�33@�ȴ@�5?@�X@�%@���@���@��D@��@�;d@��#@�r�@띲@�@��y@��H@�R@�ff@�J@��@�b@畁@�33@�p�@��@�t�@�K�@��@�E�@�O�@���@�b@޸R@�=q@���@�  @��y@�{@�&�@�Ĝ@�(�@ׅ@��y@�E�@�`B@�I�@��@�"�@�{@��@Ь@�A�@���@�+@��H@�$�@�?}@̼j@�z�@�A�@��m@�dZ@�o@ʰ!@���@�x�@���@Ȭ@�1'@��@�b@Ǖ�@�33@��@��@Ƨ�@�-@š�@�z�@�bN@�1@�|�@�@�$�@���@���@�?}@�7L@�/@��j@�S�@�ȴ@���@��+@�ff@�M�@���@��#@��-@��T@�O�@��u@���@��@���@��y@�ȴ@���@�n�@���@��h@��@��D@�I�@���@��@��@�5?@��-@�G�@��/@�Z@�Q�@�I�@�1'@�b@���@�ƨ@�\)@�+@��y@���@��+@�M�@�J@�/@�r�@��m@��w@��@�"�@���@�5?@�@�/@��9@��@�Z@��@�ƨ@�\)@���@��@��R@���@��+@�{@��7@�/@��9@�b@��@�;d@���@���@�n�@�V@�=q@��@��@���@���@���@�x�@�G�@��`@��u@�(�@���@�S�@�;d@���@�^5@�J@���@��^@�G�@�V@��@�Q�@�I�@�b@�l�@��@�@���@�-@�{@���@��^@���@��7@�p�@�X@���@�z�@�(�@��m@��w@���@�l�@�+@��+@�M�@�J@��@��^@��-@���@���@�X@��`@��@�j@�A�@��;@���@�dZ@�"�@���@���@�v�@�=q@�J@���@��-@���@�X@�/@���@��j@�I�@��@���@��@�t�@�+@��y@���@�ff@���@��7@�V@���@��u@�(�@��F@�|�@�\)@�+@�"�@�@���@�v�@�M�@�@���@��@��`@��@� �@��;@�ƨ@��@�t�@��\@�5?@�{@��@���@���@�z�@��m@���@�K�@�@���@�ff@�-@�@��@�@�x�@�&�@��`@��@�Z@�ƨ@���@�t�@�@���@���@�V@�J@���@���@�O�@��@�%@��@�Ĝ@�z�@�1'@�  @��@\)@~�y@~��@~�+@}@}p�@}V@|��@{�F@z�@z~�@z^5@y�^@x��@x�u@w�;@w|�@wK�@w+@w�@v��@v�R@v��@vv�@v{@u�@u`B@u/@uV@t��@t�@t�/@t�j@t��@t��@tz�@tz�@tZ@t1@s�m@s�F@s��@st�@sC�@s"�@s@r�@r��@r=q@q��@q��@q��@qG�@p��@pQ�@p  @o��@o+@o
=@n��@n�R@n��@n��@n��@n�+@n$�@m��@m�-@mp�@m�@l��@l�@k�
@kt�@j�!@j^5@i��@iX@h�`@h �@g��@f�+@e��@e�h@d�@dz�@cƨ@c33@c@b�!@b~�@b�@a�7@a7L@a�@a%@`�`@`�u@_�w@_l�@_+@^�@^�+@^5?@]��@]��@]�-@]`B@\�@\��@\9X@[t�@[S�@["�@Z�H@ZJ@Y�7@Y%@XĜ@W�@V��@Vȴ@V�R@V��@V��@V��@V�+@Vv�@VV@V5?@V{@U��@U�@T��@T9X@S�@R�!@Rn�@R^5@R^5@RJ@Q�#@Q��@Q7L@PA�@Ol�@O\)@O�@NV@N$�@M��@M/@L�@LI�@L1@Kƨ@J��@Jn�@J^5@J-@I��@I�^@Ix�@I7L@G\)@F��@F��@FV@E�h@D�j@D�D@Dj@DZ@DI�@D9X@D9X@D(�@D1@C�
@C�@BJ@A�@@��@@r�@@  @?��@?l�@?�@>��@>�y@>��@>ff@>$�@=�@<�@<I�@;ƨ@;33@:��@:n�@:J@9��@9�#@9�7@9G�@9�@8��@8�@8Q�@8Q�@8A�@8 �@7�@7|�@7|�@7l�@7+@6��@6�y@6�R@6v�@6ff@6ff@6E�@65?@6@5�T@5��@5/@4j@4�@3�F@3C�@3o@2�!@2J@1�7@0 �@/�w@/;d@.�@.��@.E�@-�T@-@-�-@-O�@,��@,��@,�j@,(�@,1@+��@+��@+o@*�\@*~�@*n�@*M�@*�@)��@)�^@)�7@)hs@)X@)&�@(�9@(�9@(��@(r�@(Q�@'�w@'�P@'�P@';d@&�y@&ȴ@&��@&V@&{@&@%�T@%�-@%�@%?}@$��@$�@$Z@$�@#��@#�
@#�@#"�@#o@"�@"��@"�\@"n�@"^5@"^5@"M�@!�@!��@!x�@!X@!7L@!%@ ��@ ��@ �u@ 1'@ b@�;@��@K�@
=@�@�R@�+@$�@@p�@?}@�@�@�/@��@�@��@�D@j@(�@�m@�
@ƨ@t�@33@�@�H@��@~�@M�@=q@-@��@�^@�7@G�@7L@&�@�`@�u@�@A�@  @�@�@��@��@�w@�@l�@K�@�@�R@��@V@{@@�-@�@�@�/@�@�D@�D@Z@9X@�m@ƨ@��@�@~�@J@�@��@��@�7@X@�`@�@ �@��@|�@|�@l�@K�@�y@�@��@��@�+@ff@V@5?@{@�@�T@@�@?}@�@�@��@z�@j@I�@9X@(�@�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aߏ\Aߗ�Aߗ�Aߙ�Aߗ�Aߗ�Aߗ�Aߗ�Aߕ�A߬A߰!Aߩ�Aߟ�Aߡ�A߉7A�
=Aޗ�A�^5A�K�A�ȴA܅A�1A۾wA��
Aه+AՍPA��
A��A��Aɗ�AȾwAǏ\A�&�A���A���A¬A��mA��A��A�;dA�  A�(�A�ZA�l�A��/A���A��A��A��A��yA��A�&�A��FA�VA��A�hsA���A�v�A���A��RA��A�bNA���A��-A�+A��A��A�hsA���A�A�E�A|�jAt �Aop�AlJAg��Ac�A`z�A^r�A\�\AZ  AW;dAS��AQx�AN��AL��AKoAH�jAD�HAB�+A@ȴA?A=��A<��A<(�A;\)A9��A7|�A4ĜA4-A4^5A3��A2�9A2�+A2jA2^5A2�A1K�A1�A0��A0��A/�#A.�/A.n�A.E�A.1'A.�A.A-�mA-��A,�9A*{A((�A'�A&��A&n�A&$�A%�PA%%A$bNA"�DA ĜA^5A{An�A�A ��A!A!�7A �A ^5A��A�A��A5?AbA�A�A�jA�DA~�AffA1AK�A%A�DA��A�RA�mA��A`BA/A�A$�A��AXA��A �A�
AdZA
=A�/A�9AffA��At�A\)A&�A��A��An�A�TAdZAAĜAv�A(�A��Al�A�A��AĜA-A��A�-At�A%A
��A
��A
�\A
z�A
jA
M�A
-A	��A	+A	AĜA��Az�AffA�AG�A
=A�`A�jAn�A1A�-Al�AVA�9A�!A�AZA(�A��At�AA��AbNA��A��AO�AA ��A VA I�A   @�33@�ȴ@�n�@���@�p�@���@��@�(�@�b@���@�|�@��@��@�Q�@��
@���@�K�@��@�`B@�&�@���@�Z@�  @�33@�ȴ@�5?@�X@�%@���@���@��D@��@�;d@��#@�r�@띲@�@��y@��H@�R@�ff@�J@��@�b@畁@�33@�p�@��@�t�@�K�@��@�E�@�O�@���@�b@޸R@�=q@���@�  @��y@�{@�&�@�Ĝ@�(�@ׅ@��y@�E�@�`B@�I�@��@�"�@�{@��@Ь@�A�@���@�+@��H@�$�@�?}@̼j@�z�@�A�@��m@�dZ@�o@ʰ!@���@�x�@���@Ȭ@�1'@��@�b@Ǖ�@�33@��@��@Ƨ�@�-@š�@�z�@�bN@�1@�|�@�@�$�@���@���@�?}@�7L@�/@��j@�S�@�ȴ@���@��+@�ff@�M�@���@��#@��-@��T@�O�@��u@���@��@���@��y@�ȴ@���@�n�@���@��h@��@��D@�I�@���@��@��@�5?@��-@�G�@��/@�Z@�Q�@�I�@�1'@�b@���@�ƨ@�\)@�+@��y@���@��+@�M�@�J@�/@�r�@��m@��w@��@�"�@���@�5?@�@�/@��9@��@�Z@��@�ƨ@�\)@���@��@��R@���@��+@�{@��7@�/@��9@�b@��@�;d@���@���@�n�@�V@�=q@��@��@���@���@���@�x�@�G�@��`@��u@�(�@���@�S�@�;d@���@�^5@�J@���@��^@�G�@�V@��@�Q�@�I�@�b@�l�@��@�@���@�-@�{@���@��^@���@��7@�p�@�X@���@�z�@�(�@��m@��w@���@�l�@�+@��+@�M�@�J@��@��^@��-@���@���@�X@��`@��@�j@�A�@��;@���@�dZ@�"�@���@���@�v�@�=q@�J@���@��-@���@�X@�/@���@��j@�I�@��@���@��@�t�@�+@��y@���@�ff@���@��7@�V@���@��u@�(�@��F@�|�@�\)@�+@�"�@�@���@�v�@�M�@�@���@��@��`@��@� �@��;@�ƨ@��@�t�@��\@�5?@�{@��@���@���@�z�@��m@���@�K�@�@���@�ff@�-@�@��@�@�x�@�&�@��`@��@�Z@�ƨ@���@�t�@�@���@���@�V@�J@���@���@�O�@��@�%@��@�Ĝ@�z�@�1'@�  @��@\)@~�y@~��@~�+@}@}p�@}V@|��@{�F@z�@z~�@z^5@y�^@x��@x�u@w�;@w|�@wK�@w+@w�@v��@v�R@v��@vv�@v{@u�@u`B@u/@uV@t��@t�@t�/@t�j@t��@t��@tz�@tz�@tZ@t1@s�m@s�F@s��@st�@sC�@s"�@s@r�@r��@r=q@q��@q��@q��@qG�@p��@pQ�@p  @o��@o+@o
=@n��@n�R@n��@n��@n��@n�+@n$�@m��@m�-@mp�@m�@l��@l�@k�
@kt�@j�!@j^5@i��@iX@h�`@h �@g��@f�+@e��@e�h@d�@dz�@cƨ@c33@c@b�!@b~�@b�@a�7@a7L@a�@a%@`�`@`�u@_�w@_l�@_+@^�@^�+@^5?@]��@]��@]�-@]`B@\�@\��@\9X@[t�@[S�@["�@Z�H@ZJ@Y�7@Y%@XĜ@W�@V��@Vȴ@V�R@V��@V��@V��@V�+@Vv�@VV@V5?@V{@U��@U�@T��@T9X@S�@R�!@Rn�@R^5@R^5@RJ@Q�#@Q��@Q7L@PA�@Ol�@O\)@O�@NV@N$�@M��@M/@L�@LI�@L1@Kƨ@J��@Jn�@J^5@J-@I��@I�^@Ix�@I7L@G\)@F��@F��@FV@E�h@D�j@D�D@Dj@DZ@DI�@D9X@D9X@D(�@D1@C�
@C�@BJ@A�@@��@@r�@@  @?��@?l�@?�@>��@>�y@>��@>ff@>$�@=�@<�@<I�@;ƨ@;33@:��@:n�@:J@9��@9�#@9�7@9G�@9�@8��@8�@8Q�@8Q�@8A�@8 �@7�@7|�@7|�@7l�@7+@6��@6�y@6�R@6v�@6ff@6ff@6E�@65?@6@5�T@5��@5/@4j@4�@3�F@3C�@3o@2�!@2J@1�7@0 �@/�w@/;d@.�@.��@.E�@-�T@-@-�-@-O�@,��@,��@,�j@,(�@,1@+��@+��@+o@*�\@*~�@*n�@*M�@*�@)��@)�^@)�7@)hs@)X@)&�@(�9@(�9@(��@(r�@(Q�@'�w@'�P@'�P@';d@&�y@&ȴ@&��@&V@&{@&@%�T@%�-@%�@%?}@$��@$�@$Z@$�@#��@#�
@#�@#"�@#o@"�@"��@"�\@"n�@"^5@"^5@"M�@!�@!��@!x�@!X@!7L@!%@ ��@ ��@ �u@ 1'@ b@�;@��@K�@
=@�@�R@�+@$�@@p�@?}@�@�@�/@��@�@��@�D@j@(�@�m@�
@ƨ@t�@33@�@�H@��@~�@M�@=q@-@��@�^@�7@G�@7L@&�@�`@�u@�@A�@  @�@�@��@��@�w@�@l�@K�@�@�R@��@V@{@@�-@�@�@�/@�@�D@�D@Z@9X@�m@ƨ@��@�@~�@J@�@��@��@�7@X@�`@�@ �@��@|�@|�@l�@K�@�y@�@��@��@�+@ff@V@5?@{@�@�T@@�@?}@�@�@��@z�@j@I�@9X@(�@�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B��B��B��B	
=B	N�B	�yB
.B
;dB
\)B
iyB
m�B
w�B
x�B
�JB
��B
�B
�`BL�B��B�^B��B�;B�yBDBuB�B)�BuB$�B�B�B��B�B��B �B1'B/B-B$�BPB��B��BgmBcTBL�B6FB$�B�B  B
��B
�B
�NB
�XB
�\B
l�B
H�B
1'B
�B

=B	�B	ŢB	�bB	t�B	k�B	YB	J�B	A�B	>wB	33B	$�B	�B	DB	B��B��B��B�B�B�B��B	  B	bB	,B	2-B	2-B	'�B	)�B	+B	W
B	q�B	�=B	�VB	��B	��B	��B	��B	�B	�RB	�LB	�?B	�-B	ƨB	�B	��B	��B	��B	��B	�B	�B	�HB	�#B	�ZB	�B	�B	�B	�B	�`B	�TB	�fB	�)B	��B	ȴB	��B	�BB	�B
B
)�B
.B
1'B
2-B
2-B
0!B
1'B
1'B
5?B
5?B
33B
8RB
9XB
;dB
9XB
7LB
5?B
9XB
8RB
49B
7LB
:^B
?}B
?}B
?}B
?}B
<jB
A�B
E�B
D�B
D�B
I�B
I�B
K�B
M�B
M�B
K�B
J�B
N�B
P�B
O�B
N�B
O�B
K�B
H�B
J�B
K�B
M�B
L�B
K�B
L�B
L�B
L�B
O�B
O�B
K�B
M�B
Q�B
O�B
N�B
P�B
Q�B
R�B
Q�B
Q�B
P�B
O�B
M�B
L�B
O�B
O�B
O�B
O�B
O�B
L�B
I�B
M�B
N�B
M�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
M�B
J�B
I�B
G�B
F�B
E�B
D�B
E�B
C�B
C�B
C�B
B�B
B�B
A�B
C�B
A�B
?}B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
@�B
?}B
<jB
:^B
8RB
5?B
:^B
;dB
9XB
6FB
6FB
9XB
9XB
8RB
8RB
6FB
7LB
6FB
5?B
8RB
9XB
8RB
5?B
49B
0!B
.B
,B
/B
1'B
5?B
6FB
5?B
49B
2-B
/B
-B
/B
,B
&�B
&�B
+B
-B
+B
'�B
$�B
%�B
$�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
oB
�B
uB
\B
hB
uB
uB
uB
bB
oB
\B
hB
oB
{B
{B
{B
uB
{B
{B
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
 �B
 �B
 �B
 �B
�B
!�B
!�B
!�B
"�B
!�B
 �B
�B
�B
!�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
#�B
&�B
&�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
%�B
%�B
&�B
&�B
%�B
'�B
'�B
)�B
)�B
+B
,B
,B
,B
+B
,B
,B
,B
+B
)�B
)�B
(�B
(�B
(�B
+B
,B
+B
)�B
+B
-B
,B
+B
,B
+B
-B
.B
-B
+B
-B
/B
.B
.B
0!B
0!B
1'B
1'B
0!B
0!B
/B
.B
.B
/B
0!B
0!B
0!B
0!B
/B
.B
1'B
1'B
33B
2-B
33B
33B
2-B
0!B
0!B
0!B
2-B
2-B
1'B
1'B
2-B
1'B
2-B
33B
2-B
33B
33B
33B
49B
5?B
33B
49B
49B
33B
33B
49B
5?B
7LB
7LB
5?B
6FB
6FB
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
;dB
:^B
9XB
:^B
9XB
9XB
9XB
:^B
;dB
;dB
:^B
<jB
=qB
<jB
=qB
;dB
=qB
?}B
?}B
>wB
<jB
>wB
?}B
B�B
B�B
B�B
B�B
D�B
D�B
E�B
F�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
J�B
K�B
L�B
N�B
L�B
L�B
N�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
T�B
T�B
T�B
S�B
S�B
R�B
S�B
S�B
R�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
VB
VB
W
B
VB
VB
T�B
VB
W
B
VB
T�B
W
B
W
B
VB
VB
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
ZB
[#B
[#B
ZB
ZB
[#B
]/B
]/B
\)B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
^5B
]/B
^5B
^5B
^5B
]/B
`BB
`BB
_;B
^5B
_;B
_;B
`BB
_;B
_;B
cTB
cTB
cTB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
aHB
bNB
bNB
dZB
e`B
e`B
dZB
cTB
cTB
cTB
bNB
aHB
e`B
dZB
cTB
e`B
dZB
cTB
cTB
dZB
dZB
dZB
bNB
e`B
ffB
e`B
e`B
e`B
e`B
cTB
`BB
e`B
ffB
ffB
dZB
e`B
gmB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
gmB
ffB
cTB
ffB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
k�B
k�B
jB
k�B
k�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
t�B
v�B
v�B
v�B
w�B
v�B
u�B
u�B
s�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
x�B
y�B
y�B
x�B
x�B
x�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
|�B
{�B
{�B
{�B
z�B
{�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
~�B
� B
� B
~�B
~�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�%B
�%B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�=B
�=B
�=B
�=B
�7B
�7B
�7B
�7B
�1B
�=B
�=B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�DB
�=B
�7B
�=B
�DB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�PB
�VB
�VB
�bB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
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
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B��B��B��B	
=B	N�B	�yB
.B
;dB
\)B
iyB
m�B
w�B
x�B
�JB
��B
�B
�`BL�B��B�^B��B�;B�yBDBuB�B)�BuB$�B�B�B��B�B��B �B1'B/B-B$�BPB��B��BgmBcTBL�B6FB$�B�B  B
��B
�B
�NB
�XB
�\B
l�B
H�B
1'B
�B

=B	�B	ŢB	�bB	t�B	k�B	YB	J�B	A�B	>wB	33B	$�B	�B	DB	B��B��B��B�B�B�B��B	  B	bB	,B	2-B	2-B	'�B	)�B	+B	W
B	q�B	�=B	�VB	��B	��B	��B	��B	�B	�RB	�LB	�?B	�-B	ƨB	�B	��B	��B	��B	��B	�B	�B	�HB	�#B	�ZB	�B	�B	�B	�B	�`B	�TB	�fB	�)B	��B	ȴB	��B	�BB	�B
B
)�B
.B
1'B
2-B
2-B
0!B
1'B
1'B
5?B
5?B
33B
8RB
9XB
;dB
9XB
7LB
5?B
9XB
8RB
49B
7LB
:^B
?}B
?}B
?}B
?}B
<jB
A�B
E�B
D�B
D�B
I�B
I�B
K�B
M�B
M�B
K�B
J�B
N�B
P�B
O�B
N�B
O�B
K�B
H�B
J�B
K�B
M�B
L�B
K�B
L�B
L�B
L�B
O�B
O�B
K�B
M�B
Q�B
O�B
N�B
P�B
Q�B
R�B
Q�B
Q�B
P�B
O�B
M�B
L�B
O�B
O�B
O�B
O�B
O�B
L�B
I�B
M�B
N�B
M�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
M�B
J�B
I�B
G�B
F�B
E�B
D�B
E�B
C�B
C�B
C�B
B�B
B�B
A�B
C�B
A�B
?}B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
@�B
?}B
<jB
:^B
8RB
5?B
:^B
;dB
9XB
6FB
6FB
9XB
9XB
8RB
8RB
6FB
7LB
6FB
5?B
8RB
9XB
8RB
5?B
49B
0!B
.B
,B
/B
1'B
5?B
6FB
5?B
49B
2-B
/B
-B
/B
,B
&�B
&�B
+B
-B
+B
'�B
$�B
%�B
$�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
oB
�B
uB
\B
hB
uB
uB
uB
bB
oB
\B
hB
oB
{B
{B
{B
uB
{B
{B
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
 �B
 �B
 �B
 �B
�B
!�B
!�B
!�B
"�B
!�B
 �B
�B
�B
!�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
#�B
&�B
&�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
%�B
%�B
&�B
&�B
%�B
'�B
'�B
)�B
)�B
+B
,B
,B
,B
+B
,B
,B
,B
+B
)�B
)�B
(�B
(�B
(�B
+B
,B
+B
)�B
+B
-B
,B
+B
,B
+B
-B
.B
-B
+B
-B
/B
.B
.B
0!B
0!B
1'B
1'B
0!B
0!B
/B
.B
.B
/B
0!B
0!B
0!B
0!B
/B
.B
1'B
1'B
33B
2-B
33B
33B
2-B
0!B
0!B
0!B
2-B
2-B
1'B
1'B
2-B
1'B
2-B
33B
2-B
33B
33B
33B
49B
5?B
33B
49B
49B
33B
33B
49B
5?B
7LB
7LB
5?B
6FB
6FB
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
;dB
:^B
9XB
:^B
9XB
9XB
9XB
:^B
;dB
;dB
:^B
<jB
=qB
<jB
=qB
;dB
=qB
?}B
?}B
>wB
<jB
>wB
?}B
B�B
B�B
B�B
B�B
D�B
D�B
E�B
F�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
J�B
K�B
L�B
N�B
L�B
L�B
N�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
T�B
T�B
T�B
S�B
S�B
R�B
S�B
S�B
R�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
VB
VB
W
B
VB
VB
T�B
VB
W
B
VB
T�B
W
B
W
B
VB
VB
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
ZB
[#B
[#B
ZB
ZB
[#B
]/B
]/B
\)B
[#B
[#B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
^5B
]/B
^5B
^5B
^5B
]/B
`BB
`BB
_;B
^5B
_;B
_;B
`BB
_;B
_;B
cTB
cTB
cTB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
aHB
bNB
bNB
dZB
e`B
e`B
dZB
cTB
cTB
cTB
bNB
aHB
e`B
dZB
cTB
e`B
dZB
cTB
cTB
dZB
dZB
dZB
bNB
e`B
ffB
e`B
e`B
e`B
e`B
cTB
`BB
e`B
ffB
ffB
dZB
e`B
gmB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
gmB
ffB
cTB
ffB
iyB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
k�B
k�B
jB
k�B
k�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
s�B
r�B
s�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
t�B
v�B
v�B
v�B
w�B
v�B
u�B
u�B
s�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
x�B
y�B
y�B
x�B
x�B
x�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
z�B
z�B
|�B
{�B
{�B
{�B
z�B
{�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
~�B
� B
� B
~�B
~�B
�B
�B
� B
�B
�B
�B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�%B
�%B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�=B
�=B
�=B
�=B
�7B
�7B
�7B
�7B
�1B
�=B
�=B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�DB
�=B
�7B
�=B
�DB
�PB
�PB
�PB
�PB
�PB
�JB
�JB
�PB
�VB
�VB
�bB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
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
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221003090125                              AO  ARCAADJP                                                                    20221003090125    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221003090125  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221003090125  QCF$                G�O�G�O�G�O�4000            