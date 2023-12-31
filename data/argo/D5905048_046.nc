CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-09T00:35:42Z creation;2016-10-09T00:35:44Z conversion to V3.1;2019-12-19T08:24:40Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161009003542  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA  I2_0577_046                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @������ 1   @�����@3�i�B���d׮�1��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@�Q�@��AA<(�A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�B��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D;�qD<w
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8RD�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��RD���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D��RD�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�>�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A��A���A��A��A��TA��A��
A��A��A��
A��
A���A���A�Aܲ-A�|�A�%A�ȴA�x�A��A���A�VA��A���A��A��A�A�=qA�33A��A��HAׁA��HAթ�A�n�Aӗ�A�5?A�33A�t�A͑hA��HA���A�ƨA���A�$�A�;dA�Q�A��yA�=qA��/A��mA�A�t�A�XA�ȴA�p�A���A�r�A���A�5?A��A��A��A��A��#A�~�A���A���A�^5A��A��/A��#A�
=A��uA�?}A��A��PA�7LA�5?A�oA��TA���A��DA��`A��A�r�A�oA��PA�~�A��FA�;dA�XA���A�%A�bNA�XA� �A��jA��PA���A��!A�-A��A�A���A�  A��+A�^5A���A�K�A�S�A�dZA���A���A�-A��TA���A�1'A���A�I�A��#A�E�A�`BA�;dAVA|Q�Az��Ay`BAw�hAs��Aqt�An�RAkK�Ag�Af��Ac?}A`�A_��A]�7AZĜAX��AX$�AWK�AV�/AU��ATAR��AR1AQ\)AP�AN�ALr�AJ�AH�/AE�#AC;dAA��A@A�A<�A;7LA8�A6��A5/A2�A1�
A0��A/�FA.�9A.ZA-�A-��A, �A)��A'K�A$�HA$v�A$(�A"�jA"A!A!l�A ��A   A�FAhsAjA��A`BA�AbNAC�A�^A|�AK�A��A�\A�A{AjA��AA��AbNA�A��A^5A`BA�/A$�AXA
��A	�;A	XA�yA��A��AQ�AdZA�A �A��A M�@�C�@�n�@�/@��P@�@�l�@��T@�\)@�@�@�+@�/@�r�@�"�@��@蛦@�w@���@�ff@�$�@��@�?}@�@�  @��@�dZ@�@߶F@�
=@��@��@��T@ݡ�@��@�^5@���@���@٩�@٩�@�p�@أ�@׾w@׍P@�
=@֏\@պ^@Ԭ@�t�@�E�@�x�@�A�@Ϯ@�K�@�;d@�K�@Ο�@�V@�J@́@���@�{@ȣ�@�Q�@Ǿw@�
=@�~�@�V@�@ř�@�Ĝ@�9X@Å@�@�n�@�$�@��#@���@��@�r�@� �@��F@���@�x�@�V@�Ĝ@��u@�1@�l�@�|�@�;d@�J@���@���@��h@�/@�A�@�ƨ@��R@�-@���@���@��@�z�@�Z@�Q�@�I�@��m@��!@�V@�-@�V@�@�X@��@��j@��@���@�"�@��H@���@��#@��-@�?}@��@�%@���@���@�bN@�Z@�9X@�1@��@�
=@��@��+@�M�@�5?@��-@�O�@��`@�Ĝ@���@���@�b@���@�+@��@���@�~�@�M�@�$�@�{@���@�@��h@�/@��/@�(�@�
=@��\@�V@�@��@�-@�J@�=q@��^@��@��`@��`@��/@��9@���@� �@��@�A�@�bN@�bN@�1@��w@���@�C�@�"�@���@�J@�hs@���@�Z@�I�@�A�@�I�@�b@���@�@�V@��@�O�@���@�z�@��
@�\)@�S�@�o@�o@��\@���@��+@�=q@��-@���@��h@���@���@�X@�V@��@��j@���@���@�V@���@��@��/@��j@���@�  @��@�K�@�33@�+@��@��@���@��@���@��@��H@��@��+@�5?@���@��h@�G�@�?}@�7L@�7L@��@��D@�9X@�(�@�  @��m@���@�dZ@�S�@�;d@�"�@�o@��+@��@���@���@���@��^@�G�@��/@��@��u@�r�@�I�@��;@�C�@�dZ@�
=@��\@��\@�~�@�n�@�=q@�J@�$�@�J@�@��@��#@��h@���@��`@��/@�Ĝ@�bN@�I�@�9X@���@�t�@�;d@���@�n�@�E�@�@�J@�@�O�@��j@��D@�9X@��@��w@�dZ@�K�@�;d@�
=@��!@�n�@�V@�=q@��@�@���@�x�@�X@�?}@��@��9@�j@�9X@�  @�;@K�@~�R@~{@}�-@}O�@|��@|(�@{"�@z^5@z�@yX@xĜ@w�;@w�P@v��@v{@u��@u�h@u?}@t�/@tj@s�m@s�@so@r��@rM�@q�@q��@q��@q�7@qX@pĜ@o��@o;d@o+@o;d@oK�@o�@n�y@n�+@m�T@m��@m�-@m`B@l�@l��@lj@l9X@k��@kdZ@ko@j�!@j~�@jM�@jJ@i��@iX@i�@h�`@h�9@h�u@g��@g
=@f��@f��@f��@e�@e�-@e�@dj@d(�@d(�@d�@d�@d�@d1@b��@a�@a��@aX@a&�@a%@`�`@`�@_�;@_+@^�R@^v�@]�@]�T@]��@]@]`B@\�j@\Z@[��@[dZ@Z��@Z��@Z~�@Z=q@Z�@Y��@YX@X��@X�9@X��@XbN@Wl�@W
=@W
=@Vȴ@V�+@V{@V@U�@U��@UO�@U�@T�/@T�D@T�@S�
@S�@SS�@R�@R�!@R^5@R=q@R�@Q�#@Q��@Qx�@P�@Pb@O�;@O�;@O��@O��@O\)@O
=@N�y@N�@N��@NV@M�@M�@LI�@L�@L�@K�m@K��@K��@KS�@K"�@K@J�H@J��@J=q@Ihs@H�u@H1'@G�@G�P@G+@Fȴ@Fv�@Fff@FV@F@E�-@EO�@D��@Dj@C�m@C"�@B�H@B�!@Bn�@B=q@A�@Ahs@A�@@�u@@1'@?�;@?��@?K�@>��@>ff@=��@=�@<�@<9X@;��@;��@:�!@9��@8�@7�;@7�w@7�@7��@7\)@7+@6ȴ@6{@5��@5�@4��@4�j@4I�@4�@3ƨ@3C�@3@2~�@2-@1�@1��@1��@1G�@0�9@01'@0b@/��@/|�@/;d@/
=@.�R@.ff@.5?@-�@-�@-`B@-/@,�/@,��@,I�@,1@+�F@+t�@+C�@+@*��@*�!@*��@)��@)x�@)&�@(��@(��@(�@(r�@(r�@(bN@(Q�@(b@(  @'�@'��@'�w@'��@'l�@&��@&V@&5?@&{@&{@%�@%�@%��@%�@$��@$�D@$j@$�@#�
@#��@#�@#�@#t�@#t�@#S�@#C�@#@"n�@!��@!�@ Ĝ@ ��@ 1'@   @��@|�@\)@;d@;d@��@�@�R@��@�+@V@E�@5?@{@�@��@@�h@��@�@�@�/@�/@��@��@Z@�@�m@��@dZ@S�@C�@"�@��@��@~�@~�@^5@�@��@hs@�@bN@ �@�@�;@��@�w@�@��@�P@;d@�@
=@��@ȴ@�+@5?@$�@@�T@��@�h@`B@O�@/@/@�@�/@��@Z@�@1@��@1@1@��@�
@��@�@C�@@��@�!@��@^5@�@�@�^@��@��@�7@x�@hs@G�@�@��@r�@b@�@�;@�w@|�@\)@K�@K�@+@
=@��@��@��@�y@�y@�y@�@�R@��@ff@V@E�@5?@$�@{@@��@�@O�@/@�@��@�@�@��@z�@z�@I�@�@�
@��@dZ@33@33@"�@
��@
~�@
n�@
^5@
^5@
^5@
^5@
^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A��A���A��A��A��TA��A��
A��A��A��
A��
A���A���A�Aܲ-A�|�A�%A�ȴA�x�A��A���A�VA��A���A��A��A�A�=qA�33A��A��HAׁA��HAթ�A�n�Aӗ�A�5?A�33A�t�A͑hA��HA���A�ƨA���A�$�A�;dA�Q�A��yA�=qA��/A��mA�A�t�A�XA�ȴA�p�A���A�r�A���A�5?A��A��A��A��A��#A�~�A���A���A�^5A��A��/A��#A�
=A��uA�?}A��A��PA�7LA�5?A�oA��TA���A��DA��`A��A�r�A�oA��PA�~�A��FA�;dA�XA���A�%A�bNA�XA� �A��jA��PA���A��!A�-A��A�A���A�  A��+A�^5A���A�K�A�S�A�dZA���A���A�-A��TA���A�1'A���A�I�A��#A�E�A�`BA�;dAVA|Q�Az��Ay`BAw�hAs��Aqt�An�RAkK�Ag�Af��Ac?}A`�A_��A]�7AZĜAX��AX$�AWK�AV�/AU��ATAR��AR1AQ\)AP�AN�ALr�AJ�AH�/AE�#AC;dAA��A@A�A<�A;7LA8�A6��A5/A2�A1�
A0��A/�FA.�9A.ZA-�A-��A, �A)��A'K�A$�HA$v�A$(�A"�jA"A!A!l�A ��A   A�FAhsAjA��A`BA�AbNAC�A�^A|�AK�A��A�\A�A{AjA��AA��AbNA�A��A^5A`BA�/A$�AXA
��A	�;A	XA�yA��A��AQ�AdZA�A �A��A M�@�C�@�n�@�/@��P@�@�l�@��T@�\)@�@�@�+@�/@�r�@�"�@��@蛦@�w@���@�ff@�$�@��@�?}@�@�  @��@�dZ@�@߶F@�
=@��@��@��T@ݡ�@��@�^5@���@���@٩�@٩�@�p�@أ�@׾w@׍P@�
=@֏\@պ^@Ԭ@�t�@�E�@�x�@�A�@Ϯ@�K�@�;d@�K�@Ο�@�V@�J@́@���@�{@ȣ�@�Q�@Ǿw@�
=@�~�@�V@�@ř�@�Ĝ@�9X@Å@�@�n�@�$�@��#@���@��@�r�@� �@��F@���@�x�@�V@�Ĝ@��u@�1@�l�@�|�@�;d@�J@���@���@��h@�/@�A�@�ƨ@��R@�-@���@���@��@�z�@�Z@�Q�@�I�@��m@��!@�V@�-@�V@�@�X@��@��j@��@���@�"�@��H@���@��#@��-@�?}@��@�%@���@���@�bN@�Z@�9X@�1@��@�
=@��@��+@�M�@�5?@��-@�O�@��`@�Ĝ@���@���@�b@���@�+@��@���@�~�@�M�@�$�@�{@���@�@��h@�/@��/@�(�@�
=@��\@�V@�@��@�-@�J@�=q@��^@��@��`@��`@��/@��9@���@� �@��@�A�@�bN@�bN@�1@��w@���@�C�@�"�@���@�J@�hs@���@�Z@�I�@�A�@�I�@�b@���@�@�V@��@�O�@���@�z�@��
@�\)@�S�@�o@�o@��\@���@��+@�=q@��-@���@��h@���@���@�X@�V@��@��j@���@���@�V@���@��@��/@��j@���@�  @��@�K�@�33@�+@��@��@���@��@���@��@��H@��@��+@�5?@���@��h@�G�@�?}@�7L@�7L@��@��D@�9X@�(�@�  @��m@���@�dZ@�S�@�;d@�"�@�o@��+@��@���@���@���@��^@�G�@��/@��@��u@�r�@�I�@��;@�C�@�dZ@�
=@��\@��\@�~�@�n�@�=q@�J@�$�@�J@�@��@��#@��h@���@��`@��/@�Ĝ@�bN@�I�@�9X@���@�t�@�;d@���@�n�@�E�@�@�J@�@�O�@��j@��D@�9X@��@��w@�dZ@�K�@�;d@�
=@��!@�n�@�V@�=q@��@�@���@�x�@�X@�?}@��@��9@�j@�9X@�  @�;@K�@~�R@~{@}�-@}O�@|��@|(�@{"�@z^5@z�@yX@xĜ@w�;@w�P@v��@v{@u��@u�h@u?}@t�/@tj@s�m@s�@so@r��@rM�@q�@q��@q��@q�7@qX@pĜ@o��@o;d@o+@o;d@oK�@o�@n�y@n�+@m�T@m��@m�-@m`B@l�@l��@lj@l9X@k��@kdZ@ko@j�!@j~�@jM�@jJ@i��@iX@i�@h�`@h�9@h�u@g��@g
=@f��@f��@f��@e�@e�-@e�@dj@d(�@d(�@d�@d�@d�@d1@b��@a�@a��@aX@a&�@a%@`�`@`�@_�;@_+@^�R@^v�@]�@]�T@]��@]@]`B@\�j@\Z@[��@[dZ@Z��@Z��@Z~�@Z=q@Z�@Y��@YX@X��@X�9@X��@XbN@Wl�@W
=@W
=@Vȴ@V�+@V{@V@U�@U��@UO�@U�@T�/@T�D@T�@S�
@S�@SS�@R�@R�!@R^5@R=q@R�@Q�#@Q��@Qx�@P�@Pb@O�;@O�;@O��@O��@O\)@O
=@N�y@N�@N��@NV@M�@M�@LI�@L�@L�@K�m@K��@K��@KS�@K"�@K@J�H@J��@J=q@Ihs@H�u@H1'@G�@G�P@G+@Fȴ@Fv�@Fff@FV@F@E�-@EO�@D��@Dj@C�m@C"�@B�H@B�!@Bn�@B=q@A�@Ahs@A�@@�u@@1'@?�;@?��@?K�@>��@>ff@=��@=�@<�@<9X@;��@;��@:�!@9��@8�@7�;@7�w@7�@7��@7\)@7+@6ȴ@6{@5��@5�@4��@4�j@4I�@4�@3ƨ@3C�@3@2~�@2-@1�@1��@1��@1G�@0�9@01'@0b@/��@/|�@/;d@/
=@.�R@.ff@.5?@-�@-�@-`B@-/@,�/@,��@,I�@,1@+�F@+t�@+C�@+@*��@*�!@*��@)��@)x�@)&�@(��@(��@(�@(r�@(r�@(bN@(Q�@(b@(  @'�@'��@'�w@'��@'l�@&��@&V@&5?@&{@&{@%�@%�@%��@%�@$��@$�D@$j@$�@#�
@#��@#�@#�@#t�@#t�@#S�@#C�@#@"n�@!��@!�@ Ĝ@ ��@ 1'@   @��@|�@\)@;d@;d@��@�@�R@��@�+@V@E�@5?@{@�@��@@�h@��@�@�@�/@�/@��@��@Z@�@�m@��@dZ@S�@C�@"�@��@��@~�@~�@^5@�@��@hs@�@bN@ �@�@�;@��@�w@�@��@�P@;d@�@
=@��@ȴ@�+@5?@$�@@�T@��@�h@`B@O�@/@/@�@�/@��@Z@�@1@��@1@1@��@�
@��@�@C�@@��@�!@��@^5@�@�@�^@��@��@�7@x�@hs@G�@�@��@r�@b@�@�;@�w@|�@\)@K�@K�@+@
=@��@��@��@�y@�y@�y@�@�R@��@ff@V@E�@5?@$�@{@@��@�@O�@/@�@��@�@�@��@z�@z�@I�@�@�
@��@dZ@33@33@"�@
��@
~�@
n�@
^5@
^5@
^5@
^5@
^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B"�B"�B#�B#�B$�B%�B%�B&�B'�B(�B,B1'B9XBH�B\)BXB7LB�B�B�B!�B'�B.B$�B(�B2-B8RB@�BN�BbNBn�Bo�BZB;dB5?B0!B2-B'�B'�B(�B9XB)�BG�Bo�B�\B�^B��B�
B�5B�NB�`B�B�B�B�B�BB�B�
B��B��BƨB��B��BĜB��B��B�/B�B��B��B��BȴB��B�!B��B��B��B��B��B�uB� Be`BS�BE�B?}B49B)�B�B��B�sB�B�B�
B��B�B�\By�B^5BK�BH�BYBS�BJ�BF�BA�B-B�BDB  B
�B
�`B
�NB
��B
��B
ŢB
�wB
�LB
�B
��B
�\B
�+B
w�B
m�B
aHB
W
B
<jB
+B
�B	��B	�`B	�#B	ǮB	�FB	�B	��B	�JB	}�B	u�B	l�B	iyB	bNB	W
B	M�B	K�B	L�B	I�B	F�B	6FB	49B	-B	!�B	uB	JB	B��B�B�`B�/B��B��B��BɺBǮBĜBÖBB��B�}B�XB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�JB�B|�Bu�Bt�Bw�B|�Bz�B|�B}�B}�B|�B{�By�Bx�Bv�Bv�Bz�B{�B|�B� B�%B�+B�=B�PB�VB�\B�hB�uB��B��B��B��B��B�B�B�B�3B�9B�9B�RBƨBɺB��B��B��B��B��B�B�B�B�#B�)B�5B�BB�TB�ZB�mB�yB�B�B��B��B��B��B��B	  B		7B	DB	PB	bB	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	(�B	)�B	)�B	,B	/B	0!B	33B	8RB	?}B	@�B	C�B	E�B	G�B	I�B	N�B	R�B	O�B	P�B	Q�B	Q�B	R�B	W
B	XB	\)B	_;B	bNB	dZB	gmB	iyB	p�B	u�B	u�B	u�B	v�B	y�B	� B	�B	�%B	�=B	�VB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�9B	�FB	�LB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	��B	��B	��B	ÖB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�yB	�sB	�fB	�`B	�ZB	�`B	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB

=B

=B
	7B
1B
+B
+B
	7B

=B

=B
DB
JB
JB
DB
DB
DB
DB
DB
JB
DB
DB
VB
VB
VB
bB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
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
:^B
;dB
;dB
;dB
<jB
;dB
<jB
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
>wB
>wB
?}B
?}B
?}B
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
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
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B"�B"�B#�B#�B$�B%�B&B'B(
B)_B,�B2-B:BI�B]IBZ7B8�BB�B�B!�B(sB/5B%B)yB2�B9rBB[BQ�Bd�Bq'BtB]�B=<B7B1�B3�B($B(�B+6B<�B,�BJ�Br�B�:B�6B�9BٚB��B�B�$B�B��B�B�)B��B�WBؓB��B��BˬB�bBуB��BðB�B��B��B�B��B��B��B��B��B�nB�-B�;B��B��B�
B�MBh�BW$BHBAUB5�B,�BWB B��BڠB�B�1B�oB��B�&B~�Ba�BMBI�B[=BT�BK�BH�BD�B/iB�BBB
��B
�B
�B
�2B
�"B
��B
��B
��B
�}B
�tB
��B
�=B
zB
o�B
d@B
[WB
?�B
.�B
�B	��B	��B	��B	�rB	�lB	� B	��B	�pB	B	v�B	m�B	kB	dZB	X�B	N�B	L�B	N"B	L~B	I�B	8�B	72B	0�B	$�B	�B	BB		7B��B�B�
BߤBרB�oB�6B�DB��B�SB�MBðB��B��B��B��B��B��B��B��B�bB��B��B��B�bB��B�B�~B��B��B��B�]B�EB�(B�VB�HB�vB��B�B��B��B�eB�EB�+B�~B�ZB��B�6B��B�B�B��B�-B�xB��B�9B��B��B�aB~(Bv�Bu�ByrB}�B{�B}�BHBcB~�B}"B{By�Bw�Bx�B{�B|�B}�B�UB��B��B��B��B��B��B��B�B�B�B�CB��B�>B��B�}B��B��B��B�%B��B��B�	B��B�B�JBΊBԕB�_BؓBںB��B�B�!B�-B�B�FB��B��B�B��B�FB�B�RB�B��B	�B	
#B	�B	�B	 B	�B	�B	�B	
B	7B	/B	;B	!HB	#B	'8B	)DB	*KB	*B	,�B	/�B	0�B	4B	9>B	?�B	@�B	C�B	F%B	H1B	J	B	O\B	S�B	PHB	Q B	R:B	RoB	S�B	W�B	X�B	\�B	_�B	b�B	d�B	g�B	i�B	p�B	u�B	vFB	v�B	v�B	z*B	�4B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	�-B	��B	�B	�8B	�$B	�KB	�=B	�=B	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	żB	��B	��B	��B	��B	��B	�B	�B	�(B	�BB	�bB	ѝB	ҽB	�NB	�(B	�.B	�B	�B	�2B	�?B	ٚB	چB	�QB	�CB	�CB	�]B	�xB	ܒB	�dB	�B	�ZB	�B	��B	��B	�B	��B	��B	�B	�!B	�;B	�;B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�tB	�B	�B	��B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�.B
 4B
 iB	�cB	�(B	�(B	�(B	�.B	�B	�B	�B	�.B
 B
B
MB
�B
{B
aB
GB
aB
?B
KB
KB
�B
�B
	�B
	lB
	RB
	RB
	lB
	�B

rB

rB
xB

rB

�B
	�B
fB
EB
+B
	lB

�B

�B
xB
~B
dB
�B
�B
�B
^B
�B
�B
^B
xB
�B
�B
pB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
B
�B
�B
B
B
B
;B
!B
B
!B
B
B
B
 B
 'B
�B
�B
�B
 B
!B
 �B
!B
"B
!�B
"B
#B
"�B
"�B
#B
#B
#:B
$@B
%B
$�B
$�B
%�B
'B
'B
'B
'8B
'B
($B
($B
(>B
($B
($B
)B
)B
)DB
*0B
*B
*0B
+6B
+6B
+6B
,=B
,"B
,=B
,=B
,=B
,WB
-CB
-CB
-)B
-]B
.cB
/5B
/iB
/iB
0;B
0!B
0;B
0;B
0UB
0UB
0�B
1vB
2aB
2aB
2aB
3MB
3MB
3�B
3�B
3�B
4�B
4TB
5tB
5ZB
5ZB
5tB
5�B
5�B
6�B
6zB
7�B
7�B
7fB
7�B
8�B
7fB
7�B
8lB
8�B
8�B
8lB
8�B
8�B
9�B
9rB
9�B
9�B
:xB
:xB
:�B
:xB
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>wB
>�B
>�B
>�B
?�B
?�B
?�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
IB
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
LB
LB
MB
MB
MB
NB
N"B
NVB
O\B
O\B
PB
Q B
Q B
QB
Q B
Q B
QB
R B
RB
R B
S&B
SB
S@B
T,B
T,B
T,B
U2B
UMB
UB
V9B
VB
V9B
VSB
VSB
W?B
XEB
X+B
XEB
X+B
YKB
YKB
Y1B
ZQB
ZQB
Z7B
Z7B
ZQB
[=B
[WB
[WB
\]B
\]B
\CB
\]B
\]B
]IB
]IB
]dB
]dB
]dB
^jB
^OB
^jB
_pB
_VB
_;B
_;B
_pB
_VB
_VB
_VB
_pB
_VB
`vB
`vB
`�B
a|B
a|B
a|B
aHB
a|B
aHB
abB
a|B
b�B
b�B
cnB
cnB
c�B
c�B
dtB
dtB
dtB
cTB
c�B
dtB
d�B
d�B
d�B
e�B
e�B
ezB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
gmB
g�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
iyB
j�B
jB
j�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
w�B
w�B
x�B
x�B
x�B
y	B
y	B
y	B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
z�B
y�B
{B
z�B
{B
{B
{B
z�B
{B
{B
|B
{�B
|B
|B
|B
|B
{�B
}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610180052192016101800521920161018005219201806221303182018062213031820180622130318201804050702542018040507025420180405070254  JA  ARFMdecpA19c                                                                20161009093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161009003542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161009003542  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161009003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161009003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161009003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161009003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161009003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161009003544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161009003544                      G�O�G�O�G�O�                JA  ARUP                                                                        20161009011927                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161009153823  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161017155219  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161017155219  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220254  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040318  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                