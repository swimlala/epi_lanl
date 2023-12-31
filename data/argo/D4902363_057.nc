CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-14T00:35:26Z creation;2016-11-14T00:35:28Z conversion to V3.1;2019-12-19T08:25:32Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161114003526  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA  I2_0576_057                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�����v�1   @����[�@:�Y��|��d����$t1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A���A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�C3D��3D�� D�3D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @7
=@w
=@��@��AA<(�A]A}A��HA��HA��HA��AϮA߮A��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
DS�qDTw
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
D}�qD~w
D~�
Dw
D�
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�Dž�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D��RD�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8RD�{�D���D���D�>�D�~�D���D���D�E11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�-A�/A�/A�$�AάA�E�A�VA���A���A�;dA�jA���A��#A�r�A�ȴA��A��A���A�VA��/A�^5A�5?A�&�A�^5A��A���A�I�A��#A��RA�G�A���A��wA�1A�E�A�A�K�A�O�A�A�$�A���A��hA�A�A�\)A�=qA���A���A�
=A�I�A�(�A�ȴA��A���A�1'A���A�K�A�$�A�O�A�S�A�1A��/A��!A�dZA���A�M�A���A���A�M�A���A�hsA��A�O�A��#A���A���A���A��+A���A�&�A�E�A�^5A���A��A~�\A}�wA}dZA}&�A|�A{��Ax  AuAu?}Av��Aw�Av��As�Ar�\Aq��AqG�Apv�Ao�An�+An~�An~�Ann�Am/Al5?Ak�
Ak�^Ak�7Ajz�Ai�wAi7LAhn�Ag
=Ae+Ac��Ab�!Ab$�Aa�FA`bA^�HA]�AW��AS��AR�9AQ��AP��AN�AL(�AI�AH�AH��AH(�AGAF�9AE�AE;dAC�AC�AAl�A@$�A?hsA=�A<ffA;x�A:�A:ZA:1A9��A9�A8VA6ȴA6r�A5ƨA5%A4A3�A2��A2�jA2�RA2r�A2�A1�A0�DA/��A/;dA.=qA-�A,��A+ƨA*��A)G�A'A&�HA&E�A&�A%S�A%
=A$jA$ �A$�A#�7A"�+A!�FA!33A ��A�-A"�A\)A=qA��A
=A�hA�TA�+A�mAl�A�!AVA�uA=qA�
A��A��A��A�FA�!A
�!A
9XA	�A�
A��A�;A��A=qA��A/A�AȴA�!A��A
=A r�A {@�t�@���@���@��@�S�@�v�@��T@�j@�33@�J@��j@�\)@�E�@�bN@�@홚@�7L@�I�@�@��@�=q@�X@�\@�t�@�-@��@��;@ݡ�@�(�@ڗ�@���@�n�@��/@��@�  @���@�X@�A�@�dZ@��@̬@ɩ�@�  @��@�33@�;d@Ƨ�@Ĭ@��@��j@�t�@��@��@�x�@�hs@�hs@�`B@�`B@�X@��u@�+@��H@�5?@�j@��y@�`B@�Ĝ@�A�@��@�ȴ@��^@���@���@�o@�ȴ@��@���@�`B@� �@�
=@��@�O�@�G�@��j@�j@���@���@��F@�t�@�C�@�v�@�`B@�Ĝ@��D@�9X@�ƨ@���@�C�@��\@�`B@�z�@��@�(�@��@��7@���@��@��R@��@��R@���@��R@�J@�?}@�Q�@�  @��F@�ƨ@��
@���@��@���@��u@�Q�@�j@�Q�@��F@��@�hs@�X@�V@��9@���@�S�@���@�@���@���@�ff@��#@�V@�(�@��@�dZ@�+@��@���@�ff@�-@���@��#@���@�?}@�/@�G�@��D@��;@�l�@�o@��R@�ff@�V@�=q@�-@�E�@�V@�=q@�$�@��@���@��-@�hs@��@�I�@�(�@� �@� �@�b@�1@��m@��F@�|�@�l�@�;d@��@���@�V@�M�@�E�@�=q@�5?@�@���@��^@��h@�hs@�&�@��@��`@��@��@�Z@�I�@�(�@� �@� �@�b@�@��@|�@+@�@~�y@~�y@~�y@~ȴ@~ff@}�T@}�-@}O�@|�/@|Z@{��@{ƨ@{��@z�@z��@z~�@zn�@z^5@z=q@y��@yx�@yhs@yX@y%@xr�@xQ�@xQ�@x  @w�w@wK�@v�+@u��@u�@t�j@t��@t�D@tZ@s��@st�@r�H@r�!@r~�@rn�@rM�@r=q@rJ@q��@p��@p�u@p�9@p�`@qhs@q��@qx�@p��@pbN@p  @n��@m�-@l��@k�m@kƨ@k��@k��@k�@kS�@ko@j��@j^5@jM�@jJ@i�@i�7@iG�@i&�@h�`@hbN@hb@g��@g;d@f�+@e��@e�@e@e`B@d��@d�j@d�@d9X@ct�@c33@c"�@c"�@cS�@ct�@c33@b��@bM�@b=q@a�@a�#@a�#@aX@`Ĝ@`Ĝ@`Ĝ@`�u@`��@`bN@` �@`  @_�;@_��@_+@_
=@^�@^�+@^E�@^@]�h@]O�@]V@\��@\j@[��@[�
@[�F@[C�@Z��@Z��@ZM�@Y��@Y��@Y�^@Y�^@Yx�@Y7L@Y%@X��@Xr�@X1'@X  @W|�@V�@VE�@V@UO�@U�@T��@T��@T(�@S��@SdZ@Rn�@Q��@Q�7@QX@QG�@Q�@O�@O|�@Ol�@OK�@Nff@M@L��@L��@L�@Lj@L(�@Kƨ@K��@Kt�@J��@JM�@I��@Ix�@IX@I7L@H��@HbN@Hb@G��@G��@G|�@Gl�@G;d@G+@F�y@F�y@Fȴ@F�+@E��@E�@D�@D��@C��@C��@C��@C�
@C�
@C��@CS�@C@B�\@B-@A��@A��@AX@A7L@A%@@�@@�@@A�@?�@?+@>��@?
=@?
=@>��@>�y@>��@>ff@=��@=`B@=�@<�@<j@;ƨ@;�m@;�@;S�@;33@;@:�@:�H@:�!@:n�@9��@9��@9��@9x�@9�@8�@7�@7�w@7�@7��@7�P@7l�@7K�@7�@6ff@5@5�@5/@4�@49X@3�m@3�m@3�
@3��@3"�@2�@2��@2M�@2J@1�#@1x�@1G�@1&�@0��@0bN@01'@0b@/\)@.�y@.��@.$�@-�-@-O�@-V@,�D@,(�@,1@+��@+o@*�H@*�\@*n�@*M�@*=q@)�@)�#@)�#@)�#@)�#@)�^@)��@)x�@)hs@)X@)�@(Ĝ@(�9@(�u@(Q�@'�@'�w@'K�@&�y@&�R@&�+@&�+@&E�@%�@%��@%�@%�@%`B@%?}@%?}@%/@$�j@$Z@#��@#�@#t�@#t�@#dZ@#dZ@#C�@#o@#@"�@"�\@"=q@"�@!�@!�^@!7L@!&�@!%@ �`@ ��@ bN@ 1'@�@;d@�@
=@�y@�@�R@�+@5?@{@{@��@�h@`B@/@V@�@z�@�@�m@t�@@�\@n�@n�@^5@M�@J@��@��@x�@hs@hs@�@��@��@�9@��@�u@Q�@b@�;@��@|�@K�@�@�@�R@�+@�+@$�@��@�-@�h@�@`B@O�@O�@?}@/@V@�@�/@�j@��@�D@j@I�@�@�m@�F@��@t�@dZ@dZ@S�@C�@�@M�@�@�#@hs@&�@�@��@��@Ĝ@Ĝ@Q�@b@�@�P@+@��@��@��@�y@ȴ@�R@v�@V@5?@$�@�@�T@�T@��@��@p�@O�@?}@�@��@�@�@�@��@��@�D@j@I�@(�@1@�m@�F@��@dZ@33@
��@
~�@
=q@	��@	�^@	�^@	��@	�7@	hs@	7L@	&�@	�@	�@	%@��@�9@�@�@1'@ �@b@  @�;@\)@��@�+@V@5?@{@��@`B@�/@��@�@z�@z�@j@I�@9X@(�@�@�@�m@�
@ƨ@��@��@��@��@�@�@dZ@C�@C�@33@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�-A�/A�/A�$�AάA�E�A�VA���A���A�;dA�jA���A��#A�r�A�ȴA��A��A���A�VA��/A�^5A�5?A�&�A�^5A��A���A�I�A��#A��RA�G�A���A��wA�1A�E�A�A�K�A�O�A�A�$�A���A��hA�A�A�\)A�=qA���A���A�
=A�I�A�(�A�ȴA��A���A�1'A���A�K�A�$�A�O�A�S�A�1A��/A��!A�dZA���A�M�A���A���A�M�A���A�hsA��A�O�A��#A���A���A���A��+A���A�&�A�E�A�^5A���A��A~�\A}�wA}dZA}&�A|�A{��Ax  AuAu?}Av��Aw�Av��As�Ar�\Aq��AqG�Apv�Ao�An�+An~�An~�Ann�Am/Al5?Ak�
Ak�^Ak�7Ajz�Ai�wAi7LAhn�Ag
=Ae+Ac��Ab�!Ab$�Aa�FA`bA^�HA]�AW��AS��AR�9AQ��AP��AN�AL(�AI�AH�AH��AH(�AGAF�9AE�AE;dAC�AC�AAl�A@$�A?hsA=�A<ffA;x�A:�A:ZA:1A9��A9�A8VA6ȴA6r�A5ƨA5%A4A3�A2��A2�jA2�RA2r�A2�A1�A0�DA/��A/;dA.=qA-�A,��A+ƨA*��A)G�A'A&�HA&E�A&�A%S�A%
=A$jA$ �A$�A#�7A"�+A!�FA!33A ��A�-A"�A\)A=qA��A
=A�hA�TA�+A�mAl�A�!AVA�uA=qA�
A��A��A��A�FA�!A
�!A
9XA	�A�
A��A�;A��A=qA��A/A�AȴA�!A��A
=A r�A {@�t�@���@���@��@�S�@�v�@��T@�j@�33@�J@��j@�\)@�E�@�bN@�@홚@�7L@�I�@�@��@�=q@�X@�\@�t�@�-@��@��;@ݡ�@�(�@ڗ�@���@�n�@��/@��@�  @���@�X@�A�@�dZ@��@̬@ɩ�@�  @��@�33@�;d@Ƨ�@Ĭ@��@��j@�t�@��@��@�x�@�hs@�hs@�`B@�`B@�X@��u@�+@��H@�5?@�j@��y@�`B@�Ĝ@�A�@��@�ȴ@��^@���@���@�o@�ȴ@��@���@�`B@� �@�
=@��@�O�@�G�@��j@�j@���@���@��F@�t�@�C�@�v�@�`B@�Ĝ@��D@�9X@�ƨ@���@�C�@��\@�`B@�z�@��@�(�@��@��7@���@��@��R@��@��R@���@��R@�J@�?}@�Q�@�  @��F@�ƨ@��
@���@��@���@��u@�Q�@�j@�Q�@��F@��@�hs@�X@�V@��9@���@�S�@���@�@���@���@�ff@��#@�V@�(�@��@�dZ@�+@��@���@�ff@�-@���@��#@���@�?}@�/@�G�@��D@��;@�l�@�o@��R@�ff@�V@�=q@�-@�E�@�V@�=q@�$�@��@���@��-@�hs@��@�I�@�(�@� �@� �@�b@�1@��m@��F@�|�@�l�@�;d@��@���@�V@�M�@�E�@�=q@�5?@�@���@��^@��h@�hs@�&�@��@��`@��@��@�Z@�I�@�(�@� �@� �@�b@�@��@|�@+@�@~�y@~�y@~�y@~ȴ@~ff@}�T@}�-@}O�@|�/@|Z@{��@{ƨ@{��@z�@z��@z~�@zn�@z^5@z=q@y��@yx�@yhs@yX@y%@xr�@xQ�@xQ�@x  @w�w@wK�@v�+@u��@u�@t�j@t��@t�D@tZ@s��@st�@r�H@r�!@r~�@rn�@rM�@r=q@rJ@q��@p��@p�u@p�9@p�`@qhs@q��@qx�@p��@pbN@p  @n��@m�-@l��@k�m@kƨ@k��@k��@k�@kS�@ko@j��@j^5@jM�@jJ@i�@i�7@iG�@i&�@h�`@hbN@hb@g��@g;d@f�+@e��@e�@e@e`B@d��@d�j@d�@d9X@ct�@c33@c"�@c"�@cS�@ct�@c33@b��@bM�@b=q@a�@a�#@a�#@aX@`Ĝ@`Ĝ@`Ĝ@`�u@`��@`bN@` �@`  @_�;@_��@_+@_
=@^�@^�+@^E�@^@]�h@]O�@]V@\��@\j@[��@[�
@[�F@[C�@Z��@Z��@ZM�@Y��@Y��@Y�^@Y�^@Yx�@Y7L@Y%@X��@Xr�@X1'@X  @W|�@V�@VE�@V@UO�@U�@T��@T��@T(�@S��@SdZ@Rn�@Q��@Q�7@QX@QG�@Q�@O�@O|�@Ol�@OK�@Nff@M@L��@L��@L�@Lj@L(�@Kƨ@K��@Kt�@J��@JM�@I��@Ix�@IX@I7L@H��@HbN@Hb@G��@G��@G|�@Gl�@G;d@G+@F�y@F�y@Fȴ@F�+@E��@E�@D�@D��@C��@C��@C��@C�
@C�
@C��@CS�@C@B�\@B-@A��@A��@AX@A7L@A%@@�@@�@@A�@?�@?+@>��@?
=@?
=@>��@>�y@>��@>ff@=��@=`B@=�@<�@<j@;ƨ@;�m@;�@;S�@;33@;@:�@:�H@:�!@:n�@9��@9��@9��@9x�@9�@8�@7�@7�w@7�@7��@7�P@7l�@7K�@7�@6ff@5@5�@5/@4�@49X@3�m@3�m@3�
@3��@3"�@2�@2��@2M�@2J@1�#@1x�@1G�@1&�@0��@0bN@01'@0b@/\)@.�y@.��@.$�@-�-@-O�@-V@,�D@,(�@,1@+��@+o@*�H@*�\@*n�@*M�@*=q@)�@)�#@)�#@)�#@)�#@)�^@)��@)x�@)hs@)X@)�@(Ĝ@(�9@(�u@(Q�@'�@'�w@'K�@&�y@&�R@&�+@&�+@&E�@%�@%��@%�@%�@%`B@%?}@%?}@%/@$�j@$Z@#��@#�@#t�@#t�@#dZ@#dZ@#C�@#o@#@"�@"�\@"=q@"�@!�@!�^@!7L@!&�@!%@ �`@ ��@ bN@ 1'@�@;d@�@
=@�y@�@�R@�+@5?@{@{@��@�h@`B@/@V@�@z�@�@�m@t�@@�\@n�@n�@^5@M�@J@��@��@x�@hs@hs@�@��@��@�9@��@�u@Q�@b@�;@��@|�@K�@�@�@�R@�+@�+@$�@��@�-@�h@�@`B@O�@O�@?}@/@V@�@�/@�j@��@�D@j@I�@�@�m@�F@��@t�@dZ@dZ@S�@C�@�@M�@�@�#@hs@&�@�@��@��@Ĝ@Ĝ@Q�@b@�@�P@+@��@��@��@�y@ȴ@�R@v�@V@5?@$�@�@�T@�T@��@��@p�@O�@?}@�@��@�@�@�@��@��@�D@j@I�@(�@1@�m@�F@��@dZ@33@
��@
~�@
=q@	��@	�^@	�^@	��@	�7@	hs@	7L@	&�@	�@	�@	%@��@�9@�@�@1'@ �@b@  @�;@\)@��@�+@V@5?@{@��@`B@�/@��@�@z�@z�@j@I�@9X@(�@�@�@�m@�
@ƨ@��@��@��@��@�@�@dZ@C�@C�@33@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�}B�}B�}B�}B�}B�}B�wB�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B��B��B�}B�}B��B�}B��B��B��B��B��BǮB��B�HB�TB�`B�B�mB�BB�;B�B�B�B�B�fB�ZB�NB�5B�
B��B��B��B��BȴBĜB�XB��B��B��B�\B�JB�7B�B{�Bu�B^5BG�BA�B5?B%�B\BB��B�B�;B�B��BÖB�FB�B��B��B�bB�B}�B{�Bz�Bu�Bl�BVBO�BI�B9XB�B1B
�BB
ǮB
�jB
�XB
�XB
�XB
�^B
�XB
�!B
��B
�uB
�%B
z�B
o�B
ffB
bNB
_;B
[#B
P�B
&�B
PB
{B
K�B
_;B
e`B
ZB
P�B
L�B
H�B
B�B
>wB
6FB
5?B
5?B
49B
.B
%�B
#�B
"�B
"�B
�B
�B
\B
1B	��B	�`B	��B	ƨB	�}B	�wB	�B	��B	�7B	S�B	�B	�B	bB	DB	%B��B��B�B�B�B�B�B�B�B�B�fB�NB�#B�B��B��B��B��B��B��B��BɺBŢB�}B�qB�jB�XB�LB�?B�?B�?B�?B�FB�FB�XB�LB�3B�-B�'B�B��B��B��B��B�DB�JB�hB��B��B��B��B��B��B��B��B�\B�oB�uB�hB�VB�7B�B� B{�Bw�Bs�Bm�Bk�BhsBffBdZB`BB_;B^5B]/B[#BYBS�BQ�BK�BG�BF�BD�B@�B?}B=qB9XB9XB7LB6FB5?B5?B5?B33B2-B2-B2-B1'B33B0!B/B.B-B,B+B)�B(�B(�B&�B'�B&�B$�B$�B#�B#�B"�B"�B!�B�B�B�B�B�B{B{BoB{BuBuBoBoBuBuB{B�B�B�B�B�B�B�B �B"�B#�B"�B �B!�B#�B"�B%�B(�B(�B(�B(�B(�B+B'�B'�B'�B"�B �B�B�B �B!�B#�B$�B%�B(�B(�B(�B-B33B49B49B49B6FB8RB7LB9XB:^B?}BA�BC�BD�BE�BI�BH�BG�BG�BH�BH�BI�BM�BO�BVBZBZBZBYB[#B^5B`BBdZBiyBn�Bv�Bv�Bw�Bt�Bp�Br�Bw�B{�B~�B�B�7B�7B�DB�PB�VB�bB�oB��B��B��B��B��B��B��B�!B�9B�?B�FB�?B�LB�RB�^B�jB�wB��BÖBƨBǮBȴBȴB��B��B��B��B��B�B�#B�)B�/B�;B�BB�NB�ZB�B�B�B�B��B��B��B��B��B	B		7B	DB	DB	JB	PB	VB	bB	{B	�B	�B	�B	�B	�B	"�B	"�B	#�B	#�B	#�B	&�B	&�B	)�B	,B	-B	0!B	1'B	33B	5?B	6FB	8RB	9XB	:^B	:^B	:^B	;dB	=qB	=qB	@�B	B�B	D�B	F�B	G�B	H�B	H�B	I�B	J�B	K�B	L�B	M�B	O�B	P�B	P�B	Q�B	VB	W
B	YB	ZB	ZB	[#B	^5B	_;B	_;B	_;B	_;B	cTB	dZB	e`B	gmB	hsB	jB	m�B	p�B	q�B	t�B	t�B	u�B	u�B	x�B	y�B	}�B	� B	�B	�B	�B	�B	�7B	�DB	�DB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�jB	�qB	�}B	�}B	��B	B	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
%B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
PB
VB
VB
\B
\B
\B
bB
bB
hB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
/B
0!B
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
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
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
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
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
\)B
\)B
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
cTB
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
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�}B��B��B�}B�}B�wB��B�}B��B�}B�}B�}B��B�}B��B�}B�}B�}B�}B��B��B�}B�}B��B�}B�}B�}B��B��B�}B�}B��B�}B��B��B�oB��B�?B�VB�sB�nB�B�DB��B�KB�tB�`B��B�2B�TB�|B�RB��B��B�\BخB�B��B��B̈́B�BɠB��B�B�|B�QB��B��B�^B��B~]BzBaBIBC�B8B(�B4BEB��B��B�vBڠB�+BňB��B�}B��B��B�TB��B~�B|�B|jBy>BoiBW
BQ�BMB=<B)B�B
�B
��B
��B
��B
��B
�B
�PB
��B
�GB
��B
�MB
��B
|�B
p�B
gB
b�B
`BB
]dB
T�B
)�B
B
&B
K�B
`vB
g�B
[�B
Q�B
M�B
I�B
C�B
?�B
6�B
5�B
5�B
5�B
/5B
&�B
$@B
#nB
$&B
�B
�B
�B

	B	��B	�B	�SB	ǔB	��B	��B	�CB	�B	�(B	W�B	!�B	$B	TB	B		B�}B�$B�-B�vB�B��B��B��B�;B�B�B��BܒB�+BөB� BοB̈́B�^B�)BˬB�xB�_B�4B��B��B��B�lB��B��B��B��B��B�LB��B�lB�9B��B��B�B�KB��B��B�yB�~B�6B�B��B�EB�eB�B�)B��B�B��B�bB�uB��B��B�}B��B�9B�UB}�BzButBn}BlqBi�Bh>Be,B`�B`'B_pB^B\�B[�BU�BTFBL�BI7BH�BF%BA�B@�B>�B:^B:B7�B6�B5�B6�B6FB4B2�B2�B2�B2GB49B0�B/�B.�B.B-B,B+B*B*B(sB)DB'mB%zB%�B$tB$tB#�B#�B#�B!�B�B�ByB�B�B�B�BB{B�B�B[BaBaBgB�BB�B�BOB�B 'B!�B$ZB%�B#�B!�B"�B$&B#B&B)B)*B)DB)DB)�B,B(�B(�B)DB$B!�B BB \B!bB"�B$�B%�B&�B)yB)_B)�B-�B3�B5%B5%B5B6�B8�B7�B9�B:�B?�BA�BC�BEBFtBJ�BI7BG�BHBI7BIBJ=BN�BP�BV�BZQBZ�BZ�BZ�B\CB^�BaBd�BiDBn�Bw2Bw�Bx�ButBp�BsBw�B|B�B�SB��B��B��B�jB��B�4B��B��B��B�#B�!B��B�B�B��B�TB��B��B��B��B�	B��B��B��B��B��B�B��B�B��B�B�B�B�:BӏBخB�qBܒB�~BߊB�vB�hB�tB�B�B��B��B�B�B�6B�PB��B	uB		lB	xB	xB	~B	�B	�B	�B	�B	�B	�B		B	B	�B	#B	#B	$B	$B	$&B	'B	'8B	*KB	,WB	-]B	0;B	1vB	3�B	5tB	6zB	8�B	9rB	:xB	:�B	:�B	;�B	=�B	=�B	@�B	B�B	D�B	F�B	G�B	H�B	IB	J	B	J�B	LB	MB	N"B	P.B	QB	Q4B	RTB	V9B	W?B	YKB	ZQB	ZQB	[WB	^jB	_pB	_VB	_�B	_�B	c�B	d�B	e�B	g�B	h�B	j�B	m�B	p�B	q�B	t�B	t�B	u�B	v+B	y	B	z*B	~(B	�4B	� B	�GB	�3B	�mB	�lB	�xB	�xB	�dB	�}B	��B	��B	��B	��B	�B	�-B	�bB	�tB	�sB	�QB	�)B	�AB	�`B	�fB	�lB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	��B	�B	�(B	�B	� B	� B	�,B	�9B	�$B	�
B	�+B	�B	�QB	�qB	�CB	�]B	�CB	�IB	�dB	�dB	ބB	�5B	�OB	�OB	�5B	�jB	�VB	�vB	�vB	�|B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�*B	�B	�"B	�<B	�HB
;B
AB
AB
-B
MB
gB
tB
YB
�B
�B
	�B

rB

rB

rB

�B

�B
^B
^B
xB
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
B
�B
�B
�B
�B
�B
!B
 �B
 �B
 �B
!�B
#B
$�B
$�B
%�B
&B
%�B
'8B
'8B
($B
)B
)*B
)DB
)DB
)B
*0B
,=B
,"B
,=B
,=B
,=B
,"B
,=B
-]B
.IB
./B
./B
.cB
.cB
/iB
0;B
0;B
0;B
0UB
0;B
0UB
0UB
0�B
1[B
2GB
2|B
3hB
3�B
4nB
49B
4nB
4TB
5�B
5tB
5tB
6zB
5tB
6zB
6zB
7�B
7�B
7�B
8�B
9�B
9�B
9�B
:�B
;B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J	B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
MB
L�B
NB
N"B
N�B
OB
N�B
O�B
PB
O�B
PB
PB
QB
Q B
QB
QB
Q B
R B
R B
RB
R B
SB
SB
S&B
TFB
U2B
U2B
UB
UB
U2B
U2B
V9B
V9B
V9B
VB
V9B
W$B
W?B
W?B
W?B
W
B
W?B
W$B
XEB
XEB
XEB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
ZQB
ZQB
[WB
[=B
[WB
[=B
[=B
[=B
\CB
\CB
\)B
\]B
\CB
\CB
\]B
\CB
\]B
]dB
]IB
]dB
]dB
]dB
]dB
^OB
^OB
^OB
^OB
^jB
^�B
^�B
_pB
_pB
`\B
`\B
`\B
`vB
abB
abB
a|B
a|B
abB
a|B
b�B
bhB
bhB
cTB
bNB
cnB
b�B
cnB
cnB
cnB
c�B
cnB
cnB
cTB
cnB
c�B
c�B
dtB
dtB
dtB
dtB
d�B
ezB
e`B
ezB
ezB
ezB
e`B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
iyB
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611180033522016111800335220161118003352201806221216492018062212164920180622121649201804050409462018040504094620180405040946  JA  ARFMdecpA19c                                                                20161114093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161114003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161114003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161114003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161114003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161114003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161114003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161114003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161114003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161114003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20161114012917                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161114153443  CV  JULD            G�O�G�O�F�Ϡ                JM  ARCAJMQC2.0                                                                 20161117153352  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161117153352  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190946  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031649  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                