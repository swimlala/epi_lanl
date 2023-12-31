CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-04T21:35:20Z creation;2018-05-04T21:35:26Z conversion to V3.1;2019-12-19T07:43:08Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180504213520  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_236                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�`3=�/�1   @�`4�[ @:7-�d^�Z���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @0��@w
=@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_
=Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D}qD�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��RD���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�~�D㾸D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�>�D�~�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D��RD���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�ED�hR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��uA��uA��\A��+A�|�A�|�A�|�A�|�A�|�A�~�A�~�A�~�A��A�~�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A�hsA�dZA�C�A�1'A�/A�1'A�+A�(�A� �A���A� �A�(�A��+A��yA�bA���A� �A��A�/A�$�A�oA��!A��
A�
=A�&�A�z�A�33A���A��A�A�A��A��`A�O�A�/A�ȴA�&�A�ĜA�ȴA��/A�VA�  A�hsA���A�I�A���A��RA�Q�A�dZA��+A�-A�r�A��uA�l�A��A��A��TA���A��A~�/A~^5A|ĜAz�AxbAtM�As��As�wAr�Ap�Ao��AohsAnz�AnbAmƨAmp�Al�Al5?AjȴAg�FAe�TAd��Ac�AaAa\)A`��A`  A^��A]�7A[hsAX��AW�AW�AVbNAVJAU�AU��AU��AT�jASp�AQ�AP��AO�AO33AN��AM�;AM�AM�ALffAL �AK�PAJ�jAI%AG�FAF�\AE��AEAD$�AC33ABjAA�hA?�FA>��A=�#A<�A;A;;dA:-A9K�A8��A8{A7?}A5��A533A4��A41'A2�/A1�#A1?}A0�A0�A0JA.�\A,-A+;dA*��A*�yA*�`A*�/A*�A*�jA(�HA'&�A&I�A%A$�/A$ffA#��A#t�A"Q�A!XA ��A n�A {A�
A�AA�A��A7LA��A$�A�;A��A/A��A^5A��A��A��A�A�#AO�A�A��A-A�A��AC�AM�A|�A�!A�uAffA�A�FAt�A��AffA��A��Av�A$�A��A`BA
��A
r�A	XAjA��A1'A��A�uAt�A��AbA �`A �!A ff@�V@�t�@�o@�V@���@��#@��@�@��@�-@���@���@��@��@��T@���@�@�h@���@�G�@�5?@ۥ�@ڧ�@�n�@��@׶F@��@ְ!@֏\@�J@���@��@Ӯ@�\)@ҏ\@�7L@ϥ�@�=q@��@�Ĝ@̓u@�C�@�M�@��@�`B@�bN@�@�ff@�5?@�x�@���@�I�@Å@��@��D@��@���@���@�ȴ@�v�@�?}@�n�@�?}@�V@���@�A�@�b@��;@���@�33@�{@��@��m@�ƨ@��
@�\)@���@���@�5?@��@�X@�&�@���@�z�@���@�S�@��R@��@��T@�&�@��@��R@��+@�J@��/@�I�@�b@�ƨ@�|�@�o@���@��\@�{@�@��7@���@��F@�@�=q@�7L@�bN@���@��@�@�v�@�@��^@�p�@��@�1@�33@��@�G�@�&�@���@���@�bN@�S�@��\@�n�@�^5@�=q@��@�7L@��j@�9X@��;@�ƨ@��F@���@�dZ@�C�@��@��@��R@�ff@��T@���@���@�G�@���@��D@� �@��@��w@�S�@�o@���@�$�@�p�@���@�Q�@�1@��m@���@���@�t�@�;d@���@�M�@�J@���@���@�hs@��@�r�@��m@�\)@��@��@���@�ȴ@��R@��!@���@��!@���@�v�@�{@�`B@��@���@�Ĝ@�r�@�A�@�  @�w@|�@l�@;d@~��@~ȴ@~�@}�@}�@}�@}/@}V@|�/@|�j@|��@|j@|Z@|I�@{�
@z�H@z�\@z�\@z^5@zM�@y��@y��@y�^@yx�@x��@x  @w��@w��@w|�@wK�@w;d@w+@w+@w
=@vȴ@v5?@u�@u�-@up�@uV@t��@t��@tj@tI�@t(�@s��@s�m@s��@r�@r=q@r-@r�@q�@q�^@p�@o��@o;d@n�+@m�@k�m@kt�@kdZ@k33@j��@j�\@j^5@j=q@j-@jJ@i�@i&�@h��@h�9@hb@g�P@gl�@gl�@g�@f�R@f�+@fE�@f5?@f{@e�-@e�@d��@dz�@d�@d�@c�
@cdZ@cS�@c33@b�H@b^5@a��@a��@a��@a��@aG�@a%@`��@`Ĝ@`��@`�u@`Q�@`  @_|�@_
=@^�@^@]�@\��@\�@\�@\��@\�D@\z�@\j@\1@Z�@Y��@YX@Y%@X��@X�@XQ�@X �@W�@W�w@W�P@Wl�@WK�@V��@V$�@U@T�@T�@S��@SS�@S"�@So@R�@R��@Q��@Qx�@Q7L@Q%@P��@P�`@P��@PĜ@P�9@P�u@PbN@PbN@PQ�@O��@N�y@Nv�@NV@N5?@N@M�T@M�-@M`B@M?}@L��@LZ@Kƨ@Kt�@J�\@I��@I�^@I�^@I��@I�7@Ix�@H�9@H  @G�w@G�@F��@Fv�@Fff@F5?@F5?@E�-@E�@E/@D��@D�/@D��@Dj@D9X@D�@D�@D1@C�m@C�F@C��@C��@C�@CdZ@B�\@B^5@BM�@B=q@B=q@B-@B�@A�@Ahs@A7L@A�@@��@@bN@@b@?�@?�w@?\)@?;d@?+@?�@>��@>�@>�+@>E�@>@=��@=O�@=V@<�j@<�D@<(�@<�@;�m@;��@;"�@:�!@:n�@:=q@9��@9X@9�@8Ĝ@8A�@8 �@7�;@7��@7l�@7;d@7+@7�@7�@6�R@6v�@6E�@5�-@4��@4�@4�D@4j@49X@4(�@4(�@41@3��@3t�@3C�@3@2�H@2�!@2~�@2=q@2�@2J@1�@1��@1�^@1��@1hs@1%@0�@0b@/�w@/��@/�w@/�w@/�w@/�w@/��@/+@/
=@.�@.ff@.@-O�@,��@,�@,�D@,j@,I�@+��@+�@+S�@*�H@*�\@*M�@*J@)�7@)�@(��@(�u@(r�@(r�@(bN@( �@'�;@'�w@'�@'��@'|�@';d@'+@&��@&v�@%�T@%`B@%O�@%/@$�@$Z@$(�@$1@#t�@#"�@"�@"��@"��@"��@"��@"�\@"~�@"=q@!&�@ ��@ bN@  �@  �@   @�w@�P@|�@K�@K�@K�@��@V@{@@@�@�-@�@�j@z�@j@j@9X@�@�@�@1@�m@�
@��@t�@S�@C�@33@33@o@�!@�\@�\@�\@�\@�\@�\@�\@�\@n�@M�@-@J@J@��@��@�@�#@��@��@�7@7L@&�@%@�`@Ĝ@��@��@��@��@��@��@r�@ �@��@�P@l�@K�@;d@�@�y@��@V@E�@5?@E�@5?@�-@�@p�@`B@O�@��@��@9X@�
@dZ@�@��@�\@~�@n�@M�@-@J@�@�^@��@x�@X@7L@�@��@��@�9@�9@�@�@bN@�;@+@
=@��@�R@��@��@�+@ff@�@O�@V@�/@�j@z�@1@�m@�F@dZ@"�@"�@o@
�@
�H@
��@
��@
n�@
=q@
�@	�@	�#@	�^@	��@	hs@	&�@	�@��@��@�9@��@�u@Q�@ �@  @  @�@�w@�@��@�P@|�@|�@;d@�y@��@E�@{@��@�-@p�@V@��@��@��@�@�@�/@�@�D@z�@Z@9X@(�@�@��@�@S�@�@�H@��@n�@^5@-@�@��@��@��@X@ �`@ Ĝ@ ��@ �u@ bN@ Q�@ A�@ A�@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��uA��uA��\A��+A�|�A�|�A�|�A�|�A�|�A�~�A�~�A�~�A��A�~�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A�hsA�dZA�C�A�1'A�/A�1'A�+A�(�A� �A���A� �A�(�A��+A��yA�bA���A� �A��A�/A�$�A�oA��!A��
A�
=A�&�A�z�A�33A���A��A�A�A��A��`A�O�A�/A�ȴA�&�A�ĜA�ȴA��/A�VA�  A�hsA���A�I�A���A��RA�Q�A�dZA��+A�-A�r�A��uA�l�A��A��A��TA���A��A~�/A~^5A|ĜAz�AxbAtM�As��As�wAr�Ap�Ao��AohsAnz�AnbAmƨAmp�Al�Al5?AjȴAg�FAe�TAd��Ac�AaAa\)A`��A`  A^��A]�7A[hsAX��AW�AW�AVbNAVJAU�AU��AU��AT�jASp�AQ�AP��AO�AO33AN��AM�;AM�AM�ALffAL �AK�PAJ�jAI%AG�FAF�\AE��AEAD$�AC33ABjAA�hA?�FA>��A=�#A<�A;A;;dA:-A9K�A8��A8{A7?}A5��A533A4��A41'A2�/A1�#A1?}A0�A0�A0JA.�\A,-A+;dA*��A*�yA*�`A*�/A*�A*�jA(�HA'&�A&I�A%A$�/A$ffA#��A#t�A"Q�A!XA ��A n�A {A�
A�AA�A��A7LA��A$�A�;A��A/A��A^5A��A��A��A�A�#AO�A�A��A-A�A��AC�AM�A|�A�!A�uAffA�A�FAt�A��AffA��A��Av�A$�A��A`BA
��A
r�A	XAjA��A1'A��A�uAt�A��AbA �`A �!A ff@�V@�t�@�o@�V@���@��#@��@�@��@�-@���@���@��@��@��T@���@�@�h@���@�G�@�5?@ۥ�@ڧ�@�n�@��@׶F@��@ְ!@֏\@�J@���@��@Ӯ@�\)@ҏ\@�7L@ϥ�@�=q@��@�Ĝ@̓u@�C�@�M�@��@�`B@�bN@�@�ff@�5?@�x�@���@�I�@Å@��@��D@��@���@���@�ȴ@�v�@�?}@�n�@�?}@�V@���@�A�@�b@��;@���@�33@�{@��@��m@�ƨ@��
@�\)@���@���@�5?@��@�X@�&�@���@�z�@���@�S�@��R@��@��T@�&�@��@��R@��+@�J@��/@�I�@�b@�ƨ@�|�@�o@���@��\@�{@�@��7@���@��F@�@�=q@�7L@�bN@���@��@�@�v�@�@��^@�p�@��@�1@�33@��@�G�@�&�@���@���@�bN@�S�@��\@�n�@�^5@�=q@��@�7L@��j@�9X@��;@�ƨ@��F@���@�dZ@�C�@��@��@��R@�ff@��T@���@���@�G�@���@��D@� �@��@��w@�S�@�o@���@�$�@�p�@���@�Q�@�1@��m@���@���@�t�@�;d@���@�M�@�J@���@���@�hs@��@�r�@��m@�\)@��@��@���@�ȴ@��R@��!@���@��!@���@�v�@�{@�`B@��@���@�Ĝ@�r�@�A�@�  @�w@|�@l�@;d@~��@~ȴ@~�@}�@}�@}�@}/@}V@|�/@|�j@|��@|j@|Z@|I�@{�
@z�H@z�\@z�\@z^5@zM�@y��@y��@y�^@yx�@x��@x  @w��@w��@w|�@wK�@w;d@w+@w+@w
=@vȴ@v5?@u�@u�-@up�@uV@t��@t��@tj@tI�@t(�@s��@s�m@s��@r�@r=q@r-@r�@q�@q�^@p�@o��@o;d@n�+@m�@k�m@kt�@kdZ@k33@j��@j�\@j^5@j=q@j-@jJ@i�@i&�@h��@h�9@hb@g�P@gl�@gl�@g�@f�R@f�+@fE�@f5?@f{@e�-@e�@d��@dz�@d�@d�@c�
@cdZ@cS�@c33@b�H@b^5@a��@a��@a��@a��@aG�@a%@`��@`Ĝ@`��@`�u@`Q�@`  @_|�@_
=@^�@^@]�@\��@\�@\�@\��@\�D@\z�@\j@\1@Z�@Y��@YX@Y%@X��@X�@XQ�@X �@W�@W�w@W�P@Wl�@WK�@V��@V$�@U@T�@T�@S��@SS�@S"�@So@R�@R��@Q��@Qx�@Q7L@Q%@P��@P�`@P��@PĜ@P�9@P�u@PbN@PbN@PQ�@O��@N�y@Nv�@NV@N5?@N@M�T@M�-@M`B@M?}@L��@LZ@Kƨ@Kt�@J�\@I��@I�^@I�^@I��@I�7@Ix�@H�9@H  @G�w@G�@F��@Fv�@Fff@F5?@F5?@E�-@E�@E/@D��@D�/@D��@Dj@D9X@D�@D�@D1@C�m@C�F@C��@C��@C�@CdZ@B�\@B^5@BM�@B=q@B=q@B-@B�@A�@Ahs@A7L@A�@@��@@bN@@b@?�@?�w@?\)@?;d@?+@?�@>��@>�@>�+@>E�@>@=��@=O�@=V@<�j@<�D@<(�@<�@;�m@;��@;"�@:�!@:n�@:=q@9��@9X@9�@8Ĝ@8A�@8 �@7�;@7��@7l�@7;d@7+@7�@7�@6�R@6v�@6E�@5�-@4��@4�@4�D@4j@49X@4(�@4(�@41@3��@3t�@3C�@3@2�H@2�!@2~�@2=q@2�@2J@1�@1��@1�^@1��@1hs@1%@0�@0b@/�w@/��@/�w@/�w@/�w@/�w@/��@/+@/
=@.�@.ff@.@-O�@,��@,�@,�D@,j@,I�@+��@+�@+S�@*�H@*�\@*M�@*J@)�7@)�@(��@(�u@(r�@(r�@(bN@( �@'�;@'�w@'�@'��@'|�@';d@'+@&��@&v�@%�T@%`B@%O�@%/@$�@$Z@$(�@$1@#t�@#"�@"�@"��@"��@"��@"��@"�\@"~�@"=q@!&�@ ��@ bN@  �@  �@   @�w@�P@|�@K�@K�@K�@��@V@{@@@�@�-@�@�j@z�@j@j@9X@�@�@�@1@�m@�
@��@t�@S�@C�@33@33@o@�!@�\@�\@�\@�\@�\@�\@�\@�\@n�@M�@-@J@J@��@��@�@�#@��@��@�7@7L@&�@%@�`@Ĝ@��@��@��@��@��@��@r�@ �@��@�P@l�@K�@;d@�@�y@��@V@E�@5?@E�@5?@�-@�@p�@`B@O�@��@��@9X@�
@dZ@�@��@�\@~�@n�@M�@-@J@�@�^@��@x�@X@7L@�@��@��@�9@�9@�@�@bN@�;@+@
=@��@�R@��@��@�+@ff@�@O�@V@�/@�j@z�@1@�m@�F@dZ@"�@"�@o@
�@
�H@
��@
��@
n�@
=q@
�@	�@	�#@	�^@	��@	hs@	&�@	�@��@��@�9@��@�u@Q�@ �@  @  @�@�w@�@��@�P@|�@|�@;d@�y@��@E�@{@��@�-@p�@V@��@��@��@�@�@�/@�@�D@z�@Z@9X@(�@�@��@�@S�@�@�H@��@n�@^5@-@�@��@��@��@X@ �`@ Ĝ@ ��@ �u@ bN@ Q�@ A�@ A�@ A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�
B�
B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�
B��B��B��B��B��B��B��B��BƨB�jB�!B��B��B�JBw�BYB�B%�B=qB@�B;dB1'B�BoB1B%BBB��B�B��BB��B}�B�Bk�BJ�B)�B,B1'B�B�B�BJBB  B
�yB
�3B
��B
��B
�\B
u�B
l�B
�B
�B
�B
w�B
e`B
^5B
ZB
C�B
(�B
�B
B
�B
 �B
+B
	7B
%B
%B	��B
  B
  B	��B	�B	�B	�B	�3B	�FB	�qB	�B	��B	�-B	�B	��B	�hB	�7B	w�B	aHB	v�B	u�B	u�B	w�B	x�B	u�B	n�B	`BB	O�B	C�B	G�B	=qB	F�B	B�B	8RB	:^B	49B	+B	+B	$�B	�B	B	B	B	B	  B��B�B�yB�TB��B��B�#B��B��B�B��B��B��B��B��B�3B�^B�jB�RB�B�B�!B�B�B��B�=B� B�\B��B��B��B��B��B�\Bw�Bk�B�B�B�B�B�B{�Br�Bp�Bz�Bz�B}�Bz�Bq�Bn�Br�Bq�Bo�Bq�Bs�Bq�Bl�BgmBk�BcTBaHBk�BhsB`BBcTBdZBe`BaHBcTB`BBZBQ�BQ�BP�B^5B\)BW
BQ�BO�BE�B:^B�B8RBD�BF�BC�B@�B8RB9XB+B'�B!�B,B-B#�B�B(�B(�B!�B-B&�BhB�B.B&�B�B�B%�B%�B&�B �BPB�B"�B&�B#�B!�B�B�B�BVBJB{B�B'�B�B�B �B&�B$�B �B�B�B#�B �B�B{BhB�B�B&�B$�B�B�B%�B"�B�B�B&�B+B&�B%�B&�B!�B�B�B{B&�B)�B)�B/B&�B�B49BA�B@�BB�BC�BD�BA�B>wB9XB49BL�BM�BM�BJ�BI�BK�BK�BN�BK�BP�BO�BO�BL�BO�BN�BP�BS�BN�BI�BT�B^5B\)BYBbNBhsBiyBiyBiyBjBn�Bk�Bl�Bm�BhsBgmBo�Br�Bs�B{�B�B�B�%B�1B�DB�PB�PB�DB�=B�PB�bB��B��B��B��B��B��B��B�'B�-B�'B�!B�B�-B�LB�dB��B��B��B��BÖBÖBĜBƨBƨBƨB��B��B��B��B��B��B�B�B�B�B�#B�5B�;B�ZB�sB�B��B��B��B��B��B��B��B	  B	B	B	B	B	B		7B	VB	�B	�B	�B	�B	 �B	"�B	$�B	&�B	'�B	+B	,B	-B	33B	6FB	6FB	6FB	:^B	;dB	?}B	@�B	B�B	C�B	C�B	F�B	M�B	N�B	Q�B	ZB	]/B	]/B	^5B	_;B	`BB	aHB	cTB	cTB	bNB	dZB	k�B	o�B	n�B	p�B	p�B	r�B	s�B	s�B	s�B	w�B	~�B	�B	� B	�B	�%B	�1B	�7B	�7B	�1B	�=B	�VB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�RB	�XB	�^B	�^B	�^B	�wB	��B	�}B	��B	ÖB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�HB	�TB	�TB	�TB	�TB	�TB	�NB	�;B	�/B	�HB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
  B
B
B
B
B
1B

=B
	7B
	7B
	7B
%B
+B
DB

=B
JB
\B
bB
\B
bB
VB
bB
bB
hB
oB
oB
hB
uB
{B
{B
{B
uB
{B
�B
�B
{B
uB
oB
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
#�B
#�B
#�B
#�B
&�B
&�B
%�B
'�B
'�B
(�B
(�B
(�B
&�B
'�B
'�B
&�B
&�B
+B
-B
-B
-B
.B
.B
-B
,B
-B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
0!B
/B
/B
.B
/B
1'B
2-B
49B
49B
49B
49B
33B
2-B
33B
33B
2-B
2-B
2-B
33B
5?B
5?B
5?B
6FB
6FB
5?B
7LB
6FB
7LB
7LB
8RB
8RB
8RB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
<jB
<jB
<jB
?}B
?}B
=qB
?}B
@�B
A�B
?}B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
B�B
A�B
=qB
A�B
C�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
H�B
G�B
F�B
D�B
H�B
I�B
I�B
G�B
G�B
E�B
G�B
I�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
L�B
K�B
M�B
N�B
O�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
N�B
N�B
N�B
O�B
N�B
M�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
N�B
O�B
P�B
Q�B
R�B
R�B
R�B
R�B
Q�B
R�B
T�B
T�B
T�B
S�B
R�B
S�B
VB
VB
T�B
S�B
R�B
S�B
T�B
T�B
VB
YB
YB
[#B
ZB
ZB
[#B
[#B
[#B
ZB
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
[#B
ZB
ZB
^5B
_;B
^5B
_;B
`BB
_;B
^5B
]/B
\)B
`BB
`BB
aHB
`BB
`BB
bNB
bNB
bNB
cTB
e`B
e`B
dZB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
e`B
ffB
gmB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
hsB
ffB
ffB
gmB
gmB
iyB
iyB
jB
iyB
iyB
l�B
l�B
m�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
l�B
m�B
m�B
k�B
m�B
m�B
l�B
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
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�+B�$B�$B�?B�+B�B�B�+B�+B�+B�B�B�B�+B�B�B�+B�B�+B�B�+B�+B�+B�B�$B�$B�FB�B� B�(B��B��B��B��B�+B��B��B��B�/B�"Bz�B]�B$�B)B>B@�B;�B2GB�B{B
#B�B�B�B��B�B��B�B��B��B��Bm�BNVB.cB.IB2�B�BB�B�B%B �B
�QB
�B
�B
��B
��B
y�B
oB
��B
��B
�[B
x�B
gB
_�B
[#B
E�B
,"B
�B
�B
	B
!HB
	lB

�B
EB
�B
 B
 �B
 �B	��B	�B	�B	�]B	��B	��B	��B	�]B	�eB	��B	��B	�4B	�[B	�B	zxB	d�B	w�B	v�B	v�B	x8B	y$B	v+B	o5B	a�B	Q�B	E�B	IRB	>�B	G+B	CaB	9XB	:�B	5%B	+�B	+�B	%�B	B	zB	�B	�B	AB	B�B�B�B��B�2BԕB�)B�TB�pB��B�hB��BοBˬB��B��B�JB�VB�>B��B�WB��B��B��B�B��B��B�bB�B��B��B�B�B�HBzDBm�B��B��B�AB��B��B|�BtTBq�B{B{�B~wB{�Br�Bo�Bs�Br�Bp�BraBt9BrGBmCBh>BlBdZBbBk�Bi*BabBd&BeBe�Ba�Bc�B`�BZ�BSuBS&BQ�B^�B\�BW�BR�BP�BF�B;�B!-B9XBEmBGEBD3BA;B9XB:DB,�B)_B#�B-)B.B%zB!bB)�B*0B#:B-�B'�BuB �B.}B'�B_B�B&�B&�B'�B!�B�B�B#�B'�B$�B"�B�B�B�BbBpB�B �B(sB �B�B!|B'8B%,B!bB�BdB$&B!-B�B�BoB�B�B'B%`B�BjB&LB#�B �B�B'mB+QB'�B&�B'�B"�B	B�BSB'�B*�B*�B/�B(
B�B4�BA�B@�BB�BC�BD�BBB?B:^B5�BMBN"BNBKDBJ=BLBL0BOBBLdBQNBPHBPbBM�BPbBOvBQhBTaBO�BJ�BU�B^�B\�BY�Bb�Bh�Bi�Bi�Bi�BkBn�BlBl�Bm�Bi*BhsBpUBshBt�B|�B�{B��B��B��B��B��B��B��B�)B�"B�NB�B�B�,B�@B�bB��B��B�AB�GB�vB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�0B�PB�&B�@B�EB�mB�mBچBیBޞB��B��B�B��B��B��B��B��B�8B�fB�PB	 OB	;B	GB	{B	uB	�B		�B	�B	�B	�B	�B	�B	 �B	"�B	%B	'B	($B	+QB	,qB	-�B	3hB	6`B	6�B	6�B	:�B	;�B	?�B	@�B	B�B	C�B	C�B	F�B	M�B	OBB	RTB	Z7B	]IB	]dB	^jB	_VB	`vB	a|B	cnB	c�B	b�B	d�B	k�B	o�B	n�B	p�B	p�B	r�B	s�B	tB	tB	xB	.B	�B	�4B	�AB	�YB	�KB	�RB	�RB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	�B	�B	�4B	�-B	�pB	�B	�IB	�OB	�oB	�hB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	żB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�(B	��B	� B	�:B	� B	�B	�9B	�
B	�?B	�YB	�EB	�QB	�QB	�QB	�1B	�KB	�_B	�EB	�eB	�WB	�B	چB	�|B	�nB	�nB	�nB	�B	�B	�B	ߊB	ݲB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�%B	��B	��B	�B	�B	�B	�B	�"B	�B	�"B	�"B	�B	�B	�*B	�XB	�B
;B
;B
;B
'B
;B
;B
AB
;B
 OB
;B
aB
uB
SB
fB

XB
	lB
	lB
	RB
�B
�B
xB

�B
�B
�B
bB
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
�B
�B
B
�B
!�B
!�B
!B
$B
$B
#�B
$&B
'B
'B
&B
($B
($B
(�B
)B
)B
'B
(
B
($B
'B
'8B
+6B
-B
-CB
-CB
.B
./B
-CB
,WB
-CB
.IB
.IB
/OB
/5B
/OB
/OB
0;B
1'B
1[B
1[B
1AB
0UB
/5B
/iB
.cB
/iB
1AB
2GB
49B
49B
4TB
4TB
3hB
2|B
3hB
3hB
2|B
2|B
2|B
3�B
5tB
5ZB
5ZB
6`B
6zB
5tB
7�B
6�B
7fB
7�B
8lB
8�B
8�B
:�B
:xB
;�B
<�B
<�B
<�B
<�B
>�B
>�B
?}B
>�B
>�B
>�B
>�B
<�B
<�B
<�B
?�B
?�B
=�B
?�B
@�B
A�B
?�B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
B�B
A�B
=�B
A�B
C�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
H�B
G�B
F�B
D�B
H�B
I�B
I�B
G�B
G�B
E�B
G�B
I�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
M�B
M�B
M�B
M�B
MB
K�B
M�B
N�B
O�B
N�B
N�B
N�B
N�B
N�B
NB
M�B
M�B
N�B
O�B
O�B
O�B
N�B
N�B
OB
O�B
OB
NB
O�B
PB
PB
QB
Q B
Q�B
RB
RB
RB
Q B
PB
OB
PB
Q B
R B
S&B
SB
S&B
S&B
RB
S&B
UB
T�B
UB
T,B
S@B
TB
VB
VB
UB
T,B
S@B
TFB
U2B
UMB
V9B
YKB
YKB
[#B
ZQB
Z7B
[WB
[WB
[WB
ZQB
[=B
\CB
[WB
\]B
\CB
\]B
\]B
\CB
]/B
\CB
]/B
[=B
ZkB
ZkB
^5B
_VB
^OB
_VB
`\B
_pB
^jB
]dB
\xB
`vB
`\B
a|B
`vB
`vB
b�B
b�B
b�B
cnB
e`B
e`B
dtB
d�B
e`B
d�B
d�B
d�B
e�B
e�B
ezB
e�B
e�B
d�B
d�B
ezB
e�B
e�B
ezB
ffB
f�B
e�B
f�B
g�B
hsB
hsB
g�B
g�B
h�B
hsB
h�B
h�B
f�B
f�B
g�B
g�B
i�B
i�B
j�B
i�B
i�B
l�B
l�B
m�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
l�B
m�B
m�B
k�B
m�B
m�B
l�B
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
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805090037112018050900371120180509003711201806221241262018062212412620180622124126201806042119522018060421195220180604211952  JA  ARFMdecpA19c                                                                20180505063518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180504213520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180504213523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180504213524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180504213524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180504213524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180504213525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180504213525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180504213526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180504213526                      G�O�G�O�G�O�                JA  ARUP                                                                        20180504215604                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180505153402  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180508153711  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180508153711  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604121952  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034126  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                