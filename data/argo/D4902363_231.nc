CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-20T00:35:24Z creation;2018-04-20T00:35:31Z conversion to V3.1;2019-12-19T07:44:21Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180420003524  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_231                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�\t.֩ 1   @�\t��� @:��dlH��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D���D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @w
=@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CACC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
p�D
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
D&p�D&�
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�8RD�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�>�D�{�Dڻ�D��RD�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D�RD���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��+A�t�A�r�A�v�A�r�A�VA�ZA�VA�9XA���A��-A�z�A�=qA���A���A��uA�t�A�hsA�VA�;dA�oA�A��A��!A�p�A�bNA�S�A� �A�7LA��/A��\A�+A��;A���A�9XA�M�A���A��A���A���A�t�A�  A�`BA��FA�bNA��#A�G�A�/A��A�S�A���A�;dA��PA���A�z�A�dZA���A�hsA���A�n�A�
=A�&�A���A���A��/A���A��A��yA�I�A�^5A���A��RA�;dA��
A�r�A�+A�9XA��hA�(�A��#A��RA��A�C�A�TA}�A{�#A{�Az��Az{Axv�Aw�PAw
=Av��Av�Au�AuoAt�Aq�-Aq
=Apr�Ao�Ao�PAn�An{Am�AmVAlbNAk�Ak?}Aj�HAjv�Aj  Ah��Ag\)Ae�Ad{Ac?}A`�HA_ƨA^�A]7LA\=qAZ��AY��AYXAY+AXĜAW�;AU?}AQ��AP�!AO�hAOp�AOl�AN��AM"�AL-AK�TAKC�AJ�`AJ��AJ �AH�\AG?}AE�hAD-AC�mAC"�ABv�AB1AA
=A@bA?�7A>r�A=��A=�-A="�A<ZA:�9A:�A9XA7�^A69XA4�!A3�A3C�A2ĜA2jA1|�A0�uA/�A/oA. �A-`BA-+A-VA,��A,�\A+�A*A�A)�wA)A(9XA'��A'��A&��A&I�A&$�A%x�A$�uA#�A#`BA#S�A#A"�A!�;A!VA E�A�^A/AffA��A;dAM�AoA
=AjA��AXAĜA{Ap�AI�A�
AG�Az�A�FA�7A�uA&�A�^AAz�A(�AJA  A�AA�A?}A
�9A	�A�AbA��AXA�uAjAA��AK�Ar�A"�A ��@���@���@�1@��@���@�1@���@���@�+@��#@�@�A�@��
@�R@���@�V@�9X@�S�@�@�\@��;@�V@�%@���@��#@�r�@�|�@���@ޏ\@�r�@��@�I�@׾w@֧�@�$�@�G�@�V@��/@ԣ�@�A�@�=q@�/@��m@Χ�@�p�@�
=@ȃ@�K�@�1'@��D@���@���@�M�@�E�@��@���@�7L@��@��`@���@�z�@��m@�|�@���@�Ĝ@���@��7@��@�1'@��P@�33@��H@���@�~�@�v�@�ff@�=q@��@���@���@�X@� �@�33@�@�`B@�G�@�7L@�%@��u@�33@�J@���@�(�@��@��@�\)@�@���@�$�@��T@�p�@���@��;@�@�V@�X@��@��w@�~�@��@���@���@��h@�7L@�bN@�b@��F@��+@�@��7@��D@��@�S�@���@�5?@�@�hs@��/@�1'@�S�@��+@��T@��`@���@��u@��@�I�@��@��@��P@��P@�|�@�l�@�l�@�;d@��H@�^5@��T@��@�`B@�G�@��@��`@���@���@�r�@�bN@�bN@�(�@�b@���@��;@���@���@�t�@�dZ@�;d@�"�@��@���@��@���@��!@���@�n�@�ff@�^5@�@�|�@�^5@�=q@�5?@�{@��T@���@���@��@�z�@�Q�@���@��@�K�@���@�ȴ@���@�~�@�ff@�-@��#@�`B@���@���@���@�Z@�@;d@~�@}��@}O�@|�/@|��@|�j@|�j@|�j@|�@|z�@|Z@|�@{33@z��@z^5@y�7@x��@x�@xr�@xr�@xbN@xQ�@x �@w�w@w|�@wl�@w+@v��@vV@u�-@uO�@u/@uV@t��@t��@t�D@tZ@tI�@t9X@t(�@s��@s�m@sƨ@sƨ@s�F@s��@r�@rJ@qhs@o�P@o\)@o
=@nȴ@nV@nE�@n{@l��@lj@k��@k�@kdZ@kS�@k"�@j�!@j~�@i��@ix�@i7L@i%@i%@h��@hbN@g�@g�;@g�@h  @h1'@h  @g�@g��@gK�@f��@f�+@fE�@fE�@e�T@ep�@ep�@e?}@e/@e/@d�@dj@d9X@d(�@cƨ@b��@`�@_+@^�y@^ȴ@^�R@^��@^��@^v�@^ff@^V@^E�@^E�@^E�@^5?@^5?@^$�@^@^@^@]�@]@]`B@\�/@\1@[t�@[C�@["�@Z�H@Z~�@YG�@XA�@W�w@W�P@W;d@V�y@Vȴ@Vv�@V5?@U�T@U��@U`B@T��@TZ@S�
@S��@SS�@R�@R�\@Rn�@RM�@R-@Q��@Q��@Q�^@Q�7@Qhs@Q%@P �@O�w@O�@O��@O�P@O�P@O\)@O
=@N�+@M�T@M��@M�@M/@L�@L9X@L�@K��@KS�@KC�@K33@Ko@J��@I��@IX@I7L@H�9@HbN@H �@G��@GK�@G;d@G;d@G+@F��@F�y@F�+@FE�@F@E��@D�@D(�@C�
@C�F@Ct�@C33@C@B�H@B�H@B��@B��@B��@B^5@A�@A��@A7L@A�@@��@@A�@@b@?�@?�;@?�w@?��@?\)@?K�@?;d@?
=@>�R@>v�@>E�@>@=��@=��@=`B@=/@<�j@<(�@;�
@;��@;��@;��@;�@;�@;33@9��@9��@9X@9�@9%@8�`@8��@8r�@8r�@8Q�@8b@7��@7�@7l�@6��@6��@6ff@6V@65?@5�h@5�@4��@4j@4Z@4Z@49X@3�m@3��@3t�@3S�@3"�@3@2��@2^5@1�#@1x�@1�@0��@0��@01'@/�@/�P@/|�@/;d@.ȴ@.5?@-�T@-�@-?}@,��@,�D@,��@,z�@,j@,(�@,1@+��@+t�@+33@+o@*��@*~�@*M�@)��@(��@(�u@(Q�@(1'@(1'@(b@'�;@'|�@'K�@';d@'
=@&�y@&�@&�+@%�T@%�h@%/@$�@$�@$I�@#��@#�F@#��@#��@#��@#��@#S�@"��@"-@"J@!��@!�@!��@!x�@!�@ bN@ b@��@;d@+@�R@v�@�@�@`B@/@�@�D@j@(�@��@�
@ƨ@��@t�@t�@dZ@dZ@S�@"�@�@�!@^5@J@J@��@��@��@��@��@��@�7@x�@hs@hs@X@G�@7L@�@��@��@bN@1'@  @�@�P@|�@K�@�y@��@��@�+@V@{@�@@O�@�@�j@�D@Z@�@��@�m@ƨ@�F@��@�@t�@S�@"�@�H@��@M�@-@�@�@�^@��@hs@G�@G�@G�@G�@7L@7L@&�@�@��@�`@��@��@��@Ĝ@��@�u@Q�@ �@b@  @  @�@�w@|�@K�@+@�@�y@�@�R@v�@E�@@�h@`B@/@�@��@�@��@Z@I�@9X@1@�@"�@@
�H@
��@
�!@
��@
~�@
^5@
�@	�@	�7@	�7@	x�@	hs@	X@	G�@	7L@	7L@	&�@	%@��@��@��@�@r�@1'@  @��@�w@�P@l�@;d@�y@ȴ@�R@V@@`B@/@�@�/@�D@j@9X@�@1@��@�m@�
@ƨ@��@�@�@�@��@��@��@��@��@��@��@C�@@�H@��@��@~�@^5@-@�@-@�@�@�#@G�@&�@�@%@ ��@ �9@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��+A�t�A�r�A�v�A�r�A�VA�ZA�VA�9XA���A��-A�z�A�=qA���A���A��uA�t�A�hsA�VA�;dA�oA�A��A��!A�p�A�bNA�S�A� �A�7LA��/A��\A�+A��;A���A�9XA�M�A���A��A���A���A�t�A�  A�`BA��FA�bNA��#A�G�A�/A��A�S�A���A�;dA��PA���A�z�A�dZA���A�hsA���A�n�A�
=A�&�A���A���A��/A���A��A��yA�I�A�^5A���A��RA�;dA��
A�r�A�+A�9XA��hA�(�A��#A��RA��A�C�A�TA}�A{�#A{�Az��Az{Axv�Aw�PAw
=Av��Av�Au�AuoAt�Aq�-Aq
=Apr�Ao�Ao�PAn�An{Am�AmVAlbNAk�Ak?}Aj�HAjv�Aj  Ah��Ag\)Ae�Ad{Ac?}A`�HA_ƨA^�A]7LA\=qAZ��AY��AYXAY+AXĜAW�;AU?}AQ��AP�!AO�hAOp�AOl�AN��AM"�AL-AK�TAKC�AJ�`AJ��AJ �AH�\AG?}AE�hAD-AC�mAC"�ABv�AB1AA
=A@bA?�7A>r�A=��A=�-A="�A<ZA:�9A:�A9XA7�^A69XA4�!A3�A3C�A2ĜA2jA1|�A0�uA/�A/oA. �A-`BA-+A-VA,��A,�\A+�A*A�A)�wA)A(9XA'��A'��A&��A&I�A&$�A%x�A$�uA#�A#`BA#S�A#A"�A!�;A!VA E�A�^A/AffA��A;dAM�AoA
=AjA��AXAĜA{Ap�AI�A�
AG�Az�A�FA�7A�uA&�A�^AAz�A(�AJA  A�AA�A?}A
�9A	�A�AbA��AXA�uAjAA��AK�Ar�A"�A ��@���@���@�1@��@���@�1@���@���@�+@��#@�@�A�@��
@�R@���@�V@�9X@�S�@�@�\@��;@�V@�%@���@��#@�r�@�|�@���@ޏ\@�r�@��@�I�@׾w@֧�@�$�@�G�@�V@��/@ԣ�@�A�@�=q@�/@��m@Χ�@�p�@�
=@ȃ@�K�@�1'@��D@���@���@�M�@�E�@��@���@�7L@��@��`@���@�z�@��m@�|�@���@�Ĝ@���@��7@��@�1'@��P@�33@��H@���@�~�@�v�@�ff@�=q@��@���@���@�X@� �@�33@�@�`B@�G�@�7L@�%@��u@�33@�J@���@�(�@��@��@�\)@�@���@�$�@��T@�p�@���@��;@�@�V@�X@��@��w@�~�@��@���@���@��h@�7L@�bN@�b@��F@��+@�@��7@��D@��@�S�@���@�5?@�@�hs@��/@�1'@�S�@��+@��T@��`@���@��u@��@�I�@��@��@��P@��P@�|�@�l�@�l�@�;d@��H@�^5@��T@��@�`B@�G�@��@��`@���@���@�r�@�bN@�bN@�(�@�b@���@��;@���@���@�t�@�dZ@�;d@�"�@��@���@��@���@��!@���@�n�@�ff@�^5@�@�|�@�^5@�=q@�5?@�{@��T@���@���@��@�z�@�Q�@���@��@�K�@���@�ȴ@���@�~�@�ff@�-@��#@�`B@���@���@���@�Z@�@;d@~�@}��@}O�@|�/@|��@|�j@|�j@|�j@|�@|z�@|Z@|�@{33@z��@z^5@y�7@x��@x�@xr�@xr�@xbN@xQ�@x �@w�w@w|�@wl�@w+@v��@vV@u�-@uO�@u/@uV@t��@t��@t�D@tZ@tI�@t9X@t(�@s��@s�m@sƨ@sƨ@s�F@s��@r�@rJ@qhs@o�P@o\)@o
=@nȴ@nV@nE�@n{@l��@lj@k��@k�@kdZ@kS�@k"�@j�!@j~�@i��@ix�@i7L@i%@i%@h��@hbN@g�@g�;@g�@h  @h1'@h  @g�@g��@gK�@f��@f�+@fE�@fE�@e�T@ep�@ep�@e?}@e/@e/@d�@dj@d9X@d(�@cƨ@b��@`�@_+@^�y@^ȴ@^�R@^��@^��@^v�@^ff@^V@^E�@^E�@^E�@^5?@^5?@^$�@^@^@^@]�@]@]`B@\�/@\1@[t�@[C�@["�@Z�H@Z~�@YG�@XA�@W�w@W�P@W;d@V�y@Vȴ@Vv�@V5?@U�T@U��@U`B@T��@TZ@S�
@S��@SS�@R�@R�\@Rn�@RM�@R-@Q��@Q��@Q�^@Q�7@Qhs@Q%@P �@O�w@O�@O��@O�P@O�P@O\)@O
=@N�+@M�T@M��@M�@M/@L�@L9X@L�@K��@KS�@KC�@K33@Ko@J��@I��@IX@I7L@H�9@HbN@H �@G��@GK�@G;d@G;d@G+@F��@F�y@F�+@FE�@F@E��@D�@D(�@C�
@C�F@Ct�@C33@C@B�H@B�H@B��@B��@B��@B^5@A�@A��@A7L@A�@@��@@A�@@b@?�@?�;@?�w@?��@?\)@?K�@?;d@?
=@>�R@>v�@>E�@>@=��@=��@=`B@=/@<�j@<(�@;�
@;��@;��@;��@;�@;�@;33@9��@9��@9X@9�@9%@8�`@8��@8r�@8r�@8Q�@8b@7��@7�@7l�@6��@6��@6ff@6V@65?@5�h@5�@4��@4j@4Z@4Z@49X@3�m@3��@3t�@3S�@3"�@3@2��@2^5@1�#@1x�@1�@0��@0��@01'@/�@/�P@/|�@/;d@.ȴ@.5?@-�T@-�@-?}@,��@,�D@,��@,z�@,j@,(�@,1@+��@+t�@+33@+o@*��@*~�@*M�@)��@(��@(�u@(Q�@(1'@(1'@(b@'�;@'|�@'K�@';d@'
=@&�y@&�@&�+@%�T@%�h@%/@$�@$�@$I�@#��@#�F@#��@#��@#��@#��@#S�@"��@"-@"J@!��@!�@!��@!x�@!�@ bN@ b@��@;d@+@�R@v�@�@�@`B@/@�@�D@j@(�@��@�
@ƨ@��@t�@t�@dZ@dZ@S�@"�@�@�!@^5@J@J@��@��@��@��@��@��@�7@x�@hs@hs@X@G�@7L@�@��@��@bN@1'@  @�@�P@|�@K�@�y@��@��@�+@V@{@�@@O�@�@�j@�D@Z@�@��@�m@ƨ@�F@��@�@t�@S�@"�@�H@��@M�@-@�@�@�^@��@hs@G�@G�@G�@G�@7L@7L@&�@�@��@�`@��@��@��@Ĝ@��@�u@Q�@ �@b@  @  @�@�w@|�@K�@+@�@�y@�@�R@v�@E�@@�h@`B@/@�@��@�@��@Z@I�@9X@1@�@"�@@
�H@
��@
�!@
��@
~�@
^5@
�@	�@	�7@	�7@	x�@	hs@	X@	G�@	7L@	7L@	&�@	%@��@��@��@�@r�@1'@  @��@�w@�P@l�@;d@�y@ȴ@�R@V@@`B@/@�@�/@�D@j@9X@�@1@��@�m@�
@ƨ@��@�@�@�@��@��@��@��@��@��@��@C�@@�H@��@��@~�@^5@-@�@-@�@�@�#@G�@&�@�@%@ ��@ �9@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��BȴBB�}B��B�jB�dB�}B�wB��BŢBÖB��B�wB�}B�qB�FB�'B�?B�B��B�+B�PB�VB�B�Bz�Bk�BVBK�BO�BgmBbNBR�BH�B33B+B$�BuB�`B�mB��B�NB��B��B�B��B�B�7B��B�oB�%B}�Br�BQ�B33B
��BoB+BB
��B
�mB
��B
�?B
��B
�wB
��B
�3B
�B
�hB
�%B
m�B
�=B
�DB
�B
iyB
9XB
7LB
?}B
R�B
I�B
B�B
)�B
1'B
6FB
6FB
0!B
"�B
{B
+B	�B
B
B
B	��B	�B	�B	�B	�B	�B	�yB	�NB	�NB	�)B	��B	�wB	�3B	�B	��B	��B	~�B	�1B	� B	|�B	}�B	u�B	w�B	}�B	}�B	q�B	VB	1'B		7B	#�B	$�B	6FB	6FB	)�B	�B	�B	1'B	+B	/B	(�B	�B	B	B�B��B	oB	+B	B	B�B�B��B�B��B��B�B�/B��B��B��B�FB�9B�-B�wB�jB��B�jB�!B��B��B�B��B��B�9B�3B�B��B�uB�DB��B��B�hB��B��B�VB�=B�hB�%B|�B� B�=B�PB�Bt�B}�Bs�Bl�Bp�Bn�BffBjB\)BS�BE�B9XBVBZBW
BT�BK�BM�BA�BL�BH�BB�B?}BF�B33B'�B)�B;dBD�BH�BM�BM�BL�BI�BC�BB�B8RB0!B(�B#�B�B�B(�B6FB1'B+B+B�BVB�B�BPB{B�B�BJB�BhBBbB�B!�B�B�B�B�B�B�B�BhB��B+BPB
=BB	7BhB{BVB  B��B1B�B�B�B�B!�B�B�B{BB	7B1B	7BB��B��BB��B�B'�B(�B49B9XB6FB6FB5?B8RB:^B8RB33B.B-B#�B�B�B/B;dBA�BC�BG�BJ�BL�BN�BO�BN�BL�BI�BJ�BJ�BD�B8RBA�BB�BN�BVBT�BP�BI�B@�BG�BL�B^5BbNBaHB`BB`BB_;B`BBaHB_;B\)B]/B]/BdZB_;BffBhsBk�By�B}�B�B�B|�Bz�B�B�Bz�B�B�JB�B�DB�{B�{B��B��B��B��B��B��B��B��B�B�jB�dB�wB�qB�wB�qBÖBŢBĜBĜBĜBB��B��BĜBȴB��B��B��B��B��B��B�
B�B�B�B�/B�5B�5B�BB�;B�NB�ZB�ZB�fB�sB�sB�yB�B�B�B�B�B�yB�BB��B�B	  B	B	B	B	B	
=B	�B	�B	�B	�B	�B	!�B	'�B	.B	1'B	49B	49B	33B	33B	6FB	;dB	D�B	E�B	F�B	H�B	P�B	R�B	R�B	YB	\)B	`BB	aHB	aHB	aHB	aHB	aHB	bNB	cTB	bNB	iyB	l�B	l�B	q�B	u�B	y�B	z�B	z�B	z�B	z�B	� B	�B	�B	�B	�%B	�+B	�JB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�'B	�3B	�FB	�RB	�XB	�^B	�jB	��B	��B	��B	B	��B	B	B	B	ÖB	ǮB	ǮB	ȴB	ǮB	ƨB	ɺB	ȴB	ȴB	��B	ɺB	ɺB	��B	��B	��B	ǮB	ŢB	��B	�)B	�;B	�;B	�BB	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�HB	�HB	�NB	�HB	�BB	�;B	�5B	�5B	�;B	�TB	�fB	�fB	�`B	�ZB	�HB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B
B
B
B
%B
B
B
B
  B
B
1B
+B
1B
	7B

=B

=B
VB
VB
VB
PB
PB
JB
PB
PB
JB

=B
VB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
{B
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
!�B
!�B
 �B
�B
�B
#�B
#�B
#�B
%�B
%�B
$�B
%�B
&�B
%�B
%�B
%�B
&�B
%�B
%�B
&�B
(�B
(�B
(�B
%�B
'�B
)�B
,B
.B
.B
-B
,B
-B
/B
.B
.B
/B
-B
.B
.B
.B
0!B
1'B
1'B
1'B
1'B
49B
49B
33B
2-B
1'B
5?B
49B
6FB
5?B
9XB
:^B
9XB
8RB
8RB
8RB
9XB
7LB
8RB
:^B
8RB
9XB
9XB
6FB
7LB
:^B
<jB
=qB
>wB
=qB
<jB
<jB
>wB
>wB
=qB
>wB
>wB
<jB
;dB
=qB
>wB
?}B
@�B
?}B
@�B
A�B
C�B
C�B
C�B
B�B
A�B
?}B
B�B
D�B
D�B
D�B
C�B
C�B
C�B
A�B
C�B
D�B
F�B
H�B
F�B
G�B
F�B
F�B
I�B
I�B
H�B
I�B
K�B
K�B
L�B
L�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
K�B
K�B
K�B
N�B
M�B
L�B
N�B
O�B
O�B
O�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
P�B
Q�B
P�B
O�B
P�B
R�B
R�B
Q�B
R�B
S�B
R�B
R�B
R�B
T�B
VB
VB
VB
XB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
XB
XB
YB
ZB
YB
ZB
ZB
[#B
\)B
\)B
]/B
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
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
]/B
]/B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
]/B
]/B
_;B
_;B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
`BB
_;B
aHB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
cTB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
ffB
ffB
hsB
jB
k�B
jB
jB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
o�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
n�B
m�B
p�B
q�B
q�B
q�B
p�B
q�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B� B�B��B�B�.B� B�B�B�GB�4B� B�"B�B� B�B��B��B��B��B��B��B��B��B��B�tB�cB��B�B�"B�B�B��B{�Bl�BXyBN�BQ�Bg�Bb�BS�BI�B4�B,qB%�BB��B��B�tB��B��B�B��B��B��B�xB��B��B�_BcBtBT{B6zB
��BaB	7BMB
�LB
�*B
�VB
��B
��B
�}B
��B
�nB
� B
��B
�B
poB
��B
��B
��B
k6B
<�B
:B
A;B
SuB
J�B
C{B
,B
2-B
6�B
6�B
0�B
#�B
�B
�B	�cB
�B
�B
�B	��B	��B	��B	�B	�B	�}B	�0B	�:B	��B	��B	��B	�iB	��B	��B	��B	��B	��B	��B	��B	~�B	HB	w�B	x�B	~�B	~]B	r�B	W�B	4�B	�B	%FB	&2B	6�B	6�B	+B	�B	�B	1�B	,B	/�B	)�B	�B	aB	�B�B�qB	�B	fB	B	�B�%B�B��B��B�tB�`B�BބB̳B��B�B��B�FB�B�cB��B�AB�<B�vB�kB�_B��B�*B��B��B��B�}B��B�B�B�mB��B�oB�	B�9B�BB�DB��B�+B~]B�B��B��B��BvB~�Bt�Bm�BqvBo�Bg�BkB]~BU�BG�B<BV�B[	BW�BU�BL�BN�BC-BM�BI�BC�B@�BGzB4�B)�B+�B<jBEmBI7BNBN"BMBJ#BD3BC-B9rB1[B*�B%FB�B=B)�B6�B1�B+�B+�BBHBpB�B�B�BdBsB�B9BTBB4BkB"4B;B_B1BBBB	B B��BKB<BDB�B
=B B�BB�B��B	RB$BSB#BQB"4B 'BB�BmB
#B	7B
	BGB��B��B3B�B�B(sB)�B4�B9rB6�B6�B5�B8�B:�B8�B3�B.�B-�B$�B5B BB0B<BA�BDBG�BK)BMBN�BO�BOBMBJ#BKBJ�BEB9XBBABC�BO\BV9BUMBQNBJXBA�BH�BM�B^�Bb�Ba�B`�B`�B_�B`�Ba�B_�B\�B^B^Bd�B`'BgBi*BlqBzDB~BB�UB�UB}qB{�B�uB�{B{�B��B��B�B��B��B�B�B�B�B�=B�KB��B��B�B��B�jB��B��B��B��B��B��BżB��B��BĶB��B��B� B�B�B�B�B�B�4B�B�FB�?B�KB�7B�KB�IB�OB�OB�vBߊB�B�B�B�B�B��B��B��B�B�B�B�B��B��B��B�QB	 4B	AB	AB	GB	�B	
�B	�B	�B	�B	�B	!B	"4B	(XB	.IB	1[B	4nB	4nB	3hB	3�B	6�B	;�B	D�B	E�B	GB	I7B	Q4B	S&B	S[B	YeB	\]B	`\B	abB	abB	a|B	abB	a|B	b�B	c�B	b�B	i�B	l�B	l�B	q�B	vB	y�B	z�B	{B	z�B	{B	�OB	�GB	�SB	�SB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	��B	��B	��B	�B	��B	�B	�;B	�B	�2B	�>B	�6B	�6B	�B	�KB	�=B	�qB	�[B	�hB	�zB	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	�B	��B	�)B	�1B	ƎB	�TB	�]B	�pB	�pB	�\B	�pB	�vB	�HB	�bB	�bB	�HB	�bB	�bB	�hB	�|B	�bB	�hB	�bB	�vB	�pB	ބB	ބB	ߤB	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	��B	�	B	��B	�B	�B	�B	��B	��B	�B	�B	�(B	�"B	�6B	�0B	�6B
 4B
 B
 OB
 B	�.B
GB
[B
MB
YB
9B
SB
[B
 iB
gB
fB
zB
fB
	lB

rB

rB
pB
VB
�B
�B
jB
�B
�B
�B
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
!�B
!�B
 �B
B
B
$B
$B
$B
%�B
&B
$�B
&B
'B
%�B
&B
%�B
'B
&B
&2B
'B
)*B
)*B
)B
&2B
(>B
*KB
,=B
./B
.B
-)B
,=B
-CB
/OB
./B
.IB
/5B
-]B
.IB
.IB
.cB
0UB
1[B
1AB
1[B
1[B
4TB
4TB
3hB
2|B
1�B
5tB
4nB
6zB
5�B
9rB
:xB
9rB
8�B
8�B
8�B
9�B
7�B
8�B
:xB
8�B
9�B
9�B
6�B
7�B
:xB
<�B
=�B
>�B
=�B
<�B
<�B
>�B
>�B
=�B
>�B
>�B
<�B
;�B
=�B
>�B
?�B
@�B
?�B
@�B
A�B
C�B
C�B
C�B
B�B
A�B
?�B
B�B
D�B
D�B
D�B
C�B
C�B
C�B
A�B
C�B
D�B
F�B
H�B
F�B
G�B
F�B
F�B
I�B
I�B
H�B
I�B
K�B
K�B
MB
MB
M�B
MB
MB
M�B
M�B
M�B
M�B
L�B
L�B
K�B
K�B
K�B
N�B
M�B
MB
N�B
O�B
O�B
O�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
OB
OB
OB
NB
OB
PB
PB
QB
R B
QB
P.B
QB
SB
S&B
RB
S&B
T,B
S&B
S@B
S@B
U2B
VB
VB
VB
X+B
X+B
X+B
X+B
XB
XEB
XEB
X+B
XEB
W?B
X+B
XEB
Y1B
ZB
YKB
ZQB
Z7B
[WB
\CB
\CB
]IB
\CB
\CB
\CB
\CB
\)B
\]B
\CB
]IB
]/B
]IB
\]B
\]B
\]B
\]B
\CB
]IB
^5B
^OB
]dB
]dB
\]B
]IB
^jB
^OB
^jB
^jB
^jB
]dB
^jB
]dB
]dB
_pB
_pB
_pB
aHB
a|B
a|B
abB
abB
a|B
`\B
_�B
a|B
c�B
d�B
d�B
dZB
dtB
d�B
c�B
cnB
d�B
c�B
f�B
f�B
f�B
g�B
gmB
g�B
gmB
gmB
f�B
g�B
gmB
g�B
g�B
hsB
g�B
g�B
h�B
h�B
h�B
g�B
g�B
g�B
h�B
h�B
f�B
f�B
h�B
j�B
k�B
j�B
j�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
o�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
o�B
n�B
m�B
p�B
q�B
q�B
q�B
p�B
q�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804240032532018042400325320180424003253201806221240432018062212404320180622124043201804271406122018042714061220180427140612  JA  ARFMdecpA19c                                                                20180420093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180420003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180420003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180420003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180420003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180420003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180420003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180420003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180420003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180420003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20180420005705                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180420153710  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180423153253  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180423153253  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050612  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034043  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                