CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:36Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20230616190836  20230616190836  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @��Z�[�1   @��[s���@+o��-V�d�"��`B1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@}p�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��B��RB��B��B��B��RB��B��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw��Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
Db�Dcw
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
Do�pDpw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D��RD�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�D�RD���D�>�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�dZA�ffA�hsA�hsA�jA�jA�l�A�jA�jA�l�A�l�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�t�A�v�A�jA�dZA�bNA�^5A�bNA�\)A�XA�XA�Q�A�I�A�I�A�G�A�E�A�A�A�;dA�5?A�/A�(�A��A���A�t�A�v�AĮAA�\)A��A���A�`BA��yA��A��A��+A�C�A���A��uA��A��\A���A���A��A�1'A���A�A�A�`BA���A��wA�1'A��`A�~�A��A�Q�A�x�A��A�G�A���A��FA�  A��hA���A���A�ĜA��9A��A��A�ZA~n�A{��Azr�Av~�Aq��Ap1AnĜAlĜAl1Ag��A]��AVM�AR��AO�wAN�\ALM�AGhsAC�FAA�-A>ȴA=t�A<r�A;�FA:�A9`BA8��A8A�A6��A6�DA5�A5�A5`BA5
=A4��A4n�A3��A2�RA0�/A/S�A.ȴA.~�A. �A-�A-�hA,�A+�A*ĜA)C�A'�TA'��A&��A%A%�-A%�hA%dZA%7LA%�A$~�A$�A!�
A!
=A A�A�A-A�AȴAI�AC�A�TAt�A;dA
=A��A�AAAE�A\)AjAp�A�A��AjA-A�A�
A��A`BAoAJAhsA�A��A�Ar�A-A�AĜA��A�uA$�A�
AhsA�A
�!A
5?A	�mA	�#A	�#A	��A	S�A	K�A	C�A	C�A	+A�A1'A�
AhsA&�A�!Ar�AE�A�A1A  A��A�A�#AA�-A��A��A�Al�A�uA�FA�A^5A  A�;A��A��A��A\)A�A �A Q�A 1@�\)@���@�n�@�Q�@���@�S�@��\@�E�@�X@���@���@�V@�%@���@�bN@�C�@���@�X@�bN@��
@�ff@홚@�/@�9@�9X@��m@�ȴ@�X@��@�9X@�ƨ@�M�@�%@��;@㝲@�\)@��@��@◍@�7@�  @ߥ�@�C�@��@�v�@��#@�X@���@�Z@�+@�~�@�@���@�j@�  @�|�@�C�@�^5@�@���@�@պ^@ՙ�@ԣ�@��;@�S�@�"�@�@�o@҇+@��@�@Ѻ^@�7L@��@��`@ЋD@�I�@�9X@��@�  @��;@ϕ�@�S�@���@���@ΰ!@�~�@�5?@͉7@˾w@���@�=q@���@�@ɑh@�G�@��@�%@��@���@ț�@�I�@Ǿw@�S�@�@Ə\@�$�@��@ũ�@�X@ģ�@��@�K�@+@��-@�p�@�?}@�%@���@�A�@�  @��m@��w@���@�C�@�@�ff@��@��#@��-@���@�&�@�Q�@�b@��
@�t�@�C�@��@���@�5?@���@���@���@�C�@��R@��+@�ff@��-@�7L@��u@���@��@�|�@�K�@�
=@���@��@���@��j@���@�r�@�A�@��@�ƨ@�l�@��@���@���@��T@�7L@���@���@��j@�bN@� �@�  @���@��F@���@��@��@���@�~�@�M�@�J@��T@��-@���@��h@�G�@��j@�1@�dZ@�
=@���@���@�^5@�@�p�@�V@�Ĝ@��@��m@���@��@�|�@�|�@��@��@�t�@�@�ȴ@�~�@�E�@��@�@��7@�x�@�p�@�?}@�/@��@��@��9@�r�@�A�@��@��m@��w@���@�33@���@�~�@��@�@��7@��@�`B@�V@���@��@��@�=q@��@�7L@��`@���@�Ĝ@��9@��@�Z@�1@��;@��w@���@�|�@�t�@�S�@�33@�o@��!@�-@��@���@��T@���@�`B@���@��@�9X@��@��m@�dZ@��@��+@�J@��^@�G�@�%@��j@�z�@�I�@�  @��w@�|�@�dZ@�K�@�"�@��@��!@�~�@�ff@�E�@�@��T@���@�hs@�&�@�%@���@��D@�z�@�(�@��;@���@��@���@�V@���@��#@��h@�x�@�p�@�G�@���@��9@�z�@�Z@�(�@��m@��@�+@��H@���@�n�@��@��@���@�hs@�O�@�V@��j@��D@�z�@�Q�@�1@��w@���@��@�K�@�@��@���@���@���@��\@�~�@�ff@�M�@�5?@�-@�{@�@��T@��7@�/@��@���@�z�@�Z@�A�@�1@;d@~��@~ff@}��@}�-@}V@|�@|�@|(�@{�
@{��@{�@{t�@{S�@{@z^5@y�#@x��@xQ�@xb@w�P@vv�@v5?@u��@uO�@t�j@sƨ@rn�@r=q@q�^@p�@p�9@p �@n��@nE�@m��@m�@k�
@kdZ@j�@j�!@j~�@j�@i�^@ihs@i%@hbN@hQ�@g�@gl�@g\)@f�y@e�@d�@dz�@d1@cƨ@cC�@c33@c33@c"�@b��@b�@a�@a��@a��@`��@`A�@_�@^��@^{@]�@^@]O�@\��@\�D@\9X@[�F@[t�@[33@Z�H@Z�!@Z�\@ZJ@Y��@Yhs@Yhs@Y&�@Xr�@W�@W+@V�R@Vv�@Vff@VV@V$�@V{@U�@UO�@T�/@TI�@S�m@S�F@So@R��@R�!@R�\@R�\@R�\@Rn�@R-@Q��@P��@Pr�@PbN@PQ�@Pb@O�@O�w@O|�@OK�@O+@N��@N5?@M�h@L��@L�@L�@L�D@LZ@L�@K�m@K�F@K��@J�@JJ@I�@I�#@I�#@I��@I�7@H��@H�9@H��@Hr�@H1'@H �@Hb@G�@G��@G��@Gl�@G�@F�R@Fff@F{@E�@EO�@E�@D�/@D��@Dj@D(�@C��@Cƨ@C�@B��@B=q@A��@A�^@A��@A��@AX@@bN@@  @?�;@?�@?��@?K�@>v�@>{@>@=@=��@=�@=�@=p�@=O�@=V@<��@;��@;�
@;�F@;C�@:�\@:�@9�@9��@9%@8�`@8��@8�9@8�u@81'@7�w@7|�@7+@6ȴ@6�+@6E�@6$�@6{@6@5�T@5�-@5/@4�D@4(�@41@3�
@3��@3S�@333@3"�@3@2��@2n�@2=q@2-@1��@1hs@17L@0��@0r�@0Q�@0  @/�w@/l�@/K�@.��@.ȴ@.�+@.$�@.@-@-�@-p�@-`B@-�@,�D@,9X@,1@+��@+�m@+�m@+�
@+�
@+��@+�@+"�@*�@*n�@)�#@)�7@)hs@)&�@)�@)�@(��@(��@(��@(bN@(A�@(  @'�@'\)@'
=@&��@&V@&5?@&$�@&$�@%��@%�-@%�h@%O�@%/@$�@$�j@$�@$�D@$9X@$1@#ƨ@#��@#�@#�@#dZ@#33@#"�@#o@#@"�@"��@"n�@"M�@"-@!�#@!��@!x�@!G�@!%@ ��@ r�@ b@�@��@��@l�@
=@��@ȴ@ȴ@��@v�@$�@�@@p�@�@�/@�j@Z@(�@�@ƨ@��@t�@dZ@33@��@�!@�\@~�@M�@-@J@J@J@��@�@�^@�7@hs@7L@��@��@�u@�u@�@A�@  @  @  @�@�w@��@\)@\)@;d@;d@+@��@ȴ@�R@�R@��@ff@E�@{@��@`B@/@�@Z@�m@ƨ@��@�@33@"�@o@�H@��@�\@=q@�#@�^@&�@�`@Ĝ@r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�dZA�ffA�hsA�hsA�jA�jA�l�A�jA�jA�l�A�l�A�p�A�p�A�r�A�r�A�t�A�t�A�t�A�t�A�v�A�jA�dZA�bNA�^5A�bNA�\)A�XA�XA�Q�A�I�A�I�A�G�A�E�A�A�A�;dA�5?A�/A�(�A��A���A�t�A�v�AĮAA�\)A��A���A�`BA��yA��A��A��+A�C�A���A��uA��A��\A���A���A��A�1'A���A�A�A�`BA���A��wA�1'A��`A�~�A��A�Q�A�x�A��A�G�A���A��FA�  A��hA���A���A�ĜA��9A��A��A�ZA~n�A{��Azr�Av~�Aq��Ap1AnĜAlĜAl1Ag��A]��AVM�AR��AO�wAN�\ALM�AGhsAC�FAA�-A>ȴA=t�A<r�A;�FA:�A9`BA8��A8A�A6��A6�DA5�A5�A5`BA5
=A4��A4n�A3��A2�RA0�/A/S�A.ȴA.~�A. �A-�A-�hA,�A+�A*ĜA)C�A'�TA'��A&��A%A%�-A%�hA%dZA%7LA%�A$~�A$�A!�
A!
=A A�A�A-A�AȴAI�AC�A�TAt�A;dA
=A��A�AAAE�A\)AjAp�A�A��AjA-A�A�
A��A`BAoAJAhsA�A��A�Ar�A-A�AĜA��A�uA$�A�
AhsA�A
�!A
5?A	�mA	�#A	�#A	��A	S�A	K�A	C�A	C�A	+A�A1'A�
AhsA&�A�!Ar�AE�A�A1A  A��A�A�#AA�-A��A��A�Al�A�uA�FA�A^5A  A�;A��A��A��A\)A�A �A Q�A 1@�\)@���@�n�@�Q�@���@�S�@��\@�E�@�X@���@���@�V@�%@���@�bN@�C�@���@�X@�bN@��
@�ff@홚@�/@�9@�9X@��m@�ȴ@�X@��@�9X@�ƨ@�M�@�%@��;@㝲@�\)@��@��@◍@�7@�  @ߥ�@�C�@��@�v�@��#@�X@���@�Z@�+@�~�@�@���@�j@�  @�|�@�C�@�^5@�@���@�@պ^@ՙ�@ԣ�@��;@�S�@�"�@�@�o@҇+@��@�@Ѻ^@�7L@��@��`@ЋD@�I�@�9X@��@�  @��;@ϕ�@�S�@���@���@ΰ!@�~�@�5?@͉7@˾w@���@�=q@���@�@ɑh@�G�@��@�%@��@���@ț�@�I�@Ǿw@�S�@�@Ə\@�$�@��@ũ�@�X@ģ�@��@�K�@+@��-@�p�@�?}@�%@���@�A�@�  @��m@��w@���@�C�@�@�ff@��@��#@��-@���@�&�@�Q�@�b@��
@�t�@�C�@��@���@�5?@���@���@���@�C�@��R@��+@�ff@��-@�7L@��u@���@��@�|�@�K�@�
=@���@��@���@��j@���@�r�@�A�@��@�ƨ@�l�@��@���@���@��T@�7L@���@���@��j@�bN@� �@�  @���@��F@���@��@��@���@�~�@�M�@�J@��T@��-@���@��h@�G�@��j@�1@�dZ@�
=@���@���@�^5@�@�p�@�V@�Ĝ@��@��m@���@��@�|�@�|�@��@��@�t�@�@�ȴ@�~�@�E�@��@�@��7@�x�@�p�@�?}@�/@��@��@��9@�r�@�A�@��@��m@��w@���@�33@���@�~�@��@�@��7@��@�`B@�V@���@��@��@�=q@��@�7L@��`@���@�Ĝ@��9@��@�Z@�1@��;@��w@���@�|�@�t�@�S�@�33@�o@��!@�-@��@���@��T@���@�`B@���@��@�9X@��@��m@�dZ@��@��+@�J@��^@�G�@�%@��j@�z�@�I�@�  @��w@�|�@�dZ@�K�@�"�@��@��!@�~�@�ff@�E�@�@��T@���@�hs@�&�@�%@���@��D@�z�@�(�@��;@���@��@���@�V@���@��#@��h@�x�@�p�@�G�@���@��9@�z�@�Z@�(�@��m@��@�+@��H@���@�n�@��@��@���@�hs@�O�@�V@��j@��D@�z�@�Q�@�1@��w@���@��@�K�@�@��@���@���@���@��\@�~�@�ff@�M�@�5?@�-@�{@�@��T@��7@�/@��@���@�z�@�Z@�A�@�1@;d@~��@~ff@}��@}�-@}V@|�@|�@|(�@{�
@{��@{�@{t�@{S�@{@z^5@y�#@x��@xQ�@xb@w�P@vv�@v5?@u��@uO�@t�j@sƨ@rn�@r=q@q�^@p�@p�9@p �@n��@nE�@m��@m�@k�
@kdZ@j�@j�!@j~�@j�@i�^@ihs@i%@hbN@hQ�@g�@gl�@g\)@f�y@e�@d�@dz�@d1@cƨ@cC�@c33@c33@c"�@b��@b�@a�@a��@a��@`��@`A�@_�@^��@^{@]�@^@]O�@\��@\�D@\9X@[�F@[t�@[33@Z�H@Z�!@Z�\@ZJ@Y��@Yhs@Yhs@Y&�@Xr�@W�@W+@V�R@Vv�@Vff@VV@V$�@V{@U�@UO�@T�/@TI�@S�m@S�F@So@R��@R�!@R�\@R�\@R�\@Rn�@R-@Q��@P��@Pr�@PbN@PQ�@Pb@O�@O�w@O|�@OK�@O+@N��@N5?@M�h@L��@L�@L�@L�D@LZ@L�@K�m@K�F@K��@J�@JJ@I�@I�#@I�#@I��@I�7@H��@H�9@H��@Hr�@H1'@H �@Hb@G�@G��@G��@Gl�@G�@F�R@Fff@F{@E�@EO�@E�@D�/@D��@Dj@D(�@C��@Cƨ@C�@B��@B=q@A��@A�^@A��@A��@AX@@bN@@  @?�;@?�@?��@?K�@>v�@>{@>@=@=��@=�@=�@=p�@=O�@=V@<��@;��@;�
@;�F@;C�@:�\@:�@9�@9��@9%@8�`@8��@8�9@8�u@81'@7�w@7|�@7+@6ȴ@6�+@6E�@6$�@6{@6@5�T@5�-@5/@4�D@4(�@41@3�
@3��@3S�@333@3"�@3@2��@2n�@2=q@2-@1��@1hs@17L@0��@0r�@0Q�@0  @/�w@/l�@/K�@.��@.ȴ@.�+@.$�@.@-@-�@-p�@-`B@-�@,�D@,9X@,1@+��@+�m@+�m@+�
@+�
@+��@+�@+"�@*�@*n�@)�#@)�7@)hs@)&�@)�@)�@(��@(��@(��@(bN@(A�@(  @'�@'\)@'
=@&��@&V@&5?@&$�@&$�@%��@%�-@%�h@%O�@%/@$�@$�j@$�@$�D@$9X@$1@#ƨ@#��@#�@#�@#dZ@#33@#"�@#o@#@"�@"��@"n�@"M�@"-@!�#@!��@!x�@!G�@!%@ ��@ r�@ b@�@��@��@l�@
=@��@ȴ@ȴ@��@v�@$�@�@@p�@�@�/@�j@Z@(�@�@ƨ@��@t�@dZ@33@��@�!@�\@~�@M�@-@J@J@J@��@�@�^@�7@hs@7L@��@��@�u@�u@�@A�@  @  @  @�@�w@��@\)@\)@;d@;d@+@��@ȴ@�R@�R@��@ff@E�@{@��@`B@/@�@Z@�m@ƨ@��@�@33@"�@o@�H@��@�\@=q@�#@�^@&�@�`@Ĝ@r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�+A�-A�/A�/A�/A���A�+A�(�A�&�A�$�A�"�A��A��A��A��A�oA�VA�
=A�%A�A�  A�  A���A���A���A��A��A��A��A��A��A��yA��mA��`A��HA��/A���A�ĜA��A�|�A�A��mA�A�dZA�`BA���A��yA�$�A�r�A�7LA�~�A��hA��+A��+A�dZA�E�A�A�ƨA��!A��A�^5A�K�A�/A���A���A�z�A�`BA�^5A�ĜA���A���A���B �{B   A��uB��BN�B��B �JB��B8RBJ�B |�B �Bo�B��B#�B��B�B��B��B	��B�B �B �)B�BPBr�B��B�\B�JB�qB�HB�)B�B�fB�B��B�B1'B49B<jBP�BQ�BYB[#BaHBl�Bq�Bn�Bk�By�B��BȴB��B��B��B��BȴBǮB�qB��BP�B�'B��BĜB��B�=B�B�;B��BuB�-BdZB�LB�5B[#Bn�B� B�B�B�7B��B�B�?B�?B�'B�B�9B�FBǮB��B�B�5B�B�B��B��B  BB��B  BBBhB�B �B!�B�B�B�B#�B+B(�B%�B'�B&�B)�B+B.B49B8RB9XB9XB<jB?}B=qB;dB8RB7LB9XBB�BE�BI�BJ�BP�BR�BT�BW
BXBYBZBZB[#B[#BZBXBW
BR�BL�BS�B]/BbNBl�Bq�Bq�Bq�Bn�Bm�Bl�BjBl�Bk�BjBhsBbNB[#BjBl�Bk�Bk�BdZB`BBhsB_;BO�BK�BE�BE�BM�BR�BW
B[#Bp�B�VB��B��B�B�B�B�-B�LB�^B�XB�FB�jBĜB��BɺBɺBȴBĜB�}B�wBĜB��B�jB�LB�-B��B��B�B\)B�B�7BM�B�Bm�B
+BcTBB%�B�/B��B�B�?B�B�+B�VB�oB�{B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B�FB��BĜBƨBǮBȴB��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B��B�B�/B�BB�fB�B�B�B�B��B��B��B��B��B��B��B��BBBBBB+BVBbBbBoBhBbBVBPB�B�B �B"�B&�B&�B$�B(�B+B/B5?B6FB6FB5?B49B5?BA�BF�BH�BH�BH�BI�BI�BJ�BL�BM�BN�BM�BS�BZB\)B]/B\)B`BBcTBcTBe`BffBffBe`BhsBn�Bn�Bo�Bq�Br�Bs�Br�Bo�Bo�Bq�Bw�B|�B� B� B�B�B�B�+B�7B�=B�hB�oB��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�-B�B�B�B�B�9B�B��B�B�B�!B�'B�!B�B�'B�FB�3B�9B�FB�RB�LB�LB�LB�FB�XB�}B�qB�qB�dB�jB�jB�}BÖBĜBÖBBƨBɺB��B��B��B��B�
B�B�B�#B�)B�;B�NB�NB�NB�TB�`B�yB�mB�mB�sB�B�B�B�B�B�B�sB�B�B�B�B�B�B��B��B  BBBBBBBBBBBB%B
=BDBDBPBVB\BuBuBoB{B�B�BbBDBVBhBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B%�B&�B%�B&�B+B+B-B1'B0!B33B33B33B6FB8RB9XB9XB9XB8RB8RB9XB:^B>wBA�BA�BC�BH�BI�BI�BI�BH�BK�BQ�BP�BQ�BZBXBXB]/B_;B`BBbNBffBhsBiyBjBk�Bk�Bl�Bm�Bm�Bp�Bp�Br�Bq�Bp�Bp�Bv�By�Bz�B|�B}�B�B�B~�B}�B~�B�B�B�B� B�B�%B�B�7B�PB�PB�DB�bB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�'B�'B�?B�FB�LB�LB�LB�LB�FB�LB�RB�jB�}B�}B��B��B��BBBÖBBŢBƨBȴB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�/B�;B�BB�HB�TB�ZB�ZB�`B�`B�fB�fB�mB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B  BBBBB+B	7B1B1B+B+BJBVBPB\BbBhBoBoBoBoB{B�B�B�B�B�B"�B#�B%�B+B.B0!B33B49B5?B8RB9XB;dB?}BC�BE�BF�BG�BH�BH�BG�BJ�BO�BQ�BQ�BR�BS�BVBVBVBVBW
BXBYBXBZB\)B\)B]/B_;B_;B`BBaHBcTBbNBdZBe`Be`BgmBhsBiyBjBjBiyBjBm�Bo�Bp�Bq�Br�Br�Br�Bq�Bq�Bq�Bs�Bs�Bt�Bw�By�By�B{�B{�B{�B{�B{�B|�B|�B|�B}�B}�B� B�B�B�B�B�%B�B�+B�1B�1B�7B�=B�DB�DB�DB�JB�PB�VB�bB�hB�oB�hB�oB�uB�uB�{B�{B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�3B�3B�9B�9B�?B�FB�FB�FB�FB�FB�FB�LB�LB�RB�XB�dB�jB�jB�jB�qB�}B�}B�wB�wB�}B��BBBÖBÖBBÖBĜBĜBĜBÖBĜBĜBĜBƨBǮBƨBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444A��A���A���A���A���A���A�A���A��A��A��A��A��yA��aA��TA��HA��0A��A���A���A���A���A���A�ƨA�ĜA���A��wA��_A��RA��RA��_A��FA��:A��.A��!A��	A���A���A��]A�v�A�G�A���A��.A���A�/A�+A�`BA��:A��A�=qA�A�I�A�\)A�Q�A�Q�A�/A�cA��PA��iA�z�A��aA�(�A��A���A�`BA�`BA�E�A�+A�(�A��]A�jA�l�A���B y�A���A�^6B�GB49B�%B q�B�EB�B0!B bNB glBT�B�TB	7B�NB��B�XB�GB	�BB �B ��B��B�BXBz�Bt�Bq�B��BƨB��B�B��B�dB�EB��B�B�B!�B6EB7LB>wB@�BF�BQ�BW
BS�BP�B_;B�=B�B�EB�RB�XB�3B�B�B��B�B6EB��B�1B��B�+Bo�B�}BěB�=B��B��BI�B��BÕB@�BS�Be`BglBglBn�B� B�hB��B��B��B�{B��B��B�B�9B�dBÕB��B�B�GB�NB�`B�fB�ZB�`B�lB�lB��B  B%B+BBBB	7BbBVBCBPBIB\BbBtB�B�B�B�B!�B$�B"�B �B�B�B�B'�B+B/B0!B6EB8RB:^B<jB=pB>wB?}B?}B@�B@�B?}B=pB<jB8RB2-B9XBB�BG�BQ�BW
BW
BW
BS�BR�BQ�BO�BQ�BP�BO�BM�BG�B@�BO�BQ�BP�BP�BI�BE�BM�BD�B5?B1'B+B+B33B8RB<jB@�BVBs�B�B�IB�bB�tB�tB��B��B��B��B��B��B��B�!B�B�B�B��B��B��B��B��B��B��B��B�VB� BhrBA�BBn�B33B�bBR�B	�BH�B�lBCBB�%B�
B��BffBl�Bs�Bw�By�Bz�Bx�B}�B�B�B�B�1B�=B�=B�PB�\B�\B�\B�\B�VB�bB�bB�{B�tB�bB�PB�=B�CB��B��B��B�B�B�B�'B�-B�-B�-B�'B�'B�'B�?B�LB�RB�^B�pB�jB�jB�^B�wBBŢB��B��B�B�B�
B�B�/B�;B�;B�;B�;B�AB�GB�lB�rB�yB�yB�yB�B�B��B��B��B��B��B�B�B��BB%B1BIBIB
=BVBbB{B�B�B�B�B�B�B&�B,B.B.B.B/B/B0!B2-B33B49B33B9XB?}BA�BB�BA�BE�BH�BH�BJ�BK�BK�BJ�BM�BS�BS�BT�BW
BXBYBXBT�BT�BW
B]/BbNBe`Be`BffBffBglBl�Bn�Bo�Bv�Bw�B{�B|�B}�B|�B{�Bz�By�B|�B~�B�B�B�B�%B�7B�7B�7B�=B�=B�7B�=B�CB�PB�VB�\B�\B�\B�VB�bB�tB�{B��B��B��B��B�{B�tB�tB�hB��B�tB�\B�hB�{B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�EB�XB�jB�wB�}B��B��BěBǮBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�NB�`B�fB�B�B�rB�rB�rB�rB�yB�yB�yB�rB�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��BBBBBB  BBBBB  B  BB%B%B+BCBIBCBIBbBbBnB�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B&�B&�B(�B.B/B/B/B.B1'B7LB6EB7LB?}B=pB=pBB�BD�BE�BG�BK�BM�BN�BO�BP�BP�BQ�BR�BR�BVBVBXBW
BVBVB\)B_;B`ABbNBcTBffBffBdZBcTBdZBglBglBffBe`BglBk�BjBn�Br�Br�Bp�Bu�Bx�Bx�By�B{�B|�B}�B~�B�B� B� B�B�B�B�B�B�%B�IB�VB�bB�bB�hB�hB�bB�bB�nB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�3B�3B�3B�3B�9B�9B�9B�LB�pB�pB�wB�wB�pB�}BBěBŢBƨBȴBɺBɺB��B��B��B��B��B��B��B��B��B�
B�B�B�B�#B�)B�)B�)B�)B�AB�TB�`B�fB�lB�lB�fB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��BB1B	7BCBbBtB�B�B�B�B�B�B �B$�B(�B+B,B-B.B.B-B0!B5?B7LB7LB8RB9XB;dB;dB;dB;dB<jB=pB>wB=pB?}BA�BA�BB�BD�BD�BE�BF�BH�BG�BI�BJ�BJ�BL�BM�BN�BO�BO�BN�BO�BR�BT�BVBW
BXBXBXBW
BW
BW
BYBYBZB]/B_;B_;BaGBaGBaGBaGBaGBbNBbNBbNBcTBcTBe`BffBiyBjBjBk�BjBl�Bm�Bm�Bn�Bo�Bp�Bp�Bp�Bq�Br�Bs�Bu�Bv�Bw�Bv�Bw�Bx�Bx�By�By�Bx�Bx�B{�B{�B{�B}�B}�B~�B� B� B� B�B�B�B�B�%B�%B�1B�1B�7B�1B�7B�7B�=B�CB�CB�IB�\B�VB�VB�hB�hB�nB�tB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�3B�3B�?B�?B�?B�?B�?B�?B�LB�RB�RB�dB�pB�wB4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190836              20230616190836  AO  ARCAADJP                                                                    20230616190836    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190836    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190836  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190836  QCF$                G�O�G�O�G�O�8800            