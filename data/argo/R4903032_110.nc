CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-28T09:00:46Z creation      
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
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20210628090046  20210628090046  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               nA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @��P6�1   @���>�6@;*=p��
�c�ȴ9X1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         nA   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D)}pD)�
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
DCp�DC�
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
DQ�DRw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D澸D���D�;�D�{�D绅D���D�>�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�|�A�t�A�^5A�?}AҶFA��HAΣ�A·+A�l�A�XA�A�oA�z�A�{A���A�VA�Q�A��A��FA�K�A�A�A�bNA��jA�+A�M�A���A��uA��!A�O�A�$�A��A�Q�A��A��jA�`BA��hA�A���A�XA���A�-A���A�1'A��^A�+A��!A�hsA�{A�ƨA�?}A��TA�5?A��#A�ĜA��TA���A�^5A���A���A��wA��7A���A��FA��hA��A�E�A�VA��wA�
=A��9A���A��/A�7LA�E�A�1'A��;A��uA���A��
A���A�`BA��FA���A�
=A��^A�l�A��uA��A�hsA�1'A�&�A��TA��/A�-A�(�A~��A~A�A}��A|ȴA|1'A{��Azr�Au�Ar�Ar1'Aq�Ao�wAoVAn��Ak�Ai%Ag�7Ae�FAe|�Ad��Ac��Ab�A`��A`1'A[��AZVAX�AWt�AV1'AUC�AT��AS�FARQ�AQ
=AP�yAP�`AP�AP�AO�AO��AOt�AOO�AO�AN�uAM��ALffAKt�AI�AIK�AH  AFn�AE�AES�AD��AC�-AB��ABQ�AB�AAl�A@�RA?ƨA>r�A>(�A=�;A<�jA:�yA9��A9;dA8ĜA7��A7?}A6�A6�jA6=qA5/A4��A2�!A0��A05?A.��A.-A-��A,�\A+"�A*ffA*A)�hA(ĜA'K�A&�RA&ZA%�
A%�hA$M�A"��A"v�A"I�A"5?A!��A ��AAjAƨA��A�PAdZAG�A�+A�mA�-At�A�/A~�A�^A`BA�jAXA��A�DAr�A�A��A�mAVA��A�^A\)A�HAffA��AK�A
ȴA	At�A\)AO�AK�A?}AoA��A�A��A��A|�AhsA%A��AXA �u@�\)@��@�Z@��@�(�@�;d@��-@�Z@��H@��;@�C�@ꗍ@�G�@�u@�(�@��m@畁@�+@�!@���@�?}@�&�@�?}@��@�@�@�5?@�9X@�C�@�v�@��@ۅ@�X@�\)@֟�@�p�@��@�bN@��m@��H@щ7@��/@Ͼw@͙�@�;d@�$�@ȋD@��H@�@��#@�X@���@�dZ@���@���@�9X@��@��@�(�@��H@��@�ƨ@�V@��^@��y@�5?@��@�p�@�1@�C�@�$�@��@��7@��@��@�I�@� �@��m@���@���@��@���@�9X@�
=@�V@���@�bN@�A�@�1@�1@��m@�l�@��H@�M�@�hs@�7L@��9@�j@���@� �@�b@��;@��@��@�-@��@��-@�%@��/@��@��m@���@�\)@��@��@���@�ff@��@��@��@���@���@���@�r�@�bN@�9X@��;@���@�l�@�33@���@��!@�ff@���@�hs@�`B@�?}@��`@�j@���@�C�@��@�-@��@�$�@��^@�G�@��j@�1'@��F@���@�t�@�S�@��y@�5?@�-@�{@�@���@��#@�7L@���@��j@���@�Q�@� �@���@��@�t�@���@��!@���@�&�@�%@�%@���@��`@���@��/@���@�z�@�1'@���@���@�o@��\@�M�@�$�@��@��#@�@�@��^@��h@�`B@�O�@��@�V@�%@��@���@�Ĝ@�Ĝ@��j@��@���@���@�1@|�@|�@|�@l�@~��@}�@}��@|��@|z�@{�F@{S�@z��@z~�@y��@yhs@yhs@yG�@y%@xr�@w��@w|�@w\)@w\)@w\)@w\)@w;d@w+@w+@w�@w�@v��@vȴ@v5?@t��@tZ@t�D@t�j@t�j@t9X@r�\@r~�@sS�@t1@t(�@t1@s��@s"�@s"�@r�!@r=q@q�@qX@q7L@q7L@p��@p�9@pr�@pA�@o\)@o
=@n5?@n{@n@m�@m��@mO�@l��@l�@l��@lZ@l(�@k�F@k"�@j�H@j~�@j�@i�^@i��@iX@i�@hr�@g�@g��@g�P@gK�@f�@fE�@f@e��@e�-@e��@ep�@d��@dz�@d9X@c�
@c��@ct�@cS�@c33@co@b�@b��@b=q@a��@aG�@`��@`b@_�w@_�P@_�P@_�P@_;d@^�@^�R@^��@]�T@]?}@\��@\z�@\(�@[t�@["�@Z��@Z��@Zn�@ZM�@Z�@Y��@Y�7@Y7L@XĜ@XbN@W�w@W;d@V��@W�@W;d@W;d@W+@V��@V$�@U�T@U�@U?}@UV@T�@T�@S��@S�@St�@SdZ@SC�@So@S@R�H@R�!@Rn�@RM�@Q�@Q��@Q7L@P�@O�;@O�P@OK�@N��@N$�@M�-@M�@M?}@MV@L��@K�m@Kt�@KS�@KC�@K33@K"�@J�@J��@J^5@JJ@Ihs@IG�@IG�@IG�@IG�@I7L@I�@H��@H��@H�u@HbN@H1'@Hb@G�;@G�@Gl�@Fȴ@F{@E�@D(�@Cƨ@C��@Ct�@CC�@C"�@B�!@A�@Ahs@@Ĝ@@bN@@ �@?�@?��@?|�@?l�@?l�@?K�@>��@>ff@>E�@>5?@>{@>@=��@=��@=p�@=O�@=?}@=�@=V@<��@<�/@<�D@<9X@;�@;dZ@;C�@:�\@:-@9��@9�@9��@9�^@9hs@9�@9%@9%@9%@8��@8��@8�`@8Ĝ@8�9@8r�@8  @7�P@7+@6�R@6V@5��@5�@5V@4�j@4I�@49X@4(�@4(�@4(�@4(�@4�@4�@41@3��@3ƨ@3C�@3@2��@2n�@2^5@2n�@2n�@2^5@2^5@2-@2�@2J@2J@1��@1x�@1&�@0�`@0��@0��@/�;@/;d@.��@-�@,�@,�@,Z@+t�@+S�@+33@*�H@*��@*��@*J@)�#@)�7@(��@(�9@(��@(�@(bN@(Q�@(b@(  @'�@'�;@'�w@'��@'\)@'+@'+@'+@'�@'
=@&��@&��@&��@&�y@&ȴ@&�+@&V@&V@&5?@&5?@&$�@%�@%��@%`B@$�@$�j@$j@$�@#ƨ@#��@#�@#t�@#t�@#dZ@#@"��@"��@"�\@"n�@"^5@"=q@"�@!��@!��@!�7@!7L@ ��@ �@ r�@ bN@ A�@   @�w@��@�P@�P@|�@;d@��@ȴ@ȴ@�R@��@��@��@�+@5?@�@?}@��@�j@�D@9X@(�@9X@(�@�@�@1@�
@�
@�
@�
@�
@�@S�@@��@n�@^5@M�@=q@=q@��@�7@x�@hs@7L@�9@�@r�@bN@bN@bN@Q�@A�@b@  @  @  @�@�@\)@��@�+@�T@��@`B@/@�@V@��@�@�@��@Z@1@ƨ@��@��@dZ@"�@~�@J@��@X@�`@�@ �@b@  @�@�@�;@��@�w@�P@|�@l�@\)@K�@
=@��@5?@$�@@�T@@�-@��@�h@�h@`B@O�@?}@/@�@��@�D@z�@I�@1@��@�m@ƨ@�@"�@
�@
��@
^5@
�@	��@	�^@	��@	�7@	x�@	x�@	hs@	hs@	X@	%@Ĝ@�@�@�@�@�@r�@bN@Q�@A�@�@�@�@��@�@��@|�@K�@�@�@
=@��@�y@�R@�R@��@�+@v�@ff@E�@E�@E�@E�@5?@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�jA�|�A�t�A�^5A�?}AҶFA��HAΣ�A·+A�l�A�XA�A�oA�z�A�{A���A�VA�Q�A��A��FA�K�A�A�A�bNA��jA�+A�M�A���A��uA��!A�O�A�$�A��A�Q�A��A��jA�`BA��hA�A���A�XA���A�-A���A�1'A��^A�+A��!A�hsA�{A�ƨA�?}A��TA�5?A��#A�ĜA��TA���A�^5A���A���A��wA��7A���A��FA��hA��A�E�A�VA��wA�
=A��9A���A��/A�7LA�E�A�1'A��;A��uA���A��
A���A�`BA��FA���A�
=A��^A�l�A��uA��A�hsA�1'A�&�A��TA��/A�-A�(�A~��A~A�A}��A|ȴA|1'A{��Azr�Au�Ar�Ar1'Aq�Ao�wAoVAn��Ak�Ai%Ag�7Ae�FAe|�Ad��Ac��Ab�A`��A`1'A[��AZVAX�AWt�AV1'AUC�AT��AS�FARQ�AQ
=AP�yAP�`AP�AP�AO�AO��AOt�AOO�AO�AN�uAM��ALffAKt�AI�AIK�AH  AFn�AE�AES�AD��AC�-AB��ABQ�AB�AAl�A@�RA?ƨA>r�A>(�A=�;A<�jA:�yA9��A9;dA8ĜA7��A7?}A6�A6�jA6=qA5/A4��A2�!A0��A05?A.��A.-A-��A,�\A+"�A*ffA*A)�hA(ĜA'K�A&�RA&ZA%�
A%�hA$M�A"��A"v�A"I�A"5?A!��A ��AAjAƨA��A�PAdZAG�A�+A�mA�-At�A�/A~�A�^A`BA�jAXA��A�DAr�A�A��A�mAVA��A�^A\)A�HAffA��AK�A
ȴA	At�A\)AO�AK�A?}AoA��A�A��A��A|�AhsA%A��AXA �u@�\)@��@�Z@��@�(�@�;d@��-@�Z@��H@��;@�C�@ꗍ@�G�@�u@�(�@��m@畁@�+@�!@���@�?}@�&�@�?}@��@�@�@�5?@�9X@�C�@�v�@��@ۅ@�X@�\)@֟�@�p�@��@�bN@��m@��H@щ7@��/@Ͼw@͙�@�;d@�$�@ȋD@��H@�@��#@�X@���@�dZ@���@���@�9X@��@��@�(�@��H@��@�ƨ@�V@��^@��y@�5?@��@�p�@�1@�C�@�$�@��@��7@��@��@�I�@� �@��m@���@���@��@���@�9X@�
=@�V@���@�bN@�A�@�1@�1@��m@�l�@��H@�M�@�hs@�7L@��9@�j@���@� �@�b@��;@��@��@�-@��@��-@�%@��/@��@��m@���@�\)@��@��@���@�ff@��@��@��@���@���@���@�r�@�bN@�9X@��;@���@�l�@�33@���@��!@�ff@���@�hs@�`B@�?}@��`@�j@���@�C�@��@�-@��@�$�@��^@�G�@��j@�1'@��F@���@�t�@�S�@��y@�5?@�-@�{@�@���@��#@�7L@���@��j@���@�Q�@� �@���@��@�t�@���@��!@���@�&�@�%@�%@���@��`@���@��/@���@�z�@�1'@���@���@�o@��\@�M�@�$�@��@��#@�@�@��^@��h@�`B@�O�@��@�V@�%@��@���@�Ĝ@�Ĝ@��j@��@���@���@�1@|�@|�@|�@l�@~��@}�@}��@|��@|z�@{�F@{S�@z��@z~�@y��@yhs@yhs@yG�@y%@xr�@w��@w|�@w\)@w\)@w\)@w\)@w;d@w+@w+@w�@w�@v��@vȴ@v5?@t��@tZ@t�D@t�j@t�j@t9X@r�\@r~�@sS�@t1@t(�@t1@s��@s"�@s"�@r�!@r=q@q�@qX@q7L@q7L@p��@p�9@pr�@pA�@o\)@o
=@n5?@n{@n@m�@m��@mO�@l��@l�@l��@lZ@l(�@k�F@k"�@j�H@j~�@j�@i�^@i��@iX@i�@hr�@g�@g��@g�P@gK�@f�@fE�@f@e��@e�-@e��@ep�@d��@dz�@d9X@c�
@c��@ct�@cS�@c33@co@b�@b��@b=q@a��@aG�@`��@`b@_�w@_�P@_�P@_�P@_;d@^�@^�R@^��@]�T@]?}@\��@\z�@\(�@[t�@["�@Z��@Z��@Zn�@ZM�@Z�@Y��@Y�7@Y7L@XĜ@XbN@W�w@W;d@V��@W�@W;d@W;d@W+@V��@V$�@U�T@U�@U?}@UV@T�@T�@S��@S�@St�@SdZ@SC�@So@S@R�H@R�!@Rn�@RM�@Q�@Q��@Q7L@P�@O�;@O�P@OK�@N��@N$�@M�-@M�@M?}@MV@L��@K�m@Kt�@KS�@KC�@K33@K"�@J�@J��@J^5@JJ@Ihs@IG�@IG�@IG�@IG�@I7L@I�@H��@H��@H�u@HbN@H1'@Hb@G�;@G�@Gl�@Fȴ@F{@E�@D(�@Cƨ@C��@Ct�@CC�@C"�@B�!@A�@Ahs@@Ĝ@@bN@@ �@?�@?��@?|�@?l�@?l�@?K�@>��@>ff@>E�@>5?@>{@>@=��@=��@=p�@=O�@=?}@=�@=V@<��@<�/@<�D@<9X@;�@;dZ@;C�@:�\@:-@9��@9�@9��@9�^@9hs@9�@9%@9%@9%@8��@8��@8�`@8Ĝ@8�9@8r�@8  @7�P@7+@6�R@6V@5��@5�@5V@4�j@4I�@49X@4(�@4(�@4(�@4(�@4�@4�@41@3��@3ƨ@3C�@3@2��@2n�@2^5@2n�@2n�@2^5@2^5@2-@2�@2J@2J@1��@1x�@1&�@0�`@0��@0��@/�;@/;d@.��@-�@,�@,�@,Z@+t�@+S�@+33@*�H@*��@*��@*J@)�#@)�7@(��@(�9@(��@(�@(bN@(Q�@(b@(  @'�@'�;@'�w@'��@'\)@'+@'+@'+@'�@'
=@&��@&��@&��@&�y@&ȴ@&�+@&V@&V@&5?@&5?@&$�@%�@%��@%`B@$�@$�j@$j@$�@#ƨ@#��@#�@#t�@#t�@#dZ@#@"��@"��@"�\@"n�@"^5@"=q@"�@!��@!��@!�7@!7L@ ��@ �@ r�@ bN@ A�@   @�w@��@�P@�P@|�@;d@��@ȴ@ȴ@�R@��@��@��@�+@5?@�@?}@��@�j@�D@9X@(�@9X@(�@�@�@1@�
@�
@�
@�
@�
@�@S�@@��@n�@^5@M�@=q@=q@��@�7@x�@hs@7L@�9@�@r�@bN@bN@bN@Q�@A�@b@  @  @  @�@�@\)@��@�+@�T@��@`B@/@�@V@��@�@�@��@Z@1@ƨ@��@��@dZ@"�@~�@J@��@X@�`@�@ �@b@  @�@�@�;@��@�w@�P@|�@l�@\)@K�@
=@��@5?@$�@@�T@@�-@��@�h@�h@`B@O�@?}@/@�@��@�D@z�@I�@1@��@�m@ƨ@�@"�@
�@
��@
^5@
�@	��@	�^@	��@	�7@	x�@	x�@	hs@	hs@	X@	%@Ĝ@�@�@�@�@�@r�@bN@Q�@A�@�@�@�@��@�@��@|�@K�@�@�@
=@��@�y@�R@�R@��@�+@v�@ff@E�@E�@E�@E�@5?@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B�B��B  B	7B�BD�Br�B�B�-B�FB�XB��B�LB�B�{B}�Be`BR�BQ�B\)BcTB`BB\)BYBR�BZBhsBn�Bp�Bk�BjBjBl�Bk�BjBk�Bk�Bk�BjBiyBgmBffBdZBbNB_;BYBT�BM�BH�B<jB)�B{B	7B	7B+B	7B	7BB  B��B��B�sB�;B�BĜB��B�BW
BJ�B<jB+B$�B�BuB��B�B�}B�?B��B�oB}�Bn�B_;BXBN�BJ�BI�BD�B7LB+B
=B��B��B�B�B�yB�TB�)B�}B�B��B��B��B�{B�VB�Bo�BgmB^5B\)BYBQ�BM�BB�B<jB.B!�B�B�B\B	7B%BB
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�yB
�`B
�HB
�)B
�B
��B
��B
ǮB
ĜB
��B
�}B
�jB
�LB
�FB
�9B
�-B
�B
�B
��B
��B
��B
��B
��B
��B
�uB
�oB
�hB
�PB
�=B
�1B
�+B
�B
~�B
z�B
q�B
p�B
jB
hsB
ffB
dZB
_;B
[#B
YB
W
B
T�B
Q�B
M�B
K�B
I�B
G�B
F�B
A�B
@�B
>wB
=qB
<jB
9XB
7LB
49B
2-B
1'B
1'B
0!B
/B
0!B
.B
-B
+B
(�B
'�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
oB
\B

=B
+B
%B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�fB	�`B	�TB	�NB	�HB	�5B	�5B	�#B	�/B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�
B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�)B	�5B	�BB	�TB	�fB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
1B

=B

=B
DB
DB
PB
bB
oB
uB
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
$�B
'�B
)�B
.B
/B
0!B
0!B
?}B
E�B
E�B
E�B
G�B
J�B
P�B
Q�B
R�B
T�B
T�B
VB
XB
YB
[#B
^5B
_;B
^5B
aHB
dZB
ffB
gmB
gmB
gmB
hsB
iyB
jB
k�B
m�B
n�B
p�B
r�B
s�B
u�B
w�B
�B
�B
�B
�B
�=B
�hB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�9B
�RB
�XB
�jB
ÖB
ŢB
ǮB
ɺB
��B
��B
��B
��B
�B
�
B
�B
�/B
�HB
�TB
�TB
�NB
�NB
�NB
�sB
�sB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBBBBB%B%B%B1BDBDBVBVB\BbBoBoBuBuBuB{B{B�B�B�B�B!�B%�B+B/B49B5?B7LB8RB9XB9XB;dB=qB=qB>wB@�BD�BH�BI�BJ�BJ�BJ�BK�BL�BL�BM�BM�BM�BN�BN�BO�BR�BXB]/Be`BgmBk�Bm�Bp�Bs�Bv�Bv�Bv�Bx�B{�B}�B�B�B�B�%B�%B�+B�+B�1B�1B�7B�JB�PB�bB�bB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�!B�-B�9B�?B�FB�RB�XB�XB�XB�XB�^B�dB�dB�dB�qB�wB�}B��B��BBBÖBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBĜBĜBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�#B�)B�5B�;B�;B�;B�BB�BB�NB�NB�NB�NB�NB�NB�TB�TB�ZB�ZB�`B�fB�fB�fB�fB�fB�fB�mB�mB�mB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBBBBB%B+B1B1B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B
=B
=BDBDBDBJBDBDBJBJBJBJBJBPBPBPBVBVBVB\BbBhBoB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B&�B&�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B(�B)�B)�B)�B)�B)�B)�B)�B)�B,B,B,B-B-B.B.B-B.B.B.B.B.B.B.B.B.B/B/B/B0!B0!B0!B0!B0!B0!B1'B2-B2-B2-B2-B33B33B33B33B33B33B33B49B49B49B49B49B49B49B5?B5?B6FB7LB7LB7LB8RB8RB8RB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B;dB<jB<jB=qB=qB>wB?}B?}B?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BA�BB�BB�BB�BB�BB�BB�BB�BC�BB�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BM�BN�BM�BN�BN�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B��B��B��B��B��B�B��B  B	7B�BD�Br�B�B�-B�FB�XB��B�LB�B�{B}�Be`BR�BQ�B\)BcTB`BB\)BYBR�BZBhsBn�Bp�Bk�BjBjBl�Bk�BjBk�Bk�Bk�BjBiyBgmBffBdZBbNB_;BYBT�BM�BH�B<jB)�B{B	7B	7B+B	7B	7BB  B��B��B�sB�;B�BĜB��B�BW
BJ�B<jB+B$�B�BuB��B�B�}B�?B��B�oB}�Bn�B_;BXBN�BJ�BI�BD�B7LB+B
=B��B��B�B�B�yB�TB�)B�}B�B��B��B��B�{B�VB�Bo�BgmB^5B\)BYBQ�BM�BB�B<jB.B!�B�B�B\B	7B%BB
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�yB
�`B
�HB
�)B
�B
��B
��B
ǮB
ĜB
��B
�}B
�jB
�LB
�FB
�9B
�-B
�B
�B
��B
��B
��B
��B
��B
��B
�uB
�oB
�hB
�PB
�=B
�1B
�+B
�B
~�B
z�B
q�B
p�B
jB
hsB
ffB
dZB
_;B
[#B
YB
W
B
T�B
Q�B
M�B
K�B
I�B
G�B
F�B
A�B
@�B
>wB
=qB
<jB
9XB
7LB
49B
2-B
1'B
1'B
0!B
/B
0!B
.B
-B
+B
(�B
'�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
oB
\B

=B
+B
%B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�fB	�`B	�TB	�NB	�HB	�5B	�5B	�#B	�/B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�
B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�)B	�5B	�BB	�TB	�fB	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
1B

=B

=B
DB
DB
PB
bB
oB
uB
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
$�B
'�B
)�B
.B
/B
0!B
0!B
?}B
E�B
E�B
E�B
G�B
J�B
P�B
Q�B
R�B
T�B
T�B
VB
XB
YB
[#B
^5B
_;B
^5B
aHB
dZB
ffB
gmB
gmB
gmB
hsB
iyB
jB
k�B
m�B
n�B
p�B
r�B
s�B
u�B
w�B
�B
�B
�B
�B
�=B
�hB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�9B
�RB
�XB
�jB
ÖB
ŢB
ǮB
ɺB
��B
��B
��B
��B
�B
�
B
�B
�/B
�HB
�TB
�TB
�NB
�NB
�NB
�sB
�sB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBBBBB%B%B%B1BDBDBVBVB\BbBoBoBuBuBuB{B{B�B�B�B�B!�B%�B+B/B49B5?B7LB8RB9XB9XB;dB=qB=qB>wB@�BD�BH�BI�BJ�BJ�BJ�BK�BL�BL�BM�BM�BM�BN�BN�BO�BR�BXB]/Be`BgmBk�Bm�Bp�Bs�Bv�Bv�Bv�Bx�B{�B}�B�B�B�B�%B�%B�+B�+B�1B�1B�7B�JB�PB�bB�bB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�!B�-B�9B�?B�FB�RB�XB�XB�XB�XB�^B�dB�dB�dB�qB�wB�}B��B��BBBÖBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBĜBĜBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�#B�)B�5B�;B�;B�;B�BB�BB�NB�NB�NB�NB�NB�NB�TB�TB�ZB�ZB�`B�fB�fB�fB�fB�fB�fB�mB�mB�mB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBBBBB%B+B1B1B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B	7B
=B
=BDBDBDBJBDBDBJBJBJBJBJBPBPBPBVBVBVB\BbBhBoB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B&�B&�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B(�B)�B)�B)�B)�B)�B)�B)�B)�B,B,B,B-B-B.B.B-B.B.B.B.B.B.B.B.B.B/B/B/B0!B0!B0!B0!B0!B0!B1'B2-B2-B2-B2-B33B33B33B33B33B33B33B49B49B49B49B49B49B49B5?B5?B6FB7LB7LB7LB8RB8RB8RB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B;dB<jB<jB=qB=qB>wB?}B?}B?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BA�BB�BB�BB�BB�BB�BB�BB�BC�BB�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BM�BN�BM�BN�BN�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210628090046                              AO  ARCAADJP                                                                    20210628090046    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210628090046  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210628090046  QCF$                G�O�G�O�G�O�8000            