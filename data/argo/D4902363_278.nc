CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-08T00:35:15Z creation;2018-09-08T00:35:20Z conversion to V3.1;2019-12-19T07:33:15Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180908003515  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_278                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��6`�1   @���΀@9��n.��dY�kP��1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @w
=@��@��RAA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B'p�B/�
B7p�B?p�BGp�BOp�BWp�B_�
Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˅BϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)CuCw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D�qDw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�>�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�8RD�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�8RD�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�r�A�l�A�ffA�jA�ffA�VA�I�A�G�A�E�A�G�A�G�A�C�A�C�A�A�A�A�A�?}A�9XA�33A�(�A��A�A�  A�XA�+A��A��;A��HAìA�VA���A��A�A�E�A�p�A��A��jA�{A���A��7A��#A�$�A�Q�A�{A���A�K�A�`BA� �A��uA�33A�
=A��;A���A��+A�E�A���A�bNA�A�x�A�bA�`BA�S�A�ffA��A�O�A�z�A�ȴA�C�A�ffA�oA�ȴA�=qA��\A��A��`A��hA�"�A���A��hA���A�n�A�dZA��RA���A��-A��;A��hA��A�ffA��;A��RA�t�A�t�A���A���A���A��#A��A��uA��A+A~1'A{S�Ax�/Aw��Avn�AtQ�Ar��Aqx�Ap�+Ao
=Am�Al�Akt�AjjAh�Ag�hAe��Ad�/AdZAd �Ac�wAb��Aa��AaO�A`jA_`BA^ �A\�A\�+A[�A[|�AZjAY��AY`BAX�RAX$�AWXAU�AS�#ARZAP^5AN�AMx�AL�!AL �AK�AJ��AI�PAH{AF�uAE+AC�mAC
=AA�AA%A@�9A@Q�A?hsA>Q�A<��A;�PA;%A:ffA:JA9��A9t�A8�/A8�uA8I�A8=qA8 �A7�-A6��A6n�A5��A5�wA5p�A5A4��A4n�A3��A2M�A1��A1K�A1&�A0�A0��A/�
A/��A/l�A.�!A,E�A*�+A'�A&n�A%�^A$��A$VA#�A!�A �!A ��A �A ffA $�A��A7LA��AVA�#A�A�HA^5A�AVA��A��A  A�FAC�Av�AC�A�A^5A?}A��A
��AVA��A�AJA%An�A��A ��@��@�z�@�$�@�&�@��9@�r�@�9X@�@�@��@���@�@��@�O�@��@���@���@���@�F@�V@�D@�|�@�\@�5?@��#@�`B@�z�@߶F@���@�@ܬ@��H@�p�@�Ĝ@��@�33@�@���@ԛ�@� �@��
@Ӯ@�;d@�M�@Ѻ^@�X@���@�Ĝ@�l�@�\)@ɲ-@Ȭ@ǍP@ź^@��@�A�@�33@���@�/@�Ĝ@�ƨ@���@�l�@�ff@��h@��`@��@�n�@��@��+@�x�@��`@��j@���@�I�@��!@��@�x�@���@�I�@�
=@�ȴ@��R@���@�-@���@�z�@��;@�@�&�@�1@��!@�@���@�Ĝ@��9@���@�z�@�9X@��;@��+@��#@�hs@�V@�Z@���@�o@��@���@��@�%@�Z@���@��y@��+@�5?@�`B@�?}@��@���@���@�j@�Q�@��@�K�@���@�7L@���@���@���@��9@���@��@�Q�@�9X@� �@��;@�S�@���@�E�@�G�@���@�A�@�(�@��@��w@�t�@�33@�$�@�@�x�@���@��@��@���@��@�l�@�K�@�"�@�
=@���@�ff@�-@�J@��#@���@��@� �@�  @���@��@�|�@�\)@�K�@�+@��@��\@�@���@�x�@�p�@�p�@�p�@�`B@�&�@�Z@��;@��P@�t�@�33@�
=@�ȴ@��\@�~�@�n�@�E�@��-@��h@�x�@�X@�/@�V@��9@�j@��@�1@�;@�P@~�y@~@}��@}?}@|�@|j@{��@{@z�!@z�\@z-@y�#@yhs@x��@x�@x1'@xb@w�P@w�@vV@u@uO�@t�j@t�D@tZ@t(�@sƨ@s@r�H@r��@r�!@rn�@r-@q�7@qhs@qhs@qX@q7L@q&�@p�`@pĜ@p�u@pr�@pr�@pbN@p  @o�;@o��@o�@o�@o��@o|�@oK�@o
=@n�y@n��@nE�@n$�@n{@m�-@m`B@mV@l��@l�D@lI�@k"�@j~�@jn�@j=q@j�@i�@i��@i�#@i�@i�7@i%@i�@h��@hr�@h1'@g�@g�w@g�P@gK�@f��@f�+@e��@e�h@e?}@e/@d��@d�D@d9X@c�m@cƨ@c�@cC�@co@b~�@a��@ahs@aG�@a7L@a7L@`��@`�9@`�u@`�@`A�@`b@_�@_�@_��@_l�@^��@^$�@]�h@]O�@]V@\�/@\��@\��@\�@\j@\9X@\(�@[�
@[��@[t�@["�@Z��@Z^5@Y��@YG�@Y�@X�9@X  @W|�@W;d@W�@V�@Vv�@VV@V5?@V{@U�T@U�@U/@T�j@T�@T�D@T9X@Sƨ@SC�@So@R�H@R��@R�\@R�@Q�^@Q�7@Qhs@QG�@PĜ@Pb@O�;@O��@O��@Ol�@Ol�@OK�@N��@N�@N�R@N�+@N5?@M��@M�@L�/@L�D@Lj@LZ@LI�@K�m@Kƨ@K��@K�@K33@J��@I�#@I��@Ix�@I�@HbN@H �@Hb@G��@G�P@G|�@G|�@G\)@G+@F��@Fv�@F5?@F$�@F{@F{@F{@F@E��@E�h@EO�@EV@D�/@D�j@D��@C��@C�@Ct�@CC�@B�!@B^5@B-@A�@A��@A��@A�7@AG�@@��@@�@@ �@?�w@?|�@?\)@?K�@>��@>�+@>V@=�@=@=`B@=�@<�/@<�D@<Z@<I�@<�@;��@;�m@;�F@;dZ@;o@;@:��@:n�@9��@9X@8Ĝ@8�9@8�9@8�u@8bN@8b@7�;@7�@7l�@7\)@7+@6ȴ@6$�@5�@5�-@5�h@4��@4j@4z�@4z�@4�D@4�D@4j@49X@3��@2��@2�@1��@1�^@1�^@1��@1��@1x�@1G�@1%@0�u@0b@/�w@/;d@.$�@-p�@-?}@-/@-/@-�@-V@,��@,��@,�D@,z�@+��@+��@+t�@+S�@+33@+o@+@*��@*��@*^5@*=q@*-@*J@)�^@)%@(Ĝ@(��@(�u@(�@(Q�@'�;@'|�@'\)@&��@&�+@&@%�T@%�@%�@%�@%V@$�D@#��@#�F@#�@#S�@#33@"�@"�\@"M�@"J@!��@!�7@!7L@!%@ Ĝ@ r�@ 1'@ b@��@�w@�w@�P@\)@\)@;d@�@��@�+@ff@5?@��@�h@?}@�/@�j@z�@�@�
@�
@�
@�@t�@dZ@S�@33@o@�@��@��@�\@M�@J@��@7L@%@��@�9@r�@1'@  @�w@�P@\)@�@�@��@E�@@��@p�@`B@O�@/@�@��@Z@9X@�m@�@dZ@C�@"�@��@��@�\@^5@-@��@��@��@X@%@��@��@�9@��@�@bN@A�@A�@ �@  @��@�P@l�@\)@;d@�@��@ȴ@��@��@ff@5?@�@@��@�h@�@p�@`B@O�@V@��@��@�D@j@9X@�m@�
@�F@S�@o@
��@
��@
~�@
M�@	�#@	��@	�7@	x�@	hs@	X@	G�@�`@��@�u@�@A�@��@�@\)@�@
=@ȴ@��@�R@��@v�@V@E�@{@@��@��@�h@`B@�@��@�@�@�@�@�@�D@j@Z@Z@Z@I�@9X@9X@9X@1@�m@�m@ƨ@�@t�@S�@C�@C�@33@"�@�@��@��@�\@~�@=q@-@�@J@J@��@�#@7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�|�A�r�A�l�A�ffA�jA�ffA�VA�I�A�G�A�E�A�G�A�G�A�C�A�C�A�A�A�A�A�?}A�9XA�33A�(�A��A�A�  A�XA�+A��A��;A��HAìA�VA���A��A�A�E�A�p�A��A��jA�{A���A��7A��#A�$�A�Q�A�{A���A�K�A�`BA� �A��uA�33A�
=A��;A���A��+A�E�A���A�bNA�A�x�A�bA�`BA�S�A�ffA��A�O�A�z�A�ȴA�C�A�ffA�oA�ȴA�=qA��\A��A��`A��hA�"�A���A��hA���A�n�A�dZA��RA���A��-A��;A��hA��A�ffA��;A��RA�t�A�t�A���A���G�O�G�O�A��A��uA��A+A~1'A{S�Ax�/Aw��Avn�AtQ�Ar��Aqx�Ap�+Ao
=Am�Al�Akt�AjjAh�Ag�hAe��Ad�/AdZAd �Ac�wAb��Aa��AaO�A`jA_`BA^ �A\�A\�+A[�A[|�AZjAY��AY`BAX�RAX$�AWXAU�AS�#ARZAP^5AN�AMx�AL�!AL �AK�AJ��AI�PAH{AF�uAE+AC�mAC
=AA�AA%A@�9A@Q�A?hsA>Q�A<��A;�PA;%A:ffA:JA9��A9t�A8�/A8�uA8I�A8=qA8 �A7�-A6��A6n�A5��A5�wA5p�A5A4��A4n�A3��A2M�A1��A1K�A1&�A0�A0��A/�
A/��A/l�A.�!A,E�A*�+A'�A&n�A%�^A$��A$VA#�A!�A �!A ��A �A ffA $�A��A7LA��AVA�#A�A�HA^5A�AVA��A��A  A�FAC�Av�AC�A�A^5A?}A��A
��AVA��A�AJA%An�A��A ��@��@�z�@�$�@�&�@��9@�r�@�9X@�@�@��@���@�@��@�O�@��@���@���@���@�F@�V@�D@�|�@�\@�5?@��#@�`B@�z�@߶F@���@�@ܬ@��H@�p�@�Ĝ@��@�33@�@���@ԛ�@� �@��
@Ӯ@�;d@�M�@Ѻ^@�X@���@�Ĝ@�l�@�\)@ɲ-@Ȭ@ǍP@ź^@��@�A�@�33@���@�/@�Ĝ@�ƨ@���@�l�@�ff@��h@��`@��@�n�@��@��+@�x�@��`@��j@���@�I�@��!@��@�x�@���@�I�@�
=@�ȴ@��R@���@�-@���@�z�@��;@�@�&�@�1@��!@�@���@�Ĝ@��9@���@�z�@�9X@��;@��+@��#@�hs@�V@�Z@���@�o@��@���@��@�%@�Z@���@��y@��+@�5?@�`B@�?}@��@���@���@�j@�Q�@��@�K�@���@�7L@���@���@���@��9@���@��@�Q�@�9X@� �@��;@�S�@���@�E�@�G�@���@�A�@�(�@��@��w@�t�@�33@�$�@�@�x�@���@��@��@���@��@�l�@�K�@�"�@�
=@���@�ff@�-@�J@��#@���@��@� �@�  @���@��@�|�@�\)@�K�@�+@��@��\@�@���@�x�@�p�@�p�@�p�@�`B@�&�@�Z@��;@��P@�t�@�33@�
=@�ȴ@��\@�~�@�n�@�E�@��-@��h@�x�@�X@�/@�V@��9@�j@��@�1@�;@�P@~�y@~@}��@}?}@|�@|j@{��@{@z�!@z�\@z-@y�#@yhs@x��@x�@x1'@xb@w�P@w�@vV@u@uO�@t�j@t�D@tZ@t(�@sƨ@s@r�H@r��@r�!@rn�@r-@q�7@qhs@qhs@qX@q7L@q&�@p�`@pĜ@p�u@pr�@pr�@pbN@p  @o�;@o��@o�@o�@o��@o|�@oK�@o
=@n�y@n��@nE�@n$�@n{@m�-@m`B@mV@l��@l�D@lI�@k"�@j~�@jn�@j=q@j�@i�@i��@i�#@i�@i�7@i%@i�@h��@hr�@h1'@g�@g�w@g�P@gK�@f��@f�+@e��@e�h@e?}@e/@d��@d�D@d9X@c�m@cƨ@c�@cC�@co@b~�@a��@ahs@aG�@a7L@a7L@`��@`�9@`�u@`�@`A�@`b@_�@_�@_��@_l�@^��@^$�@]�h@]O�@]V@\�/@\��@\��@\�@\j@\9X@\(�@[�
@[��@[t�@["�@Z��@Z^5@Y��@YG�@Y�@X�9@X  @W|�@W;d@W�@V�@Vv�@VV@V5?@V{@U�T@U�@U/@T�j@T�@T�D@T9X@Sƨ@SC�@So@R�H@R��@R�\@R�@Q�^@Q�7@Qhs@QG�@PĜ@Pb@O�;@O��@O��@Ol�@Ol�@OK�@N��@N�@N�R@N�+@N5?@M��@M�@L�/@L�D@Lj@LZ@LI�@K�m@Kƨ@K��@K�@K33@J��@I�#@I��@Ix�@I�@HbN@H �@Hb@G��@G�P@G|�@G|�@G\)@G+@F��@Fv�@F5?@F$�@F{@F{@F{@F@E��@E�h@EO�@EV@D�/@D�j@D��@C��@C�@Ct�@CC�@B�!@B^5@B-@A�@A��@A��@A�7@AG�@@��@@�@@ �@?�w@?|�@?\)@?K�@>��@>�+@>V@=�@=@=`B@=�@<�/@<�D@<Z@<I�@<�@;��@;�m@;�F@;dZ@;o@;@:��@:n�@9��@9X@8Ĝ@8�9@8�9@8�u@8bN@8b@7�;@7�@7l�@7\)@7+@6ȴ@6$�@5�@5�-@5�h@4��@4j@4z�@4z�@4�D@4�D@4j@49X@3��@2��@2�@1��@1�^@1�^@1��@1��@1x�@1G�@1%@0�u@0b@/�w@/;d@.$�@-p�@-?}@-/@-/@-�@-V@,��@,��@,�D@,z�@+��@+��@+t�@+S�@+33@+o@+@*��@*��@*^5@*=q@*-@*J@)�^@)%@(Ĝ@(��@(�u@(�@(Q�@'�;@'|�@'\)@&��@&�+@&@%�T@%�@%�@%�@%V@$�D@#��@#�F@#�@#S�@#33@"�@"�\@"M�@"J@!��@!�7@!7L@!%@ Ĝ@ r�@ 1'@ b@��@�w@�w@�P@\)@\)@;d@�@��@�+@ff@5?@��@�h@?}@�/@�j@z�@�@�
@�
@�
@�@t�@dZ@S�@33@o@�@��@��@�\@M�@J@��@7L@%@��@�9@r�@1'@  @�w@�P@\)@�@�@��@E�@@��@p�@`B@O�@/@�@��@Z@9X@�m@�@dZ@C�@"�@��@��@�\@^5@-@��@��@��@X@%@��@��@�9@��@�@bN@A�@A�@ �@  @��@�P@l�@\)@;d@�@��@ȴ@��@��@ff@5?@�@@��@�h@�@p�@`B@O�@V@��@��@�D@j@9X@�m@�
@�F@S�@o@
��@
��@
~�@
M�@	�#@	��@	�7@	x�@	hs@	X@	G�@�`@��@�u@�@A�@��@�@\)@�@
=@ȴ@��@�R@��@v�@V@E�@{@@��@��@�h@`B@�@��@�@�@�@�@�@�D@j@Z@Z@Z@I�@9X@9X@9X@1@�m@�m@ƨ@�@t�@S�@C�@C�@33@"�@�@��@��@�\@~�@=q@-@�@J@J@��@�#@7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}�BgmBR�Bv�B�dB�BG�B-BP�BG�B8RBl�B\)BC�BB�Bm�Bv�B��B��B�oBs�B�JB�VB�B�1By�BhsBdZBXBN�BhsBhsBbNB]/BT�BS�BP�BI�BC�B7LB&�B#�B�BPBPBDB	7B%BhBJBB��B�B�B�TB�LB�?B�^B��B|�Bu�B]/B+B2-B(�B%�B	7B
��B
��B
�B
��B
�qB
��B
�}B
�9B
��B
M�B
gmB
cTB
`BB
S�B
1'B
#�B
�B
�B
B	��B
1B
B	��B	�yB	�yB	�)B	�NB	��B	��B	ƨB	��B	��B	�B	��B	ǮB	�FB	��B	�?B	�B	��B	��B	��B	��B	��B	��B	�uB	��B	�VB	�1B	~�B	o�B	ZB	K�B	:^B	.B	1'B	,B	,B	�B	!�B	�B	1B	B��B��B�B�B�mB�B�sB�#B��B��B�B�HB�TB�fB�`B�fB�NB�ZB�fB�sB�mB�HB�)B�/B�)B�;B�/B�B�B�B��BɺB��B��B�B��B��BǮB��BĜB�?B��B��B�7B�DB��B�bB�=B�Bu�B�B�DB�DB�7B�B� B~�B{�B}�By�Bw�Bn�BdZBVBH�BYBS�B\)B_;BYBQ�BE�B>wB49B�B/B.B'�B"�BA�B9XB.B.B!�B�B$�B!�B#�B.B0!B/B(�B�BuB�B�B�B�B�B�B\B�BoB\BbBPB�B�B �B �B�B�B�B�B�B�BoB�B�B �B �B�B!�B(�B'�B(�B)�B&�B#�B$�B&�B$�B!�B�B1B�B"�B �B�B&�B)�B&�B"�B-B-B(�B.B.B(�B&�B+B$�B&�B'�B$�B6FB=qBD�BC�BA�B;dBE�BI�BH�BH�BE�BP�BQ�BO�BM�BJ�BH�BK�BK�BF�BP�BS�B[#B`BBiyBl�Bm�Bl�Bk�Bk�BhsBp�Bs�Bu�Bu�Bw�B{�B� B~�Bz�By�B~�B�B�B�7B�DB�DB�{B��B��B��B��B��B��B��B�{B��B��B��B�B�B�B�B�B�!B�!B�!B�'B�?B�dB�dBBȴB��B��B��B��B��BɺB��B�B��B�B�BB�BB�`B�fB�mB�fB�fB�mB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	  B��B	  B	B	1B	DB	VB	VB	VB	PB	JB	
=B	bB	�B	�B	�B	�B	 �B	#�B	'�B	)�B	)�B	(�B	/B	0!B	1'B	2-B	49B	49B	8RB	;dB	@�B	@�B	@�B	@�B	A�B	F�B	H�B	J�B	J�B	O�B	T�B	ZB	]/B	]/B	`BB	bNB	dZB	gmB	jB	k�B	k�B	m�B	o�B	t�B	x�B	z�B	}�B	� B	�B	�B	�B	�+B	�1B	�1B	�1B	�7B	�DB	�\B	�bB	�hB	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�-B	�-B	�?B	�FB	�RB	�XB	�XB	�^B	�dB	�jB	��B	B	ĜB	ĜB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�HB	�NB	�TB	�TB	�NB	�NB	�TB	�ZB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
B
B
%B
	7B

=B
	7B
	7B
JB
PB
JB
PB
VB
VB
VB
PB
PB
VB
\B
hB
hB
hB
hB
hB
bB
bB
hB
hB
oB
oB
oB
bB
{B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
!�B
!�B
 �B
 �B
"�B
"�B
%�B
&�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
&�B
'�B
)�B
)�B
)�B
(�B
+B
.B
.B
.B
.B
-B
,B
)�B
)�B
,B
/B
2-B
2-B
2-B
1'B
1'B
1'B
0!B
/B
0!B
0!B
/B
/B
1'B
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
7LB
6FB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
:^B
9XB
8RB
;dB
=qB
=qB
=qB
<jB
;dB
<jB
=qB
<jB
<jB
=qB
?}B
?}B
?}B
B�B
A�B
?}B
?}B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
I�B
I�B
J�B
J�B
I�B
K�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
N�B
P�B
O�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
P�B
R�B
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
VB
VB
VB
W
B
XB
YB
XB
XB
XB
XB
XB
ZB
YB
YB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
dZB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
ffB
ffB
e`B
e`B
gmB
ffB
gmB
hsB
hsB
hsB
jB
iyB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
jB
k�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
p�B
p�B
o�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�AB�-B�-B�3B�3B�GB�3B�9B�9B�9B�9B�B�9B�B�B�9B�MB�3B�MB�uB~�Bj�BYKB}qB�;B �BJ�B3�BT�BMB>BBn�B`BBHBG�Bo�Bx8B�hB��B��Bw�B��B� B��B��B|6BkBfB[	BQ4Bh�Bh�BcB]�BVBT�BQ�BKBD�B9>B)yB&B�BB\BjBJB�BTBBuB�XB��B�vB��B�6B�zB�B��B�Bx�Ba-B/�B49B*�B'B�B
�fB
�FB
�)B
̈́B
��B
��B
�B
�ZG�O�G�O�B
i_B
d�B
abB
U�B
4�B
&�B
!|B
qB
�B	�B
	�B
aB	��B	�6B	�B	��B	��B	��B	ңB	ȴB	��B	ԯB	�mB	ΥB	ȴB	��B	�B	��B	�}B	��B	�-B	��B	��B	��B	��B	�{B	�
B	�\B	�7B	�iB	q�B	\�B	M�B	<�B	0UB	2aB	-)B	,�B	!-B	"�B	+B	
=B	�B��B�fB��B��B�B�B�*BܬB֡B�B�YB��B�&B�B��B��B�B��B��B��B��B�B�/B�B��BߤB��B��BٴBںB�2B�xB��B�{B�mB�[B�}B��B�DB�mB��B�B�B�JB�B�mB��B�xB��Bw�B��B��B��B��B��B��B�B|�B~�Bz�BxlBo�Be�BXEBK)BZ�BU�B\�B_�BZBS[BG�B@�B6�B�B0�B0UB*�B%BA�B:^B/�B/iB$@B+B&�B#�B%`B.�B0�B/�B)�BqB�B�BqByB�BEBeB}B1B[BHBNB�BQB]B!B!HB;B]BqBqBB�B�B�B \B!�B!�B �B"�B)_B(sB)_B*eB'�B$�B%`B'mB%`B"hB
B
�B�B#�B!�B B'�B*�B'�B#�B-wB-�B)�B.cB.�B)�B'�B+�B%�B'�B)DB&�B6�B=�BD�BC�BBB<�BFYBJ#BIRBIlBF�BQ4BRBP.BNVBKxBI�BLdBL�BHBQ�BUB[�B`�Bi�Bl�Bm�Bl�Bk�Bl"Bi�Bq'Bt9BvFBv`Bx�B|jB�4BHB{Bz�B�B��B��B��B��B��B��B��B��B��B��B��B�B�?B��B�NB�*B�KB�"B�"B�CB�]B�cB�UB�oB��B��B��B�B�6B��B�B�B�"B�"B�"B�PBʌB�[B�SBԕB�B��B�B�B�B�B�B�B�B�B��B��B��B��B�6B�B��B�B�6B�<B�.B	 4B	 OB�HB	 �B	�B	�B	�B	pB	�B	�B	�B	�B	
�B	�B	�B	�B	�B	B	!B	$B	($B	*0B	*KB	)yB	/5B	0UB	1vB	2|B	4�B	4�B	8�B	;�B	@�B	@�B	@�B	@�B	BB	F�B	IB	KB	KB	PHB	UMB	ZQB	]dB	]~B	`�B	b�B	d�B	g�B	j�B	k�B	k�B	m�B	pB	uB	y	B	{0B	~(B	�4B	� B	�UB	�uB	�EB	�KB	�fB	�fB	��B	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�&B	�TB	�8B	�B	�6B	�=B	�CB	�5B	�OB	�5B	�cB	�iB	�MB	�aB	�aB	�tB	�zB	��B	�rB	�rB	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�<B	� B	�B	�2B	�B	�,B	�B	�9B	�$B	�9B	�$B	�EB	�KB	�EB	�_B	�yB	�qB	�]B	�pB	�|B	�B	�nB	�nB	�B	�B	�B	�B	�B	�tB	�tB	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�	B	��B	�$B	�	B	�B	�B	�B	�B	�0B	�B	�B	�.B	�.B
  B
 4B	�B
 B
 B
 4B
 OB
 OB
;B
-B
MB
SB
SB
9B
MB
YB
YB
YB
SB
mB
�B
	lB

XB
	�B
	�B
~B
jB
dB
�B
pB
VB
�B
jB
jB
�B
vB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
!�B
!�B
 �B
!B
#B
#B
%�B
'B
&B
&B
&B
'B
'B
'B
($B
($B
'8B
(>B
*0B
*0B
*B
)_B
+6B
.B
.B
./B
./B
-CB
,"B
*eB
*eB
,WB
/5B
2GB
2-B
2GB
1[B
1AB
1AB
0UB
/iB
0UB
0oB
/�B
/�B
1�B
6`B
7LB
7fB
7fB
7fB
7fB
6zB
6zB
7�B
6zB
7�B
9�B
9rB
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;B
:�B
9�B
8�B
;�B
=�B
=�B
=�B
<�B
;�B
<�B
=�B
<�B
<�B
=�B
?�B
?�B
?�B
B�B
A�B
?�B
?�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
I�B
I�B
J�B
J�B
I�B
K�B
L�B
K�B
K�B
K�B
K�B
K�B
MB
NB
NB
M�B
N�B
Q B
O�B
OB
Q B
Q B
Q B
QB
Q B
QB
QB
R B
R B
RB
Q B
R B
Q4B
S&B
TB
TB
TB
TB
T,B
U2B
UB
UB
UB
U2B
VB
VB
V9B
W?B
XEB
Y1B
X+B
XEB
X+B
XEB
XEB
ZQB
YKB
Y1B
[=B
[WB
[=B
[WB
\CB
\]B
\]B
[WB
\]B
\]B
\]B
\]B
\CB
^5B
^jB
^OB
^5B
^jB
^OB
_;B
_VB
_VB
^jB
^OB
^jB
_pB
`\B
`\B
_pB
`vB
`\B
`vB
`\B
`vB
`\B
`vB
a|B
bNB
bhB
bNB
bhB
bNB
b�B
bhB
bhB
c�B
cnB
cnB
b�B
b�B
dtB
c�B
c�B
c�B
d�B
cnB
cnB
c�B
b�B
dtB
e`B
e`B
ezB
ezB
ezB
d�B
e�B
f�B
f�B
e�B
e�B
g�B
f�B
g�B
hsB
h�B
h�B
j�B
i�B
h�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
i�B
j�B
k�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
p�B
p�B
o�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809120037122018091200371220180912003712201809120200152018091202001520180912020015201809130026512018091300265120180913002651  JA  ARFMdecpA19c                                                                20180908093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180908003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180908003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180908003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180908003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180908003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180908003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180908003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180908003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180908003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180908003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180908003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20180908005616                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180908153251  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180911153712  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180911153712  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180911170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180912152651  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                