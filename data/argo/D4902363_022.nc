CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-01T00:35:15Z creation;2016-08-01T00:35:18Z conversion to V3.1;2019-12-19T08:34:11Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160801003515  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_022                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׿�i 1   @׿���� @<�4m���dm��,1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @w
=@��@��AA=A]A}A��HA��HA��HA��HA��HA��HA��HA��HB�
B
=Bp�Bp�B'p�B/p�B7p�B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��RC�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)CC!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
D �
Dw
D�
Dw
D�qDw
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
DL�qDMw
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
Dw�Dxw
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
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8RD�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȾ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�>�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AͲ-Aʹ9AͶFA͸RAͶFAͲ-Aͧ�Aͧ�Aͥ�Aͩ�AͬAͩ�Aͧ�Aͥ�A͟�A���A�jA�^5A¾wA���A��;A�ZA�A�33A���A�dZA�jA�;dA��A��A��A��9A�v�A�C�A��A��DA��hA���A�I�A�v�A��^A�|�A��A�/A�v�A���A��A�{A��A�ȴA��A�(�A��\A�+A���A���A��A��9A��PA�M�A��A���A�ȴA�l�A��#A��+A��hA��jA�-A�v�A�`BA�E�A�E�A��A�|�A�-A�A��^A�G�A\)A/A~�/A~bA}�A}G�A|z�A{%Ay��Aw�Av��Av  At��Ar�/AqK�Ao��An��Am�7Am+AmoAl�Ak�FAj$�Ai��Agt�AfĜAe\)Adv�Ac��AcG�Ab�Ab�/AbĜAb-A`�yA_G�A^�HA^A�A]K�AZ��AY&�AX�AW�AV�`AV  AU7LAT�9AS�AS33AR��AQl�AOdZAM�7AMx�AMdZAL�/ALffALQ�ALbAKAK��AK|�AJ�+AH�DAGl�AFn�AE`BAE
=AD��AD^5AC�;ACK�AB�uAA�A@1A>��A>�DA>^5A>9XA>bA=ƨA=�A<��A;\)A8�A6�`A6��A5�7A4�uA3�A2Q�A1�A1�A/��A-��A-G�A,�yA,n�A+K�A*1'A)�#A)�7A)%A'�A%�PA$VA#��A#;dA#VA"��A!�7A   A&�AVA��A�A\)A�`A-A��AS�A�\At�A;dA�/A��A1A�AXA�A�uA��AVA�/A�9A=qA��AVA��A�#AC�A��Ap�A�AQ�A
��A
  A	|�A	G�A�A=qAp�A�AVA��A"�A�HAG�A �HA �D@���@��-@���@�$�@�x�@�G�@��@���@���@�-@�%@�A�@�\)@�5?@�Q�@�F@��@��T@���@�@陚@�G�@�b@��@�  @��@�-@��@�b@��y@�(�@�\)@�o@�ff@ٲ-@�7L@ؼj@�I�@ץ�@��@��@�bN@��m@Ӯ@Ұ!@ЋD@�C�@��@�x�@�&�@�Ĝ@�b@��H@�V@Ɂ@ț�@�l�@Ł@Ý�@��@�r�@��w@�
=@��H@�^5@���@�@��j@���@�p�@�z�@��w@�/@�\)@���@�-@�O�@��@��@� �@�  @��
@��F@�C�@���@��+@���@���@�Ĝ@��D@��F@��@�ff@�@�7L@��`@� �@�+@�v�@�@���@�O�@��/@��;@�t�@�l�@��H@���@��/@��u@��9@��9@�9X@�;d@�;d@��H@�G�@��@�(�@��m@�ƨ@��F@���@�+@���@��R@�v�@�-@���@��7@�Z@�  @���@�n�@�v�@��\@��+@�=q@���@�@��h@��@��u@�bN@���@�  @��
@��P@�K�@�@�V@�@�x�@�G�@�?}@��@���@�r�@�~�@��#@��7@�p�@�X@���@�Ĝ@��@���@��@�Q�@���@�t�@��y@��@���@�^5@��T@�X@�&�@��j@� �@���@��@���@�
=@��!@���@���@��y@�@��/@���@�j@�b@�t�@�S�@�+@�C�@�@��y@��R@���@���@�n�@��@���@�x�@�X@�/@���@��@��D@�r�@�Z@�I�@�A�@�9X@�  @�;@��@�P@l�@
=@~��@;d@~�R@~��@}��@|�D@|�@|(�@}�h@~V@~�R@~��@|�j@z��@z^5@z��@}V@}��@}��@|�/@|�@{ƨ@{��@{dZ@{"�@z��@z��@z��@z~�@zn�@zJ@y��@y&�@xr�@x1'@w�@u��@t�D@tI�@s�F@r��@q��@q�@p�u@pA�@p1'@pb@o�@o��@n�y@m��@l��@l9X@l1@l1@l1@k��@k�@k"�@ko@j��@j��@k@k33@kS�@kS�@j��@i�#@iG�@h�`@h��@hr�@hQ�@hA�@hA�@h1'@g�@gK�@f�+@f$�@e�T@e�h@ep�@e/@d�/@d�/@d�j@dj@d1@c�F@cS�@c33@cS�@cdZ@ct�@cS�@b��@b��@b�H@b�H@c@b�H@b��@b�!@b�!@b=q@a��@aG�@aG�@ax�@ahs@a7L@`�`@`Ĝ@`�9@`��@ahs@`��@`�@`Q�@`�9@`A�@_��@_��@_l�@_+@^ȴ@^5?@^$�@^��@^v�@^5?@]��@]��@\�/@\j@\1@[dZ@[33@[o@Z�H@Z��@ZM�@Y�^@ZJ@Z�@Z=q@Y��@X��@X��@Xb@W�@W�w@W+@V��@V�+@V�+@VE�@U�T@U��@U�h@UO�@UV@TI�@Sƨ@SdZ@So@S@S@Q��@Qhs@Q&�@P��@PĜ@P�u@PbN@PA�@P  @O�@O�;@O�w@O��@O�P@Ol�@O\)@O;d@O+@O�@O
=@N�@N�R@N��@Nv�@NE�@N5?@M�-@L�@L�@K�F@K��@KdZ@K"�@Jn�@J-@I��@IG�@I7L@I&�@H�`@H�9@HA�@H1'@H1'@H1'@H1'@H �@H �@H  @G�@GK�@G�@G�@F�y@F�@F�@Fȴ@F�+@F5?@E�@E��@E`B@D��@D�D@Dz�@D(�@C�
@C�@CS�@C33@Co@C@C@C@B�H@B��@B~�@Bn�@B^5@B^5@A��@A�#@A��@AX@A�@@�`@@��@@�9@@�@@Q�@?�w@?��@?\)@?+@?
=@>��@>{@=��@=?}@<��@<��@<��@<I�@;�m@;�F@;�@;C�@;C�@;C�@;C�@;33@;33@:��@:^5@:=q@9��@9�#@9x�@9&�@9�@8��@8��@8A�@7�P@6��@6ff@6V@6V@6{@5��@5�h@5?}@4�/@4�@4�D@4Z@3��@3C�@2�@2�\@2M�@2�@1�#@1�#@1��@1��@1G�@0Ĝ@0�u@0r�@/��@.V@-@-?}@,�@,��@,�D@,Z@,9X@,(�@+��@+dZ@+��@+��@+��@+�@+S�@*��@*~�@*=q@)�@)�^@)�7@)hs@)G�@(��@(��@(A�@(b@'�@'�;@'�@'�@'
=@&��@&v�@&V@&E�@&@&@%�@%�-@%�@%p�@%O�@$�@$��@$�j@$��@$j@$Z@$(�@$�@#��@#ƨ@#C�@"��@"��@"=q@!x�@!&�@ ��@ ��@ Ĝ@ �@ r�@ bN@ r�@ bN@ bN@ bN@�;@;d@+@
=@��@V@�T@�@?}@�@��@�D@�D@9X@�m@�F@�F@�F@�F@�F@t�@S�@S�@S�@S�@C�@o@��@�!@�\@^5@M�@-@��@�@Ĝ@Q�@ �@ �@�w@�P@\)@�@��@v�@5?@{@@p�@`B@�@(�@1@1@�m@�
@�
@ƨ@�F@��@��@t�@S�@C�@�H@n�@=q@J@�@�#@��@hs@X@7L@�@��@�9@�@bN@bN@A�@ �@�@��@��@l�@;d@�@5?@��@��@��@�h@`B@/@��@�@�@�@z�@I�@9X@(�@1@�F@�F@��@�@C�@33@33@o@
��@
~�@
n�@
n�@
n�@
M�@	�@	��@	x�@	hs@	X@	7L@	&�@��@�9@�@r�@bN@  @�w@�w@��@|�@l�@K�@+@�@
=@
=@��@�@ȴ@ȴ@�R@�R@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AͲ-Aʹ9AͶFA͸RAͶFAͲ-Aͧ�Aͧ�Aͥ�Aͩ�AͬAͩ�Aͧ�Aͥ�A͟�A���A�jA�^5A¾wA���A��;A�ZA�A�33A���A�dZA�jA�;dA��A��A��A��9A�v�A�C�A��A��DA��hA���A�I�A�v�A��^A�|�A��A�/A�v�A���A��A�{A��A�ȴA��A�(�A��\A�+A���A���A��A��9A��PA�M�A��A���A�ȴA�l�A��#A��+A��hA��jA�-A�v�A�`BA�E�A�E�A��A�|�A�-A�A��^A�G�A\)A/A~�/A~bA}�A}G�A|z�A{%Ay��Aw�Av��Av  At��Ar�/AqK�Ao��An��Am�7Am+AmoAl�Ak�FAj$�Ai��Agt�AfĜAe\)Adv�Ac��AcG�Ab�Ab�/AbĜAb-A`�yA_G�A^�HA^A�A]K�AZ��AY&�AX�AW�AV�`AV  AU7LAT�9AS�AS33AR��AQl�AOdZAM�7AMx�AMdZAL�/ALffALQ�ALbAKAK��AK|�AJ�+AH�DAGl�AFn�AE`BAE
=AD��AD^5AC�;ACK�AB�uAA�A@1A>��A>�DA>^5A>9XA>bA=ƨA=�A<��A;\)A8�A6�`A6��A5�7A4�uA3�A2Q�A1�A1�A/��A-��A-G�A,�yA,n�A+K�A*1'A)�#A)�7A)%A'�A%�PA$VA#��A#;dA#VA"��A!�7A   A&�AVA��A�A\)A�`A-A��AS�A�\At�A;dA�/A��A1A�AXA�A�uA��AVA�/A�9A=qA��AVA��A�#AC�A��Ap�A�AQ�A
��A
  A	|�A	G�A�A=qAp�A�AVA��A"�A�HAG�A �HA �D@���@��-@���@�$�@�x�@�G�@��@���@���@�-@�%@�A�@�\)@�5?@�Q�@�F@��@��T@���@�@陚@�G�@�b@��@�  @��@�-@��@�b@��y@�(�@�\)@�o@�ff@ٲ-@�7L@ؼj@�I�@ץ�@��@��@�bN@��m@Ӯ@Ұ!@ЋD@�C�@��@�x�@�&�@�Ĝ@�b@��H@�V@Ɂ@ț�@�l�@Ł@Ý�@��@�r�@��w@�
=@��H@�^5@���@�@��j@���@�p�@�z�@��w@�/@�\)@���@�-@�O�@��@��@� �@�  @��
@��F@�C�@���@��+@���@���@�Ĝ@��D@��F@��@�ff@�@�7L@��`@� �@�+@�v�@�@���@�O�@��/@��;@�t�@�l�@��H@���@��/@��u@��9@��9@�9X@�;d@�;d@��H@�G�@��@�(�@��m@�ƨ@��F@���@�+@���@��R@�v�@�-@���@��7@�Z@�  @���@�n�@�v�@��\@��+@�=q@���@�@��h@��@��u@�bN@���@�  @��
@��P@�K�@�@�V@�@�x�@�G�@�?}@��@���@�r�@�~�@��#@��7@�p�@�X@���@�Ĝ@��@���@��@�Q�@���@�t�@��y@��@���@�^5@��T@�X@�&�@��j@� �@���@��@���@�
=@��!@���@���@��y@�@��/@���@�j@�b@�t�@�S�@�+@�C�@�@��y@��R@���@���@�n�@��@���@�x�@�X@�/@���@��@��D@�r�@�Z@�I�@�A�@�9X@�  @�;@��@�P@l�@
=@~��@;d@~�R@~��@}��@|�D@|�@|(�@}�h@~V@~�R@~��@|�j@z��@z^5@z��@}V@}��@}��@|�/@|�@{ƨ@{��@{dZ@{"�@z��@z��@z��@z~�@zn�@zJ@y��@y&�@xr�@x1'@w�@u��@t�D@tI�@s�F@r��@q��@q�@p�u@pA�@p1'@pb@o�@o��@n�y@m��@l��@l9X@l1@l1@l1@k��@k�@k"�@ko@j��@j��@k@k33@kS�@kS�@j��@i�#@iG�@h�`@h��@hr�@hQ�@hA�@hA�@h1'@g�@gK�@f�+@f$�@e�T@e�h@ep�@e/@d�/@d�/@d�j@dj@d1@c�F@cS�@c33@cS�@cdZ@ct�@cS�@b��@b��@b�H@b�H@c@b�H@b��@b�!@b�!@b=q@a��@aG�@aG�@ax�@ahs@a7L@`�`@`Ĝ@`�9@`��@ahs@`��@`�@`Q�@`�9@`A�@_��@_��@_l�@_+@^ȴ@^5?@^$�@^��@^v�@^5?@]��@]��@\�/@\j@\1@[dZ@[33@[o@Z�H@Z��@ZM�@Y�^@ZJ@Z�@Z=q@Y��@X��@X��@Xb@W�@W�w@W+@V��@V�+@V�+@VE�@U�T@U��@U�h@UO�@UV@TI�@Sƨ@SdZ@So@S@S@Q��@Qhs@Q&�@P��@PĜ@P�u@PbN@PA�@P  @O�@O�;@O�w@O��@O�P@Ol�@O\)@O;d@O+@O�@O
=@N�@N�R@N��@Nv�@NE�@N5?@M�-@L�@L�@K�F@K��@KdZ@K"�@Jn�@J-@I��@IG�@I7L@I&�@H�`@H�9@HA�@H1'@H1'@H1'@H1'@H �@H �@H  @G�@GK�@G�@G�@F�y@F�@F�@Fȴ@F�+@F5?@E�@E��@E`B@D��@D�D@Dz�@D(�@C�
@C�@CS�@C33@Co@C@C@C@B�H@B��@B~�@Bn�@B^5@B^5@A��@A�#@A��@AX@A�@@�`@@��@@�9@@�@@Q�@?�w@?��@?\)@?+@?
=@>��@>{@=��@=?}@<��@<��@<��@<I�@;�m@;�F@;�@;C�@;C�@;C�@;C�@;33@;33@:��@:^5@:=q@9��@9�#@9x�@9&�@9�@8��@8��@8A�@7�P@6��@6ff@6V@6V@6{@5��@5�h@5?}@4�/@4�@4�D@4Z@3��@3C�@2�@2�\@2M�@2�@1�#@1�#@1��@1��@1G�@0Ĝ@0�u@0r�@/��@.V@-@-?}@,�@,��@,�D@,Z@,9X@,(�@+��@+dZ@+��@+��@+��@+�@+S�@*��@*~�@*=q@)�@)�^@)�7@)hs@)G�@(��@(��@(A�@(b@'�@'�;@'�@'�@'
=@&��@&v�@&V@&E�@&@&@%�@%�-@%�@%p�@%O�@$�@$��@$�j@$��@$j@$Z@$(�@$�@#��@#ƨ@#C�@"��@"��@"=q@!x�@!&�@ ��@ ��@ Ĝ@ �@ r�@ bN@ r�@ bN@ bN@ bN@�;@;d@+@
=@��@V@�T@�@?}@�@��@�D@�D@9X@�m@�F@�F@�F@�F@�F@t�@S�@S�@S�@S�@C�@o@��@�!@�\@^5@M�@-@��@�@Ĝ@Q�@ �@ �@�w@�P@\)@�@��@v�@5?@{@@p�@`B@�@(�@1@1@�m@�
@�
@ƨ@�F@��@��@t�@S�@C�@�H@n�@=q@J@�@�#@��@hs@X@7L@�@��@�9@�@bN@bN@A�@ �@�@��@��@l�@;d@�@5?@��@��@��@�h@`B@/@��@�@�@�@z�@I�@9X@(�@1@�F@�F@��@�@C�@33@33@o@
��@
~�@
n�@
n�@
n�@
M�@	�@	��@	x�@	hs@	X@	7L@	&�@��@�9@�@r�@bN@  @�w@�w@��@|�@l�@K�@+@�@
=@
=@��@�@ȴ@ȴ@�R@�R@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B  B  B  BB\B(�B_;BiyB]/B=qB �B+B��B�B�}B�3B�B��B�1Bz�Bv�Bs�Bq�BiyBT�BVBN�BG�B2-B"�B �BuB+B��B��B�B�B�TB�B��BŢB�jB�?B�B��B�oB�VB�DB�+B� Bs�BhsBcTB\)BK�B>wB2-B'�B�B\B
��B
�B
�B
��B
ǮB
ĜB
�}B
�XB
�B
�B
��B
��B
��B
��B
��B
�=B
� B
s�B
e`B
bNB
YB
F�B
<jB
/B
'�B
!�B
�B
�B
�B
uB
+B
B	�B	�B	�NB	�B	��B	��B	��B	��B	��B	��B	ÖB	�XB	�?B	�B	��B	��B	�bB	�7B	�B	�B	{�B	v�B	r�B	n�B	iyB	gmB	cTB	`BB	T�B	S�B	R�B	Q�B	M�B	M�B	L�B	K�B	I�B	H�B	D�B	<jB	5?B	0!B	,B	'�B	&�B	$�B	!�B	�B	�B	�B	oB		7B	1B	%B	%B	B	B	B	  B��B�B�ZB�NB�BB�#B�
B��B��B��BȴB��B�qB�jB�^B�XB�?B�3B�-B�B�B��B��B��B��B��B��B��B�VB�DB�=B�+B�%B�B�B�B~�B}�By�Bs�Br�Bp�Bq�Bo�Bn�Bm�Bl�Bk�BjBhsBffBffBe`BdZBbNB`BB_;B^5B]/BZBXBXBXBS�BS�BR�BP�BN�BP�BM�BK�BF�BC�BD�BB�B?}B=qB:^B5?B1'B)�B'�B&�B&�B%�B%�B%�B$�B$�B#�B#�B#�B#�B$�B'�B'�B'�B'�B'�B'�B&�B%�B#�B"�B"�B!�B!�B!�B!�B �B!�B!�B!�B!�B!�B!�B#�B#�B$�B$�B#�B$�B%�B&�B'�B'�B'�B'�B(�B)�B)�B)�B+B,B.B0!B2-B49B5?B6FB6FB7LB7LB7LB9XB:^B?}B?}B@�BC�BB�BB�BB�BC�BD�BF�BH�BH�BI�BI�BK�BJ�BJ�BL�BM�BM�BN�BQ�BS�BT�BVBW
BW
BZB]/B^5B`BBaHBbNBdZBjBl�Bm�Bo�Bt�Bx�Bz�B~�B�B�%B�B�B�B�%B�+B�1B�=B�DB�JB�JB�\B�\B�bB�hB�hB�oB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�-B�9B�?B�FB�LB�LB�dBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�5B�NB�ZB�sB�B�B��B��B��B��B��B	  B	B	B	B	B	B	1B	PB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	'�B	+B	.B	/B	2-B	33B	:^B	>wB	@�B	A�B	C�B	D�B	E�B	H�B	M�B	O�B	O�B	O�B	P�B	Q�B	S�B	XB	\)B	_;B	cTB	cTB	bNB	bNB	dZB	l�B	o�B	r�B	z�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�1B	�%B	�%B	�%B	�+B	�1B	�DB	�JB	�VB	�bB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�qB	�wB	��B	B	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�B	�#B	�B	�B	�#B	�)B	�/B	�;B	�BB	�BB	�BB	�HB	�TB	�`B	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
1B
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
	7B

=B

=B
DB
DB
DB
JB
PB
\B
\B
bB
bB
bB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
;dB
:^B
:^B
;dB
<jB
>wB
?}B
?}B
?}B
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
A�B
A�B
A�B
A�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
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
T�B
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
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
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
\)B
\)B
\)B
]/B
\)B
\)B
]/B
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
_;B
_;B
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
bNB
bNB
bNB
bNB
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
ffB
ffB
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
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�B�B�B��B��B�B�B 4B iB �B�BgB3MBg�Bs�BezBD�B&�BdB�VB��B�[B�`B� B��B�B|Bw�BuBt�Bk�BWYBW�BQ BK)B4B$@B#�B�B�B�.B�>B�3B�UB��B�BϫB�B��B��B�IB�'B�&B��B�JB��B�Bu�Bi�BeFB_!BNB@�B3�B)�B!|B B�B
�!B
یB
͹B
�fB
ňB
��B
��B
��B
��B
��B
��B
�OB
��B
�YB
�B
�'B
u%B
f�B
d&B
[=B
H�B
>]B
0UB
)DB
"4B
�B
WB
#B
2B
KB
�B	��B	�=B	�B	�	B	ՁB	�uB	�B	�HB	��B	̈́B	�SB	�DB	�`B	��B	��B	��B	��B	�=B	�B	�AB	|�B	w�B	s�B	o�B	j�B	iDB	e�B	a�B	UMB	TaB	S�B	R�B	N"B	NVB	MPB	LJB	JXB	JrB	F�B	=�B	6�B	1AB	,�B	(�B	'�B	%�B	"�B	�B	�B	�B	�B		�B	�B	�B	�B	�B	�B	{B	�B�PB�!B�,B��B�B��B�B��B�TB��B��B� B�BB�qB�B��B��B�B�MB�B��B�zB��B�CB�7B�B�EB�YB��B�dB��B��B��B�B�3B��B�iB�B{BtTBsMBqABr�BpoBoBnIBm]Bl�Bk�Bh�BgBgRBf�Be�Bb�B`�B`BB_pB^�B[	BYeBY�BYeBT�BT�BS�BQ�BPbBR�BO\BMjBG�BD�BFtBCGB@OB>�B<PB7�B2�B*�B(XB'�B'�B&�B&�B&�B%�B%�B$�B$�B$tB$�B%zB($B(>B(>B(�B)B)DB(�B&�B$�B#�B#�B"�B#nB"hB"4B!|B"hB"NB"NB"hB"�B#B$�B$tB%`B%zB$�B&fB&�B'�B(sB(sB(sB(�B)�B*�B*�B*�B,"B-wB/�B1AB33B4�B5�B6�B6�B7�B7�B8RB:^B;�B@OB@�BBABD�BC-BC-BCGBC�BD�BGBIBIBJ	BJ=BLJBKDBK�BMjBNBNVBO�BR�BTaBU�BV�BW�BW�BZ�B]�B^�B`�Ba�Bb�BeBj�Bl�Bn/Bp�ButBy$B{BHB��B��B�gB��B�?B��B��B�fB�rB�xB�~B��B��B��B��B��B��B��B�[B�B�eB�B��B��B��B�:B�2B�LB�XB�_B�qB�CB�wB�CB�]B�wB�cB��B��B��B��B�nB�tB��B��B�B��B�1B�B��B�B�0B�B�B�B�.B�HB�hB�[BևB�KB�KBچBܬB޸B�B��B��B��B��B�+B�+B�B��B��B	 OB	�B	�B	AB	{B	�B	�B	�B	vB	�B	�B	�B	�B	�B	�B	B	B	�B	!B	!�B	!�B	"B	$&B	(
B	+6B	.IB	/5B	2aB	3hB	:�B	>�B	@�B	A�B	C�B	D�B	E�B	H�B	N"B	P.B	PbB	P.B	QB	Q�B	S�B	W�B	\CB	_�B	dB	c�B	b�B	b4B	c�B	l�B	o�B	sB	{0B	B	�B	�B	�B	� B	�;B	� B	�'B	�AB	�aB	�mB	�_B	�zB	��B	��B	��B	��B	�tB	�tB	��B	�zB	��B	�xB	�~B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�,B	�8B	�*B	�B	�"B	�)B	�)B	�CB	�]B	�OB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�&B	�&B	��B	�B	�B	�9B	�?B	�9B	�B	�$B	�1B	�WB	�7B	�QB	�#B	�xB	�~B	�pB	�\B	�\B	��B	�B	�nB	�FB	�B	�B	��B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�	B	�B	�B	��B	�B	�B	�6B	�PB	�"B	�6B	�(B
 4B
 B
 �B
-B
3B
SB
SB
?B
YB
_B
_B
1B
fB
fB
fB
KB
	RB
	lB
	lB
	RB
	RB
	lB
	RB

rB

rB
xB
xB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#B
$B
$B
$B
$B
$�B
%B
&B
&B
'B
'B
'8B
(>B
)*B
)DB
*0B
*0B
+B
+B
,"B
,=B
,=B
-)B
-)B
-)B
-B
-CB
-CB
-CB
./B
./B
.IB
/OB
/OB
0;B
0UB
0;B
0UB
1vB
1�B
1�B
1AB
2GB
3MB
3hB
4nB
4nB
5ZB
5tB
6zB
6zB
6zB
6�B
7�B
7�B
8lB
8lB
9�B
9�B
9rB
9rB
9rB
9�B
:�B
:�B
:�B
:�B
:�B
9�B
:�B
:xB
:�B
:�B
:�B
;B
:�B
:�B
;B
<jB
>�B
?}B
?�B
?�B
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
A�B
A�B
A�B
A�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
H�B
I�B
I�B
J�B
K�B
MB
MB
MB
MB
M�B
M�B
M�B
M�B
M�B
NB
NB
N"B
N�B
OB
OB
OB
PB
PB
O�B
PB
QB
QB
Q B
Q B
RB
R B
SB
SB
R�B
R�B
S&B
SB
SB
SB
SB
S&B
S&B
S&B
S&B
T,B
TB
TB
T,B
TFB
UMB
V9B
V9B
W?B
W?B
W?B
W?B
W?B
W?B
X_B
Y1B
Y1B
YKB
Y1B
YKB
Y1B
YeB
YeB
YKB
YB
Y1B
Y1B
YB
Y1B
ZB
Z7B
Z7B
Z7B
ZQB
[WB
[WB
\CB
\CB
\CB
]dB
\]B
\CB
]dB
]IB
]IB
]dB
]dB
]dB
]dB
^OB
^5B
^jB
^OB
^jB
^jB
^jB
_pB
_pB
_�B
_pB
`vB
abB
abB
a|B
a|B
a|B
a|B
bhB
bNB
b�B
b�B
b�B
bhB
b�B
c�B
cnB
cnB
cnB
cnB
d�B
dZB
dtB
d�B
dtB
e�B
e`B
ezB
ezB
e�B
f�B
f�B
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
g�B
gmB
g�B
hsB
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
iyB
i�B
i�B
i�B
iyB
j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608050039252016080500392520160805003925201806221211502018062212115020180622121150201804050404162018040504041620180405040416  JA  ARFMdecpA19c                                                                20160801093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160801003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160801003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160801003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160801003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160801003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160801003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160801003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160801003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160801003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20160801012042                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160801153346  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160804153925  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160804153925  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190416  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031150  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                