CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-18T00:36:33Z creation;2019-01-18T00:36:38Z conversion to V3.1;2019-12-19T07:23:06Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190118003633  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              BA   JA  I2_0576_322                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؠ�ڤ� 1   @ؠ�З� @9�kP��|�dO�i�B�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@w
=@�Q�@��A(�A=A]A}A��HA��HA��HA��HA��HA߮A��HA��HBp�Bp�Bp�Bp�B'p�B/p�B7
=B?p�BGp�BOp�BWp�B_p�Bgp�Bop�Bwp�Bp�B��RB��RB��RB��B��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RB��RBøRBǸRB˸RBϸRBӸRB׸RB۸RB߸RB�RB�RB�RB�RB�RB��RB��RB��C�)C�)C�)C�)C	�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)CkCm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D w
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
D#}qD#�
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
DN}qDN�
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
DV}qDV�
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
Dep�De�
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
Do}qDo�
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
D�D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�~�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��RD���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D��RD�8RD�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D໅D���D�;�D�{�DễD���D�;�D�{�D⻅D���D�;�D�{�D㻅D���D�;�D�{�D仅D���D�;�D�{�D廅D���D�;�D�{�D滅D���D�;�D�{�D绅D���D�;�D�{�D軅D���D�;�D�{�D黅D���D�;�D�{�D껅D���D�;�D�{�D뻅D���D�;�D�{�D컅D���D�;�D�{�D���D���D�;�D�{�DD���D�;�D�{�DﻅD���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D�D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A�ȴA��jA��!A��!A��-A��9A���A��7A�K�A�33A�A���A���A���A���A���A���A��uA��PA�v�A�`BA�Q�A��A�A���A���A��A���A�v�A�=qA��RA�"�A�A��RA�;dA��yA�9XA��/A�I�A��wA��!A��A�%A��;A�bNA�%A���A���A��A���A�S�A�
=A�x�A��A�|�A�|�A�9XA��
A��hA�ZA��A��;A�dZA�5?A�r�A��7A���A� �A��A��9A���A���A��#A�9XA��RA���A�{A� �A��#A�I�A�r�A�C�A�VA�Q�A�dZA�x�A���A�ffA�A�5?A��A�^A~�A}+A|ĜA|  Azr�Ay��Ay
=Awx�Av1At��As�;Ar�+Aop�AlI�AjA�Ah��Ah9XAg�FAgK�Af�AfVAe�wAd{A`z�A]�A\�DAZ�9AYƨAW��AV�/AV$�ATȴASp�AR��AR�AR�AR$�AQ�FAQhsAP�AOdZAOAN��AN�ANI�AM�hAL��AKt�AJ��AJn�AI�AI"�AG��AF�`AF�+AE�hAD�uAD^5ADM�AD5?AC�AC7LAA%A@v�A?�FA>�A=ƨA<ȴA;��A:��A:  A8bA5S�A4��A4z�A3��A2�yA2M�A1��A17LA0I�A.��A-��A,M�A+C�A*v�A* �A*  A)dZA(�`A(�+A&bNA%A$r�A$$�A#x�A"jA!`BA jA A�-A�A~�AVA%A\)AI�AA��AG�A��A��AK�A-A��A��A��A1'A�-A�A�AE�A�A
��A
��A
=qA	&�A��A  A�9A�+A��A��A�7A;dA��A��A��A �A �@��+@��@���@� �@��P@��y@�J@���@�(�@�n�@�+@�-@��/@��@��@��H@�/@�@�ff@�@��`@�b@ߝ�@�S�@��@�
=@��H@ޟ�@�@�X@�/@���@�(�@�  @ۍP@�ȴ@�M�@�$�@���@�G�@�Q�@�n�@��@���@θR@�G�@˕�@ʟ�@�V@�@ɩ�@��/@�|�@��@�+@�V@��@�  @�\)@��\@��@�x�@�b@�33@�5?@�G�@��H@���@�^5@�hs@�I�@��@��@��@��@�r�@�1'@��m@��F@��F@��F@���@�l�@�K�@��@�
=@�M�@��/@�1'@��;@���@���@��@�+@�n�@��@��@�bN@� �@���@�t�@�V@�Ĝ@�1'@�S�@��@�v�@��T@��u@���@��h@�%@�r�@�I�@���@���@��@���@�\)@�+@��!@��+@�n�@�ff@�M�@��-@��`@�bN@�(�@�ƨ@��@�V@��@���@��9@��D@�z�@�r�@�r�@�z�@�r�@�j@�r�@�j@�bN@�I�@��;@��@�~�@�^5@�$�@�{@�{@�J@���@��-@�%@�A�@� �@�(�@�A�@�b@�dZ@�=q@�5?@�-@�$�@��@�{@�-@��!@���@�?}@��@��@�A�@���@���@���@���@��+@�^5@�{@�7L@��@��u@��D@�j@�A�@�1'@���@��;@��w@��F@��F@��F@���@��P@�\)@�
=@���@��7@�hs@�`B@�p�@�?}@���@��/@��/@���@�A�@�t�@�\)@�C�@���@���@��+@�V@�{@��@��@���@�@��@�$�@��@���@�?}@��@�j@��@\)@~��@~ff@}��@}?}@|I�@{�F@{"�@z�H@z-@y��@yG�@x�9@xbN@x1'@x  @w��@w�w@w��@w��@w��@wK�@v�@v�+@v$�@u?}@t��@s�
@s��@s�@sC�@s"�@r�!@r�\@r^5@r^5@rM�@q�^@qx�@p�`@p1'@o�@o\)@n��@n��@nE�@m�@m��@m`B@m�@m�@l��@lj@k��@j�H@j�@jJ@i�#@ix�@i&�@hĜ@g�@g\)@g+@fȴ@f��@fv�@fE�@fE�@f5?@e@e�@e`B@eO�@e�@d�j@dZ@c�F@c"�@b��@b^5@bJ@a�#@a�7@a%@`��@`�9@_�@_+@^�+@]�h@\��@\��@\�j@\��@\�D@\z�@\9X@[�
@[C�@[o@Z=q@Y&�@X��@XĜ@XbN@XQ�@Xb@W��@W�@W;d@V�@Vv�@V$�@V{@U�-@T�@S��@SS�@R�H@R�\@Q�@QG�@P��@P�`@P�`@P�9@P�@Pb@O�;@O��@O;d@O�@O
=@O
=@N��@Nȴ@N�+@M�-@M�-@M��@M�h@M�h@Mp�@M?}@L�@L��@Lj@LI�@L(�@L(�@K�m@K�
@Kƨ@K��@K��@Kt�@K33@J�@J��@Jn�@J-@I��@I��@IG�@I&�@I7L@H�`@Hr�@Hb@G�@G;d@G
=@F��@F�+@Fff@F5?@E�T@E��@D�@D�j@Dz�@Dj@DZ@D9X@D(�@D1@D1@C�m@C�
@C��@C33@B��@BM�@BJ@BJ@A��@A��@A�7@A�7@Ahs@A&�@@�@@r�@@1'@?�@?|�@?�@>ȴ@>{@=@=�@<j@<9X@;S�@:�\@:�@9��@9��@9hs@9�@9%@8��@8�9@8Q�@81'@8 �@8 �@7�;@6�@6@5�h@5O�@5?}@5?}@4�@4j@4Z@4Z@4(�@41@3��@3�m@3��@3C�@2�@2M�@1��@1%@0�u@0Q�@0b@/�w@/|�@/\)@/
=@.�+@.E�@.5?@.5?@.5?@.$�@.@-��@-O�@-/@-�@-V@,��@,�j@,�D@,I�@+��@+�@+dZ@+"�@*��@)�@)��@)x�@)X@)&�@)%@(Ĝ@(Q�@(1'@'�@'�w@'|�@'�@&ȴ@&�+@&v�@&{@%��@%?}@%V@$��@$Z@$9X@$(�@$(�@$�@$1@#�m@#ƨ@#�F@#�@#dZ@#dZ@#"�@"�H@"�!@"^5@"J@!��@!�^@!x�@!�@ ��@ Ĝ@ �@  �@�@�P@|�@\)@K�@;d@+@�@ȴ@ff@@��@�@�@�D@I�@1@�
@ƨ@t�@C�@"�@�!@^5@-@��@�@�#@��@��@hs@G�@&�@%@��@Ĝ@��@�u@Q�@  @�w@�@��@\)@�@ȴ@�+@V@�@��@�-@�-@�-@�h@?}@�@��@z�@Z@(�@9X@(�@�@�m@ƨ@��@��@�@t�@dZ@dZ@C�@"�@�@��@~�@=q@�^@��@�7@x�@G�@&�@%@��@Ĝ@�@Q�@A�@1'@ �@b@  @�;@��@|�@l�@\)@;d@;d@
=@
=@
=@��@��@�y@�@��@��@�+@v�@V@�@��@�@��@z�@Z@(�@1@1@��@�m@ƨ@�F@��@S�@"�@"�@"�@"�@@
��@
~�@
n�@
^5@
=q@
J@	��@	�7@	x�@	G�@	&�@	%@�`@��@��@�u@�@�@Q�@A�@  @��@|�@K�@+@�@�+@E�@@�T@��@�-@�@`B@/@V@��@��@��@��@�j@�@�D@j@(�@��@�m@ƨ@��@t�@S�@33@"�@�@�H@��@�!@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A�ȴA��jA��!A��!A��-A��9A���A��7A�K�A�33A�A���A���A���A���A���A���A��uA��PA�v�A�`BA�Q�A��A�A���A���A��A���A�v�A�=qA��RA�"�A�A��RA�;dA��yA�9XA��/A�I�A��wA��!A��A�%A��;A�bNA�%A���A���A��A���A�S�A�
=A�x�A��A�|�A�|�A�9XA��
A��hA�ZA��A��;A�dZA�5?A�r�A��7A���A� �A��A��9A���A���A��#A�9XA��RA���A�{A� �A��#A�I�A�r�A�C�A�VA�Q�A�dZA�x�A���A�ffA�A�5?A��A�^A~�A}+A|ĜA|  Azr�Ay��Ay
=Awx�Av1At��As�;Ar�+Aop�AlI�AjA�Ah��Ah9XAg�FAgK�Af�AfVAe�wAd{A`z�A]�A\�DAZ�9AYƨAW��AV�/AV$�ATȴASp�AR��AR�AR�AR$�AQ�FAQhsAP�AOdZAOAN��AN�ANI�AM�hAL��AKt�AJ��AJn�AI�AI"�AG��AF�`AF�+AE�hAD�uAD^5ADM�AD5?AC�AC7LAA%A@v�A?�FA>�A=ƨA<ȴA;��A:��A:  A8bA5S�A4��A4z�A3��A2�yA2M�A1��A17LA0I�A.��A-��A,M�A+C�A*v�A* �A*  A)dZA(�`A(�+A&bNA%A$r�A$$�A#x�A"jA!`BA jA A�-A�A~�AVA%A\)AI�AA��AG�A��A��AK�A-A��A��A��A1'A�-A�A�AE�A�A
��A
��A
=qA	&�A��A  A�9A�+A��A��A�7A;dA��A��A��A �A �@��+@��@���@� �@��P@��y@�J@���@�(�@�n�@�+@�-@��/@��@��@��H@�/@�@�ff@�@��`@�b@ߝ�@�S�@��@�
=@��H@ޟ�@�@�X@�/@���@�(�@�  @ۍP@�ȴ@�M�@�$�@���@�G�@�Q�@�n�@��@���@θR@�G�@˕�@ʟ�@�V@�@ɩ�@��/@�|�@��@�+@�V@��@�  @�\)@��\@��@�x�@�b@�33@�5?@�G�@��H@���@�^5@�hs@�I�@��@��@��@��@�r�@�1'@��m@��F@��F@��F@���@�l�@�K�@��@�
=@�M�@��/@�1'@��;@���@���@��@�+@�n�@��@��@�bN@� �@���@�t�@�V@�Ĝ@�1'@�S�@��@�v�@��T@��u@���@��h@�%@�r�@�I�@���@���@��@���@�\)@�+@��!@��+@�n�@�ff@�M�@��-@��`@�bN@�(�@�ƨ@��@�V@��@���@��9@��D@�z�@�r�@�r�@�z�@�r�@�j@�r�@�j@�bN@�I�@��;@��@�~�@�^5@�$�@�{@�{@�J@���@��-@�%@�A�@� �@�(�@�A�@�b@�dZ@�=q@�5?@�-@�$�@��@�{@�-@��!@���@�?}@��@��@�A�@���@���@���@���@��+@�^5@�{@�7L@��@��u@��D@�j@�A�@�1'@���@��;@��w@��F@��F@��F@���@��P@�\)@�
=@���@��7@�hs@�`B@�p�@�?}@���@��/@��/@���@�A�@�t�@�\)@�C�@���@���@��+@�V@�{@��@��@���@�@��@�$�@��@���@�?}@��@�j@��@\)@~��@~ff@}��@}?}@|I�@{�F@{"�@z�H@z-@y��@yG�@x�9@xbN@x1'@x  @w��@w�w@w��@w��@w��@wK�@v�@v�+@v$�@u?}@t��@s�
@s��@s�@sC�@s"�@r�!@r�\@r^5@r^5@rM�@q�^@qx�@p�`@p1'@o�@o\)@n��@n��@nE�@m�@m��@m`B@m�@m�@l��@lj@k��@j�H@j�@jJ@i�#@ix�@i&�@hĜ@g�@g\)@g+@fȴ@f��@fv�@fE�@fE�@f5?@e@e�@e`B@eO�@e�@d�j@dZ@c�F@c"�@b��@b^5@bJ@a�#@a�7@a%@`��@`�9@_�@_+@^�+@]�h@\��@\��@\�j@\��@\�D@\z�@\9X@[�
@[C�@[o@Z=q@Y&�@X��@XĜ@XbN@XQ�@Xb@W��@W�@W;d@V�@Vv�@V$�@V{@U�-@T�@S��@SS�@R�H@R�\@Q�@QG�@P��@P�`@P�`@P�9@P�@Pb@O�;@O��@O;d@O�@O
=@O
=@N��@Nȴ@N�+@M�-@M�-@M��@M�h@M�h@Mp�@M?}@L�@L��@Lj@LI�@L(�@L(�@K�m@K�
@Kƨ@K��@K��@Kt�@K33@J�@J��@Jn�@J-@I��@I��@IG�@I&�@I7L@H�`@Hr�@Hb@G�@G;d@G
=@F��@F�+@Fff@F5?@E�T@E��@D�@D�j@Dz�@Dj@DZ@D9X@D(�@D1@D1@C�m@C�
@C��@C33@B��@BM�@BJ@BJ@A��@A��@A�7@A�7@Ahs@A&�@@�@@r�@@1'@?�@?|�@?�@>ȴ@>{@=@=�@<j@<9X@;S�@:�\@:�@9��@9��@9hs@9�@9%@8��@8�9@8Q�@81'@8 �@8 �@7�;@6�@6@5�h@5O�@5?}@5?}@4�@4j@4Z@4Z@4(�@41@3��@3�m@3��@3C�@2�@2M�@1��@1%@0�u@0Q�@0b@/�w@/|�@/\)@/
=@.�+@.E�@.5?@.5?@.5?@.$�@.@-��@-O�@-/@-�@-V@,��@,�j@,�D@,I�@+��@+�@+dZ@+"�@*��@)�@)��@)x�@)X@)&�@)%@(Ĝ@(Q�@(1'@'�@'�w@'|�@'�@&ȴ@&�+@&v�@&{@%��@%?}@%V@$��@$Z@$9X@$(�@$(�@$�@$1@#�m@#ƨ@#�F@#�@#dZ@#dZ@#"�@"�H@"�!@"^5@"J@!��@!�^@!x�@!�@ ��@ Ĝ@ �@  �@�@�P@|�@\)@K�@;d@+@�@ȴ@ff@@��@�@�@�D@I�@1@�
@ƨ@t�@C�@"�@�!@^5@-@��@�@�#@��@��@hs@G�@&�@%@��@Ĝ@��@�u@Q�@  @�w@�@��@\)@�@ȴ@�+@V@�@��@�-@�-@�-@�h@?}@�@��@z�@Z@(�@9X@(�@�@�m@ƨ@��@��@�@t�@dZ@dZ@C�@"�@�@��@~�@=q@�^@��@�7@x�@G�@&�@%@��@Ĝ@�@Q�@A�@1'@ �@b@  @�;@��@|�@l�@\)@;d@;d@
=@
=@
=@��@��@�y@�@��@��@�+@v�@V@�@��@�@��@z�@Z@(�@1@1@��@�m@ƨ@�F@��@S�@"�@"�@"�@"�@@
��@
~�@
n�@
^5@
=q@
J@	��@	�7@	x�@	G�@	&�@	%@�`@��@��@�u@�@�@Q�@A�@  @��@|�@K�@+@�@�+@E�@@�T@��@�-@�@`B@/@V@��@��@��@��@�j@�@�D@j@(�@��@�m@ƨ@��@t�@S�@33@"�@�@�H@��@�!@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B{�B�B�B�B�B�B� B� B}�B�B�B�B�DB�hB�hB�oB�oB�oB�hB�bB�oB��B�{B��B��B��B��B�{B�PB�DB�B^5BffBn�BffBo�B��B��B��B��B�DBq�B|�BaHBk�BL�BK�BQ�BXBYBN�BQ�BF�B=qB0!B�B-B#�B�B�B�B�B�5B��B��B�3B�9B�?B�dB�B�{Bz�Bx�B\)BXBH�B�B!�B�BB
��BDB
�`B
�B
��B
�^B
�B
�%B
u�B
dZB
O�B
0!B
I�B
O�B
P�B
E�B
33B
1'B
1'B
�B
VB
\B
B	�sB	ĜB	B	�jB	��B	��B	��B	��B	ÖB	ŢB	�9B	��B	k�B	aHB	v�B	ffB	n�B	XB	e`B	_;B	VB	O�B	^5B	cTB	bNB	\)B	W
B	T�B	I�B	@�B	M�B	N�B	K�B	C�B	9XB	1'B	'�B	.B	/B	)�B	�B	VB	oB	�B	DB		7B	{B	�B	\B	%B��B�#B�B�B�/B�#B��B��BŢBƨB��B��B�jB��B�RB�B�'B�B�B��B�\B�7B�%B�7B�uB��B��B�VB�+B�BdZBk�Bx�B{�Br�Be`BgmBffBp�Bp�BgmBdZBcTBK�B=qBD�BL�B9XB,B-B)�B&�B)�B'�B�B,B6FB6FB49B49B2-B%�B6FB8RB,B�B$�B �B\BB"�B33B33B-B,B&�B�B�B�B�B%�B&�B)�B(�B$�B!�B�B�BoB��BhBhBB�mBBuB�B�B"�B(�B&�B-B.B0!B1'B/B-B(�B.B1'B/B-B2-B/B,B/B2-B-B%�B�BVB  B%B$�B$�B'�B33B;dB:^B6FB/B&�B�B%�B1'B;dBG�BB�B>wBA�B@�B9XB;dB=qB=qB2-BK�BI�BB�B>wB<jBQ�BVB]/B_;B`BBaHBe`BhsBgmBffBffBgmBe`BffB`BB_;Bl�Bt�Bv�Bx�Bv�Br�Bo�Bo�B|�B�%B�B�B�B{�B�B�{B�uB��B��B��B��B��B�B�qB�}BƨBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�`B�B��B��B��B��B��B��B��B��B��B	+B		7B	+B	B��B��B		7B		7B		7B	
=B	
=B	DB	\B	+B	%B	uB	�B	uB	�B	�B	�B	!�B	+B	-B	+B	&�B	+B	49B	49B	49B	5?B	7LB	7LB	:^B	<jB	>wB	?}B	?}B	?}B	>wB	;dB	:^B	6FB	C�B	N�B	R�B	W
B	VB	VB	YB	ZB	\)B	XB	W
B	dZB	e`B	cTB	e`B	ffB	iyB	iyB	l�B	o�B	p�B	p�B	p�B	o�B	n�B	o�B	m�B	s�B	s�B	w�B	x�B	z�B	~�B	}�B	~�B	~�B	�B	�%B	�1B	�+B	�=B	�JB	�PB	�bB	�oB	�uB	�uB	�{B	�{B	��B	�{B	�oB	�hB	�uB	�{B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�!B	�-B	�3B	�'B	�'B	�!B	�'B	�9B	�^B	�XB	�RB	�XB	�XB	�RB	�jB	��B	�}B	��B	��B	B	ĜB	ÖB	B	ĜB	ŢB	ƨB	ŢB	ĜB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�fB	�sB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
  B
%B
%B
%B
%B
%B
B
B
%B
+B
1B
	7B

=B
	7B
DB
DB
DB
DB
DB
DB
DB
JB
DB
JB
JB
VB
VB
\B
bB
\B
\B
bB
bB
bB
oB
oB
hB
uB
uB
uB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
�B
"�B
$�B
&�B
'�B
'�B
'�B
)�B
(�B
(�B
(�B
)�B
+B
)�B
'�B
#�B
'�B
+B
.B
/B
/B
.B
-B
1'B
1'B
0!B
1'B
0!B
0!B
/B
.B
-B
,B
-B
.B
1'B
2-B
33B
33B
49B
5?B
49B
49B
6FB
8RB
8RB
8RB
7LB
7LB
6FB
7LB
8RB
9XB
9XB
9XB
8RB
8RB
8RB
6FB
:^B
:^B
9XB
9XB
7LB
;dB
=qB
=qB
=qB
=qB
=qB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
?}B
@�B
@�B
B�B
B�B
B�B
D�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
F�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
I�B
J�B
J�B
K�B
K�B
J�B
L�B
N�B
N�B
O�B
O�B
N�B
O�B
O�B
N�B
O�B
Q�B
Q�B
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
S�B
R�B
T�B
VB
T�B
T�B
T�B
T�B
VB
VB
VB
YB
YB
ZB
YB
XB
W
B
W
B
YB
ZB
ZB
[#B
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
\)B
[#B
[#B
\)B
\)B
\)B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
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
bNB
bNB
cTB
cTB
bNB
aHB
_;B
^5B
cTB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
ffB
gmB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
gmB
gmB
ffB
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
k�B
k�B
k�B
k�B
jB
k�B
jB
iyB
l�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
q�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B� B�B�'B�'B�B�B�'B�'B�B�B�B�B�'B�B�B�B� BB|jB�-B�-B�B�B�'B�4B�4B~wB�uB��B��B��B�hB��B�oB�oB��B��B��B��B��B��B��B��B��B��B��B�<B�JB��BbBiyBq[Bi�Br�B��B��B�'B�#B��BtnB~�BdBl�BO�BN<BS�BY1BZQBPbBR�BG�B>wB1�B�B-�B$�B�B��B�=BیBߊBāB��B�?B�B�`B�B�OB�$B}�Bz�B_�BY�BK^B�B$@B�BYBB�B
�>B
�)B
�(B
��B
�/B
�RB
x�B
f�B
S�B
3�B
KxB
P�B
Q�B
F�B
4�B
2GB
2B
�B
B
�B
�B	��B	ȴB	�%B	��B	�(B	��B	өB	ΊB	ĜB	�%B	��B	�WB	pB	d�B	xB	h�B	o�B	Z�B	f�B	`\B	W�B	Q�B	^�B	c�B	b�B	\�B	W�B	U�B	KB	A�B	NVB	O(B	LB	DMB	:xB	2|B	)yB	/ B	/�B	*�B	�B	.B	uB	?B	�B	
�B	�B	�B	�B	�B�B��B��B�B��B�xB�gB�pB�EB��B��B��B��B� B�rB�;B�B�B��B�BB�4B�B��B��B�{B�B�B�\B�1B�Bg8BmCBy�B|�Bs�BgBh�Bg�Bq[Bq'BhsBeFBd@BM�B?�BFBM�B;JB./B.�B+�B(�B+�B)yB!�B-CB7B7B5?B5B3B'mB6�B8�B-)B5B%�B!�BhB�B#�B3hB3�B-�B,�B'�B�B�B�B�B&�B'�B*�B)�B%�B"�B�B�B�B OBTB�B-B�B�B�B�B�B#�B)�B'�B-]B.cB0UB1[B/iB-wB)�B.cB1[B/�B-�B2|B/�B,�B/�B2|B-�B&�B�BB�BKB%�B%�B)DB3�B;�B:�B6�B/�B($B�B'RB2�B<BG�BCGB?.BB'BA B:^B<6B>BB>]B3�BK�BJ=BC{B?�B=�BRoBV�B]dB_�B`�Ba�Be�Bh�Bg�Bf�Bf�Bg�Be�Bf�BaB`vBmBuBwBy	BwBs3BpUBp�B}qB�?B��B��B��B}B�3B��B�FB��B�bB�|B��B�-B��B��B� B��B�B��B�B��B�B�"B�PB� B�&B�B�:B�jB�jB�oB�_B�yBؓB�B�B�B��B��B�B��B��B��B��B��B��B��B�B��B��B�B��B��B�B��B�B�B�0B�>B�lB��B	EB		RB	_B	[B�B��B		7B		lB		lB	
rB	
rB	^B	\B	�B	�B	�B	�B	�B	B	B	OB	"4B	+B	-]B	+kB	'�B	+�B	4TB	4nB	4nB	5tB	7�B	7�B	:xB	<�B	>�B	?�B	?�B	?�B	>�B	;�B	:�B	7B	C�B	N�B	S&B	W$B	V9B	VSB	YKB	Z7B	\]B	X�B	W�B	dZB	e�B	c�B	e�B	f�B	i�B	i�B	l�B	o�B	p�B	p�B	p�B	o�B	n�B	o�B	nB	tB	t9B	x8B	y>B	{JB	.B	~]B	cB	}B	�gB	�tB	�fB	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�&B	�B	�KB	�WB	�]B	�/B	�OB	�[B	�oB	�aB	�3B	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	żB	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�"B	�B	� B	� B	�HB	�4B	�MB	�gB	�WB	�\B	�|B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�8B	�*B	�"B
 B
 B	�.B	�.B	�HB
;B
;B
UB
MB
3B
B
MB
-B
[B
 iB
?B
%B
?B
?B
YB
SB
SB
YB
EB
fB
	lB

=B
	RB
DB
xB
xB
xB
xB
xB
^B
~B
xB
dB
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
B
�B
 'B
 'B
"�B
 'B
# B
%,B
'B
(
B
(
B
($B
*B
)*B
)*B
)*B
*0B
+B
*B
(>B
$@B
(XB
+QB
.IB
/B
/5B
.IB
-CB
1AB
1'B
0;B
1[B
0;B
0UB
/OB
.IB
-]B
,qB
-]B
.cB
1vB
2aB
3hB
3hB
4nB
5ZB
4�B
4�B
6`B
8RB
8lB
8lB
7�B
7�B
6`B
7fB
8�B
9XB
9rB
9rB
8�B
8�B
8�B
6zB
:xB
:xB
9�B
9�B
7�B
;B
=qB
=�B
=�B
=�B
=�B
<�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
?�B
@�B
@�B
B�B
B�B
B�B
D�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
F�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
H�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
J	B
J�B
KB
K�B
LB
KB
MB
N�B
OB
O�B
O�B
OB
O�B
O�B
O(B
O�B
RB
RB
SB
SB
S&B
S&B
S&B
TB
T,B
T,B
T,B
T,B
T,B
T,B
TB
SB
UB
VB
UB
U2B
U2B
UB
V9B
VB
VB
Y1B
YKB
Z7B
Y1B
XEB
W?B
WYB
YKB
Z7B
ZQB
[WB
\)B
\)B
[=B
[WB
[WB
\CB
\)B
\CB
\)B
\CB
]IB
\]B
\CB
[WB
[WB
\]B
\CB
\xB
^5B
_pB
_VB
^jB
_VB
_pB
_pB
_pB
_pB
`vB
`\B
aHB
abB
abB
aHB
`vB
`\B
a|B
bhB
bNB
b�B
bNB
bhB
cnB
cnB
cnB
cTB
cTB
b�B
b�B
cTB
cnB
b�B
a|B
_pB
^�B
c�B
e`B
e�B
e�B
f�B
f�B
gmB
gmB
f�B
g�B
f�B
f�B
f�B
g�B
hsB
hsB
hsB
g�B
g�B
f�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
j�B
k�B
j�B
i�B
l�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
q�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901220033352019012200333520190122003335201901220200172019012202001720190122020017201901230020392019012300203920190123002039  JA  ARFMdecpA19c                                                                20190118093631  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190118003633  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190118003636  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190118003637  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190118003637  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190118003637  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190118003638  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190118003638  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190118003638  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190118003638                      G�O�G�O�G�O�                JA  ARUP                                                                        20190118005832                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190118153503  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190121153335  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190121153335  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190121170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190122152039  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                