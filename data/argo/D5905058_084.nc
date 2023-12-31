CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-24T09:35:49Z creation;2018-08-24T09:35:52Z conversion to V3.1;2019-12-23T06:16:21Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20180824093549  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               TA   JA  I2_0675_084                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�|_#�1   @�|���@8#�e��O�c4)�y��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
CW
CW
CW
CW
C
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8\D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D��D�
�D�J�D���D�ǮD�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D�D���D�
�D�J�DÊ�D���D�
�D�J�DĊ�D���D�
�D�J�DŊ�D���D�
�D�J�DƊ�D���D�
�D�J�DǊ�D���D�
�D�J�DȊ�D���D�
�D�J�DɊ�D���D�
�D�J�Dʊ�D���D�
�D�G�Dˊ�D���D�
�D�J�D̊�D���D�
�D�J�D͊�D���D�
�D�J�DΊ�D���D�
�D�J�Dϊ�D���D�
�D�J�DЊ�D���D�
�D�J�Dъ�D���D�
�D�J�DҊ�D���D�
�D�J�Dӊ�D���D�
�D�J�DԊ�D���D�
�D�J�DՊ�D���D�
�D�J�D֊�D���D�
�D�J�D׊�D���D�
�D�J�D؊�D���D�
�D�J�Dي�D���D�
�D�J�Dڊ�D���D�
�D�J�Dۊ�D���D�
�D�J�D܊�D���D�
�D�J�D݊�D���D�
�D�J�Dފ�D���D�
�D�J�Dߊ�D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D�ǮD�
�D�J�D���D���D�
�D�J�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AσAυAω7AϋDAύPAύPAϏ\AϏ\AϏ\AϑhAϑhAϑhAϑhAϓuAϓuAσAω7AυA�bNA���A�%A���A�$�AˑhAˑhA˓uAˑhAˍPA˃A�S�AʓuA��A�n�A�33A�hsA��A�hsA��
A�G�A�v�A��/A��;A�A�Q�A��A�^5A�JA��7A��HA�E�A�ĜA�C�A��7A���A�G�A���A�x�A��A��A�C�A��`A��RA�I�A��/A�
=A��wA��/A�5?A�O�A��yA�VA���A�+A�  A��A�-A�VA�$�A��^A��uA�`BA���A��A��-A�ffA���A��A�"�A�p�A���A���A�hsA�oA��uA���A�JA�I�A�bA��DA��/A�;dA�&�A���A��DA��RA�$�A��#A�XA��A���A��A��A���A��FA�JA�M�A��7A��A�bA��\A���A��A���A�XA�{A~�`A|��A{x�Az�yAyAy%Ax(�Aut�As%Ap1'Am33Ak�#Ai`BAg"�AeoAc�A`1A^��A]�7A[��AY��AUS�AS�FAQ��AO��ANA�AM��AM�AL��AIAE"�AA�#A@9XA?�A>�9A<�+A;K�A:n�A8�jA7`BA6A�A4�DA3x�A1S�A/�^A. �A+�;A+�A)��A'l�A%/A!�A!
=A �HA ��A ��A ffA ffA (�A?}A�TAt�A�+A�#A�yAE�A�wA��A�+A�-A~�AAI�A��A��A�A�7AO�A��A��Ar�A33A  A��A��AAbA��A?}A
^5A	K�A�DA�Az�AAZA�#AoA�uA�Ap�AS�A%@�ƨ@�`B@�r�@���@���@��-@���@��F@��@�V@�  @�@�R@���@�V@�@� �@�@���@�hs@�z�@�P@�ȴ@��#@��@��y@�?}@�w@���@�V@��@�Q�@އ+@���@���@�^5@�G�@� �@׍P@�
=@�-@�7L@�Q�@�33@҇+@��T@�G�@�bN@�ƨ@�+@�V@��@�O�@��/@�j@��;@�@�~�@ɑh@��@Ǯ@�ȴ@�bN@�"�@���@�?}@��u@�n�@�
=@��P@��@�O�@���@�bN@�1@�ƨ@��F@���@�C�@���@�v�@��@���@�?}@��@���@�;d@���@���@��^@�/@���@���@���@��#@�%@�1'@��@��@���@�`B@���@�;d@���@�v�@�=q@���@��@�j@���@�o@���@���@���@��y@���@��!@��@�$�@��@�~�@��\@���@��@�b@���@��`@���@��@�o@�
=@�
=@�@���@���@��y@��\@�-@��#@��@���@�Z@��@��@��@��;@��P@�;d@�o@���@��R@�v�@�^5@�M�@�$�@���@���@���@�hs@��@���@�;d@���@�M�@�E�@�@��@���@�Z@��@� �@�\)@��H@���@�~�@�^5@�5?@�-@��@��-@��7@�p�@�p�@�X@�?}@�/@�/@���@���@�z�@�I�@�(�@�b@��@��@�t�@�K�@��@��+@�M�@�=q@�-@��@�@��@��h@�7L@�V@��@���@��9@��@�Q�@�9X@��@���@��@�\)@�S�@�+@�+@�b@�bN@�Z@�1'@� �@��m@��w@���@�l�@�
=@��@�n�@��@�@��#@��#@��^@��7@�p�@�hs@�G�@���@���@���@���@��u@�j@�b@��m@�ƨ@���@�;d@��y@���@�v�@�v�@�^5@�J@�@�p�@���@�r�@�1'@� �@�  @��
@��
@�ƨ@���@��@�\)@�33@��@��@��@��!@���@�v�@�E�@�5?@�$�@�{@��^@���@��h@��@�X@��@���@���@�Ĝ@���@��@�r�@�j@�1'@�w@;d@~�@~��@~��@~��@~ff@~@}@}`B@}O�@|�@|(�@{�
@{�F@{��@{@y�#@y��@yhs@y&�@x�`@x��@xbN@xA�@xb@w�@w;d@w;d@v�y@vff@v{@u�h@uO�@u�@uV@t��@t�/@t��@t(�@s�
@s��@sS�@s33@r��@r~�@rM�@qhs@pr�@p �@o�@o�@ol�@o
=@n�@n�R@n��@nv�@n@mO�@lz�@l9X@kt�@kC�@k33@ko@j�@j=q@ix�@i&�@h��@h��@h�u@hbN@g�w@g;d@f�@fv�@f5?@e�@e�@e/@d�/@d�D@dz�@dI�@d1@c��@c@b��@b^5@b=q@a�@ax�@a�@a%@a%@`�`@`Ĝ@`�9@`bN@` �@_\)@^��@^�+@^V@]�@]p�@\�@[��@[dZ@[S�@[33@["�@[@Z�H@Z�\@Y��@Y��@YG�@X�`@XA�@W�w@W|�@W+@V�@Vff@V{@U�T@U�@UV@T�/@T��@T�j@Tj@T1@S@RJ@Q��@QX@Q�@Q%@P��@P��@P��@Pr�@PA�@P  @O�;@O�P@Ol�@O
=@N�R@NE�@M��@M`B@MO�@M/@MV@Lz�@L1@K�@K"�@J�!@I��@I�#@I��@I�7@I7L@H�`@HĜ@H�u@G�w@G;d@Fȴ@F��@Fff@E��@EO�@E/@D��@D��@D�@D�@D�@Dz�@DZ@D9X@D�@C�F@CS�@B��@Bn�@BJ@A�@A��@A7L@A%@@Ĝ@@�@@bN@@  @?��@?+@>v�@>@=@=�@=p�@=/@<��@<j@;�m@;S�@;"�@;@:�H@:��@:�\@:^5@:M�@9��@9��@9&�@8��@8Ĝ@8�@8r�@8Q�@8 �@7��@7�@7|�@7K�@6�y@6v�@6{@5�-@5p�@5/@4�/@4�D@49X@41@3�F@3t�@333@3"�@2�H@2=q@1�^@1�7@1hs@1%@0��@0��@0Q�@/��@/|�@/|�@/\)@/
=@.��@.�@.�+@.V@.{@.@-��@-V@,�@,�/@,�D@,1@+�F@+�@+t�@+dZ@+"�@*��@*�\@*n�@*^5@*M�@*-@)��@)hs@)G�@)&�@)�@(��@(��@(��@(Q�@(b@'�@'�w@'l�@';d@&�y@&�R@&�+@&E�@&@%�@%��@%��@%?}@$�/@$��@$�j@$�@$�D@$I�@#�
@#��@#t�@#dZ@#S�@#"�@"�!@"��@"�\@"n�@"^5@"=q@"-@"-@"J@!�@!��@!��@!X@!G�@!%@ ��@ Ĝ@ ��@ r�@ b@�;@�@��@|�@K�@��@ȴ@��@ff@�@@�h@p�@O�@?}@/@V@�/@��@��@�j@�j@�@��@z�@j@9X@1@��@1@�m@dZ@"�@�H@�\@n�@=q@J@��@�#@��@G�@��@Ĝ@�9@�@bN@1'@  @�@�@�;@�;@�w@��@\)@
=@�R@�R@�+@$�@��@��@@@@�-@O�@/@��@�j@z�@I�@1@��@dZ@S�@33@o@�H@~�@-@J@�#@��@�7@hs@G�@&�@%@�9@�u@r�@bN@A�@1'@ �@�@��@�w@\)@�@
=@�y@�y@�@ȴ@v�@E�@$�@�@@p�@O�@?}@�j@�D@�@��@S�@"�@@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AσAυAω7AϋDAύPAύPAϏ\AϏ\AϏ\AϑhAϑhAϑhAϑhAϓuAϓuAσAω7AυA�bNA���A�%A���A�$�AˑhAˑhA˓uAˑhAˍPA˃A�S�AʓuA��A�n�A�33A�hsA��A�hsA��
A�G�A�v�A��/A��;A�A�Q�A��A�^5A�JA��7A��HA�E�A�ĜA�C�A��7A���A�G�A���A�x�A��A��A�C�A��`A��RA�I�A��/A�
=A��wA��/A�5?A�O�A��yA�VA���A�+A�  A��A�-A�VA�$�A��^A��uA�`BA���A��A��-A�ffA���A��A�"�A�p�A���A���A�hsA�oA��uA���A�JA�I�A�bA��DA��/A�;dA�&�A���A��DA��RA�$�A��#A�XA��A���A��A��A���A��FA�JA�M�A��7A��A�bA��\A���A��A���A�XA�{A~�`A|��A{x�Az�yAyAy%Ax(�Aut�As%Ap1'Am33Ak�#Ai`BAg"�AeoAc�A`1A^��A]�7A[��AY��AUS�AS�FAQ��AO��ANA�AM��AM�AL��AIAE"�AA�#A@9XA?�A>�9A<�+A;K�A:n�A8�jA7`BA6A�A4�DA3x�A1S�A/�^A. �A+�;A+�A)��A'l�A%/A!�A!
=A �HA ��A ��A ffA ffA (�A?}A�TAt�A�+A�#A�yAE�A�wA��A�+A�-A~�AAI�A��A��A�A�7AO�A��A��Ar�A33A  A��A��AAbA��A?}A
^5A	K�A�DA�Az�AAZA�#AoA�uA�Ap�AS�A%@�ƨ@�`B@�r�@���@���@��-@���@��F@��@�V@�  @�@�R@���@�V@�@� �@�@���@�hs@�z�@�P@�ȴ@��#@��@��y@�?}@�w@���@�V@��@�Q�@އ+@���@���@�^5@�G�@� �@׍P@�
=@�-@�7L@�Q�@�33@҇+@��T@�G�@�bN@�ƨ@�+@�V@��@�O�@��/@�j@��;@�@�~�@ɑh@��@Ǯ@�ȴ@�bN@�"�@���@�?}@��u@�n�@�
=@��P@��@�O�@���@�bN@�1@�ƨ@��F@���@�C�@���@�v�@��@���@�?}@��@���@�;d@���@���@��^@�/@���@���@���@��#@�%@�1'@��@��@���@�`B@���@�;d@���@�v�@�=q@���@��@�j@���@�o@���@���@���@��y@���@��!@��@�$�@��@�~�@��\@���@��@�b@���@��`@���@��@�o@�
=@�
=@�@���@���@��y@��\@�-@��#@��@���@�Z@��@��@��@��;@��P@�;d@�o@���@��R@�v�@�^5@�M�@�$�@���@���@���@�hs@��@���@�;d@���@�M�@�E�@�@��@���@�Z@��@� �@�\)@��H@���@�~�@�^5@�5?@�-@��@��-@��7@�p�@�p�@�X@�?}@�/@�/@���@���@�z�@�I�@�(�@�b@��@��@�t�@�K�@��@��+@�M�@�=q@�-@��@�@��@��h@�7L@�V@��@���@��9@��@�Q�@�9X@��@���@��@�\)@�S�@�+@�+@�b@�bN@�Z@�1'@� �@��m@��w@���@�l�@�
=@��@�n�@��@�@��#@��#@��^@��7@�p�@�hs@�G�@���@���@���@���@��u@�j@�b@��m@�ƨ@���@�;d@��y@���@�v�@�v�@�^5@�J@�@�p�@���@�r�@�1'@� �@�  @��
@��
@�ƨ@���@��@�\)@�33@��@��@��@��!@���@�v�@�E�@�5?@�$�@�{@��^@���@��h@��@�X@��@���@���@�Ĝ@���@��@�r�@�j@�1'@�w@;d@~�@~��@~��@~��@~ff@~@}@}`B@}O�@|�@|(�@{�
@{�F@{��@{@y�#@y��@yhs@y&�@x�`@x��@xbN@xA�@xb@w�@w;d@w;d@v�y@vff@v{@u�h@uO�@u�@uV@t��@t�/@t��@t(�@s�
@s��@sS�@s33@r��@r~�@rM�@qhs@pr�@p �@o�@o�@ol�@o
=@n�@n�R@n��@nv�@n@mO�@lz�@l9X@kt�@kC�@k33@ko@j�@j=q@ix�@i&�@h��@h��@h�u@hbN@g�w@g;d@f�@fv�@f5?@e�@e�@e/@d�/@d�D@dz�@dI�@d1@c��@c@b��@b^5@b=q@a�@ax�@a�@a%@a%@`�`@`Ĝ@`�9@`bN@` �@_\)@^��@^�+@^V@]�@]p�@\�@[��@[dZ@[S�@[33@["�@[@Z�H@Z�\@Y��@Y��@YG�@X�`@XA�@W�w@W|�@W+@V�@Vff@V{@U�T@U�@UV@T�/@T��@T�j@Tj@T1@S@RJ@Q��@QX@Q�@Q%@P��@P��@P��@Pr�@PA�@P  @O�;@O�P@Ol�@O
=@N�R@NE�@M��@M`B@MO�@M/@MV@Lz�@L1@K�@K"�@J�!@I��@I�#@I��@I�7@I7L@H�`@HĜ@H�u@G�w@G;d@Fȴ@F��@Fff@E��@EO�@E/@D��@D��@D�@D�@D�@Dz�@DZ@D9X@D�@C�F@CS�@B��@Bn�@BJ@A�@A��@A7L@A%@@Ĝ@@�@@bN@@  @?��@?+@>v�@>@=@=�@=p�@=/@<��@<j@;�m@;S�@;"�@;@:�H@:��@:�\@:^5@:M�@9��@9��@9&�@8��@8Ĝ@8�@8r�@8Q�@8 �@7��@7�@7|�@7K�@6�y@6v�@6{@5�-@5p�@5/@4�/@4�D@49X@41@3�F@3t�@333@3"�@2�H@2=q@1�^@1�7@1hs@1%@0��@0��@0Q�@/��@/|�@/|�@/\)@/
=@.��@.�@.�+@.V@.{@.@-��@-V@,�@,�/@,�D@,1@+�F@+�@+t�@+dZ@+"�@*��@*�\@*n�@*^5@*M�@*-@)��@)hs@)G�@)&�@)�@(��@(��@(��@(Q�@(b@'�@'�w@'l�@';d@&�y@&�R@&�+@&E�@&@%�@%��@%��@%?}@$�/@$��@$�j@$�@$�D@$I�@#�
@#��@#t�@#dZ@#S�@#"�@"�!@"��@"�\@"n�@"^5@"=q@"-@"-@"J@!�@!��@!��@!X@!G�@!%@ ��@ Ĝ@ ��@ r�@ b@�;@�@��@|�@K�@��@ȴ@��@ff@�@@�h@p�@O�@?}@/@V@�/@��@��@�j@�j@�@��@z�@j@9X@1@��@1@�m@dZ@"�@�H@�\@n�@=q@J@��@�#@��@G�@��@Ĝ@�9@�@bN@1'@  @�@�@�;@�;@�w@��@\)@
=@�R@�R@�+@$�@��@��@@@@�-@O�@/@��@�j@z�@I�@1@��@dZ@S�@33@o@�H@~�@-@J@�#@��@�7@hs@G�@&�@%@�9@�u@r�@bN@A�@1'@ �@�@��@�w@\)@�@
=@�y@�y@�@ȴ@v�@E�@$�@�@@p�@O�@?}@�j@�D@�@��@S�@"�@@
��@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBDBDBDB	7BB��B��B�B�B�B��B��B��BBVBDB��B��BBhB6FBC�BD�BQ�BVBXB\)B[#B\)BYBXBYBYBS�BXBcTBp�Bo�BhsBv�Bw�Bx�B{�B}�B~�B�B�%B�7B��B�DB�=B~�B{�Bw�Br�Bk�Be`BjBy�B}�B� B� B�DB�B�B� By�Bp�BbNBYBQ�B@�B/B�BoB�BbB
=B  B�B�HB��BƨB��B�XB��B�Bz�Bk�B]/BT�BN�B;dB+B�BPB  B
�B
�B
��B
��B
��B
�JB
��B
�dB
�^B
��B
�uB
�\B
�B
m�B
XB
K�B
E�B
=qB
33B
%�B

=B	�B	��B	�B	��B	�B	l�B	R�B	P�B	<jB	/B	#�B	oB	+B�B��B�XB��B��B��B��B��B�PBk�BQ�B`BBe`Bk�BffB_;B]/B[#BXBXBS�BZBXBW
BN�BH�BD�B>wB49B33B7LB33B33B33B33B33B2-B2-B2-B33B2-B2-B2-B2-B2-B33B2-B2-B33B33B33B33B2-B2-B2-B33B49B5?B49B33B5?B5?B33B33B2-B1'B0!B0!B1'B0!B1'B/B.B.B.B-B.B-B/B.B.B.B/B/B/B.B.B/B/B.B-B,B)�B)�B)�B)�B)�B(�B(�B)�B(�B(�B(�B(�B'�B'�B(�B(�B&�B&�B&�B&�B'�B'�B(�B'�B'�B(�B(�B)�B)�B)�B,B,B-B.B.B/B/B0!B1'B1'B33B49B6FB6FB8RB:^BA�BC�BI�BK�BK�BM�BM�BR�BW
B]/B`BBbNB`BB\)BaHBcTBdZBhsBjBk�Bl�Bl�Bm�Bp�Bq�Bs�Bu�Bv�Bz�B|�B� B�B�B�+B�7B�bB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�?B�^B�jB��BŢB��B��B��B�NB�B�B�B�sB�mB�B�B�B��B	B	%B	
=B	PB	hB	uB	uB	uB	uB	uB	uB	uB	{B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	#�B	%�B	&�B	)�B	,B	.B	/B	/B	0!B	1'B	2-B	5?B	8RB	:^B	<jB	?}B	G�B	H�B	K�B	M�B	O�B	VB	XB	\)B	aHB	aHB	ffB	hsB	l�B	m�B	o�B	q�B	u�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�%B	�1B	�DB	�PB	�VB	�VB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�3B	�FB	�qB	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
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
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
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
G�B
H�B
H�B
H�B
H�B
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
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
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
P�B
P�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
Q�B
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
XB
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
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
]/B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
bNB
bNB
cTB
cTB
cTB
cTB
dZB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0B0B0B0B0B0B0B0B0B0B0B0B0B0B0B)B)B)B	B�B��B��B�B��B�B��B��B��B�B<B)B��B��BBNB6+BC{BD�BQ�BU�BW�B\B[	B\BX�BW�BX�BX�BS�BW�Bc:Bp�Bo�BhXBv�Bw�Bx�B{�B}�B~�B��B�B�B�gB�)B�#B~�B{�Bw�Br�BkkBeFBjeBy�B}�B�B�B�)B�B��B�By�Bp�Bb4BX�BQ�B@iB.�B�BTBgBHB
#B��B�oB�B̳BƎB�oB�>B��B��Bz�BkkB]BT�BN�B;JB*�B�BB
��B
�B
��B
̳B
�iB
��B
�0B
�kB
�0B
�DB
��B
�[B
�BB
��B
mwB
W�B
K�B
E�B
=VB
3B
%�B

#B	�iB	��B	��B	��B	��B	lWB	R�B	P�B	<PB	/ B	#�B	TB	B�BʌB�$B��B��B��B��B��B�BkQBQ�B`Be,BkQBfLB_B\�B[	BW�BW�BS�BY�BW�BV�BN�BH�BDgB>]B4B2�B7B3B2�B3B2�B3B2B2B1�B2�B1�B1�B2B1�B2B2�B1�B1�B3B3B3B2�B2B1�B2B2�B4B5%B4B2�B5%B5B2�B3B1�B1B0B0B0�B0B1B.�B-�B-�B-�B,�B-�B,�B.�B-�B-�B-�B/ B/ B.�B-�B-�B.�B.�B-�B,�B+�B)�B)�B)�B)�B)�B(�B(�B)�B(�B(�B(�B(�B'�B'�B(�B(�B&�B&�B&�B&�B'�B'�B(�B'�B'�B(�B(�B)�B)�B)�B+�B+�B,�B-�B-�B.�B.�B/�B1B0�B3B4B6+B6+B8B:DBAoBCaBI�BK�BK�BM�BM�BR�BV�B\�B`BbB`B\BaBc Bd&Bh>BjKBkkBlWBlWBm]BpoBqvBs�Bu�Bv�Bz�B|�B�B��B��B�B�B�.B�FB�MB�yB�eB�xB��B��B��B��B��B��B��B��B��B�B�B�DB�6B�iB�mBˬBбB��B�B�iB�|B�vB�>B�8B�eB�]B�B��B	�B	�B	
#B	6B	4B	[B	@B	@B	[B	@B	@B	@B	FB	sB	eB	�B	xB	 �B	!�B	!�B	!�B	#�B	%�B	&�B	)�B	+�B	-�B	/ B	/ B	0B	0�B	2B	5%B	8B	:*B	<6B	?HB	GzB	H�B	K�B	M�B	O�B	U�B	W�B	[�B	aB	aB	fLB	h>B	lWB	m]B	o�B	q�B	u�B	y�B	{�B	}�B	�B	��B	��B	��B	��B	�B	��B	�B	�6B	�"B	�"B	�(B	�4B	�[B	�aB	�gB	�_B	�_B	�_B	�eB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�+B	�<B	�aB	�gB	�mB	ƎB	ǔB	�zB	ȀB	ɠB	ʌB	˒B	ˬB	ʌB	ˬB	ˬB	̘B	��B	��B	��B	бB	ѷB	ҽB	ҽB	ҽB	��B	ѷB	ѷB	��B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�:B	�:B	�&B	�@B	�,B	�,B	�2B	�RB	�XB	�>B	�XB	�_B	�KB	�KB	�KB	�WB	�WB	�qB	�qB	�WB	�]B	�cB	�cB	�}B	�cB	�cB	�iB	�iB	�oB	�oB	�vB	�vB	�vB	�B	�vB	�vB	�vB	�|B	�B	�|B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
	B

#B

#B

	B
B
B
B
B
B
B
B
6B
B
B
"B
"B
"B
(B
BB
BB
(B
BB
(B
.B
.B
NB
NB
4B
:B
[B
@B
@B
[B
aB
aB
MB
MB
gB
mB
SB
YB
YB
YB
YB
YB
_B
_B
_B
B
eB
eB
eB
kB
kB
qB
�B
xB
�B
xB
xB
xB
�B
xB
xB
xB
�B
~B
�B
�B
~B
~B
�B
�B
~B
~B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
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
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
.�B
/ B
/�B
0B
0B
/�B
/�B
0�B
0�B
1B
2B
2B
1�B
2�B
2�B
2�B
4B
4B
4B
5B
5B
72B
72B
8B
8B
8B
8B
9$B
9$B
:*B
;0B
;0B
;0B
<6B
<6B
<PB
<PB
<6B
<PB
<6B
<6B
=<B
=VB
>BB
>]B
>]B
>]B
>BB
?HB
?HB
?HB
?cB
?cB
@iB
@iB
@iB
@OB
AoB
AUB
AoB
AoB
BuB
BuB
B[B
BuB
CaB
DgB
DgB
DgB
EmB
EmB
EmB
EmB
F�B
GzB
G�B
GzB
GzB
GzB
GzB
H�B
H�B
H�B
H�B
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
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
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
P�B
P�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
Q�B
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
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
Z�B
Z�B
[	B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
\B
[�B
[	B
[�B
\B
[�B
[�B
\�B
]B
]B
]B
\�B
]B
]B
^B
_!B
_B
^B
_!B
_B
_B
_B
_!B
_!B
`B
`B
`B
`'B
`B
a-B
aB
aB
aB
aB
b4B
b4B
bB
bB
bB
b4B
b4B
bB
bB
c:B
c:B
c:B
c B
d&B
d&B
d&B
d&B
d@B
d@B
eFB
e,B
eFB
eFB
eFB
fLB
f2B
fLB
f2B
f2B
gRB
g8B
g8B
g8B
g8B
g8B
gRB
gRB
g8B
gRB
hXB
iDB
iDB
iDB
iDB
iDB
jKB
jKB
jKB
jKB
jKB
kQB
kkB
kQB
kQB
kQB
kQB
kQB
kQB
kQB
lqB
lWB
lWB
mw11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.34(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808290041182018082900411820180829004118201808300037172018083000371720180830003717JA  ARFMdecpA19c                                                                20180824183548  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180824093549  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180824093550  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180824093551  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180824093552  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180824093552  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180824093552  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180824093552  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180824093552  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180824093552                      G�O�G�O�G�O�                JA  ARUP                                                                        20180824095541                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180824154121  CV  JULD            G�O�G�O�F��k                JM  ARGQJMQC2.0                                                                 20180824154121  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20180824154121  CV  LATITUDE        G�O�G�O�A�&�                JM  ARGQJMQC2.0                                                                 20180824154121  CV  LONGITUDE       G�O�G�O���                JM  ARCAJMQC2.0                                                                 20180828154118  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180828154118  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180829153717  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                