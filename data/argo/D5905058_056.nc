CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-27T00:35:24Z creation;2018-04-27T00:35:28Z conversion to V3.1;2019-12-23T06:22:54Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20180427003524  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA  I2_0675_056                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�^5'�} 1   @�^5�}( @7D�j~���b��J�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�3D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
Clp�Cnp�CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]�)D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�
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
�D�J�D���D���D�D�J�D���D���D�
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
�D�J�Dˊ�D���D�
�D�J�D̊�D���D�
�D�J�D͊�D���D�
�D�J�DΊ�D���D�
�D�J�Dϊ�D���D�
�D�J�DЊ�D���D�
�D�J�Dъ�D���D�
�D�J�DҊ�D���D�
�D�J�DӇ�D���D�
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
�D�J�D臮D���D�
�D�J�D��D�ǮD�
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
�D�J�D���D��D�D�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��+A��+A��7A��7A��A�z�A�E�A�I�A�G�A�XA�n�A�A��\A��DA�p�A�dZA�C�A�$�A�oA��A��A���A��A���A���A�?}A��;A���A�K�A��A�A��DA�jA�p�A�1'A���A�jA��A��A���A��;A�^5A��mA���A�z�A�VA���A�G�A��A���A�`BA���A��A�ƨA�;dA��
A��\A��A�{A�1'A��A�n�A��HA���A���A�\)A��RA�O�A���A�(�A��#A��
A�{A���A�ZA�x�A�Q�A��uA��yA��A�JA�|�A�x�A���A��A�$�A���A��mA�%A��A�jA��A��A���A��hA��\A���A���A�1A�(�A���A���A��;A��A|n�Az1AwS�Au;dAs��Aq��Aox�Aj1AfVAex�AdĜAb�A^�AZ�AXbAV�AV=qAUl�AR��AP�\AOx�AO+ANbNAM��AM+AK��AI��AG�7AF�9AEAD�jAC��ACVABn�A@ �A>�A=x�A;ƨA9��A7�^A6�\A5�hA3�A3G�A2��A25?A1?}A0��A0bA/C�A-�TA,�/A+�A+O�A*�+A)�A)C�A)�A)%A(ȴA(1'A'XA%&�A#dZA"�A!p�A �\AAhsA|�An�A�wA��A��A�A�^A�AJAoA�A7LA�A33AQ�A�AoA��AbNAC�A1A
{A5?AhsA�!A{A+AI�A%AM�A�AXA"�A ��A A�@��T@���@���@�z�@�ƨ@�~�@��@��m@�ȴ@�&�@@�+@�`B@�l�@��@�1@�33@�=q@���@���@�F@��@��@��@��@�Ĝ@��;@ڧ�@���@��
@֏\@ԋD@Ӿw@җ�@��#@�p�@��@�Ĝ@�l�@͑h@���@�1'@�+@�V@�@��@���@�l�@�"�@Ƨ�@���@���@Ý�@�$�@�bN@���@�E�@�$�@�7L@���@��H@���@�^5@�=q@��@��@�&�@��9@��;@���@�v�@�ff@�v�@��@���@���@�z�@�|�@���@���@�bN@��P@�v�@��#@���@�1@��@�v�@�E�@�5?@�J@�O�@��D@���@���@��F@��P@�t�@�
=@�M�@��h@��7@�O�@��h@��-@�7L@�Ĝ@���@���@�&�@��9@� �@�ƨ@�dZ@��@���@�{@��@�@���@�bN@��m@��;@��;@��w@��F@��P@�;d@�
=@��y@���@�M�@�5?@�{@�@�p�@��@���@��@��9@�bN@��w@�l�@�t�@�\)@�S�@�
=@���@��!@�~�@��@���@���@���@�X@���@��9@���@�z�@�I�@�1'@��@��m@�ƨ@���@�33@��y@��H@��@���@��!@���@��\@�v�@�^5@�@���@���@���@��@�&�@���@��@�9X@�  @��@��P@��P@��P@�l�@�;d@�"�@�
=@��@��@���@�n�@�=q@�{@���@���@�O�@���@��j@���@��D@�Q�@��@���@�t�@�S�@�"�@���@��\@�V@�$�@�J@��T@�@���@��h@�`B@��@��/@��@�z�@�r�@�j@�Q�@�9X@�  @���@���@��@�|�@�dZ@�@��H@�ȴ@���@�V@�-@��@�J@��T@���@�O�@�&�@��@���@��j@��D@�Z@� �@� �@�b@��
@���@�l�@�\)@�"�@���@���@�~�@�M�@�@���@��7@�hs@��@���@��@���@��@�Z@��
@�\)@�+@��@���@�=q@��@���@��-@�x�@���@���@��9@���@�bN@�b@�;@�@\)@~�y@~��@~ff@~V@~5?@~@}��@}��@}p�@|��@|(�@{�m@{t�@{o@z�@z��@z��@zn�@z�@y��@y�7@y�@x��@xb@w+@v��@vff@u�@u�-@up�@u?}@u�@t�@t�@t9X@s�F@st�@so@r�@r�H@r�!@r�\@r~�@r�@q�@q�^@q&�@p�9@p1'@p  @o�;@oK�@n�@nv�@nV@n5?@n$�@n$�@l�j@l9X@l(�@k�F@j�H@j�!@j^5@ix�@iG�@iX@i7L@i�@i%@h�u@g�;@g��@g�w@g�P@f5?@e�-@eV@d�@d�@d9X@c��@d(�@c�
@cdZ@co@b��@bJ@a��@`Ĝ@_K�@^��@^��@^$�@]�@\��@\�j@\��@[�m@[C�@Z�@Z�\@Z~�@Zn�@Zn�@Zn�@ZM�@Y��@YX@X�9@XA�@W�;@Wl�@Vȴ@Vff@VE�@U@U?}@U?}@T��@T�@UV@T��@T�@St�@S"�@SdZ@SS�@Q��@Qx�@Q7L@P��@P�`@P�u@Pb@O��@N��@Nȴ@N�R@Nȴ@O
=@N��@N��@N�R@Nȴ@Nȴ@N�R@N5?@N$�@M@M�@M`B@M/@L��@Lz�@L9X@K�m@K��@Ko@J��@J�\@J-@I��@I��@Ihs@IG�@I&�@HĜ@Hr�@HA�@H1'@H �@Hb@G��@G�P@G�@G
=@F�@F��@F�+@FE�@E�@E�@E�@D��@C��@C�F@CC�@B�!@B^5@B-@BJ@A�#@A�^@A�7@AX@A7L@A�@A�@@�`@@Ĝ@@��@@1'@?K�@?;d@>��@>��@>V@>5?@>$�@=@=�@=?}@<�/@<��@<Z@<(�@;�m@;ƨ@;ƨ@;��@;t�@;33@;"�@;o@:�@:��@:��@:�!@:��@:^5@:J@9�@9��@9��@9x�@9%@8�9@8r�@8A�@8A�@8b@7�@7�P@7�P@7|�@7;d@7�@7
=@6�@6�R@6v�@6ff@6E�@5�-@4��@4��@4��@4�@4z�@4�@3�
@3ƨ@3�F@3t�@3o@2��@2M�@2J@1�#@1�^@1��@1��@1��@1�7@1&�@0�@0bN@0A�@0  @/�;@/��@/�w@/��@/+@.��@.�@.��@.V@.5?@-@-��@-�@-p�@-p�@-O�@-�@,�@,�@,�D@,j@,Z@,(�@+�
@+�F@+��@+��@+t�@+t�@+S�@*�@*��@*�\@*~�@*^5@*-@*J@)�7@)G�@)7L@)&�@(��@(Ĝ@(�@(Q�@( �@(b@'�@'�;@'�w@'|�@'K�@'+@&��@&�R@&v�@&ff@&E�@%�T@%��@%�h@%�h@%`B@$�@$��@$j@$I�@$9X@$�@#��@#�F@#��@#�@#t�@#C�@"�H@"�\@"^5@"M�@"=q@"J@!�@!�^@ �9@ bN@ Q�@  �@�@K�@+@��@�y@ȴ@��@v�@E�@E�@5?@5?@$�@�T@�T@��@�h@/@V@��@��@��@�D@z�@j@I�@1@�
@��@�@C�@o@�@��@�\@-@�@�#@�#@��@�^@�7@�@�`@�9@�u@bN@1'@1'@ �@�@��@��@\)@\)@+@��@�@ȴ@�R@V@@�T@@p�@��@z�@I�@9X@��@�F@��@�@dZ@"�@o@@�@��@n�@M�@-@��@�@��@��@��@�7@hs@7L@%@��@��@r�@Q�@1'@  @��@�@�P@|�@l�@\)@K�@;d@��@��@v�@V@@��@@@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��+A��+A��7A��7A��A�z�A�E�A�I�A�G�A�XA�n�A�A��\A��DA�p�A�dZA�C�A�$�A�oA��A��A���A��A���A���A�?}A��;A���A�K�A��A�A��DA�jA�p�A�1'A���A�jA��A��A���A��;A�^5A��mA���A�z�A�VA���A�G�A��A���A�`BA���A��A�ƨA�;dA��
A��\A��A�{A�1'A��A�n�A��HA���A���A�\)A��RA�O�A���A�(�A��#A��
A�{A���A�ZA�x�A�Q�A��uA��yA��A�JA�|�A�x�A���A��A�$�A���A��mA�%A��A�jA��A��A���A��hA��\A���A���A�1A�(�A���A���A��;A��A|n�Az1AwS�Au;dAs��Aq��Aox�Aj1AfVAex�AdĜAb�A^�AZ�AXbAV�AV=qAUl�AR��AP�\AOx�AO+ANbNAM��AM+AK��AI��AG�7AF�9AEAD�jAC��ACVABn�A@ �A>�A=x�A;ƨA9��A7�^A6�\A5�hA3�A3G�A2��A25?A1?}A0��A0bA/C�A-�TA,�/A+�A+O�A*�+A)�A)C�A)�A)%A(ȴA(1'A'XA%&�A#dZA"�A!p�A �\AAhsA|�An�A�wA��A��A�A�^A�AJAoA�A7LA�A33AQ�A�AoA��AbNAC�A1A
{A5?AhsA�!A{A+AI�A%AM�A�AXA"�A ��A A�@��T@���@���@�z�@�ƨ@�~�@��@��m@�ȴ@�&�@@�+@�`B@�l�@��@�1@�33@�=q@���@���@�F@��@��@��@��@�Ĝ@��;@ڧ�@���@��
@֏\@ԋD@Ӿw@җ�@��#@�p�@��@�Ĝ@�l�@͑h@���@�1'@�+@�V@�@��@���@�l�@�"�@Ƨ�@���@���@Ý�@�$�@�bN@���@�E�@�$�@�7L@���@��H@���@�^5@�=q@��@��@�&�@��9@��;@���@�v�@�ff@�v�@��@���@���@�z�@�|�@���@���@�bN@��P@�v�@��#@���@�1@��@�v�@�E�@�5?@�J@�O�@��D@���@���@��F@��P@�t�@�
=@�M�@��h@��7@�O�@��h@��-@�7L@�Ĝ@���@���@�&�@��9@� �@�ƨ@�dZ@��@���@�{@��@�@���@�bN@��m@��;@��;@��w@��F@��P@�;d@�
=@��y@���@�M�@�5?@�{@�@�p�@��@���@��@��9@�bN@��w@�l�@�t�@�\)@�S�@�
=@���@��!@�~�@��@���@���@���@�X@���@��9@���@�z�@�I�@�1'@��@��m@�ƨ@���@�33@��y@��H@��@���@��!@���@��\@�v�@�^5@�@���@���@���@��@�&�@���@��@�9X@�  @��@��P@��P@��P@�l�@�;d@�"�@�
=@��@��@���@�n�@�=q@�{@���@���@�O�@���@��j@���@��D@�Q�@��@���@�t�@�S�@�"�@���@��\@�V@�$�@�J@��T@�@���@��h@�`B@��@��/@��@�z�@�r�@�j@�Q�@�9X@�  @���@���@��@�|�@�dZ@�@��H@�ȴ@���@�V@�-@��@�J@��T@���@�O�@�&�@��@���@��j@��D@�Z@� �@� �@�b@��
@���@�l�@�\)@�"�@���@���@�~�@�M�@�@���@��7@�hs@��@���@��@���@��@�Z@��
@�\)@�+@��@���@�=q@��@���@��-@�x�@���@���@��9@���@�bN@�b@�;@�@\)@~�y@~��@~ff@~V@~5?@~@}��@}��@}p�@|��@|(�@{�m@{t�@{o@z�@z��@z��@zn�@z�@y��@y�7@y�@x��@xb@w+@v��@vff@u�@u�-@up�@u?}@u�@t�@t�@t9X@s�F@st�@so@r�@r�H@r�!@r�\@r~�@r�@q�@q�^@q&�@p�9@p1'@p  @o�;@oK�@n�@nv�@nV@n5?@n$�@n$�@l�j@l9X@l(�@k�F@j�H@j�!@j^5@ix�@iG�@iX@i7L@i�@i%@h�u@g�;@g��@g�w@g�P@f5?@e�-@eV@d�@d�@d9X@c��@d(�@c�
@cdZ@co@b��@bJ@a��@`Ĝ@_K�@^��@^��@^$�@]�@\��@\�j@\��@[�m@[C�@Z�@Z�\@Z~�@Zn�@Zn�@Zn�@ZM�@Y��@YX@X�9@XA�@W�;@Wl�@Vȴ@Vff@VE�@U@U?}@U?}@T��@T�@UV@T��@T�@St�@S"�@SdZ@SS�@Q��@Qx�@Q7L@P��@P�`@P�u@Pb@O��@N��@Nȴ@N�R@Nȴ@O
=@N��@N��@N�R@Nȴ@Nȴ@N�R@N5?@N$�@M@M�@M`B@M/@L��@Lz�@L9X@K�m@K��@Ko@J��@J�\@J-@I��@I��@Ihs@IG�@I&�@HĜ@Hr�@HA�@H1'@H �@Hb@G��@G�P@G�@G
=@F�@F��@F�+@FE�@E�@E�@E�@D��@C��@C�F@CC�@B�!@B^5@B-@BJ@A�#@A�^@A�7@AX@A7L@A�@A�@@�`@@Ĝ@@��@@1'@?K�@?;d@>��@>��@>V@>5?@>$�@=@=�@=?}@<�/@<��@<Z@<(�@;�m@;ƨ@;ƨ@;��@;t�@;33@;"�@;o@:�@:��@:��@:�!@:��@:^5@:J@9�@9��@9��@9x�@9%@8�9@8r�@8A�@8A�@8b@7�@7�P@7�P@7|�@7;d@7�@7
=@6�@6�R@6v�@6ff@6E�@5�-@4��@4��@4��@4�@4z�@4�@3�
@3ƨ@3�F@3t�@3o@2��@2M�@2J@1�#@1�^@1��@1��@1��@1�7@1&�@0�@0bN@0A�@0  @/�;@/��@/�w@/��@/+@.��@.�@.��@.V@.5?@-@-��@-�@-p�@-p�@-O�@-�@,�@,�@,�D@,j@,Z@,(�@+�
@+�F@+��@+��@+t�@+t�@+S�@*�@*��@*�\@*~�@*^5@*-@*J@)�7@)G�@)7L@)&�@(��@(Ĝ@(�@(Q�@( �@(b@'�@'�;@'�w@'|�@'K�@'+@&��@&�R@&v�@&ff@&E�@%�T@%��@%�h@%�h@%`B@$�@$��@$j@$I�@$9X@$�@#��@#�F@#��@#�@#t�@#C�@"�H@"�\@"^5@"M�@"=q@"J@!�@!�^@ �9@ bN@ Q�@  �@�@K�@+@��@�y@ȴ@��@v�@E�@E�@5?@5?@$�@�T@�T@��@�h@/@V@��@��@��@�D@z�@j@I�@1@�
@��@�@C�@o@�@��@�\@-@�@�#@�#@��@�^@�7@�@�`@�9@�u@bN@1'@1'@ �@�@��@��@\)@\)@+@��@�@ȴ@�R@V@@�T@@p�@��@z�@I�@9X@��@�F@��@�@dZ@"�@o@@�@��@n�@M�@-@��@�@��@��@��@�7@hs@7L@%@��@��@r�@Q�@1'@  @��@�@�P@|�@l�@\)@K�@;d@��@��@v�@V@@��@@@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�XB
�XB
�XB
�XB
�XB
�jB
��B�B^5B�PBB��B��B��B  BBB%B%BB%BJB�B�B33B=qBA�BD�BI�BM�BN�BQ�B^5Be`BhsBl�Bn�Bq�B{�B�=B�PB�DB�1B�=B�DB�\B�bB�hB�{B�{B��B�{B�{B�PB�+B�B~�Bw�B~�Bt�B�B�7B� Br�BaHBXBP�BO�BS�BL�BG�B?}B5?B0!B,B$�BhBB��B�NB��BȴB�jB��B� Bm�BVB?}B33BbB
��B
�mB
�;B
��B
�LB
�uB
y�B
cTB
I�B
7LB
0!B
)�B
�B
DB	�mB	��B	�}B	�B	��B	�7B	{�B	Q�B	1'B	'�B	 �B	uB��B�B��B��B��B��B�}B�3B�B�B�!B�!B�B��B��B�PB�JB�B� By�Bu�Bp�B_;BS�BL�BE�BB�BD�BH�BD�BD�BE�BH�BN�BO�BT�BVBT�BS�BQ�BO�BN�BO�BQ�BR�BR�BR�BQ�BP�BO�BN�BN�BN�BK�BI�BI�BL�BL�BH�BL�BQ�BR�BO�BP�BN�BJ�BH�BH�BE�BD�BA�B?}B?}BC�BH�BG�BD�B?}B9XB1'B-B-B-B+B+B+B0!B2-B1'B0!B0!B.B,B+B+B+B+B+B(�B'�B'�B'�B'�B&�B%�B&�B$�B%�B#�B%�B&�B'�B)�B)�B(�B(�B(�B(�B(�B)�B)�B)�B,B,B-B.B.B/B/B.B2-B33B49B5?B7LB9XB9XB<jB@�B?}BC�BF�BJ�BO�BN�BM�BL�BL�BO�BQ�BS�BR�BT�BT�B[#B`BBe`BjBm�Bq�Bv�Bw�Bw�Bx�B{�B�B�B�B�B� B�B�1B�DB�=B�1B�DB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�LB�dB�dB��BŢBĜB��B��B��B�B�5B�;B�NB�ZB�ZB�fB�`B�B�B�B�B��B��B	B	B	%B	PB	oB	�B	�B	�B	�B	�B	�B	"�B	%�B	(�B	)�B	+B	-B	/B	33B	:^B	=qB	>wB	?}B	@�B	C�B	G�B	H�B	L�B	N�B	P�B	Q�B	S�B	YB	ZB	[#B	\)B	^5B	_;B	`BB	bNB	cTB	dZB	gmB	hsB	iyB	jB	k�B	l�B	l�B	m�B	n�B	p�B	t�B	w�B	x�B	z�B	z�B	}�B	�B	�B	�%B	�=B	�7B	�JB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�?B	�FB	�LB	�RB	�RB	�^B	�jB	�qB	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�NB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
JB
JB
PB
PB
PB
PB
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
#�B
#�B
#�B
%�B
&�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
,B
,B
-B
-B
-B
-B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
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
7LB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
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
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
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
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
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
VB
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
XB
XB
XB
XB
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
ZB
ZB
[#B
[#B
[#B
[#B
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
^5B
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
`BB
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
aHB
aHB
aHB
aHB
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
gmB
gmB
ffB
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
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�>B
�>B
�>B
�>B
�>B
�PB
οByB^B�6B�uB��B��B��B��B�BBBBBB0BgB�B3B=VBAoBD�BI�BM�BN�BQ�B^BeFBhXBlqBn}Bq�B{�B�#B�6B�)B�B�#B�)B�BB�HB�NB�aB�aB�mB�aB�aB�6B�B��B~�Bw�B~�Bt�B��B�B�Br�Ba-BW�BP�BO�BS�BL�BG�B?cB5%B0B+�B$�BNBB��B�4BΥBȚB�PB�~B�BmwBU�B?cB3B.B
��B
�RB
�!B
��B
�B
�[B
y�B
c B
I�B
72B
/�B
)�B
�B
)B	�RB	��B	�cB	��B	��B	�B	{�B	Q�B	1B	'�B	 �B	[B��B��B̳BˬBбBΥB�HB�B��B��B�B�B��B��B�eB�B�0B��B�By�Bu�BpoB_!BS�BL�BE�BBuBD�BH�BD�BDgBE�BH�BN�BO�BT�BU�BT�BS�BQ�BO�BN�BO�BQ�BR�BR�BR�BQ�BP�BO�BN�BN�BN�BK�BI�BI�BL�BL�BH�BL�BQ�BR�BO�BP�BN�BJ�BH�BH�BEmBDgBAoB?cB?cBCaBH�BG�BD�B?HB9>B0�B,�B,�B,�B*�B*�B*�B/�B1�B0�B/�B/�B-�B+�B*�B*�B*�B*�B*�B(�B'�B'�B'�B'�B&�B%�B&�B$�B%�B#�B%�B&�B'�B)�B)�B(�B(�B(�B(�B(�B)�B)�B)�B+�B+�B,�B-�B-�B.�B.�B-�B1�B2�B4B5B72B9$B9$B<6B@iB?HBCaBF�BJ�BO�BN�BM�BL�BL�BO�BQ�BS�BR�BT�BT�BZ�B`'Be,BjeBm]BqvBv�Bw�Bw�Bx�B{�B��B��B��B��B�B��B��B�B�#B��B�B�:B�@B�FB�MB�mB�YB��B��B��B��B��B��B��B��B��B��B�B�JB�0B�oBňB�gB˒B̳BѷB��B�B�B�4B�&B�&B�LB�FB�eB�iB�iB�B��B��B	�B	B	�B	B	TB	MB	SB	kB	~B	�B	�B	"�B	%�B	(�B	)�B	*�B	,�B	.�B	3B	:*B	=VB	>BB	?HB	@OB	CaB	G�B	H�B	L�B	N�B	P�B	Q�B	S�B	X�B	Y�B	[	B	[�B	^B	_B	`B	bB	c B	d@B	g8B	h>B	i_B	jKB	kQB	lWB	lWB	m]B	ncB	p�B	t�B	w�B	x�B	z�B	z�B	}�B	��B	��B	��B	�	B	�B	�B	�BB	�.B	�:B	�@B	�aB	�MB	�SB	�SB	�yB	�B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	��B	�B	�B	�B	�B	�8B	�8B	�*B	�6B	�<B	�cB	�iB	�OB	�oB	�oB	�[B	�aB	ňB	�tB	�tB	ƎB	�zB	ɠB	ʌB	ʌB	̳B	͟B	οB	οB	ϫB	ϫB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�&B	�,B	�,B	�2B	�8B	�8B	�8B	�DB	�eB	�eB	�KB	�eB	�eB	�kB	�]B	�}B	�iB	�iB	��B	�vB	�|B	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
B
�B
�B
	B
	B
	B

#B

	B

	B
	B
	B
B
B
B
B
0B
B
B
6B
B
B
B
6B
6B
B
(B
(B
HB
HB
.B
.B
.B
NB
TB
:B
:B
TB
TB
@B
FB
FB
MB
mB
B
eB
eB
�B
kB
�B
eB
eB
_B
yB
_B
sB
YB
YB
YB
YB
B
eB
eB
kB
kB
�B
�B
qB
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
#�B
$�B
#�B
#�B
#�B
%�B
&�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
/ B
/�B
0�B
1B
1B
1B
0�B
1B
0�B
0�B
0�B
1�B
2B
2�B
3B
3B
4B
4B
4B
4B
5B
5B
5B
5%B
5B
5%B
6B
6+B
72B
7B
7B
7B
7B
8B
9>B
9>B
:DB
:*B
:*B
:*B
;JB
;0B
;JB
;JB
<PB
<PB
<PB
<PB
<PB
=VB
=VB
=<B
=<B
=VB
=<B
>]B
?cB
?HB
?HB
@OB
@iB
AoB
AUB
AUB
AUB
BuB
BuB
BuB
CaB
CaB
C{B
CaB
C{B
C{B
C{B
DgB
D�B
DgB
DgB
D�B
DgB
DgB
DgB
D�B
EmB
EmB
EmB
EmB
E�B
FtB
F�B
F�B
F�B
FtB
GzB
GzB
GzB
GzB
G�B
GzB
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
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
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
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
U�B
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
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
[	B
[	B
Z�B
[	B
\B
[�B
[�B
\�B
]B
\�B
\�B
\�B
^B
^B
^B
^B
_B
_B
_!B
_!B
_!B
_B
_B
`B
`B
`B
`'B
`B
`'B
`B
`B
`B
aB
aB
a-B
aB
aB
aB
aB
aB
a-B
bB
bB
b4B
b4B
c B
c:B
c B
c B
c B
d&B
d&B
d&B
d&B
d&B
e,B
e,B
e,B
e,B
e,B
fLB
fLB
f2B
f2B
f2B
fLB
fLB
f2B
g8B
g8B
f2B
gRB
g8B
g8B
g8B
g8B
h>B
iDB
i_B
iDB
iDB
i_B
iDB
i_B
iDB
jeB
jKB
iDB
jeB
jeB
jKB
kQB
kkB
kkB
kQB
lqB
lWB
lqB
lWB
m]B
m]B
mwB
m]B
n}B
n}B
ncB
ncB
ncB
ncB
ncB
ncB
o�B
oiB
oiB
o�B
oiB
oiB
o�B
o�B
poB
poB
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.34(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805020038472018050200384720180502003847201806042357382018060423573820180604235738JA  ARFMdecpA19c                                                                20180427093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180427003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180427003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180427003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180427003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180427003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180427003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180427003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180427003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180427003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180427005747                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180427153550  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180501153847  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180501153847  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604145738  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                