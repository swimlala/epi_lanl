CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-24T00:46:42Z creation;2018-07-24T00:46:47Z conversion to V3.1;2019-12-23T06:18:30Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180724004642  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               KA   JA  I2_0675_075                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�rr_1� 1   @�rs+��@9[��Q��c,�+J1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ DŃ3D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
A�A%�AE�Ae�A���A���A�(�A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&EC(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��DHD�HD�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��DlHDl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D�D���D��D�K�DË�D���D��D�K�Dċ�D���D��D�K�Dŏ
D���D��D�O
DƋ�D���D��D�K�Dǋ�D���D��D�K�Dȋ�D���D��D�K�Dɋ�D���D��D�K�Dʋ�D���D��D�K�Dˋ�D���D��D�K�D̋�D���D��D�K�D͋�D���D��D�K�D΋�D���D��D�K�Dϋ�D���D��D�K�DЋ�D���D��D�K�Dы�D���D��D�K�Dҋ�D���D��D�K�DӋ�D���D��D�K�Dԋ�D���D��D�K�DՋ�D���D��D�K�D֋�D���D��D�K�D׋�D���D��D�K�D؋�D���D��D�K�Dً�D���D��D�K�Dڋ�D���D��D�K�Dۋ�D���D��D�K�D܋�D���D��D�K�D݋�D���D��D�K�Dދ�D���D��D�K�Dߋ�D���D��D�K�D���D���D�
D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D���D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D���D���D��D�K�D��
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʾwAʾwA���A�A�A���A�A���A�A���AʾwAʾwAʾwAʼjAʸRAʛ�A�ffA��A�=qA��#Aė�A�
=A�l�A���A��A�~�A��A�v�A�bNA�~�A���A��HA�VA�v�A�{A��A��7A�O�A���A�jA��A��A�7LA��DA�bA�l�A��9A�bNA�VA��A�K�A�7LA���A��A��#A��hA�7LA��#A��A�A��yA�A��mA�ƨA���A�A��!A�bNA�G�A��HA�1'A�A���A���A�?}A��`A�K�A�(�A���A�C�A��#A�ĜA�O�A��7A��7A��`A�\)A�ffA�S�A��A�ȴA�^5A��A���A�t�A���A��A�/A��uA�5?A�  A}�A{��A{S�Ay`BAw;dAv1Au\)At�DAsO�Ar��AqXApA�An�yAl��Ajz�Ai��Ai%Ag�hAe�mAdAbv�Aa�wAa�A_/A]�A\�/AZ�yAY�AX�uAW33AVI�AU�ARbNAP�AO�AM�TAM�AK�-AJ{AI33AF��AE\)ACƨABI�A@-A?O�A>(�A<�A:z�A9�TA9XA9VA8��A8ȴA7��A6��A6  A5��A5�hA4��A4  A3�^A3�wA3�-A2�9A2Q�A1�A1;dA0�A0�+A/�TA.��A.$�A-�FA,��A+VA)��A(�`A(bA'�A'&�A&��A%A$=qA#`BA#A"�A!�^A!`BA �\A�FA&�A�A�hAr�Al�A��A�^A��Az�A�AA�AS�A"�AffAƨA�PA
=AQ�AhsA��A\)A�A�A�RA�A
n�A	�#A	G�A�DA|�A��A�mA`BA%A=qA�hAx�A ��A {@�^5@�1@�ȴ@�x�@�\)@��@���@��;@��@�5?@�bN@��H@홚@���@�
=@�V@�@��T@�O�@�G�@�&�@�O�@�9X@���@�-@��#@�h@���@��y@�E�@�?}@���@�A�@ڗ�@�@�hs@ش9@�1'@�o@�V@�7L@ԋD@Ӆ@�J@�b@�9X@��;@·+@ͺ^@ͺ^@��@͉7@�&�@�|�@��@�v�@��@�G�@ǥ�@�V@�r�@°!@�?}@�j@�ȴ@��T@���@��@���@��y@�o@�ff@�p�@��`@�A�@���@�K�@�-@�Q�@���@�K�@�@�~�@�5?@�O�@���@�Z@�\)@�~�@�@�hs@���@�(�@�1@��w@���@��@�M�@��7@���@�bN@�Z@�Z@� �@���@�
=@�o@�@��!@�^5@�@�x�@��`@��@�A�@�Q�@��@�K�@�"�@��y@�^5@�$�@�$�@�J@���@���@��7@�/@��@��j@�A�@�t�@��@��R@�=q@�E�@�5?@��@��#@���@��@�O�@��@�bN@�Q�@�A�@�1@���@��
@��@���@��P@��@�|�@�+@�t�@�K�@�@�ff@���@�7L@���@���@�Z@���@���@��w@���@��@��P@�|�@�o@��H@��@��y@��!@�ff@��@��T@�/@�Ĝ@�z�@�  @��w@���@���@���@��P@�\)@��@��H@�ȴ@���@�ff@�V@�5?@�J@��T@�@��7@�p�@�O�@�%@��@�j@�A�@��@�ƨ@��@�|�@�t�@�K�@��@�ȴ@���@�~�@�E�@���@��@���@��^@��-@��-@��-@���@��@�G�@��@�%@���@���@��9@���@��@�I�@�(�@�b@���@�  @���@�|�@�33@��@��@��@�@��y@��H@��@�ȴ@���@�5?@���@�p�@��@���@���@��D@�(�@�ƨ@���@�K�@���@��F@���@�|�@�;d@��R@�^5@���@�p�@��@���@��@�j@�A�@��@�ƨ@���@�dZ@��y@���@�v�@�V@�-@��^@���@��h@��^@�@�`B@�?}@�/@�%@�V@�V@���@���@�I�@�1@�1@�9X@�1@�@+@~��@~E�@~V@~5?@}�T@}/@|z�@{�@{�F@{o@z��@z��@z��@z�H@z�H@z~�@z�@y�@y��@y��@y�^@y�@y��@yhs@y&�@x��@x��@x�@xbN@w�;@w�w@w\)@w�@v�@u�@u�-@u/@t��@t��@t(�@sƨ@sdZ@s"�@r��@r^5@r�@q�^@q�7@q&�@p��@p�9@pA�@ol�@o
=@n��@n{@m�@l1@l1@l�@l(�@l9X@l(�@l�@k�F@k�F@kC�@i��@iG�@hĜ@h  @g�w@g�@hA�@hbN@hQ�@hbN@g�@f��@f�@f��@f��@f��@fE�@e�@e��@d�/@d�@d�j@d�j@dZ@ct�@b�H@b�\@b=q@a%@_�@_
=@^ȴ@^ff@]��@]��@]�-@]��@]�@]�@\1@[��@[t�@[S�@[C�@["�@Z��@Y�@Y��@Y��@Yx�@Y&�@X�`@XĜ@X�9@X�u@X�@XbN@W�P@W�@V�@V�R@Vff@U@U�-@UO�@T�j@Tj@TI�@S�F@St�@SS�@S33@R��@R^5@RM�@RM�@R-@Qx�@P��@P�u@O�;@O;d@O+@O
=@Nv�@N@M��@M?}@L�j@LI�@L1@Kƨ@K��@Kt�@J��@J^5@IX@HĜ@Hr�@H  @G|�@G
=@F�y@F��@E�@E�T@E?}@D��@Dj@C�m@Ct�@B�\@B=q@BJ@A��@A�@A��@Ahs@@��@@��@@��@@�@@Q�@?|�@?+@>��@>v�@>$�@=�T@=��@=��@=/@<��@<�@<��@<�@<I�@;�
@;��@;��@;t�@;o@:�@:��@:n�@:M�@:�@:J@9��@9��@9hs@9X@9G�@97L@8�`@8�@8bN@81'@8 �@8 �@8b@7�@7l�@7
=@6�@6�+@6E�@6$�@5��@5p�@5?}@5�@4�@3�m@3��@3S�@333@333@333@333@333@3o@2��@2�!@2~�@2M�@1��@1��@1x�@1x�@1&�@0��@0�`@0Ĝ@0Q�@/�@/�w@/��@/�P@/\)@/
=@.�@.E�@-�-@-�@-/@,��@,�@,�@,�D@,Z@,�@+�
@+�F@+��@+t�@+@*�!@*�\@*~�@*-@)��@)��@)�7@)7L@(�u@(1'@'�;@'�@'|�@'l�@'\)@'\)@';d@&�@&V@&{@&@%�T@%��@%@%�-@%��@%`B@$��@$��@$�j@$Z@$(�@$1@#��@#�F@#S�@#@"�@"��@"n�@"=q@!�@!��@!��@!�7@!&�@ �9@ bN@  �@�;@|�@l�@K�@+@��@�R@v�@V@{@�T@��@��@��@`B@?}@V@��@�@��@�@Z@1@�m@ƨ@��@dZ@C�@�@��@�\@~�@^5@-@�@��@�^@x�@hs@X@7L@�@�9@r�@bN@A�@  @�;@�w@��@�P@��@v�@E�@{@@@�@�T@�-@�h@`B@?}@�@�/@��@z�@I�@1@�m@��@t�@t�@S�@C�@"�@"�@@��@~�@~�@n�@n�@M�@-@��@��@��@hs@G�@7L@7L@7L@&�@%@��@�u@r�@bN@Q�@ �@b@b@�@�@��@l�@K�@
=@ȴ@�+@V@@��@@�h@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AʾwAʾwA���A�A�A���A�A���A�A���AʾwAʾwAʾwAʼjAʸRAʛ�A�ffA��A�=qA��#Aė�A�
=A�l�A���A��A�~�A��A�v�A�bNA�~�A���A��HA�VA�v�A�{A��A��7A�O�A���A�jA��A��A�7LA��DA�bA�l�A��9A�bNA�VA��A�K�A�7LA���A��A��#A��hA�7LA��#A��A�A��yA�A��mA�ƨA���A�A��!A�bNA�G�A��HA�1'A�A���A���A�?}A��`A�K�A�(�A���A�C�A��#A�ĜA�O�A��7A��7A��`A�\)A�ffA�S�A��A�ȴA�^5A��A���A�t�A���A��A�/A��uA�5?A�  A}�A{��A{S�Ay`BAw;dAv1Au\)At�DAsO�Ar��AqXApA�An�yAl��Ajz�Ai��Ai%Ag�hAe�mAdAbv�Aa�wAa�A_/A]�A\�/AZ�yAY�AX�uAW33AVI�AU�ARbNAP�AO�AM�TAM�AK�-AJ{AI33AF��AE\)ACƨABI�A@-A?O�A>(�A<�A:z�A9�TA9XA9VA8��A8ȴA7��A6��A6  A5��A5�hA4��A4  A3�^A3�wA3�-A2�9A2Q�A1�A1;dA0�A0�+A/�TA.��A.$�A-�FA,��A+VA)��A(�`A(bA'�A'&�A&��A%A$=qA#`BA#A"�A!�^A!`BA �\A�FA&�A�A�hAr�Al�A��A�^A��Az�A�AA�AS�A"�AffAƨA�PA
=AQ�AhsA��A\)A�A�A�RA�A
n�A	�#A	G�A�DA|�A��A�mA`BA%A=qA�hAx�A ��A {@�^5@�1@�ȴ@�x�@�\)@��@���@��;@��@�5?@�bN@��H@홚@���@�
=@�V@�@��T@�O�@�G�@�&�@�O�@�9X@���@�-@��#@�h@���@��y@�E�@�?}@���@�A�@ڗ�@�@�hs@ش9@�1'@�o@�V@�7L@ԋD@Ӆ@�J@�b@�9X@��;@·+@ͺ^@ͺ^@��@͉7@�&�@�|�@��@�v�@��@�G�@ǥ�@�V@�r�@°!@�?}@�j@�ȴ@��T@���@��@���@��y@�o@�ff@�p�@��`@�A�@���@�K�@�-@�Q�@���@�K�@�@�~�@�5?@�O�@���@�Z@�\)@�~�@�@�hs@���@�(�@�1@��w@���@��@�M�@��7@���@�bN@�Z@�Z@� �@���@�
=@�o@�@��!@�^5@�@�x�@��`@��@�A�@�Q�@��@�K�@�"�@��y@�^5@�$�@�$�@�J@���@���@��7@�/@��@��j@�A�@�t�@��@��R@�=q@�E�@�5?@��@��#@���@��@�O�@��@�bN@�Q�@�A�@�1@���@��
@��@���@��P@��@�|�@�+@�t�@�K�@�@�ff@���@�7L@���@���@�Z@���@���@��w@���@��@��P@�|�@�o@��H@��@��y@��!@�ff@��@��T@�/@�Ĝ@�z�@�  @��w@���@���@���@��P@�\)@��@��H@�ȴ@���@�ff@�V@�5?@�J@��T@�@��7@�p�@�O�@�%@��@�j@�A�@��@�ƨ@��@�|�@�t�@�K�@��@�ȴ@���@�~�@�E�@���@��@���@��^@��-@��-@��-@���@��@�G�@��@�%@���@���@��9@���@��@�I�@�(�@�b@���@�  @���@�|�@�33@��@��@��@�@��y@��H@��@�ȴ@���@�5?@���@�p�@��@���@���@��D@�(�@�ƨ@���@�K�@���@��F@���@�|�@�;d@��R@�^5@���@�p�@��@���@��@�j@�A�@��@�ƨ@���@�dZ@��y@���@�v�@�V@�-@��^@���@��h@��^@�@�`B@�?}@�/@�%@�V@�V@���@���@�I�@�1@�1@�9X@�1@�@+@~��@~E�@~V@~5?@}�T@}/@|z�@{�@{�F@{o@z��@z��@z��@z�H@z�H@z~�@z�@y�@y��@y��@y�^@y�@y��@yhs@y&�@x��@x��@x�@xbN@w�;@w�w@w\)@w�@v�@u�@u�-@u/@t��@t��@t(�@sƨ@sdZ@s"�@r��@r^5@r�@q�^@q�7@q&�@p��@p�9@pA�@ol�@o
=@n��@n{@m�@l1@l1@l�@l(�@l9X@l(�@l�@k�F@k�F@kC�@i��@iG�@hĜ@h  @g�w@g�@hA�@hbN@hQ�@hbN@g�@f��@f�@f��@f��@f��@fE�@e�@e��@d�/@d�@d�j@d�j@dZ@ct�@b�H@b�\@b=q@a%@_�@_
=@^ȴ@^ff@]��@]��@]�-@]��@]�@]�@\1@[��@[t�@[S�@[C�@["�@Z��@Y�@Y��@Y��@Yx�@Y&�@X�`@XĜ@X�9@X�u@X�@XbN@W�P@W�@V�@V�R@Vff@U@U�-@UO�@T�j@Tj@TI�@S�F@St�@SS�@S33@R��@R^5@RM�@RM�@R-@Qx�@P��@P�u@O�;@O;d@O+@O
=@Nv�@N@M��@M?}@L�j@LI�@L1@Kƨ@K��@Kt�@J��@J^5@IX@HĜ@Hr�@H  @G|�@G
=@F�y@F��@E�@E�T@E?}@D��@Dj@C�m@Ct�@B�\@B=q@BJ@A��@A�@A��@Ahs@@��@@��@@��@@�@@Q�@?|�@?+@>��@>v�@>$�@=�T@=��@=��@=/@<��@<�@<��@<�@<I�@;�
@;��@;��@;t�@;o@:�@:��@:n�@:M�@:�@:J@9��@9��@9hs@9X@9G�@97L@8�`@8�@8bN@81'@8 �@8 �@8b@7�@7l�@7
=@6�@6�+@6E�@6$�@5��@5p�@5?}@5�@4�@3�m@3��@3S�@333@333@333@333@333@3o@2��@2�!@2~�@2M�@1��@1��@1x�@1x�@1&�@0��@0�`@0Ĝ@0Q�@/�@/�w@/��@/�P@/\)@/
=@.�@.E�@-�-@-�@-/@,��@,�@,�@,�D@,Z@,�@+�
@+�F@+��@+t�@+@*�!@*�\@*~�@*-@)��@)��@)�7@)7L@(�u@(1'@'�;@'�@'|�@'l�@'\)@'\)@';d@&�@&V@&{@&@%�T@%��@%@%�-@%��@%`B@$��@$��@$�j@$Z@$(�@$1@#��@#�F@#S�@#@"�@"��@"n�@"=q@!�@!��@!��@!�7@!&�@ �9@ bN@  �@�;@|�@l�@K�@+@��@�R@v�@V@{@�T@��@��@��@`B@?}@V@��@�@��@�@Z@1@�m@ƨ@��@dZ@C�@�@��@�\@~�@^5@-@�@��@�^@x�@hs@X@7L@�@�9@r�@bN@A�@  @�;@�w@��@�P@��@v�@E�@{@@@�@�T@�-@�h@`B@?}@�@�/@��@z�@I�@1@�m@��@t�@t�@S�@C�@"�@"�@@��@~�@~�@n�@n�@M�@-@��@��@��@hs@G�@7L@7L@7L@&�@%@��@�u@r�@bN@Q�@ �@b@b@�@�@��@l�@K�@
=@ȴ@�+@V@@��@@�h@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�RB�RB�RB�XB�RB�LB�RB�jB�wB�wB�}B�}B�}B�}B��BŢB��B�B�yBhB1'BG�BjB�PB��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B�hB�B�JB�oB�B�'B�-B�!B�-B�9B�LB�qB�qBĜBÖBB��B�}B�^B�3B��B�{B�PB|�BdZBO�B@�B'�BbBB�B�`B��B��B�B��B��B�hB�%B`BBS�BN�BA�B<jB33B�BuB	7B
��B
�ZB
��B
��B
ƨB
�3B
��B
��B
��B
�\B
�B
z�B
t�B
cTB
I�B
>wB
<jB
0!B
�B
�B
PB
+B	��B	��B	��B	�B	�TB	��B	ĜB	�qB	�RB	�!B	��B	��B	�\B	�1B	�1B	}�B	t�B	n�B	dZB	\)B	T�B	M�B	F�B	@�B	5?B	(�B	#�B	�B	{B	JB	B��B�B�B�/B�B��B��BǮB�}B�dB�XB�LB�FB�XB�RB�RB�?B�-B�3B�-B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�{B�\B�7B�%B�B� B}�B|�Bw�Bt�Bo�Bm�Bk�BgmBe`BcTBaHB^5B[#BQ�BM�BK�BI�BF�BE�BD�BB�BB�B=qB<jB;dB9XB8RB7LB6FB6FB33B2-B1'B0!B-B,B.B-B+B,B-B0!B.B.B.B,B,B%�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B$�B%�B&�B(�B0!B33B0!B2-B33B7LB7LB9XB8RB:^B9XB9XB8RB9XB9XB9XB7LB6FB7LB8RB9XB<jB>wB:^B@�B@�B@�B?}BA�BG�BQ�BR�BP�BN�BZB]/B]/B]/B]/B]/B\)B\)BXBVBYBYB\)B]/BbNBgmBk�Bn�Bp�Bq�Br�Bt�Br�Bs�Bw�B{�B{�B� B� B�B�B�B�B�+B�=B�VB�hB��B��B��B��B��B��B��B��B��B�B�B�B�3B�?B�FB�RB�^B�dB�qB�wB�qBĜBȴB��B��B��B��B��B��B��B�B�
B�B�B�B�#B�#B�)B�;B�BB�HB�TB�`B�sB�B�B�B��B��B��B��B	  B	B	B	B	B	B	
=B	PB	oB	{B	�B	�B	�B	 �B	"�B	$�B	%�B	#�B	$�B	'�B	.B	0!B	1'B	49B	6FB	9XB	9XB	:^B	<jB	>wB	A�B	D�B	E�B	G�B	G�B	G�B	G�B	G�B	H�B	L�B	M�B	O�B	VB	YB	ZB	^5B	bNB	dZB	dZB	ffB	hsB	jB	k�B	m�B	n�B	q�B	t�B	x�B	y�B	}�B	� B	�B	�B	�+B	�=B	�JB	�PB	�VB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�3B	�9B	�?B	�FB	�LB	�LB	�XB	�dB	�dB	�dB	�jB	�dB	�jB	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�5B	�5B	�;B	�HB	�TB	�ZB	�`B	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
1B
1B
	7B
1B
1B
	7B

=B

=B

=B

=B
DB
JB
PB
VB
VB
\B
\B
\B
VB
PB
PB
PB
PB
VB
\B
\B
\B
\B
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
49B
49B
49B
49B
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
8RB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
C�B
D�B
D�B
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
G�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
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
O�B
O�B
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
Q�B
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
T�B
T�B
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
XB
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
[#B
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
bNB
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
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�8B�8B�8B�>B�8B�2B�8B�PB�]B�]B�cB�cB�cB�cB�iBňB͹B��B�_BNB1BG�BjeB�6B�sB�qB��B��B��B�B�yB��B��B��B��B�mB�FB�mB�sB�NB��B�0B�TB��B��B�B�B�B�B�2B�VB�<BāB�{B�uB�UB�cB�DB�B��B�aB�6B|�Bd@BO�B@iB'�B.B�B�}B�,B��B�UB��B��B��B�NB�B`'BS�BN�BAUB<6B2�B�B[B	B
��B
�@B
��B
͹B
ƎB
�B
��B
��B
�B
�(B
��B
z�B
t�B
c B
I�B
>BB
<PB
/�B
�B
mB
6B
B	��B	��B	�B	�kB	�:B	ҽB	āB	�<B	�B	��B	��B	�B	�(B	�B	��B	}�B	t�B	n}B	d@B	\B	T�B	M�B	F�B	@iB	5%B	(�B	#�B	�B	FB	B	�B��B�B�QB��B��BοBʦB�zB�cB�JB�$B�B�+B�>B�B�B�%B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�yB�mB�FB�BB�B��B��B�B}�B|�Bw�Bt�BoiBm]BkkBg8Be,Bc BaB^B[	BQ�BM�BK�BI�BFtBEmBDgBBuBB[B=<B<6B;0B9>B8B7B6+B6B3B1�B0�B0B,�B+�B-�B,�B*�B+�B,�B0B-�B-�B-�B+�B+�B%�B"�B �B�B�B�B�B~B�B~BxB~BxB�B�B�B~B�B!�B"�B$�B%�B&�B(�B0B2�B/�B1�B3B7B7B9$B88B:DB9$B9$B8B9$B9$B9$B7B6B7B8B9>B<6B>BB:*B@iB@iB@OB?cBAUBG�BQ�BR�BP�BN�BY�B\�B\�B\�B]B]B\B[�BW�BU�BX�BX�B[�B]BbBgRBkkBncBpoBqvBr|Bt�Br|Bs�Bw�B{�B{�B�B�B��B��B��B��B��B�	B�"B�NB�SB�yB�kB��B��B��B��B��B��B��B��B��B��B�B�B�B�DB�0B�VB�BB�<BāBȀB̘BϫBбBѷBҽB��B��B��B��B��B��B��B��B��B�B�B�B�B� B�,B�XB�KB�cB�|B��B��B��B��B��B	 �B	�B	�B	�B	�B	
	B	6B	:B	FB	_B	_B	~B	 �B	"�B	$�B	%�B	#�B	$�B	'�B	-�B	/�B	0�B	4B	6+B	9$B	9$B	:DB	<6B	>BB	AUB	DgB	EmB	GzB	GzB	GzB	GzB	GzB	H�B	L�B	M�B	O�B	U�B	X�B	Y�B	^B	bB	d&B	d&B	f2B	h>B	jKB	kQB	m]B	ncB	qvB	t�B	x�B	y�B	}�B	�B	��B	��B	�B	�	B	�B	�6B	�"B	�BB	�.B	�4B	�@B	�FB	�SB	�B	�kB	��B	�xB	��B	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�+B	�B	�B	�$B	�0B	�JB	�0B	�6B	�0B	�6B	�OB	�UB	�UB	�aB	�gB	�mB	ƎB	ȚB	˒B	бB	бB	бB	бB	бB	ϫB	��B	ΥB	��B	бB	��B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�-B	� B	�&B	�,B	�RB	�8B	�8B	�DB	�kB	�QB	�KB	�WB	�}B	�iB	�vB	�B	�vB	��B	�vB	�vB	�|B	�|B	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
B
�B
B
�B
�B
	B
	B
	B
B
�B
	B
B
�B
	B

#B

#B

	B

#B
B
B
6B
"B
"B
(B
(B
(B
"B
B
B
B
6B
"B
(B
(B
BB
(B
4B
NB
:B
@B
@B
@B
[B
@B
aB
FB
FB
aB
FB
MB
MB
SB
SB
MB
SB
MB
FB
gB
MB
MB
MB
gB
MB
MB
gB
YB
YB
sB
YB
YB
YB
_B
eB
eB
eB
eB
eB
eB
kB
kB
kB
kB
kB
�B
qB
xB
xB
xB
~B
�B
�B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
.�B
0B
0B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0B
0B
0�B
1B
1�B
1�B
4B
4B
4B
4B
5B
5B
5B
5%B
5B
6B
6B
72B
7B
7B
72B
8B
:*B
:*B
;0B
;0B
;0B
;0B
;0B
;JB
;0B
;JB
;0B
<6B
=<B
=<B
>BB
>]B
>BB
>BB
?HB
?cB
@iB
@iB
@OB
@OB
@OB
@iB
@OB
AoB
AUB
B[B
BuB
B[B
C{B
CaB
C{B
CaB
CaB
CaB
CaB
CaB
CaB
CaB
DgB
DgB
DgB
DgB
D�B
EmB
EmB
EmB
E�B
FtB
F�B
F�B
FtB
FtB
FtB
G�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
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
O�B
O�B
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
Q�B
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
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
V�B
W�B
W�B
W�B
W�B
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
Y�B
Y�B
Y�B
Y�B
Z�B
[	B
Z�B
[	B
[	B
Z�B
[	B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
]B
\�B
\�B
\�B
^B
^B
^B
^B
_B
_B
_!B
_!B
_B
_B
_!B
`B
`B
`'B
`'B
`B
`B
aB
a-B
aB
aB
b4B
bB
bB
bB
b4B
bB
b4B
c B
bB
c B
c B
c:B
c:B
c B
c B
c:B
d@B
d&B
d&B
d&B
d&B
e,B
e,B
e,B
eFB
e,B
eFB
e,B
eFB
e,B
eFB
e,B
f2B
f2B
f2B
f2B
f2B
fLB
g8B
g8B
gRB
g8B
g8B
h>B
hXB
h>B
h>B
h>B
h>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.37(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807220044192018072200441920180722004419201807230031352018072300313520180723003135JA  ARFMdecpA19c                                                                20180724093530  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180724004642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180724004645  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180724004645  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180724004646  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180724004646  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180724004646  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180724004646  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180724004646  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180724004647                      G�O�G�O�G�O�                JA  ARUP                                                                        20180724005613                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180717154017  CV  JULD            G�O�G�O�FÓ�                JM  ARCAJMQC2.0                                                                 20180721154419  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180721154419  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180722153135  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                