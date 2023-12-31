CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-07-26T12:46:25Z creation;2021-07-26T12:46:32Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210726124625  20210726125254  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA                                  2B  A   APEX                            7906                            051216                          846 @ن`��1   @ن�l@3�/��w�d�333331   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@fD@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�C3D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@��
A�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�B{Bz�B!z�B)z�B1z�B9z�BA�GBIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��B��B��qB��qB��qB��qB��qB��qB��qB��qB��B��B��qB��>BĽqBȽqB̽qBнqBԽqBؽqB��B��B�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�CEC^�C^�C^�C^�C ^�C"^�C$xRC&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CPxRCR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|xRC~xRC�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D�HD�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D�HD�D��D�D��DD��D�D��D�D��D�D�HD�D��D�D�D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�D(�D(��D)�D)�D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8HD8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�D@D@��DA�DA��DBHDB��DC�DC��DD�DD��DE�DE��DFDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU�DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��DdHDd��De�De��DfDf��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz�D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D���D�ȤD��D�O
D���D���D��D�K�D���D���D��D�K�D���D��
D��D�K�D���D���D��D�O
D���D���D��D�K�D���D���D��D�K�D���D��
D�
D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�H�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D�D���D��D�K�DË�D���D��D�K�Dċ�D���D��D�K�Dŋ�D���D��D�K�DƋ�D���D��D�K�Dǋ�D���D��D�K�Dȋ�D��
D��D�K�Dɋ�D���D��D�K�Dʋ�D���D��D�K�Dˋ�D���D��D�K�D̋�D���D��D�H�D͋�D���D��D�K�D΋�D�ȤD��D�K�Dϋ�D���D��D�K�DЋ�D���D��D�K�Dы�D���D��D�K�Dҋ�D���D��D�K�DӋ�D���D��D�K�Dԋ�D���D��D�K�DՋ�D���D��D�K�D֋�D���D��D�K�D׋�D���D��D�O
D؋�D���D��D�K�Dً�D���D��D�K�Dڋ�D���D��D�K�Dۋ�D���D��D�H�D܋�D���D��D�K�D݋�D���D��D�K�Dވ�D���D��D�K�Dߋ�D���D��D�K�D���D���D��D�K�D��D�ȤD��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D判D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D�ȤD��D�K�D���D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�O
D���D���D��D�K�D���D���D��D�K�D���D���D�
D�K�D��
D��
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AہAۃAۃAۃAۅAۇ+Aۇ+Aۉ7Aۉ7Aۉ7AۋDAۋDAۋDAۋDAۋDAۍPAۍPAۏ\AۋDAۋDAۅAۃA�v�A�jA�VA�ZA�ZA�E�A�;dA�(�A��A��
A�M�A�  A֍PA�l�A��A��#Aգ�A���A�5?A��A�I�A�XA���A�5?A��
A�XA�/A˟�A���Aǟ�Ař�A�p�A°!A�(�A��mA�\)A��-A��;A��`A�1A�n�A��/A�"�A��jA��;A���A�
=A�~�A��;A���A�%A��hA�dZA���A��A��FA�bNA��HA�l�A��yA���A��A�(�A�E�A���A���A���A���A�`BA�%A�XA�r�A��A�dZA�1'A��A��A�r�A��A�v�A�~�A��yA��+A�?}A��hA��`A���A���A�hsA�%A~9XA{x�Ax��Av��Au7LAt9XAr~�Apn�AnffAl�DAg��Ab��A`�RA^�!A]�A[C�AX��AUXATjAS��ARVAP�`AMVAK
=AG�AC��AAC�A>�`A<A;oA:ZA8M�A6��A4�jA3C�A1��A/�wA.bA-�FA-S�A,jA+p�A+A*�\A*E�A)`BA&��A%��A$�DA"��A!��A!G�A (�AC�AbA�-A�A��A��A��An�A�;AĜA�FA�#A�A"�A�A�AbA�A�RA�DAE�A��AQ�AdZA�#A
��A�A
=A	�wA	"�A��AI�Ax�AbNA��A?}A��AQ�A��A��A��Av�A��A
=A ��@��
@��-@���@��#@�V@�r�@���@���@�o@��^@�A�@���@���@�j@��m@�\@�?}@�E�@�A�@��@ߍP@�n�@�hs@ܛ�@ە�@��T@�hs@؋D@ץ�@�\)@���@ְ!@֗�@�@�G�@�z�@�I�@�(�@��@�+@�M�@�$�@Ѻ^@��`@�A�@ύP@�ȴ@�J@�@͑h@�&�@�j@˾w@˅@�
=@�hs@�r�@�b@��m@�ƨ@�E�@��#@ź^@�G�@��@ċD@�b@�;d@�E�@��@��@��P@�;d@�ȴ@��\@�ff@�$�@��@�x�@���@�z�@��@�33@��y@�ff@�hs@�V@���@���@�(�@��y@��\@��\@�n�@��@���@�r�@� �@���@���@�\)@��H@���@�ff@�-@���@��@�b@��m@���@�l�@�"�@�
=@�^5@���@��T@��^@�?}@�Ĝ@��@�9X@�1@���@�;d@���@��R@�v�@��@���@�hs@�/@�/@���@�r�@�9X@��m@���@�t�@�K�@�C�@��@�v�@�{@�J@��@���@�7L@��`@���@��j@�j@� �@��m@���@��P@��P@��@�S�@��@��@��\@��@���@��h@�O�@��@��`@�r�@�(�@� �@�9X@�A�@� �@�  @��;@�t�@��@�ȴ@�@���@��7@���@���@�Q�@�b@��m@���@�\)@�@��y@��@���@�-@��#@��^@�hs@��@��j@�z�@�9X@��@��F@���@�t�@�+@�"�@�o@��R@�=q@�-@�J@���@��T@�@���@�x�@�G�@���@��9@��u@��D@�bN@� �@���@���@�K�@��@���@�ȴ@�ȴ@�ȴ@��R@�v�@��@��^@�`B@���@�Q�@�I�@�Q�@�I�@�  @��m@��F@�|�@�C�@�+@��@�@��H@��!@�=q@��T@�@��@�&�@���@��9@���@�bN@�  @��F@�K�@��@���@��+@��T@�O�@��@���@���@��j@���@�r�@�j@�Z@� �@���@���@��m@��w@���@�\)@��@���@�~�@�E�@�$�@���@��T@���@���@��h@��@�`B@�/@���@��9@�r�@�I�@�b@��@��P@�t�@�K�@�@��@��@��+@�-@���@��7@�G�@��@��@�Q�@�(�@��@�w@l�@;d@�@~�R@~$�@}�-@|�@|�/@|��@|�j@|�@|��@|�@{�F@{�@{"�@{@zM�@yhs@x�`@xr�@w�;@w
=@vv�@up�@t��@tI�@t(�@s��@r�@r��@rM�@q�^@q&�@pA�@o��@o�@n�R@n�+@nff@n$�@m�@l�@l�@l��@l�@k�@kS�@j�H@j��@j�\@j^5@j^5@j�@i�@i�^@ihs@i7L@h��@h�@h1'@hb@g�w@g+@fȴ@f��@f$�@ep�@d�@d9X@c�F@cS�@c@b��@b�\@b~�@bn�@a��@aG�@`�`@`��@`r�@`  @_�;@_|�@^��@^E�@]p�@\��@\�@\�@\I�@\9X@\9X@\(�@\�@\1@[ƨ@[o@Z�@Yhs@Y&�@Y%@XĜ@X�@X �@W�;@W�w@W�@Wl�@W
=@Vȴ@V�+@Vff@V$�@U�-@U`B@UO�@U?}@UV@TZ@Sƨ@St�@SS�@SC�@R�@R�H@R��@R��@R�\@R-@Q��@Q�^@Q��@QG�@Q�@P�u@P1'@O��@O�@O�P@Ol�@OK�@O+@N�y@N�R@N��@N5?@N@M��@M�@L��@Lz�@Lj@LI�@L(�@L1@K�m@K��@KS�@K@J��@I�@H��@H��@HbN@H  @G�@F�@F��@E�@Ep�@EV@D��@Dj@C��@Cƨ@Ct�@CS�@C"�@B�\@B�@A�^@Ax�@AX@A�@@��@@�u@@A�@?�@?�;@?��@?\)@>��@>ȴ@>�R@>�+@>v�@>5?@>$�@>$�@=�@=��@=@=�@<�@<Z@<9X@<(�@<(�@;33@:~�@:^5@:M�@:=q@9��@9��@8��@8�@7�@7�w@7��@7K�@7�@6ȴ@6�R@6��@6v�@6V@5@5�h@5p�@5O�@5V@4j@3��@3�m@3��@2�H@2�\@2=q@1�#@1�7@1�7@1�7@1�@0��@0Ĝ@0��@0r�@0Q�@0Q�@0A�@01'@0 �@/�@/\)@/�@.��@.ȴ@.��@.��@.�+@.ff@.E�@.$�@.@-�T@-��@,�@,�D@,j@,�@+�
@+�@+o@*�H@*��@*~�@)�@)�7@)X@)7L@)%@(��@(�9@(�u@(r�@(Q�@(b@'��@'�@'�P@'K�@'
=@&��@&�y@&ȴ@&��@&�+@&v�@&5?@&{@&@&@%@%`B@$�j@$j@$Z@$(�@#�m@#�@#"�@#@#@#@"�H@"��@"^5@"�@!�#@!��@!�7@!%@ �@ bN@ Q�@ A�@ b@�@�;@��@�+@E�@��@�@/@�/@j@9X@�F@dZ@S�@C�@33@"�@@�!@~�@M�@�@�#@��@x�@x�@hs@�@��@��@Ĝ@Q�@�@�;@|�@+@��@�@�R@��@��@ff@�T@p�@`B@/@�@��@�j@�@��@�D@z�@Z@�@1@�
@��@t�@dZ@S�@C�@C�@"�@�@�!@n�@M�@-@�@��@��@�^@�^@�^@�7@hs@�@%@%@%@��@�u@bN@bN@Q�@ �@�;@�;@��@�@��@|�@|�@l�@\)@K�@;d@
=@�R@�+@ff@5?@$�@��@��@O�@�@V@V@V@V@�@��@z�@I�@9X@�@�F@�F@��@t�@C�@o@
�!@
=q@	�^@	x�@	X@	7L@	&�@Ĝ@Q�@Q�@A�@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AہAۃAۃAۃAۅAۇ+Aۇ+Aۉ7Aۉ7Aۉ7AۋDAۋDAۋDAۋDAۋDAۍPAۍPAۏ\AۋDAۋDAۅAۃA�v�A�jA�VA�ZA�ZA�E�A�;dA�(�A��A��
A�M�A�  A֍PA�l�A��A��#Aգ�A���A�5?A��A�I�A�XA���A�5?A��
A�XA�/A˟�A���Aǟ�Ař�A�p�A°!A�(�A��mA�\)A��-A��;A��`A�1A�n�A��/A�"�A��jA��;A���A�
=A�~�A��;A���A�%A��hA�dZA���A��A��FA�bNA��HA�l�A��yA���A��A�(�A�E�A���A���A���A���A�`BA�%A�XA�r�A��A�dZA�1'A��A��A�r�A��A�v�A�~�A��yA��+A�?}A��hA��`A���A���A�hsA�%A~9XA{x�Ax��Av��Au7LAt9XAr~�Apn�AnffAl�DAg��Ab��A`�RA^�!A]�A[C�AX��AUXATjAS��ARVAP�`AMVAK
=AG�AC��AAC�A>�`A<A;oA:ZA8M�A6��A4�jA3C�A1��A/�wA.bA-�FA-S�A,jA+p�A+A*�\A*E�A)`BA&��A%��A$�DA"��A!��A!G�A (�AC�AbA�-A�A��A��A��An�A�;AĜA�FA�#A�A"�A�A�AbA�A�RA�DAE�A��AQ�AdZA�#A
��A�A
=A	�wA	"�A��AI�Ax�AbNA��A?}A��AQ�A��A��A��Av�A��A
=A ��@��
@��-@���@��#@�V@�r�@���@���@�o@��^@�A�@���@���@�j@��m@�\@�?}@�E�@�A�@��@ߍP@�n�@�hs@ܛ�@ە�@��T@�hs@؋D@ץ�@�\)@���@ְ!@֗�@�@�G�@�z�@�I�@�(�@��@�+@�M�@�$�@Ѻ^@��`@�A�@ύP@�ȴ@�J@�@͑h@�&�@�j@˾w@˅@�
=@�hs@�r�@�b@��m@�ƨ@�E�@��#@ź^@�G�@��@ċD@�b@�;d@�E�@��@��@��P@�;d@�ȴ@��\@�ff@�$�@��@�x�@���@�z�@��@�33@��y@�ff@�hs@�V@���@���@�(�@��y@��\@��\@�n�@��@���@�r�@� �@���@���@�\)@��H@���@�ff@�-@���@��@�b@��m@���@�l�@�"�@�
=@�^5@���@��T@��^@�?}@�Ĝ@��@�9X@�1@���@�;d@���@��R@�v�@��@���@�hs@�/@�/@���@�r�@�9X@��m@���@�t�@�K�@�C�@��@�v�@�{@�J@��@���@�7L@��`@���@��j@�j@� �@��m@���@��P@��P@��@�S�@��@��@��\@��@���@��h@�O�@��@��`@�r�@�(�@� �@�9X@�A�@� �@�  @��;@�t�@��@�ȴ@�@���@��7@���@���@�Q�@�b@��m@���@�\)@�@��y@��@���@�-@��#@��^@�hs@��@��j@�z�@�9X@��@��F@���@�t�@�+@�"�@�o@��R@�=q@�-@�J@���@��T@�@���@�x�@�G�@���@��9@��u@��D@�bN@� �@���@���@�K�@��@���@�ȴ@�ȴ@�ȴ@��R@�v�@��@��^@�`B@���@�Q�@�I�@�Q�@�I�@�  @��m@��F@�|�@�C�@�+@��@�@��H@��!@�=q@��T@�@��@�&�@���@��9@���@�bN@�  @��F@�K�@��@���@��+@��T@�O�@��@���@���@��j@���@�r�@�j@�Z@� �@���@���@��m@��w@���@�\)@��@���@�~�@�E�@�$�@���@��T@���@���@��h@��@�`B@�/@���@��9@�r�@�I�@�b@��@��P@�t�@�K�@�@��@��@��+@�-@���@��7@�G�@��@��@�Q�@�(�@��@�w@l�@;d@�@~�R@~$�@}�-@|�@|�/@|��@|�j@|�@|��@|�@{�F@{�@{"�@{@zM�@yhs@x�`@xr�@w�;@w
=@vv�@up�@t��@tI�@t(�@s��@r�@r��@rM�@q�^@q&�@pA�@o��@o�@n�R@n�+@nff@n$�@m�@l�@l�@l��@l�@k�@kS�@j�H@j��@j�\@j^5@j^5@j�@i�@i�^@ihs@i7L@h��@h�@h1'@hb@g�w@g+@fȴ@f��@f$�@ep�@d�@d9X@c�F@cS�@c@b��@b�\@b~�@bn�@a��@aG�@`�`@`��@`r�@`  @_�;@_|�@^��@^E�@]p�@\��@\�@\�@\I�@\9X@\9X@\(�@\�@\1@[ƨ@[o@Z�@Yhs@Y&�@Y%@XĜ@X�@X �@W�;@W�w@W�@Wl�@W
=@Vȴ@V�+@Vff@V$�@U�-@U`B@UO�@U?}@UV@TZ@Sƨ@St�@SS�@SC�@R�@R�H@R��@R��@R�\@R-@Q��@Q�^@Q��@QG�@Q�@P�u@P1'@O��@O�@O�P@Ol�@OK�@O+@N�y@N�R@N��@N5?@N@M��@M�@L��@Lz�@Lj@LI�@L(�@L1@K�m@K��@KS�@K@J��@I�@H��@H��@HbN@H  @G�@F�@F��@E�@Ep�@EV@D��@Dj@C��@Cƨ@Ct�@CS�@C"�@B�\@B�@A�^@Ax�@AX@A�@@��@@�u@@A�@?�@?�;@?��@?\)@>��@>ȴ@>�R@>�+@>v�@>5?@>$�@>$�@=�@=��@=@=�@<�@<Z@<9X@<(�@<(�@;33@:~�@:^5@:M�@:=q@9��@9��@8��@8�@7�@7�w@7��@7K�@7�@6ȴ@6�R@6��@6v�@6V@5@5�h@5p�@5O�@5V@4j@3��@3�m@3��@2�H@2�\@2=q@1�#@1�7@1�7@1�7@1�@0��@0Ĝ@0��@0r�@0Q�@0Q�@0A�@01'@0 �@/�@/\)@/�@.��@.ȴ@.��@.��@.�+@.ff@.E�@.$�@.@-�T@-��@,�@,�D@,j@,�@+�
@+�@+o@*�H@*��@*~�@)�@)�7@)X@)7L@)%@(��@(�9@(�u@(r�@(Q�@(b@'��@'�@'�P@'K�@'
=@&��@&�y@&ȴ@&��@&�+@&v�@&5?@&{@&@&@%@%`B@$�j@$j@$Z@$(�@#�m@#�@#"�@#@#@#@"�H@"��@"^5@"�@!�#@!��@!�7@!%@ �@ bN@ Q�@ A�@ b@�@�;@��@�+@E�@��@�@/@�/@j@9X@�F@dZ@S�@C�@33@"�@@�!@~�@M�@�@�#@��@x�@x�@hs@�@��@��@Ĝ@Q�@�@�;@|�@+@��@�@�R@��@��@ff@�T@p�@`B@/@�@��@�j@�@��@�D@z�@Z@�@1@�
@��@t�@dZ@S�@C�@C�@"�@�@�!@n�@M�@-@�@��@��@�^@�^@�^@�7@hs@�@%@%@%@��@�u@bN@bN@Q�@ �@�;@�;@��@�@��@|�@|�@l�@\)@K�@;d@
=@�R@�+@ff@5?@$�@��@��@O�@�@V@V@V@V@�@��@z�@I�@9X@�@�F@�F@��@t�@C�@o@
�!@
=q@	�^@	x�@	X@	7L@	&�@Ĝ@Q�@Q�@A�@1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
e`B
e`B
dZB
dZB
bNB
bNB
aHB
`BB
^5B
]/B
ZB
T�B
7LB
VB
B
B	��B
B
  B	��B	�TB	�5B	��B
%�B
C�B
iyB
�%B
�=B
�=B
�uB
�FB
�}B
ŢB
��B
�NB  B�Bk�B��BɺB=qBN�BVBiyBjBcTB_;B�+B�BdZBZBI�BQ�B8RBB�3B��B�RB�/B�sB��B%BbBVB	7B��B�B�!B_;BK�B-B�BbBB
=B
��B
��B
��B
��B
��B
�BB
��B
��B
�oB
�+B
w�B
x�B
cTB
iyB
�+B
w�B
n�B
^5B
D�B
#�B
oB
hB
oB
	7B
B	��B	�TB	��B	��B	��B	�DB	�+B	}�B	t�B	dZB	ZB	T�B	N�B	G�B	;dB	0!B	&�B	�B	{B	�B	{B	PB	DB��B�B�mB�BB�#B��B��B��BɺBƨBÖB��B�}B�wB�}B�XB�LB�LB�jB�LB�FB�?B�FB�?B�3B�3B�'B�B�B�B�!B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�JB��B�B�B�3B�9B�9B�3B�FB�FB�RB�RB�dB��BǮBǮBȴB��B��B�B�
B�/B�5B�;B�TB�TB�ZB�TB�ZB�ZB�ZB�TB�NB�HB�ZB�`B�ZB�TB�)B��B��B��B��B�B�
B�B�B�)B�HB�NB�TB�mB�B��B��B��B��B��B	  B	B	B	1B	PB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	#�B	#�B	$�B	'�B	&�B	(�B	-B	/B	0!B	/B	.B	.B	-B	.B	-B	)�B	(�B	(�B	+B	+B	,B	,B	,B	-B	.B	0!B	49B	6FB	7LB	9XB	<jB	<jB	=qB	=qB	=qB	@�B	A�B	B�B	C�B	G�B	N�B	R�B	T�B	VB	XB	YB	\)B	^5B	bNB	cTB	e`B	jB	l�B	l�B	m�B	n�B	p�B	p�B	v�B	x�B	z�B	z�B	~�B	�B	�B	�%B	�1B	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�?B	�FB	�RB	�dB	�jB	�jB	�jB	�qB	��B	B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�/B	�;B	�;B	�5B	�BB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B

=B

=B
JB
JB
PB
PB
VB
\B
bB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
)�B
)�B
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
/B
/B
/B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
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
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
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
W
B
XB
XB
YB
YB
YB
ZB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
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
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
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
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
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
jB
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
l�B
l�B
l�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
e`B
e`B
dZB
dZB
bNB
bNB
aHB
`BB
^5B
]/B
ZB
T�B
7LB
VB
B
B	��B
B
  B	��B	�TB	�5B	��B
%�B
C�B
iyB
�%B
�=B
�=B
�uB
�FB
�}B
ŢB
��B
�NB  B�Bk�B��BɺB=qBN�BVBiyBjBcTB_;B�+B�BdZBZBI�BQ�B8RBB�3B��B�RB�/B�sB��B%BbBVB	7B��B�B�!B_;BK�B-B�BbBB
=B
��B
��B
��B
��B
��B
�BB
��B
��B
�oB
�+B
w�B
x�B
cTB
iyB
�+B
w�B
n�B
^5B
D�B
#�B
oB
hB
oB
	7B
B	��B	�TB	��B	��B	��B	�DB	�+B	}�B	t�B	dZB	ZB	T�B	N�B	G�B	;dB	0!B	&�B	�B	{B	�B	{B	PB	DB��B�B�mB�BB�#B��B��B��BɺBƨBÖB��B�}B�wB�}B�XB�LB�LB�jB�LB�FB�?B�FB�?B�3B�3B�'B�B�B�B�!B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�JB��B�B�B�3B�9B�9B�3B�FB�FB�RB�RB�dB��BǮBǮBȴB��B��B�B�
B�/B�5B�;B�TB�TB�ZB�TB�ZB�ZB�ZB�TB�NB�HB�ZB�`B�ZB�TB�)B��B��B��B��B�B�
B�B�B�)B�HB�NB�TB�mB�B��B��B��B��B��B	  B	B	B	1B	PB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	#�B	#�B	$�B	'�B	&�B	(�B	-B	/B	0!B	/B	.B	.B	-B	.B	-B	)�B	(�B	(�B	+B	+B	,B	,B	,B	-B	.B	0!B	49B	6FB	7LB	9XB	<jB	<jB	=qB	=qB	=qB	@�B	A�B	B�B	C�B	G�B	N�B	R�B	T�B	VB	XB	YB	\)B	^5B	bNB	cTB	e`B	jB	l�B	l�B	m�B	n�B	p�B	p�B	v�B	x�B	z�B	z�B	~�B	�B	�B	�%B	�1B	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�?B	�FB	�RB	�dB	�jB	�jB	�jB	�qB	��B	B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�/B	�;B	�;B	�5B	�BB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B

=B

=B
JB
JB
PB
PB
VB
\B
bB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
)�B
)�B
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
/B
/B
/B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
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
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
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
W
B
XB
XB
YB
YB
YB
ZB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
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
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
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
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
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
jB
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
l�B
l�B
l�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210726214439  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210726124625  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210726124629  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210726124630  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210726124631  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210726124631  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210726124631  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210726124631  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210726124631  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210726124632                      G�O�G�O�G�O�                JA  ARUP                                                                        20210726125254                      G�O�G�O�G�O�                