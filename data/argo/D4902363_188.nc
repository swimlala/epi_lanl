CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-12T00:35:13Z creation;2017-12-12T00:35:17Z conversion to V3.1;2019-12-19T07:54:30Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20171212003513  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_188                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�<3��0 1   @�<4q��@;�2a|��dZ{���m1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��H@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cw�Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oA�oA�{A�oA��A��A��A��A��A��A��A��A�"�A��A�A�A�A�VA�oA���A��A��A��HA��A��#A��#A��A�&�A���A�\)A�A���A�t�A�\)A��TA�x�A���A���A���A�VA��\A�1A�ZA���A�n�A���A�A���A�z�A��A�A�A�bA���A�`BA���A�\)A�5?A�bNA��!A�5?A�A���A�VA��A�7LA���A��+A�&�A���A��yA��RA�A�A��A�jA�/A���A��A��9A��A�-A��DA�9XA��A�A}��A}&�A|bA{l�Azn�Ax��Aw�wAw%Av1'At�As?}AqK�Ao��An��An(�Al�yAk+Ai�;Ah�`AhI�AghsAf�\AfJAe�7Adv�Ac�AcC�Aa�mAaA_�A^��A^5?A]%A[�
AZ��AY�AX��AXĜAX$�AW�AU��AUoATr�ASl�AR�AQ��AP��AP(�AO\)AM�AL�AK�AK&�AJ�yAJA�AH��AFAE+AD�AD�AC�^ABbNAB{AA�A@~�A?�A?�7A>�+A=�A<z�A;��A;hsA:�+A9A8z�A6��A6VA6�A5��A5K�A4�yA3�-A2{A1dZA1S�A1;dA0��A/p�A.�9A.n�A-�7A,JA+��A+dZA++A*��A*��A*��A*~�A*VA)��A(�A&�`A&�9A&v�A$�/A#G�A"r�A!33A �+A E�A (�A 1A�A�
AĜA��A=qA��A��A|�AG�A
=A��A��A$�A+Az�A��A�Az�A�TA�hA7LAA�!A-Ar�A��A�9A��A&�A�DA��A�7A��A�!A�#A�AAdZA+A��A��A�/A��A{A��A�A�/A=qA��A%A��A5?A��A ��@�|�@�1'@�J@�?}@���@��\@��#@���@��j@�b@�33@���@�v�@�-@��@�1'@��@�l�@�^5@�^@�%@�1@��@��/@�j@��m@�33@޸R@���@�v�@�J@�p�@ܛ�@��@�bN@�=q@���@�j@�+@�@ҧ�@�J@Ѳ-@�Z@�^5@�{@Ͳ-@̬@˕�@ʏ\@���@��@Ɵ�@ř�@��@��
@�=q@��7@��u@�K�@�ȴ@�{@���@��@��@��@���@��7@���@�A�@�"�@�J@���@��/@��@�C�@��R@�J@�{@��-@�?}@� �@�"�@�$�@���@�?}@�l�@���@�V@�5?@�-@�-@�{@��T@��T@���@���@�1@��F@�33@��y@���@�=q@�O�@��u@�Z@�1'@���@���@�
=@��^@�&�@��9@�bN@��@�
=@�n�@��T@�`B@��@�I�@��m@�ȴ@��7@�z�@���@�@��\@��@�G�@���@��m@�ƨ@�t�@�;d@��y@��H@��H@�ff@�hs@�O�@�/@���@��F@�dZ@�C�@���@��+@�V@�=q@��@���@��`@��@�Q�@�  @��m@�;d@�-@���@���@�G�@���@��`@�Ĝ@��9@��@��@�bN@�I�@� �@�  @�  @�1@��@���@��R@��#@�?}@��/@���@�9X@��
@�l�@�C�@�33@�o@���@��y@���@���@�ff@�J@��@�O�@��@���@�Z@�9X@��@�  @��@�@\)@l�@l�@l�@l�@|�@l�@\)@\)@+@~��@}�@|�@|Z@|(�@{��@{�F@{�@{33@z-@y��@yX@y&�@xĜ@x1'@v��@u?}@u�@t��@tI�@t(�@t1@s��@s��@s�m@s��@s��@sS�@r^5@qhs@q�@pĜ@pA�@o�;@ol�@n�@n$�@m�@l��@lz�@l�D@lz�@lz�@lj@lj@k�
@ko@ko@ko@ko@j�H@jn�@i��@ix�@ihs@hĜ@hbN@h�u@h�9@h�u@h  @h  @h�@h1'@g��@gl�@g\)@gK�@g�@fE�@f{@e�h@e/@e/@d�@d�j@d��@d�D@dj@d9X@d1@ct�@c"�@co@co@b�@b��@b�!@b�!@b��@b��@b�\@b~�@bM�@a�^@ax�@a7L@`�`@`Ĝ@`��@`r�@`A�@`1'@` �@_�w@_��@_�P@_l�@_l�@_;d@^��@^��@^E�@^@]?}@]?}@\��@\�@\�@[��@Z��@Zn�@Z=q@Y��@Y��@Y�^@Yx�@Y�@X�9@X�u@X�@XbN@XQ�@XA�@W�@W\)@Vȴ@U@U��@U?}@T�j@T9X@TI�@Sƨ@So@R�@Q�#@Q�^@Q�7@PĜ@O�;@O\)@N��@N�@N�R@N��@N�+@N5?@N$�@M�@M`B@M�@L��@L�@L�/@L�j@Lz�@Lj@L(�@K�F@K33@K@J�@I%@HĜ@H�@Hr�@HbN@HQ�@H1'@G�@GK�@G+@F�R@Fff@F5?@E�-@Cƨ@C@B��@B��@Bn�@B=q@A��@Ahs@AG�@AX@Ahs@Ahs@Ahs@AX@A%@@Ĝ@@bN@@1'@@ �@?�@?|�@?�@>�R@>�+@=�@=�-@=�h@=�h@=�@=`B@<�@<j@<�@;�
@;��@;C�@;@;@:��@:n�@:=q@:=q@:=q@:�@:�@:J@:J@9�@9��@9�7@9X@97L@97L@9�@8��@8�9@8bN@8  @7��@7�@7�P@7l�@7K�@6�y@6�+@5�@5�h@5?}@4��@4��@4�@4z�@3��@3t�@3"�@2��@2~�@2-@1��@1X@1%@0Ĝ@0r�@01'@/�w@/�P@/�P@/|�@/K�@/;d@.��@.��@.�+@.v�@.ff@.$�@-�@-��@-��@-�h@-`B@-�@,��@,��@,9X@+�
@+��@+�@+�@+t�@+C�@+o@*�H@*�!@*�\@*~�@*�@)��@)�^@)�^@)��@)X@(�`@(��@(bN@(Q�@(  @'��@'�@'��@'|�@'+@'
=@&�y@&�+@%�@%��@%��@%��@$��@$�@$�D@$z�@$Z@$�@#��@#dZ@#C�@#@"=q@!�@!��@!hs@!&�@ ��@ �`@ �`@ ��@ Ĝ@ �@ A�@ 1'@ b@   @�@�;@��@�@|�@K�@�@��@�@��@�+@ff@@�-@�h@?}@��@�/@�@�/@��@��@�j@��@��@��@�D@�D@z�@Z@9X@1@�F@"�@^5@x�@�@%@�`@��@r�@ �@�;@��@l�@;d@�y@��@�+@v�@v�@ff@ff@ff@V@5?@$�@�@�-@�h@�h@p�@O�@��@�@ƨ@dZ@C�@C�@C�@o@��@^5@��@�#@��@�^@��@��@x�@X@�@�`@�u@bN@1'@b@�@�;@�w@K�@�y@�@��@ff@V@5?@@��@��@@��@�h@�@p�@O�@?}@�@j@1@�
@t�@t�@t�@dZ@C�@33@33@
�@
��@
�!@
��@
�\@
~�@
n�@
^5@
M�@
=q@
�@	��@	�@	��@	��@	x�@��@Ĝ@�u@Q�@  @�w@�w@��@��@�P@l�@
=@�@ȴ@ȴ@�R@��@��@�+@ff@E�@E�@5?@$�@��@�-@�h@p�@?}@/@�@�/@�@��@j@I�@I�@(�@�m@�m@�
@�
@��@�@C�@"�@@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oA�oA�{A�oA��A��A��A��A��A��A��A��A�"�A��A�A�A�A�VA�oA���A��A��A��HA��A��#A��#A��A�&�A���A�\)A�A���A�t�A�\)A��TA�x�A���A���A���A�VA��\A�1A�ZA���A�n�A���A�A���A�z�A��A�A�A�bA���A�`BA���A�\)A�5?A�bNA��!A�5?A�A���A�VA��A�7LA���A��+A�&�A���A��yA��RA�A�A��A�jA�/A���A��A��9A��A�-A��DA�9XA��A�A}��A}&�A|bA{l�Azn�Ax��Aw�wAw%Av1'At�As?}AqK�Ao��An��An(�Al�yAk+Ai�;Ah�`AhI�AghsAf�\AfJAe�7Adv�Ac�AcC�Aa�mAaA_�A^��A^5?A]%A[�
AZ��AY�AX��AXĜAX$�AW�AU��AUoATr�ASl�AR�AQ��AP��AP(�AO\)AM�AL�AK�AK&�AJ�yAJA�AH��AFAE+AD�AD�AC�^ABbNAB{AA�A@~�A?�A?�7A>�+A=�A<z�A;��A;hsA:�+A9A8z�A6��A6VA6�A5��A5K�A4�yA3�-A2{A1dZA1S�A1;dA0��A/p�A.�9A.n�A-�7A,JA+��A+dZA++A*��A*��A*��A*~�A*VA)��A(�A&�`A&�9A&v�A$�/A#G�A"r�A!33A �+A E�A (�A 1A�A�
AĜA��A=qA��A��A|�AG�A
=A��A��A$�A+Az�A��A�Az�A�TA�hA7LAA�!A-Ar�A��A�9A��A&�A�DA��A�7A��A�!A�#A�AAdZA+A��A��A�/A��A{A��A�A�/A=qA��A%A��A5?A��A ��@�|�@�1'@�J@�?}@���@��\@��#@���@��j@�b@�33@���@�v�@�-@��@�1'@��@�l�@�^5@�^@�%@�1@��@��/@�j@��m@�33@޸R@���@�v�@�J@�p�@ܛ�@��@�bN@�=q@���@�j@�+@�@ҧ�@�J@Ѳ-@�Z@�^5@�{@Ͳ-@̬@˕�@ʏ\@���@��@Ɵ�@ř�@��@��
@�=q@��7@��u@�K�@�ȴ@�{@���@��@��@��@���@��7@���@�A�@�"�@�J@���@��/@��@�C�@��R@�J@�{@��-@�?}@� �@�"�@�$�@���@�?}@�l�@���@�V@�5?@�-@�-@�{@��T@��T@���@���@�1@��F@�33@��y@���@�=q@�O�@��u@�Z@�1'@���@���@�
=@��^@�&�@��9@�bN@��@�
=@�n�@��T@�`B@��@�I�@��m@�ȴ@��7@�z�@���@�@��\@��@�G�@���@��m@�ƨ@�t�@�;d@��y@��H@��H@�ff@�hs@�O�@�/@���@��F@�dZ@�C�@���@��+@�V@�=q@��@���@��`@��@�Q�@�  @��m@�;d@�-@���@���@�G�@���@��`@�Ĝ@��9@��@��@�bN@�I�@� �@�  @�  @�1@��@���@��R@��#@�?}@��/@���@�9X@��
@�l�@�C�@�33@�o@���@��y@���@���@�ff@�J@��@�O�@��@���@�Z@�9X@��@�  @��@�@\)@l�@l�@l�@l�@|�@l�@\)@\)@+@~��@}�@|�@|Z@|(�@{��@{�F@{�@{33@z-@y��@yX@y&�@xĜ@x1'@v��@u?}@u�@t��@tI�@t(�@t1@s��@s��@s�m@s��@s��@sS�@r^5@qhs@q�@pĜ@pA�@o�;@ol�@n�@n$�@m�@l��@lz�@l�D@lz�@lz�@lj@lj@k�
@ko@ko@ko@ko@j�H@jn�@i��@ix�@ihs@hĜ@hbN@h�u@h�9@h�u@h  @h  @h�@h1'@g��@gl�@g\)@gK�@g�@fE�@f{@e�h@e/@e/@d�@d�j@d��@d�D@dj@d9X@d1@ct�@c"�@co@co@b�@b��@b�!@b�!@b��@b��@b�\@b~�@bM�@a�^@ax�@a7L@`�`@`Ĝ@`��@`r�@`A�@`1'@` �@_�w@_��@_�P@_l�@_l�@_;d@^��@^��@^E�@^@]?}@]?}@\��@\�@\�@[��@Z��@Zn�@Z=q@Y��@Y��@Y�^@Yx�@Y�@X�9@X�u@X�@XbN@XQ�@XA�@W�@W\)@Vȴ@U@U��@U?}@T�j@T9X@TI�@Sƨ@So@R�@Q�#@Q�^@Q�7@PĜ@O�;@O\)@N��@N�@N�R@N��@N�+@N5?@N$�@M�@M`B@M�@L��@L�@L�/@L�j@Lz�@Lj@L(�@K�F@K33@K@J�@I%@HĜ@H�@Hr�@HbN@HQ�@H1'@G�@GK�@G+@F�R@Fff@F5?@E�-@Cƨ@C@B��@B��@Bn�@B=q@A��@Ahs@AG�@AX@Ahs@Ahs@Ahs@AX@A%@@Ĝ@@bN@@1'@@ �@?�@?|�@?�@>�R@>�+@=�@=�-@=�h@=�h@=�@=`B@<�@<j@<�@;�
@;��@;C�@;@;@:��@:n�@:=q@:=q@:=q@:�@:�@:J@:J@9�@9��@9�7@9X@97L@97L@9�@8��@8�9@8bN@8  @7��@7�@7�P@7l�@7K�@6�y@6�+@5�@5�h@5?}@4��@4��@4�@4z�@3��@3t�@3"�@2��@2~�@2-@1��@1X@1%@0Ĝ@0r�@01'@/�w@/�P@/�P@/|�@/K�@/;d@.��@.��@.�+@.v�@.ff@.$�@-�@-��@-��@-�h@-`B@-�@,��@,��@,9X@+�
@+��@+�@+�@+t�@+C�@+o@*�H@*�!@*�\@*~�@*�@)��@)�^@)�^@)��@)X@(�`@(��@(bN@(Q�@(  @'��@'�@'��@'|�@'+@'
=@&�y@&�+@%�@%��@%��@%��@$��@$�@$�D@$z�@$Z@$�@#��@#dZ@#C�@#@"=q@!�@!��@!hs@!&�@ ��@ �`@ �`@ ��@ Ĝ@ �@ A�@ 1'@ b@   @�@�;@��@�@|�@K�@�@��@�@��@�+@ff@@�-@�h@?}@��@�/@�@�/@��@��@�j@��@��@��@�D@�D@z�@Z@9X@1@�F@"�@^5@x�@�@%@�`@��@r�@ �@�;@��@l�@;d@�y@��@�+@v�@v�@ff@ff@ff@V@5?@$�@�@�-@�h@�h@p�@O�@��@�@ƨ@dZ@C�@C�@C�@o@��@^5@��@�#@��@�^@��@��@x�@X@�@�`@�u@bN@1'@b@�@�;@�w@K�@�y@�@��@ff@V@5?@@��@��@@��@�h@�@p�@O�@?}@�@j@1@�
@t�@t�@t�@dZ@C�@33@33@
�@
��@
�!@
��@
�\@
~�@
n�@
^5@
M�@
=q@
�@	��@	�@	��@	��@	x�@��@Ĝ@�u@Q�@  @�w@�w@��@��@�P@l�@
=@�@ȴ@ȴ@�R@��@��@�+@ff@E�@E�@5?@$�@��@�-@�h@p�@?}@/@�@�/@�@��@j@I�@I�@(�@�m@�m@�
@�
@��@�@C�@"�@@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB1B��B�B�B��B�B�B��B1B�BuB	7B��BɺB��Bl�B>wB�B��B��B��B��B��B��B�JBk�BiyBe`B[#BR�BL�BG�BA�B?}B:^B49B,B �BbB%BBBB
��B
�B
�fB
��B
��B
��B
ǮB
�wB
�B
�B
��B
��B
��B
�{B
�PB
�1B
�B
{�B
u�B
k�B
dZB
_;B
XB
O�B
D�B
9XB
-B
&�B
 �B
�B
\B
	7B
B	��B	��B	��B	�B	�B	�fB	�HB	�)B	��B	��B	ǮB	��B	�qB	�FB	�!B	�B	��B	��B	��B	��B	�oB	�DB	�1B	�B	~�B	{�B	u�B	p�B	l�B	hsB	`BB	ZB	S�B	Q�B	N�B	H�B	=qB	1'B	/B	.B	+B	%�B	�B	�B	�B	�B	oB	bB	
=B	+B	B	%B	B��B��B�B�B�B�B�B�sB�ZB�/B�
B�B�
B�B��B��BȴBȴBĜB�qB��BB��B��B��B�}B�wB�jB�XB�-B��B�B��B��B��B��B��B��B��B��B��B��B��B�VB�%B�DB�DB�DB�DB�=B�7B�1B�+B�B~�B}�B{�Bz�Bz�Bx�Bx�Bv�Bu�Bs�Bn�BffBhsBdZBcTBgmBcTBdZBdZBbNB^5BW
BE�BP�BYBZBZBZBZBXBT�BS�BR�BS�BP�BN�BO�BM�BJ�BG�BF�B>wB:^B=qBC�B?}B?}BA�B?}BA�BB�BB�BD�BD�BB�B?}B8RB-B%�B1'B1'B0!B/B/B/B9XB9XB9XB:^B=qB?}B@�B>wB<jB8RB5?B1'B49B7LB5?B:^B9XB8RB8RB6FB33B=qB<jB:^B9XB:^B9XB;dB;dB<jB=qB:^B9XB<jB<jB=qBB�BA�BC�BG�BJ�BN�BJ�BH�BF�BH�BH�BM�BO�BO�BO�BR�BR�BR�BVBT�BR�BP�BR�BXB[#BZBVB_;BffBjBk�Bk�Bk�Bk�Bk�BiyBffBhsBk�Bk�Bm�Bo�Bo�Bm�Bq�Bx�Bz�Bz�Bz�B{�By�B|�B}�B}�B}�B{�B� B�B�%B�DB�\B�bB�\B�{B��B��B��B�B�B�!B�-B�3B�LB�^B�qB�wB��BĜBȴBɺB��B��B��B�B�BB�`B�ZB�yB�B�B�B�B�B��B��B��B��B��B	B	1B	
=B	
=B	DB	PB	PB	VB	VB	PB	VB	\B	\B	\B	bB	bB	\B	VB	PB	hB	{B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	"�B	"�B	#�B	$�B	'�B	)�B	.B	.B	0!B	2-B	7LB	9XB	:^B	;dB	<jB	<jB	>wB	>wB	>wB	>wB	>wB	>wB	>wB	>wB	>wB	>wB	?}B	F�B	H�B	L�B	M�B	N�B	P�B	Q�B	R�B	YB	\)B	]/B	^5B	_;B	aHB	iyB	n�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	y�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�=B	�DB	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�-B	�?B	�FB	�RB	�dB	�dB	�jB	�qB	�wB	��B	ÖB	ĜB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�)B	�/B	�BB	�HB	�TB	�TB	�ZB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
1B
1B
+B
+B
%B
1B

=B

=B

=B
DB
DB
DB
PB
VB
\B
\B
\B
\B
\B
bB
\B
\B
\B
\B
bB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
8RB
7LB
7LB
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
<jB
;dB
;dB
=qB
=qB
<jB
;dB
=qB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
=qB
?}B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
B�B
C�B
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
F�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
J�B
I�B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
R�B
S�B
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
XB
XB
XB
XB
XB
ZB
YB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
^5B
^5B
`BB
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
bNB
bNB
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
dZB
dZB
e`B
e`B
e`B
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
gmB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
jB
iyB
jB
k�B
k�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�B	RB��B��B�TB�fB�B��B�`B�B�B,B)B��BΊB�BqABAoB%�B��B�B�B��B�$B��B�vBraBl=BgmB^BU�BN�BI7BB�B@B;JB5%B-]B#B�BKB�B{BUB
��B
�9B
��B
��B
ӏB
ϑB
�B
��B
�vB
�B
�2B
��B
�WB
��B
��B
�RB
�GB
|�B
wB
mwB
ezB
`BB
YKB
Q�B
F�B
;�B
.�B
(
B
!�B
/B
hB

�B
MB	��B	��B	��B	�[B	�cB	�B	�B	�IB	՛B	�(B	�7B	ªB	�wB	��B	��B	�=B	�fB	�|B	�;B	��B	��B	��B	�7B	�B	�4B	|�B	wB	r-B	m]B	i�B	a�B	[�B	UMB	RoB	OvB	I�B	?�B	3�B	/�B	.}B	+�B	'8B	!-B	VB	�B	�B	@B	4B	�B	�B	YB	�B	�B�.B�B�tB�)B�QB��B�"B�B�FB��B��B��B�?B�mBөBΊBɺB�lB��B�.B� B��B��B��B��B��B��B��B�*B��B�"B��B��B��B��B��B��B�qB�B��B��B��B�B�B�KB��B��B��B�xB��B��B��B��B�B�OB~�B}B{�B{�By�ByXBwLBvFBtTBo�Bh�Bi�Be�Bd�BhXBd&Be,BeBc B_BX�BIBR BY�BZkBZkBZ7BZkBXyBU�BT�BS�BTaBQ�BO�BP�BNpBKxBH�BG�B@OB<�B>�BDMB@�B@OBBB@BA�BCBCBEBD�BB�B@OB9�B/�B(XB1�B1�B0�B/�B/�B0�B9�B9�B9�B:�B=qB?�B@�B?B=<B9�B7B2�B5B7�B6B:�B9�B8�B8�B7fB4nB=�B<�B;B:*B;JB:xB<6B<jB=B>B;dB:xB=B=<B>]BB�BA�BDBHKBK^BO\BK�BI�BGEBIBI�BN�BPbBP�BP�BS[BSuBSuBVBUgBSuBQ�BS�BX�B[�BZ�BW?B_�Bf�Bj�Bk�Bk�Bk�Bk�Bk�Bi�BgBh�Bk�Bk�Bm�Bo�Bp!BnIBr-By	B{B{0B{JB|�Bz�B}VB~]B~BB~]B|�B��B�{B��B��B��B��B�HB�gB�kB�VB�ZB�WB�}B��B��B��B��B��B��B��B��B��B�B�XB� B�,B�gBٚB��B�zB��B�B�B�B��B��B�!B��B�B�B�B�]B	�B	KB	
rB	
�B	xB	jB	PB	pB	pB	jB	VB	\B	vB	vB	}B	bB	�B	�B	B	B	�B	�B	�B	�B	�B	B	�B	!�B	!�B	"�B	"�B	#B	#�B	%B	(>B	*eB	.IB	./B	0oB	2|B	7LB	9rB	:^B	;B	<�B	<�B	>wB	>wB	>wB	>wB	>]B	>�B	>�B	>wB	>�B	>�B	?�B	F�B	H�B	L�B	M�B	N�B	Q B	R B	S[B	YKB	\CB	]IB	^OB	_�B	a�B	i�B	n�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	y�B	}B	.B	cB	�[B	�9B	�9B	�3B	�MB	�3B	�gB	�mB	��B	��B	�jB	�<B	�pB	�VB	�pB	�pB	��B	��B	�TB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�5B	�'B	�GB	�GB	�GB	�aB	�ZB	�`B	�RB	�dB	�B	��B	��B	�wB	��B	ðB	ĶB	��B	��B	��B	��B	��B	��B	͹B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�+B	�B	�B	�1B	�B	�#B	�=B	�CB	�/B	�IB	�IB	�IB	�IB	�]B	�IB	�BB	�|B	�TB	�nB	�tB	�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�$B	�*B	�"B	�B	�B
 B
 B
 B
  B
B
 B
;B
'B
B
3B
B
3B
B
3B
3B
GB
MB
SB
mB
YB
1B
KB
	B

#B

XB
	RB
	RB
	RB
	RB
fB
KB
_B
zB
�B
fB

XB

XB

XB
^B
DB
xB
PB
VB
BB
BB
\B
\B
vB
}B
vB
vB
vB
\B
�B
�B
�B
{B
�B
�B
�B
yB
�B
�B
�B
�B
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
"�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&B
&B
&B
'B
'B
'�B
(
B
(
B
'B
($B
)B
)*B
)B
)B
*0B
*0B
*B
*B
*B
+B
+6B
,B
,B
,"B
,B
-)B
-B
./B
/B
/ B
/5B
/5B
0;B
0;B
1'B
1'B
1AB
1AB
2GB
33B
3MB
3MB
49B
6FB
6FB
6`B
6`B
6`B
7fB
7fB
7fB
8lB
7fB
7fB
8RB
88B
8lB
7fB
7�B
9rB
:xB
:xB
:xB
:xB
;dB
;B
;B
;dB
;B
<�B
;�B
;B
=qB
=VB
<jB
;�B
=�B
>wB
>�B
>�B
=�B
=�B
>�B
>�B
>�B
=�B
?�B
@�B
@�B
A�B
B�B
C{B
C�B
C�B
C�B
B�B
C�B
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
F�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J	B
KB
J	B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
P�B
Q B
P�B
O�B
Q B
Q�B
Q�B
RB
RB
Q B
PB
Q B
RB
SB
S�B
R�B
SB
S&B
S�B
TB
UB
VB
VB
U�B
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
X+B
XB
X+B
XEB
X+B
ZB
Y1B
Z7B
ZB
Z7B
Z7B
[=B
\)B
\)B
\CB
\)B
]/B
]B
]IB
]/B
\]B
\]B
]IB
^OB
^OB
`BB
`BB
`BB
`\B
`'B
`BB
`\B
aHB
abB
bNB
bNB
bNB
bNB
b4B
bNB
bNB
bNB
bhB
bNB
bhB
bhB
bhB
b�B
cnB
cnB
cnB
dtB
dtB
e`B
ezB
eFB
e`B
ezB
e`B
ffB
fLB
ffB
gmB
gRB
gmB
gmB
gmB
g�B
gmB
gmB
g�B
ffB
g�B
h�B
hsB
h�B
hsB
hsB
g�B
h�B
iyB
iyB
iyB
iyB
i�B
i�B
jB
jB
jB
iyB
j�B
i�B
jB
k�B
k�B
lqB
lq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712160035352017121600353520171216003535201806221234462018062212344620180622123446201804050430582018040504305820180405043058  JA  ARFMdecpA19c                                                                20171212093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171212003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171212003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171212003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171212003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171212003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171212003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171212003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171212003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171212003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20171212005541                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171212153111  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20171215153535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171215153535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193058  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033446  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                