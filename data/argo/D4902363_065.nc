CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-08T03:35:38Z creation;2016-12-08T03:35:41Z conversion to V3.1;2019-12-19T08:23:41Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161208033538  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA  I2_0576_065                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���q�� 1   @���)�� @:�e��O�d���}Vm1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=Cx
=Cz
=C|
=C~
=C�C�C��RC�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�D{DρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD��D�>D؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�%A�A�%A�%A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�
=A�%A�A�A�A�A�%A�A�%A�1A�A�  A��A��A��yA��A���A���A�  A�A�A�bA��A���AŰ!AđhA�ffA� �A��`A��7A��
A���A�1'A�bA�A�A��wA�33A��/A��\A�z�A�+A�~�A�$�A�&�A��!A���A�S�A��+A�hsA��jA���A�A�ZA�bNA��A�
=A��A�^5A�VA�A��A��A�-A�ƨA�ȴA�%A�E�A��#A�t�A��A�-A��A�9XA�r�A��jA�"�A�z�A���A�p�A�?}A��A�G�A�"�A�hsA��mA�n�A�n�A��A���A���A�1'A�33A��^A���AƨA~1A|�Az��AyO�Ax~�Aw"�Au�Au%At~�As��AsdZAr��Aq/ApbAn�uAl�9Ak�Akl�Aj�yAj5?Ah�Ag�#AgAe��Ae"�Ad��AdffAd �AcC�AbVA`��A`  A_\)A^n�A]p�A\jA[l�AZz�AZ�AY��AXv�AWx�AV�9AVjAV-AU��AU&�AS��AR~�AQ�AO�wAOoAMO�AJ^5AHbAG+AE��AD�!ACƨAB�`AAt�A@ �A>�A=��A<�/A<ZA< �A;�A:A�A9�;A9��A8�9A7;dA6�9A5��A4�A3\)A2ĜA1�7A/VA-O�A,��A,�A+��A*�A((�A'dZA&(�A$�A$��A$��A$��A$�uA$jA$(�A"��A =qA��A�hA?}A�A��A��A��A�A�A�PAv�A;dA��A�A�AVAjA��A�7A+AbNA�At�A��A�;Al�A��A=qA��A��AXA
�/A	C�A�#A�yA�A`BA�A�A��AdZA�AbAG�A ��A   @�@��y@���@��@�~�@���@�@�9X@�  @��@�-@�z�@��m@�F@�K�@�ȴ@���@�;d@�7@�(�@�"�@�M�@��@ޟ�@�X@��#@��@�S�@�V@պ^@ԃ@�l�@��@���@�M�@�p�@���@ЋD@�1'@�C�@�V@˾w@��@���@ʸR@�ȴ@ʸR@ʗ�@�~�@��@��y@�ff@Ł@Ý�@�+@�
=@�@�
=@�{@��@�  @�ff@��@�M�@���@��u@���@��\@�`B@�Q�@��@�/@�o@���@���@�$�@���@�I�@�v�@���@��j@�9X@��P@�+@��+@�@��7@�V@�z�@�(�@�A�@�I�@�t�@�33@�dZ@�ƨ@��+@��^@�O�@�O�@��j@�(�@�|�@�;d@�ff@�=q@��@�9X@�K�@���@��\@�$�@��@�p�@��@��!@���@���@��@�1'@�(�@��@���@���@��^@�G�@�7L@�&�@�V@��`@�j@�b@��;@�l�@�@���@�-@��@�@���@��h@�`B@�V@�z�@��@��;@�ƨ@��@��y@��R@���@���@�ff@��R@�o@���@���@��@�Ĝ@�Z@��@�K�@�
=@�+@�;d@�
=@�M�@�p�@�7L@�r�@�b@�|�@�l�@�dZ@�\)@�S�@�C�@�33@��@�ȴ@���@�^5@�E�@�@���@���@���@���@�@��^@���@��7@�hs@�X@�7L@��@��j@��@�Q�@�P@K�@+@�@~�y@~ȴ@~�R@~V@|�/@|9X@|��@{��@z�H@zn�@y��@y&�@y�@y�@y�@y%@x�`@x�`@x��@x��@x  @w�@w�;@w+@v�@v�@v��@v{@u��@u`B@t��@t��@s�F@s�@sS�@sS�@s33@r��@r^5@q��@q�7@qhs@qG�@q7L@q�@p��@pĜ@p��@pQ�@p1'@p1'@p  @o�w@o��@o
=@n�R@n��@n$�@m��@m��@m`B@m?}@l�@k�m@kƨ@kƨ@k��@kC�@k33@j��@jM�@jM�@i�^@ix�@h��@g�@g��@g�w@g�@g�w@g�@g��@g�P@gl�@g+@f�R@f5?@e��@e�-@e��@ep�@e/@d�@d�@dZ@d�@c��@cƨ@c"�@b�H@b~�@bM�@a�#@a��@aG�@a�@a%@`��@`��@`Ĝ@`�9@`bN@`Q�@_�;@_|�@_K�@^��@^��@^ff@^E�@]��@]O�@\��@\��@\9X@[�m@[�@[S�@Z�@Z�!@Zn�@ZM�@ZM�@Z�@Y��@Y�@Y��@Yx�@Y7L@X��@XĜ@Xr�@XA�@W�;@W�w@W�@W��@W�P@W+@V��@V�@V��@V5?@U��@Up�@T�@T�j@T��@T�D@Tz�@Tj@T�@SS�@R��@R~�@R�@Q��@Q��@QX@Q7L@P�`@PA�@O��@OK�@O;d@O+@Nȴ@N�+@NV@N@M@M��@Mp�@M`B@M/@M�@L��@L�@L�@L�/@Lz�@L9X@K�m@K��@K@J��@J-@I��@I�@I�#@I�7@IG�@I&�@I%@H��@H�9@H1'@G�P@G+@F��@F5?@F{@E�@E?}@D��@D�/@D�j@Cƨ@CS�@C33@C"�@B�@B��@B��@B-@Ax�@@Ĝ@@r�@?�@?�w@?�@?��@?l�@?�@>�@>v�@>$�@>@=�T@=�-@=p�@<�@<�/@<�@<z�@<j@<Z@<I�@<�@<1@;��@;o@:��@:~�@:M�@9�#@9��@9��@9��@9�7@9��@9hs@97L@8�`@8Q�@7�;@7\)@6�y@6�R@6�+@6v�@6v�@6v�@6v�@6v�@6v�@6ff@6ff@6E�@6{@5�T@5�-@5�h@5`B@5�@4�/@4�j@4�@4��@4j@4I�@4�@3��@3�
@3�
@3�F@3��@3t�@3S�@3C�@3"�@3@2�H@2�\@2=q@2J@1�@1�^@1X@0��@0�9@0�@0A�@0b@/��@/�P@/K�@.��@.�+@.ff@.ff@.ff@.ff@.5?@.@-�T@-�-@-��@-��@-�@-`B@,��@,�D@,�D@,z�@,z�@,j@,9X@,�@,1@+�
@+S�@*��@*M�@*-@*J@)�@)��@)7L@(��@(��@(�@(bN@'�;@'�@&�@&E�@&@%��@%�@%p�@%O�@$�@$�@$�D@$9X@#��@#�@#"�@"�!@"M�@"J@!x�@!G�@!%@ r�@K�@�@
=@
=@��@�@��@V@{@p�@/@�D@I�@(�@�@�m@�
@ƨ@t�@33@�H@�!@n�@M�@=q@-@��@�#@��@7L@��@Q�@A�@1'@ �@  @�@��@�w@�@�P@��@�P@|�@\)@K�@
=@�R@�R@��@��@��@��@�+@ff@ff@5?@{@{@�@@��@�@��@�D@I�@1@�m@ƨ@ƨ@�F@��@t�@dZ@S�@C�@33@"�@��@M�@J@��@�7@X@%@��@Ĝ@�9@��@r�@1'@�;@�P@|�@\)@;d@+@�@�@��@�y@�@�R@��@ff@{@�@��@@p�@V@�j@z�@Z@�@�
@S�@
�H@
��@
��@
�\@
�\@
~�@
^5@
-@	��@	��@	�@	�^@	��@	��@	��@	�7@	x�@	G�@	�@	%@��@�9@r�@1'@ �@�;@��@�P@K�@�y@ȴ@ȴ@ȴ@�R@��@��@��@�+@�+@�+@�+@�+@E�@@�@O�@�@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�%A�A�%A�%A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�
=A�%A�A�A�A�A�%A�A�%A�1A�A�  A��A��A��yA��A���A���A�  A�A�A�bA��A���AŰ!AđhA�ffA� �A��`A��7A��
A���A�1'A�bA�A�A��wA�33A��/A��\A�z�A�+A�~�A�$�A�&�A��!A���A�S�A��+A�hsA��jA���A�A�ZA�bNA��A�
=A��A�^5A�VA�A��A��A�-A�ƨA�ȴA�%A�E�A��#A�t�A��A�-A��A�9XA�r�A��jA�"�A�z�A���A�p�A�?}A��A�G�A�"�A�hsA��mA�n�A�n�A��A���A���A�1'A�33A��^A���AƨA~1A|�Az��AyO�Ax~�Aw"�Au�Au%At~�As��AsdZAr��Aq/ApbAn�uAl�9Ak�Akl�Aj�yAj5?Ah�Ag�#AgAe��Ae"�Ad��AdffAd �AcC�AbVA`��A`  A_\)A^n�A]p�A\jA[l�AZz�AZ�AY��AXv�AWx�AV�9AVjAV-AU��AU&�AS��AR~�AQ�AO�wAOoAMO�AJ^5AHbAG+AE��AD�!ACƨAB�`AAt�A@ �A>�A=��A<�/A<ZA< �A;�A:A�A9�;A9��A8�9A7;dA6�9A5��A4�A3\)A2ĜA1�7A/VA-O�A,��A,�A+��A*�A((�A'dZA&(�A$�A$��A$��A$��A$�uA$jA$(�A"��A =qA��A�hA?}A�A��A��A��A�A�A�PAv�A;dA��A�A�AVAjA��A�7A+AbNA�At�A��A�;Al�A��A=qA��A��AXA
�/A	C�A�#A�yA�A`BA�A�A��AdZA�AbAG�A ��A   @�@��y@���@��@�~�@���@�@�9X@�  @��@�-@�z�@��m@�F@�K�@�ȴ@���@�;d@�7@�(�@�"�@�M�@��@ޟ�@�X@��#@��@�S�@�V@պ^@ԃ@�l�@��@���@�M�@�p�@���@ЋD@�1'@�C�@�V@˾w@��@���@ʸR@�ȴ@ʸR@ʗ�@�~�@��@��y@�ff@Ł@Ý�@�+@�
=@�@�
=@�{@��@�  @�ff@��@�M�@���@��u@���@��\@�`B@�Q�@��@�/@�o@���@���@�$�@���@�I�@�v�@���@��j@�9X@��P@�+@��+@�@��7@�V@�z�@�(�@�A�@�I�@�t�@�33@�dZ@�ƨ@��+@��^@�O�@�O�@��j@�(�@�|�@�;d@�ff@�=q@��@�9X@�K�@���@��\@�$�@��@�p�@��@��!@���@���@��@�1'@�(�@��@���@���@��^@�G�@�7L@�&�@�V@��`@�j@�b@��;@�l�@�@���@�-@��@�@���@��h@�`B@�V@�z�@��@��;@�ƨ@��@��y@��R@���@���@�ff@��R@�o@���@���@��@�Ĝ@�Z@��@�K�@�
=@�+@�;d@�
=@�M�@�p�@�7L@�r�@�b@�|�@�l�@�dZ@�\)@�S�@�C�@�33@��@�ȴ@���@�^5@�E�@�@���@���@���@���@�@��^@���@��7@�hs@�X@�7L@��@��j@��@�Q�@�P@K�@+@�@~�y@~ȴ@~�R@~V@|�/@|9X@|��@{��@z�H@zn�@y��@y&�@y�@y�@y�@y%@x�`@x�`@x��@x��@x  @w�@w�;@w+@v�@v�@v��@v{@u��@u`B@t��@t��@s�F@s�@sS�@sS�@s33@r��@r^5@q��@q�7@qhs@qG�@q7L@q�@p��@pĜ@p��@pQ�@p1'@p1'@p  @o�w@o��@o
=@n�R@n��@n$�@m��@m��@m`B@m?}@l�@k�m@kƨ@kƨ@k��@kC�@k33@j��@jM�@jM�@i�^@ix�@h��@g�@g��@g�w@g�@g�w@g�@g��@g�P@gl�@g+@f�R@f5?@e��@e�-@e��@ep�@e/@d�@d�@dZ@d�@c��@cƨ@c"�@b�H@b~�@bM�@a�#@a��@aG�@a�@a%@`��@`��@`Ĝ@`�9@`bN@`Q�@_�;@_|�@_K�@^��@^��@^ff@^E�@]��@]O�@\��@\��@\9X@[�m@[�@[S�@Z�@Z�!@Zn�@ZM�@ZM�@Z�@Y��@Y�@Y��@Yx�@Y7L@X��@XĜ@Xr�@XA�@W�;@W�w@W�@W��@W�P@W+@V��@V�@V��@V5?@U��@Up�@T�@T�j@T��@T�D@Tz�@Tj@T�@SS�@R��@R~�@R�@Q��@Q��@QX@Q7L@P�`@PA�@O��@OK�@O;d@O+@Nȴ@N�+@NV@N@M@M��@Mp�@M`B@M/@M�@L��@L�@L�@L�/@Lz�@L9X@K�m@K��@K@J��@J-@I��@I�@I�#@I�7@IG�@I&�@I%@H��@H�9@H1'@G�P@G+@F��@F5?@F{@E�@E?}@D��@D�/@D�j@Cƨ@CS�@C33@C"�@B�@B��@B��@B-@Ax�@@Ĝ@@r�@?�@?�w@?�@?��@?l�@?�@>�@>v�@>$�@>@=�T@=�-@=p�@<�@<�/@<�@<z�@<j@<Z@<I�@<�@<1@;��@;o@:��@:~�@:M�@9�#@9��@9��@9��@9�7@9��@9hs@97L@8�`@8Q�@7�;@7\)@6�y@6�R@6�+@6v�@6v�@6v�@6v�@6v�@6v�@6ff@6ff@6E�@6{@5�T@5�-@5�h@5`B@5�@4�/@4�j@4�@4��@4j@4I�@4�@3��@3�
@3�
@3�F@3��@3t�@3S�@3C�@3"�@3@2�H@2�\@2=q@2J@1�@1�^@1X@0��@0�9@0�@0A�@0b@/��@/�P@/K�@.��@.�+@.ff@.ff@.ff@.ff@.5?@.@-�T@-�-@-��@-��@-�@-`B@,��@,�D@,�D@,z�@,z�@,j@,9X@,�@,1@+�
@+S�@*��@*M�@*-@*J@)�@)��@)7L@(��@(��@(�@(bN@'�;@'�@&�@&E�@&@%��@%�@%p�@%O�@$�@$�@$�D@$9X@#��@#�@#"�@"�!@"M�@"J@!x�@!G�@!%@ r�@K�@�@
=@
=@��@�@��@V@{@p�@/@�D@I�@(�@�@�m@�
@ƨ@t�@33@�H@�!@n�@M�@=q@-@��@�#@��@7L@��@Q�@A�@1'@ �@  @�@��@�w@�@�P@��@�P@|�@\)@K�@
=@�R@�R@��@��@��@��@�+@ff@ff@5?@{@{@�@@��@�@��@�D@I�@1@�m@ƨ@ƨ@�F@��@t�@dZ@S�@C�@33@"�@��@M�@J@��@�7@X@%@��@Ĝ@�9@��@r�@1'@�;@�P@|�@\)@;d@+@�@�@��@�y@�@�R@��@ff@{@�@��@@p�@V@�j@z�@Z@�@�
@S�@
�H@
��@
��@
�\@
�\@
~�@
^5@
-@	��@	��@	�@	�^@	��@	��@	��@	�7@	x�@	G�@	�@	%@��@�9@r�@1'@ �@�;@��@�P@K�@�y@ȴ@ȴ@ȴ@�R@��@��@��@�+@�+@�+@�+@�+@E�@@�@O�@�@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBBBBBBBBBBBBBBBBBBBBBBBB%B%B1B
=BDB\BuB{B�B�B�B,B9XB;dB:^B7LB5?B,B�B�B��B��B�B�B�fB�)B��BĜB�LB��B��B�1Bt�BhsBYBK�BE�B?}BD�BT�B^5BN�BB�B6FB(�B#�B�B�B{BVBB�B�mB��BƨB�RB�B��B��B��B��B��B�{B�VB�B}�Bs�Bm�Be`BaHB]/BVBD�B9XB1'B(�B�BVBB
��B
�B
�)B
��B
ȴB
�B
��B
��B
�DB
|�B
v�B
m�B
dZB
]/B
XB
T�B
O�B
M�B
@�B
9XB
-B
!�B
�B
�B
oB
PB
B	��B	��B	�B	�B	�yB	�mB	�`B	�HB	�#B	��B	��B	ǮB	B	�jB	�?B	�!B	��B	��B	��B	��B	��B	�oB	�bB	�VB	�JB	�7B	�B	z�B	u�B	l�B	ffB	^5B	Q�B	A�B	<jB	6FB	/B	,B	$�B	 �B	�B	oB	VB	
=B	1B	%B	B	  B��B��B��B�B�B�`B�)B�B�)B�B��BĜBB�wB�jB�FB��B��B��B��B��B��B��B��B��B��B��B�%B�1B�JB�DB�DB�DB�=B�=B�=B�7B�JB�+B�Bx�Br�Bl�BgmBdZBbNBbNBbNB`BB_;B^5B[#BXBW
BVBT�BP�BM�BJ�BI�BI�BD�BB�BA�BB�BA�B?}B=qB=qB<jB>wB=qB:^B8RB33B33B/B/B-B+B+B%�B$�B#�B"�B"�B!�B �B �B!�B#�B%�B$�B#�B!�B!�B�B�B�B�B{B�B�B�B�B�B{BuBuB{BuBuBuBuB�B�B�B�B�B�B�B�B�B�B�B{B�B�B"�B&�B'�B,B5?B6FB8RB9XB9XB=qB<jB?}B?}B=qB:^B8RB,B+B1'B1'B/B1'B2-B2-B9XB8RB:^B<jB=qB<jB>wB?}BA�BC�BE�BM�BR�BbNBcTBjBo�Bu�Bx�Bv�Bw�B|�B�B�%B�%B�1B�DB�VB�VB�uB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�FB�XB�jB�}BBÖBŢBƨBƨBɺB��B�B�B�B�B�)B�/B�5B�NB�fB�B�B��B��B��B��B	B	B	B	B	B	+B	DB	JB	PB	JB	PB	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	)�B	+B	+B	.B	1'B	1'B	1'B	2-B	49B	9XB	A�B	C�B	C�B	C�B	C�B	D�B	E�B	G�B	I�B	I�B	J�B	M�B	N�B	N�B	N�B	N�B	P�B	VB	XB	\)B	^5B	aHB	dZB	gmB	iyB	hsB	iyB	iyB	jB	l�B	p�B	q�B	r�B	s�B	u�B	y�B	}�B	~�B	~�B	�B	�B	�%B	�+B	�1B	�7B	�JB	�PB	�VB	�VB	�\B	�bB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�-B	�3B	�9B	�FB	�LB	�^B	�dB	�qB	�wB	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
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
JB
JB
PB
VB
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
hB
hB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
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
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
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
`BB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBBBBBBBBBBBBBBBBBBBBBBB9B%B?BKB
=BDB\BuB{B�B�B�B,WB:xB=�B<�B:*B8lB0!B%BCB B�0B�FB�+B��BߊB� BǔB�B�CB�;B�)BwLBkkBZ�BMPBG�BA�BF�BW?B`�BQ�BD�B8B*0B$�BBBBbB�B�+B�eB�MB�fB��B�;B�B�LB��B�!B��B�SB��B�gBcBt�Bn�BfBbhB_BX_BFYB:�B2�B+QB�BbB�B
��B
�B
��B
ԯB
̘B
�UB
�|B
��B
�B
~BB
xRB
n�B
e`B
]�B
X�B
U�B
Q B
O�B
BB
;0B
/ B
"�B
WB
YB
�B
B
B	�.B	�0B	�UB	�/B	��B	��B	�B	�B	ܬB	�B	��B	��B	��B	��B	�zB	�B	��B	��B	�`B	��B	�YB	��B	��B	��B	��B	��B	��B	|�B	wfB	m�B	h�B	a�B	T{B	CB	>BB	7�B	0UB	-CB	&�B	"�B	]B	�B	BB	
�B	�B	+B	tB	 �B��B�PB�rB�B�)B�B�dB�kB�5B�BҽBŢB�aB��B��B�lB�KB��B�TB��B��B��B��B�B�B�B�B��B��B��B��B�xB�xB�XB��B��B��B��B��B��BzDBt�BncBi�BeBb�BcBcTB`�B`B_VB\)BX�BW�BW$BVBRTBO\BK�BK�BKxBE�BC�BB�BCGBB�B@B>B>]B=�B?�B>wB;�B9�B5%B4nB0B0!B./B-]B+�B&LB%�B$�B#�B#:B"B!-B!|B# B%B'B%�B$�B"�B"�B!bB�B�B�B�B?BBsBB�B�B�B�B�B�B�BFB�BmB
B�B�B�B�B�BB�B�BBgB�BB#B'B(>B,�B5�B7B9$B9�B9�B>BB<�B@OB@iB>wB;B9�B,�B,�B2|B1�B0UB2-B2�B3hB9�B8�B:�B<�B=�B<�B>�B?�BA�BC�BE�BM�BS@Bb�Bc�BjBo�Bv�ByXBwBxB}VB��B��B��B��B��B��B�vB�B��B��B��B��B�+B��B�_B�9B�B��B��B��B�B�'B�HB��B�KB�"B�B�IB�IB�iB�vB�nB��B��B��B��B��B��BżBƨB��B�#B�BB�SB�B�7B�kBܒB�dB�OB�hB�fB�kB�B��B�+B��B�BB	UB	{B	{B	3B	B	EB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	%B	'B	*0B	+B	+6B	.IB	1'B	1'B	1AB	2-B	49B	9rB	A�B	C�B	C�B	C�B	C�B	D�B	E�B	G�B	J	B	I�B	J�B	M�B	N�B	N�B	N�B	OB	QhB	VB	XB	\�B	^�B	a|B	d�B	g�B	iyB	h�B	iyB	iyB	j�B	l�B	p�B	q�B	r�B	s�B	u�B	zB	~B	B	.B	�GB	�SB	�?B	�EB	�fB	��B	�JB	�jB	�VB	�pB	�vB	�}B	�}B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	�B	�WB	�/B	�5B	�;B	�-B	�GB	�aB	�hB	�9B	�zB	�fB	��B	��B	��B	�wB	��B	��B	��B	B	ðB	ÖB	ĶB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�,B	�B	�?B	�+B	�EB	�1B	�7B	�=B	�)B	�)B	�/B	�/B	�5B	�OB	�VB	�pB	�HB	�NB	�nB	�tB	�zB	�fB	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�(B	�.B
B
 B
B
B
'B
 B
UB
GB
GB
3B
9B
%B
EB
EB
_B
_B
KB
	7B

XB

XB

XB
DB
dB
dB
jB
pB
VB
pB
pB
\B
vB
\B
\B
vB
�B
}B
hB
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
"�B
$B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
'B
'B
'B
(
B
($B
)B
(�B
(�B
)�B
)�B
*B
)�B
*B
*0B
+6B
+6B
,"B
,"B
-CB
-)B
-�B
.B
-�B
.B
./B
./B
.IB
/OB
0UB
0UB
1[B
2GB
2GB
2-B
2-B
2B
2-B
2-B
2B
2-B
2B
2GB
3MB
3MB
3MB
3MB
4TB
4TB
4TB
4TB
5?B
5ZB
5ZB
5ZB
5?B
5ZB
6FB
6+B
6`B
6FB
6`B
6`B
6`B
7fB
7LB
7fB
7fB
7LB
8lB
8RB
8lB
8�B
9rB
9rB
9rB
:^B
:xB
:xB
;dB
;B
;B
<�B
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>�B
>�B
?�B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
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
K�B
K�B
MB
MB
M�B
NB
N�B
OB
O(B
P.B
Q�B
Q�B
Q�B
RB
RB
Q�B
SB
SB
SB
S�B
T,B
UB
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
XB
X+B
W�B
X+B
X+B
X+B
Y1B
YKB
YKB
Z7B
ZB
ZB
ZB
Z7B
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
[=B
\CB
\)B
\)B
\)B
\)B
\)B
\CB
]/B
]B
]/B
]/B
]/B
]/B
]IB
]IB
]dB
^OB
^OB
_;B
_VB
_VB
_VB
`'B
`BB
`\B
`\B
`'B
`BB
`BB
`\B
`\B
`\B
abB
aHB
bhB
bhB
bNB
bNB
cnB
cTB
cTB
cnB
cTB
cTB
dtB
dZB
dZB
dtB
dtB
e`B
e`B
e`B
ezB
eFB
e`B
ezB
ezB
e`B
f�B
f�B
f�B
ffB
f�B
g�B
gmB
h�B
hsB
h�B
hsB
h�B
i�B
jB
j�B
jB
jB
jB
j�B
j�B
j�B
jB
jB
j�B
k�B
k�B
kkB
kkB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n}B
n�B
n�B
n�B
n}B
n}B
n}B
n}B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612120032242016121200322420161212003224201806221217572018062212175720180622121757201804050411022018040504110220180405041102  JA  ARFMdecpA19c                                                                20161208123505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161208033538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161208033539  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161208033539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161208033540  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161208033540  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161208033540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161208033540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161208033540  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161208033541                      G�O�G�O�G�O�                JA  ARUP                                                                        20161208044249                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161208153553  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161211153224  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161211153224  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191102  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031757  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                