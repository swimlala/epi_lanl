CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-18T00:35:14Z creation;2017-12-18T00:35:18Z conversion to V3.1;2019-12-19T07:54:00Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171218003514  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_190                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�=���5 1   @�=�����@;�ڹ�Y��dY1���1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C/�C2
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
=CH#�CJ
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
=Cs�Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׄ{D��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�>D�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�7LA�7LA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�9XA�33A�&�A�bA��-A�G�A��;A��A�S�A��A�r�A��PA��hA��FA���A�JA���A���A��A���A�p�A��A�p�A�t�A�n�A��\A���A���A�33A�O�A�K�A���A���A�$�A��A�Q�A�A�n�A��A��A�"�A��uA�;dA�A��\A�9XA��mA�hsA���A�\)A�ZA�XA�O�A�33A��DA��A~�jA}�-A|bNA{dZAz�AxĜAx{Av��Au�Aup�Au&�AuoAtz�AtJAsXAr��Ar$�Ap�DAo33Am��Am;dAlQ�AkK�Ah��Af~�Ae��Ae\)AdVAb�Aa%A`9XA_�A_33A]��A[�AY��AW�AW|�AW�AV�AT��AS�mAS�FAS�hAR��AQ��AQdZAP�AO��AO&�ANĜAN=qAM��AM?}AL�9AL1'AK�^AK+AJ��AJJAI�AIp�AH�RAH{AG�AE�ADA�AC��AB��AA�#A@�A?�A?S�A>5?A<�/A;��A:�\A:9XA9��A9S�A8�A7&�A6ffA5��A5�-A5��A5t�A5?}A4��A4�!A2��A1�A1A01A.��A-x�A-
=A,�A,�A,=qA,JA+��A+O�A*^5A)��A(�A'�
A'x�A'A&�\A%�TA%"�A$�A#�^A"�A"��A"JA!hsA!A �A7LA��Ap�A�uA�FA/A��Ar�A�A��A�A+Ar�A�A33AVA�uA�;Ax�A&�A�jAE�A�A/A&�A�A
�HAVAA�A�^AoA��AQ�AA�mA�-A+A�HA�!A�AZAI�A�;A ȴA A�@��
@���@�~�@���@���@�;d@� �@�-@�P@�+@��@�Z@�h@�@�I�@�33@��@�|�@�9X@݁@�33@�ȴ@ڰ!@�~�@�=q@١�@��@�z�@�S�@ְ!@�E�@�{@��#@��@д9@�+@�(�@���@��H@ȼj@�K�@Ų-@�`B@�Ĝ@�(�@�S�@��@�
=@�@���@���@��@¸R@+@�5?@���@��@�9X@�@�{@� �@��@��P@�{@��h@�`B@�x�@�G�@�\)@��@���@��@��;@���@��@�|�@�{@�  @�~�@���@���@���@��@�`B@��j@�A�@�o@��#@���@��j@��@���@��@���@���@��u@�C�@��@�O�@��D@��@�l�@���@�~�@�ff@�^5@�=q@��-@�V@���@�dZ@�=q@���@��h@�X@���@��9@�r�@��@���@��@�n�@��h@�V@��`@���@�Ĝ@��@�j@�b@�dZ@�J@��F@�dZ@�o@��H@���@���@���@���@��\@�^5@��@��-@���@��h@��h@�hs@��/@�j@� �@��
@�l�@�+@��!@�E�@�/@��D@��@��m@��w@�S�@���@��h@�x�@�X@���@�j@�(�@�b@��;@�dZ@�ff@��@��@�{@���@��@��@��@��@��#@��-@�hs@�7L@�Ĝ@�1'@�;@��@+@}��@}`B@}/@|�/@|�@|Z@|(�@{�m@{ƨ@{��@{C�@z��@z^5@z-@y��@yhs@yX@yX@y7L@y&�@x��@x�`@xĜ@xĜ@x�u@x �@w�;@w�w@v��@v@v@v5?@v$�@u@up�@u�@t�@t��@tI�@t�@t1@t1@s��@s�
@s��@sS�@so@s@r�H@r�\@r=q@r-@r�@r�@q�#@q�^@q�^@q��@qX@q&�@q�@q7L@qG�@qX@qhs@qx�@qx�@q�7@q�7@q%@pĜ@pĜ@p��@p��@pbN@pr�@pA�@pb@p  @o�@o+@nv�@m��@l��@lZ@k��@i�^@i�#@i��@iX@i&�@i�@h��@h�9@hbN@hA�@g�@g�P@g+@f�y@f��@fff@f@e��@e/@d�j@d��@d��@d�D@dj@c�@b^5@a�#@a��@ahs@aX@aG�@a�@`��@_|�@^�+@^ff@^V@^$�@]@]?}@\�@[��@[S�@Z�@Z�@Z��@Z��@Z�\@Z�@Y��@Y�@Y��@YG�@X��@X�u@Xr�@W�;@W�w@Wl�@V��@Vȴ@V�R@Vv�@V5?@U�@U@U��@Up�@T�@T�D@TZ@T(�@T�@S��@So@R^5@R-@Q�@Q�^@Q&�@P�`@PĜ@P�9@O�;@O�P@N��@N�+@Nff@NV@N5?@N{@M�@M�T@M��@M�h@L�@L�@L(�@K�
@K�@J�H@J�!@J�\@J~�@J~�@J~�@JM�@JJ@HĜ@Hb@H  @Hb@G�@G��@G;d@F�R@F�+@F��@F�@F�@Fv�@EV@D��@D��@D��@D��@D��@D��@DI�@Cƨ@C�F@C��@Ct�@Ct�@CdZ@CS�@B�@B��@A��@A�#@A�^@A��@AX@A%@@�u@@  @?��@?�P@?K�@>�@>ff@=��@=?}@=V@<�@<��@<�@<��@<j@;�m@;dZ@:J@9�^@9G�@8bN@8bN@8Q�@8Q�@7�@7�;@7�@7��@7;d@7
=@6�y@6�@6ȴ@6�R@6ff@5�T@5��@5p�@5�@4�D@49X@3��@3�m@3�m@3��@41@41@3��@3t�@2�H@2~�@2=q@2=q@2=q@2=q@2-@1��@1�@1��@1x�@1G�@0�`@0Ĝ@0�@0Q�@01'@/�@/��@/�P@/+@.5?@-?}@,��@,�@,j@,I�@,9X@,9X@,(�@+�F@+"�@+33@*��@*^5@*-@*J@)�@)��@(Ĝ@(  @'�w@'��@'|�@'K�@&��@&��@&E�@&$�@&{@%�T@%��@%�@%�@$�/@$��@$��@$�j@$�@$Z@#��@#�F@#��@#�@#t�@#C�@#@"�!@"�\@"n�@"M�@"M�@"J@!��@!7L@!&�@!&�@ ��@ Ĝ@ ��@ bN@  �@��@;d@�y@E�@$�@��@`B@�@��@��@��@��@�j@�@z�@I�@9X@9X@�@�F@��@��@��@t�@"�@�@��@��@�\@M�@��@��@��@x�@��@r�@ �@  @�@��@l�@K�@��@v�@E�@$�@�@{@�T@��@O�@��@�j@�D@j@Z@9X@ƨ@��@��@S�@33@�@��@-@�@�#@��@x�@hs@hs@hs@X@G�@&�@�`@�u@�@r�@b@�@l�@;d@+@+@��@�R@��@��@�+@�+@E�@5?@$�@{@�@��@��@`B@�@�/@�D@z�@9X@�@��@�
@�F@�@S�@C�@33@"�@
��@
�!@
��@
��@
�\@
�\@
�\@
�\@
~�@
~�@
~�@
~�@
~�@
~�@
^5@
=q@	�@	�#@	�#@	�7@	X@	X@	hs@	G�@	7L@	%@��@�9@bN@A�@1'@  @�;@�@+@�@��@�@��@ff@V@E�@$�@�@��@@�h@p�@?}@�@�@�@�@�@��@Z@��@ƨ@��@t�@dZ@S�@S�@S�@S�@C�@C�@33@33@"�@��@n�@J@��@��@�#@��@�^@��@hs@G�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�7LA�7LA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�9XA�33A�&�A�bA��-A�G�A��;A��A�S�A��A�r�A��PA��hA��FA���A�JA���A���A��A���A�p�A��A�p�A�t�A�n�A��\A���A���A�33A�O�A�K�A���A���A�$�A��A�Q�A�A�n�A��A��A�"�A��uA�;dA�A��\A�9XA��mA�hsA���A�\)A�ZA�XA�O�A�33A��DA��A~�jA}�-A|bNA{dZAz�AxĜAx{Av��Au�Aup�Au&�AuoAtz�AtJAsXAr��Ar$�Ap�DAo33Am��Am;dAlQ�AkK�Ah��Af~�Ae��Ae\)AdVAb�Aa%A`9XA_�A_33A]��A[�AY��AW�AW|�AW�AV�AT��AS�mAS�FAS�hAR��AQ��AQdZAP�AO��AO&�ANĜAN=qAM��AM?}AL�9AL1'AK�^AK+AJ��AJJAI�AIp�AH�RAH{AG�AE�ADA�AC��AB��AA�#A@�A?�A?S�A>5?A<�/A;��A:�\A:9XA9��A9S�A8�A7&�A6ffA5��A5�-A5��A5t�A5?}A4��A4�!A2��A1�A1A01A.��A-x�A-
=A,�A,�A,=qA,JA+��A+O�A*^5A)��A(�A'�
A'x�A'A&�\A%�TA%"�A$�A#�^A"�A"��A"JA!hsA!A �A7LA��Ap�A�uA�FA/A��Ar�A�A��A�A+Ar�A�A33AVA�uA�;Ax�A&�A�jAE�A�A/A&�A�A
�HAVAA�A�^AoA��AQ�AA�mA�-A+A�HA�!A�AZAI�A�;A ȴA A�@��
@���@�~�@���@���@�;d@� �@�-@�P@�+@��@�Z@�h@�@�I�@�33@��@�|�@�9X@݁@�33@�ȴ@ڰ!@�~�@�=q@١�@��@�z�@�S�@ְ!@�E�@�{@��#@��@д9@�+@�(�@���@��H@ȼj@�K�@Ų-@�`B@�Ĝ@�(�@�S�@��@�
=@�@���@���@��@¸R@+@�5?@���@��@�9X@�@�{@� �@��@��P@�{@��h@�`B@�x�@�G�@�\)@��@���@��@��;@���@��@�|�@�{@�  @�~�@���@���@���@��@�`B@��j@�A�@�o@��#@���@��j@��@���@��@���@���@��u@�C�@��@�O�@��D@��@�l�@���@�~�@�ff@�^5@�=q@��-@�V@���@�dZ@�=q@���@��h@�X@���@��9@�r�@��@���@��@�n�@��h@�V@��`@���@�Ĝ@��@�j@�b@�dZ@�J@��F@�dZ@�o@��H@���@���@���@���@��\@�^5@��@��-@���@��h@��h@�hs@��/@�j@� �@��
@�l�@�+@��!@�E�@�/@��D@��@��m@��w@�S�@���@��h@�x�@�X@���@�j@�(�@�b@��;@�dZ@�ff@��@��@�{@���@��@��@��@��@��#@��-@�hs@�7L@�Ĝ@�1'@�;@��@+@}��@}`B@}/@|�/@|�@|Z@|(�@{�m@{ƨ@{��@{C�@z��@z^5@z-@y��@yhs@yX@yX@y7L@y&�@x��@x�`@xĜ@xĜ@x�u@x �@w�;@w�w@v��@v@v@v5?@v$�@u@up�@u�@t�@t��@tI�@t�@t1@t1@s��@s�
@s��@sS�@so@s@r�H@r�\@r=q@r-@r�@r�@q�#@q�^@q�^@q��@qX@q&�@q�@q7L@qG�@qX@qhs@qx�@qx�@q�7@q�7@q%@pĜ@pĜ@p��@p��@pbN@pr�@pA�@pb@p  @o�@o+@nv�@m��@l��@lZ@k��@i�^@i�#@i��@iX@i&�@i�@h��@h�9@hbN@hA�@g�@g�P@g+@f�y@f��@fff@f@e��@e/@d�j@d��@d��@d�D@dj@c�@b^5@a�#@a��@ahs@aX@aG�@a�@`��@_|�@^�+@^ff@^V@^$�@]@]?}@\�@[��@[S�@Z�@Z�@Z��@Z��@Z�\@Z�@Y��@Y�@Y��@YG�@X��@X�u@Xr�@W�;@W�w@Wl�@V��@Vȴ@V�R@Vv�@V5?@U�@U@U��@Up�@T�@T�D@TZ@T(�@T�@S��@So@R^5@R-@Q�@Q�^@Q&�@P�`@PĜ@P�9@O�;@O�P@N��@N�+@Nff@NV@N5?@N{@M�@M�T@M��@M�h@L�@L�@L(�@K�
@K�@J�H@J�!@J�\@J~�@J~�@J~�@JM�@JJ@HĜ@Hb@H  @Hb@G�@G��@G;d@F�R@F�+@F��@F�@F�@Fv�@EV@D��@D��@D��@D��@D��@D��@DI�@Cƨ@C�F@C��@Ct�@Ct�@CdZ@CS�@B�@B��@A��@A�#@A�^@A��@AX@A%@@�u@@  @?��@?�P@?K�@>�@>ff@=��@=?}@=V@<�@<��@<�@<��@<j@;�m@;dZ@:J@9�^@9G�@8bN@8bN@8Q�@8Q�@7�@7�;@7�@7��@7;d@7
=@6�y@6�@6ȴ@6�R@6ff@5�T@5��@5p�@5�@4�D@49X@3��@3�m@3�m@3��@41@41@3��@3t�@2�H@2~�@2=q@2=q@2=q@2=q@2-@1��@1�@1��@1x�@1G�@0�`@0Ĝ@0�@0Q�@01'@/�@/��@/�P@/+@.5?@-?}@,��@,�@,j@,I�@,9X@,9X@,(�@+�F@+"�@+33@*��@*^5@*-@*J@)�@)��@(Ĝ@(  @'�w@'��@'|�@'K�@&��@&��@&E�@&$�@&{@%�T@%��@%�@%�@$�/@$��@$��@$�j@$�@$Z@#��@#�F@#��@#�@#t�@#C�@#@"�!@"�\@"n�@"M�@"M�@"J@!��@!7L@!&�@!&�@ ��@ Ĝ@ ��@ bN@  �@��@;d@�y@E�@$�@��@`B@�@��@��@��@��@�j@�@z�@I�@9X@9X@�@�F@��@��@��@t�@"�@�@��@��@�\@M�@��@��@��@x�@��@r�@ �@  @�@��@l�@K�@��@v�@E�@$�@�@{@�T@��@O�@��@�j@�D@j@Z@9X@ƨ@��@��@S�@33@�@��@-@�@�#@��@x�@hs@hs@hs@X@G�@&�@�`@�u@�@r�@b@�@l�@;d@+@+@��@�R@��@��@�+@�+@E�@5?@$�@{@�@��@��@`B@�@�/@�D@z�@9X@�@��@�
@�F@�@S�@C�@33@"�@
��@
�!@
��@
��@
�\@
�\@
�\@
�\@
~�@
~�@
~�@
~�@
~�@
~�@
^5@
=q@	�@	�#@	�#@	�7@	X@	X@	hs@	G�@	7L@	%@��@�9@bN@A�@1'@  @�;@�@+@�@��@�@��@ff@V@E�@$�@�@��@@�h@p�@?}@�@�@�@�@�@��@Z@��@ƨ@��@t�@dZ@S�@S�@S�@S�@C�@C�@33@33@"�@��@n�@J@��@��@�#@��@�^@��@hs@G�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B#�B"�B!�B �B�B�BVB+BB��B�fB�qB�DBgmB-B�yB�RB�{B�+B�B�Bx�Bo�B`BBT�BN�BD�B<jB0!B$�BJB%B
��B
��B
�B
�sB
�B
�B
�B
�fB
�HB
�B
��B
ɺB
��B
ƨB
��B
�^B
�-B
�B
�B
�B
�B
��B
��B
�{B
�VB
�+B
� B
w�B
q�B
gmB
dZB
\)B
XB
VB
R�B
Q�B
M�B
J�B
E�B
A�B
;dB
/B
)�B
�B
�B
�B
PB
  B	�B	�B	�B	�TB	�B	��B	��B	ȴB	��B	�LB	��B	��B	��B	��B	��B	�PB	�+B	�B	�B	�B	{�B	u�B	t�B	p�B	jB	gmB	gmB	dZB	bNB	_;B	\)B	YB	W
B	R�B	P�B	L�B	J�B	H�B	C�B	=qB	7LB	.B	(�B	#�B	�B	�B	{B	oB	VB	+B	  B��B��B��B�B�B�B�`B�`B�`B�`B�fB�fB�`B�TB�;B�B��B��B��BɺBĜBŢBƨBŢBĜBÖB��B�}B�^B�FB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�B�B� B~�B~�B}�B|�B|�B{�Bz�Bx�Bw�Bw�Bs�Bk�BjBjBiyBffBbNBYBN�B\)B[#BXBM�BH�BN�BN�BM�BM�BL�BL�BK�BJ�BK�BL�BL�BK�BJ�BG�BC�BE�BD�BA�B9XB=qB?}B<jB6FB9XB:^B;dB:^B7LB0!B33B33B/B+B'�B$�B(�B-B5?B7LB6FB6FB49B5?B5?B49B6FB7LB7LB6FB1'B)�B2-B0!B9XB6FB0!B7LB9XBA�BB�BC�BD�BH�BJ�BK�BK�BK�BK�BK�BM�BM�BM�BL�BL�BI�BF�BC�B@�B@�BE�BM�BP�BR�BQ�BN�BP�BT�BW
BYB\)B\)BZBT�BS�B\)B`BBffBgmBhsBjBo�Bu�Bw�Bv�Bv�Bv�B{�B~�B� B� B� B~�B|�B{�B~�B}�B�B�B�%B�DB�\B�hB�bB�VB�\B�bB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�XBŢBƨBɺB��B��B��B��B��B��B��B��B�B�B�
B�B�B�NB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	1B	1B	1B	
=B	bB	uB	{B	{B	�B	 �B	#�B	#�B	#�B	%�B	&�B	&�B	&�B	'�B	'�B	)�B	.B	/B	33B	9XB	;dB	;dB	<jB	E�B	F�B	G�B	H�B	I�B	I�B	J�B	J�B	J�B	J�B	K�B	L�B	N�B	P�B	T�B	W
B	W
B	XB	XB	YB	ZB	ZB	[#B	[#B	\)B	_;B	aHB	aHB	dZB	hsB	hsB	jB	l�B	m�B	o�B	p�B	q�B	r�B	t�B	u�B	u�B	u�B	u�B	u�B	v�B	w�B	y�B	y�B	y�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�7B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�FB	�FB	�^B	�wB	��B	ĜB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�)B	�#B	�B	�)B	�;B	�BB	�BB	�;B	�BB	�NB	�NB	�ZB	�`B	�mB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
1B

=B

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
PB
hB
hB
hB
uB
uB
uB
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
+B
,B
,B
,B
,B
,B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
0!B
2-B
33B
33B
49B
49B
49B
49B
49B
33B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
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
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
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
E�B
F�B
F�B
H�B
H�B
I�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
M�B
N�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
XB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
\)B
]/B
^5B
^5B
^5B
_;B
^5B
_;B
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
_;B
_;B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
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
dZB
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
dZB
e`B
dZB
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
hsB
gmB
gmB
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
jB
k�B
k�B
k�B
k�B
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B#�B"�B!�B!-B~BsBBB�BB��B�B��B��BlqB4�B�B�B��B�#B��B��B{JBr�BdZBW?BP�BF�B>�B1�B'�BHB�B
��B
��B
�B
��B
��B
�B
��B
�mB
�hB
��B
�}B
ʦB
�JB
�zB
�oB
��B
��B
��B
� B
�/B
�=B
��B
�HB
�B
��B
��B
��B
y$B
s3B
h�B
e`B
]�B
X�B
V�B
S[B
R:B
NpB
K^B
F�B
B'B
<�B
1B
+�B
!HB
�B
�B
B
�B	�3B	�B	�kB	��B	�7B	҉B	��B	�lB	��B	�rB	��B	��B	�7B	�EB	�B	��B	��B	��B	�gB	��B	}"B	v�B	u?B	q[B	k�B	h>B	h
B	d�B	b�B	_�B	\�B	Y�B	W�B	S�B	Q�B	M�B	KDB	I7B	D�B	>�B	8�B	0B	*eB	$�B	 �B	�B	�B	[B	\B	�B	�B�jB��B�`B�nB�oB�B��B�LB��B��B�B�B��B��B�'B�B�MB�:B�BB�xB��B�%B��B�?B�B��B�'B�4B�B�fB�?B�[B��B��B��B��B��B��B��B��B�BB��B��B�QB��B�NB�xB��B�MB� B�B�B~�B}qB}VB|jB{�By�Bx�Bx�Bu?BmwBkQBkBi�BgRBc�B\BQ�B\CB[�BY1BQ BKBO�BO�BN�BN<BM6BMBLJBKxBL0BMBMBLBKBH�BEBFtBEmBB�B;�B>�B@4B=VB8RB:�B;�B<6B;B8�B1�B3�B3�B0!B,WB)�B'B*�B.IB5�B7�B6�B6�B4�B5�B5�B5B6�B7�B7�B6�B2aB,�B3hB1�B9�B7B1�B8RB:^BA�BCBC�BE9BH�BJ�BK�BK�BK�BK�BLBN"BN<BNVBMjBMPBJ�BG�BEBBBB'BF�BN<BQBSBRoBPBQhBU�BW�BYB\]B\]BZ�BVBUMB]B`�Bf�Bg�Bh�Bj�Bp!BvFBx�Bw�BwLBv�B|BB�B� B�4BcB}�B|�B}B~�B��B��B��B�xB�vB��B��B��B��B��B�HB�9B��B��B�B��B��B�B�-B� B�TB�`B�`B�_B�)B�/B�/B�CB�]B�wB��B�[B��BżB��B��B��B��B��B��B��B��B�(B�,B�B�B�$B�EBڠB�B��B��B��B��B�B�'B�iB�%B�B�B��B�*B�^B��B	1B	KB	�B	
�B	�B	�B	�B	�B	
B	 �B	#�B	#�B	#�B	%�B	&�B	'B	'B	(
B	($B	*KB	.IB	/�B	3�B	9rB	;�B	;�B	<�B	E�B	F�B	G�B	H�B	I�B	I�B	J�B	J�B	J�B	J�B	K�B	L�B	N�B	Q B	UB	W
B	W
B	X+B	XB	Y1B	ZB	Z7B	[=B	[=B	\]B	_VB	aHB	a|B	d�B	hsB	hsB	j�B	l�B	m�B	o�B	p�B	q�B	r�B	t�B	u�B	u�B	u�B	u�B	u�B	v�B	w�B	y�B	y�B	zB	|B	}�B	~B	B	�B	�-B	�-B	�3B	�3B	�RB	�JB	�bB	�uB	�aB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�/B	�5B	�iB	�vB	�|B	��B	��B	��B	��B	�^B	�wB	��B	ĶB	ǮB	��B	ȴB	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	� B	�B	��B	��B	��B	�,B	�[B	�mB	�KB	�=B	�CB	�CB	�)B	�CB	�qB	چB	�xB	�VB	�\B	�\B	�pB	�vB	�B	�B	�tB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	�0B	�B	�"B	�(B
  B
  B
 B
B
 B
 B
 B
 B
;B
-B
-B
B
GB
GB
9B
%B
%B
%B
%B
9B
YB
�B
fB

=B

=B

XB

XB

rB
^B
dB
6B
VB
VB
�B
�B
hB
hB
�B
uB
uB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
�B
�B
 B
#�B
$�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'B
'�B
(
B
(
B
'B
'B
'B
&�B
'B
'B
(
B
)B
+B
+�B
,B
,B
,B
,"B
+6B
+6B
,"B
,"B
-B
,�B
-B
-)B
-)B
./B
./B
/5B
/5B
0;B
2GB
3MB
33B
4TB
4TB
4TB
49B
4nB
3�B
3�B
3MB
4TB
4nB
5?B
6+B
6FB
6FB
5tB
5tB
6`B
6zB
7LB
8lB
8lB
8lB
8�B
7�B
7�B
8RB
8lB
8lB
8RB
8RB
9rB
:xB
:xB
;B
;B
;B
;B
;B
<�B
=VB
=qB
=�B
=�B
=�B
=�B
>�B
?}B
?cB
?�B
?}B
@�B
@�B
@�B
A�B
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
E�B
F�B
F�B
H�B
H�B
I�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
M�B
N�B
M�B
M�B
M�B
M�B
N�B
N�B
OB
O�B
Q B
Q B
Q�B
RB
RB
RB
RB
R B
SB
TB
TB
S�B
S�B
S�B
TB
S�B
TB
T�B
VB
VB
VB
V9B
X+B
X�B
Y1B
Y1B
Y1B
Z7B
ZQB
[=B
[#B
[=B
\B
\)B
\)B
\)B
\)B
\)B
\CB
[#B
\CB
\)B
]/B
\CB
]/B
^5B
^OB
^5B
_!B
^5B
_VB
_;B
_!B
_;B
_;B
_;B
_;B
_;B
`BB
`\B
`\B
_;B
_;B
_VB
_VB
_;B
`BB
_;B
`\B
`\B
aHB
abB
abB
bhB
cTB
cTB
cnB
bNB
cTB
cTB
cTB
c:B
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cnB
cnB
cTB
dZB
d@B
dtB
dtB
dZB
dZB
dtB
e`B
dtB
e`B
ezB
ezB
ffB
ffB
f�B
f�B
ffB
f�B
gmB
hsB
g�B
g�B
h�B
hsB
hsB
hsB
i�B
i�B
iyB
i�B
i�B
iyB
iyB
j�B
kkB
k�B
k�B
k�B
j�B
j�B
k�B
k�B
lqB
l�B
lqB
l�B
m�B
m�B
l�B
mwB
mwB
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712220040362017122200403620171222004036201806221235022018062212350220180622123502201804050431172018040504311720180405043117  JA  ARFMdecpA19c                                                                20171218093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171218003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171218003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171218003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171218003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171218003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171218003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171218003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171218003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171218003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20171218005601                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171218153345  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171219000000  CF  PSAL_ADJUSTED_QCD�@ D�� G�O�                JM  ARCAJMQC2.0                                                                 20171221154036  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171221154036  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193117  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033502  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                