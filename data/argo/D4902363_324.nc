CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-24T00:38:19Z creation;2019-01-24T00:38:24Z conversion to V3.1;2019-12-19T07:22:39Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190124003819  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              DA   JA  I2_0576_324                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @آ5�ۀ1   @آ6���@9Ԏ�q�j�dQJ#9��1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C	�C
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
=C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�>DՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��{D�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�4{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�-A�9XA�1'A�-A�(�A�&�A�$�A�$�A�"�A� �A��A��A��A��A� �A� �A��A� �A��A��A��A��A��A��A��A��A��A�%A��mA�ĜA���A�Q�A�+A�A�z�A��hA�C�A���A���A�O�A��yA���A��A�^5A�
=A��\A���A��9A�?}A���A�"�A�p�A�bNA�\)A�K�A�A��^A���A�G�A�~�A�\)A�E�A�1A��A�A��;A�l�A���A���A��/A��A�(�A��
A��A��-A�%A�$�A��hA�;dA��\A��HA�VA��A�r�A�bA�&�A�p�A�JA�9XA���A��7A��#A�x�A��mA��A�9XA�A��^A�&�A�x�A��\A��yA�Q�A�I�A�VA���A��PA��mA��\A�$�A��yA�A�$�A��A���A�&�A�A~�RA~��A~^5A|��Ax��AwC�Avv�AuK�Arv�Aq�Ao�Anz�AlbNAj(�Ah�!AhbNAg�Afr�Ad��Acx�Ab�yAbVA`�+A]��AZ�AXr�AX �AW�^AW�AVv�ATr�AS"�AQ�;AQ`BAP��AOt�AN�uAMS�AK�mAKAK�PAJ�AJ  AI�#AH^5AGp�AE��AEC�AC�;AB�yAB��ABv�AA;dA@VA@{A?��A?XA>ĜA>�\A=�-A=\)A=\)A=K�A=%A<�DA;C�A9�^A9�A8�A8$�A5�^A4bA3�7A2��A1�^A1��A1A0�+A0�A.�yA.~�A.VA-�A,�/A+;dA)�A)��A(��A'"�A&�RA%�-A$�!A$(�A#/A#A"�!A"r�A"1'A!�A!�A �+A��A�-AS�A�jA9XA�TA��Ap�A�A�AJAAhsA�!A=qAS�AA�A=qAG�A��A��AC�A  A`BA�`AE�A��A&�A��Ar�A�wA�A�A?}A
jA	�TA	hsA	7LAbNA%A1AoA�A�PA$�A7LA �HA ��A A�@��
@�C�@��+@�hs@���@�1@��`@��
@���@���@���@�"�@�E�@�^@�?}@��;@��@�x�@�9@�V@�7@��@�@�\@�`B@�j@��@��@��@��@��m@ާ�@��@�@ܓu@�^5@��m@׍P@ְ!@���@�9X@�n�@��@���@�dZ@�"�@θR@�V@ʰ!@�?}@���@��@ě�@Å@�;d@�v�@���@���@���@���@��@���@��R@���@�ff@�J@��-@��h@�x�@���@��P@�K�@�+@���@�v�@�J@�`B@��@�dZ@���@���@�J@��D@�dZ@�K�@�"�@��+@��#@�G�@�%@��@�bN@�|�@��y@�~�@�E�@�$�@��T@��-@��@���@�I�@���@�K�@��y@��+@�%@�  @���@�;d@�o@���@�M�@��7@��@�Z@��@�ȴ@�=q@��@��@���@��\@�V@�$�@��@�=q@�5?@���@��#@�x�@�  @�"�@�V@���@�`B@��@�Z@�v�@�5?@���@�r�@�@���@��#@��h@��@��9@�r�@�A�@���@�K�@�o@��@���@��+@�M�@���@�`B@���@�j@�I�@��@�ƨ@��@�|�@�dZ@���@���@���@��+@�ff@�ff@�V@�E�@�5?@�$�@��@��7@��@��j@��D@�bN@�I�@�  @�\)@��@��H@���@��!@���@�~�@�V@��#@���@�7L@��@��`@��u@�r�@�K�@�C�@�"�@���@���@���@���@���@���@��\@�v�@�ff@�^5@�E�@�{@��^@�p�@��/@�bN@�Q�@�9X@��@;d@~�+@~$�@}�h@|9X@{��@{C�@z�@z��@z�\@z=q@y�@y��@y�7@x��@xbN@xQ�@xQ�@xQ�@xbN@xbN@xbN@xb@wl�@v��@v��@v��@v��@vff@v@u��@u�@t��@t�/@t��@tj@s��@sƨ@s��@s"�@r�!@rM�@r�@r�@r�@q�#@qX@pQ�@o�;@o��@ol�@o�@nv�@m�T@mp�@l��@lz�@kƨ@kC�@k"�@j��@j�!@j��@j~�@j�@i�@h�`@h��@hĜ@h�u@hr�@hb@g\)@g\)@f$�@e�h@e�-@eO�@dI�@d1@cC�@co@cS�@c�@c��@c�F@c�F@c�F@c��@cS�@b��@b�!@b��@bn�@b-@b�@a��@a��@aX@a&�@a&�@a�@a�@a%@`��@`�u@`1'@_��@_�@^ff@]@]`B@\�@\Z@\(�@\1@[ƨ@Z��@YG�@X�9@XA�@Xb@W��@W��@W;d@V��@VE�@V$�@V@U�T@U�@U�T@Up�@UO�@UV@T�D@Tj@TI�@S��@Sƨ@S��@S�@S�@St�@St�@SS�@So@R^5@Q�^@P�u@P �@O+@M�T@Mp�@M/@L��@Lz�@LI�@L1@K�@K33@J�H@J��@J��@I��@I�7@I&�@H1'@Gl�@G;d@G�@F�@FE�@F$�@E�@E�@EO�@E/@D�@D�j@D�@D�D@D9X@Cƨ@CS�@C"�@B�@B�H@B��@B�!@B~�@B^5@B-@A��@A�#@A��@A�^@A��@Ax�@A&�@@�`@@��@@Ĝ@@Ĝ@@��@@1'@?�P@?;d@>��@>ȴ@>ȴ@>�+@>ff@>5?@=�-@=V@=V@<�@<�@<��@<��@<��@<�@<9X@;�m@;��@;dZ@;33@:��@:~�@9��@9X@8�@8b@7��@7�P@7K�@7�@6ȴ@6�+@6V@6E�@6@5��@5�h@4�@4�D@4�D@4�D@4I�@4(�@3�m@3��@3S�@333@2�H@2��@2M�@1��@1��@0��@0A�@01'@0  @/��@/l�@/K�@/;d@/+@/�@.ȴ@.ff@.$�@-�h@-�@-V@,�@,�j@,��@,z�@,j@,j@,j@,j@,I�@,1@+�
@+�F@+��@+C�@*�H@*�\@*~�@*=q@)��@)�^@)�@(��@(��@(Ĝ@(��@(bN@(  @'��@';d@'
=@&ȴ@&�+@&V@&{@%��@%�-@%�h@%p�@$�@#�
@#C�@#33@#"�@#"�@#"�@#"�@"�@"�!@"��@"��@"�\@"�\@"~�@"M�@"=q@!��@!%@ ��@ ��@ bN@ b@��@|�@�@��@��@ff@5?@$�@�@�@`B@`B@`B@/@��@��@�D@9X@�@�F@dZ@C�@@�H@�!@^5@=q@�@��@G�@&�@�@%@��@�9@bN@�@|�@l�@K�@
=@�y@�y@�y@�@�@�@�@�@ȴ@ȴ@��@v�@�@�T@�-@�@`B@O�@�@�@�j@��@(�@�m@�
@�
@�
@�
@�@@�!@M�@�@��@x�@hs@G�@&�@��@�u@A�@�;@�@��@\)@+@+@+@+@
=@ȴ@v�@V@E�@$�@�@��@@�-@��@`B@V@�@�j@�D@j@9X@�@�@�@��@��@��@��@"�@
�!@
^5@
M�@
M�@
=q@
-@
�@	��@	X@	G�@	G�@	7L@�`@r�@bN@A�@  @�;@�w@��@�P@|�@l�@\)@;d@
=@�R@�+@v�@V@E�@5?@$�@@@��@p�@O�@V@�@z�@Z@I�@9X@9X@�@1@�
@��@��@��@�@�111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�-A�9XA�1'A�-A�(�A�&�A�$�A�$�A�"�A� �A��A��A��A��A� �A� �A��A� �A��A��A��A��A��A��A��A��A��A�%A��mA�ĜA���A�Q�A�+A�A�z�A��hA�C�A���A���A�O�A��yA���A��A�^5A�
=A��\A���A��9A�?}A���A�"�A�p�A�bNA�\)A�K�A�A��^A���A�G�A�~�A�\)G�O�G�O�A��A�A��;A�l�A���A���A��/A��A�(�A��
A��A��-A�%A�$�A��hA�;dA��\A��HA�VA��A�r�A�bA�&�A�p�A�JA�9XA���A��7A��#A�x�A��mA��A�9XA�A��^A�&�A�x�A��\A��yA�Q�A�I�A�VA���A��PA��mA��\A�$�A��yA�A�$�A��A���A�&�A�A~�RA~��A~^5A|��Ax��AwC�Avv�AuK�Arv�Aq�Ao�Anz�AlbNAj(�Ah�!AhbNAg�Afr�Ad��Acx�Ab�yAbVA`�+A]��AZ�AXr�AX �AW�^AW�AVv�ATr�AS"�AQ�;AQ`BAP��AOt�AN�uAMS�AK�mAKAK�PAJ�AJ  AI�#AH^5AGp�AE��AEC�AC�;AB�yAB��ABv�AA;dA@VA@{A?��A?XA>ĜA>�\A=�-A=\)A=\)A=K�A=%A<�DA;C�A9�^A9�A8�A8$�A5�^A4bA3�7A2��A1�^A1��A1A0�+A0�A.�yA.~�A.VA-�A,�/A+;dA)�A)��A(��A'"�A&�RA%�-A$�!A$(�A#/A#A"�!A"r�A"1'A!�A!�A �+A��A�-AS�A�jA9XA�TA��Ap�A�A�AJAAhsA�!A=qAS�AA�A=qAG�A��A��AC�A  A`BA�`AE�A��A&�A��Ar�A�wA�A�A?}A
jA	�TA	hsA	7LAbNA%A1AoA�A�PA$�A7LA �HA ��A A�@��
@�C�@��+@�hs@���@�1@��`@��
@���@���@���@�"�@�E�@�^@�?}@��;@��@�x�@�9@�V@�7@��@�@�\@�`B@�j@��@��@��@��@��m@ާ�@��@�@ܓu@�^5@��m@׍P@ְ!@���@�9X@�n�@��@���@�dZ@�"�@θR@�V@ʰ!@�?}@���@��@ě�@Å@�;d@�v�@���@���@���@���@��@���@��R@���@�ff@�J@��-@��h@�x�@���@��P@�K�@�+@���@�v�@�J@�`B@��@�dZ@���@���@�J@��D@�dZ@�K�@�"�@��+@��#@�G�@�%@��@�bN@�|�@��y@�~�@�E�@�$�@��T@��-@��@���@�I�@���@�K�@��y@��+@�%@�  @���@�;d@�o@���@�M�@��7@��@�Z@��@�ȴ@�=q@��@��@���@��\@�V@�$�@��@�=q@�5?@���@��#@�x�@�  @�"�@�V@���@�`B@��@�Z@�v�@�5?@���@�r�@�@���@��#@��h@��@��9@�r�@�A�@���@�K�@�o@��@���@��+@�M�@���@�`B@���@�j@�I�@��@�ƨ@��@�|�@�dZ@���@���@���@��+@�ff@�ff@�V@�E�@�5?@�$�@��@��7@��@��j@��D@�bN@�I�@�  @�\)@��@��H@���@��!@���@�~�@�V@��#@���@�7L@��@��`@��u@�r�@�K�@�C�@�"�@���@���@���@���@���@���@��\@�v�@�ff@�^5@�E�@�{@��^@�p�@��/@�bN@�Q�@�9X@��@;d@~�+@~$�@}�h@|9X@{��@{C�@z�@z��@z�\@z=q@y�@y��@y�7@x��@xbN@xQ�@xQ�@xQ�@xbN@xbN@xbN@xb@wl�@v��@v��@v��@v��@vff@v@u��@u�@t��@t�/@t��@tj@s��@sƨ@s��@s"�@r�!@rM�@r�@r�@r�@q�#@qX@pQ�@o�;@o��@ol�@o�@nv�@m�T@mp�@l��@lz�@kƨ@kC�@k"�@j��@j�!@j��@j~�@j�@i�@h�`@h��@hĜ@h�u@hr�@hb@g\)@g\)@f$�@e�h@e�-@eO�@dI�@d1@cC�@co@cS�@c�@c��@c�F@c�F@c�F@c��@cS�@b��@b�!@b��@bn�@b-@b�@a��@a��@aX@a&�@a&�@a�@a�@a%@`��@`�u@`1'@_��@_�@^ff@]@]`B@\�@\Z@\(�@\1@[ƨ@Z��@YG�@X�9@XA�@Xb@W��@W��@W;d@V��@VE�@V$�@V@U�T@U�@U�T@Up�@UO�@UV@T�D@Tj@TI�@S��@Sƨ@S��@S�@S�@St�@St�@SS�@So@R^5@Q�^@P�u@P �@O+@M�T@Mp�@M/@L��@Lz�@LI�@L1@K�@K33@J�H@J��@J��@I��@I�7@I&�@H1'@Gl�@G;d@G�@F�@FE�@F$�@E�@E�@EO�@E/@D�@D�j@D�@D�D@D9X@Cƨ@CS�@C"�@B�@B�H@B��@B�!@B~�@B^5@B-@A��@A�#@A��@A�^@A��@Ax�@A&�@@�`@@��@@Ĝ@@Ĝ@@��@@1'@?�P@?;d@>��@>ȴ@>ȴ@>�+@>ff@>5?@=�-@=V@=V@<�@<�@<��@<��@<��@<�@<9X@;�m@;��@;dZ@;33@:��@:~�@9��@9X@8�@8b@7��@7�P@7K�@7�@6ȴ@6�+@6V@6E�@6@5��@5�h@4�@4�D@4�D@4�D@4I�@4(�@3�m@3��@3S�@333@2�H@2��@2M�@1��@1��@0��@0A�@01'@0  @/��@/l�@/K�@/;d@/+@/�@.ȴ@.ff@.$�@-�h@-�@-V@,�@,�j@,��@,z�@,j@,j@,j@,j@,I�@,1@+�
@+�F@+��@+C�@*�H@*�\@*~�@*=q@)��@)�^@)�@(��@(��@(Ĝ@(��@(bN@(  @'��@';d@'
=@&ȴ@&�+@&V@&{@%��@%�-@%�h@%p�@$�@#�
@#C�@#33@#"�@#"�@#"�@#"�@"�@"�!@"��@"��@"�\@"�\@"~�@"M�@"=q@!��@!%@ ��@ ��@ bN@ b@��@|�@�@��@��@ff@5?@$�@�@�@`B@`B@`B@/@��@��@�D@9X@�@�F@dZ@C�@@�H@�!@^5@=q@�@��@G�@&�@�@%@��@�9@bN@�@|�@l�@K�@
=@�y@�y@�y@�@�@�@�@�@ȴ@ȴ@��@v�@�@�T@�-@�@`B@O�@�@�@�j@��@(�@�m@�
@�
@�
@�
@�@@�!@M�@�@��@x�@hs@G�@&�@��@�u@A�@�;@�@��@\)@+@+@+@+@
=@ȴ@v�@V@E�@$�@�@��@@�-@��@`B@V@�@�j@�D@j@9X@�@�@�@��@��@��@��@"�@
�!@
^5@
M�@
M�@
=q@
-@
�@	��@	X@	G�@	G�@	7L@�`@r�@bN@A�@  @�;@�w@��@�P@|�@l�@\)@;d@
=@�R@�+@v�@V@E�@5?@$�@@@��@p�@O�@V@�@z�@Z@I�@9X@9X@�@1@�
@��@��@��@�@�111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��BBBB%B%B+B+B1B	7B
=BDB
=B	7B	7B
=B
=B
=BDBJBPBJBJBbBoB{B�B&�B.B8RBI�BL�BG�BVB� B�1B��B��B��B��B�B��B��B��B�JB��B�JB�Bv�BhsBbNBW
BS�BO�BcTBhsB`BBO�B`BBYBC�BJBuB�B5?B,B0!B�BB$�B�BPB��BPB��B�HB�)B�ZB�mB�)B�;B��B��B�RB�-B�RB��B�!B��B�JB�B[#B+B[#B`BBVBA�B1'B�B�BVB
�B
�ZB
��B
�sB
��B
��B
�jB
�uB
w�B
?}B
iyB
q�B
hsB
W
B
L�B
aHB
P�B
1'B	��B
PB
�B
B	�yB	�sB	�B	��B	��B	�RB	ĜB	��B	ƨB	�B	��B	��B	��B	�uB	q�B	N�B	B�B	YB	s�B	l�B	_;B	T�B	<jB	<jB	;dB	G�B	?}B	.B	,B	"�B	�B	.B	'�B	�B	�B	�B	%B	  B��B��B�B�B��B��B�B�mB��B�B�B�B�B�sB�yB�B�B�fB�B��B�XB��BŢB�-B��B��B�?B�B��B�FB�B��B��B��B��B��B�{B}�Bs�Bp�B�Bp�BgmBo�Bp�BgmBs�BiyBy�Bu�Bt�Bq�BffBiyBffBbNBm�BjBe`Be`BiyBhsBhsB`BB[#BYB\)BVBJ�BI�B:^B49B5?B2-B6FB#�BD�B>wB-B33B9XB5?B6FB8RB9XB7LB.B)�B%�B&�B&�B2-B0!B0!B"�B�B�B�B�B#�BuB�B+B+B+B(�B)�B&�B �B#�B�BPB�B'�B �B�BVB�B#�B �B�BoB�B�B
=B�B�B$�B�B�B�B�B"�B�B�B�B�B�B�BhBDBVB)�B#�B �B�B�B"�B+B5?B5?B0!B#�B�B(�B/B0!B&�B;dBC�B@�B=qB@�B?}BF�BO�BP�BP�BO�BM�BL�BK�BN�BK�BE�B@�BN�BO�BM�BI�BH�BD�BC�BC�BK�BO�BI�BD�BI�BZBYBS�BS�BXB]/B[#B_;BZB^5BgmBiyBk�BiyBhsBffBiyBiyBiyBr�Bs�Bq�BjBp�B~�B�B�+B�1B�%B�+B�1B��B�\B�{B��B��B�{B��B��B�B�!B�9B�FB�FB�9B�^B�XB�?B��BȴB��B��B��BƨB��B�B�B�B�#B�fB�B��B��B��B��B	B	B	1B	PB	\B	\B	\B	bB	\B	PB	bB	�B	�B	�B	�B	#�B	"�B	$�B	 �B	)�B	,B	-B	.B	/B	/B	.B	.B	.B	-B	,B	0!B	5?B	:^B	;dB	;dB	:^B	:^B	C�B	F�B	H�B	J�B	K�B	I�B	I�B	H�B	O�B	P�B	VB	]/B	YB	\)B	R�B	cTB	cTB	dZB	gmB	iyB	jB	jB	jB	l�B	l�B	m�B	m�B	l�B	m�B	m�B	n�B	m�B	q�B	x�B	w�B	v�B	s�B	u�B	x�B	y�B	w�B	�B	�B	�+B	�1B	�7B	�1B	�7B	�=B	�=B	�1B	�VB	�hB	�hB	�hB	�hB	�hB	�bB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�XB	�qB	�qB	�jB	�dB	�XB	�qB	�}B	��B	��B	��B	�wB	�qB	��B	�}B	B	ǮB	ŢB	��B	ɺB	ɺB	��B	��B	�
B	�B	�
B	�
B	�
B	�
B	�
B	�B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�NB	�HB	�HB	�;B	�;B	�BB	�HB	�NB	�TB	�fB	�mB	�mB	�B	�B	�sB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B	��B	��B	��B	��B
  B	��B	��B
B
%B
B
+B
1B
1B
+B
1B

=B
DB
DB
1B
	7B
DB
	7B
JB
bB
hB
bB
bB
oB
oB
hB
{B
{B
{B
{B
�B
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
�B
�B
 �B
!�B
!�B
 �B
 �B
 �B
 �B
!�B
$�B
&�B
'�B
(�B
(�B
(�B
(�B
+B
+B
)�B
)�B
)�B
(�B
,B
/B
/B
-B
.B
.B
.B
.B
/B
.B
/B
/B
0!B
/B
/B
0!B
5?B
5?B
49B
6FB
6FB
7LB
7LB
6FB
5?B
5?B
6FB
5?B
7LB
:^B
:^B
9XB
:^B
:^B
;dB
;dB
;dB
;dB
:^B
:^B
:^B
;dB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
;dB
?}B
?}B
?}B
>wB
>wB
=qB
>wB
>wB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
A�B
?}B
=qB
C�B
F�B
G�B
G�B
G�B
G�B
F�B
E�B
G�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
E�B
H�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
L�B
L�B
L�B
L�B
L�B
K�B
M�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
P�B
P�B
S�B
S�B
S�B
S�B
R�B
Q�B
Q�B
R�B
W
B
VB
VB
W
B
XB
XB
XB
XB
YB
XB
XB
XB
XB
W
B
VB
T�B
YB
XB
XB
YB
YB
YB
YB
YB
YB
XB
ZB
\)B
\)B
\)B
\)B
ZB
YB
[#B
[#B
\)B
]/B
_;B
_;B
_;B
^5B
^5B
]/B
^5B
_;B
`BB
bNB
aHB
bNB
cTB
cTB
bNB
bNB
aHB
aHB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
e`B
e`B
e`B
ffB
ffB
hsB
hsB
hsB
gmB
ffB
hsB
gmB
e`B
ffB
hsB
jB
jB
jB
jB
iyB
hsB
iyB
k�B
k�B
k�B
iyB
iyB
l�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
l�B
m�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�BBBB%B%B+B+B1B	7B
=BDB
=B	7B	7B
=B
=B
=BDBJBPBJBdBbB�B�BB'8B.�B8�BJ	BMjBI7BW�B��B�B��B�YB�xB�nB�B�LB��B��B��B�
B�jB�ABxlBjeBd�BYeBV9BRTBc�Bh�Ba-BQ�B`vBY�G�O�G�O�B�B"B6+B-]B0�B�B1B%�B �BvBںB�B iB��B�B��B��B�~B�'B�2B��B�DB��B�>B��B��B�B�"B��B^�B/B[�B`�BV�BCB2�B�B7B�B
�?B
�B
՛B
�*B
ՁB
��B
��B
��B
{0B
D�B
kQB
r�B
i�B
YB
N"B
abB
Q�B
3�B	��B
B
�B
�B	�qB	�0B	��B	�?B	�HB	��B	�%B	�:B	ǔB	� B	��B	�;B	��B	��B	tnB	R�B	F�B	Z�B	tB	m)B	`'B	V9B	>�B	>B	<�B	HKB	@iB	/�B	-]B	$ZB	QB	.B	(�B	�B	eB	)B	�B	oB��B��B�GB�wB�<B�rB�B�sB�+B�3B�OB�OB�'B�yB��B�B��B�B�B�PB�0B�oBƨB��B��B��B�B�UB�$B�zB��B��B��B�B�\B�:B�MB�Bu�BrB�{BrBi_BpoBq�Bh�BtTBj�BzDBvFBu%Br-Bg8Bj0Bg8Bc:Bm�BkBf2BfBi�Bh�Bh�B`�B[�BY�B\�BV�BK�BJrB;�B5�B6�B3�B7�B%`BD�B?.B.�B4B:B6FB72B9	B9�B7�B/5B+B'B(
B(
B2�B0�B0�B$&B_B�B�B�B$tBgB�B+�B+�B+�B)�B*B'mB!�B$ZB~BB;B(>B!|ByB�B BB$&B!HB�B�BB#B�BOB�B%B�B_BdB�B#:BjB BB�BeB 'B \B�B�B�B*0B$�B!�B�B�B#�B+�B5�B5�B0�B%FB vB)�B/�B1B(�B<BC�BA B>(BA B@iBG+BPBP�BP�BPBN"BMBLBN�BK�BF?BAoBN�BPBN"BJ=BIBE9BDMBD3BLBO�BJrBE�BJrBZBYKBT�BT{BXyB]dB[�B_�BZ�B^�Bg�Bi�Bk�Bi�Bh�Bf�Bi�Bi�BjBr�BtBr-Bk�BqvBHB�aB�_B��B��B��B��B��B��B��B�B�B�gB�_B�xB�6B�UB�TB�FB�`B��B�xB��B�FB�AB�RB�)B�B�<B�_B��B�BٴB��B�)B�B��B��B�%B�*B�(B	AB	oB	�B	�B	vB	vB	�B	�B	�B	�B	�B	�B	�B	�B	 B	#�B	#B	$�B	!HB	*B	,B	-)B	.B	/5B	/5B	./B	./B	./B	-)B	,WB	0UB	5�B	:�B	;B	;�B	:�B	:�B	C�B	F�B	H�B	J�B	K�B	I�B	I�B	IB	O�B	Q4B	V9B	]/B	YeB	\]B	S�B	c:B	c�B	d�B	g�B	i�B	jB	j�B	j�B	l�B	l�B	m�B	m�B	l�B	m�B	m�B	n�B	m�B	q�B	x�B	w�B	v�B	tB	vB	y	B	z*B	x8B	�;B	�MB	�EB	�KB	�7B	�fB	�RB	�XB	�rB	��B	�pB	�hB	�NB	�hB	�NB	�hB	�}B	�pB	�vB	��B	��B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�4B	�
B	�B	�"B	�6B	�QB	�)B	�5B	�5B	�UB	�vB	�nB	�lB	��B	�qB	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�
B	�$B	�$B	�EB	�)B	�CB	�)B	�)B	�IB	�IB	�dB	�VB	�bB	�NB	�NB	�NB	�bB	�HB	�VB	�pB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B
 B
B
B
 B
 �B
  B	�B	�6B	�6B	�JB
 4B	�BB	�VB
-B
%B
9B
EB
KB
KB
_B
KB

=B
^B
^B
�B
	lB
^B
	�B
~B
}B
�B
}B
�B
�B
�B
�B
{B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
�B
�B
 �B
!�B
!�B
 �B
 �B
 �B
!B
"B
%B
'B
'�B
)B
)B
)B
)B
+B
+B
*B
*B
)�B
)*B
,"B
/B
/B
-)B
./B
./B
./B
./B
/B
.IB
/5B
/5B
0;B
/OB
/iB
0UB
5%B
5ZB
4nB
6`B
6`B
7LB
7LB
6`B
5ZB
5tB
6`B
5tB
7fB
:^B
:xB
9rB
:xB
:xB
;dB
;JB
;dB
;dB
:xB
:xB
:xB
;dB
:xB
:xB
:�B
;B
<�B
<�B
<�B
<�B
;�B
?�B
?}B
?�B
>�B
>�B
=�B
>�B
>�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
A�B
?�B
=�B
C�B
F�B
G�B
G�B
G�B
G�B
F�B
E�B
G�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
E�B
H�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
L�B
L�B
L�B
L�B
L�B
K�B
M�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
Q B
P�B
Q B
P�B
Q B
RB
RB
Q B
Q B
S�B
S�B
S�B
TB
SB
R B
R B
S&B
W
B
VB
VB
W$B
XB
W�B
W�B
XB
X�B
XB
W�B
XB
XB
W$B
VB
UB
X�B
X+B
X+B
Y1B
Y1B
Y1B
Y1B
Y1B
Y1B
XEB
Z7B
\)B
\)B
\B
\)B
ZQB
YKB
[#B
[WB
\CB
]IB
_;B
_;B
_VB
^OB
^OB
]dB
^OB
_VB
`\B
bNB
aHB
bhB
cTB
cTB
bNB
bhB
abB
abB
cTB
dZB
cnB
cnB
dZB
dZB
dZB
dZB
cnB
cnB
ezB
e`B
e`B
f�B
f�B
hsB
hsB
hXB
g�B
f�B
hsB
g�B
e�B
f�B
hsB
jeB
jB
jB
jB
iyB
h�B
i�B
k�B
k�B
k�B
i�B
i�B
lqB
l�B
k�B
l�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
l�B
m�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901280034072019012800340720190128003407201901280200162019012802001620190128020016201901290021492019012900214920190129002149  JA  ARFMdecpA19c                                                                20190124093806  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190124003819  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190124003822  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190124003822  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190124003823  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190124003823  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190124003823  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190124003823  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20190124003823  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190124003823  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20190124003824  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190124003824                      G�O�G�O�G�O�                JA  ARUP                                                                        20190124005736                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190124153309  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190127153407  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190127153407  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190127170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190128152149  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                