CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-05T00:35:24Z creation;2018-04-05T00:35:32Z conversion to V3.1;2019-12-19T07:45:35Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180405003524  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_226                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�X���w 1   @�X��m� @:��s�dm5�Xy>1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ D�|�D�� D�  D�C3D׃3D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A ��A"=qA@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=CG�CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C[�C^
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHD�~D��HD�HD�D{Dׄ{D��HD�{D�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�>D�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��{D��{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�(�A�(�A��A�
=A���A��;A��!A���A�(�A��A���A��A�dZA���A���A�n�A��A���A���A�~�A�dZA�I�A�(�A��A�VA�z�A��7A��^A�$�A��A���A�bA�p�A�G�A���A��uA�+A���A��#A���A�E�A���A���A�=qA��#A��wA�(�A���A�A�A��RA�G�A�XA�I�A���A��7A��wA�JA�/A�O�A�VA�bNA��hA�jA���A�^5A�jA�;dA���A��hA���A�r�A���A��A���A�VA��jA��RA�?}A�  A���A��A���A�A�ĜA�bA�VAK�A~n�A}|�A|^5A{��A{�Ay�Aw�#Au�7As&�Ar��Aq�Ao�-An�Al�HAkAjĜAj=qAiAh�AgG�AfbNAe��Ad�/Ac&�Aa;dA`�uA_A^�A^JA]�PA]oA[�AYC�AX�AX  AWƨAWG�AV�DAU��AU|�AT��AT �ASdZAR��ARVAQ��AQp�AP��AP��AO�#AM��AL��ALr�AL=qAK�TAK��AK�AJQ�AI�-AH�`AG;dAF-AE7LAD9XAC��AC�^AC��ACO�AB�/AB�9AB��AA�A?�A>VA>�A=�#A=��A=l�A=&�A<Q�A:�A:A�A:{A9��A8ffA6�`A5�hA4�jA4I�A3�A3��A3/A1��A0��A/7LA.E�A-�-A,�RA+��A+?}A+�A+�A+
=A*�A)��A(�A({A'XA'%A&�DA&^5A&�A%�-A%VA$��A$n�A#`BA!A 9XA�;A�^A�AoA�
AVA��A�hAz�A=qAAl�AĜA�AXA7LA"�AoA��A7LA��A�+AVAA��A��AS�A
(�A�/AM�A��A;dA�\A�mA�HAA�AA�A�FA bN@��+@��u@��@�l�@�S�@�33@���@�7L@�Q�@�ƨ@��@�ff@�-@�V@�R@���@�bN@땁@�K�@���@�!@ꟾ@�1'@�ȴ@�+@�G�@�-@�z�@��
@���@�x�@ۍP@�M�@ف@�7L@��@�1'@�v�@��@ӝ�@�ȴ@���@�&�@��;@��H@�p�@̃@��@���@ɑh@�Ĝ@ȃ@�9X@��;@�S�@�
=@�@�A�@�M�@�hs@�G�@�7L@���@���@���@���@���@��u@�v�@��T@�x�@��@�I�@�|�@�`B@��m@���@�&�@�  @�M�@���@�1'@��;@��P@���@���@�X@�V@�1'@���@��P@�t�@�+@�V@��#@�r�@��F@��!@�n�@�hs@�&�@���@�I�@��@�t�@���@���@�n�@�V@�E�@�-@��@���@��T@���@��h@��@��u@�9X@��
@�C�@���@���@�`B@��D@�b@��m@�ƨ@��@�S�@��R@�-@���@���@���@�r�@�1'@��@��@��R@�$�@���@�`B@��@��`@��j@��9@���@�r�@�I�@���@�C�@��H@�^5@���@��h@�O�@�bN@���@�|�@�dZ@�o@��+@�^5@�E�@���@��#@�@��^@��^@��^@�@�@��7@��7@�hs@�/@��/@���@��u@��@�z�@�r�@�1'@�  @��w@��F@��F@���@�t�@�o@��!@�J@��-@�O�@��`@��j@���@�Q�@�  @��
@�|�@��R@��+@�n�@�5?@���@���@�x�@��@��7@�p�@�%@�Ĝ@��@�z�@�1@K�@�@~��@~ȴ@~v�@~$�@}O�@|��@|z�@|9X@|�@{ƨ@{t�@{33@{33@z��@z~�@y�@yhs@x�u@x �@w��@w
=@v5?@v@u��@tz�@r�@r~�@r^5@rM�@r-@q��@q%@p��@p��@p�u@p1'@n��@nff@n{@m`B@l�@lj@l(�@k�F@k��@kt�@kS�@j�@j�H@j�\@i�#@i��@i�7@ihs@iX@i�@h�u@hbN@hb@g�@g\)@g+@f�R@fff@e�@e�@eO�@e/@d��@dI�@dI�@d9X@c��@c�m@c�m@c��@cC�@c@bM�@a�@a��@a�^@ahs@aG�@a7L@`��@`�@`Q�@`b@_��@_�w@_��@_K�@_;d@_
=@^�y@^�y@^�y@^�@^ȴ@^�R@^v�@^@]�T@]�-@]`B@]V@\�j@\I�@\(�@[��@[�
@[��@[S�@Z�H@Z�@Y�7@YG�@Y7L@Y7L@Y%@X�9@XQ�@W�@W�P@W;d@V��@V{@U�T@U��@U�-@U�-@U��@U�@U`B@U/@T�j@T(�@SdZ@So@S@R��@R�\@R=q@RJ@Q��@Q�@Q�#@Q�#@Q�^@Q�7@Q�@P��@P1'@O�@O|�@O\)@O;d@N�y@N�+@M�T@M�h@M?}@L�D@Lj@L(�@K�m@Kƨ@K�@KC�@Ko@J�!@JJ@I��@IG�@IG�@H��@Hb@G�;@G|�@GK�@F��@F�@Fȴ@F�R@Fv�@F$�@E��@E`B@D��@DZ@DI�@DI�@D1@C�@CC�@CC�@C@B�!@B~�@B-@A��@A��@A��@Ax�@AG�@A%@@�u@@ �@@  @@  @@  @?�@?�w@?�P@>��@>v�@>V@>5?@=�@=��@=�h@=�h@=p�@=O�@=�@<�D@<1@;�@;@:��@:��@:^5@9�@9�^@8�`@8�9@8�u@8r�@7�@7l�@7;d@7
=@7
=@6��@6ff@5�@5/@4�@4�@4z�@4(�@3dZ@2�@2�\@2M�@2-@1�#@1��@1hs@1X@1�@0��@0�`@0Ĝ@0�@0Q�@0 �@/�@/\)@/+@/
=@.�+@.E�@.5?@.$�@.$�@.{@-�T@-p�@-O�@,�j@,9X@+�
@+S�@+@*�\@*M�@*=q@*�@)��@)x�@)hs@)G�@)�@(��@(�u@(Q�@( �@'�;@'|�@'�@&�@&ff@%��@%��@%�h@%�@%p�@%O�@%/@%V@$�/@$�j@$��@$��@$�D@$�D@$z�@$Z@$9X@#��@#�@#C�@"��@"�\@"^5@!�#@!��@!hs@!7L@!&�@!�@!%@ �`@ �9@ �9@ r�@ A�@ A�@   @|�@�y@ȴ@V@@�T@@�h@`B@?}@V@�@��@�j@��@Z@��@ƨ@ƨ@ƨ@�F@�F@��@��@�@S�@"�@��@�!@��@^5@J@�#@��@7L@�@��@��@�`@�9@��@�u@bN@A�@1'@ �@  @�w@�@��@�P@�P@l�@+@�@��@ȴ@��@�+@$�@@��@p�@�@�/@��@�D@�D@�D@9X@��@�
@ƨ@��@��@C�@�@��@�\@J@�^@��@hs@G�@G�@7L@�`@�@ �@�;@��@��@�w@�w@��@|�@\)@;d@+@�@��@��@E�@@��@@��@`B@?}@/@/@�@��@�m@�F@��@��@t�@@
��@
��@
�\@
~�@
n�@
^5@	�#@	��@	��@	��@	�7@	x�@	hs@	7L@	�@Ĝ@��@bN@A�@ �@��@��@�w@�@�P@l�@;d@�@�y@ȴ@��@E�@$�@��@�-@p�@?}@�@��@��@z�@Z@9X@1@��@ƨ@��@�@dZ@C�@��@�\@n�@n�@n�@n�@^5@=q@-@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�(�A�(�A��A�
=A���A��;A��!A���A�(�A��A���A��A�dZA���A���A�n�A��A���A���A�~�A�dZA�I�A�(�A��A�VA�z�A��7A��^A�$�A��A���A�bA�p�A�G�A���A��uA�+A���A��#A���A�E�A���A���A�=qA��#A��wA�(�A���A�A�A��RA�G�A�XA�I�A���A��7A��wA�JA�/A�O�A�VA�bNA��hA�jA���A�^5A�jA�;dA���A��hA���A�r�A���A��A���A�VA��jA��RA�?}A�  A���A��A���A�A�ĜA�bA�VAK�A~n�A}|�A|^5A{��A{�Ay�Aw�#Au�7As&�Ar��Aq�Ao�-An�Al�HAkAjĜAj=qAiAh�AgG�AfbNAe��Ad�/Ac&�Aa;dA`�uA_A^�A^JA]�PA]oA[�AYC�AX�AX  AWƨAWG�AV�DAU��AU|�AT��AT �ASdZAR��ARVAQ��AQp�AP��AP��AO�#AM��AL��ALr�AL=qAK�TAK��AK�AJQ�AI�-AH�`AG;dAF-AE7LAD9XAC��AC�^AC��ACO�AB�/AB�9AB��AA�A?�A>VA>�A=�#A=��A=l�A=&�A<Q�A:�A:A�A:{A9��A8ffA6�`A5�hA4�jA4I�A3�A3��A3/A1��A0��A/7LA.E�A-�-A,�RA+��A+?}A+�A+�A+
=A*�A)��A(�A({A'XA'%A&�DA&^5A&�A%�-A%VA$��A$n�A#`BA!A 9XA�;A�^A�AoA�
AVA��A�hAz�A=qAAl�AĜA�AXA7LA"�AoA��A7LA��A�+AVAA��A��AS�A
(�A�/AM�A��A;dA�\A�mA�HAA�AA�A�FA bN@��+@��u@��@�l�@�S�@�33@���@�7L@�Q�@�ƨ@��@�ff@�-@�V@�R@���@�bN@땁@�K�@���@�!@ꟾ@�1'@�ȴ@�+@�G�@�-@�z�@��
@���@�x�@ۍP@�M�@ف@�7L@��@�1'@�v�@��@ӝ�@�ȴ@���@�&�@��;@��H@�p�@̃@��@���@ɑh@�Ĝ@ȃ@�9X@��;@�S�@�
=@�@�A�@�M�@�hs@�G�@�7L@���@���@���@���@���@��u@�v�@��T@�x�@��@�I�@�|�@�`B@��m@���@�&�@�  @�M�@���@�1'@��;@��P@���@���@�X@�V@�1'@���@��P@�t�@�+@�V@��#@�r�@��F@��!@�n�@�hs@�&�@���@�I�@��@�t�@���@���@�n�@�V@�E�@�-@��@���@��T@���@��h@��@��u@�9X@��
@�C�@���@���@�`B@��D@�b@��m@�ƨ@��@�S�@��R@�-@���@���@���@�r�@�1'@��@��@��R@�$�@���@�`B@��@��`@��j@��9@���@�r�@�I�@���@�C�@��H@�^5@���@��h@�O�@�bN@���@�|�@�dZ@�o@��+@�^5@�E�@���@��#@�@��^@��^@��^@�@�@��7@��7@�hs@�/@��/@���@��u@��@�z�@�r�@�1'@�  @��w@��F@��F@���@�t�@�o@��!@�J@��-@�O�@��`@��j@���@�Q�@�  @��
@�|�@��R@��+@�n�@�5?@���@���@�x�@��@��7@�p�@�%@�Ĝ@��@�z�@�1@K�@�@~��@~ȴ@~v�@~$�@}O�@|��@|z�@|9X@|�@{ƨ@{t�@{33@{33@z��@z~�@y�@yhs@x�u@x �@w��@w
=@v5?@v@u��@tz�@r�@r~�@r^5@rM�@r-@q��@q%@p��@p��@p�u@p1'@n��@nff@n{@m`B@l�@lj@l(�@k�F@k��@kt�@kS�@j�@j�H@j�\@i�#@i��@i�7@ihs@iX@i�@h�u@hbN@hb@g�@g\)@g+@f�R@fff@e�@e�@eO�@e/@d��@dI�@dI�@d9X@c��@c�m@c�m@c��@cC�@c@bM�@a�@a��@a�^@ahs@aG�@a7L@`��@`�@`Q�@`b@_��@_�w@_��@_K�@_;d@_
=@^�y@^�y@^�y@^�@^ȴ@^�R@^v�@^@]�T@]�-@]`B@]V@\�j@\I�@\(�@[��@[�
@[��@[S�@Z�H@Z�@Y�7@YG�@Y7L@Y7L@Y%@X�9@XQ�@W�@W�P@W;d@V��@V{@U�T@U��@U�-@U�-@U��@U�@U`B@U/@T�j@T(�@SdZ@So@S@R��@R�\@R=q@RJ@Q��@Q�@Q�#@Q�#@Q�^@Q�7@Q�@P��@P1'@O�@O|�@O\)@O;d@N�y@N�+@M�T@M�h@M?}@L�D@Lj@L(�@K�m@Kƨ@K�@KC�@Ko@J�!@JJ@I��@IG�@IG�@H��@Hb@G�;@G|�@GK�@F��@F�@Fȴ@F�R@Fv�@F$�@E��@E`B@D��@DZ@DI�@DI�@D1@C�@CC�@CC�@C@B�!@B~�@B-@A��@A��@A��@Ax�@AG�@A%@@�u@@ �@@  @@  @@  @?�@?�w@?�P@>��@>v�@>V@>5?@=�@=��@=�h@=�h@=p�@=O�@=�@<�D@<1@;�@;@:��@:��@:^5@9�@9�^@8�`@8�9@8�u@8r�@7�@7l�@7;d@7
=@7
=@6��@6ff@5�@5/@4�@4�@4z�@4(�@3dZ@2�@2�\@2M�@2-@1�#@1��@1hs@1X@1�@0��@0�`@0Ĝ@0�@0Q�@0 �@/�@/\)@/+@/
=@.�+@.E�@.5?@.$�@.$�@.{@-�T@-p�@-O�@,�j@,9X@+�
@+S�@+@*�\@*M�@*=q@*�@)��@)x�@)hs@)G�@)�@(��@(�u@(Q�@( �@'�;@'|�@'�@&�@&ff@%��@%��@%�h@%�@%p�@%O�@%/@%V@$�/@$�j@$��@$��@$�D@$�D@$z�@$Z@$9X@#��@#�@#C�@"��@"�\@"^5@!�#@!��@!hs@!7L@!&�@!�@!%@ �`@ �9@ �9@ r�@ A�@ A�@   @|�@�y@ȴ@V@@�T@@�h@`B@?}@V@�@��@�j@��@Z@��@ƨ@ƨ@ƨ@�F@�F@��@��@�@S�@"�@��@�!@��@^5@J@�#@��@7L@�@��@��@�`@�9@��@�u@bN@A�@1'@ �@  @�w@�@��@�P@�P@l�@+@�@��@ȴ@��@�+@$�@@��@p�@�@�/@��@�D@�D@�D@9X@��@�
@ƨ@��@��@C�@�@��@�\@J@�^@��@hs@G�@G�@7L@�`@�@ �@�;@��@��@�w@�w@��@|�@\)@;d@+@�@��@��@E�@@��@@��@`B@?}@/@/@�@��@�m@�F@��@��@t�@@
��@
��@
�\@
~�@
n�@
^5@	�#@	��@	��@	��@	�7@	x�@	hs@	7L@	�@Ĝ@��@bN@A�@ �@��@��@�w@�@�P@l�@;d@�@�y@ȴ@��@E�@$�@��@�-@p�@?}@�@��@��@z�@Z@9X@1@��@ƨ@��@�@dZ@C�@��@�\@n�@n�@n�@n�@^5@=q@-@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B��B��B��B��B��B��B�B�B�-B�^B�}B�qB�XB�RB�RB�XB�XB�RB�?B�9B�B��B�\B�\B�=B��B��B�oB�bB��B��B�uB�hB��B��B�\B�+Bt�BbNBZBZBaHBW
BQ�BN�BA�B5?B!�B\BB
=B�B��B��B�-B��B�uB�DB�bB� Be`B7LB+B:^B9XB!�B�BbB
�B
�;B
��B
�9B
��B
�?B
�3B
��B
��B
��B
�7B
�1B
v�B
bNB
jB
gmB
cTB
T�B
YB
L�B
9XB
+B
VB
JB
{B
	7B	�B	�B	�B	�TB	�TB	�5B	�B	��B	��B	ɺB	��B	��B	�9B	��B	�'B	�B	��B	��B	��B	��B	�PB	p�B	�B	�B	�B	{�B	s�B	s�B	o�B	jB	cTB	dZB	bNB	bNB	cTB	`BB	\)B	VB	L�B	8RB	F�B	H�B	K�B	F�B	C�B	=qB	5?B	2-B	%�B	�B	�B	�B	�B	�B	"�B	!�B	�B	�B	�B	{B	+B�B�B	B	B	B	B��B�B�TB�B�B�`B�B��B��B��B��B��B��BƨB�LB�9B�B�B�B��B��B�'B�'B�-B�!B��B��B��B��B��B��B��B��B��B��B�\B�JB�7Bv�BdZBz�B�B�B}�Bv�BjBl�Br�BiyBffBs�Bp�BiyBdZBcTBdZBl�Bk�BffB[#BD�B<jB?}B:^B6FB49B'�B/B5?B7LBC�BB�B>wB;dB9XB2-B33B1'B,B'�B&�B)�B-B33B9XB9XB7LB49B.B0!B1'B/B/B.B#�B�B	7B�B'�B)�B+B)�B&�B�B�B#�B�B\B�B#�B�B�B�B�B �B$�B"�B�BuB�B{B�B�B�B�B�B{B�B�B�B�B#�B'�B'�B&�B$�B$�B�B�B�B'�B0!B1'B/B-B,B#�B�B%�B!�B0!B33B2-B.B.B&�B-B2-B0!B33B5?B6FBD�BF�BF�BD�BI�BL�BN�BI�BP�BR�BT�BQ�BM�BT�BQ�BZB[#BcTBaHBiyBl�BhsBn�Bm�Bn�Bw�Bw�By�By�By�Bz�By�By�By�Bx�Bw�Bx�B|�B|�B|�B~�B�B�1B�7B�bB��B��B��B��B�uB��B��B��B��B��B��B��B��B�B�B�'B�LB�^B�qB�}B��BBBÖBÖBƨBɺB��B��B��B��B��B�#B�TB�mB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B		7B	PB	VB	VB	PB	PB	\B	hB	�B	�B	�B	!�B	#�B	"�B	#�B	&�B	%�B	%�B	/B	1'B	1'B	2-B	33B	6FB	;dB	>wB	C�B	B�B	E�B	F�B	F�B	F�B	G�B	M�B	N�B	O�B	Q�B	VB	W
B	[#B	]/B	`BB	bNB	bNB	dZB	e`B	ffB	ffB	hsB	iyB	k�B	m�B	p�B	r�B	s�B	v�B	}�B	~�B	|�B	�B	�7B	�DB	�DB	�DB	�=B	�JB	�bB	�oB	�hB	�hB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�-B	�?B	�FB	�LB	�LB	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�5B	�5B	�BB	�BB	�HB	�NB	�ZB	�ZB	�fB	�mB	�fB	�mB	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
+B
%B
+B
1B

=B
	7B
1B
JB
JB
PB
PB
VB
\B
VB
VB
VB
VB
\B
VB
oB
uB
uB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
"�B
"�B
#�B
"�B
&�B
&�B
&�B
%�B
&�B
(�B
)�B
)�B
)�B
'�B
&�B
+B
,B
,B
,B
,B
+B
-B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
2-B
2-B
2-B
2-B
2-B
49B
49B
33B
5?B
7LB
7LB
7LB
6FB
5?B
5?B
6FB
5?B
6FB
7LB
7LB
9XB
9XB
:^B
<jB
;dB
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
@�B
A�B
B�B
B�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
C�B
C�B
C�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
K�B
L�B
N�B
N�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
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
Q�B
S�B
S�B
S�B
S�B
S�B
R�B
T�B
T�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
VB
VB
W
B
XB
XB
W
B
W
B
W
B
XB
XB
W
B
XB
ZB
ZB
ZB
[#B
ZB
YB
YB
ZB
[#B
]/B
]/B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
^5B
aHB
bNB
bNB
bNB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
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
gmB
gmB
hsB
hsB
hsB
iyB
iyB
jB
iyB
iyB
iyB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�"B�0B�0B�DB�`B�8B�B��B�B��B��B�B��B��B�B��B��B��B��B��B��B�nB��B�HB��B� B�^B��B��B��B��B��B�EB�aB�TB��B��B�B�Bv`Bd&B[WB[	Ba�BX_BS&BO�BB�B6�B$B�B�B
�B�B�mBÖB�nB�VB�B�B��B�UBg�B;�B-�B;0B:*B#nB�B�B
�9B
�B
��B
��B
�WB
�FB
��B
�B
��B
��B
��B
�B
x�B
d�B
k�B
h�B
dtB
VmB
Y�B
NB
;dB
-)B
NB
�B
MB

�B	�nB	�qB	�B	��B	�tB	�!B	�B	�&B	��B	��B	̘B	��B	�FB	� B	��B	�)B	�,B	��B	��B	��B	�(B	s�B	��B	��B	�oB	|�B	t�B	tnB	pUB	kkB	dZB	eFB	c B	cB	c�B	`�B	\�B	V�B	N"B	:�B	GzB	I7B	LB	G+B	DB	>(B	6`B	33B	'B	�B	�B	�B	�B	5B	"�B	"B	/B	1B	�B	�B	�B�B�[B	MB	gB	{B	oB��B��B��B�cB�B�LB��B��B�vB��BӏB�hB�pBǔB�>B��B��B�OB�B�QB�2B�vB�AB�GB��B��B��B��B��B�B�;B�CB��B�B�9B�HB�B��Bx�BgB{�B�uB�[B~wBw�Bl=Bm�Bs�Bj�Bg�Bs�BqBjeBeFBdZBe,Bl�Bk�Bf�B[�BF�B>BBA;B<jB88B6B*eB0�B6�B8�BDgBCGB?HB<jB:xB3hB4B2B-wB)�B(�B+kB./B3�B9rB9rB7�B4�B/B0�B1�B/�B/�B.�B%BqBB�B(XB*KB+QB*0B'mB#B�B$ZB�BNB�B$ZB�B�B�B�B!bB%,B#:BxB�BmBgB5BdBCBBEB�B/B;BqB�B$@B(>B(>B'RB%`B%FB�B�B�B(XB0;B1[B/OB-]B,WB$�BB&�B#:B0�B3�B2�B.�B.�B(XB./B2�B1[B49B6zB7fBD�BF�BGBESBJ#BMPBO(BJXBQBS@BUBRTBN�BU�BSBZ�B[�Bc�BbBi�Bl�Bh�Bn�BnBoBxBxBy�By�By�Bz�BzBzBzBy$BxByXB}<B}VB}qB}B��B��B��B��B��B��B��B��B��B��B�B�/B�B�B�B�2B�RB�kB��B��B��B��B��B�}B��BªBªB��B��B�B�#B�B�<B�@B�MBөBیB�nB�B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�0B�<B	 4B	-B	B	3B	3B	GB	YB		lB	PB	pB	VB	�B	�B	�B	�B	�B	�B	B	!�B	$B	# B	$B	'B	&2B	&�B	/5B	1'B	1[B	2aB	3hB	6`B	;dB	>wB	C�B	B�B	E�B	F�B	F�B	F�B	G�B	M�B	N�B	O�B	R B	V9B	WYB	[WB	]dB	`\B	bNB	bhB	dtB	ezB	f�B	f�B	h�B	i�B	k�B	m�B	p�B	r�B	tB	v�B	~(B	B	}qB	��B	�RB	�^B	�^B	�DB	�rB	�~B	�}B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�0B	�;B	�GB	�ZB	�`B	�LB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ðB	��B	ĶB	ƨB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	� B	��B	�B	�$B	�+B	�7B	�7B	�=B	�/B	�IB	�5B	�;B	�;B	�VB	�;B	�VB	�5B	�jB	�BB	�\B	�bB	�B	�tB	�tB	�B	�mB	�B	�mB	�B	�B	�B	�B	�B	�B	�wB	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�B
 B
 B
 B
  B	�.B
 4B
;B
GB
[B
9B
9B
9B
?B
?B
+B
EB
_B
YB
_B
KB

XB
	lB
�B
dB
dB
jB
jB
pB
\B
VB
pB
pB
�B
�B
�B
�B
[B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
"�B
#B
#�B
# B
&�B
'B
'B
%�B
'B
)B
*B
)�B
)�B
(>B
'8B
+B
,B
,"B
,"B
,=B
+6B
-)B
./B
/5B
0!B
0;B
0;B
1AB
2GB
2GB
3MB
3MB
3MB
2GB
2GB
2GB
2GB
2GB
49B
49B
3hB
5ZB
7LB
7LB
7LB
6`B
5ZB
5tB
6`B
5tB
6zB
7�B
7fB
9rB
9�B
:xB
<jB
;dB
:�B
<jB
<jB
<�B
<�B
<�B
<�B
<�B
=�B
=�B
<�B
=�B
>wB
>�B
>�B
@�B
AoB
B�B
B�B
A�B
B�B
B�B
B�B
B�B
C�B
C{B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
C�B
C�B
C�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
K�B
K�B
L�B
N�B
N�B
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
N�B
N�B
N�B
N�B
OB
O�B
O�B
P�B
P�B
Q B
Q�B
P�B
Q B
Q�B
Q�B
RB
RB
Q�B
RB
R�B
R�B
R�B
R�B
RB
TB
TB
TB
S�B
TB
SB
T�B
UB
T,B
TB
UB
VB
W
B
W
B
W
B
VB
VB
W$B
XB
X+B
W$B
W
B
W$B
X+B
X+B
W?B
XB
ZB
Z7B
Z7B
[#B
Z7B
Y1B
YKB
Z7B
[=B
]B
]/B
]/B
]B
\)B
]/B
]IB
]IB
]B
]IB
]/B
\CB
\CB
]/B
_VB
_VB
_VB
_VB
_VB
`'B
`BB
_VB
_pB
^OB
aHB
b4B
bhB
bhB
a|B
cTB
cTB
cnB
cTB
cnB
cTB
b�B
dZB
e`B
e`B
e`B
e`B
ezB
dtB
dtB
dtB
ezB
ezB
f�B
ffB
f�B
gmB
gmB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
gmB
g�B
h�B
h�B
h�B
i�B
iyB
jeB
i�B
i�B
i�B
jB
k�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
mwB
n}B
n}B
n}B
n}B
n�B
n}B
n�B
n�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804090038442018040900384420180409003844201806221240022018062212400220180622124002201804271405212018042714052120180427140521  JA  ARFMdecpA19c                                                                20180405093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180405003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180405003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180405003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180405003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180405003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180405003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180405003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180405003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180405003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180405005632                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180405153240  CV  JULD            G�O�G�O�F�Ů                JM  ARCAJMQC2.0                                                                 20180408153844  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180408153844  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050521  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034002  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                