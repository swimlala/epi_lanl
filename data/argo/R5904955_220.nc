CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140749  20181024140749  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @��d�6�61   @��e[�@5ȴ9X�c���l�D1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�\D�@�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=Cp#�Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�B>D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɅA�t�A�r�A�~�Aɏ\AɍPAɕ�AɑhAɃAɍPAɋDAɋDAɇ+Aɏ\Aə�Aɰ!Aɲ-Aɺ^AɾwA���A��TA�A�{A�$�A�^5A���A˧�A�dZA�E�A�-A� �A��A�{A�
=A�A���A��A��A�ƨA�S�A��Aʩ�A�n�A�
=A�JA�Q�A�ȴA��A�\)A��mA�%A��A���A���A�?}A��A�r�A��hA��PA�~�A��A�VA��A�`BA�x�A�\)A�VA�|�A���A�33A�7LA���A�^5A�r�A��A���A���A�n�A�v�A��A�l�A���A��HA��FA���A�VA�hsA���A���A�S�A�ffA��#A�{A�VA��/A���A�hsA�x�A���A���A�?}A��A~�A{?}Av�At{AsƨAs�-As7LAn��Ak��Aj�Ah1'Ad��AcO�AbM�A`�A]dZA\n�A\ffA\1A[�-A[+AY`BAU��AS�;AR�uAOp�AMS�ALn�AK�TAK�AK`BAK33AJ�`AI��AE��AD��AD�RAD��AD(�AC�ABĜAB-AA�A?�A?�^A<�RA:ffA9�PA7%A5p�A3p�A21A/�A,��A+�TA*VA)C�A(��A( �A'oA&��A%?}A$ZA$-A#ƨA"�uA!C�A ZAt�A�/A1AA�AA�A�yA�DA�A�+A��Al�A�\A��A�-AhsA��A�9A-AȴA�DA`BA�+AhsAn�A�A�hA�A
��A
��A
(�A	/A$�A�`A�
A{A��Ax�A33A �!@�33@�G�@�S�@���@���@�r�@��@�o@�ȴ@��@��@���@�-@�|�@�5?@�`B@�@�I�@�w@�C�@�ȴ@�@��@��@���@��@�A�@�n�@�A�@��m@�X@�o@���@�v�@�{@ٙ�@��/@�"�@���@��@�t�@��@��T@��m@���@�/@ʰ!@�5?@��@��/@�Z@Ǿw@��@�=q@š�@�?}@��`@��`@ļj@�1@î@ÍP@�dZ@���@�$�@�@�J@�@�?}@���@�I�@���@�l�@�S�@�+@��@��\@��+@�V@��@���@���@���@��h@���@��7@��9@���@�;d@�5?@��#@���@�hs@��@��D@��@�S�@��@��@�5?@�=q@�M�@�V@�V@�V@��^@� �@��;@�K�@�"�@�@��R@�J@��7@��/@�A�@���@��@�E�@��@���@�\)@�n�@�$�@�p�@��@��j@���@���@�dZ@�o@�ȴ@�E�@���@��h@�x�@�`B@�`B@�X@�?}@���@��j@��@�Z@�I�@�1'@� �@���@��@��;@��w@���@�\)@��@�ȴ@���@��+@�v�@�5?@�@���@��@���@��`@���@��j@��9@��@�A�@�b@���@�l�@�l�@�C�@���@�n�@���@�x�@�&�@��@��@�bN@�9X@��@���@��P@�C�@��@���@���@���@���@�v�@�^5@�M�@��@���@�&�@��@�Ĝ@�z�@�9X@�b@���@��;@�ƨ@��F@��F@��F@��@��@�\)@�K�@�+@�"�@�o@���@�ȴ@��R@���@��\@�5?@��T@��-@�X@��@���@�Z@�9X@�(�@��
@��P@�;d@�@���@���@�ff@�$�@���@�hs@�O�@�?}@�&�@��`@�bN@� �@��@�33@��@���@��+@�^5@�^5@�V@�E�@�-@��@��@���@��#@�@�@�@��-@�x�@�&�@��/@��D@�A�@�1'@��@�1@��;@�t�@�33@���@�ȴ@��R@���@�n�@�^5@�V@�-@��T@��-@��@��P@xu�@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AɅA�t�A�r�A�~�Aɏ\AɍPAɕ�AɑhAɃAɍPAɋDAɋDAɇ+Aɏ\Aə�Aɰ!Aɲ-Aɺ^AɾwA���A��TA�A�{A�$�A�^5A���A˧�A�dZA�E�A�-A� �A��A�{A�
=A�A���A��A��A�ƨA�S�A��Aʩ�A�n�A�
=A�JA�Q�A�ȴA��A�\)A��mA�%A��A���A���A�?}A��A�r�A��hA��PA�~�A��A�VA��A�`BA�x�A�\)A�VA�|�A���A�33A�7LA���A�^5A�r�A��A���A���A�n�A�v�A��A�l�A���A��HA��FA���A�VA�hsA���A���A�S�A�ffA��#A�{A�VA��/A���A�hsA�x�A���A���A�?}A��A~�A{?}Av�At{AsƨAs�-As7LAn��Ak��Aj�Ah1'Ad��AcO�AbM�A`�A]dZA\n�A\ffA\1A[�-A[+AY`BAU��AS�;AR�uAOp�AMS�ALn�AK�TAK�AK`BAK33AJ�`AI��AE��AD��AD�RAD��AD(�AC�ABĜAB-AA�A?�A?�^A<�RA:ffA9�PA7%A5p�A3p�A21A/�A,��A+�TA*VA)C�A(��A( �A'oA&��A%?}A$ZA$-A#ƨA"�uA!C�A ZAt�A�/A1AA�AA�A�yA�DA�A�+A��Al�A�\A��A�-AhsA��A�9A-AȴA�DA`BA�+AhsAn�A�A�hA�A
��A
��A
(�A	/A$�A�`A�
A{A��Ax�A33A �!@�33@�G�@�S�@���@���@�r�@��@�o@�ȴ@��@��@���@�-@�|�@�5?@�`B@�@�I�@�w@�C�@�ȴ@�@��@��@���@��@�A�@�n�@�A�@��m@�X@�o@���@�v�@�{@ٙ�@��/@�"�@���@��@�t�@��@��T@��m@���@�/@ʰ!@�5?@��@��/@�Z@Ǿw@��@�=q@š�@�?}@��`@��`@ļj@�1@î@ÍP@�dZ@���@�$�@�@�J@�@�?}@���@�I�@���@�l�@�S�@�+@��@��\@��+@�V@��@���@���@���@��h@���@��7@��9@���@�;d@�5?@��#@���@�hs@��@��D@��@�S�@��@��@�5?@�=q@�M�@�V@�V@�V@��^@� �@��;@�K�@�"�@�@��R@�J@��7@��/@�A�@���@��@�E�@��@���@�\)@�n�@�$�@�p�@��@��j@���@���@�dZ@�o@�ȴ@�E�@���@��h@�x�@�`B@�`B@�X@�?}@���@��j@��@�Z@�I�@�1'@� �@���@��@��;@��w@���@�\)@��@�ȴ@���@��+@�v�@�5?@�@���@��@���@��`@���@��j@��9@��@�A�@�b@���@�l�@�l�@�C�@���@�n�@���@�x�@�&�@��@��@�bN@�9X@��@���@��P@�C�@��@���@���@���@���@�v�@�^5@�M�@��@���@�&�@��@�Ĝ@�z�@�9X@�b@���@��;@�ƨ@��F@��F@��F@��@��@�\)@�K�@�+@�"�@�o@���@�ȴ@��R@���@��\@�5?@��T@��-@�X@��@���@�Z@�9X@�(�@��
@��P@�;d@�@���@���@�ff@�$�@���@�hs@�O�@�?}@�&�@��`@�bN@� �@��@�33@��@���@��+@�^5@�^5@�V@�E�@�-@��@��@���@��#@�@�@�@��-@�x�@�&�@��/@��D@�A�@�1'@��@�1@��;@�t�@�33@���@�ȴ@��R@���@�n�@�^5@�V@�-@��T@��-@��@��P@xu�@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B33B33B49B6FB5?B7LB6FB49B5?B5?B49B33B5?B8RB=qB>wBB�BC�BJ�BS�B`BBiyBq�B�1B�XB�B1'B33B33B5?B7LB8RB<jB@�BA�BA�BB�BE�BM�BS�BW
BYBXBQ�BB�B9XB7LB;dB:^B6FB9XB:^B>wB@�BC�BF�BG�BP�BT�Bq�B�B�JB�PB��B��B��B��B��B��B��B��B�uB�BffBI�B>wB6FB+B�BDBĜB��B�oB}�Bx�BaHBQ�BE�B33B$�B�B
�B
�B
��B
��B
�DB
{�B
t�B
q�B
hsB
G�B
 �B
+B	�B	�)B	�B	�B	�
B	ǮB	�B	��B	�uB	�B	y�B	s�B	jB	[#B	T�B	S�B	Q�B	O�B	K�B	@�B	0!B	'�B	 �B	�B	VB	DB	1B	+B	%B	B	B��B�B�B�B�B�B�B�yB�sB�fB�HB�5B��BȴBĜB�^B�?B�B��B��B��B��B�{B�{B�uB�oB�oB�bB�bB�\B�VB�PB�PB�PB�DB�DB�DB�PB�\B�JB�+B�B� B� B� B~�B�B�B�B�B�B�B�B�B�B}�B}�B�B�B}�B}�B�B�B�B�B�B�1B�\B�uB�uB�JB�B�B�B}�Bx�Bw�Bz�B}�B�B�B�B�B�By�B�B�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�3B�9B�9B�?B�FB�^B��B��BBÖBÖBǮB��B��B�#B�)B�5B�BB�ZB�`B�sB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	1B		7B		7B	
=B	JB	PB	PB	VB	bB	hB	oB	oB	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	#�B	"�B	$�B	"�B	"�B	$�B	&�B	'�B	)�B	-B	1'B	7LB	8RB	9XB	>wB	I�B	J�B	I�B	N�B	R�B	T�B	ZB	\)B	`BB	e`B	gmB	hsB	l�B	m�B	p�B	r�B	t�B	u�B	w�B	w�B	w�B	x�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�JB	�\B	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�FB	�LB	�LB	�LB	�LB	�RB	�RB	�^B	�jB	�jB	�qB	�wB	�wB	��B	��B	��B	B	ÖB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
�B
"N1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B33B33B33B49B6FB5?B7LB6FB49B5?B5?B49B33B5?B8RB=qB>wBB�BC�BJ�BS�B`BBiyBq�B�1B�XB�B1'B33B33B5?B7LB8RB<jB@�BA�BA�BB�BE�BM�BS�BW
BYBXBQ�BB�B9XB7LB;dB:^B6FB9XB:^B>wB@�BC�BF�BG�BP�BT�Bq�B�B�JB�PB��B��B��B��B��B��B��B��B�uB�BffBI�B>wB6FB+B�BDBĜB��B�oB}�Bx�BaHBQ�BE�B33B$�B�B
�B
�B
��B
��B
�DB
{�B
t�B
q�B
hsB
G�B
 �B
+B	�B	�)B	�B	�B	�
B	ǮB	�B	��B	�uB	�B	y�B	s�B	jB	[#B	T�B	S�B	Q�B	O�B	K�B	@�B	0!B	'�B	 �B	�B	VB	DB	1B	+B	%B	B	B��B�B�B�B�B�B�B�yB�sB�fB�HB�5B��BȴBĜB�^B�?B�B��B��B��B��B�{B�{B�uB�oB�oB�bB�bB�\B�VB�PB�PB�PB�DB�DB�DB�PB�\B�JB�+B�B� B� B� B~�B�B�B�B�B�B�B�B�B�B}�B}�B�B�B}�B}�B�B�B�B�B�B�1B�\B�uB�uB�JB�B�B�B}�Bx�Bw�Bz�B}�B�B�B�B�B�By�B�B�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�3B�9B�9B�?B�FB�^B��B��BBÖBÖBǮB��B��B�#B�)B�5B�BB�ZB�`B�sB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	1B		7B		7B	
=B	JB	PB	PB	VB	bB	hB	oB	oB	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	#�B	"�B	$�B	"�B	"�B	$�B	&�B	'�B	)�B	-B	1'B	7LB	8RB	9XB	>wB	I�B	J�B	I�B	N�B	R�B	T�B	ZB	\)B	`BB	e`B	gmB	hsB	l�B	m�B	p�B	r�B	t�B	u�B	w�B	w�B	w�B	x�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�JB	�\B	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�FB	�LB	�LB	�LB	�LB	�RB	�RB	�^B	�jB	�jB	�qB	�wB	�wB	��B	��B	��B	B	ÖB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�BB	�HB	�TB	�ZB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
�B
"N1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140749                              AO  ARCAADJP                                                                    20181024140749    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140749  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140749  QCF$                G�O�G�O�G�O�0               