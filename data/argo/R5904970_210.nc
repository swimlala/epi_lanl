CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:42Z creation      
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
_FillValue                 �  A    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Jt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  LX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  S�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ]\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  n`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �@   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141542  20181024141542  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @�餖�~31   @��%��@77���+�dbM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D3��D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Dy�)D�9�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@B�\@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B��HB�{B�{B�{C 
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D3�)D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Dy��D�:�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�E�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�VA�A�A�(�A��A���A���A�?}A��#A��A�oA�{A���A�ȴA��-A�|�A�XA�33A��A��!A�z�A�bNA�\)A�\)A�bNA�bNA�M�A�?}A�"�A��A�%A���A�VA���A��A�~�A�{A�ȴA���A��DA��A���A��`A��hA��A��9A���A��A�\)A�(�A��FA��A�t�A�bA�+A�33A��hA���A�"�A��/A�`BA���A���A��A���A���A��9A�=qA�
=A�?}A��\A��
A�9XA��wA�?}A���A�A��A��\A�33A�{A��A�/A��A�A�A�ZA��-A���A�ZA��`A�;dA�-A��+A�  A���A�33A���A��PA���A�wA~E�A{�7Ay�;Awl�At�As�Ar�!ArE�Ar{Aq�Aq;dAodZAl��Ak�Ai�-Agx�Ag�Af �Ad�jA`��A_"�A\�+AZ�AYK�AW�wAV�9AT�uAR�AQ�hAP��AN��AM��AM�FAK��AJZAIhsAHv�AFȴAEl�ADVAC�AB5?AA7LA@(�A?33A<��A;%A:{A9+A85?A6��A6  A4��A2�A2$�A1�hA0�!A/`BA-��A-oA,n�A+C�A)�
A'`BA&��A&n�A%�7A$5?A#hsA#%A!��AĜA�;A;dA~�A��A�7A�HA��AJA��A�A�^Ax�A�A�DA��A+A�;A?}A��A�A��AQ�A  A�FAl�A��AI�A�A	�
A��Ap�A��A�#A��A��A�AS�A �D@��H@���@��@�j@�~�@��-@��j@�dZ@�7L@�1'@�\@�j@�~�@�D@��@�h@�O�@�7L@��@��#@��@��u@��u@�Q�@��
@ߕ�@���@�v�@ާ�@�C�@�33@��@��/@؋D@�=q@�G�@�(�@�o@��@��/@�+@�-@�^5@�v�@Ώ\@�v�@ա�@��@��@҇+@��@�X@�=q@�|�@�5?@��y@�A�@ǶF@�\)@�V@§�@��@��`@�j@��@���@���@�/@�Q�@�1@��m@҇+@��@�X@�=q@�|�@�5?@��y@�A�@ǶF@�\)@�V@§�@��@��`@�j@��@���@���@�/@�Q�@�1@��m@��
@���@�=q@��#@�G�@�Z@���@��u@���@���@��T@�^5@�M�@�V@�`B@�\)@��@���@�X@�Ĝ@�"�@�^5@�V@���@��@��y@�V@��@���@�7L@��@��u@�9X@�b@��F@�;d@��@��R@��+@�V@�V@�v�@��-@���@��;@���@�;d@�@�ȴ@�n�@��@�X@��@��/@��j@���@��m@�K�@��@��@�C�@�9X@�Z@���@���@�Q�@�b@�1@�Q�@��
@�ƨ@��F@��@�K�@�{@��D@�j@��D@��@�S�@�V@�n�@��+@��^@��9@�b@�|�@�=q@�@�E�@��H@��R@���@�n�@�E�@�=q@���@�5?@�5?@��H@��R@�$�@��@��@���@�p�@�7L@�Ĝ@�A�@��P@��H@��@�@��7@�p�@��@��`@���@���@���@�Ĝ@��@� �@�1'@�b@��m@��F@���@��@��P@��P@�33@��y@��R@�E�@�=q@���@��@��@���@�J@��@���@�Q�@��
@��@��
@��P@�t�@��H@��R@���@���@��R@�V@���@���@�G�@�o@z
�@gK�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�E�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�\)A�\)A�VA�A�A�(�A��A���A���A�?}A��#A��A�oA�{A���A�ȴA��-A�|�A�XA�33A��A��!A�z�A�bNA�\)A�\)A�bNA�bNA�M�A�?}A�"�A��A�%A���A�VA���A��A�~�A�{A�ȴA���A��DA��A���A��`A��hA��A��9A���A��A�\)A�(�A��FA��A�t�A�bA�+A�33A��hA���A�"�A��/A�`BA���A���A��A���A���A��9A�=qA�
=A�?}A��\A��
A�9XA��wA�?}A���A�A��A��\A�33A�{A��A�/A��A�A�A�ZA��-A���A�ZA��`A�;dA�-A��+A�  A���A�33A���A��PA���A�wA~E�A{�7Ay�;Awl�At�As�Ar�!ArE�Ar{Aq�Aq;dAodZAl��Ak�Ai�-Agx�Ag�Af �Ad�jA`��A_"�A\�+AZ�AYK�AW�wAV�9AT�uAR�AQ�hAP��AN��AM��AM�FAK��AJZAIhsAHv�AFȴAEl�ADVAC�AB5?AA7LA@(�A?33A<��A;%A:{A9+A85?A6��A6  A4��A2�A2$�A1�hA0�!A/`BA-��A-oA,n�A+C�A)�
A'`BA&��A&n�A%�7A$5?A#hsA#%A!��AĜA�;A;dA~�A��A�7A�HA��AJA��A�A�^Ax�A�A�DA��A+A�;A?}A��A�A��AQ�A  A�FAl�A��AI�A�A	�
A��Ap�A��A�#A��A��A�AS�A �D@��H@���@��@�j@�~�@��-@��j@�dZ@�7L@�1'@�\@�j@�~�@�D@��@�h@�O�@�7L@��@��#@��@��u@��u@�Q�@��
@ߕ�@���@�v�@ާ�@�C�@�33@��@��/@؋D@�=q@�G�@�(�@�o@��@��/@�+@�-@�^5@�v�@Ώ\@�v�@ա�@��@��@҇+@��@�X@�=q@�|�@�5?@��y@�A�@ǶF@�\)@�V@§�@��@��`@�j@��@���@���@�/@�Q�@�1@��m@҇+@��@�X@�=q@�|�@�5?@��y@�A�@ǶF@�\)@�V@§�@��@��`@�j@��@���@���@�/@�Q�@�1@��m@��
@���@�=q@��#@�G�@�Z@���@��u@���@���@��T@�^5@�M�@�V@�`B@�\)@��@���@�X@�Ĝ@�"�@�^5@�V@���@��@��y@�V@��@���@�7L@��@��u@�9X@�b@��F@�;d@��@��R@��+@�V@�V@�v�@��-@���@��;@���@�;d@�@�ȴ@�n�@��@�X@��@��/@��j@���@��m@�K�@��@��@�C�@�9X@�Z@���@���@�Q�@�b@�1@�Q�@��
@�ƨ@��F@��@�K�@�{@��D@�j@��D@��@�S�@�V@�n�@��+@��^@��9@�b@�|�@�=q@�@�E�@��H@��R@���@�n�@�E�@�=q@���@�5?@�5?@��H@��R@�$�@��@��@���@�p�@�7L@�Ĝ@�A�@��P@��H@��@�@��7@�p�@��@��`@���@���@���@�Ĝ@��@� �@�1'@�b@��m@��F@���@��@��P@��P@�33@��y@��R@�E�@�=q@���@��@��@���@�J@��@���@�Q�@��
@��@��
@��P@�t�@��H@��R@���@���@��R@�V@���@���@�G�@�o@z
�@gK�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bo�Bp�Bp�Bp�Bs�Bx�B|�B~�B�B�B�B�B�PB��B��B��B��B�B�!B�3B�9B�3B�'B�dBBȴB��B��B��B�5B�ZB�B�B�B�B��B�B��B��B�jB�LB�?B�LB��B�B  B�fBƨB��B�}B�qB�dB�RB�?B�B��B��B�DB� BcTBXBQ�BM�BK�BB�B;dB33B,B"�B��B�B�;B��B��B��B�fB�B�`B�#BɺB�dB�9B�B��B�BcTBL�B:^B.B"�B�B{BDBB
�B
�NB
�/B
�
B
��B
��B
�qB
�B
��B
��B
�%B
v�B
hsB
N�B
D�B
B�B
>wB
<jB
;dB
5?B
%�B
uB
1B
  B	�B	�B	�ZB	�B	�wB	�B	��B	��B	�JB	�B	z�B	q�B	hsB	`BB	\)B	R�B	I�B	G�B	:^B	0!B	'�B	!�B	�B	uB	VB		7B	B	  B��B��B�B�TB�5B�B�B��B��BȴBB�wB�jB�LB�?B�B�B��B��B��B��B�hB�\B�VB�1B�%B�B�Bx�Bt�Bs�Bq�Bp�Bo�Bp�Bq�Bu�Bv�Bt�Bt�BiyBgmBgmBe`BbNB`BB]/B]/B]/B]/BYBW
BVBVBS�BQ�BO�BM�BI�BE�BD�BA�B@�B@�B>wB=qB<jB;dB9XB:^B8RB6FB33B33B33B2-B1'B0!B/B.B.B-B-B,B+B+B.B49B;dB=qB;dB;dB<jB;dB:^B;dB?}B=qB7LB?}BF�BA�B>wB;dB9XB:^B<jB>wB?}BB�BC�BE�BaHB�Bx�Bs�B�B�VB�7B� BhsB|�B�bB�VB�hB�\B�DB� By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bo�Bp�B�B�VB�7B� BhsB|�B�bB�VB�hB�\B�DB� By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bo�Bp�Bp�Bp�Bo�Bn�Bu�B{�B�B�uB��B��B�B�3B�?B�?B�?B�-B�'B�'B�-B�FB�^B�^B�FB�-B�!B�B�B�B�'B�-B�9B�FB�RB�dB�wB��BBŢBǮB��B��B��B��B��B��B��B��B��B�B�B�B�/B�;B�;B�HB�mB�B�B�B��B��B	%B	JB	�B	"�B	'�B	)�B	1'B	7LB	=qB	?}B	A�B	C�B	B�B	?}B	<jB	@�B	I�B	K�B	J�B	I�B	J�B	L�B	K�B	J�B	I�B	G�B	E�B	F�B	L�B	T�B	T�B	W
B	YB	XB	YB	`BB	e`B	jB	q�B	t�B	x�B	x�B	z�B	{�B	}�B	� B	� B	�B	�B	�B	�%B	�1B	�1B	�=B	�7B	�=B	�DB	�DB	�JB	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�!B	�B	�!B	�-B	�?B	�FB	�FB	�FB	�RB	�dB	�dB	�dB	�qB	�wB	�qB	�}B	�@B
 �B

1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bp�Bo�Bp�Bp�Bp�Bs�Bx�B|�B~�B�B�B�B�B�PB��B��B��B��B�B�!B�3B�9B�3B�'B�dBBȴB��B��B��B�5B�ZB�B�B�B�B��B�B��B��B�jB�LB�?B�LB��B�B  B�fBƨB��B�}B�qB�dB�RB�?B�B��B��B�DB� BcTBXBQ�BM�BK�BB�B;dB33B,B"�B��B�B�;B��B��B��B�fB�B�`B�#BɺB�dB�9B�B��B�BcTBL�B:^B.B"�B�B{BDBB
�B
�NB
�/B
�
B
��B
��B
�qB
�B
��B
��B
�%B
v�B
hsB
N�B
D�B
B�B
>wB
<jB
;dB
5?B
%�B
uB
1B
  B	�B	�B	�ZB	�B	�wB	�B	��B	��B	�JB	�B	z�B	q�B	hsB	`BB	\)B	R�B	I�B	G�B	:^B	0!B	'�B	!�B	�B	uB	VB		7B	B	  B��B��B�B�TB�5B�B�B��B��BȴBB�wB�jB�LB�?B�B�B��B��B��B��B�hB�\B�VB�1B�%B�B�Bx�Bt�Bs�Bq�Bp�Bo�Bp�Bq�Bu�Bv�Bt�Bt�BiyBgmBgmBe`BbNB`BB]/B]/B]/B]/BYBW
BVBVBS�BQ�BO�BM�BI�BE�BD�BA�B@�B@�B>wB=qB<jB;dB9XB:^B8RB6FB33B33B33B2-B1'B0!B/B.B.B-B-B,B+B+B.B49B;dB=qB;dB;dB<jB;dB:^B;dB?}B=qB7LB?}BF�BA�B>wB;dB9XB:^B<jB>wB?}BB�BC�BE�BaHB�Bx�Bs�B�B�VB�7B� BhsB|�B�bB�VB�hB�\B�DB� By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bo�Bp�B�B�VB�7B� BhsB|�B�bB�VB�hB�\B�DB� By�Bx�Bx�Bw�Bu�Bs�Bq�Bp�Bo�Bp�Bp�Bp�Bo�Bn�Bu�B{�B�B�uB��B��B�B�3B�?B�?B�?B�-B�'B�'B�-B�FB�^B�^B�FB�-B�!B�B�B�B�'B�-B�9B�FB�RB�dB�wB��BBŢBǮB��B��B��B��B��B��B��B��B��B�B�B�B�/B�;B�;B�HB�mB�B�B�B��B��B	%B	JB	�B	"�B	'�B	)�B	1'B	7LB	=qB	?}B	A�B	C�B	B�B	?}B	<jB	@�B	I�B	K�B	J�B	I�B	J�B	L�B	K�B	J�B	I�B	G�B	E�B	F�B	L�B	T�B	T�B	W
B	YB	XB	YB	`BB	e`B	jB	q�B	t�B	x�B	x�B	z�B	{�B	}�B	� B	� B	�B	�B	�B	�%B	�1B	�1B	�=B	�7B	�=B	�DB	�DB	�JB	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�!B	�B	�!B	�-B	�?B	�FB	�FB	�FB	�RB	�dB	�dB	�dB	�qB	�wB	�qB	�}B	�@B
 �B

1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141542                              AO  ARCAADJP                                                                    20181024141542    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141542  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141542  QCF$                G�O�G�O�G�O�4000            