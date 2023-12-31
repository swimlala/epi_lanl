CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:10Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190610  20181005190610  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @� ���\�1   @� �UUjF@1У�
=q�c��-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @9��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  Dy�D  D� D  D�fD  D� D��Dy�D  D� D  D� D��D� D	  D	� D
fD
� D  Dy�D  D� D��Dy�D  D� D  Dy�D��Dy�D��D� D��D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D��D� D   D � D ��D!� D"  D"� D#  D#y�D$  D$� D%  D%y�D%��D&� D'  D'� D'��D(� D)  D)�fD*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D.��D/y�D/��D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D;��D<� D=fD=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DDfDD�fDE  DE�fDFfDF�fDGfDG� DG��DHy�DH��DIy�DJ  DJ� DK  DK� DL  DL�fDMfDM�fDN  DNy�DN��DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DS��DT� DUfDU� DV  DV� DV��DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Da��Dby�Db��Dc� DdfDd�fDe  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn�fDo  Doy�Dp  Dp� Dq  Dq�fDrfDr�fDsfDs� Ds��Dt� DufDu� Dv  Dv� Dw  Dw�fDwٚDy��D�NfD�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @L(�@�G�@�G�A��A&=qAD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B8B@BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B�ǮB�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{B�aHB̔{BД{BԔ{B�ǮB�ǮB�ǮB�{B�{B�{B�{B��{B��{B�aHC J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C60�C8J=C:J=C<J=C>J=C@J=CBc�CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�1�C�1�C�1�C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�RC�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�1�C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D�)D�D��D�D��D�D��D)D�)D�D��D�D��D)D��D	�D	��D
�D
��D�D�)D�D��D)D�)D�D��D�D�)D)D�)D)D��D)D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D�)D�D��D�D��D)D��D �D ��D!)D!��D"�D"��D#�D#�)D$�D$��D%�D%�)D&)D&��D'�D'��D()D(��D)�D)��D*�D*�)D+�D+��D,�D,��D-�D-��D.�D.��D/)D/�)D0)D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<)D<��D=�D=��D>�D>��D?�D?��D@�D@��DA)DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH)DH�)DI)DI�)DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN�)DO)DO��DP�DP��DQ�DQ�)DR�DR��DS�DS��DT)DT��DU�DU��DV�DV��DW)DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da�)Db)Db�)Dc)Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�)Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt)Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�)Dy�)D�W�D�iH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʡ�Aʛ�Aʛ�Aʝ�Aʗ�Aʛ�Aʛ�Aʛ�Aʙ�Aʛ�Aʡ�Aʧ�AʬAʬAʬAʡ�Aʗ�Aʕ�A�|�A�"�A���A�z�A��mAȼjAț�A�r�A�9XA��TAǲ-AǕ�Aǉ7A�bNA�5?A�A��yAƬA���A�bA�oA�(�A�"�A�  A��yA�S�A��#A�K�A�9XA�{A���A�ƨA�1'A���A��+A�O�A�"�A�JA�JA��A���A��DA�jA�M�A��A�ffA�n�A���A�
=A��A���A��-A��A�=qA�E�A��7A�n�A���A�ĜA�A�A��HA�jA�A�~�A�$�A���A�K�A�x�A�1'A�?}A�VA��
A��A�C�A��DA���A�1A�  A���A�oA�bNA��\A��mA��A��A�XA��uA�{A�1'A�oA�hsA���AS�AxAvȴAq�Alv�Ak��AiVAf�jAb�9A^r�A[�#AX��AU+AS�-AS|�AS?}AQS�AN(�AK��AJ5?AI��AI7LAF��AB��A?33A=l�A;�
A;�A:ZA85?A5�TA4��A4M�A3C�A1?}A0{A/�A.�9A,��A*�/A*M�A*$�A*  A)�-A)?}A'�wA&^5A%+A$-A#�A#33A"{A ��A�HAC�A�A��Ax�A"�A��A��AbNA1A"�A�/Ar�At�A��AbNA`BAoA�A��AA�A�A7LA��A�A�FA�A�/AM�A�A��A��A
��A
n�A
-A
$�A	��A
E�A
�\A	��A1'A��A��A9XAl�A�9A �AVA�DA$�Az�A�uAz�AA�A(�AbA�wA �yA �A E�@�\)@���@���@�b@��#@�/@���@�ƨ@��\@�/@�t�@�R@�&�@��@�@��@��;@�"�@��@��@�hs@�1@�@�!@�-@���@�z�@�\)@ޟ�@ޏ\@�n�@�n�@�v�@�v�@ޏ\@ޏ\@ݡ�@�G�@݉7@���@�J@�5?@ޟ�@އ+@܃@��/@���@�X@�@ݺ^@��@��@ݺ^@ݑh@��@��@��;@�C�@�@ڰ!@��@��#@���@ٙ�@�x�@�`B@�?}@��@���@أ�@�j@�t�@��@�ff@��@�x�@ԃ@�j@�9X@���@Ӆ@�@���@҇+@�{@ѡ�@�p�@�/@���@ЋD@�1'@���@��
@Ͼw@�dZ@�
=@��H@��@���@ΰ!@��@�V@̣�@��;@�l�@�o@�@ʸR@�ff@�~�@�K�@�t�@ʇ+@�E�@�hs@���@ȣ�@�bN@�b@��@���@ƸR@���@�
=@�@Ƈ+@��@ēu@�C�@�E�@��T@�@�7L@�r�@��
@��@�|�@�33@�
=@���@�{@��h@��@��`@�1'@�33@��R@���@�~�@�E�@��#@�hs@�O�@��@��j@��@�Z@�1@���@�|�@�K�@�@��@��#@�hs@��9@��@�j@� �@���@��
@�ƨ@��F@��@�\)@��R@��\@�E�@��7@���@��@�z�@��
@�;d@�"�@��@�o@��@���@�5?@�@�%@��u@��D@��D@�z�@�Z@�1'@�  @��m@���@�;d@�v�@��-@���@� �@��F@�t�@���@���@��-@���@��h@�7L@�z�@�r�@��u@��@�1'@�ƨ@��@��P@�"�@���@��-@��@�7L@�Ĝ@�Q�@�1@���@��y@��+@�V@�E�@�-@���@��h@�&�@�9X@��
@��w@��w@��@���@��P@�K�@��@�~�@�M�@�{@���@��-@��@��@�dZ@�S�@�"�@�
=@���@�n�@��@�@��-@�p�@�G�@��@�V@��@�(�@��@�ƨ@�l�@���@�H@xz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʡ�Aʛ�Aʛ�Aʝ�Aʗ�Aʛ�Aʛ�Aʛ�Aʙ�Aʛ�Aʡ�Aʧ�AʬAʬAʬAʡ�Aʗ�Aʕ�A�|�A�"�A���A�z�A��mAȼjAț�A�r�A�9XA��TAǲ-AǕ�Aǉ7A�bNA�5?A�A��yAƬA���A�bA�oA�(�A�"�A�  A��yA�S�A��#A�K�A�9XA�{A���A�ƨA�1'A���A��+A�O�A�"�A�JA�JA��A���A��DA�jA�M�A��A�ffA�n�A���A�
=A��A���A��-A��A�=qA�E�A��7A�n�A���A�ĜA�A�A��HA�jA�A�~�A�$�A���A�K�A�x�A�1'A�?}A�VA��
A��A�C�A��DA���A�1A�  A���A�oA�bNA��\A��mA��A��A�XA��uA�{A�1'A�oA�hsA���AS�AxAvȴAq�Alv�Ak��AiVAf�jAb�9A^r�A[�#AX��AU+AS�-AS|�AS?}AQS�AN(�AK��AJ5?AI��AI7LAF��AB��A?33A=l�A;�
A;�A:ZA85?A5�TA4��A4M�A3C�A1?}A0{A/�A.�9A,��A*�/A*M�A*$�A*  A)�-A)?}A'�wA&^5A%+A$-A#�A#33A"{A ��A�HAC�A�A��Ax�A"�A��A��AbNA1A"�A�/Ar�At�A��AbNA`BAoA�A��AA�A�A7LA��A�A�FA�A�/AM�A�A��A��A
��A
n�A
-A
$�A	��A
E�A
�\A	��A1'A��A��A9XAl�A�9A �AVA�DA$�Az�A�uAz�AA�A(�AbA�wA �yA �A E�@�\)@���@���@�b@��#@�/@���@�ƨ@��\@�/@�t�@�R@�&�@��@�@��@��;@�"�@��@��@�hs@�1@�@�!@�-@���@�z�@�\)@ޟ�@ޏ\@�n�@�n�@�v�@�v�@ޏ\@ޏ\@ݡ�@�G�@݉7@���@�J@�5?@ޟ�@އ+@܃@��/@���@�X@�@ݺ^@��@��@ݺ^@ݑh@��@��@��;@�C�@�@ڰ!@��@��#@���@ٙ�@�x�@�`B@�?}@��@���@أ�@�j@�t�@��@�ff@��@�x�@ԃ@�j@�9X@���@Ӆ@�@���@҇+@�{@ѡ�@�p�@�/@���@ЋD@�1'@���@��
@Ͼw@�dZ@�
=@��H@��@���@ΰ!@��@�V@̣�@��;@�l�@�o@�@ʸR@�ff@�~�@�K�@�t�@ʇ+@�E�@�hs@���@ȣ�@�bN@�b@��@���@ƸR@���@�
=@�@Ƈ+@��@ēu@�C�@�E�@��T@�@�7L@�r�@��
@��@�|�@�33@�
=@���@�{@��h@��@��`@�1'@�33@��R@���@�~�@�E�@��#@�hs@�O�@��@��j@��@�Z@�1@���@�|�@�K�@�@��@��#@�hs@��9@��@�j@� �@���@��
@�ƨ@��F@��@�\)@��R@��\@�E�@��7@���@��@�z�@��
@�;d@�"�@��@�o@��@���@�5?@�@�%@��u@��D@��D@�z�@�Z@�1'@�  @��m@���@�;d@�v�@��-@���@� �@��F@�t�@���@���@��-@���@��h@�7L@�z�@�r�@��u@��@�1'@�ƨ@��@��P@�"�@���@��-@��@�7L@�Ĝ@�Q�@�1@���@��y@��+@�V@�E�@�-@���@��h@�&�@�9X@��
@��w@��w@��@���@��P@�K�@��@�~�@�M�@�{@���@��-@��@��@�dZ@�S�@�"�@�
=@���@�n�@��@�@��-@�p�@�G�@��@�V@��@�(�@��@�ƨ@�l�@���@�H@xz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	+B	PB	VB	�B	P�B	r�B	��B
B
VB
�B
�B
?}B
O�B
VB
[#B
aHB
l�B
�B
��B
��B
��B
ȴB
�)B
�HB
�B+BoB�B?}BM�Bn�Bz�B�B�%B�B�B}�B~�B�B�uB��B��B�XB�jB�^BȴB�B�5B�mB�yB�BB �B&�BB�BVB]/B_;B\)BN�B;dB49B49B49B49B1'B+B�BhBB�B�TB�B��BɺB�}B�9B��B��B�1BT�B{B
��BuB(�B�BB  B
�B
ĜB
�B
<jB
1B	�`B	ɺB	��B	{�B	q�B	[#B	E�B	>wB	2-B	&�B	uB	B�B�mB�/B�B�
B��B��B��BȴBȴBǮBÖB��BB�'B�B��B��B��B�B�B�9B�3B�9B�jBĜBŢB��B��BɺBȴBǮBǮBƨBŢBÖB�wB�jB�}B�}BĜBɺBÖB�}BŢB��B��B�BB�/B�BB�fB�mB�fB�B�B�B��B	  B	PB	PB	oB	hB	+B	+B	PB	PB	DB		7B		7B	DB	PB	PB	JB	PB	\B	oB	�B	�B	'�B	)�B	.B	49B	1'B	'�B	!�B	"�B	"�B	"�B	!�B	 �B	�B	�B	�B	/B	5?B	6FB	5?B	6FB	;dB	A�B	?}B	C�B	D�B	C�B	B�B	C�B	K�B	D�B	F�B	I�B	L�B	J�B	H�B	J�B	L�B	O�B	P�B	O�B	M�B	I�B	K�B	F�B	=qB	8RB	5?B	5?B	5?B	6FB	8RB	7LB	8RB	;dB	<jB	=qB	>wB	>wB	@�B	A�B	G�B	K�B	N�B	S�B	YB	^5B	dZB	k�B	p�B	p�B	y�B	}�B	�%B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�9B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	�qB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�#B	�/B	�)B	�5B	�/B	�)B	�)B	�#B	�#B	�B	�)B	�)B	�/B	�HB	�NB	�HB	�;B	�HB	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
VB
	RB
KB
"�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	B	+B	PB	VB	�B	P�B	r�B	��B
B
VB
�B
�B
?}B
O�B
VB
[#B
aHB
l�B
�B
��B
��B
��B
ȴB
�)B
�HB
�B+BoB�B?}BM�Bn�Bz�B�B�%B�B�B}�B~�B�B�uB��B��B�XB�jB�^BȴB�B�5B�mB�yB�BB �B&�BB�BVB]/B_;B\)BN�B;dB49B49B49B49B1'B+B�BhBB�B�TB�B��BɺB�}B�9B��B��B�1BT�B{B
��BuB(�B�BB  B
�B
ĜB
�B
<jB
1B	�`B	ɺB	��B	{�B	q�B	[#B	E�B	>wB	2-B	&�B	uB	B�B�mB�/B�B�
B��B��B��BȴBȴBǮBÖB��BB�'B�B��B��B��B�B�B�9B�3B�9B�jBĜBŢB��B��BɺBȴBǮBǮBƨBŢBÖB�wB�jB�}B�}BĜBɺBÖB�}BŢB��B��B�BB�/B�BB�fB�mB�fB�B�B�B��B	  B	PB	PB	oB	hB	+B	+B	PB	PB	DB		7B		7B	DB	PB	PB	JB	PB	\B	oB	�B	�B	'�B	)�B	.B	49B	1'B	'�B	!�B	"�B	"�B	"�B	!�B	 �B	�B	�B	�B	/B	5?B	6FB	5?B	6FB	;dB	A�B	?}B	C�B	D�B	C�B	B�B	C�B	K�B	D�B	F�B	I�B	L�B	J�B	H�B	J�B	L�B	O�B	P�B	O�B	M�B	I�B	K�B	F�B	=qB	8RB	5?B	5?B	5?B	6FB	8RB	7LB	8RB	;dB	<jB	=qB	>wB	>wB	@�B	A�B	G�B	K�B	N�B	S�B	YB	^5B	dZB	k�B	p�B	p�B	y�B	}�B	�%B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�9B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	�qB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�#B	�/B	�)B	�5B	�/B	�)B	�)B	�#B	�#B	�B	�)B	�)B	�/B	�HB	�NB	�HB	�;B	�HB	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
VB
	RB
KB
"�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190610                              AO  ARCAADJP                                                                    20181005190610    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190610  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190610  QCF$                G�O�G�O�G�O�8000            