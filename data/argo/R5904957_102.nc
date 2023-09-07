CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:22Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140822  20181024140822  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               fA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��	z1   @��%`�@3�9XbN�c�Q��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      fA   A   B   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B��B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B���B�  C �C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D�fDfD� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0fD0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6�fD7fD7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDWfDW� DX  DX� DY  DY�fDZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^y�D^��D_y�D`  D`� Da  Da�fDbfDb� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dm��Dn� Do  Do� Dp  Dp� Dp��Dqy�Dq��Dry�Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� DwfDw� Dw��Dy��D�E�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@2�\@�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A��B(�BB(�B(�B!(�B)(�B1(�B9�\BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B�ǮB��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{B�aHB��{B�{B�aHB�{B�{B��{B�aHB��{C c�Cc�Cc�CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C0�C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C60�C80�C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\c�C^J=C`J=Cb0�CdJ=CfJ=ChJ=CjJ=ClJ=Cnc�CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�RC�RC�RC�RC�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�)DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�)D_)D_�)D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df)Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�)Dn)Dn��Do�Do��Dp�Dp��Dq)Dq�)Dr)Dr�)Ds�Ds��Dt)Dt��Du�Du��Dv�Dv��Dw�Dw��Dx)Dy�D�OD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��A�A�A�A��A�`BA�oA���A���A�&�A�/A�9XA��A׏\A�$�A�33Aԥ�AԍPA�l�A�1A��#A̬A��TA��mA���AȲ-A��A�K�A��;AƃAş�A�-AčPA���A�\)A��A���A�^5A�ƨA���A�v�A���A�bNA�jA���A���A�$�A��A�33A���A�(�A�~�A�7LA���A��A��jA���A��A��A���A��9A��+A�jA��A��\A�jA�5?A���A���A�ZA�\)A�"�A��yA���A�VA�A�Q�A�x�A�t�A���A��+A�\)A�1'A��`A�  A���A�oA��FA��+A�ĜA���A�Q�A�&�A��#A�33A���A��jA�C�A{��Av�Ar~�Ap-AlffAj9XAf��AdffAbbAa&�A^1AXȴAUl�AT��AR��AR-AP�RAL�AKS�AI��AG;dAE�#AE��AE�AE\)AE?}AD�9AA��A?33A<��A:�HA9?}A8Q�A7�A6ffA5��A5�A3t�A1G�A0  A.�A-VA+��A+XA*��A)�
A(�RA'S�A%��A$��A#�A"�A!�AK�AZA~�A�AbA��A�A�^A�AdZA��A�FA n�@���@�bN@�5?@���@��@�@�ȴ@��T@���@�X@�  @�ƨ@�ȴ@�J@��@��T@�`B@�u@�l�@�+@�{@�`B@��/@�1'@�ȴ@�Z@�`B@�\)@�p�@�Z@�C�@��@֏\@�M�@��T@ՙ�@�x�@�X@�O�@�X@�x�@Ցh@ՙ�@�`B@��@�&�@��@�Ĝ@�
=@�^5@���@�X@��/@мj@�bN@�"�@�ȴ@��@Η�@�@͡�@�&�@̼j@�1'@��@��@�Ĝ@�I�@���@��H@��T@�A�@î@��@��@�$�@�Ĝ@�z�@�bN@�Q�@��w@�S�@�33@�|�@�ƨ@��P@�+@�o@�
=@���@��T@���@��-@�r�@���@��@���@���@�"�@�-@���@���@��9@�b@�j@�r�@���@�n�@�@��^@�hs@�?}@�&�@�V@���@��`@���@���@��D@�j@��m@�@�M�@�{@��T@���@�O�@�&�@�Ĝ@� �@��m@��
@��F@���@��@��@��R@��+@���@���@�x�@�G�@��@�r�@�Q�@�(�@� �@��@���@���@�S�@�|�@���@�"�@��y@�n�@�{@�@�X@��@���@���@�r�@� �@�  @��@��
@���@�|�@�l�@�K�@�;d@�;d@�C�@�S�@�K�@��H@�=q@���@��@�/@�X@���@�I�@�1@��w@��P@�K�@���@�M�@�M�@�M�@�E�@�{@��@�{@��@��-@��7@�x�@��7@�`B@��@��@���@�I�@�(�@�b@�  @�ƨ@��@�|�@�|�@�+@�=q@��^@�hs@�O�@�7L@�/@�V@��@��D@�t�@�+@�ȴ@�ff@���@���@��@���@�l�@�ȴ@�n�@�ff@�ff@�~�@��\@�~�@�M�@��@��-@���@���@���@��h@�hs@���@�r�@��@��;@��F@�dZ@�+@�+@�+@�"�@�o@�@���@��\@�-@��@��#@���@���@�?}@���@�Z@�9X@�1'@�  @��F@�t�@��@���@��@���@�j@� �@�  @�ƨ@��@��@��@���@�-@��h@�p�@�p�@�p�@�`B@�`B@�`B@�/@���@��/@���@�1'@��F@�K�@�o@��z@zp;@d��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A��A�A�A�A��A�`BA�oA���A���A�&�A�/A�9XA��A׏\A�$�A�33Aԥ�AԍPA�l�A�1A��#A̬A��TA��mA���AȲ-A��A�K�A��;AƃAş�A�-AčPA���A�\)A��A���A�^5A�ƨA���A�v�A���A�bNA�jA���A���A�$�A��A�33A���A�(�A�~�A�7LA���A��A��jA���A��A��A���A��9A��+A�jA��A��\A�jA�5?A���A���A�ZA�\)A�"�A��yA���A�VA�A�Q�A�x�A�t�A���A��+A�\)A�1'A��`A�  A���A�oA��FA��+A�ĜA���A�Q�A�&�A��#A�33A���A��jA�C�A{��Av�Ar~�Ap-AlffAj9XAf��AdffAbbAa&�A^1AXȴAUl�AT��AR��AR-AP�RAL�AKS�AI��AG;dAE�#AE��AE�AE\)AE?}AD�9AA��A?33A<��A:�HA9?}A8Q�A7�A6ffA5��A5�A3t�A1G�A0  A.�A-VA+��A+XA*��A)�
A(�RA'S�A%��A$��A#�A"�A!�AK�AZA~�A�AbA��A�A�^A�AdZA��A�FA n�@���@�bN@�5?@���@��@�@�ȴ@��T@���@�X@�  @�ƨ@�ȴ@�J@��@��T@�`B@�u@�l�@�+@�{@�`B@��/@�1'@�ȴ@�Z@�`B@�\)@�p�@�Z@�C�@��@֏\@�M�@��T@ՙ�@�x�@�X@�O�@�X@�x�@Ցh@ՙ�@�`B@��@�&�@��@�Ĝ@�
=@�^5@���@�X@��/@мj@�bN@�"�@�ȴ@��@Η�@�@͡�@�&�@̼j@�1'@��@��@�Ĝ@�I�@���@��H@��T@�A�@î@��@��@�$�@�Ĝ@�z�@�bN@�Q�@��w@�S�@�33@�|�@�ƨ@��P@�+@�o@�
=@���@��T@���@��-@�r�@���@��@���@���@�"�@�-@���@���@��9@�b@�j@�r�@���@�n�@�@��^@�hs@�?}@�&�@�V@���@��`@���@���@��D@�j@��m@�@�M�@�{@��T@���@�O�@�&�@�Ĝ@� �@��m@��
@��F@���@��@��@��R@��+@���@���@�x�@�G�@��@�r�@�Q�@�(�@� �@��@���@���@�S�@�|�@���@�"�@��y@�n�@�{@�@�X@��@���@���@�r�@� �@�  @��@��
@���@�|�@�l�@�K�@�;d@�;d@�C�@�S�@�K�@��H@�=q@���@��@�/@�X@���@�I�@�1@��w@��P@�K�@���@�M�@�M�@�M�@�E�@�{@��@�{@��@��-@��7@�x�@��7@�`B@��@��@���@�I�@�(�@�b@�  @�ƨ@��@�|�@�|�@�+@�=q@��^@�hs@�O�@�7L@�/@�V@��@��D@�t�@�+@�ȴ@�ff@���@���@��@���@�l�@�ȴ@�n�@�ff@�ff@�~�@��\@�~�@�M�@��@��-@���@���@���@��h@�hs@���@�r�@��@��;@��F@�dZ@�+@�+@�+@�"�@�o@�@���@��\@�-@��@��#@���@���@�?}@���@�Z@�9X@�1'@�  @��F@�t�@��@���@��@���@�j@� �@�  @�ƨ@��@��@��@���@�-@��h@�p�@�p�@�p�@�`B@�`B@�`B@�/@���@��/@���@�1'@��F@�K�@�o@��z@zp;@d��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B%�B%�B$�B%�B%�B%�B%�B%�B$�B%�B%�B'�BD�BYB\)B[#BZBx�Bs�Bm�Bp�B|�B�oB��B��B��B�PBs�Bw�B�1B��B�-B�}B�B'�B(�B%�B8RBH�BT�B_;BcTBy�B�B�%B�=B�DB�Bx�B_;BO�BJ�B@�B!�B&�B9XBG�BL�BP�BQ�BR�BR�BT�BYBW
BT�BXB33B	7B�B/B\)BjBgmBq�Bp�Bk�BffBaHBF�B �B�BhB	7B�B�mB�BB�#B�
B��B��B�^B��B�Bp�B_;BR�BE�B,BJB
��B
�)B
��B
��B
iyB
?}B
�B
B	�B	�B	��B	�'B	��B	�oB	�+B	k�B	H�B	5?B	:^B	1'B	+B	 �B	hB		7B	B��B�B�B�B�B�B�B�ZB�#B��B��B��B��BǮBŢBB��B�}B�FB�3B�3B�B�LB��BÖBŢBǮBɺBɺBǮBĜB�jB��B�qBÖBŢB��B�}B�jB�jB�XB]O�B��B��B��B��B��B��B��B�bB�DB�7B�+B�+B�+B�1B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�FB�dB�wBBĜBŢBȴBɺB��B��B��B�B�B�#B�;B�BB�HB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�fB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	+B	+B	1B	B	+B	
=B	JB	JB	PB	\B	uB	�B	�B	�B	 �B	 �B	$�B	1'B	2-B	49B	<jB	>wB	>wB	>wB	>wB	>wB	<jB	9XB	;dB	<jB	=qB	G�B	K�B	O�B	R�B	T�B	VB	XB	XB	YB	YB	YB	ZB	ZB	[#B	[#B	\)B	^5B	dZB	hsB	jB	m�B	o�B	p�B	p�B	s�B	x�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�+B	�=B	�JB	�PB	�PB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�9B	�?B	�FB	�FB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�wB	��B	��B	��B	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�B	�B	�#B	�)B	�5B	�5B	�;B	�HB	�NB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B

=B
DB
JB
JB
JB
PB
PB
VB
\B
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
B
 �B
0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B%�B%�B$�B%�B%�B%�B%�B%�B$�B%�B%�B'�BD�BYB\)B[#BZBx�Bs�Bm�Bp�B|�B�oB��B��B��B�PBs�Bw�B�1B��B�-B�}B�B'�B(�B%�B8RBH�BT�B_;BcTBy�B�B�%B�=B�DB�Bx�B_;BO�BJ�B@�B!�B&�B9XBG�BL�BP�BQ�BR�BR�BT�BYBW
BT�BXB33B	7B�B/B\)BjBgmBq�Bp�Bk�BffBaHBF�B �B�BhB	7B�B�mB�BB�#B�
B��B��B�^B��B�Bp�B_;BR�BE�B,BJB
��B
�)B
��B
��B
iyB
?}B
�B
B	�B	�B	��B	�'B	��B	�oB	�+B	k�B	H�B	5?B	:^B	1'B	+B	 �B	hB		7B	B��B�B�B�B�B�B�B�ZB�#B��B��B��B��BǮBŢBB��B�}B�FB�3B�3B�B�LB��BÖBŢBǮBɺBɺBǮBĜB�jB��B�qBÖBŢB��B�}B�jB�jB�XB]O�B��B��B��B��B��B��B��B�bB�DB�7B�+B�+B�+B�1B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�FB�dB�wBBĜBŢBȴBɺB��B��B��B�B�B�#B�;B�BB�HB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�fB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	+B	+B	1B	B	+B	
=B	JB	JB	PB	\B	uB	�B	�B	�B	 �B	 �B	$�B	1'B	2-B	49B	<jB	>wB	>wB	>wB	>wB	>wB	<jB	9XB	;dB	<jB	=qB	G�B	K�B	O�B	R�B	T�B	VB	XB	XB	YB	YB	YB	ZB	ZB	[#B	[#B	\)B	^5B	dZB	hsB	jB	m�B	o�B	p�B	p�B	s�B	x�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�+B	�=B	�JB	�PB	�PB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�9B	�?B	�FB	�FB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�wB	��B	��B	��B	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�B	�B	�#B	�)B	�5B	�5B	�;B	�HB	�NB	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B

=B
DB
JB
JB
JB
PB
PB
VB
\B
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
B
 �B
0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140822                              AO  ARCAADJP                                                                    20181024140822    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140822  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140822  QCF$                G�O�G�O�G�O�0               