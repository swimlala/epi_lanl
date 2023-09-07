CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:45Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ST   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  m8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  vx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190545  20181005190545  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i��1   @��jUUjF@1cn��P�c��j~��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�  @�  A   A   A@  A`  A~ffA�33A�33A�  A�  A���A�  A�33B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�33B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  D   D y�D  D� D��Dy�D  D�fDfD� D  D� D  D� DfD�fD  D� D	fD	� D
  D
�fD  D� D  D�fDfD� D��Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD  Dy�D��Dy�D��Dy�D  D� D  Dy�D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D"��D#y�D#��D$y�D$��D%y�D&  D&�fD'fD3  D3y�D3��D4� D5  D5� D6fD6� D6��D7� D8fD8� D9fD9� D9��D:� D;  D;�fD<fD<�fD=  D=� D>fD>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DD��DE� DFfDF� DG  DG�fDH  DH� DI  DIy�DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DUy�DU��DV� DW  DWy�DX  DX� DYfDY�fDZ  DZ� D[  D[�fD\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da�fDbfDb� Db��Dc� Dd  Dd� DefDe� De��Df� Dg  Dg� Dh  Dh� DifDi� Di��Dj� Dk  Dk�fDlfDl� Dl��Dmy�Dn  Dn� Do  Doy�Do��Dpy�Dq  Dq�fDr  Dr� DsfDs� Dt  Dt� Du  Du�fDv  Dv� Dw  Dwy�Dyl)D�T{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A��A$��AD��Ad��A��A��A��A�Q�A�Q�A��A�Q�A�B(�B	(�B(�B(�B!(�B)(�B1(�B8BA(�BI(�BQ(�BY(�B`Bi(�Bq(�By(�B��{B�ǮB�ǮB�ǮB��{B��{B��{B�aHB��{B��{B��{B��{B��{B�aHB�aHB��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�aHB�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=Cc�CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=Ch0�Cj0�ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�1�C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�RC�RC�%C�RC�RC�RC�RC�RC�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�RC�RC�%C�1�C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�RC�%C�1�C�%C�%C�%C�%C�%C�RC�RC�RC�RC�RC�RC�RC�RC�%C�%C�%C�RC�%C�%C�%C�RC�%C�1�C�1�C�1�C�%C�%C�%C�%D �D �)D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D)D�)D)D�)D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"�)D#)D#�)D$)D$�)D%)D%�)D&�D&��D'�D3�D3�)D4)D4��D5�D5��D6�D6��D7)D7��D8�D8��D9�D9��D:)D:��D;�D;��D<�D<��D=�D=��D>�D>��D?)D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE)DE��DF�DF��DG�DG��DH�DH��DI�DI�)DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�)DS�DS��DT�DT��DU�DU�)DV)DV��DW�DW�)DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc)Dc��Dd�Dd��De�De��Df)Df��Dg�Dg��Dh�Dh��Di�Di��Dj)Dj��Dk�Dk��Dl�Dl��Dm)Dm�)Dn�Dn��Do�Do�)Dp)Dp�)Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�)Dy~�D�]�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�1A�1A�
=A�VA�VA�
=A�VA�{A�\)AΥ�AΡ�AΑhAζFA�O�A̾wA̾wA��;A��A�C�A�n�A��A�(�A�M�Aŗ�A�$�AÛ�A� �A��+A�+A�C�A��/A�O�A���A�VA���A���A�bA�z�A��A�hsA�
=A���A�JA�9XA��!A�ffA���A�|�A��A�l�A�%A�n�A�E�A���A���A��A�bNA���A�hsA���A��9A�-A�?}A�C�A��!A�l�A�ĜA��A�&�A��9A�A�7LA��PA���A���A�  A���A��A|��Ax�\AwK�As�Ap{AnA�Al�Ag�#Ad�uA`�AZJAW/AU��AS��AQoAN�AL��AK�AJ-AIO�AG�AD�uAC��AB��A@$�A=��A:�9A7�A6��A5x�A3�TA3�A1�
A/�#A-�-A,1A+�hA* �A'�;A%S�A#;dA"�9A"n�A!�#A!+A z�AƨA^5At�AG�A�!A|�A�A�#A��At�A��A��A�AƨAƨA�A��A1'A��A�TA�FAVAffA�
A�AM�A��A��A �A^5Al�A
�uA	��A�AdZA��A�DA�mA&�A�RA�AAS�AA �/@���@��T@�j@��@��@��@��@�Ĝ@�l�@�/@�A�@��@�7@��@�bN@��@���@�o@�hs@�o@�^@�x�@�9@��m@畁@�K�@�\@噚@��/@�dZ@��H@�5?@�`B@��@�
=@�-@ݑh@�/@�Q�@ۅ@�"�@���@�n�@ٲ-@�V@؃@� �@��
@֧�@�hs@���@ԋD@�bN@�(�@� �@�1@ӥ�@��T@�j@υ@�"�@��@�hs@�bN@˅@�dZ@�C�@�@ʧ�@�~�@�-@�p�@��/@ȋD@ț�@ȓu@Ǯ@�~�@Ɨ�@Ɵ�@Ɵ�@Ƈ+@�@�-@���@š�@�x�@�G�@�/@ļj@ģ�@�r�@��;@�dZ@�o@�@��-@��@��u@�1@��;@��w@�t�@�dZ@��@�E�@��-@�`B@���@���@�Z@�b@��;@�ƨ@�"�@��@�@��T@���@���@��@���@�Ĝ@�b@��w@�S�@��@��^@�%@��@�Z@�Q�@� �@�9X@�ƨ@���@�z�@�I�@��@�b@�ƨ@��@��@��@�|�@�;d@�+@�+@�+@�"�@�
=@���@���@���@�%@��`@��@�S�@���@�`B@���@���@��@�/@�Ĝ@��u@�A�@�dZ@�ff@�-@���@���@��7@��h@�&�@��u@��F@���@��@��@�;d@���@��@���@��@��@���@�Z@�I�@��@���@�\)@�@���@�{@��@�{@�J@��#@���@��@��D@���@���@�?}@�%@���@���@��j@�Q�@��@���@�
=@���@���@��@�7L@���@���@�I�@�1@�dZ@�@���@��R@���@�n�@�-@��@�{@�J@���@��@�O�@�%@��`@��9@�j@� �@�ƨ@���@�|�@�;d@�+@�o@��H@��@��R@��+@�ff@��-@�V@�r�@�A�@� �@�  @��m@��;@��
@�ƨ@��@��@�S�@�o@�ȴ@��!@�ff@�V@�-@�{@��T@��7@�/@��@�:�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�1A�1A�
=A�VA�VA�
=A�VA�{A�\)AΥ�AΡ�AΑhAζFA�O�A̾wA̾wA��;A��A�C�A�n�A��A�(�A�M�Aŗ�A�$�AÛ�A� �A��+A�+A�C�A��/A�O�A���A�VA���A���A�bA�z�A��A�hsA�
=A���A�JA�9XA��!A�ffA���A�|�A��A�l�A�%A�n�A�E�A���A���A��A�bNA���A�hsA���A��9A�-A�?}A�C�A��!A�l�A�ĜA��A�&�A��9A�A�7LA��PA���A���A�  A���A��A|��Ax�\AwK�As�Ap{AnA�Al�Ag�#Ad�uA`�AZJAW/AU��AS��AQoAN�AL��AK�AJ-AIO�AG�AD�uAC��AB��A@$�A=��A:�9A7�A6��A5x�A3�TA3�A1�
A/�#A-�-A,1A+�hA* �A'�;A%S�A#;dA"�9A"n�A!�#A!+A z�AƨA^5At�AG�A�!A|�A�A�#A��At�A��A��A�AƨAƨA�A��A1'A��A�TA�FAVAffA�
A�AM�A��A��A �A^5Al�A
�uA	��A�AdZA��A�DA�mA&�A�RA�AAS�AA �/@���@��T@�j@��@��@��@��@�Ĝ@�l�@�/@�A�@��@�7@��@�bN@��@���@�o@�hs@�o@�^@�x�@�9@��m@畁@�K�@�\@噚@��/@�dZ@��H@�5?@�`B@��@�
=@�-@ݑh@�/@�Q�@ۅ@�"�@���@�n�@ٲ-@�V@؃@� �@��
@֧�@�hs@���@ԋD@�bN@�(�@� �@�1@ӥ�@��T@�j@υ@�"�@��@�hs@�bN@˅@�dZ@�C�@�@ʧ�@�~�@�-@�p�@��/@ȋD@ț�@ȓu@Ǯ@�~�@Ɨ�@Ɵ�@Ɵ�@Ƈ+@�@�-@���@š�@�x�@�G�@�/@ļj@ģ�@�r�@��;@�dZ@�o@�@��-@��@��u@�1@��;@��w@�t�@�dZ@��@�E�@��-@�`B@���@���@�Z@�b@��;@�ƨ@�"�@��@�@��T@���@���@��@���@�Ĝ@�b@��w@�S�@��@��^@�%@��@�Z@�Q�@� �@�9X@�ƨ@���@�z�@�I�@��@�b@�ƨ@��@��@��@�|�@�;d@�+@�+@�+@�"�@�
=@���@���@���@�%@��`@��@�S�@���@�`B@���@���@��@�/@�Ĝ@��u@�A�@�dZ@�ff@�-@���@���@��7@��h@�&�@��u@��F@���@��@��@�;d@���@��@���@��@��@���@�Z@�I�@��@���@�\)@�@���@�{@��@�{@�J@��#@���@��@��D@���@���@�?}@�%@���@���@��j@�Q�@��@���@�
=@���@���@��@�7L@���@���@�I�@�1@�dZ@�@���@��R@���@�n�@�-@��@�{@�J@���@��@�O�@�%@��`@��9@�j@� �@�ƨ@���@�|�@�;d@�+@�o@��H@��@��R@��+@�ff@��-@�V@�r�@�A�@� �@�  @��m@��;@��
@�ƨ@��@��@�S�@�o@�ȴ@��!@�ff@�V@�-@�{@��T@��7@�/@��@�:�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�BhsB�`B	 �B	�\B
W
B
�RBVB!�B49B:^B>wB2-B%�B�BuB �B)�BE�BffBp�Bq�B�bB��B��B��B��B�B�^B�;B�B��B(�B6FB7LB9XB=qB?}B@�B>wB>wBA�BD�BG�BA�B/B#�B"�B �B�B+B�yBȴB�9B�B��B� BH�B1'B�BPB
��B
��B
�oB
z�B
m�B
[#B
49B
�B	��B	�
B	B	�B	��B	�B	jB	^5B	N�B	6FB	�B	B�B�HB�)B��B��BɺBǮBŢBƨB��BŢB�jB�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�3B�'B�B�B�9B�jB�XB�LB�XB�dB�dB�}BÖBÖBǮB��B��B��B��B��B��BǮBB��BǮB��B��B��BɺBǮB��B��B��B��B��B�B�/B�5B�BB�/B�)B�/B�/B�;B�BB�BB�HB�NB�ZB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	1B		7B	JB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	%�B	(�B	+B	+B	,B	-B	0!B	33B	7LB	9XB	;dB	?}B	B�B	C�B	C�B	D�B	E�B	H�B	H�B	H�B	L�B	P�B	R�B	S�B	VB	XB	YB	ZB	ZB	[#B	\)B	^5B	_;B	`BB	aHB	dZB	gmB	k�B	l�B	o�B	q�B	q�B	s�B	u�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�=B	�PB	�\B	�bB	�hB	�oB	�uB	�uB	�oB	�hB	�uB	�uB	��B	��B	��B	��B	��BF�B	��B	��B	�B	�'B	�!B	�!B	�-B	�3B	�3B	�3B	�9B	�9B	�?B	�?B	�?B	�?B	�RB	�XB	�wB	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�HB	�HB	�NB	�TB	�`B	�yB	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
	7B
DB
xB
$22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�BhsB�`B	 �B	�\B
W
B
�RBVB!�B49B:^B>wB2-B%�B�BuB �B)�BE�BffBp�Bq�B�bB��B��B��B��B�B�^B�;B�B��B(�B6FB7LB9XB=qB?}B@�B>wB>wBA�BD�BG�BA�B/B#�B"�B �B�B+B�yBȴB�9B�B��B� BH�B1'B�BPB
��B
��B
�oB
z�B
m�B
[#B
49B
�B	��B	�
B	B	�B	��B	�B	jB	^5B	N�B	6FB	�B	B�B�HB�)B��B��BɺBǮBŢBƨB��BŢB�jB�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�3B�'B�B�B�9B�jB�XB�LB�XB�dB�dB�}BÖBÖBǮB��B��B��B��B��B��BǮBB��BǮB��B��B��BɺBǮB��B��B��B��B��B�B�/B�5B�BB�/B�)B�/B�/B�;B�BB�BB�HB�NB�ZB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	1B		7B	JB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	%�B	(�B	+B	+B	,B	-B	0!B	33B	7LB	9XB	;dB	?}B	B�B	C�B	C�B	D�B	E�B	H�B	H�B	H�B	L�B	P�B	R�B	S�B	VB	XB	YB	ZB	ZB	[#B	\)B	^5B	_;B	`BB	aHB	dZB	gmB	k�B	l�B	o�B	q�B	q�B	s�B	u�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�=B	�PB	�\B	�bB	�hB	�oB	�uB	�uB	�oB	�hB	�uB	�uB	��B	��B	��B	��B	��BF�B	��B	��B	�B	�'B	�!B	�!B	�-B	�3B	�3B	�3B	�9B	�9B	�?B	�?B	�?B	�?B	�RB	�XB	�wB	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�HB	�HB	�NB	�TB	�`B	�yB	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
	7B
DB
xB
$22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190545                              AO  ARCAADJP                                                                    20181005190545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190545  QCF$                G�O�G�O�G�O�8000            