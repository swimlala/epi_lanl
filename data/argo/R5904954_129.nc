CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:18Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191718  20181005191718  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�Ϥ���1   @�ϥ��@4�XbM��dm���l�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B:  B>��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C�fC�fC  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<�C>�C@�CB  CC�fCE�fCH  CJ33CL�CN  CP  CR�CT  CV  CX  CY�fC[�fC^  C`�Cb  Cc�fCf  Ch  Cj�Cl  Cn  Cp�Cr  Ct  Cu�fCx  Cz  C{�fC}�fC�  C��3C��C�  C�  C��3C�  C��3C�  C��3C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C��3C��C�  C��3C�  C�  C�  C�  C�  C��3C��C��3C�  C��C�  C��3C��3C��3C��C��3C�  C��3C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C��C�  C�  C��C�  C��3C��3C��3C��3C�  C��C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  D   D �fDfD�fD  Dy�D  D� D��D� D  Dy�D  D� D��Dy�D  D� D	  D	� D
  D
� D  D� D��D� D  D� D��D� DfD� D��Dy�D��Dy�DfD� D  D� D  D� D  Dy�D��D� D��D� D  D�fDfD� D��D� D  Dy�D��D� D  Dy�D��Dy�D  D� D   D y�D ��D!s3D"  D"�fD#fD#�fD$  D$� D$��D%� D&fD&� D'  D'y�D(  D(�fD)  D)� D*fD*�fD+fD+�fD,  D,y�D,�3D-y�D.fD.� D.��D/y�D0fD0� D0��D1y�D2  D2� D2��D3y�D4  D4� D5  D5y�D6  D6�fD7�D7��D8  D8y�D8��D9y�D:  D:� D;  D;�fD<fD<� D=  D=�fD>  D>� D?  D?y�D?��D@� DAfDA� DA��DBy�DB��DCy�DC��DD� DE  DE�fDF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ�fDK  DK� DLfDL�fDM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DQ��DR� DR��DSy�DS��DT� DU  DUy�DU��DVy�DV��DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^�fD_  D_� D_��D`y�Da  Da� Db  Db� Dc  Dc�fDd  Ddy�De  De�fDffDf�fDg  Dg� Dh  Dh� DifDiy�Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dm��Dn� Dn��Do�fDpfDp�fDq  Dqy�Dq��Dr� Ds  Dsy�Dt  Dt�fDufDu� Du��Dvy�Dw  Dw�fDw�3Dy�HD�?
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @L(�@�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B;(�B?��BHBQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B�ǮB��{B��{B��{B��{B��{B�ǮB��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B�ǮB��{C J=CJ=CJ=CJ=CJ=C
J=C0�C0�CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"c�C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6c�C8J=C:J=C<c�C>c�C@c�CBJ=CD0�CF0�CHJ=CJ}pCLc�CNJ=CPJ=CRc�CTJ=CVJ=CXJ=CZ0�C\0�C^J=C`c�CbJ=Cd0�CfJ=ChJ=Cjc�ClJ=CnJ=Cpc�CrJ=CtJ=Cv0�CxJ=CzJ=C|0�C~0�C�%C�RC�1�C�%C�%C�RC�%C�RC�%C�RC�%C�RC�RC�%C�%C�RC�%C�%C�%C�%C�RC�1�C�%C�RC�%C�%C�%C�%C�%C�RC�1�C�RC�%C�1�C�%C�RC�RC�RC�1�C�RC�%C�RC�%C�%C�1�C�1�C�%C�%C�1�C�1�C�%C�%C�%C�1�C�1�C�%C�RC�%C�%C�%C�RC�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�RC�%C�%C�RC�RC�1�C�%C�%C�1�C�%C�RC�RC�RC�RC�%C�1�C�RC�RC�RC�RC�%C�1�C�%C�%C�%C�%C�1�C�1�C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�1�C�%C�RC�%C�%C�%C�%C�1�C�%C�RC�%C�%C�%D �D ��D�D��D�D�)D�D��D)D��D�D�)D�D��D)D�)D�D��D	�D	��D
�D
��D�D��D)D��D�D��D)D��D�D��D)D�)D)D�)D�D��D�D��D�D��D�D�)D)D��D)D��D�D��D�D��D)D��D�D�)D)D��D�D�)D)D�)D�D��D �D �)D!)D!��D"�D"��D#�D#��D$�D$��D%)D%��D&�D&��D'�D'�)D(�D(��D)�D)��D*�D*��D+�D+��D,�D,�)D-�D-�)D.�D.��D/)D/�)D0�D0��D1)D1�)D2�D2��D3)D3�)D4�D4��D5�D5�)D6�D6��D7\D7�\D8�D8�)D9)D9�)D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�)D@)D@��DA�DA��DB)DB�)DC)DC�)DD)DD��DE�DE��DF�DF��DG�DG�)DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR)DR��DS)DS�)DT)DT��DU�DU�)DV)DV�)DW)DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`)D`�)Da�Da��Db�Db��Dc�Dc��Dd�Dd�)De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�)Dj�Dj��Dk�Dk��Dl�Dl��Dm)Dm�)Dn)Dn��Do)Do��Dp�Dp��Dq�Dq�)Dr)Dr��Ds�Ds�)Dt�Dt��Du�Du��Dv)Dv�)Dw�Dw��Dx�Dy��D�HR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�l�A�p�A�p�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�r�A�r�A�t�A�v�A�v�A�v�A�t�A�p�A�p�A�p�A�jA�1A�;dA�oAˮAǗ�A�ȴA�bNA�ffA�ĜA�M�AÇ+A�&�A���A�VA�{A�?}A���A��A�E�A��mA���A��RA�|�A�/A��!A�`BA��A��A� �A�VA�=qA��wA��\A��A��A�A�A�I�A��
A�C�A��A��-A��A�I�A��A�?}A�
=A�ffA���A��wA���A�C�A�S�A��jA�r�A�A�^5A��uA�|�A�XA�v�A��!A�C�A~v�A}S�Az-At�As7LAr��Ar�ApM�Am�AkO�AjAg�^Adr�AcA^�9A\�A\�\A\9XA[��AX~�AUXAT�\AT  AR��AQ|�AO�AM��AL1AJ�9AI�AH��AFĜAD��ADn�AC/AA�A@JA??}A>��A=��A=�A<9XA<  A;�
A;ƨA:�uA8�A7�^A6��A5/A4bA3�7A3"�A2��A2�A21A/�A.�!A,��A*VA)�PA(��A'��A%��A$5?A#?}A"ZA!�#A��AI�A�AM�A�PAoAr�A�A1A��AjA
=A�A�^A��A�DA��A�9A��A1'A`BA
=A
��A
�`A
�A
ȴA
�\A
9XA	��A	�wA	S�A��A�\A1A�AE�A�`A  AdZA7LA�9AƨA �A Q�@��F@�@���@��#@��@�r�@�b@��^@�I�@�dZ@�v�@��@�@���@��-@�bN@�l�@�C�@�R@��@��@�p�@�5?@��;@�E�@��@�9@�b@���@�^5@�7L@ָR@�-@��@�7L@���@̓u@�bN@�1'@�  @���@˾w@�t�@��@��@ʟ�@�M�@��#@ɑh@�?}@ǅ@���@�5?@��T@ũ�@���@��y@°!@�~�@�j@�Ĝ@�t�@�33@�-@�/@� �@���@��#@���@��-@��7@�7L@�r�@���@�K�@�ȴ@�M�@�Z@��!@�
=@���@���@�|�@��
@�dZ@�@��
@�-@�@��T@���@�p�@�/@���@���@��@��+@�hs@�&�@���@�A�@�  @���@��w@���@�t�@��@���@���@�o@�K�@�dZ@��
@�ƨ@�l�@���@�C�@�+@�\)@�;d@��@��y@���@�~�@��^@���@�b@�t�@�l�@�dZ@�t�@�|�@�|�@�|�@�K�@�K�@�;d@�;d@�+@�o@���@�$�@��@���@���@�`B@�X@�X@�O�@�%@���@��w@�l�@�K�@�C�@�C�@�K�@�K�@�K�@�;d@�+@�
=@��H@��!@���@��R@���@�v�@���@��@��m@� �@���@�l�@��w@��;@�ƨ@�;d@�"�@���@��+@�ff@��@��9@���@�l�@�+@��@���@�~�@�v�@�v�@�v�@�~�@�$�@�?}@�j@�bN@�@�V@���@�K�@���@��P@���@���@��!@�ff@�-@��^@���@��7@��`@��@���@�C�@��@�o@���@��y@��R@��+@�5?@��T@��h@�hs@��@��7@��^@�p�@��`@���@�Ĝ@���@��j@�(�@�"�@���@���@�M�@�M�@�=q@�5?@���@��y@�n�@���@�Ĝ@��9@��u@�Q�@��@���@�33@�@�ȴ@���@��+@�^5@�5?@���@��-@�x�@�?}@��@�V@���@��`@���@��j@���@��`@��`@��/@���@��j@��j@�j@� �@�1@�bN@�9X@�;@;d@~V@}�h@}p�@}/@~{@�1@~5?@|��@}�h@~{@~��@
=@�b@��@�Q�@�1'@��@j;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�l�A�p�A�p�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�r�A�r�A�t�A�v�A�v�A�v�A�t�A�p�A�p�A�p�A�jA�1A�;dA�oAˮAǗ�A�ȴA�bNA�ffA�ĜA�M�AÇ+A�&�A���A�VA�{A�?}A���A��A�E�A��mA���A��RA�|�A�/A��!A�`BA��A��A� �A�VA�=qA��wA��\A��A��A�A�A�I�A��
A�C�A��A��-A��A�I�A��A�?}A�
=A�ffA���A��wA���A�C�A�S�A��jA�r�A�A�^5A��uA�|�A�XA�v�A��!A�C�A~v�A}S�Az-At�As7LAr��Ar�ApM�Am�AkO�AjAg�^Adr�AcA^�9A\�A\�\A\9XA[��AX~�AUXAT�\AT  AR��AQ|�AO�AM��AL1AJ�9AI�AH��AFĜAD��ADn�AC/AA�A@JA??}A>��A=��A=�A<9XA<  A;�
A;ƨA:�uA8�A7�^A6��A5/A4bA3�7A3"�A2��A2�A21A/�A.�!A,��A*VA)�PA(��A'��A%��A$5?A#?}A"ZA!�#A��AI�A�AM�A�PAoAr�A�A1A��AjA
=A�A�^A��A�DA��A�9A��A1'A`BA
=A
��A
�`A
�A
ȴA
�\A
9XA	��A	�wA	S�A��A�\A1A�AE�A�`A  AdZA7LA�9AƨA �A Q�@��F@�@���@��#@��@�r�@�b@��^@�I�@�dZ@�v�@��@�@���@��-@�bN@�l�@�C�@�R@��@��@�p�@�5?@��;@�E�@��@�9@�b@���@�^5@�7L@ָR@�-@��@�7L@���@̓u@�bN@�1'@�  @���@˾w@�t�@��@��@ʟ�@�M�@��#@ɑh@�?}@ǅ@���@�5?@��T@ũ�@���@��y@°!@�~�@�j@�Ĝ@�t�@�33@�-@�/@� �@���@��#@���@��-@��7@�7L@�r�@���@�K�@�ȴ@�M�@�Z@��!@�
=@���@���@�|�@��
@�dZ@�@��
@�-@�@��T@���@�p�@�/@���@���@��@��+@�hs@�&�@���@�A�@�  @���@��w@���@�t�@��@���@���@�o@�K�@�dZ@��
@�ƨ@�l�@���@�C�@�+@�\)@�;d@��@��y@���@�~�@��^@���@�b@�t�@�l�@�dZ@�t�@�|�@�|�@�|�@�K�@�K�@�;d@�;d@�+@�o@���@�$�@��@���@���@�`B@�X@�X@�O�@�%@���@��w@�l�@�K�@�C�@�C�@�K�@�K�@�K�@�;d@�+@�
=@��H@��!@���@��R@���@�v�@���@��@��m@� �@���@�l�@��w@��;@�ƨ@�;d@�"�@���@��+@�ff@��@��9@���@�l�@�+@��@���@�~�@�v�@�v�@�v�@�~�@�$�@�?}@�j@�bN@�@�V@���@�K�@���@��P@���@���@��!@�ff@�-@��^@���@��7@��`@��@���@�C�@��@�o@���@��y@��R@��+@�5?@��T@��h@�hs@��@��7@��^@�p�@��`@���@�Ĝ@���@��j@�(�@�"�@���@���@�M�@�M�@�=q@�5?@���@��y@�n�@���@�Ĝ@��9@��u@�Q�@��@���@�33@�@�ȴ@���@��+@�^5@�5?@���@��-@�x�@�?}@��@�V@���@��`@���@��j@���@��`@��`@��/@���@��j@��j@�j@� �@�1@�bN@�9X@�;@;d@~V@}�h@}p�@}/@~{@�1@~5?@|��@}�h@~{@~��@
=@�b@��@�Q�@�1'@��@j;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B/BG�BXB[#B]/BdZBhsBjBq�Bq�B{�B��B��B��B��B�B�!B�B�dB�}BɺBǮB�XB��B��B��B�\BdZBF�B+B�B��B�B�ZB�BĜB�XB�3B��B��B�By�Bt�Bp�BgmB[#BN�BA�B,B�B{BB
�B
�ZB
ɺB
��B
�VB
u�B
]/B
I�B
;dB
1'B
�B	��B	�B	�yB	�`B	�B	ǮB	�RB	�B	��B	�VB	�B	q�B	dZB	bNB	^5B	YB	J�B	:^B	5?B	2-B	+B	$�B	�B	uB	DB	%B	B��B�B�B�sB�TB�)B�
B��B��B��B��B��BɺBȴBƨBÖB��B�qB�dB�jB�jB�dB�dB�^B�dB�qB�^B�XB�?B�B��B��B��B��B��B��B��B��B�oB�bB�VB�hB�{B�uB�bB�1B�B�B�B�Bt�Bx�B�JB�VB�VB�\B�oB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�oB�hB�bB�bB�bB�bB�\B�\B�\B�bB�bB�hB�bB�bB�hB�hB�oB�oB�hB�{B�{B�{B�{B�uB�PB�+B�B�B�B�B�B�B�B�B�B�B�B�B� B|�Bt�Bo�BjBbNBQ�BQ�BT�BVBVBVBW
BYB`BBl�Bz�B~�B�B�B�B�B�B�B�B�B� B~�B}�B|�Bz�B�B�B� B�+B��B��B��B��B��B�B�B�B�!B�!B�!B�'B�'B�-B�-B�'B�B��B�oB�VB�JB�DB�hB��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�^B�dB�qB�}BĜBŢBǮB��B��B�#B�yB�B��B��B��B��B	B	%B	PB	VB	\B	\B	\B	bB	hB	�B	#�B	%�B	)�B	1'B	2-B	33B	5?B	6FB	7LB	7LB	7LB	8RB	;dB	?}B	A�B	B�B	D�B	F�B	F�B	F�B	F�B	G�B	I�B	S�B	T�B	VB	VB	W
B	W
B	YB	[#B	\)B	]/B	^5B	^5B	`BB	`BB	aHB	bNB	ffB	ffB	gmB	hsB	iyB	k�B	k�B	n�B	o�B	p�B	q�B	r�B	r�B	s�B	t�B	v�B	v�B	v�B	{�B	|�B	}�B	~�B	� B	�B	�+B	�7B	�DB	�VB	�VB	�oB	��B	��B	��B	�B	�FB	�XB	�^B	�XB	�^B	�wB	��B	��B	��B	��B	��B	ÖB	ŢB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�
B	�B	�
B	�B	�B	�B	�/B	�HB	�BB	�HB	�5B	�;B	�;B	�BB	�;B	�;B	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B
B
B
B
+B
1B
DB
JB
�B
&222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B/BG�BXB[#B]/BdZBhsBjBq�Bq�B{�B��B��B��B��B�B�!B�B�dB�}BɺBǮB�XB��B��B��B�\BdZBF�B+B�B��B�B�ZB�BĜB�XB�3B��B��B�By�Bt�Bp�BgmB[#BN�BA�B,B�B{BB
�B
�ZB
ɺB
��B
�VB
u�B
]/B
I�B
;dB
1'B
�B	��B	�B	�yB	�`B	�B	ǮB	�RB	�B	��B	�VB	�B	q�B	dZB	bNB	^5B	YB	J�B	:^B	5?B	2-B	+B	$�B	�B	uB	DB	%B	B��B�B�B�sB�TB�)B�
B��B��B��B��B��BɺBȴBƨBÖB��B�qB�dB�jB�jB�dB�dB�^B�dB�qB�^B�XB�?B�B��B��B��B��B��B��B��B��B�oB�bB�VB�hB�{B�uB�bB�1B�B�B�B�Bt�Bx�B�JB�VB�VB�\B�oB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�uB�oB�hB�bB�bB�bB�bB�\B�\B�\B�bB�bB�hB�bB�bB�hB�hB�oB�oB�hB�{B�{B�{B�{B�uB�PB�+B�B�B�B�B�B�B�B�B�B�B�B�B� B|�Bt�Bo�BjBbNBQ�BQ�BT�BVBVBVBW
BYB`BBl�Bz�B~�B�B�B�B�B�B�B�B�B� B~�B}�B|�Bz�B�B�B� B�+B��B��B��B��B��B�B�B�B�!B�!B�!B�'B�'B�-B�-B�'B�B��B�oB�VB�JB�DB�hB��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�^B�dB�qB�}BĜBŢBǮB��B��B�#B�yB�B��B��B��B��B	B	%B	PB	VB	\B	\B	\B	bB	hB	�B	#�B	%�B	)�B	1'B	2-B	33B	5?B	6FB	7LB	7LB	7LB	8RB	;dB	?}B	A�B	B�B	D�B	F�B	F�B	F�B	F�B	G�B	I�B	S�B	T�B	VB	VB	W
B	W
B	YB	[#B	\)B	]/B	^5B	^5B	`BB	`BB	aHB	bNB	ffB	ffB	gmB	hsB	iyB	k�B	k�B	n�B	o�B	p�B	q�B	r�B	r�B	s�B	t�B	v�B	v�B	v�B	{�B	|�B	}�B	~�B	� B	�B	�+B	�7B	�DB	�VB	�VB	�oB	��B	��B	��B	�B	�FB	�XB	�^B	�XB	�^B	�wB	��B	��B	��B	��B	��B	ÖB	ŢB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�
B	�B	�
B	�B	�B	�B	�/B	�HB	�BB	�HB	�5B	�;B	�;B	�BB	�;B	�;B	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B	��B	��B	��B
B
B
B
+B
1B
DB
JB
�B
&222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191718                              AO  ARCAADJP                                                                    20181005191718    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191718  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191718  QCF$                G�O�G�O�G�O�8000            