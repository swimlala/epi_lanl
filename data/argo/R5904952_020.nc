CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:10Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190510  20181005190510  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׭�g(��1   @׭��$�@2�C��%�c��1'1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(ffB0ffB7��B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\�C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C��3C�  C�  C��C��C��C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��D� D  D� D  D� DfDy�D��D� D  D� D  D� D  D� D	fD	�fD
  D
� D
��D� D  D� D  D� D  D� D  Dy�D  D�fDfD� D  Dy�D  D� D��D� D  D� D  Dy�D��Dy�D  D� D  D� DfD�fDfD� D��D� DfD� D  D� D  D� D   D y�D!  D!� D"  D"�fD#  D#� D$  D$� D$��D%� D&  D&y�D'  D'� D(  D(y�D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D.��D/y�D/��D0y�D0��D1� D1��D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7y�D7��D8� D9  D9� D:  D:� D;  D;y�D<  D<�fD=  D=y�D=��D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DE��DF� DGfDG�fDH  DHy�DH��DIy�DJ  DJ�fDK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DP��DQ�fDR  DR� DS  DS� DT  DT� DU  DUy�DU��DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D]��D^� D_  D_� D`fD`� DafDa� Db  Db� Dc  Dc� Dd  Dd�fDe  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� DpfDp�fDq  Dqy�Dq��Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw` DyZ=D�!�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A�\A"�\AB�\Ab�\A�{A�G�A�G�A�G�A�G�A�z�A�G�A�G�B ��B��B��B��B ��B)
=B1
=B8=qB��B��B�Q�B�Q�B̅B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*B�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN\CP(�CR(�CT(�CV(�CX(�CZ(�C\B�C^(�C`B�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct\Cv(�Cx(�Cz(�C|(�C~(�C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�!HC�{C�{C�{C��C��C��C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C��C�{C�{C�{C�!HC�!HC�{C�{C�{C�{C�!HC�!HC�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C��C��C��C��C�{C�{C�!HC�!HC�!HC��C��C�{C�{C��C��C��C��C��C�{C�!HC�!HC�!HC�{C��C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
=D ��D�D�=D
=D�=D
=D�=D�D��D�D�=D
=D�=D
=D�=D
=D�=D	�D	��D

=D
�=D�D�=D
=D�=D
=D�=D
=D�=D
=D��D
=D��D�D�=D
=D��D
=D�=D�D�=D
=D�=D
=D��D�D��D
=D�=D
=D�=D�D��D�D�=D�D�=D�D�=D
=D�=D
=D�=D 
=D ��D!
=D!�=D"
=D"��D#
=D#�=D$
=D$�=D%�D%�=D&
=D&��D'
=D'�=D(
=D(��D)
=D)��D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-�=D.
=D.��D/�D/��D0�D0��D1�D1�=D2�D2�=D3�D3��D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7��D8�D8�=D9
=D9�=D:
=D:�=D;
=D;��D<
=D<��D=
=D=��D>�D>�=D?�D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC�=DD�DD��DE
=DE�=DF�DF�=DG�DG��DH
=DH��DI�DI��DJ
=DJ��DK
=DK��DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP�DP�=DQ�DQ��DR
=DR�=DS
=DS�=DT
=DT�=DU
=DU��DV�DV�=DW
=DW��DX
=DX�=DY
=DY�=DZ
=DZ�=D[�D[�=D\
=D\�=D]
=D]�=D^�D^�=D_
=D_�=D`�D`�=Da�Da�=Db
=Db�=Dc
=Dc�=Dd
=Dd��De
=De�=Df�Df�=Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm�Dm�=Dn
=Dn�=Do
=Do�=Dp�Dp��Dq
=Dq��Dr�Dr��Ds
=Ds�=Dt
=Dt�=Du
=Du�=Dv
=Dv�=Dw
=Dwj=DydzD�'D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�A��A��A��TA��A���A���A�A���A�1A��TA�ƨAμjA�ƨA�ƨA΃A�t�A���A��A��/A͕�A΍PA�JAͺ^A�M�A��;A̺^A�7LA���A˴9A˗�AˑhA˅A�?}Aʏ\A� �A��A��A��yAɝ�A�K�A��TAǲ-A�S�AżjA�z�A�ȴA�VA�|�A�1'A��A�t�A���A���A���A�VA� �A���A�1'A�v�A��A��A�ffA���A�1'A�v�A�5?A�dZA�O�A�  A��A�&�A���A��A��A��;A��yA�ffA���A��A�jA���A���A�r�A��A���A�1'A��\A��/A��A�z�A��A�&�A�;dA���A�  A}33AuS�AsVAp�jAjVAf�!Ac��Aa�wA_��A\�DAZ�AWdZAS�mAR�AQG�AP1AM��AI`BAH�AG"�AF�\AE/ADn�AChsAB-AA&�A@�!A@JA?oA=x�A<bA:�RA8v�A7+A5�TA5%A4�A3S�A2I�A0��A0JA.�yA-�7A,$�A*�RA*z�A)�mA)oA(��A'�7A$$�A#A"Q�A!�hA Q�A��A7LA��AJAQ�A;dA�FA33A�!AI�A��AK�Al�A�\A�
A �A�A\)A��A7LA��Az�A�-A �A�A��A�A
��A	�FA �A�hAO�A��A��AoA �A1A��A�wA�wA�-A?}A1'@�E�@���@���@�"�@�x�@���@�@�X@�K�@��@�/@�F@�n�@�x�@�%@���@��@�j@�!@�-@�G�@�(�@�o@�~�@��@�z�@��@ف@��
@�dZ@���@Չ7@�j@�A�@�1@�dZ@ҏ\@��@��/@���@Ͳ-@���@�Q�@��H@�p�@�b@�t�@��H@¸R@�@\@+@�-@��^@���@��@�t�@���@�n�@���@��@�&�@��9@��@���@��D@���@�33@���@�\)@�\)@�t�@��@�V@��7@���@��@�bN@���@���@��@��7@��@���@���@���@��@�%@�p�@��@��@�33@���@��!@���@���@���@�n�@�ff@���@�7L@�I�@��;@�|�@�@���@���@���@��@�x�@��@�hs@�?}@�%@���@��@���@��m@�Z@���@���@���@�V@�j@��@�A�@��@�K�@���@��@��!@��!@��R@��!@�v�@�J@���@�hs@�?}@�%@��u@�r�@� �@��@�b@��m@��P@��@��@�o@���@�-@��@���@�  @��
@���@�ȴ@���@���@�b@��@�l�@�\)@�S�@�C�@�;d@��@�v�@��T@��@�E�@�$�@��T@��-@��@���@���@��`@��@�%@���@���@���@�Ĝ@���@��D@�r�@�j@�bN@�A�@� �@�  @��w@�l�@�ȴ@��+@�V@�=q@��@�@��7@�&�@�%@���@��j@��@��@�(�@�dZ@���@�ȴ@��!@��\@��+@�{@���@�%@���@���@�j@�bN@�(�@��@��;@��
@��F@���@�C�@��y@��@��@�;d@�+@�+@�+@��@��@��H@���@�M�@���@���@�p�@�X@�?}@��/@�9X@��m@�t�@�"�@���@�^5@��@��#@���@��h@��@�hs@�/@�%@�Ĝ@�j@�Z@�I�@�9X@�1'@� �@�  @��m@��
@��@�|�@�o@���@�~�@���@wt�@ia�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�A��A��A��TA��A���A���A�A���A�1A��TA�ƨAμjA�ƨA�ƨA΃A�t�A���A��A��/A͕�A΍PA�JAͺ^A�M�A��;A̺^A�7LA���A˴9A˗�AˑhA˅A�?}Aʏ\A� �A��A��A��yAɝ�A�K�A��TAǲ-A�S�AżjA�z�A�ȴA�VA�|�A�1'A��A�t�A���A���A���A�VA� �A���A�1'A�v�A��A��A�ffA���A�1'A�v�A�5?A�dZA�O�A�  A��A�&�A���A��A��A��;A��yA�ffA���A��A�jA���A���A�r�A��A���A�1'A��\A��/A��A�z�A��A�&�A�;dA���A�  A}33AuS�AsVAp�jAjVAf�!Ac��Aa�wA_��A\�DAZ�AWdZAS�mAR�AQG�AP1AM��AI`BAH�AG"�AF�\AE/ADn�AChsAB-AA&�A@�!A@JA?oA=x�A<bA:�RA8v�A7+A5�TA5%A4�A3S�A2I�A0��A0JA.�yA-�7A,$�A*�RA*z�A)�mA)oA(��A'�7A$$�A#A"Q�A!�hA Q�A��A7LA��AJAQ�A;dA�FA33A�!AI�A��AK�Al�A�\A�
A �A�A\)A��A7LA��Az�A�-A �A�A��A�A
��A	�FA �A�hAO�A��A��AoA �A1A��A�wA�wA�-A?}A1'@�E�@���@���@�"�@�x�@���@�@�X@�K�@��@�/@�F@�n�@�x�@�%@���@��@�j@�!@�-@�G�@�(�@�o@�~�@��@�z�@��@ف@��
@�dZ@���@Չ7@�j@�A�@�1@�dZ@ҏ\@��@��/@���@Ͳ-@���@�Q�@��H@�p�@�b@�t�@��H@¸R@�@\@+@�-@��^@���@��@�t�@���@�n�@���@��@�&�@��9@��@���@��D@���@�33@���@�\)@�\)@�t�@��@�V@��7@���@��@�bN@���@���@��@��7@��@���@���@���@��@�%@�p�@��@��@�33@���@��!@���@���@���@�n�@�ff@���@�7L@�I�@��;@�|�@�@���@���@���@��@�x�@��@�hs@�?}@�%@���@��@���@��m@�Z@���@���@���@�V@�j@��@�A�@��@�K�@���@��@��!@��!@��R@��!@�v�@�J@���@�hs@�?}@�%@��u@�r�@� �@��@�b@��m@��P@��@��@�o@���@�-@��@���@�  @��
@���@�ȴ@���@���@�b@��@�l�@�\)@�S�@�C�@�;d@��@�v�@��T@��@�E�@�$�@��T@��-@��@���@���@��`@��@�%@���@���@���@�Ĝ@���@��D@�r�@�j@�bN@�A�@� �@�  @��w@�l�@�ȴ@��+@�V@�=q@��@�@��7@�&�@�%@���@��j@��@��@�(�@�dZ@���@�ȴ@��!@��\@��+@�{@���@�%@���@���@�j@�bN@�(�@��@��;@��
@��F@���@�C�@��y@��@��@�;d@�+@�+@�+@��@��@��H@���@�M�@���@���@�p�@�X@�?}@��/@�9X@��m@�t�@�"�@���@�^5@��@��#@���@��h@��@�hs@�/@�%@�Ĝ@�j@�Z@�I�@�9X@�1'@� �@�  @��m@��
@��@�|�@�o@���@�~�@���@wt�@ia�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�mB	�sB	�fB	�fB	�`B	�mB	�sB	�sB	�yB	�sB	�yB	�ZB	�BB	�5B	�;B	�;B	�
B	��B	ƨB	��B	��B	�B
��B
��B
�ZB
�B
��B
��B
��BB
=BDBJBDBVBJB�B'�B2-BJ�BZBdZB}�B�?B�LB�9B��B�TB��BuB�B1'B5?B2-BB�B� B�JB�\B��B��B��B��B�hB�DB�VB�1B� Bm�BM�B6FB#�B�BDB�yB�BǮB�^B�B��B}�BffB[#BC�B!�B
��B
�TB
ŢB
�?B
�B
��B
��B
�7B
k�B
YB
G�B
&�B	�B	��B	��B	�%B	r�B	P�B	=qB	-B	"�B	�B	PB	B��B�B�B�B�fB�BB�B��B��B��B�B�5B�#B�B��B��B��B��BǮBĜB��BB��BĜB��B�
B�#B�sB�B�B�B�B�fB�ZB�TB�NB�;B�5B�B�B�B�5B�;B�ZB�`B�fB�mB�mB�B�B�B�B�B�B�B�B�B�B�B�sB�`B�ZB�ZB�B�jB��B��B�HB�ZB�B�B�B�B�B�B�B�B�B�mB�TB�fB�fB�mB�sB�mB�NB�
BÖB�dB�RB�qB��B�dB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�-B�9B�FB�XB�dB�jB�qB��BBBĜBƨBƨBȴB��B��B��B�B�5B�TB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B		7B	DB	PB	VB	bB	{B	�B	�B	!�B	 �B	�B	�B	 �B	!�B	 �B	 �B	�B	"�B	$�B	%�B	'�B	(�B	(�B	+B	1'B	8RB	:^B	;dB	A�B	E�B	I�B	K�B	O�B	Q�B	S�B	VB	YB	ZB	[#B	[#B	\)B	^5B	^5B	aHB	dZB	cTB	iyB	l�B	m�B	n�B	n�B	n�B	o�B	n�B	s�B	{�B	z�B	x�B	� B	�B	�B	�B	�1B	�DB	�DB	�DB	�JB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�FB	�FB	�RB	�qB	��B	ÖB	B	B	ÖB	��B	��B	ŢB	ȴB	ȴB	ȴB	ŢB	ŢB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
DB
DB
JB
JB
PB
VB
\B
\B
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
/5222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�mB	�sB	�fB	�fB	�`B	�mB	�sB	�sB	�yB	�sB	�yB	�ZB	�BB	�5B	�;B	�;B	�
B	��B	ƨB	��B	��B	�B
��B
��B
�ZB
�B
��B
��B
��BB
=BDBJBDBVBJB�B'�B2-BJ�BZBdZB}�B�?B�LB�9B��B�TB��BuB�B1'B5?B2-BB�B� B�JB�\B��B��B��B��B�hB�DB�VB�1B� Bm�BM�B6FB#�B�BDB�yB�BǮB�^B�B��B}�BffB[#BC�B!�B
��B
�TB
ŢB
�?B
�B
��B
��B
�7B
k�B
YB
G�B
&�B	�B	��B	��B	�%B	r�B	P�B	=qB	-B	"�B	�B	PB	B��B�B�B�B�fB�BB�B��B��B��B�B�5B�#B�B��B��B��B��BǮBĜB��BB��BĜB��B�
B�#B�sB�B�B�B�B�fB�ZB�TB�NB�;B�5B�B�B�B�5B�;B�ZB�`B�fB�mB�mB�B�B�B�B�B�B�B�B�B�B�B�sB�`B�ZB�ZB�B�jB��B��B�HB�ZB�B�B�B�B�B�B�B�B�B�mB�TB�fB�fB�mB�sB�mB�NB�
BÖB�dB�RB�qB��B�dB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�-B�9B�FB�XB�dB�jB�qB��BBBĜBƨBƨBȴB��B��B��B�B�5B�TB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	%B	1B		7B	DB	PB	VB	bB	{B	�B	�B	!�B	 �B	�B	�B	 �B	!�B	 �B	 �B	�B	"�B	$�B	%�B	'�B	(�B	(�B	+B	1'B	8RB	:^B	;dB	A�B	E�B	I�B	K�B	O�B	Q�B	S�B	VB	YB	ZB	[#B	[#B	\)B	^5B	^5B	aHB	dZB	cTB	iyB	l�B	m�B	n�B	n�B	n�B	o�B	n�B	s�B	{�B	z�B	x�B	� B	�B	�B	�B	�1B	�DB	�DB	�DB	�JB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�FB	�FB	�RB	�qB	��B	ÖB	B	B	ÖB	��B	��B	ŢB	ȴB	ȴB	ȴB	ŢB	ŢB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
DB
DB
JB
JB
PB
VB
\B
\B
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
/5222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190510                              AO  ARCAADJP                                                                    20181005190510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190510  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190510  QCF$                G�O�G�O�G�O�8000            