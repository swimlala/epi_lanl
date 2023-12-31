CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:29Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191729  20181005191729  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��䉫�1   @�����@6F$�/��d"��`B1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BO��BW��B`  Bg��Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&�C(�C*  C,  C.  C0�C2  C4�C6  C7�fC:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CY�fC\  C^  C_�fCa�fCd  Cf  Ch�Cj  Cl  Cn  Co�fCr  Ct�Cu�fCx  Cz�C|  C}�fC�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��C��C�  C�  C�  C��3C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��C��C�  C��3C��3C��fC��3C��C�  C��3C��C��3C��3C��3C�  C��C��C�  C��C��C��C��C��C��C��C�  C��C�  C��C��C��C��C��3C�  C��C��3C�  C�  C�  C�  C��C��C��3C��3C��3C��3C��C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C��3C��C�  C��fC�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D��D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D	��D
� D  D� DfD�fD  D� D  D� D  D� D  Dy�D��D� DfD� D  D� D  D� D  D� D  D� DfD� D  D�fD  D� D  D� D��D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D'��D(y�D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDAfDA� DB  DB�fDCfDC� DD  DD� DE  DE�fDFfDF� DG  DG� DG��DHy�DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DPfDP�fDQ  DQ� DR  DR� DS  DS�fDT  DT�fDU  DU� DV  DV� DW  DW� DXfDX�fDY  DYy�DY��DZ� D[fD[� D\  D\y�D\�3D]� D^  D^� D_fD_� D_��D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Di��Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� DwfDw� Dw� Dy��D�G\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A�\A"�\AB�\A`��A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B!
=B)
=B0��B8��B@��BH��BP=qBX=qB`��Bh=qBp��Bx��B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�CB�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&B�C(B�C*(�C,(�C.(�C0B�C2(�C4B�C6(�C8\C:(�C<(�C>(�C@\CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CTB�CVB�CX(�CZ\C\(�C^(�C`\Cb\Cd(�Cf(�ChB�Cj(�Cl(�Cn(�Cp\Cr(�CtB�Cv\Cx(�CzB�C|(�C~\C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C��C�!HC�!HC�{C�{C�{C��C��C�{C�!HC�!HC�{C�{C��C�{C�{C�{C�{C�!HC�.C�{C��C��C���C��C�!HC�{C��C�!HC��C��C��C�{C�!HC�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�!HC�{C�!HC�!HC�!HC�!HC��C�{C�!HC��C�{C�{C�{C�{C�!HC�!HC��C��C��C��C�!HC�{C�{C�{C��C�{C�!HC�{C�{C��C��C��C�!HC�{C���C�{C�{C��C��C��C��C�{C�!HC�{C�{C�{C�{C��C��C�{C��C��C��C�{C�{C��C��C��C�{C��C�{C�{C�{C�{C��C��C�{C�{C�{C�{C�{C�{C�{C�{D 
=D �=D�D�=D
=D�=D�D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D��D	
=D	�=D
�D
�=D
=D�=D�D��D
=D�=D
=D�=D
=D�=D
=D��D�D�=D�D�=D
=D�=D
=D�=D
=D�=D
=D�=D�D�=D
=D��D
=D�=D
=D�=D�D�=D
=D�=D
=D�=D
=D��D
=D�=D 
=D �=D!
=D!�=D"�D"�=D#
=D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'��D(�D(��D)
=D)�=D*
=D*��D+
=D+�=D,
=D,�=D-
=D-�=D.�D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7�D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@��DA�DA�=DB
=DB��DC�DC�=DD
=DD�=DE
=DE��DF�DF�=DG
=DG�=DH�DH��DI
=DI�=DJ
=DJ�=DK
=DK��DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP�DP��DQ
=DQ�=DR
=DR�=DS
=DS��DT
=DT��DU
=DU�=DV
=DV�=DW
=DW�=DX�DX��DY
=DY��DZ�DZ�=D[�D[�=D\
=D\��D\�pD]�=D^
=D^�=D_�D_�=D`�D`�=Da
=Da�=Db�Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg�=Dh
=Dh�=Di�Di�=Dj�Dj��Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do��Dp
=Dp��Dq
=Dq�=Dr
=Dr�=Ds
=Ds��Dt
=Dt�=Du
=Du�=Dv
=Dv�=Dw�Dw�=Dw�=Dy�
D�L{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�v�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�z�A�v�A�z�A�|�AׁA�x�A�x�A�z�A�n�A�^5A�VA�S�A�9XA��A���AՋDA�33A�"�A��TA���A�ƨAԸRAԛ�Aԉ7A�z�A�\)A�A�S�A�G�A�?}A��/AН�A�bNA�Q�A�I�A�A�A�;dA�-A�oA�A���AυA�VA�7LA���A��
A�A���A�r�A��A�K�A�O�A�VA�\)A�z�A�(�A��`A���A�dZA�G�A��+A���A�x�A�%A�E�A�ZA�dZA�ZA�-A��HA�
=A��9A�M�A�$�A���A��^A��yA�VA�33A��/A�(�A��A���A�hsA��`A�$�A�{A���A�JA��PA��A�bA��A�XA��+A�l�A�/A���A���A�A�1A�G�A�jA��A�dZA�5?A�n�A�|�A���A�&�A�z�A��+A�M�A��mA�
=A���A+A{��Ax��Aw\)AvjAu�Au?}As�mApz�An=qAmp�Al�/Al�AlffAk�PAiƨAh1'Ag33Ae�Ac�wAb�`Aa?}A`��A`1A_�hA]O�A\v�AZ�/AW
=AT9XAR1'AQ�hAP��APJAM�AK��AJ�!AH��AH$�AE�7ADM�AC"�AAK�A@-A?|�A>�A>�DA>�A=33A<��A<��A<jA<  A:E�A8JA6ffA4��A4bA333A21'A1�^A1|�A1G�A0�yA/S�A+�A)��A)�hA)\)A(�/A(bA&�A%S�A"jA"-A!�mA!��A!�A!;dA �A M�AoA��A�\A�FA9XAp�AXA~�A�7A�RAjA$�A��A�A��A~�A�-AVAA�A
A�A	hsA��AAv�A=qA�FAXAv�AO�AM�A�A��AQ�A=qA �@�l�@�n�@�V@��@�%@��\@��@�@�@���@�h@���@�F@�V@�j@��#@�^5@�r�@�$�@�7L@��;@�@�1@�J@�1'@ۍP@ڸR@���@�(�@��@�S�@�@�V@�@�O�@��@Л�@���@���@�7L@̴9@�o@�@��@��@ȣ�@�+@�$�@��@ċD@Õ�@�
=@�S�@���@�@�ff@��^@��9@�|�@�p�@��@�J@�J@�-@���@�n�@�$�@���@�o@���@�hs@���@�  @�@��#@�O�@���@��@�x�@�z�@�(�@�t�@��@�Ĝ@�bN@��@�
=@�ȴ@���@��H@��@�o@���@��y@�@�ƨ@�  @��m@�  @�|�@�C�@���@�n�@��-@���@�`B@�z�@�b@��@���@���@��@��h@��@��@�\)@�;d@�@��!@���@�~�@�-@��T@�J@��#@��h@�x�@�`B@�/@��`@��
@�dZ@�C�@�
=@�~�@�E�@��@�@��^@�x�@���@�Ĝ@��j@�z�@�Q�@�I�@�A�@�1'@�b@��;@��m@���@�V@�(�@�t�@��@�(�@�j@�z�@�A�@� �@��;@���@�b@�;d@���@��+@�^5@��\@���@�=q@�$�@�J@���@��#@��@��u@�|�@�@��y@��R@��+@�M�@�@���@���@�=q@���@���@�@�33@�C�@��y@��R@�ff@�~�@�{@��/@��9@���@�%@��@�V@��@�9X@��@�t�@��H@�V@�{@�v�@�ff@��+@�~�@�5?@�$�@�=q@�-@�{@���@���@��-@���@��h@���@��7@�O�@�&�@��@��@�Ĝ@��@��u@��@�z�@�r�@�Z@�A�@�1'@�b@��
@��@�t�@�K�@�33@�ȴ@�v�@�ff@�$�@���@��-@�7L@��@~^5@l��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�v�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�z�A�v�A�z�A�|�AׁA�x�A�x�A�z�A�n�A�^5A�VA�S�A�9XA��A���AՋDA�33A�"�A��TA���A�ƨAԸRAԛ�Aԉ7A�z�A�\)A�A�S�A�G�A�?}A��/AН�A�bNA�Q�A�I�A�A�A�;dA�-A�oA�A���AυA�VA�7LA���A��
A�A���A�r�A��A�K�A�O�A�VA�\)A�z�A�(�A��`A���A�dZA�G�A��+A���A�x�A�%A�E�A�ZA�dZA�ZA�-A��HA�
=A��9A�M�A�$�A���A��^A��yA�VA�33A��/A�(�A��A���A�hsA��`A�$�A�{A���A�JA��PA��A�bA��A�XA��+A�l�A�/A���A���A�A�1A�G�A�jA��A�dZA�5?A�n�A�|�A���A�&�A�z�A��+A�M�A��mA�
=A���A+A{��Ax��Aw\)AvjAu�Au?}As�mApz�An=qAmp�Al�/Al�AlffAk�PAiƨAh1'Ag33Ae�Ac�wAb�`Aa?}A`��A`1A_�hA]O�A\v�AZ�/AW
=AT9XAR1'AQ�hAP��APJAM�AK��AJ�!AH��AH$�AE�7ADM�AC"�AAK�A@-A?|�A>�A>�DA>�A=33A<��A<��A<jA<  A:E�A8JA6ffA4��A4bA333A21'A1�^A1|�A1G�A0�yA/S�A+�A)��A)�hA)\)A(�/A(bA&�A%S�A"jA"-A!�mA!��A!�A!;dA �A M�AoA��A�\A�FA9XAp�AXA~�A�7A�RAjA$�A��A�A��A~�A�-AVAA�A
A�A	hsA��AAv�A=qA�FAXAv�AO�AM�A�A��AQ�A=qA �@�l�@�n�@�V@��@�%@��\@��@�@�@���@�h@���@�F@�V@�j@��#@�^5@�r�@�$�@�7L@��;@�@�1@�J@�1'@ۍP@ڸR@���@�(�@��@�S�@�@�V@�@�O�@��@Л�@���@���@�7L@̴9@�o@�@��@��@ȣ�@�+@�$�@��@ċD@Õ�@�
=@�S�@���@�@�ff@��^@��9@�|�@�p�@��@�J@�J@�-@���@�n�@�$�@���@�o@���@�hs@���@�  @�@��#@�O�@���@��@�x�@�z�@�(�@�t�@��@�Ĝ@�bN@��@�
=@�ȴ@���@��H@��@�o@���@��y@�@�ƨ@�  @��m@�  @�|�@�C�@���@�n�@��-@���@�`B@�z�@�b@��@���@���@��@��h@��@��@�\)@�;d@�@��!@���@�~�@�-@��T@�J@��#@��h@�x�@�`B@�/@��`@��
@�dZ@�C�@�
=@�~�@�E�@��@�@��^@�x�@���@�Ĝ@��j@�z�@�Q�@�I�@�A�@�1'@�b@��;@��m@���@�V@�(�@�t�@��@�(�@�j@�z�@�A�@� �@��;@���@�b@�;d@���@��+@�^5@��\@���@�=q@�$�@�J@���@��#@��@��u@�|�@�@��y@��R@��+@�M�@�@���@���@�=q@���@���@�@�33@�C�@��y@��R@�ff@�~�@�{@��/@��9@���@�%@��@�V@��@�9X@��@�t�@��H@�V@�{@�v�@�ff@��+@�~�@�5?@�$�@�=q@�-@�{@���@���@��-@���@��h@���@��7@�O�@�&�@��@��@�Ĝ@��@��u@��@�z�@�r�@�Z@�A�@�1'@�b@��
@��@�t�@�K�@�33@�ȴ@�v�@�ff@�$�@���@��-@�7L@��@~^5@l��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BN�BN�BN�BM�BO�BR�BS�BS�BXBbNB� B�DB�oB�uB��B��B��B��B��B��B��B��B��B�BB�B�`B�B  B%B	7BDBVBhBhBhBhBhBhBbBuB,BC�BL�B_;BiyBw�B�=B�bB��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�oB� Bo�Bp�Bk�BaHBVBN�BS�B\)BZBF�B9XB+B�B�BbB
=BB��B�B�)B��BÖB�XB�LB�?B�B��B|�B]/BA�B%�B�B�B1B
�B
��B
�FB
��B
��B
��B
��B
�uB
�DB
|�B
dZB
Q�B
F�B
?}B
;dB
5?B
+B
�B

=B
B	��B	��B	��B	��B	�sB	�/B	��B	ǮB	�jB	�FB	�B	��B	��B	��B	��B	�hB	�%B	o�B	_;B	T�B	Q�B	N�B	I�B	?}B	9XB	5?B	.B	)�B	�B	�B	hB	
=B	%B	B	  B��B��B��B��B��B�B�B�sB�5B�
B��B��B��BǮBŢBÖB��B�wB�RB�'B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�DB�1B�B�B}�B{�Bw�Bt�Bt�Br�Bq�Bp�Bo�Bl�BiyBffBcTB^5BXBS�BP�BM�BK�BI�BH�BH�BF�BH�BI�BG�BN�BS�BiyBl�BbNBM�BJ�BJ�BN�BXBS�BN�BN�BP�BS�B_;Bu�Bw�Bx�Bv�Bq�Bl�BiyBffBdZBaHB^5B\)BZBYBYB[#BYB\)B[#B]/B\)B\)B\)B\)B\)B[#B[#B^5B_;B_;B`BBcTBffBhsBs�Bx�B{�B}�B~�B�B�B�B�B�%B�7B�%B�B}�By�Bu�Bu�Bv�By�B�B�B�B� B{�By�Bz�B~�B�%B�7B�DB�PB�PB�PB�DB�DB�=B�7B�=B�PB�VB�bB�uB��B��B��B��B�B�'B�RB�qBŢBǮB��B��B�B�TB�sB�B��B��B	  B	B	B	B	%B	+B	JB	DB	JB	VB	VB	VB	\B	\B	\B	bB	{B	�B	!�B	!�B	#�B	$�B	&�B	&�B	(�B	33B	6FB	7LB	:^B	@�B	C�B	D�B	H�B	I�B	L�B	S�B	T�B	VB	YB	ZB	[#B	[#B	[#B	\)B	^5B	`BB	ffB	l�B	k�B	k�B	o�B	w�B	|�B	~�B	~�B	~�B	~�B	�B	�%B	�B	�B	�B	�%B	�DB	�\B	�\B	�\B	�bB	�uB	�{B	��B	��B	�uB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�3B	�-B	�'B	�'B	�3B	�RB	�^B	�jB	�jB	�jB	�dB	�dB	�^B	�^B	�jB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�#B	�)B	�/B	�;B	�5B	�;B	�;B	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�-B
�B
Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BL�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BN�BN�BN�BM�BO�BR�BS�BS�BXBbNB� B�DB�oB�uB��B��B��B��B��B��B��B��B��B�BB�B�`B�B  B%B	7BDBVBhBhBhBhBhBhBbBuB,BC�BL�B_;BiyBw�B�=B�bB��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B�oB� Bo�Bp�Bk�BaHBVBN�BS�B\)BZBF�B9XB+B�B�BbB
=BB��B�B�)B��BÖB�XB�LB�?B�B��B|�B]/BA�B%�B�B�B1B
�B
��B
�FB
��B
��B
��B
��B
�uB
�DB
|�B
dZB
Q�B
F�B
?}B
;dB
5?B
+B
�B

=B
B	��B	��B	��B	��B	�sB	�/B	��B	ǮB	�jB	�FB	�B	��B	��B	��B	��B	�hB	�%B	o�B	_;B	T�B	Q�B	N�B	I�B	?}B	9XB	5?B	.B	)�B	�B	�B	hB	
=B	%B	B	  B��B��B��B��B��B�B�B�sB�5B�
B��B��B��BǮBŢBÖB��B�wB�RB�'B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�DB�1B�B�B}�B{�Bw�Bt�Bt�Br�Bq�Bp�Bo�Bl�BiyBffBcTB^5BXBS�BP�BM�BK�BI�BH�BH�BF�BH�BI�BG�BN�BS�BiyBl�BbNBM�BJ�BJ�BN�BXBS�BN�BN�BP�BS�B_;Bu�Bw�Bx�Bv�Bq�Bl�BiyBffBdZBaHB^5B\)BZBYBYB[#BYB\)B[#B]/B\)B\)B\)B\)B\)B[#B[#B^5B_;B_;B`BBcTBffBhsBs�Bx�B{�B}�B~�B�B�B�B�B�%B�7B�%B�B}�By�Bu�Bu�Bv�By�B�B�B�B� B{�By�Bz�B~�B�%B�7B�DB�PB�PB�PB�DB�DB�=B�7B�=B�PB�VB�bB�uB��B��B��B��B�B�'B�RB�qBŢBǮB��B��B�B�TB�sB�B��B��B	  B	B	B	B	%B	+B	JB	DB	JB	VB	VB	VB	\B	\B	\B	bB	{B	�B	!�B	!�B	#�B	$�B	&�B	&�B	(�B	33B	6FB	7LB	:^B	@�B	C�B	D�B	H�B	I�B	L�B	S�B	T�B	VB	YB	ZB	[#B	[#B	[#B	\)B	^5B	`BB	ffB	l�B	k�B	k�B	o�B	w�B	|�B	~�B	~�B	~�B	~�B	�B	�%B	�B	�B	�B	�%B	�DB	�\B	�\B	�\B	�bB	�uB	�{B	��B	��B	�uB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�3B	�-B	�'B	�'B	�3B	�RB	�^B	�jB	�jB	�jB	�dB	�dB	�^B	�^B	�jB	��B	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�#B	�)B	�/B	�;B	�5B	�;B	�;B	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�-B
�B
Q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191729                              AO  ARCAADJP                                                                    20181005191729    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191729  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191729  QCF$                G�O�G�O�G�O�8000            