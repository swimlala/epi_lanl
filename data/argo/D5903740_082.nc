CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:47Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230847  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  4055_7112_082                   2C  D   APEX                            5374                            041511                          846 @���ɓ�1   @���[�?�@;#�
=p��dhr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�C3D���D���D��fD�FfD��3D���D�	�D�@ D�vfD���D�fD�FfDڐ D��fD�3D�#3D�y�D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
=D �=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D	
=D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"
=D"�=D#
=D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'�=D(
=D(�=D)
=D)�=D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-�=D.
=D.�=D/�D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG
=DG�=DH
=DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT�=DU
=DU�=DV
=DV�=DW
=DW�=DX
=DX�=DY
=DY�=DZ
=DZ�=D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`
=D`�=Da
=Da�=Db
=Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt}pDy�
D�!�D�HRD���D���D���D�K�D��RD���D��D�ED�{�D���D��D�K�DڕD�˅D�RD�(RD�~�D�Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A��A���A��A��A��A��A���A��A��A�
=A�l�A���A�dZA�r�A���A�p�A�1A�{A�ZA���A�z�A�"�A�x�A�S�A�{A�VA�ZA�A�A���A�E�A�A�VA�z�A���A�{A���A�bA�"�A�z�A���A�~�A�?}A�%A���A�jA�bA��!A�5?A���A�XA��jA�?}A�  A��A�I�A�  A���A��\A�A�A��PA�-A�oA�1A��A�=qA��A�+A�1'A��/A���A��A��A�=qA��A�x�A��A���A��A��/A���A�O�A�\)A��\A���A�r�A� �A���A�M�A��hA���A��HA�ZA���A�{A�G�A�ƨA�Q�A��A�l�A��A�x�A�+A��wA�ƨA�S�A��A��AoA~VA}x�A|�\A|bA{|�AzE�Ax9XAv��AuK�As�Aq"�Ap��Ao�FAn~�Al�+Aj �Ag�;Ae�mAd��Ac��Aa%A_?}A\��AY|�AXv�AV��AT�yATI�AS�
AS|�AR�yARz�AQ�
AQt�AQl�AQ\)AQ�AP�\AO7LAN5?AL��AH��AG�AG&�AF�\AE�hAD�AD�ADE�AC33AB-AA
=A?�hA=�7A<�jA;A:�HA:(�A9��A9"�A8�\A6�A6JA5/A3O�A2^5A1��A1�A0�RA0�A/�-A/S�A.1'A+�TA+�hA+XA*I�A)&�A(��A'��A&z�A%�^A%C�A$��A$�uA$A�A#�#A#�
A#��A"��A"�A!\)A �A9XAffA�mA�A~�AA�
A7LA^5A(�AA��A�A��A-A��AC�A��AJAK�A9XA��A�uAA�uAr�A=qA�A�A�AS�A
��A	�AA�AA�mA��AXAȴA�\AZA  A��A bN@���@��F@� �@���@��@���@�33@���@�bN@�7@�@���@�v�@�Q�@�O�@��@�J@�Q�@�E�@�j@ץ�@�+@�7L@�  @�{@��@�(�@�l�@Ο�@͙�@�A�@ˮ@ʗ�@�%@�  @���@�n�@�{@�@�G�@�7L@��@�b@��H@�E�@��#@��h@�X@���@�@�Z@���@�v�@�5?@��@���@���@���@���@���@���@���@���@�hs@�r�@�33@�J@��7@�`B@�7L@��@��/@��@��u@�r�@�Q�@�  @��P@�33@��y@��!@�~�@�n�@�V@�`B@�{@���@���@��@�^5@��-@�Z@���@�5?@�Q�@��@�o@��y@��@���@��\@��+@�~�@�^5@�$�@��@��@�{@�{@�{@�J@�J@�@�@�J@�@�@��@��T@��T@���@��@���@���@�  @��H@�M�@�hs@��@���@�S�@��@��!@�~�@�^5@�5?@���@��`@�I�@��;@��@�K�@���@�`B@�%@���@�I�@��@���@��@���@���@�S�@�33@�@��@�^5@�@��@��^@�V@�l�@�M�@�@���@���@��h@�/@��`@���@�Q�@��@�33@��H@�ff@��@��@��@��m@�l�@���@�$�@�x�@�V@��j@���@��D@��@�j@�bN@�Q�@�I�@�(�@�1@��m@��@�;d@��H@���@��@��!@��@��^@�`B@���@���@���@�Ĝ@��j@��9@���@��u@��D@��@�z�@�z�@�z�@�r�@�r�@�Z@�1'@\)@~��@}�T@}�@|�@{�F@z�H@yhs@x�9@x�u@xr�@xb@w
=@vv�@t��@l�@c��@^E�@T��@M�-@H  @A��@=V@4�/@/l�@(�@#ƨ@E�@�@��@M�@l�@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A��A���A��A��A��A��A���A��A��A�
=A�l�A���A�dZA�r�A���A�p�A�1A�{A�ZA���A�z�A�"�A�x�A�S�A�{A�VA�ZA�A�A���A�E�A�A�VA�z�A���A�{A���A�bA�"�A�z�A���A�~�A�?}A�%A���A�jA�bA��!A�5?A���A�XA��jA�?}A�  A��A�I�A�  A���A��\A�A�A��PA�-A�oA�1A��A�=qA��A�+A�1'A��/A���A��A��A�=qA��A�x�A��A���A��A��/A���A�O�A�\)A��\A���A�r�A� �A���A�M�A��hA���A��HA�ZA���A�{A�G�A�ƨA�Q�A��A�l�A��A�x�A�+A��wA�ƨA�S�A��A��AoA~VA}x�A|�\A|bA{|�AzE�Ax9XAv��AuK�As�Aq"�Ap��Ao�FAn~�Al�+Aj �Ag�;Ae�mAd��Ac��Aa%A_?}A\��AY|�AXv�AV��AT�yATI�AS�
AS|�AR�yARz�AQ�
AQt�AQl�AQ\)AQ�AP�\AO7LAN5?AL��AH��AG�AG&�AF�\AE�hAD�AD�ADE�AC33AB-AA
=A?�hA=�7A<�jA;A:�HA:(�A9��A9"�A8�\A6�A6JA5/A3O�A2^5A1��A1�A0�RA0�A/�-A/S�A.1'A+�TA+�hA+XA*I�A)&�A(��A'��A&z�A%�^A%C�A$��A$�uA$A�A#�#A#�
A#��A"��A"�A!\)A �A9XAffA�mA�A~�AA�
A7LA^5A(�AA��A�A��A-A��AC�A��AJAK�A9XA��A�uAA�uAr�A=qA�A�A�AS�A
��A	�AA�AA�mA��AXAȴA�\AZA  A��A bN@���@��F@� �@���@��@���@�33@���@�bN@�7@�@���@�v�@�Q�@�O�@��@�J@�Q�@�E�@�j@ץ�@�+@�7L@�  @�{@��@�(�@�l�@Ο�@͙�@�A�@ˮ@ʗ�@�%@�  @���@�n�@�{@�@�G�@�7L@��@�b@��H@�E�@��#@��h@�X@���@�@�Z@���@�v�@�5?@��@���@���@���@���@���@���@���@���@�hs@�r�@�33@�J@��7@�`B@�7L@��@��/@��@��u@�r�@�Q�@�  @��P@�33@��y@��!@�~�@�n�@�V@�`B@�{@���@���@��@�^5@��-@�Z@���@�5?@�Q�@��@�o@��y@��@���@��\@��+@�~�@�^5@�$�@��@��@�{@�{@�{@�J@�J@�@�@�J@�@�@��@��T@��T@���@��@���@���@�  @��H@�M�@�hs@��@���@�S�@��@��!@�~�@�^5@�5?@���@��`@�I�@��;@��@�K�@���@�`B@�%@���@�I�@��@���@��@���@���@�S�@�33@�@��@�^5@�@��@��^@�V@�l�@�M�@�@���@���@��h@�/@��`@���@�Q�@��@�33@��H@�ff@��@��@��@��m@�l�@���@�$�@�x�@�V@��j@���@��D@��@�j@�bN@�Q�@�I�@�(�@�1@��m@��@�;d@��H@���@��@��!@��@��^@�`B@���@���@���@�Ĝ@��j@��9@���@��u@��D@��@�z�@�z�@�z�@�r�@�r�@�Z@�1'@\)@~��@}�T@}�@|�@{�F@z�H@yhs@x�9@x�u@xr�@xb@w
=@vv�@t��@l�@c��@^E�@T��@M�-@H  @A��@=V@4�/@/l�@(�@#ƨ@E�@�@��@M�@l�@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��BȴBƨB��B�RB�9B�'B�B�B��B��B��B��B�bB�DB�B�B~�Bq�BgmBP�BI�BE�B@�B6FB'�B�B�BB��B�B�fB�)B�B�B�B�B��B��B��BÖB�dB�?B�B��B��B��B�VB�1B�B� Bw�Bp�Bl�BhsBk�Bk�B`BBYBXBdZBgmB`BBYBG�B>wB5?B,B+B2-B�BoBJBB��B�yB�BƨB�jB�-B��B��B�\By�BhsBZB:^B$�BbBB
�NB
ɺB
�jB
�B
��B
��B
��B
�B
y�B
l�B
^5B
W
B
O�B
G�B
>wB
9XB
33B
$�B
oB
B	��B	�sB	�B	�B	�B	��B	�}B	��B	��B	�B	y�B	p�B	aHB	S�B	F�B	8RB	33B	0!B	-B	,B	+B	)�B	(�B	'�B	&�B	%�B	%�B	$�B	"�B	 �B	�B	�B	\B	B	  B��B��B��B��B�B�B�B�mB�NB�#B��B��B��B��B��BȴBƨBÖB�wB�dB�RB�9B�3B�'B�'B�B�B�B�B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�1B�B{�Bv�Bs�Bq�Bm�BiyBffBdZBbNBbNB`BB_;B]/B\)BZBYBW
BT�BR�BP�BN�BK�BH�BE�BE�BD�BC�BC�BB�BA�B@�B>wB:^B6FB33B0!B-B+B)�B)�B(�B'�B%�B"�B�B�B�B�B�B{BhBPBDBDB
=B	7B+B%B+B%B+B+B+B1B1B+B+B+B1B1B1B1B	7B	7B
=B
=B
=BJBDBPBVB\B\BVBVB\B\BbB�B�B�B�B�B�B!�B'�B,B,B.B1'B1'B1'B1'B2-B2-B2-B33B33B8RB;dB>wB?}B?}B?}B?}B?}B@�B@�BA�BB�BB�BC�BE�BH�BK�BM�BM�BM�BO�B\)B`BBaHBcTBhsBk�Bo�Bq�Bu�B}�B�B�B�B�B�B�%B�%B�%B�%B�+B�+B�+B�+B�+B�+B�+B�+B�1B�1B�+B�1B�1B�1B�1B�1B�1B�7B�DB�DB�VB�{B��B��B��B��B��B��B�B�B�'B�-B�FB�dB��BBĜBŢBƨB��B�B�B�)B�/B�5B�BB�BB�BB�NB�TB�TB�ZB�fB�sB�yB�B�B�B��B��B��B��B��B	B	+B		7B	DB	bB	oB	{B	�B	�B	!�B	#�B	'�B	)�B	/B	33B	9XB	>wB	B�B	C�B	C�B	D�B	F�B	F�B	F�B	G�B	G�B	G�B	H�B	K�B	Q�B	VB	VB	VB	VB	]/B	_;B	bNB	gmB	hsB	hsB	iyB	iyB	iyB	k�B	k�B	k�B	k�B	k�B	k�B	k�B	l�B	k�B	l�B	l�B	n�B	o�B	p�B	o�B	q�B	u�B	x�B	~�B	�B	�B	�B	�B	�=B	�PB	�oB	�B	��B	�)B	��B
+B
{B
�B
'�B
33B
;dB
D�B
J�B
P�B
W
B
]/B
aHB
e`B
jB
p�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��BȮBƞB��B�LB�1B�B�
B��B��B��B��B��B�ZB�AB�B�B~�Bq�Bg`BP�BI�BE�B@xB6<B'�B�B�BB��B�B�XB�B�B��B�B�B��B��BʱBÈB�UB�0B��B��B��B�rB�FB�%B�	B�Bw�Bp�BlzBheBkzBkxB`4BY
BXBdIBg]B`/BYBG�B>iB52B+�B*�B2B�BaB:BB��B�jB�BƖB�XB�B��B��B�IBy�BhdBZB:NB$�BTBB
�?B
ɮB
�^B
�B
��B
��B
�|B
�B
y�B
l�B
^)B
W B
O�B
G�B
>mB
9MB
3*B
$�B
dB
B	��B	�lB	�B	�B	�	B	��B	�sB	��B	�|B	�B	y�B	p�B	aFB	S�B	F�B	8LB	31B	0 B	-B	,B	*�B	)�B	(�B	'�B	&�B	%�B	%�B	$�B	"�B	 �B	�B	�B	[B	B	 B��B��B��B��B�B�B�B�mB�OB�!B�B��B��B��B��BȴBƩBÚB�yB�hB�TB�;B�4B�(B�*B�!B�B�B�B�)B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�wB�eB�SB�4B�B{�Bv�Bs�Bq�Bm�Bi}BfjBd_BbTBbRB`HB_CB]3B\-BZ BYBWBU BR�BP�BN�BK�BH�BE�BE�BD�BC�BC�BB�BA�B@�B>}B:eB6KB3:B0(B-B+B)�B)�B(�B'�B%�B"�B�B�B|BxBvBhBTB=BLB0B
)B	%BB-B1B,B2BB1BBB1BBBBBB8B	"B	=B
&B
*B
)B6B.B<BBBaBaBBB\B`B_BMB�B�BpB�B�B�B!�B'�B,B,B.B1,B1-B1,B1+B21B23B22B37B36B8WB;gB>zB?�B?�B?�B?}B?�B@�B@�BA�BB�BB�BC�BE�BH�BK�BM�BM�BM�BO�B\*B`CBaHBcTBhtBk�Bo�Bq�Bu�B}�B�B�B�B�B�B�#B�%B�"B�#B�,B�*B�,B�,B�,B�,B�*B�)B�/B�1B�*B�0B�0B�0B�1B�2B�0B�5B�FB�CB�TB�{B��B��B��B��B��B��B�B�B�"B�(B�CB�bB��BBĜBşBƣB��B��B�B�#B�+B�/B�>B�>B�>B�IB�OB�RB�XB�^B�lB�tB�zB�B�B��B��B��B��B��B	B	$B		/B	<B	]B	hB	tB	�B	�B	!�B	#�B	'�B	)�B	/B	3)B	9RB	>mB	B�B	C�B	C�B	D�B	F�B	F�B	F�B	G�B	G�B	G�B	H�B	K�B	Q�B	U�B	U�B	U�B	U�B	]$B	_0B	bCB	gbB	hkB	hkB	imB	ioB	ipB	kzB	kzB	k|B	k}B	k|B	k|B	kzB	lB	k}B	l~B	lB	n�B	o�B	p�B	o�B	q�B	u�B	x�B	~�B	�B	�B	�B	�B	�1B	�DB	�aB	��B	˸B	�B	��B
B
jB
�B
'�B
3#B
;TB
D�B
J�B
P�B
V�B
]B
a5B
ePB
jnB
p�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140721230847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230847  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230847  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                