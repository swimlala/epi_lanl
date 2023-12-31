CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:55Z AOML 3.0 creation; 2016-08-07T21:36:32Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221355  20160807143632  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  5286_8897_032                   2C  D   APEX                            6531                            072314                          846 @�;2%+ 1   @�;2�@�@2�33333�c�     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C33C��C��C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D�	�D�FfD�� D�� D��fD�<�D�vfD���D�3D�c3D��3D��3D�fD�I�D�s3D�ٚD���D�)�D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J=q@��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C\)C��C��C\C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=D.�=D/
=D/�=D0
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
=Dtw
Dy�
D��D�K�D��D��D���D�A�D�{�D���D�RD�hRD��RD��RD��D�N�D�xRD�޹D���D�.�D�eD�e111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�^5A�`BA�^5A�ZA�\)A�^5A�\)A�O�A�+A�"�A��A��A��A��A� �A��A��A�JA�%A�A���A��A��/A��TA��A��`A�ȴAʧ�Aʟ�AʸRAʼjA���A��A���A���A��A��A��A��TA��#A��/A��/A��#A���AʼjAʶFAʣ�AʓuA�|�A�S�A���A�~�A�n�A�ZA��yA�A�A��
AǴ9AǓuA�C�A��AƃA�^5A��A��AŶFA�ZA��A�r�A���A�Q�A�O�A�^5A�(�A��A��mA��\A�JA�ffA���A��jA��A�5?A�dZA��RA��A�A�A��A�ZA�VA���A�1'A��\A�1A�-A���A�oA�-A���A��TA�t�A���A� �A��wA�(�A�VA���A|bNAsx�AmG�AkƨAj^5Ah��AhbAgK�Ab�!A`bNA\��AYx�AX9XAV�AQ�7AN-AK��AH�`AGoAD�\AB�9A@A�A=/A9�A5`BA2-A1K�A0��A0�!A0�A/��A.M�A-t�A,��A,bA*ȴA'�#A&��A&  A%K�A$ĜA$5?A#�^A"�A"�DA!�PA �!A��A�A�A�HA��AVA;dAv�A9XA�
AO�A��A��AQ�A�#A�A�PA9XA�A�A��A�AVA�A�FA33A�^A&�At�A	�TA�AbNA�PA�A�A�!A �A��A��A;dA�uA�;A I�@�t�@���@� �@��@��@�+@���@��y@�V@�@���@���@�@웦@�P@�|�@�|�@�C�@�5?@�G�@�I�@��@噚@�z�@�F@���@�~�@�7@�Ĝ@��D@�j@�  @ߍP@���@ݙ�@�&�@�  @�ff@�@��@��/@�I�@��@�-@�J@ա�@�hs@�G�@��@��/@�z�@ӶF@�v�@�=q@�5?@ѡ�@�7L@��`@�Q�@��@Ͼw@ϥ�@ϕ�@϶F@�ƨ@ϥ�@�S�@��#@���@��@̬@�Ĝ@��`@� �@��@ə�@�7L@�G�@ɉ7@ɉ7@�`B@��`@�x�@�r�@���@ȴ9@ʸR@�ȴ@�hs@��@���@�hs@��@�I�@�"�@�o@ǅ@�Z@�G�@���@�1'@��@Ɨ�@Ɨ�@���@�l�@���@�$�@š�@��@�A�@���@�t�@�dZ@�t�@��@\@�@�p�@���@�j@�  @���@�C�@��H@�V@��@��T@��-@�p�@�/@���@���@�1@�n�@�@�E�@�-@�J@��@���@�@���@��h@��@�(�@���@���@�-@�J@�$�@�J@��h@�p�@�X@�G�@�&�@�%@�A�@��y@��!@��\@�M�@�p�@��@�l�@�@��!@��@��h@�/@���@��@�bN@�1@���@�@��H@�ȴ@��+@�n�@�-@��#@�x�@�/@���@��@��@��@�o@�ȴ@�v�@�{@�x�@�7L@���@��`@��j@�(�@���@�S�@��y@�n�@��@��T@��^@���@��@�/@��@���@��/@���@���@��u@�bN@�I�@�b@�ƨ@���@��@�dZ@�l�@�\)@�S�@�+@��+@�E�@�-@��^@���@�x�@�&�@�?}@��@��j@�A�@��@��F@�S�@�K�@��@��\@�v�@�ff@�E�@���@���@��-@��7@�?}@���@���@���@��@� �@���@�t�@��@��H@��H@���@�5?@�{@��T@�p�@��@�%@���@��@���@��9@�Z@��@��@��y@��+@�5?@�-@���@��-@���@��7@�x�@���@�=q@�=q@�-@�=q@�5?@��T@�M�@�Z@�E�@z~�@qG�@i��@cC�@Z-@O�@Ihs@B-@>5?@7�@0Ĝ@+C�@&5?@!G�@��@l�@z�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�^5A�^5A�`BA�^5A�ZA�\)A�^5A�\)A�O�A�+A�"�A��A��A��A��A� �A��A��A�JA�%A�A���A��A��/A��TA��A��`A�ȴAʧ�Aʟ�AʸRAʼjA���A��A���A���A��A��A��A��TA��#A��/A��/A��#A���AʼjAʶFAʣ�AʓuA�|�A�S�A���A�~�A�n�A�ZA��yA�A�A��
AǴ9AǓuA�C�A��AƃA�^5A��A��AŶFA�ZA��A�r�A���A�Q�A�O�A�^5A�(�A��A��mA��\A�JA�ffA���A��jA��A�5?A�dZA��RA��A�A�A��A�ZA�VA���A�1'A��\A�1A�-A���A�oA�-A���A��TA�t�A���A� �A��wA�(�A�VA���A|bNAsx�AmG�AkƨAj^5Ah��AhbAgK�Ab�!A`bNA\��AYx�AX9XAV�AQ�7AN-AK��AH�`AGoAD�\AB�9A@A�A=/A9�A5`BA2-A1K�A0��A0�!A0�A/��A.M�A-t�A,��A,bA*ȴA'�#A&��A&  A%K�A$ĜA$5?A#�^A"�A"�DA!�PA �!A��A�A�A�HA��AVA;dAv�A9XA�
AO�A��A��AQ�A�#A�A�PA9XA�A�A��A�AVA�A�FA33A�^A&�At�A	�TA�AbNA�PA�A�A�!A �A��A��A;dA�uA�;A I�@�t�@���@� �@��@��@�+@���@��y@�V@�@���@���@�@웦@�P@�|�@�|�@�C�@�5?@�G�@�I�@��@噚@�z�@�F@���@�~�@�7@�Ĝ@��D@�j@�  @ߍP@���@ݙ�@�&�@�  @�ff@�@��@��/@�I�@��@�-@�J@ա�@�hs@�G�@��@��/@�z�@ӶF@�v�@�=q@�5?@ѡ�@�7L@��`@�Q�@��@Ͼw@ϥ�@ϕ�@϶F@�ƨ@ϥ�@�S�@��#@���@��@̬@�Ĝ@��`@� �@��@ə�@�7L@�G�@ɉ7@ɉ7@�`B@��`@�x�@�r�@���@ȴ9@ʸR@�ȴ@�hs@��@���@�hs@��@�I�@�"�@�o@ǅ@�Z@�G�@���@�1'@��@Ɨ�@Ɨ�@���@�l�@���@�$�@š�@��@�A�@���@�t�@�dZ@�t�@��@\@�@�p�@���@�j@�  @���@�C�@��H@�V@��@��T@��-@�p�@�/@���@���@�1@�n�@�@�E�@�-@�J@��@���@�@���@��h@��@�(�@���@���@�-@�J@�$�@�J@��h@�p�@�X@�G�@�&�@�%@�A�@��y@��!@��\@�M�@�p�@��@�l�@�@��!@��@��h@�/@���@��@�bN@�1@���@�@��H@�ȴ@��+@�n�@�-@��#@�x�@�/@���@��@��@��@�o@�ȴ@�v�@�{@�x�@�7L@���@��`@��j@�(�@���@�S�@��y@�n�@��@��T@��^@���@��@�/@��@���@��/@���@���@��u@�bN@�I�@�b@�ƨ@���@��@�dZ@�l�@�\)@�S�@�+@��+@�E�@�-@��^@���@�x�@�&�@�?}@��@��j@�A�@��@��F@�S�@�K�@��@��\@�v�@�ff@�E�@���@���@��-@��7@�?}@���@���@���@��@� �@���@�t�@��@��H@��H@���@�5?@�{@��T@�p�@��@�%@���@��@���@��9@�Z@��@��@��y@��+@�5?@�-@���@��-@���@��7@�x�@���@�=q@�=q@�-@�=q@�5?G�O�@�M�@�Z@�E�@z~�@qG�@i��@cC�@Z-@O�@Ihs@B-@>5?@7�@0Ĝ@+C�@&5?@!G�@��@l�@z�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB=qB=qB<jB=qB=qB=qB=qB>wB@�BC�BC�BD�BE�BF�BH�BJ�BL�BO�BP�BR�BVBaHBw�B�=B�uB��B��B��B��B��B�B�B�-B�LB�dB�wB�}B��B��B��BŢBǮBȴBȴB��B��B�B�5B�NB�B��BVB�B�B"�B:^BS�B_;BaHBcTBe`BjBq�Bs�Bv�Bv�Bw�Bz�B�B�Bt�BcTB-B	7B�B�B$�BD�B�=B�1B}�Bw�Bp�Be`B[#BK�B:^B'�B�BJB��B�B�mB�B�qB�B��B��B�%BgmB'�B  B
�B
�5B
��B
p�B
;dB
 �B	��B	ÖB	��B	�{B	�DB	�B	~�B	w�B	\)B	K�B	:^B	(�B	!�B	�B	VB	+B	B	B��B��B�B�NB�`B�BB��B�dB�FB�FB�FB�?B�3B�'B�B�B�3B�'B�^B�qB�}BBĜBƨBǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÖB�}B�}BBŢBȴB��B��B��B��B�
B��B��BȴBȴBȴBɺB��B��B��B��B��B��BɺBǮBÖB�qB�^B�XB�RB�^B�^B�^B�9B�-B�-B�-B�'B�!B�B�-B�-B�-B�-B�-B�LB�jB�qB�}B��BBĜBƨBȴB��B��B��B��B��B��B��B��B��B�B�#B�/B�BB�BB�TB�yB�B�B�B��B��B��B��B	  B	B		7B	PB	�B	�B	�B	!�B	"�B	%�B	&�B	&�B	(�B	1'B	5?B	8RB	?}B	?}B	?}B	?}B	@�B	E�B	P�B	R�B	P�B	O�B	P�B	R�B	W
B	W
B	W
B	W
B	\)B	]/B	`BB	iyB	{�B	~�B	y�B	}�B	� B	�B	�%B	�B	�B	�B	�+B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�?B	�XB	�XB	�XB	�^B	�^B	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�wB	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ÖB	��B	ǮB	ǮB	ǮB	ŢB	ÖB	ÖB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�;B	�BB	�HB	�HB	�TB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
+B
+B
1B
1B
	7B
JB
1B
\B
�B
�B
$�B
+B
/B
6FB
?}B
C�B
H�B
L�B
P�B
W
B
\)B
`BB
cTB
gmB
m�B
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B=iB=gB<\B=iB=gB=gB=iB>mB@yBC�BC�BD�BE�BF�BH�BJ�BL�BO�BP�BR�BU�Ba;Bw�B�3B�iB�}B��B��B��B��B��B� B�B�@B�XB�iB�nB�uB�uB�|BŖBǢBȦBȪBʱB��B�B�'B�@B�sB��BJB�B�B"�B:RBS�B_0Ba8BcHBeQBjsBq�Bs�Bv�Bv�Bw�Bz�B�B�Bt�BcFB- B	&B�B�B$�BD�B�/B�$B}�Bw�Bp�BeRB[BK�B:NB'�B�B<B��B�B�_B��B�aB��B��B��B�Bg_B'�B
��B
�B
�(B
��B
p�B
;WB
 �B	��B	ÏB	��B	�wB	�<B	�B	~�B	w�B	\'B	K�B	:YB	(�B	!�B	�B	VB	+B	B	B��B��B�B�PB�`B�BB��B�gB�FB�HB�GB�@B�5B�(B�B�B�4B�*B�`B�rB�BBĝBƦBǮBɼBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BØB�~B�zBBţBȳB��B��B��B��B�
B��B��BȵBȵBȴBɹB��B��B��B��B��B��BɹBǬBÔB�nB�[B�VB�RB�]B�\B�^B�7B�,B�,B�-B�&B� B�B�+B�+B�,B�,B�+B�LB�iB�pB�{B��BBĘBƥBȲB��B��B��B��B��B��B��B��B��B�B�!B�+B�@B�@B�RB�uB�B�B�B��B��B��B��B��B	B		2B	LB	}B	�B	�B	!�B	"�B	%�B	&�B	&�B	(�B	1"B	5:B	8HB	?uB	?tB	?vB	?tB	@~B	E�B	P�B	R�B	P�B	O�B	P�B	R�B	W B	WB	WB	WB	\ B	]&B	`8B	ipB	{�B	~�B	y�B	}�B	�B	�B	�B	�B	�B	�B	�!B	�LB	�}B	��B	��B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�3B	�KB	�KB	�OB	�RB	�RB	�YB	�ZB	�ZB	�WB	�ZB	�YB	�]B	�_B	�eB	�lB	�xB	B	ĐB	ĐB	đB	đB	ĒB	ËB	�B	ǣB	ǣB	ǡB	ŕB	ÉB	ÌB	ĒB	ŕB	ȦB	ɯB	ʵB	ʳB	˻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�#B	�#B	�(B	�*B	�.B	�-B	�-B	�6B	�4B	�-B	�4B	�9B	�;B	�IB	�DB	�EB	�NB	�UB	�RB	�XB	�_B	�gB	�kB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
B
B
%B
$B
	*G�O�B
$B
LB
xB
�B
$�B
*�B
/B
67B
?oB
C�B
H�B
L�B
P�B
V�B
\B
`3B
cEB
g]B
m~B
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436322016080714363220160807143632  AO  ARCAADJP                                                                    20150226221355    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221355  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221355  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143632  IP                  G�O�G�O�G�O�                