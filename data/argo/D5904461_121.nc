CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-01T09:15:18Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160601091518  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               yA   AO  5286_8897_121                   2C  D   APEX                            6531                            072314                          846 @װ���1   @װ�@yd�@4����m�cD�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    yA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy� D��D�L�D���D���D�3D�33D��3D�\�D�3D�FfD��fD��fD� D�L�D�l�D��3D�	�D�0 D�vfD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B
=B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=Ds�=Ds�pDy�=D��D�Q�D���D���D�RD�8RD��RD�a�D�RD�K�D���D�ۅD�D�Q�D�q�D��RD��D�5D�{�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��`A��
A�z�A�-AΥ�Aͩ�A�
=A�/A��A��;A���A�ƨA˴9A˙�AˍPA�ffA� �Aʴ9A�;dA��TA� �A�Q�A��TA�ƨA�S�Aš�A�?}AÁA�bA�l�A��PA�{A��yA�z�A�A��jA�ffA��^A���A���A�jA��A�=qA�-A��A��A��\A�O�A���A�ȴA�E�A�K�A�%A���A�JA���A���A�~�A�`BA�A�33A���A�&�A�  A��A� �A���A�oA�K�A�ZA��7A��A�;dA���A�dZA���A���A��A�x�A�9XA�O�A��A�  A��^A��A��/A�JA��TA�M�A��A�{A���A���A��/A�p�A��9A�t�A��yA��A���A��uA�ZA{G�Ay��Au��As�
As
=Aq�Am�#AlI�Ah��Ae\)Ac��Ab�HAb1'A_dZA]�
A]"�A[7LAY�7AU��ATA�AQ�AOS�AN1AM�7AMoAK�AI��AG�^AEK�AC"�AB5?AAƨA?��A>�\A=��A;�mA:A7�A6�A6JA4z�A2��A1��A/�A.I�A-p�A,�/A,~�A*�\A(ĜA(~�A(A�A'O�A&��A&��A%p�A#�#A#7LA#A"z�A!+A�
A=qA��A�TA��AdZA�yAjA�A�7A�A��A�A��AVA��A��A�DAK�A�9A�TAbNA�A5?A�
A��A�At�AS�A
=A
��A
��A
Q�A	��A	�^A	+Az�A�TA?}AO�AjA��AA�A�^A��A�
A�hA��A\)A&�A ȴA Z@��@�%@�dZ@�J@��^@��7@��9@��@���@�z�@��
@�t�@��@���@�(�@�;d@�n�@웦@�K�@�-@��@�D@�w@��#@� �@㕁@�!@�-@��@��@�@��@�  @޸R@�7L@�z�@�9X@��m@��@��@׮@���@��@�/@�j@���@�l�@��@�j@Ͼw@Χ�@���@�/@̴9@�1'@���@�t�@�+@�
=@�+@�K�@�@�Ĝ@Ǯ@�l�@�\)@�+@��@�hs@ļj@�dZ@�M�@�x�@��j@�33@���@�E�@��^@�`B@��@�r�@�t�@�v�@���@���@��u@�Q�@��F@�l�@���@���@�v�@�=q@��@���@���@�9X@���@��P@���@���@�7L@�r�@�9X@�9X@��m@��@�@��-@���@�E�@���@�`B@�&�@��`@��u@� �@�"�@�@�^5@���@�x�@�`B@�G�@�V@���@���@��@�ƨ@�t�@�o@�@�
=@�o@�@�^5@��-@�p�@�X@���@��u@�Q�@�b@���@��@�n�@�V@�V@�E�@�J@�@��h@�G�@���@��u@��@���@���@��@�l�@�+@�@��@�E�@�@�@�hs@�O�@�7L@�%@��D@�A�@�b@�  @��w@��@��@��@�C�@��y@���@�ff@�E�@�-@��@�@�`B@�%@��@��9@�z�@�1'@�1@��F@�\)@�@��+@�M�@�5?@�=q@���@���@�O�@���@���@�z�@��w@��@���@���@�dZ@��!@��+@�ff@��@��^@�hs@�G�@��D@��@���@��P@�l�@�"�@��H@��R@��+@�@�@���@���@���@�x�@�`B@���@���@���@�j@�(�@��m@��w@���@�l�@�C�@�+@�
=@�@��@��H@��!@�^5@�J@��@���@�hs@�&�@���@�bN@�9X@�  @��w@��@�"�@��@��+@�-@��@���@��-@��h@�p�@���@�@w��@n5?@d��@\�j@V@O�w@G��@=�h@8b@2��@-`B@)x�@#o@\)@�
@��@�
@  @I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A��HA��`A��
A�z�A�-AΥ�Aͩ�A�
=A�/A��A��;A���A�ƨA˴9A˙�AˍPA�ffA� �Aʴ9A�;dA��TA� �A�Q�A��TA�ƨA�S�Aš�A�?}AÁA�bA�l�A��PA�{A��yA�z�A�A��jA�ffA��^A���A���A�jA��A�=qA�-A��A��A��\A�O�A���A�ȴA�E�A�K�A�%A���A�JA���A���A�~�A�`BA�A�33A���A�&�A�  A��A� �A���A�oA�K�A�ZA��7A��A�;dA���A�dZA���A���A��A�x�A�9XA�O�A��A�  A��^A��A��/A�JA��TA�M�A��A�{A���A���A��/A�p�A��9A�t�A��yA��A���A��uA�ZA{G�Ay��Au��As�
As
=Aq�Am�#AlI�Ah��Ae\)Ac��Ab�HAb1'A_dZA]�
A]"�A[7LAY�7AU��ATA�AQ�AOS�AN1AM�7AMoAK�AI��AG�^AEK�AC"�AB5?AAƨA?��A>�\A=��A;�mA:A7�A6�A6JA4z�A2��A1��A/�A.I�A-p�A,�/A,~�A*�\A(ĜA(~�A(A�A'O�A&��A&��A%p�A#�#A#7LA#A"z�A!+A�
A=qA��A�TA��AdZA�yAjA�A�7A�A��A�A��AVA��A��A�DAK�A�9A�TAbNA�A5?A�
A��A�At�AS�A
=A
��A
��A
Q�A	��A	�^A	+Az�A�TA?}AO�AjA��AA�A�^A��A�
A�hA��A\)A&�A ȴA Z@��@�%@�dZ@�J@��^@��7@��9@��@���@�z�@��
@�t�@��@���@�(�@�;d@�n�@웦@�K�@�-@��@�D@�w@��#@� �@㕁@�!@�-@��@��@�@��@�  @޸R@�7L@�z�@�9X@��m@��@��@׮@���@��@�/@�j@���@�l�@��@�j@Ͼw@Χ�@���@�/@̴9@�1'@���@�t�@�+@�
=@�+@�K�@�@�Ĝ@Ǯ@�l�@�\)@�+@��@�hs@ļj@�dZ@�M�@�x�@��j@�33@���@�E�@��^@�`B@��@�r�@�t�@�v�@���@���@��u@�Q�@��F@�l�@���@���@�v�@�=q@��@���@���@�9X@���@��P@���@���@�7L@�r�@�9X@�9X@��m@��@�@��-@���@�E�@���@�`B@�&�@��`@��u@� �@�"�@�@�^5@���@�x�@�`B@�G�@�V@���@���@��@�ƨ@�t�@�o@�@�
=@�o@�@�^5@��-@�p�@�X@���@��u@�Q�@�b@���@��@�n�@�V@�V@�E�@�J@�@��h@�G�@���@��u@��@���@���@��@�l�@�+@�@��@�E�@�@�@�hs@�O�@�7L@�%@��D@�A�@�b@�  @��w@��@��@��@�C�@��y@���@�ff@�E�@�-@��@�@�`B@�%@��@��9@�z�@�1'@�1@��F@�\)@�@��+@�M�@�5?@�=q@���@���@�O�@���@���@�z�@��w@��@���@���@�dZ@��!@��+@�ff@��@��^@�hs@�G�@��D@��@���@��P@�l�@�"�@��H@��R@��+@�@�@���@���@���@�x�@�`B@���@���@���@�j@�(�@��m@��w@���@�l�@�C�@�+@�
=@�@��@��H@��!@�^5@�J@��@���@�hs@�&�@���@�bN@�9X@�  @��w@��@�"�@��@��+@�-@��@���@��-@��hG�O�@���@�@w��@n5?@d��@\�j@V@O�w@G��@=�h@8b@2��@-`B@)x�@#o@\)@�
@��@�
@  @I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
��B
��B
ŢB
�jB
�RB
�?B
�9B
�?B
�FB
�?B
�FB
�XB
�}B
ɺB
�TB+B'�B?}BffB�{B�BƨB��B�B��BuB�B��B+BL�BC�B<jB6FB,BoBB�B%�B6FB<jBD�B<jB:^BJ�BF�B0!B'�B'�B"�B�B�B�B$�B.B49B=qB=qB/BaHBaHB[#BP�BC�B<jB6FB/B#�BPBB��B�B�B�mB�5B��BB�^B�9B��B�DBn�BXB.BPB
�B
�B
�/B
�dB
��B
�{B
�1B
y�B
o�B
`BB
K�B
B�B
?}B
=qB
9XB
&�B	��B	�B	�B	��B	��B	�-B	�bB	y�B	T�B	?}B	6FB	2-B	.B	!�B	�B	oB	DB	B��B�B�HB�
B��B�B�#B�B��B��BǮBÖB��B�wB�dB�RB�FB�9B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�VB�PB�VB�oB�\B�JB�\B�VB�VB�bB�bB�hB�hB�uB��B��B��B��B��B��B�B�B�!B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�!B�!B�'B�'B�!B�!B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B�B�!B�!B�'B�?B�XB�XB�dB�wB�wBB��B��B��B��B�B��B�)B�ZB�`B�mB�mB�sB�yB�B�B�B�B�B�B��B��B�B�B��B��B	B	B	%B	1B	JB	VB	\B	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	+B	-B	/B	1'B	2-B	49B	8RB	:^B	<jB	?}B	@�B	C�B	E�B	I�B	N�B	P�B	R�B	R�B	S�B	W
B	ZB	[#B	_;B	aHB	aHB	cTB	e`B	hsB	hsB	jB	k�B	l�B	l�B	l�B	n�B	o�B	p�B	r�B	t�B	v�B	x�B	z�B	z�B	z�B	z�B	}�B	� B	�B	�B	�B	�%B	�+B	�=B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�9B	�?B	�?B	�LB	�dB	�dB	�dB	�jB	�wB	�}B	��B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�BB	�HB	�HB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
JB
{B
�B
%�B
(�B
0!B
6FB
<jB
D�B
I�B
N�B
S�B
XB
_;B
bNB
ffB
k�B
l�B
p�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B
�B
�	B
��B
��B
ʹB
ŕB
�_B
�IB
�2B
�1B
�7B
�?B
�2B
�?B
�OB
�rB
ɰB
�IB!B'�B?pBfYB�oB��BƙB̿B��B��BeB�B��B*�BL�BC�B<YB69B+�BaBB�B%�B6;B<]BD�B<ZB:RBJ�BF�B0B'�B'�B"�B�B�BuB$�B.B4,B=bB=cB/Ba8Ba7B[BP�BC�B<^B6:B/B#�BABB��B�B�{B�^B�*B��BB�QB�,B��B�2Bn�BXB.BBB
�B
�oB
�#B
�ZB
��B
�rB
�#B
y�B
o�B
`9B
K�B
B�B
?rB
=jB
9PB
&�B	��B	�xB	��B	ʻB	��B	�&B	�[B	y�B	T�B	?}B	6DB	2+B	.B	!�B	�B	nB	DB	B��B�B�KB�B��B�B�$B�B��B��BǰB×B��B�yB�fB�TB�JB�=B�'B�&B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�vB�_B�XB�SB�ZB�sB�_B�MB�]B�XB�VB�eB�fB�jB�lB�vB��B��B��B��B��B��B�B�B�!B�B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�B�B��B��B��B��B��B��B�B�B�#B�B�(B�%B�B� B�B�B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B�B� B�B�$B�?B�VB�VB�bB�vB�vBB��B��B��B��B� B��B�&B�WB�]B�iB�iB�nB�vB�B�B�B�B�B�B��B��B�B�B��B��B	B	B	B	,B	DB	OB	VB	hB	wB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	*�B	-B	/B	1B	2$B	41B	8HB	:UB	<`B	?uB	@zB	C�B	E�B	I�B	N�B	P�B	R�B	R�B	S�B	V�B	ZB	[B	_3B	a=B	a@B	cIB	eTB	hmB	hmB	jvB	k{B	l�B	l�B	l�B	n�B	o�B	p�B	r�B	t�B	v�B	x�B	z�B	z�B	z�B	z�B	}�B	�B	��B	��B	�B	�B	�B	�1B	�JB	�kB	�uB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�'B	�&B	�.B	�/B	�3B	�?B	�ZB	�YB	�ZB	�]B	�jB	�qB	�{B	B	ĎB	ŖB	ŕB	ƜB	ǡB	ȦB	ȧB	˸B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�"B	�"B	�5B	�;B	�;B	�JB	�LB	�RB	�RB	�SB	�ZB	�XB	�hB	�lB	�pB	�qB	�oB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�G�O�B
B
>B
kB
�B
%�B
(�B
0B
65B
<ZB
D�B
I�B
N�B
S�B
X B
_*B
b?B
fVB
ktB
l{B
p�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160601091518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160601091518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160601091518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                