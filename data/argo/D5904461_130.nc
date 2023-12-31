CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-18T18:01:29Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160718180129  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286_8897_130                   2C  D   APEX                            6531                            072314                          846 @׼m�.[�1   @׼n\�;@62���m�cC��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDty�Dyy�D��3D�` D��fD���D�3D�I�D�� D�� D� D�C3D�p D��fD�fD�9�D�y�D�� D�	�D�I�D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~B�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=D�=D�D�=D
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
=D,��D-
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
=Ds�=Dt�Dt��Dy��D��RD�eD���D���D�RD�N�D��D��D�D�HRD�uD�˅D��D�>�D�~�D��D��D�N�D�xRD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�  A�A�A�A���A���A�  A���A���A���A���A���A���A��A��A��/A���A͸RAͬA͙�A�ZA���A�  A�/AʸRA�ĜA�5?A�jAĬA�(�AÙ�A�G�A�Q�A�^5A�S�A7A�(�A�1'A�A´9A��A��A�(�A��RA���A�M�A�$�A��A���A�p�A�;dA���A��DA�z�A���A��mA��
A��A�ȴA���A��`A��^A�Q�A�"�A�~�A�-A�A�A� �A��
A�{A���A���A��9A��A�S�A��-A�?}A� �A��mA���A�ĜA���A�"�A���A��
A���A�A��A�^5A�S�A���A���A���A���A��wA�t�A���A�%A���A���A�(�A�t�A�t�A�ffA��A}�hA|��A|1'A{��A{oAy�Aw��Ar(�Ap$�An�9Ak�-AjJAg�Ae�hAe7LAd�jAb5?Aa��A`{A]��A[�7AZ��AY/AV��ATQ�AR5?AP�AN�+AK��AI33AFz�ABffAAS�A@  A>�A=G�A<9XA:��A9p�A7x�A6�/A5;dA3?}A2jA1�A0=qA/�A-�#A,M�A+p�A*��A)+A'�TA&�RA%�^A$��A$��A$r�A#�FA"bA!�AA�A��Ax�A��AAA�AS�A�A��A�hA��A��A�+A{A�
A?}A��Ar�A�AjAXA��A�PA|�Ap�A+A
�/A
Q�A	�FA	��A�\A7LAA��Av�AI�A(�A�#A
=A�A�A��A�;Ap�AO�A Q�@���@�S�@��y@�V@��-@���@���@���@�Ĝ@�K�@�ff@��@�+@�-@�@�@�"�@�5?@�@�b@�ȴ@�7@��@�bN@��;@�@ݑh@�
=@ڗ�@�b@ۥ�@۶F@�  @��@׍P@��@׶F@ٲ-@��#@١�@�ȴ@�M�@��/@�@ԃ@���@֏\@թ�@�bN@ӥ�@�dZ@�x�@�K�@�X@͑h@�Q�@�=q@ǝ�@�l�@�&�@ț�@ȼj@�X@��/@�1@�dZ@��@�;d@���@���@�X@�Ĝ@�r�@�z�@�+@�G�@���@���@��+@��@�@���@��@�+@�@�C�@��D@��@���@���@�E�@�b@��\@�O�@��@�z�@�Q�@��@��y@�M�@��\@�l�@�(�@�1'@���@�\)@�ff@�{@���@���@��@�+@���@���@��\@�^5@�V@�E�@�-@��@�`B@�7L@��@��@�A�@��@���@�|�@�t�@�\)@�+@��@���@��!@���@��\@��\@�v�@�J@�@���@�x�@��^@�`B@���@�Z@�z�@�z�@�I�@�(�@�l�@�K�@�ȴ@���@���@��@�Ĝ@��@���@�@���@�;d@�1'@�1'@���@��P@���@�ff@��#@���@�%@�I�@��@��m@��F@�C�@�+@�
=@���@���@���@��+@�~�@�V@��@��-@��@��h@�p�@���@�bN@���@��;@��F@�\)@�"�@��@�ȴ@�ȴ@���@��\@�~�@�-@�G�@�7L@��@�`B@��@�V@�%@���@�j@�1@��m@��F@���@�S�@�
=@��y@���@�5?@�J@��#@��-@���@�x�@�?}@��@��@�9X@��;@�ƨ@��F@��@���@�|�@��@���@��@���@���@�$�@��@���@�7L@���@���@�1'@��@��w@�C�@�"�@���@�v�@�n�@�V@��-@�&�@��@�Q�@� �@�  @��@�ƨ@��@�S�@��H@��\@�v�@�V@�$�@�j@���@�@{@qx�@g|�@`  @Z�@R�!@L�/@I�7@Bn�@<I�@5/@0 �@)��@$�@@�7@�@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A�A�A�A�  A�A�A�A���A���A�  A���A���A���A���A���A���A��A��A��/A���A͸RAͬA͙�A�ZA���A�  A�/AʸRA�ĜA�5?A�jAĬA�(�AÙ�A�G�A�Q�A�^5A�S�A7A�(�A�1'A�A´9A��A��A�(�A��RA���A�M�A�$�A��A���A�p�A�;dA���A��DA�z�A���A��mA��
A��A�ȴA���A��`A��^A�Q�A�"�A�~�A�-A�A�A� �A��
A�{A���A���A��9A��A�S�A��-A�?}A� �A��mA���A�ĜA���A�"�A���A��
A���A�A��A�^5A�S�A���A���A���A���A��wA�t�A���A�%A���A���A�(�A�t�A�t�A�ffA��A}�hA|��A|1'A{��A{oAy�Aw��Ar(�Ap$�An�9Ak�-AjJAg�Ae�hAe7LAd�jAb5?Aa��A`{A]��A[�7AZ��AY/AV��ATQ�AR5?AP�AN�+AK��AI33AFz�ABffAAS�A@  A>�A=G�A<9XA:��A9p�A7x�A6�/A5;dA3?}A2jA1�A0=qA/�A-�#A,M�A+p�A*��A)+A'�TA&�RA%�^A$��A$��A$r�A#�FA"bA!�AA�A��Ax�A��AAA�AS�A�A��A�hA��A��A�+A{A�
A?}A��Ar�A�AjAXA��A�PA|�Ap�A+A
�/A
Q�A	�FA	��A�\A7LAA��Av�AI�A(�A�#A
=A�A�A��A�;Ap�AO�A Q�@���@�S�@��y@�V@��-@���@���@���@�Ĝ@�K�@�ff@��@�+@�-@�@�@�"�@�5?@�@�b@�ȴ@�7@��@�bN@��;@�@ݑh@�
=@ڗ�@�b@ۥ�@۶F@�  @��@׍P@��@׶F@ٲ-@��#@١�@�ȴ@�M�@��/@�@ԃ@���@֏\@թ�@�bN@ӥ�@�dZ@�x�@�K�@�X@͑h@�Q�@�=q@ǝ�@�l�@�&�@ț�@ȼj@�X@��/@�1@�dZ@��@�;d@���@���@�X@�Ĝ@�r�@�z�@�+@�G�@���@���@��+@��@�@���@��@�+@�@�C�@��D@��@���@���@�E�@�b@��\@�O�@��@�z�@�Q�@��@��y@�M�@��\@�l�@�(�@�1'@���@�\)@�ff@�{@���@���@��@�+@���@���@��\@�^5@�V@�E�@�-@��@�`B@�7L@��@��@�A�@��@���@�|�@�t�@�\)@�+@��@���@��!@���@��\@��\@�v�@�J@�@���@�x�@��^@�`B@���@�Z@�z�@�z�@�I�@�(�@�l�@�K�@�ȴ@���@���@��@�Ĝ@��@���@�@���@�;d@�1'@�1'@���@��P@���@�ff@��#@���@�%@�I�@��@��m@��F@�C�@�+@�
=@���@���@���@��+@�~�@�V@��@��-@��@��h@�p�@���@�bN@���@��;@��F@�\)@�"�@��@�ȴ@�ȴ@���@��\@�~�@�-@�G�@�7L@��@�`B@��@�V@�%@���@�j@�1@��m@��F@���@�S�@�
=@��y@���@�5?@�J@��#@��-@���@�x�@�?}@��@��@�9X@��;@�ƨ@��F@��@���@�|�@��@���@��@���@���@�$�@��@���@�7L@���@���@�1'@��@��w@�C�@�"�@���@�v�@�n�@�V@��-@�&�@��@�Q�@� �@�  @��@�ƨ@��@�S�@��H@��\@�v�@�VG�O�@�j@���@�@{@qx�@g|�@`  @Z�@R�!@L�/@I�7@Bn�@<I�@5/@0 �@)��@$�@@�7@�@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
A�B
@�B
>wB
;dB
8RB
8RB
<jB
=qB
m�B
��B
�uB
��B
�3B
��B
��B
�
B
�/B
�NB
�HB
��B�BVBp�B�BW
B\)BcTBo�Bz�B�bB�oB��B�B�RB�jB��B��B�ZB�B33B��B1B,Bl�Bo�Bz�B��B�FB�XB�LB��B��B�1B~�B~�B��B�!B�B��B`BB%B��BI�B%�B\B
�B
�wB
��B
� B
p�B
�uB
��B
��B
�RB
�B
��B
�B
q�B
\)B
Q�B
F�B
Q�B
T�B
F�B
.B
�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	u�B	W
B	I�B	5?B	)�B	 �B	�B	�B	)�B	$�B	 �B	�B	hB	\B	uB	bB	%B��B�B�ZB�#B��B��B�?B�B�B��B��B��B��B��B��B��B�hB�VB�+B�B}�B|�B}�B~�Bw�Bv�B}�B~�B{�B{�B|�B{�Bz�Bx�Bw�B{�B~�By�Bv�Bv�Bu�Bo�Bo�Bo�Bn�Bp�Bo�Bn�Bk�BjBq�Bs�Bu�Bu�Bw�Bx�Bo�Bn�BjBffBbNBk�Bm�Bl�Bk�Bl�Bl�Bq�Bn�Bk�Bo�Bo�Bq�Bt�Bv�Bw�Bx�B{�B{�Bz�B~�B� B�B�B�B�B�B�B�B�B�B�=B�=B�DB�PB�JB�VB�bB�VB�JB�DB�7B�1B�+B�+B�7B�DB�VB�\B�7B|�Bt�Bu�B�+B�7B�PB�oB�DB�B�B�PB��B��B��B�'B�3B�B��B��B��B�-B�FB�9B�3B�'B�B�B�B�'B�'B�B��B�-B��BÖBƨB��B��B��B��B��B��B��B��B��B��B��B�B�/B�#B�
B�B�B�
B�5B�BB�NB�NB�TB�fB��B��B��B	+B		7B	B	B	B	%B	+B	1B	PB	PB	hB	�B	#�B	)�B	-B	-B	-B	,B	-B	2-B	=qB	D�B	G�B	I�B	K�B	Q�B	S�B	T�B	W
B	]/B	_;B	bNB	gmB	iyB	jB	k�B	m�B	n�B	n�B	o�B	p�B	r�B	u�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�1B	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�9B	�FB	�LB	�RB	�^B	�^B	�dB	�qB	�jB	�wB	�}B	��B	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�yB	�B	�B	�B	�B	�yB	�yB	�yB	�sB	�yB	�yB	�yB	�B	�B	�B	��B
	7B
hB
�B
"�B
'�B
-B
33B
:^B
=qB
B�B
H�B
N�B
P�B
S�B
YB
_;B
dZB
gmB
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
A�B
@~B
>qB
;^B
8MB
8NB
<dB
=jB
m�B
�xB
�lB
��B
�*B
��B
��B
�B
�#B
�BB
�>B
��B�BU�Bp�B�BV�B\BcJBo�Bz�B�XB�bB��B��B�CB�\B�|B˺B�JB�B3 B��B!B+�BlBo�Bz�B��B�8B�LB�>B��B�yB�"B~�B~�B��B�B�B��B`4BB�wBI�B%�BKB
�B
�lB
��B
�B
p�B
�kB
��B
��B
�EB
��B
��B
��B
q�B
\B
Q�B
F�B
Q�B
T�B
F�B
.	B
�B	�B	�{B	�B	��B	��B	��B	��B	��B	��B	u�B	WB	I�B	5=B	)�B	 �B	�B	�B	)�B	$�B	 �B	�B	hB	\B	tB	aB	$B��B�B�]B�!B��B��B�BB�B�B��B��B��B��B��B��B��B�mB�YB�1B�B}�B|�B}�B Bw�Bv�B}�B~�B{�B{�B|�B{�Bz�Bx�Bw�B{�B~�By�Bv�Bv�Bu�Bo�Bo�Bo�Bn�Bp�Bo�Bn�Bk�Bj�Bq�Bs�Bu�Bu�Bw�Bx�Bo�Bn�Bj�BfjBbRBk�Bm�Bl�Bk�Bl�Bl�Bq�Bn�Bk�Bo�Bo�Bq�Bt�Bv�Bw�Bx�B{�B{�Bz�B~�B�B�B�!B� B�B�B�
B�B�	B�	B�@B�<B�EB�RB�KB�VB�cB�UB�LB�GB�8B�2B�.B�-B�:B�GB�WB�[B�:B|�Bt�Bu�B�,B�7B�RB�nB�DB�B�B�PB��B��B��B�&B�1B�B��B��B��B�,B�EB�7B�/B�%B�B�B�B�'B�&B� B��B�+B��BÓBƥB��B��B��B��B��B��B��B��B��B��B��B�B�+B�B�B�B��B�B�1B�?B�HB�JB�PB�aB��B��B��B	&B		1B	B	B	
B	B	%B	,B	JB	JB	eB	�B	#�B	)�B	-
B	-B	-B	+�B	-B	2%B	=jB	D�B	G�B	I�B	K�B	Q�B	S�B	T�B	WB	]%B	_3B	bDB	geB	ioB	jxB	k{B	m�B	n�B	n�B	o�B	p�B	r�B	u�B	y�B	|�B	�B	�B	�B	�B	�B	�B	�B	�"B	�)B	�-B	�%B	�:B	�dB	�uB	�|B	�wB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�#B	�#B	�&B	�,B	�+B	�9B	�@B	�GB	�RB	�SB	�YB	�eB	�]B	�jB	�nB	�uB	ŗB	ǡB	ǡB	ȥB	ȧB	ȩB	ɯB	ɯB	ȧB	ɮB	˺B	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�.B	�-B	�-B	�7B	�4B	�:B	�;B	�AB	�?B	�:B	�?B	�@B	�=B	�BB	�?B	�FB	�FB	�HB	�DB	�lB	�sB	�rB	�oB	�oB	�kB	�jB	�kB	�eB	�lB	�kB	�mB	�sG�O�B	�B	��B
	(B
ZB
�B
"�B
'�B
,�B
3%B
:LB
=_B
BB
H�B
N�B
P�B
S�B
YB
_+B
dJB
g\B
l{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160718180129    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160718180129  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160718180129  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                