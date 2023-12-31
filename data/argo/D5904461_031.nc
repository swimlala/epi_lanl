CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:54Z AOML 3.0 creation; 2016-08-07T21:36:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221354  20160807143632  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_031                   2C  D   APEX                            6531                            072314                          846 @�9�ĥ\1   @�9�Q�?�@2��G�{�c��/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D�	�D�9�D��3D���D�3D�6fD�p D�I�D�	�D�L�D�s3D�� D��3D�I�DږfD�ɚD�  D�L�D�y�D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B܅B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�CB�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�!HD 
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
=DE��DF
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
=Dk��Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt�=Dt�pDy�=D��D�>�D��RD���D�RD�;�D�uD�N�D��D�Q�D�xRD��D��RD�N�Dڛ�D�ιD�D�Q�D�~�D�ι111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA��A��A� �A�&�A�&�A�(�A�(�A�-A�(�A�(�A�+A�&�A�(�A�+A�+A�/A�7LA�7LA�7LA�;dA�=qA�A�A�=qA�9XA�C�A�G�A�I�A�I�A�I�A�E�A�C�A�E�A�O�A�ZA�`BA�ZA�M�A�G�A�A�A�;dA�C�A�I�A�VA�dZA��A�v�A��A�dZA�VA�&�Aǲ-A�ĜA�l�A�ffAŏ\A�K�A§�A�%A�JA��9A���A���A�|�A�;dA��A�ȴA��TA��-A�dZA�G�A���A�/A�p�A�%A�$�A�5?A��+A�7LA�ĜA�l�A�ȴA��uA���A�x�A��HA��hA�A�VA��^A�hsA�5?A�A��-A���A��/A�t�A�?}A���A��A��A�XA���A�5?A�=qA��9A�A�A���A���A��;A� �A��9A��A� �A%AzVAvE�AsXApȴAm`BAhZAfbAc��Aa�A_�#A]ƨA\�AZ�9AY�AW�AVz�AU�AR�!AM/AG�ADAAG�A@$�A>��A=/A9C�A4�\A2-A0��A/��A-C�A+x�A*��A)7LA'��A&n�A%�7A$^5A#�FA"�jA"^5A!��A!XA!/A r�A�uAJAx�Av�AXAQ�A�PAȴA�A�!AE�A�;A�HAZA�wAdZA�A5?A�A��A�mA�A�;A�jA��AJA
ffA
$�A	��A	O�A�-A^5AȴA  A�!A ��@�n�@�=q@�$�@�5?@���@��@�j@�ff@��@�-@��^@���@�bN@�  @��@�E�@���@�o@�V@�A�@��@��@�@�-@��@��@ߕ�@�l�@ޗ�@�7L@���@ۍP@�|�@�dZ@�K�@�o@ڸR@�ff@�hs@��@�1'@�S�@֧�@�E�@�{@Ցh@�Ĝ@�  @�|�@��@�~�@�7L@�r�@Χ�@��T@�@���@́@��@���@̓u@��@��`@̋D@�1'@�  @˝�@�+@��H@���@��@ə�@�7L@�bN@ǥ�@���@�%@���@���@���@ǍP@Ƈ+@���@�A�@�/@�X@Ɂ@ɡ�@�{@�^5@ʇ+@�v�@ʗ�@ʏ\@�E�@��@�@�x�@�7L@���@��@ǅ@���@Ƈ+@�{@��#@Ų-@���@�J@ŉ7@�7L@ċD@Ý�@�o@�M�@��^@��h@�hs@�&�@��@��u@�9X@� �@��@�\)@�C�@�
=@���@�E�@�@�p�@��@��j@��@�j@�1'@�dZ@�E�@�@�-@��T@���@�r�@��m@���@��@�\)@��H@�~�@��7@���@�Ĝ@���@�Q�@��@�ƨ@�33@��+@�5?@�M�@�=q@�7L@�b@���@��P@�dZ@�;d@���@��H@���@�ff@��#@��^@���@��@�`B@�/@��/@�bN@���@�l�@�+@���@��\@�V@�E�@�$�@��T@���@��-@��@�&�@�%@���@��9@�r�@�Z@�(�@� �@�1@���@�33@��@��@�+@�"�@�
=@��R@�n�@��@���@���@�`B@�7L@�&�@��@�%@���@�Q�@�1@��@��@�|�@�33@��y@�^5@��@�{@�@�X@�7L@��`@��@�r�@�bN@�(�@��F@�t�@��@��!@���@�M�@�V@�V@�5?@���@���@�Ĝ@�I�@�1@�1@�1@��@���@���@��@��T@���@�V@��/@���@�z�@�(�@��@� �@��@��
@��P@�@���@�~�@�v�@��+@�^5@�V@�&�@�(�@���@�"�@��@���@���@�~�@��\@��@�|�@�S�@�ȴ@�v�@�5?@��#@�x�@�/@��+@��@�&�@��\@xQ�@m��@e/@^�+@U@MO�@EO�@?
=@7��@1�7@+o@&{@"�@E�@�^@�@|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�VA��A��A� �A�&�A�&�A�(�A�(�A�-A�(�A�(�A�+A�&�A�(�A�+A�+A�/A�7LA�7LA�7LA�;dA�=qA�A�A�=qA�9XA�C�A�G�A�I�A�I�A�I�A�E�A�C�A�E�A�O�A�ZA�`BA�ZA�M�A�G�A�A�A�;dA�C�A�I�A�VA�dZA��A�v�A��A�dZA�VA�&�Aǲ-A�ĜA�l�A�ffAŏ\A�K�A§�A�%A�JA��9A���A���A�|�A�;dA��A�ȴA��TA��-A�dZA�G�A���A�/A�p�A�%A�$�A�5?A��+A�7LA�ĜA�l�A�ȴA��uA���A�x�A��HA��hA�A�VA��^A�hsA�5?A�A��-A���A��/A�t�A�?}A���A��A��A�XA���A�5?A�=qA��9A�A�A���A���A��;A� �A��9A��A� �A%AzVAvE�AsXApȴAm`BAhZAfbAc��Aa�A_�#A]ƨA\�AZ�9AY�AW�AVz�AU�AR�!AM/AG�ADAAG�A@$�A>��A=/A9C�A4�\A2-A0��A/��A-C�A+x�A*��A)7LA'��A&n�A%�7A$^5A#�FA"�jA"^5A!��A!XA!/A r�A�uAJAx�Av�AXAQ�A�PAȴA�A�!AE�A�;A�HAZA�wAdZA�A5?A�A��A�mA�A�;A�jA��AJA
ffA
$�A	��A	O�A�-A^5AȴA  A�!A ��@�n�@�=q@�$�@�5?@���@��@�j@�ff@��@�-@��^@���@�bN@�  @��@�E�@���@�o@�V@�A�@��@��@�@�-@��@��@ߕ�@�l�@ޗ�@�7L@���@ۍP@�|�@�dZ@�K�@�o@ڸR@�ff@�hs@��@�1'@�S�@֧�@�E�@�{@Ցh@�Ĝ@�  @�|�@��@�~�@�7L@�r�@Χ�@��T@�@���@́@��@���@̓u@��@��`@̋D@�1'@�  @˝�@�+@��H@���@��@ə�@�7L@�bN@ǥ�@���@�%@���@���@���@ǍP@Ƈ+@���@�A�@�/@�X@Ɂ@ɡ�@�{@�^5@ʇ+@�v�@ʗ�@ʏ\@�E�@��@�@�x�@�7L@���@��@ǅ@���@Ƈ+@�{@��#@Ų-@���@�J@ŉ7@�7L@ċD@Ý�@�o@�M�@��^@��h@�hs@�&�@��@��u@�9X@� �@��@�\)@�C�@�
=@���@�E�@�@�p�@��@��j@��@�j@�1'@�dZ@�E�@�@�-@��T@���@�r�@��m@���@��@�\)@��H@�~�@��7@���@�Ĝ@���@�Q�@��@�ƨ@�33@��+@�5?@�M�@�=q@�7L@�b@���@��P@�dZ@�;d@���@��H@���@�ff@��#@��^@���@��@�`B@�/@��/@�bN@���@�l�@�+@���@��\@�V@�E�@�$�@��T@���@��-@��@�&�@�%@���@��9@�r�@�Z@�(�@� �@�1@���@�33@��@��@�+@�"�@�
=@��R@�n�@��@���@���@�`B@�7L@�&�@��@�%@���@�Q�@�1@��@��@�|�@�33@��y@�^5@��@�{@�@�X@�7L@��`@��@�r�@�bN@�(�@��F@�t�@��@��!@���@�M�@�V@�V@�5?@���@���@�Ĝ@�I�@�1@�1@�1@��@���@���@��@��T@���@�V@��/@���@�z�@�(�@��@� �@��@��
@��P@�@���@�~�@�v�@��+@�^5@�V@�&�@�(�@���@�"�@��@���@���@�~�@��\@��@�|�@�S�@�ȴ@�v�@�5?@��#@�x�G�O�@��+@��@�&�@��\@xQ�@m��@e/@^�+@U@MO�@EO�@?
=@7��@1�7@+o@&{@"�@E�@�^@�@|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ȴB
ǮB
ǮB
ǮB
ǮB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ǮB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ɺB
ȴB
ȴB
��B
��B
��B
��B
��B
�B
�#B
�/B
�fB
�B
�B
��B
��B
��B
��BbB"�B33BW
By�B�RB��B�/B�5B�`B��BJB"�B7LBH�BG�B6FB;dBM�BT�BS�B[#Bo�B|�Bz�Bu�Bn�Bn�Bn�Bs�B�+B�JB�VB�JB~�By�Bu�B~�B�B� B|�Bk�BP�BG�BC�B>wB9XB49B/B�B�BoBVB��B�`B�B��B�dB��B�uB�BiyBXB?}B.B!�BPB
�B
�/B
��B
�B
jB
I�B
%�B
1B	�mB	ȴB	�FB	��B	�DB	r�B	cTB	W
B	G�B	>wB	6FB	2-B	1'B	.B	+B	'�B	#�B	�B	PB��B��B�B�B�B�fB�fB�B��BǮBB�XB�qB�qB�qB�}B�wB��BƨBɺB��B��B��B��B��B��B��B��B��B��B��B��BɺBĜBƨBƨBɺBɺBȴB��B��B��B��B�B�#B�5B�BB�/B�B�B�B��B�B�BB�ZB�NB�)B�
B��B��B��BȴBÖBBBB��B��B�jB�3B�!B�!B�!B�B�B�B�!B�?B�LB�XB�wB��BŢB��B��B��B��B��B��B��B�B�B�)B�5B�;B�;B�BB�HB�NB�TB�mB�sB�B�B�B��B��B��B��B��B	B		7B	
=B	\B	oB	uB	�B	�B	�B	!�B	$�B	&�B	-B	5?B	8RB	:^B	<jB	<jB	=qB	?}B	B�B	G�B	J�B	J�B	J�B	K�B	P�B	R�B	\)B	]/B	`BB	cTB	dZB	ffB	l�B	v�B	� B	�B	�B	�%B	�7B	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�'B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�XB	�jB	��B	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�}B	�}B	B	ĜB	B	��B	��B	B	B	ÖB	ƨB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�HB	�HB	�NB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
1B
	7B
1B
1B
1B
1B
+B
+B
%B
DB
hB
�B
 �B
&�B
.B
49B
8RB
?}B
B�B
I�B
P�B
XB
[#B
`BB
bNB
ffB
k�B
n�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
ȫB
ǥB
ǥB
ǣB
ǥB
ƞB
ƝB
ƞB
ǢB
ƟB
ƟB
ǢB
ƟB
ƟB
ƟB
ƟB
ǢB
ǤB
ǤB
ǢB
ȨB
ȩB
ɮB
ȩB
ȨB
˼B
��B
��B
��B
��B
�B
�B
�&B
�]B
�B
�B
��B
��B
��B
��BXB"�B3&BV�By�B�EB��B� B�'B�SB��B:B"�B7>BH�BG�B6:B;YBM�BT�BS�B[Bo�B|�Bz�Bu�Bn�Bn�Bn�Bs�B� B�=B�IB�=B~�By�Bu�B~�B��B�B|�BkvBP�BG�BC�B>gB9GB4*B/B�ByB`BDB��B�RB��B��B�UB��B�eB�BiiBXB?qB.B!�BBB
�B
�"B
��B
��B
jtB
I�B
%�B
'B	�dB	ȯB	�?B	��B	�?B	r�B	cQB	WB	G�B	>uB	6CB	2+B	1%B	.B	+B	'�B	#�B	�B	PB��B��B�B�B�B�fB�eB�B��BǯBB�WB�pB�rB�oB�~B�wB��BƧBɺB��B��B��B��B��B��B��B��B��B��B��B��BɼBĞBƧBƨBɺBɺBȲB��B��B��B��B�B�"B�4B�>B�.B�B�B�B��B�B�>B�ZB�MB�'B�	B��B��B��BȲBÔBBBB��B�B�iB�1B�!B�B�"B�B�B�B� B�=B�JB�WB�vB��BšB��B��B��B��B��B��B��B� B�B�&B�4B�9B�7B�?B�DB�JB�RB�jB�pB�B�B�B��B��B��B��B��B	
B		2B	
8B	WB	hB	mB	�B	�B	�B	!�B	$�B	&�B	-B	5:B	8HB	:WB	<bB	<bB	=jB	?uB	B�B	G�B	J�B	J�B	J�B	K�B	P�B	R�B	\!B	]&B	`9B	cKB	dSB	f\B	l�B	v�B	�B	�	B	�B	�B	�,B	�SB	�^B	�gB	�pB	�B	�tB	�vB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�'B	�*B	�4B	�2B	�;B	�KB	�[B	�wB	�yB	�qB	�oB	�yB	�|B	�}B	�}B	�xB	�xB	�xB	�yB	�yB	�yB	�uB	�wB	�vB	�pB	�qB	�qB	B	đB	B	�vB	�}B	B	B	ÊB	ƞB	ɯB	ɰB	ɯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�!B	�'B	�'B	�+B	�+B	�-B	�8B	�9B	�AB	�SB	�WB	�YB	�\B	�aB	�dB	�eB	�hB	�gB	�`B	�`B	�`B	�^B	�bB	�`B	�eB	�rB	�xB	�wB	�zB	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
"B
	)B
$B
#B
$B
$B
G�O�B
B
8B
XB
qB
 �B
&�B
.B
4)B
8AB
?lB
B�B
I�B
P�B
XB
[B
`4B
b>B
fVB
ktB
n�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436322016080714363220160807143632  AO  ARCAADJP                                                                    20150226221354    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221354  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221354  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143632  IP                  G�O�G�O�G�O�                