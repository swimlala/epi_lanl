CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-06T09:16:12Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150406091612  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               )A   AO  5286_8897_041                   2C  D   APEX                            6531                            072314                          846 @�G���1   @�GO���@2�bM���cɁ$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    )A   B   B   @�ff@�  A��A   A@  A`  A�  A���A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt` Dy� D� D�FfD�y�D�� D�fD�<�D�s3D��fD� D�<�D���D�ɚD���D�6fD�p D��D�fD�I�D�s3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A(�A"�\AB�\Ab�\A�G�A�{A�G�A�{A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BI
=BQ
=BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`B�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=DM�=DN�DN�=DO
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
=Ds��Dt
=Dtj=Dy�=D�D�K�D�~�D��D��D�A�D�xRD�˅D�D�A�D���D�ιD���D�;�D�uD���D��D�N�D�xRD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�`BA�dZA�dZA�ffA�jA�jA�l�A�l�A�n�A�n�A�p�A�p�A�p�A�n�A�n�A�hsA�ffA�O�A��Ȧ+A˟�A�O�A�/A�t�A˙�A�p�Aɺ^A�I�A�5?AȺ^A�~�A�(�A��`Aǉ7A�bNA��A��
AƁA��A��HAŗ�A�1AāA� �A��
A��A7A��yA�ȴA��A��A�hsA���A���A��A��wA�\)A���A��!A�"�A��+A�K�A�;dA��A��A�A�&�A�VA�bA��
A�M�A��9A��A�A��A��A��\A��A�ȴA��HA�bNA�7LA���A�x�A�7LA���A�9XA�7LA��!A���A��PA��A�-A�1A��A�XA�%A�A�A��A�%A���A�t�A��
A�|�A�A��A���A}`BAz  Asx�Aj��Ad��Aa|�A_�A^M�A\ȴA\1A[�7AY�wAV��ATE�AQ��AO��AM�^AKO�AHJAG��AF{AD��AA��A?�A=�-A:9XA6$�A5VA2�A21A1��A0��A/;dA.��A-�TA-`BA+x�A*��A)��A(�HA(�A'�TA'�PA&ZA%�-A%XA%"�A#�wA"bNA!t�A!�A �jA r�A (�A�7AVAz�AbA+A��A=qA�A�A�RA1'A+A�A(�A\)AȴA��AoA��A(�AƨA�wAG�AȴAbA�-A;dA~�A��A/AVAdZA�A�`A��AffAA�A��A
�A
�9A
n�A
$�A	��A	C�A�!AM�A��A�PAl�A`BAA�9A��A��A�uAn�A�A�FAoA�hA
=A��A^5AA+A �yA �`A ��A b@�E�@�&�@��`@���@���@�M�@��@�@��9@��@�ȴ@�^5@�@�"�@�+@���@�hs@�r�@�\)@�$�@�A�@��/@��`@�Ĝ@�I�@띲@���@��@��H@�J@�h@�7@�Ĝ@�@�bN@�t�@�;d@�"�@�E�@�7@��@��;@�~�@��`@߶F@�;d@߅@�Z@ߥ�@ݙ�@�/@�hs@�Z@�t�@��H@�=q@ٲ-@�O�@ج@�9X@ׅ@�K�@ָR@�M�@�{@�&�@�Ĝ@�j@��m@�S�@҇+@��T@�?}@�1@�"�@�~�@�@���@́@�X@�%@�9X@�K�@�o@��@�;d@�|�@�t�@�S�@��@ʟ�@��@��@Ǖ�@öF@�-@�A�@���@��@�\)@�C�@�33@���@��@�+@��m@��@���@�~�@��@�@��7@�`B@��9@�A�@�(�@� �@���@�
=@�~�@���@���@��@��@�O�@�x�@��@�G�@�/@�%@��j@�9X@��P@�l�@�;d@���@��y@�ȴ@��R@�M�@��@���@���@��@���@�r�@�1'@�dZ@���@�/@��9@�9X@���@���@�S�@�\)@�dZ@�+@��R@���@�`B@��/@��@�Q�@��@��w@���@�t�@�C�@��+@��@���@�`B@���@�Ĝ@�r�@��@��;@��
@��@�1@�l�@��y@���@���@�p�@�Ĝ@��u@�z�@�bN@�(�@���@��P@�C�@�"�@�
=@��y@���@���@���@��!@���@���@��+@�E�@�5?@�-@��@���@��@�7L@�%@���@�r�@�j@�bN@�I�@�1'@� �@��@�b@���@�t�@�S�@��H@�V@�@���@��@��@���@�Z@�1'@��@��
@��F@���@�
=@��y@��@���@���@��+@�v�@�n�@�M�@�E�@�=q@�V@��\@�ff@�v�@�ff@��-@��@�r�@��;@�X@�ȴ@�S�@|1@s�
@j��@_|�@W;d@N��@Fff@A7L@9�^@41@.ȴ@)hs@$��@ A�@O�@ff@b@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�^5A�`BA�dZA�dZA�ffA�jA�jA�l�A�l�A�n�A�n�A�p�A�p�A�p�A�n�A�n�A�hsA�ffA�O�A��Ȧ+A˟�A�O�A�/A�t�A˙�A�p�Aɺ^A�I�A�5?AȺ^A�~�A�(�A��`Aǉ7A�bNA��A��
AƁA��A��HAŗ�A�1AāA� �A��
A��A7A��yA�ȴA��A��A�hsA���A���A��A��wA�\)A���A��!A�"�A��+A�K�A�;dA��A��A�A�&�A�VA�bA��
A�M�A��9A��A�A��A��A��\A��A�ȴA��HA�bNA�7LA���A�x�A�7LA���A�9XA�7LA��!A���A��PA��A�-A�1A��A�XA�%A�A�A��A�%A���A�t�A��
A�|�A�A��A���A}`BAz  Asx�Aj��Ad��Aa|�A_�A^M�A\ȴA\1A[�7AY�wAV��ATE�AQ��AO��AM�^AKO�AHJAG��AF{AD��AA��A?�A=�-A:9XA6$�A5VA2�A21A1��A0��A/;dA.��A-�TA-`BA+x�A*��A)��A(�HA(�A'�TA'�PA&ZA%�-A%XA%"�A#�wA"bNA!t�A!�A �jA r�A (�A�7AVAz�AbA+A��A=qA�A�A�RA1'A+A�A(�A\)AȴA��AoA��A(�AƨA�wAG�AȴAbA�-A;dA~�A��A/AVAdZA�A�`A��AffAA�A��A
�A
�9A
n�A
$�A	��A	C�A�!AM�A��A�PAl�A`BAA�9A��A��A�uAn�A�A�FAoA�hA
=A��A^5AA+A �yA �`A ��A b@�E�@�&�@��`@���@���@�M�@��@�@��9@��@�ȴ@�^5@�@�"�@�+@���@�hs@�r�@�\)@�$�@�A�@��/@��`@�Ĝ@�I�@띲@���@��@��H@�J@�h@�7@�Ĝ@�@�bN@�t�@�;d@�"�@�E�@�7@��@��;@�~�@��`@߶F@�;d@߅@�Z@ߥ�@ݙ�@�/@�hs@�Z@�t�@��H@�=q@ٲ-@�O�@ج@�9X@ׅ@�K�@ָR@�M�@�{@�&�@�Ĝ@�j@��m@�S�@҇+@��T@�?}@�1@�"�@�~�@�@���@́@�X@�%@�9X@�K�@�o@��@�;d@�|�@�t�@�S�@��@ʟ�@��@��@Ǖ�@öF@�-@�A�@���@��@�\)@�C�@�33@���@��@�+@��m@��@���@�~�@��@�@��7@�`B@��9@�A�@�(�@� �@���@�
=@�~�@���@���@��@��@�O�@�x�@��@�G�@�/@�%@��j@�9X@��P@�l�@�;d@���@��y@�ȴ@��R@�M�@��@���@���@��@���@�r�@�1'@�dZ@���@�/@��9@�9X@���@���@�S�@�\)@�dZ@�+@��R@���@�`B@��/@��@�Q�@��@��w@���@�t�@�C�@��+@��@���@�`B@���@�Ĝ@�r�@��@��;@��
@��@�1@�l�@��y@���@���@�p�@�Ĝ@��u@�z�@�bN@�(�@���@��P@�C�@�"�@�
=@��y@���@���@���@��!@���@���@��+@�E�@�5?@�-@��@���@��@�7L@�%@���@�r�@�j@�bN@�I�@�1'@� �@��@�b@���@�t�@�S�@��H@�V@�@���@��@��@���@�Z@�1'@��@��
@��F@���@�
=@��y@��@���@���@��+@�v�@�n�@�M�@�E�@�=q@�V@��\@�ff@�v�@�ff@��-@��@�r�G�O�@�X@�ȴ@�S�@|1@s�
@j��@_|�@W;d@N��@Fff@A7L@9�^@41@.ȴ@)hs@$��@ A�@O�@ff@b@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�\B	�\B	�\B	�\B	�\B	�bB	�bB	�\B	�\B	�\B	�\B	�bB	�bB	�bB	�oB	�oB	�{B	��B	��B	�!B	�/B
'�B
��B
�9B
�B
��B9XBYB�=B�dBǮB��B�/B�B�B��BBJBhB$�B.B.B-B;dBK�BT�Be`Bm�B~�B�{B��B�B�?B�jBƨB��BB��B��B�?B�RB�fB�B�B�B�yB�yB�ZB�#B��B��BȴBv�BE�B�B�B�/B�^B�\BXBVBbNBO�BJ�BK�BI�BG�B>wB1'B�B�B�BVB
��B
�mB
�5B
ȴB
�B
�=B
o�B
VB
N�B
D�B
=qB
6FB
,B
{B	��B	�NB	ƨB	�=B	I�B	$�B	�B	VB	+B	B��B��B�B�yB�HB�B��BɺBĜB�}B�wB��B�wB�}B�dB�3B�XB�9B�'B�-B�-B�!B�B�B�B�B�B�-B�3B�FB�^B�dB�qB�wBBĜBĜBĜBĜBɺB�
B�
B�B�#B�)B�BB�HB�NB�ZB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	JB	"�B	!�B	�B	�B	�B	 �B	"�B	%�B	)�B	'�B	)�B	+B	,B	-B	.B	.B	/B	33B	49B	5?B	6FB	7LB	:^B	>wB	A�B	D�B	E�B	F�B	F�B	I�B	P�B	R�B	S�B	S�B	S�B	S�B	R�B	O�B	S�B	R�B	R�B	Q�B	Q�B	T�B	XB	XB	YB	XB	]/B	`BB	aHB	cTB	e`B	iyB	iyB	jB	gmB	dZB	hsB	iyB	k�B	o�B	q�B	r�B	s�B	p�B	n�B	l�B	iyB	t�B	y�B	z�B	y�B	z�B	� B	� B	|�B	y�B	y�B	|�B	~�B	~�B	}�B	~�B	~�B	}�B	{�B	z�B	{�B	x�B	v�B	r�B	p�B	q�B	v�B	�B	�%B	�B	�+B	�JB	�JB	�PB	�PB	�VB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�'B	�-B	�3B	�9B	�?B	�dB	�qB	�qB	�wB	�wB	�wB	�qB	�dB	�9B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�3B	�3B	�3B	�-B	�'B	�-B	�9B	�LB	�RB	�XB	�XB	�^B	�dB	�qB	B	ĜB	ƨB	ŢB	ĜB	ĜB	ƨB	ŢB	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�;B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
+B
+B
%B
B
B

=B

=B
oB
�B
�B
"�B
(�B
/B
6FB
;dB
A�B
F�B
M�B
P�B
T�B
ZB
_;B
cTB
ffB
l�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�\B	�]B	�\B	�\B	�[B	�eB	�eB	�ZB	�\B	�\B	�[B	�eB	�bB	�bB	�pB	�nB	�|B	��B	��B	�B	�.B
'�B
��B
�0B
�B
��B9KBY	B�5B�YBǣB��B�"B�B�B��B	B:B]B$�B.B.B-B;VBK�BT�BeNBm�B~�B�oB��B�	B�1B�`BƞB�vBB��B�vB�2B�GB�ZB�B�B�xB�nB�lB�QB�B��B��BȤBv�BE�BrB�tB� B�PB�OBXBU�BbABO�BJ�BK�BI�BG�B>lB1B�BxBqBIB
��B
�^B
�'B
ȧB
�B
�1B
o�B
U�B
N�B
D�B
=gB
6=B
+�B
rB	��B	�DB	ơB	�:B	I�B	$�B	�B	VB	+B	B��B��B�B�{B�JB�B��BɽBğB�B�wB��B�yB�B�eB�4B�YB�<B�*B�/B�0B�!B�B�B�B� B�B�/B�3B�GB�`B�eB�pB�tBBĜBĚBĝBĝBɺB�B�B�B�$B�)B�AB�GB�LB�ZB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	!B	GB	"�B	!�B	�B	�B	�B	 �B	"�B	%�B	)�B	'�B	)�B	*�B	,B	-	B	.B	.B	/B	3-B	43B	5:B	6?B	7GB	:WB	>rB	A�B	D�B	E�B	F�B	F�B	I�B	P�B	R�B	S�B	S�B	S�B	S�B	R�B	O�B	S�B	R�B	R�B	Q�B	Q�B	T�B	XB	X	B	YB	XB	]'B	`:B	a?B	cLB	eYB	irB	ipB	jwB	gfB	dSB	hkB	iqB	k~B	o�B	q�B	r�B	s�B	p�B	n�B	l�B	ipB	t�B	y�B	z�B	y�B	z�B	�B	�B	|�B	y�B	y�B	|�B	~�B	~�B	}�B	~�B	~�B	}�B	{�B	z�B	{�B	x�B	v�B	r�B	p�B	q�B	v�B	�B	�B	�B	�#B	�@B	�@B	�GB	�HB	�MB	�dB	�eB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�"B	�*B	�/B	�2B	�\B	�fB	�fB	�lB	�lB	�lB	�gB	�[B	�.B	��B	��B	��B	��B	��B	��B	��B	�~B	�LB	�=B	�QB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�)B	�'B	�'B	�(B	�"B	�B	�!B	�(B	�?B	�EB	�JB	�MB	�TB	�YB	�eB	B	ĒB	ƚB	ŖB	đB	đB	ƜB	ŕB	ÉB	ÉB	ĒB	đB	ŗB	ƛB	ƝB	ǡB	ɮB	ɮB	ȩB	ɭB	˹B	̾B	̾B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�)B	�4B	�.B	�(B	�/B	�5B	�4B	�6B	�8B	�@B	�AB	�@B	�GB	�FB	�DB	�GB	�NB	�TB	�WB	�WB	�aB	�lB	�qB	�wB	�xB	�}B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
G�O�B

/B
aB
xB
�B
"�B
(�B
/B
66B
;YB
A{B
F�B
M�B
P�B
T�B
ZB
_*B
cGB
fZB
l{B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150406091612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150406091612  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150406091612  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                