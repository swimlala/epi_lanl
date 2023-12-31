CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:26Z AOML 3.0 creation; 2016-08-07T21:36:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221326  20160807143628  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_008                   2C  D   APEX                            6531                            072314                          846 @������1   @�����@1<(�\�c�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD� D�L�D�i�D�ٚD�	�D�P D��3D���D��D�0 D��fDǶfD�	�D�L�Dړ3D��3D��D�S3D�p D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�z�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B��B��B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B؅B܅B��B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=D4�=D5�D5�=D6
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
=Dt}pDy��D�D�Q�D�n�D�޹D��D�UD��RD���D��D�5D���Dǻ�D��D�Q�DژRD��RD��D�XRD�uD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�-A�1'A�33A�33A�/A�-A�"�A�1'A�/A�-A�;dA�;dA�=qA�9XA�9XA�7LA�7LA�7LA�7LA�9XA�5?A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�/A�(�A��A�oA�l�A�M�A��/A���AЕ�A�E�A���A�t�A�A�ffA���A�{A���A� �A�ffAÉ7A���A�+A���A��A��PA�oA��/A���A���A�?}A��A��/A�+A�+A���A�/A�r�A�O�A��jA�\)A��A�S�A��A��yA��7A�^5A���A��yA��A��yA��A��7A�ƨA���A��^A��9A��A��jA�%A��uA�A|ĜA|A�A{��AzffAx�DAv�9Ar�Ao|�AkƨAfVAeS�Ad�Ad~�AdM�Ac�A`A�A]\)AYhsAU�mAT��AT �AP�9AL��AK��AI�-AFE�AD�ACK�ABQ�AA�hA?�-A=hsA<��A<�RA;�;A9ƨA8jA5�TA5oA333A1�
A1O�A/G�A-��A+�-A*��A)|�A'�^A'p�A'O�A&�A$M�A#��A#�7A#�A#�A!ƨA ��A�wA�wAZA1A^5A�;Ax�AA9XA�AK�A��AjAbA�
A��A��A��AA�A�#A�^A�7AS�A
=A�A�jA$�A�-A
�RA	�wA9XA�FAK�A��A��A��A��A�-AjA(�A+A�RA�A`BAK�AM�AdZA^5A�+AbNA�mAAp�A;dA ~�@��F@��@�v�@�J@��/@�+@�@�?}@��/@��@�\)@��\@��T@��;@�  @ꟾ@��;@�@�r�@�  @�S�@◍@���@�Q�@���@��y@�v�@�E�@�$�@��@�~�@�
=@���@��T@��@�  @��y@��@�r�@�1'@���@ץ�@׍P@�;d@�33@�@�V@���@��;@�l�@Ұ!@���@�z�@�z�@Ͼw@��;@�+@ΰ!@Ο�@��`@�|�@�dZ@˕�@�A�@�|�@ʗ�@ə�@ɉ7@�?}@���@���@ȓu@�Q�@ǍP@�ȴ@�J@�x�@��/@�S�@�dZ@Õ�@Å@�dZ@��@�E�@�`B@��@�?}@��7@��@��`@�z�@�Z@�9X@��@��F@�\)@�33@�@�^5@�M�@�5?@���@�O�@�/@�V@��/@��j@��@�r�@�z�@�r�@�Z@�(�@��F@�"�@�@��@�X@�/@��@��@�V@��@�9X@��;@��@��@�ȴ@�v�@�=q@���@��@���@�`B@�7L@��@�ƨ@�
=@�dZ@��@��H@���@��\@�V@�$�@��@�=q@�E�@���@���@�p�@�`B@��`@��u@�(�@�K�@�@��@���@��7@�&�@��@�V@�b@��@���@���@�|�@�S�@��@�v�@�@���@�p�@�%@��@�z�@�1'@��@�  @��w@��@�S�@���@�$�@���@���@�hs@��@��/@��@��@�1'@���@��y@��R@��\@�M�@�$�@��@��T@��#@��T@��@��h@��h@��@��@��7@�hs@�?}@�V@��9@�j@� �@���@���@���@�S�@��+@�M�@�@�@���@�G�@�/@�%@��@���@���@���@��@�S�@�C�@�"�@�o@��y@�~�@�M�@��@�hs@��@���@��/@�I�@�1'@� �@��@�  @�|�@��@���@��R@���@�n�@��@��-@�X@��@��@�bN@�Z@�I�@�  @��w@�;d@��@���@�5?@���@��@���@��@�X@��/@��u@�bN@���@�^5@���@���@~�@t�j@o�w@e?}@ZJ@P��@G�P@A�@7�@/;d@*=q@%�@�@�F@�R@-@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�+A�-A�1'A�33A�33A�/A�-A�"�A�1'A�/A�-A�;dA�;dA�=qA�9XA�9XA�7LA�7LA�7LA�7LA�9XA�5?A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�1'A�/A�(�A��A�oA�l�A�M�A��/A���AЕ�A�E�A���A�t�A�A�ffA���A�{A���A� �A�ffAÉ7A���A�+A���A��A��PA�oA��/A���A���A�?}A��A��/A�+A�+A���A�/A�r�A�O�A��jA�\)A��A�S�A��A��yA��7A�^5A���A��yA��A��yA��A��7A�ƨA���A��^A��9A��A��jA�%A��uA�A|ĜA|A�A{��AzffAx�DAv�9Ar�Ao|�AkƨAfVAeS�Ad�Ad~�AdM�Ac�A`A�A]\)AYhsAU�mAT��AT �AP�9AL��AK��AI�-AFE�AD�ACK�ABQ�AA�hA?�-A=hsA<��A<�RA;�;A9ƨA8jA5�TA5oA333A1�
A1O�A/G�A-��A+�-A*��A)|�A'�^A'p�A'O�A&�A$M�A#��A#�7A#�A#�A!ƨA ��A�wA�wAZA1A^5A�;Ax�AA9XA�AK�A��AjAbA�
A��A��A��AA�A�#A�^A�7AS�A
=A�A�jA$�A�-A
�RA	�wA9XA�FAK�A��A��A��A��A�-AjA(�A+A�RA�A`BAK�AM�AdZA^5A�+AbNA�mAAp�A;dA ~�@��F@��@�v�@�J@��/@�+@�@�?}@��/@��@�\)@��\@��T@��;@�  @ꟾ@��;@�@�r�@�  @�S�@◍@���@�Q�@���@��y@�v�@�E�@�$�@��@�~�@�
=@���@��T@��@�  @��y@��@�r�@�1'@���@ץ�@׍P@�;d@�33@�@�V@���@��;@�l�@Ұ!@���@�z�@�z�@Ͼw@��;@�+@ΰ!@Ο�@��`@�|�@�dZ@˕�@�A�@�|�@ʗ�@ə�@ɉ7@�?}@���@���@ȓu@�Q�@ǍP@�ȴ@�J@�x�@��/@�S�@�dZ@Õ�@Å@�dZ@��@�E�@�`B@��@�?}@��7@��@��`@�z�@�Z@�9X@��@��F@�\)@�33@�@�^5@�M�@�5?@���@�O�@�/@�V@��/@��j@��@�r�@�z�@�r�@�Z@�(�@��F@�"�@�@��@�X@�/@��@��@�V@��@�9X@��;@��@��@�ȴ@�v�@�=q@���@��@���@�`B@�7L@��@�ƨ@�
=@�dZ@��@��H@���@��\@�V@�$�@��@�=q@�E�@���@���@�p�@�`B@��`@��u@�(�@�K�@�@��@���@��7@�&�@��@�V@�b@��@���@���@�|�@�S�@��@�v�@�@���@�p�@�%@��@�z�@�1'@��@�  @��w@��@�S�@���@�$�@���@���@�hs@��@��/@��@��@�1'@���@��y@��R@��\@�M�@�$�@��@��T@��#@��T@��@��h@��h@��@��@��7@�hs@�?}@�V@��9@�j@� �@���@���@���@�S�@��+@�M�@�@�@���@�G�@�/@�%@��@���@���@���@��@�S�@�C�@�"�@�o@��y@�~�@�M�@��@�hs@��@���@��/@�I�@�1'@� �@��@�  @�|�@��@���@��R@���@�n�@��@��-@�X@��@��@�bN@�Z@�I�@�  @��w@�;d@��@���@�5?@���@��@���@��@�X@��/@��u@�bNG�O�@�^5@���@���@~�@t�j@o�w@e?}@ZJ@P��@G�P@A�@7�@/;d@*=q@%�@�@�F@�R@-@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1B1B1B1B1B1B+B+B+B1B1B	7B1B	7BDBJBJBPBPBPBPBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBJB%B
��B
�TB
�TB
�B
�B
�sB
��B	7B"�B%�B"�BE�Bk�By�B�%B�BĜBÖB�
B��B1B\BVBbBoBbBVB
=BB�B�mB�NB�ZB�5B�/B�/B�
BŢB�^B�B�B��Bw�B8RB�BDB
�ZB
ɺB
�FB
�{B
p�B
`BB
A�B
�B
B	�B	�B	��B	��B	B	�?B	��B	�PB	u�B	aHB	C�B	=qB	:^B	8RB	5?B	0!B	�B	bB	  B�B�B�`B�#B��B��BĜBBBŢBB�wB�dB�^B�XB�RB�FB�9B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3BB��B��BǮBĜBŢBɺB��B�)B�TB�ZB�mB�B�B�B��B	B	JB	hB	oB	{B	{B	hB	VB	uB	hB	B��B��B��B��B	B	B	B	
=B	oB	,B	>wB	C�B	=qB	9XB	2-B	.B	.B	,B	+B	>wB	@�B	A�B	A�B	B�B	C�B	C�B	C�B	B�B	B�B	B�B	@�B	>wB	;dB	9XB	7LB	5?B	0!B	1'B	1'B	#�B	�B	bB	DB	B��B��B��B��B��B��B��B	B	+B	+B		7B	PB	oB	�B	#�B	-B	-B	+B	&�B	"�B	�B	�B	�B	 �B	!�B	#�B	'�B	-B	0!B	2-B	2-B	0!B	1'B	5?B	7LB	7LB	?}B	@�B	G�B	I�B	I�B	O�B	M�B	J�B	K�B	N�B	T�B	R�B	R�B	R�B	W
B	YB	[#B	[#B	[#B	[#B	]/B	_;B	cTB	dZB	e`B	gmB	iyB	l�B	l�B	n�B	r�B	s�B	s�B	t�B	v�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�VB	�VB	�\B	�bB	�hB	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�LB	�LB	�LB	�LB	�RB	�RB	�^B	�^B	�qB	��B	B	B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
PB
�B
�B
�B
"�B
'�B
1'B
8RB
@�B
E�B
N�B
VB
[#B
^5B
e`B
hsB
l�B
q�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B'B'B%B%B%B%B!B!BB'B'B	.B'B	,B=BBB?BFBFBFBFBLBLBOBMBLBLBMBMBLBOBOBOBKBLBMBLBNB?BB
��B
�JB
�GB
�tB
�uB
�gB
��B	,B"�B%�B"�BE�BkxBy�B�B��BĎBÆB��B��B#BLBHBVBcBPBIB
-B �B�B�_B�>B�KB�)B�#B�%B��BŔB�QB�B��B��Bw�B8EB�B5B
�MB
ɭB
�:B
�oB
p�B
`9B
AB
�B
B	�B	��B	��B	��B	B	�7B	��B	�KB	u�B	aEB	C�B	=pB	:\B	8OB	5>B	0B	�B	aB��B�B�B�`B�%B��B��BĠBBBŢBB�zB�gB�aB�\B�RB�IB�=B�1B�#B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�3BB��B��BǬBĜBŢBɺB��B�(B�TB�XB�kB�~B�B�B��B	
B	EB	gB	kB	xB	zB	eB	RB	rB	dB	B��B��B��B��B	B		B	B	
9B	hB	,B	>qB	C�B	=kB	9QB	2&B	.B	.B	,B	*�B	>nB	@}B	A�B	A�B	B�B	C�B	C�B	C�B	B�B	B�B	B�B	@}B	>pB	;_B	9PB	7FB	59B	0B	1"B	1B	#�B	�B	_B	?B	B��B��B��B��B��B��B��B	B	%B	&B		1B	KB	iB	�B	#�B	-B	-	B	*�B	&�B	"�B	�B	�B	�B	 �B	!�B	#�B	'�B	-B	0B	2%B	2&B	0B	1B	58B	7CB	7EB	?wB	@{B	G�B	I�B	I�B	O�B	M�B	J�B	K�B	N�B	T�B	R�B	R�B	R�B	WB	YB	[B	[B	[B	[B	]%B	_1B	cLB	dSB	eWB	gdB	irB	l�B	l�B	n�B	r�B	s�B	s�B	t�B	v�B	{�B	}�B	�B	�B	�B	�
B	�B	�B	�!B	�$B	�KB	�JB	�QB	�WB	�\B	�^B	�\B	�cB	�jB	�kB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�	B	��B	��B	�	B	�B	�B	�	B	�B	�B	�B	�B	�*B	�2B	�>B	�AB	�BB	�?B	�FB	�EB	�SB	�OB	�aB	�}B	B	B	B	B	ÈB	ŕB	ƜB	ǣB	ǢB	ȧB	ɭB	ʸB	˹B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�	B	�B	�B	�B	�!B	� B	�!B	�!B	�*B	�.B	�4B	�5B	�6B	�:B	�?B	�GB	�KB	�UB	�ZB	�YB	�aB	�_B	�eB	�nB	�sB	�rB	�yB	�wB	�wB	�wB	�uB	�qB	�wB	�yB	�}B	�}B	�}B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
@B
wB
�B
�B
"�B
'�B
1B
8DB
@sB
E�B
N�B
U�B
[B
^&B
eQB
haB
lxB
q�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436282016080714362820160807143628  AO  ARCAADJP                                                                    20150226221326    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221326  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221326  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143628  IP                  G�O�G�O�G�O�                